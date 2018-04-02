class_SymbolHistorical <- function(
    ticker, 
    start.date, 
    end.date, 
    envir       = .GlobalEnv, 
    ...
){

    ############################################################################
    #                             PUBLIC CLASS
    ############################################################################
    # PublicClass enables historical functionality for a symbol

    if(!is.environment(envir))
        if(is.character(envir)) 
            envir <- eval(parse(text = envir))
        else 
            stop("Please specify an available environment")

    # Defines an instrument to be traded
    self              <- new.env() # 'instance' of the object
    self$.data <- self$.instrument  <- NULL     # attributes stored here

    self$methods <- self$help <- function(){
        # returns all available methods for the symbol
        return(
        cat("
             query_csrp(...): 
             queries CRSP database from wrds  

             get_daily_crsp_data(wrds.db, start.date, end.date): 
             pulls daily bars from CRSP    
                
             make_as_stock(ticker, permno=NULL, NAICS=NULL): 
             assigns attributes, defines as FinancialInstrument        
                
             align_symbol_to_days(align.to.dates): 
             aligns all evaluated symbols across time
                
             set_quantstrat_pos_limit(portfolio, timestamp, min.pos, max.pos): 
             set's position limits for quantstrat
                
             store_data_on_hard_disk(): // NOT YET SUPPORTED
             removes data from RAM and sets into hard disk
                
             filter_by_market_cap(filter, timestamp): 
             returns logic if symbol meets market cap filter")
        )
    }

    ############################################################################
    ###                             PRIVATE METHODS                         ####
    ############################################################################

    self$make_as_stock <- function(
        ticker, 
        permno      = NULL, 
        NAICS       = NULL,
        ... 
    )
    {

        # Method defines self with Financial Instrument package, and assigns
        # it definitions and attributes
        name <- if(!is.null(permno)) 
                    paste0(ticker, ":", permno) 
                else 
                    ticker

        FinancialInstrument::stock(
            primary_id   = name, # not useful because ':'  is replaced with '.'
            identifiers  = list(
                ticker      = ticker, 
                permno      = permno,
                NAICS       = NAICS
            ), 
            currency    = "USD",  
            multiplier  = 1L,
            assign_i    = TRUE
        )

        self$.contract   <<- IBrokers::twsSTK(ticker)
        self$.instrument <<- FinancialInstrument::getInstrument(name)
    }

    

    self$query_csrp <- function(
        wrds.db, 
        start.date, 
        end.date)
    {

        if(is.null(self$.instrument))
            stop(paste0("You must specify '", self$.instrument$identifiers$ticker, "' as a FinancialInstrument first."))

        if(is.null(self$.instrument$identifiers$permno))
            stop(paste0("You must specify a PERMNO code for '", self$.instrument$identifiers$ticker, "' first."))

        start.date  <- to.POSIXct(start.date) 
        end.date    <- to.POSIXct(end.date)

        # Method returns SQL query for daily bars
        DT <- setDT(RJDBC::dbGetQuery(wrds.db,
                    strwrap(paste0("
                    SELECT 
                        DATE, HCOMNAM, HTICK, HTSYMBOL,
                        ABS(PRC) as CLOSE, 
                        ABS(OPENPRC) as OPEN,
                        BID, ASK, ASKHI, BIDLO, VOL, SHROUT, HNAICS,
                        CFACPR, HPRIMEXC
                    FROM 
                        CRSPQ.DSF
                    JOIN 
                        CRSPQ.DSFHDR
                    ON 
                        CRSPQ.DSF.PERMNO = CRSPQ.DSFHDR.PERMNO
                    WHERE 
                        CRSPQ.DSFHDR.PERMNO=", self$.instrument$identifiers$permno),
                    width = 1000L, simplify = TRUE)
        ))

        colnames(DT) <- c("DATE", "HCOMNAM", "HTICK", "HTSYMBOL",
                          "CLOSE", "OPEN", "BID", "ASK", "ASKHI",
                          "BIDLO", "VOL", "SHROUT", "HNAICS", 
                          "CFACPR", "HPRIMEXC")

        DT <- DT[DATE %between% c(start.date, end.date)]
        YEAR.MKTCAP = DT[ ,mean(SHROUT) * mean(CLOSE), by = zoo::as.yearmon(DATE)]
        setnames(YEAR.MKTCAP, c("Year", "Market.Cap"))

        self$.instrument$exchange <<- convert_crsp_exchange_code(last(DT[["HPRIMEXC"]]))
        self$.instrument$Market.Cap <<- YEAR.MKTCAP # assign attributes
        self$.instrument$identifiers$NAICS <<- unique(DT[["HNAICS"]])

        return(DT)
    }

    self$get_daily_crsp_data <- function(...)
    {
        # Method returns daily bars as XTS 
        DT = self$query_csrp(...) # quick name reference
        if(nrow(DT) == 0){
            print(paste0(self$.instrument$identifiers$ticker, " did not trade during specified range; skipping symbol"))
            return()                        
        }

        stopifnot(!any(duplicated(DT[["DATE"]]))) 
        impute_cols <- 
            c("CLOSE", "OPEN", "ASKHI", "BIDLO", "VOL", "SHROUT") # "BID", "ASK", 

         for(j in impute_cols)
            set(DT, j = j, value =  
                if(!all(is.na(DT[[j]]))) # only those that are not all NA
                    imputeTS::na.locf(
                        x            = DT[[j]],
                        option       = 'locf',
                        na.remaining = 'keep')
                else DT[[j]] # else return the column
            )

        if(all(unlist(DT[ ,lapply(.SD, # check if all columns are NA
            function(x) length(which(is.na(x))) == .N) 
            ,.SDcols = impute_cols]))){
            print(paste0(self$.instrument$identifiers$ticker, " table contains all NAs; skipping symbol"))
            return()    
        }

        # Remaining NAs will be at the beginning // drop these obs
        DT <- DT[complete.cases(DT[ ,c(impute_cols), with = FALSE])]

        if(any(is.na(DT[ ,impute_cols, with = FALSE])) || nrow(DT) == 0){
            print(paste0("Missing data detected in ", self$.instrument$identifiers$raw_id , " table; skipping symbol"))
            return()                    
        }

        XTS <- as.xts.data.table(
            DT[ ,Date := to.POSIXct(DATE)
             ][ ,.(Date,
                   OPEN    / CFACPR, 
                   ASKHI   / CFACPR, 
                   BIDLO   / CFACPR, 
                   CLOSE   / CFACPR, 
                   VOL     * CFACPR,
                   SHROUT  * CLOSE
                )])

        colnames(XTS)   <- paste0(self$.instrument$identifiers$raw_id ,
            c(".Open",".High", ".Low",".Close", ".Volume", ".Market.Cap"))
        self$.data <<- XTS
    } 
      
    self$align_symbol_to_days <- function(align.to.dates)
    {
        # aligns symbol to specified trading.dates

        dates.NA <- align.to.dates[!align.to.dates %in% index(self$.data)]

        if(length(dates.NA)){

            XTS <- rbind.xts(
                      cbind(Trading = TRUE, self$.data),  # old data
                      cbind(FALSE, as.xts(  # first a row of NAs, the all 0s
                         do.call(rbind, 
                                replicate(
                                   n         = length(dates.NA), 
                                   expr      = t(rep(as.numeric(NA), 6L)), 
                                   simplify = FALSE)),
                         order.by = dates.NA
                      ))
                   )

            last.obs <- last(index(XTS[ ,1] == 1))
            # temporarily fill the gaps with obs, last obs will be 
            # carried forward, but will be cutoff by 'last.obs'
            # *NOTE* no actual imputation is done here!

            XTS <- as.xts(apply(XTS, 2, function(j)
                imputeTS::na.locf(
                    x            = j,
                    option       = 'locf',
                    na.remaining = 'keep'
            )), order.by = index(XTS))

            block.NA <- which(XTS[ ,'Trading'] %in% 0 & 
                              XTS[ ,ncol(XTS)] %in% NA)

            XTS <- rbind.xts(
                XTS[block.NA, ], # combine periods of no trading
                XTS[XTS[ ,"Trading"] == 1, ] # gaps removed 
            )[paste0("::", last.obs)]

        } else
            XTS <- cbind(TRUE, self$.data)

        colnames(XTS)[1] <- paste(
            gsub(".Close","", colnames(Cl(XTS))), "Trading", sep = ".")

        stopifnot(ncol(XTS) == 7L) # saftey check
        self$.data <<- XTS # place new data into symbols env
    }

    self$set_quantstrat_pos_limit <- function(
        portfolio,
        timestamp, 
        max.pos, 
        min.pos)
    {

        quantstrat::addPosLimit(
            portfolio   = portfolio, 
            symbol      = self$.instrument$identifiers$raw_id ,
            timestamp   = timestamp,
            maxpos      = max.pos,
            minpos      = min.pos
        )
    }

    self$store_data_on_disk <- function()
    {

        # TO DO, needs to go to one pointer, and draws from indices intead

        # take out of memory and store off disk 
        # pattern <- gsub("[[:punct:]]", "", symbol)
        # if(!hasArg(temp.dir))
        #     temp.dir <- paste0(dir, "temp/", pattern)
        # # else use the provided temp.dir

        # dir.create(temp.dir, showWarnings = FALSE, recursive = TRUE)
        # temp.file <- tempfile(pattern = "", tmpdir  = temp.dir)

        # assign(
        #     x       = symbol,  # get.mmap 
        #     value   = as.mmap.xts(get(symbol), file = temp.file), 
        #     envir   = envir)
    }

    self$filter_by_market_cap <- function(filter, timestamp)
    {

        operation       <- stringr::str_extract(filter, ">=|<=|>|<|=|!=") 
        value           <- stringr::str_extract(filter, "[[:digit:]]+$")
        market.value    <- as.numeric( # Market.Cap at first trading observation
            Mc(self$.data)[Td(self$.data) == 1][timestamp])

        if(!length(market.value) || is.na(market.value))
            return(TRUE)

        else if(!eval(parse(text = paste(market.value, operation, value))))
            return(FALSE)  # passes ## 256000 > 5000 # if fail then not pass

        else if(eval(parse(text = paste(market.value, operation, value))))
            return(TRUE)   

        else
            stop(paste0("Invalid logic; please check your filter"))
    }

    class(self) <- c("Symbol")
    invisible(self)
}

### TO DO, ADD TICK METHOD TO CLASS SYMBOL
query_gdax <- function(
    username, 
    password, 
    host_address, 
    host_port="27017", 
    database_name="admin",
    start.date,
    end.date,
    book_depth=FALSE,
    strata=2L,
    exchange_activity=FALSE,
    trading_patterns=FALSE
){

    # strata: The desired granularity of trading data returned: 
    #           0 - returns price, size, and time
    #           1 - returns price, size, time, volume, spread, and median price
    #           2 - returns strata 1 and aggregated activity information before the previous tick

    # trading_patterns: Returns the client_id of all orders submitted to the market (identify traders)
    # book_depth:       Returns the top 5 levels of the orderbook (aggregated)

    .check_date <- function(date){
        ats <- attributes(date)
        if(!ats$class %in% c("POSIXct", "POSIXt") && ats$tzone == "America/New_York")
            FALSE
        else
            TRUE
    }

    stopifnot(sapply(c(start.date, end.date), .check_date))

    if(!strata %in% c(0L, 1L, 2L))
        stop("You may only choose [0,1,2] levels of tick data(granularity)")

    .check_py_modules <- function(module){
        if(!reticulate::py_module_available(module))
            stop(paste0("Could not find", module, "module; please run 'pip install", module,"'"))
        else   
            TRUE
    }

    stopifnot(all(sapply(c("bson", "pymongo", "dateutil"), .check_py_modules)))

    bson=reticulate::import("bson")
    dateutil=reticulate::import("dateutil", convert=FALSE)
    pymongo=reticulate::import("pymongo")

    .retrieve_r_data <- function(cursor, ...){
        json=bson$json_util$dumps(cursor)
        r_object <- jsonlite::fromJSON(json, ...)
        return(r_object)
    }
    
    # query database and get objects
    .create_monogo_db_instance <- function(host_address, host_port, username, password, database_name){
        # http://api.mongodb.com/python/current/faq.html#using-pymongo-with-multiprocessing
        return(pymongo$MongoClient(
            host=paste0(host_address, ":", host_port),
            username=username,
            password=password,
            connect=TRUE
        )[[database_name]])
    }

    query=reticulate::dict(list(
        timestamp=list(
            `$gte`= dateutil$parser$parse(format(start.date, "%Y-%m-%dT%H:%M:%SZ", "UTC")), 
            `$lte`= dateutil$parser$parse(format(end.date, "%Y-%m-%dT%H:%M:%SZ", "UTC"))
        )
    ))
    fields=reticulate::dict(tick.time=1L, tick.price=1L, tick.sequence=1L, tick.side=1L)

    cat(paste0("Quering tick data from MongoDB: ", username, "@", host_address), "\n")
    db=.create_monogo_db_instance(host_address, host_port, username, password, database_name)
    cursor=db$ticks$find(query, fields)
    ticks <- .retrieve_r_data(cursor)

    if(!nrow(ticks))
        stop("No data was found for the specified dates")

    ticks   <- data.table(`_id`=ticks$`_id`$`$oid`, ticks$tick) # get all ticks to happen in window
    ticks[ ,side := ifelse(side == "buy", 1L, -1L)]
    for(j in c("price", "sequence")) 
        set(ticks, j = j, value = as.numeric(as.character(ticks[[j]])))

    .get_side <- function(side, `_id`, db){

        side <- match.arg(side, c("asks", "bids"))
        cursor=db[[side]]$find(reticulate::dict(list(`_id`=bson$objectid$ObjectId(`_id`))))
        data <- .retrieve_r_data(cursor, simplifyDataFrame=FALSE)

        DT <- data.table(data[[1L]][[side]])
        setnames(DT, c("price", "size", "order_id"))
        for(j in c("price", "size")) 
            set(DT, j = j, value = as.numeric(as.character(DT[[j]])))
        return(DT)
    }

    .aggregate_side_to_levels <- function(DT, side, depth=5L){

        side <- match.arg(side, c("asks", "bids"))
        DT[ ,level         := .GRP,            by = .(price)      # level of order book
        ][ ,level_depth         := sum(size),       by = .(level)      # volume at each level
        ][ ,median_quote_size   := median(size),    by = .(level)      # median size of each order per level
        ][ ,max_quote_size      := max(size),       by = .(level)      # largest order per level
        ][ ,min_quote_size      := min(size),       by = .(level)      # smallest order per level
        ]

        setnames(DT, 
            old=c("level", "level_depth", "median_quote_size", 
                "max_quote_size", "min_quote_size"),
            new=paste0(side, c("_level", "_level_depth", 
                "_median_quote_size", "_max_quote_size", "_min_quote_size"))
        )

        .merge <- function(...) merge(..., all = TRUE)
        level <- colnames(DT)[grep('level', colnames(DT))][1]
        DT <- Reduce(.merge, list(
            DT,
            DT[size == 0.001,                  .N, by = level], # number of small(min) orders per level
            DT[ ,                              length(duplicated(.SD)), by = level], # number of unique orders per level
            DT[size %in% seq(0, 100, 0.01),   .N, by = level], # number of GUI clickers per level
            DT[size %in% seq(0, 100, 0.01), sum(size), by = level] # volume of GUI clickers per level  
        ))

        setnames(DT, 
            old=c("N.x", "V1.x", "N.y", "V1.y"), 
            new=paste0(side, c("_num_min_quotes","_num_unique_quotes","_num_GUI_quotes","_vol_GUI_quotes"))
        )

        for(j in seq_len(ncol(DT))) # impute NAs
            set(DT, which(is.na(DT[[j]])), j, 0L)

        DT_out <- DT[eval(parse(text=level)) <= depth, colnames(DT)[grep(side, colnames(DT))], with = FALSE]
        DT_out <- DT_out[!duplicated(DT_out)]
        return(DT_out)
    }

    .find_activity_by_side <- function(side, DT){

        .side <- match.arg(side, c("sell", "buy"))
        to_fill     <- DT[side == .side & type == "open" & is.na(reason)]
        canceled    <- DT[side == .side & type == "done" & reason == "canceled"]
        remaining   <- DT[type == "received" & side == .side]

        if(nrow(to_fill))
            to_fill <- unique(to_fill[ ,amount_to_fill    := sum(remaining_size, na.rm = TRUE)
                ][ ,max_amount_to_fill      := max(remaining_size, na.rm = TRUE)
                ][ ,median_amount_to_fill   := median(remaining_size, na.rm = TRUE)
                ][ ,median_price_to_fill    := median(price, na.rm = TRUE)
                ][ ,weighted_price_to_fill  := weighted.mean(price, remaining_size, na.rm = TRUE)
                ][ ,.(amount_to_fill, max_amount_to_fill, 
                    median_amount_to_fill, median_price_to_fill, 
                    weighted_price_to_fill)])
            setnames(to_fill, paste(colnames(to_fill), side, sep="_"))

        if(nrow(canceled))
            canceled <- unique(canceled[ ,amount_to_cancel   := sum(remaining_size, na.rm = TRUE)
                ][ ,max_amount_to_cancel      := max(remaining_size, na.rm = TRUE)
                ][ ,median_amount_to_cancel   := median(remaining_size, na.rm = TRUE)
                ][ ,median_price_to_cancel    := median(price, na.rm = TRUE)
                ][ ,weighted_price_to_cancel  := weighted.mean(price, remaining_size, na.rm = TRUE)
                ][ ,.(amount_to_cancel, max_amount_to_cancel, 
                    median_amount_to_cancel, median_price_to_cancel, 
                    weighted_price_to_cancel)])
            setnames(canceled, paste(colnames(canceled), side, sep="_"))

        if(nrow(canceled))
            remaining <- cbind(
                unique(remaining[ ,amount_received            := sum(size, na.rm = TRUE)
                    ][ ,max_amount_received        := max(size, na.rm = TRUE)
                    ][ ,median_amount_received     := median(size, na.rm = TRUE)
                    ][ ,median_price_received      := median(price, na.rm = TRUE)
                    ][ ,weighted_price_received    := weighted.mean(price, size, na.rm = TRUE)
                    ][ ,.(amount_received, max_amount_received, 
                        median_amount_received, median_price_received, 
                        weighted_price_received)]
                    ), 
                remaining=ncol(remaining) 
            )
            setnames(remaining, paste(colnames(remaining), side, sep="_"))

        to_DT <- do.call(cbind, c(
            as.list(remaining),
            as.list(canceled),
            as.list(to_fill)))

        return(if(!nrow(to_DT)) NULL else to_DT)
    }

    .get_table_stat <- function(stat, DT) t(as.matrix(table(DT[[stat]])))
    .get_table_DT <- function(stat, field, DT) tryCatch(.get_table_stat(stat, DT)[ ,field], error=function(e) 0L)
   
    total_ticks <- nrow(ticks)
    iticks <- iterators::iter(ticks, by = "row")

    output <- foreach::foreach(
            tick            = iticks, 
            .inorder        = TRUE,
            .errorhandling  = c("stop"),
            .packages       = c("lubridate", "plyr", "xts", "data.table", "reticulate")
            ) %dopar% {

                utc_time    <- strptime(tick[["time"]], "%Y-%m-%dT%H:%M:%OS", "UTC")
                timestamp   <- lubridate::with_tz(utc_time, "America/New_York")
                row         <- attributes(tick)$row.names
                `_id`       <- tick[["_id"]]
                trading_flows=NULL
                trading_activity=NULL

                # create child instance for multiprocessing
                db_child=.create_monogo_db_instance( 
                    host_address, 
                    host_port, 
                    username, 
                    password, 
                    database_name
                )

                if(row == 1L) 
                    cat(paste0("Processing ticks - at strata #", strata, "\r"))
                else if(row %% 500 == 0)
                    cat("\r", paste0(round(row / total_ticks * 100), "%..."))

                if(strata > 0L){

                    DT_bids <- .get_side("bids", `_id`, db_child)
                    DT_asks <- .get_side("asks", `_id`, db_child)
                    if(DT_bids[which.max(price), .(price)] > DT_asks[which.min(price), .(price)])
                        stop(paste("Somehow, the max bid exceeds the max ask price for OrderId(_id):", `_id`))

                    med_price       <- ((min(DT_asks[["price"]]) + max(DT_bids[["price"]])) / 2)
                    bid_ask_spread  <- (min(DT_asks[["price"]])  - max(DT_bids[["price"]]))

                    if(book_depth){

                        trading_flows = list(
                            asks = .aggregate_side_to_levels(DT_asks, "asks"),
                            bids = .aggregate_side_to_levels(DT_bids, "bids")
                        )
                    }

                    cursor=db_child$activity$find(reticulate::dict(list(`_id`=bson$objectid$ObjectId(`_id`))))
                    data <- .retrieve_r_data(cursor, simplifyDataFrame=FALSE)
                    DT_act <- as.data.table(do.call(plyr::rbind.fill, lapply(data[[1]]$activity, function(...) as.data.frame(t(unlist(c(...)))))))
                    DT_act[ ,side := ifelse(side == "buy", 1L, -1L)] # convert to integer

                    for(j in c("remaining_size", "price", "size", "sequence")) 
                        set(DT_act, j = j, value = as.numeric(as.character(DT_act[[j]])))

                    if(is.null(DT_act[['reason']])) # incase these aren't returned
                        DT_act[ ,reason := NA]

                    if(is.null(DT_act[['type']]))
                        DT_act[ ,type   := NA]

                    if(is.null(DT_act[['client_oid']]))
                        DT_act[ ,client_oid   := NA]

                    f_or_m  <- DT_act[type == 'match' | reason == 'filled']
                    if(tick[["sequence"]] > max(f_or_m[["sequence"]]))
                        stop(paste("Somehow, the ticker sequence is greater than the last fill for OrderId(_id):", `_id`))

                    strata_1 <- t(c(
                        volume      = sum(DT_act[type == "match"][["size"]]), 
                        med_price   = med_price, 
                        spread      = bid_ask_spread
                    ))

                    if(strata == 2L){

                        stats <- data.table(
                            received = .get_table_DT("type" ,'received', DT_act),
                            done     = .get_table_DT("type", "done", DT_act),
                            open     = .get_table_DT("type","open", DT_act),
                            total    =  nrow(DT_act),
                            sells    = .get_table_DT("side" ,'buy', DT_act),
                            buys     = .get_table_DT("side" ,'sell', DT_act),
                            limits   = .get_table_DT("order_type" ,'limit', DT_act),
                            canceled = .get_table_DT("reason" ,'canceled', DT_act)
                        )

                        strata_2 <- list(
                            stats,
                            .find_activity_by_side("buy", DT_act),
                            .find_activity_by_side("sell", DT_act)
                        )

                        strata_2 <- do.call(cbind, strata_2[!sapply(strata_2, is.null)])
                    }

                    if(trading_patterns){
                        if(strata != 2L)
                            stop("You need to perform exchange activity analysis (strata 2) in order to derive trading pattern statistics")
                
                        trading_activity <- DT_act[type == "received", .(client_oid, time, side, size, price)] # find trading patterns
                    }
                }

                strata_0 <- t(c(
                    price       = tick[['price']], 
                    side        = tick[['side']]
                ))

                trading <- xts(
                    if(strata == 0)
                        strata_0
                    else if(strata == 1) 
                        cbind(strata_0, strata_1)
                    else 
                        cbind(strata_0, strata_1, strata_2),
                    order.by = timestamp
                )

                return(list(tick=trading, trading_activity=trading_activity, trading_flows=trading_flows))
    }

    agg_tick_data   <- do.call(rbind, lapply(output, function(x) x$tick))
    agg_trading_act <- do.call(rbind, lapply(output, function(x) x$trading_activity))
    agg_trading_flows <- lapply(output, function(x) x$trading_flows)
    names(agg_trading_flows) <- index(agg_tick_data)
    xts::indexTZ(agg_tick_data) <- "America/New_York"

    return(list(
        ticks       = agg_tick_data,
        activity    = agg_trading_act,
        flows       = agg_trading_flows
    ))
}