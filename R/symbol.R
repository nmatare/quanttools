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
