#'
#' @title 
#' 'make_bars'
#'
#' @description 
#' Creates financial 'bars' (also known as 'candlesticks') 
#' from raw trade data. One may construct either time, volume, unit, tick, 
#' tick-runs, tick-imbalance (TIBs) volume-imbalance (VIBs), 
#' unit-imbalance (DIBs), or CUSUM-bars.
#' 
#' @details 
#' 'make_bars' creates an OHLCV (Open/High/Low/Close/Volume) matrix of 
#' class 'xts' The function requires an input of class 'data.frame' or 'matrix' 
#' with the following columns:
#' 
#'      timestamp:    time of observation; will be used as the xts index 
#' 
#'      price:        the price of the transacted units
#' 
#'      size:         number of transacted units
#' 
#'      side:         an integer vector of 1L(buy) or -1L(sell)
#' 
#' Type may be one of either time, volume, unit, tick runs, tick imbalance
#' volume imbalance, unit imbalance, or CUSUM bars.
#' 
#' time:       sampled every 'by' units of time; 
#'             must be a vector of length two, and corresponding to 
#'             [units, interval]; for example, 'by=c(1, "days")' or 
#'             'by=c(1, "secs")'. Default is c(1, "days")
#' 
#' tick:       sampled every 'by' number of ticks
#' 
#' volume:     sampled every 'by' units of volume 
#' 
#' unit:       sampled every 'by' units(dollars) of units (e.g., every $10,000)
#' 
#' *NOTE* 'tick runs', 'tick imbalance', 'volume imbalance', 'unit imbalance', 
#' and 'CUMSUM' are not yet supported
#'
#' @param x     A data.frame of raw tick data. The object must contain
#'              columns: timestamp(POSIXct), price(numeric), size(numeric), 
#'              and side(character). 
#'
#' @param type  One of either: time, volume, unit, tick, tick runs, tick 
#'              imbalance volume imbalance, unit imbalance, or CUSUM bars. 
#'              See details for further information.
#' 
#' @param by    An integer (or in the case of type='time', an integer 
#'              and interval) specifying the sampling frequency. 
#'              See details for further information.
#'
#' @return
#' 
#' a matrix of class 'xts' containing:
#' \describe{  
#' \item{bars:}{A matrix of class 'xts' containing 
#' Open/High/Low/Close/Volume time series data}
#'
#' @references  Lopez de Prado, Marcos, Advances in Financial Machine 
#'              Learning (2018) 
#'
#' @keywords    make_bars
#'
#' @author      Nathan Matare <email: nmatare@chicagobooth.com>
#'
#' @examples
#' \dontrun{
#' df <- data.frame(
#'  timestamp=seq(from = as.POSIXct("2018-01-21"), 
#'                length.out = 100, by = "secs"),
#'  price=rnorm(100,65,5),
#'  size=rnorm(100,10,1),
#'  side=rep(c(1L, -1L), 50)
#' )
#' 
#' result <- make_bars(x=df, type='time', by=c(5, "mins"))
#' result
#'
#' }
#' @export
make_bars <- function(x, type, by=c(1, "days")){

    type <- match.arg(type, c("time", "tick", "volume", "unit", 
        "tick runs", "tick imbalance", "volume imbalance", "unit imbalance",
        "CUSUM"))

    if(!all(c("timestamp", "size", "price", "side") %in% colnames(x)))
        stop("You must supply a data.frame or matrix with columns: 
             timestamp, price, size, and side")

    if(!(x[ ,'side'] == 1L || x[ ,'side'] == -1L))
        stop("You must supply 'side' as an integer 
             vector of 1L(buy) or -1L(sell)")

    if(!is.double(x[ ,'size']))
        stop("You must give the 'size' as a numeric vector")

    if(!is.double(x[ ,'price']))
        stop("You must give the 'price' as a numeric vector")

    # Transform into XTS
    ticks <- xts::xts(
        x=cbind(
            price=x[["price"]],
            size=x[["size"]],
            side=x[['side']]
        ),
        order.by=x[["timestamp"]]
    )

    .create_bars <- function(X, INDEX){

        bars <- xts::period.apply(
            x=X,
            INDEX=INDEX,
            FUN=function(x){
                ticks=zoo::coredata(x$price)
                c(xts::first(ticks), max(ticks), 
                  min(ticks), xts::last(ticks), sum(x$size))
            }
        ) 
        return(bars)
    }

    # Sample 
    bars <- switch(type,
        time={ # Time Bars

            if(length(by) != 2)
                stop("You must specify 'by' as a vector of length two, 
                     and corresponding to [units, interval]")

            choices <- c("us", "ms", "secs", "mins", "hours", 
                "days", "weeks", "months","quarters", "years")
            matched_arg <- match(x=by[2], table=choices)
            if(is.na(matched_arg)) 
                stop(gettextf("'by[2]' should be one of %s", 
                    paste(dQuote(choices), collapse = ", ")), domain = NA)            

            if(is.double(by[1]))
                stop("The first element in the vector must be of 
                     type 'numeric'")

            bars <- .create_bars( # create by 'by' units of time
                X=ticks, 
                INDEX=xts::endpoints(x=ticks, on=by[2], k=by[1])
            )          
            bars <- xts::align.time(bars, by[1])

        },
        tick={ # Tick Bars

            if(!is.double(by))
                stop("You must provide 'by' as the number of sampled ticks")

            ticks_dd <- stats::aggregate(ticks$size, by=index(ticks), FUN=sum)         
            ticks_dd <- merge.xts(
                ticks_dd[ ,1L], # aggregated volume per duplicate times
                ticks[!duplicated(index(ticks)) # attributes per dd's index
                    ][index(ticks_dd)][ ,c("price", "side")]
                )
            colnames(ticks_dd) <- c("size", "price", "side")

            bars <- .create_bars( # create every 'by' number of ticks
                X=ticks_dd, 
                INDEX=seq(1L, nrow(ticks_dd), by)
            )     
        },
        volume={ # Volume Bars 

            if(!is.double(by))
                stop("You must provide 'by' as the amount of sampled volume")

            # @TODO, perhaps this would be faster with a binary search tree?
            csummed <- cumsum_reset(ticks$size, threshold=by)
            eps <- which(!duplicated(names(csummed)))
            eps[1L] <- 0L

            # create every 'by' number of order size 
            bars <- .create_bars(X=ticks, INDEX=eps)   
        },
        unit={ # Unit(Dollar) Bars

            if(!is.double(by))
                stop("You must provide 'by' as the number of sampled units")

            csummed <- cumsum_reset((ticks$price*ticks$size), threshold=by)
            eps <- which(!duplicated(names(csummed)))
            eps[1L] <- 0L

            # create every 'by' number of units is traded
            bars <- .create_bars(X=ticks, INDEX=eps) 
        }
        # TO DO
        # CUSUM Bars
        # Tick Imbalance Bars
        # Tick Runs Bars
        # Volume Imbalance Bars
        # Unit Imbalance Bars
    )

    colnames(bars) <- c('Open', 'High', 'Low', 'Close', 'Volume')
    return(bars)
}
