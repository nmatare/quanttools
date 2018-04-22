#'
#' @title 
#' 'make_bars'
#'
#' @description 
#' Creates financial time series 'bars' (also known as 'candlesticks') 
#' from raw tick data. One may create either time, volume, unit, tick-runs, tick-imbalance (TIBs)
#' volume-imbalance (VIBs), unit-imbalance (DIBs), or CUSUM-bars.
#' 
#' @details 
#' 'make_bars' creates an OHLCV (Open/High/Low/Close/Volume) matrix of 
#' class 'xts' The function requires an input of class 'data.frame' or 'matrix' 
#' with the following columns:
#' 
#'      timestamp:    ISO 4601 time of observation
#' 
#'      price:        the price of the transacted units
#' 
#'      size:         number of transacted units
#' 
#'      side:         a character vector of values either 'buy' or 'sell'
#' 
#' Type may be one of either time, volume, unit, tick runs, tick imbalance
#' volume imbalance, unit imbalance, or CUSUM bars.
#' 
#' time:       sampled every 'by' units of time; 
#'             must be a vector of length two, and corresponding to [units, interval];
#'             for example, 'by=c(1, "days")' or 'by=c(1, "sec")'; default is c(1, "days")
#' 
#' tick:       sampled every 'by' number of ticks
#' 
#' volume:     sampled every 'by' units of volume 
#' 
#' unit:       sampled every 'by' units(dollars) of units (e.g., every $10,000)
#' 
#' *IMPORTANT* 'tick runs', 'tick imbalance', 'volume imbalance', 'unit imbalance', and 'CUMSUM' are not yet supported
#'
#' @param x     A matrix or data.frame of raw tick data. The object must contain
#'              columns: timestamp(ISO 4601), price(numeric), size(numeric), 
#'              and side(character). 
#'
#' @param type  One of either: time, volume, unit, tick runs, tick imbalance
#'              volume imbalance, unit imbalance, or CUSUM bars. See details for further 
#'              information.
#' 
#' @param by    An integer (or in the case of type='time', an integer and interval) 
#'              specifying the sampling frequency. See details for further information.
#'
#' @return
#' 
#' a matrix of class 'xts' containing:
#' \describe{  
#' \item{bars:}{A matrix of class 'xts' containing Open/High/Low/Close/Volume time series data}
#'
#' @references  Lopez de Prado, Marcos, Advances in Financial Machine Learning (2018) 
#'
#' @keywords    make_bars
#'
#' @author      Nathan Matare <email: nmatare@chicagobooth.com>
#'
#' @examples
#' \dontrun{
#' df <- data.frame(
#'  timestamp=seq(from = as.POSIXct("2018-01-21"), length.out = 100, by = "secs"),
#'  price=rnorm(100,65,5),
#'  size=rnorm(100,10,1),
#'  side=rep(c("buy", 'sell'), 50)
#' )
#' 
#' result <- make_bars(x=df,type='time', by=c(5, "mins"))
#' result
#'
#' }
#' @export

make_bars <- function(x, type, by=c(1, "days")){

    type <- match.arg(type, c("time", "tick", "volume", "unit", 
        "tick runs", "tick imbalance", "volume imbalance", "unit imbalance",
        "CUSUM"))

    if(!all(c("timestamp", "size", "price", "side") %in% colnames(x)))
        stop("You must supply a data.frame or matrix with columns: timestamp, price, size, and side")

    # Transform into XTS
    ticks <- xts::xts(
        x=cbind(
            price=x[["price"]],
            size=x[["size"]],
            side=ifelse(x[['side']] == 'buy', 1L, -1L)
        ),
        order.by=x[["timestamp"]]
    )

    .create_bars <- function(X, INDEX){

        bars <- xts::period.apply(
            x=X,
            INDEX=INDEX,
            FUN=function(x){
                ticks=zoo::coredata(x$price)
                c(xts::first(ticks), max(ticks), min(ticks), xts::last(ticks), sum(x$size))
            }
        ) 
        return(bars)
    }

    c.cumsum <- function(x, threshold){
        # TO DO, turn in Rcpp code
        stopifnot(is.numeric(x))
        runsum=0L
        group=1L
        result=numeric()

        for(i in seq(x)){
            runsum=runsum+x[i]
            if(runsum > threshold){ # reset
                runsum=x[i]
                group=group+1L
            }
            result=c(result, structure(runsum, names=group))
        }
        return(result)
    }

    # Sample 
    bars <- switch(type,
        time={ # Time Bars

            if(length(by) != 2)
                stop("You must specify 'by' as a vector of length two corresponding to [units, interval]")

            choices <- c("us", "ms", "secs", "mins", "hours", # microseconds, milliseconds
                "days", "weeks", "months","quarters", "years")
            matched_arg <- match(x=by[2], table=choices)

            if(is.na(matched_arg)) 
                stop(gettextf("'by[2]' should be one of %s", paste(dQuote(choices), collapse = ", ")), domain = NA)            

            bars <- .create_bars( # create by 'by' units of time
                X=ticks, 
                INDEX=xts::endpoints(x=ticks, on=by[2], k=as.numeric(by[1]))
            )          
            bars <- xts::align.time(bars, as.numeric(by[1]))

        },
        tick={ # Tick Bars

            if(!is.numeric(by))
                stop("You must provide 'by' as the number of ticks to sample by")

            ticks_dd <- aggregate(ticks$size, by=index(ticks), FUN=sum) # aggregate volume 
            ticks_dd <- merge.xts(
                ticks_dd[ ,1L], # aggregated volume per duplicate times
                ticks[!duplicated(index(ticks)) # attributes per ticks_dd's index
                    ][index(ticks_dd)
                    ][ ,c("price", "side")])
            colnames(ticks_dd) <- c("size", "price", "side")

            bars <- .create_bars( # create every 'by' number of ticks
                X=ticks_dd, 
                INDEX=seq(1L, nrow(ticks_dd), by)
            )     
        },
        volume={ # Volume Bars 

            if(!is.numeric(by))
                stop("You must provide 'by' as the number of ticks to sample by")

            groups <- c.cumsum(as.numeric(ticks$size), threshold=by)
            eps <- which(!duplicated(names(groups))); eps[1L] <- 0L

            bars <- .create_bars( # create every 'by' number of order size    
                X=ticks, 
                INDEX=eps
            ) 
        },
        unit={ # Unit(Dollar) Bars

            if(!is.numeric(by))
                stop("You must provide 'by' as the number of ticks to sample by")

            groups <- c.cumsum(as.numeric(ticks$price * ticks$size), threshold=by)
            eps <- which(!duplicated(names(groups))); eps[1L] <- 0L

            bars <- .create_bars( # create every 'by' number of units is traded
                X=ticks, 
                INDEX=eps
            ) 
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
