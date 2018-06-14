#'
#' @title 
#' Deflated Sharpe Ratio (DSR)
#'
#' @description 
#' Computes the deflated sharpe ratio vis-a-vis Extreme Value Theory 
#' 
#' @details 
#'
#' @param N The total number of trials (parameters) attempted
#' 
#' @param T The total number of observations (returns) s 
# 
#' @param skew The observed skew of the 'T' real returns
#' 
#' @param kurtosis The observed kurtosis of the 'T' real returns
#' 
#' @param observed.sharpe The observed Sharpe Ratio calculated from 'T' real returns
#' 
#' @param variance.sharpe The observed variance of the Sharpe Ratios's across 'N' trials
#'
#' @return
#' 
#' a list of class 'cscv' containing:
#' \describe{  
#' \item{pvalue:}{The probability of rejecting the }
#'
#' \item{dsr:}{The deflated sharpe ratio}
#'
#' @references Bailey et al (2015) "The Deflated Sharpe Ratio: 
#' Correcting for Selection Bias, Backtest Overfitting and Non-Normality" 
#' \url{https://papers.ssrn.com/sol3/papers.cfm?abstract_id=2460551}
#'
#' @keywords    dsr
#'
#'
#' @author      Nathan Matare <email: nmatare@chicagobooth.com>
#'
#' @examples
#' \dontrun{
#'M               <- replicate(10, rnorm(1000, 0, 1))
#'trials          <- ncol(M) # number of trials (e.g., parameters attempted)
#'observations    <- nrow(M) # number of observations (e.g., returns)
#'evaluation.function <- sharpe.ratio <-
#'    function(x, rf = 0.02 / 252) mean(x - rf) /  sd(x - rf)
#'
#' result <- dsr(
#'      N=observations,
#'      T=trials,
#'      observed.sharpe=
#' )

#'result
#'
#' }
#' @export

dsr <- function(N, T, observed.sharpe, variance.sharpe, skew, kurtosis){

    expected.sharpe=0L # the null hypothesis that strategies are not better than 0
   
    .expected.max <- function(mu=0, sigma =1, N){
        # Given a sample of IID random variables, the expected maximum of that 
        # sample can be approximated for a large N as:
        # *Extreme Value Theory 

        if(sigma < 0L || is.na(sigma))
            stop("assumptions require that sigma be greater than 0")

        if(is.na(N))
            stop("N must be a positive real number")

        if(N < 5L) # yes, 5 is artibratry 
            stop("N >> 1 is not satisfied")

        gamma <- -digamma(1) # E-M constant

        # Notes:
        # Mertens (2002) proves that the Normality assumption on
        # returns can be dropped, and still the estimated Sharpe
        # ratio converges to a Normal distribution with parameters, 
        # thus, __SR follows a Normal distribution, even if
        # the returns do not__ 

        # compute the expected maximum given a sample size of N and i.i.d assumptions
        max.n <- (1 - gamma) * qnorm(1 - (1 / N)) + gamma * qnorm(1 - (1 / N) * exp(-1))
        return(mu + sigma * max.n)
    }

    null.sharpe <- try(.expected.max(mu=expected.sharpe, sigma=variance.sharpe, N=N))
    if(inherits(null.sharpe, "try-error") || T == 0 || is.na(observed.sharpe)) 
        return(list(pvalue = NA_real_, dsr = NA_real_))

    # now compute PSR
    numerator   <- (observed.sharpe - null.sharpe) * sqrt(T - 1)
        adj.skew    <- 1L - skew * observed.sharpe
        adj.kurt    <- ((kurtosis - 1) / 4) * observed.sharpe ^ 2
    
    # this means that (adj.skew + adj.kurt) < 0, and sqrt(# < 0) == Nan
    if(adj.skew + adj.kurt < 0) 
        denominator <- 1 / 1e6 # will force 1 - pvalue to 1L
    else 
        denominator <- sqrt(adj.skew + adj.kurt)

    pvalue <- as.numeric(1L - pnorm(numerator / denominator))

    # on expectation, our deflated sharpe ratio is the probability of observing 
    # the expectation 0L * (1 - prob), and the observed.sharpe * (prob)
    deflated.sharpe <-  as.numeric(
        (pnorm(numerator / denominator) * observed.sharpe) + 
        (pvalue * expected.sharpe))
    return(list(pvalue = pvalue, dsr = deflated.sharpe))    
}