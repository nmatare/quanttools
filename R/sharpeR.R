#'
#' @title 
#' Defalted Sharpe Ratio (DSR)
#'
#' @description 
#' Computes the deflated sharpe ratio vis-a-vis Extreme Value Theory 
#' 
#'
#' @details 
#' The 'cscv' performs the CSCV algorithim as detailed by Bailey et al (2015) 
#' The Probability of Backtest Overfitting. Given a true matrix 'M', cscv will 
#' (1) split 'M' into 'S' number of sub-matrices, (2) form all sub-matrix
#' combinations taken in groups of size S/2, and (3) perform CSCV given an 
#' evaluation function, 'FUN'.
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
#' \item{pvalue:}{The cumulative distribution function over all 
#'                            strategies.}
#'
#' \item{dsr:}{ The cumulative distribution function over 
#'                            optimized(ranked) strategies.}
#'
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
#'S               <- 6L # number of sub matrices
#'trials          <- ncol(M) # number of models (parameters)
#'observations    <- nrow(M) # number of observations (e.g., returns)
#'evaluation.function <- sharpe.ratio <-
#'    function(x, rf = 0.02 / 252) mean(x - rf) /  sd(x - rf)
#'
#'result          <- cscv(
#'                      M           = M, 
#'                      S           = S, 
#'                      FUN         = evaluation.function, 
#'                      parallel    = FALSE
#' )
#'result
#'
#' }
#' @export

dsr <- function(N, T, observed.sharpe, variance.sharpe, skew, kurtosis){

    # Computes the probablistic sharpe ratio given extreme value theory and outputs
    # the pvalue and associated deflated sharpe ratio
    # observed.sharpe: the observed sharpe ratio calc'ed from observations(returns)
    # variance.sharpe: the observed variance across sharpe ratios from trials
    # skew:            the skew of observations(returns)
    # kurtosis:        the kurtosis of observations(returns)
    # N: number of trials (parameters)
    # T: number of observations (returns)
    # ** Note function does not take into account the periodicty of one's
    # observed sharpe ratio

    expected.sharpe = 0L 
    # that is, the null hypothesis is that strategies are not better than 0
    .expected.max <- function(mu = 0, sigma = 1, N){
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

    null.sharpe <- try(.expected.max(
        mu      = expected.sharpe, 
        sigma   = variance.sharpe, 
        N       = N
    ), silent = TRUE)

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

    # on expectation, our deflated sharpe ratio is the probabilty of observing 
    # the expectation 0L * (1 - prob), and the observed.sharpe * (prob)
    deflated.sharpe <-  as.numeric(
        (pnorm(numerator / denominator) * observed.sharpe) + 
        (pvalue * expected.sharpe)
    )

    return(list(pvalue = pvalue, dsr = deflated.sharpe))    
}