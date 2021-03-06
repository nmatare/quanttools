% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cscv.R
\name{cscv}
\alias{cscv}
\title{Combinatorially Symmetric Cross-validation (CSCV)}
\usage{
cscv(M, S = 16, FUN, digits = 3L, parallel = FALSE, relax = TRUE)
}
\arguments{
\item{M}{A true matrix where columns are the number of trials 
    (i.e., corresponding model parameters), and rows are the number 
of observations (e.g., returns).}

\item{S}{An even number corresponding to the number of 'M' sub-matrices to
be formed for the training(J) and testing(Jbar) sets; default 16.}

\item{FUN}{A function that evaluates a vector of observations (e.g., 
FUN = function(x, rf = 0.02 / 252) mean(x - rf) /  sd(x - rf)).}

\item{digits}{The number of reported digits; default 3.}

\item{parallel}{Whether to compute in parallel; default TRUE.}

\item{relax}{In the original implementation, Bailey et al (2015) 
    restrict 'S' to evenly divide 'M'. If relax is set to TRUE, one 
can choose an 'S' that does not evenly divide M while ensuring 
that the left-over splits are appropriately distributed among
the training sets J and testing sets Jbar.}
}
\value{
a list of class 'cscv' containing:
\describe{  
\item{cumdistf_Rbar_rank:}{The cumulative distribution function over all 
                           strategies.}

\item{cumdistf_Rbar_all:}{ The cumulative distribution function over 
                           optimized(ranked) strategies.}

\item{pairs:}{             S!/[(S-S/2)!*(S/2)!] number of combinations
                           (rows) including the performance of the chosen 
                           IS trial(R) and of the chosen OOS trial (Rbar), 
                           with lambda transformed via the logit function.}

\item{insample_neg:}{      The proportion of negatively chosen IS models.}

\item{outsample_neg:}{     Described as probability loss, or the  
                           probability that the model selected as optimal IS
                           will deliver a loss OOS.}

\item{num_submatrices:}{   The number of formed sub-matrices.}

\item{beta:}{              The slope of IS to OOS performance degradation.}

\item{phi:}{               The probability of backtest overfit.}
}
}
\description{
Performs combinatorial symmetric cross-validation 
given a true matrix 'M' and 'S' number of composite sub-matrices.
}
\details{
'cscv' performs the CSCV algorithim as detailed by Bailey et al (2015) 
The Probability of Backtest Over-fitting. Given a true matrix 'M', cscv will 
(1) split 'M' into 'S' number of sub-matrices, (2) form all sub-matrix
combinations taken in groups of size S/2, and (3) perform CSCV given an 
evaluation function, 'FUN'.
}
\examples{
\dontrun{
M               <- replicate(10, rnorm(1000, 0, 1))
S               <- 6L # number of sub matrices
trials          <- ncol(M) # number of models (parameters)
observations    <- nrow(M) # number of observations (e.g., returns)
evaluation.function <- sharpe.ratio <-
   function(x, rf = 0.02 / 252) mean(x - rf) /  sd(x - rf)

result          <- cscv(
                     M           = M, 
                     S           = S, 
                     FUN         = evaluation.function, 
                     parallel    = FALSE
)
result

}
}
\references{
Bailey et al (2015) "The Probability of Backtest Overfitting" 
\url{https://papers.ssrn.com/sol3/papers.cfm?abstract_id=2326253}
}
\seealso{
plot.cscv(), summary.cscv()
}
\author{
Nathan Matare <email: nmatare@chicagobooth.com>
}
\keyword{cscv}
