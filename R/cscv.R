#'
#' @title 
#' Combinatorially Symmetric Cross-validation (CSCV)
#'
#' @description 
#' Performs combinatorial symmetric cross-validation 
#' given a true matrix 'M' and 'S' number of composite sub-matrices.
#'
#' @details 
#' 'cscv' performs the CSCV algorithim as detailed by Bailey et al (2015) 
#' The Probability of Backtest Over-fitting. Given a true matrix 'M', cscv will 
#' (1) split 'M' into 'S' number of sub-matrices, (2) form all sub-matrix
#' combinations taken in groups of size S/2, and (3) perform CSCV given an 
#' evaluation function, 'FUN'.
#'
#' @param M A true matrix where columns are the number of trials 
#' 			(i.e., corresponding model parameters), and rows are the number 
#'			of observations (e.g., returns).
#'
#' @param S An even number corresponding to the number of 'M' sub-matrices to
#' 			be formed for the training(J) and testing(Jbar) sets; default 16. 
#'
#' @param FUN 	A function that evaluates a vector of observations (e.g., 
#'				FUN = function(x, rf = 0.02 / 252) mean(x - rf) /  sd(x - rf)).		
#'
#' @param parallel Whether to compute in parallel; default TRUE.
#'
#' @param digits The number of reported digits; default 3.
#'
#' @param relax In the original implementation, Bailey et al (2015) 
#' 				restrict 'S' to evenly divide 'M'. If relax is set to TRUE, one 
#'				can choose an 'S' that does not evenly divide M while ensuring 
#'				that the left-over splits are appropriately distributed among
#'				the training sets J and testing sets Jbar.
#'
#' @return
#' 
#' a list of class 'cscv' containing:
#' \describe{  
#' \item{cumdistf_Rbar_rank:}{The cumulative distribution function over all 
#'                            strategies.}
#'
#' \item{cumdistf_Rbar_all:}{ The cumulative distribution function over 
#'                            optimized(ranked) strategies.}
#'
#' \item{pairs:}{             S!/[(S-S/2)!*(S/2)!] number of combinations
#'                            (rows) including the performance of the chosen 
#'                            IS trial(R) and of the chosen OOS trial (Rbar), 
#'                            with lambda transformed via the logit function.}
#'
#' \item{insample_neg:}{      The proportion of negatively chosen IS models.}
#'
#' \item{outsample_neg:}{     Described as probability loss, or the  
#'                            probability that the model selected as optimal IS
#'                            will deliver a loss OOS.}
#'
#' \item{num_submatrices:}{   The number of formed sub-matrices.}
#'
#' \item{beta:}{              The slope of IS to OOS performance degradation.}
#'
#' \item{phi:}{               The probability of backtest overfit.}
#' }
#'
#' @references Bailey et al (2015) "The Probability of Backtest Overfitting" 
#' \url{https://papers.ssrn.com/sol3/papers.cfm?abstract_id=2326253}
#'
#' @keywords    cscv
#'
#' @seealso     plot.cscv(), summary.cscv()
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

cscv <- function(M, S = 16, FUN, digits = 3L, parallel = FALSE, relax = TRUE){

    stopifnot(is.function(FUN))
    apply.FUN <- function(x) as.vector(apply(x, 2, FUN)) # wrapper 

    M <- as.matrix(M) # (1) first, we form M
    # (ii) M is a true Matrix and FUN can be evaluated on M
    stopifnot(!any(is.na(M))) 

    T <- nrow(M) # observations
    N <- ncol(M) # strategies (parameters)
    S <- as.integer(S)

    stopifnot(S %% 2 == 0) # (2) S must be even 
    if(!relax) # (2) S of disjoint sub-matrices of equal dimensions 
        stopifnot(T %% S == 0) 

    if(!T > 2 * N) 
        warning(
            "Bailey (2015) et al suggest that T (observations) \n",
            "should be 2x the number of parameters (N), due to \n",
            "the fact that CSCV compares combinations of T/2 \n",
            "trials to their complements."
        )

    if(hasArg(parallel) && parallel && parallel::detectCores() != 1)
        `%proc%` <- foreach::`%dopar%`
    else 
        `%proc%` <- foreach::`%do%`

    # (2) partition M across rows 
    groups  <- rep(1:S, times = rep(T / S, S)) # distribute residuals 
    resids  <- if(T - length(groups) > 0) rep(1:(T - length(groups))) else NULL
    Ms      <- split.data.frame(M, sort(c(groups, resids)))

    # (2) each Ms is of order T / S * N
    if(!relax)
        stopifnot(all(sapply(Ms, function(x) length(x) == T / S * N)))

    # (3) we form all combinations Cs of Ms
    Cs <- utils::combn(S, S / 2, simplify = FALSE)

    # (3) each combination c ∈ C S is composed of S/2 submatrices Ms 
    if(!relax)
        stopifnot(all(sapply(Cs, function(x) length(x) == S / 2)))  
    
    # (4)  form the training set J
    # (4a) form training set J, by joining the S/2 submatrices M s that 
    #      constitute c in their original order          
    # (4b) form the testing set J, as the complement of J in M
    combos <- list()
    for(i in seq(Cs))
        combos[[i]] <- list(
            J       = Ms[Cs[[i]]], 
            Jbar    = Ms[which(!seq(S) %in% Cs[[i]])]
        )
    
	results <- foreach::foreach(
        combo       = combos, 
        .inorder    = FALSE
    ) %PROC% {

        # (4a) J is a matrix of order (T /S)(S/2) × N ) = T /2 × N    
        # stopifnot(length(combo$J) == ((T / S) * (S / 2) * N))    
       
        # (4c) form a vector R c of performance statistics of order N
        R       <- apply.FUN(do.call(rbind, combo$J))
        Rbar    <- apply.FUN(do.call(rbind, combo$Jbar))

        # (4d) As before rank of the components of Ris denoted by rc 
        # the IS ranking of the N strategies.
        # r       <- rank(R)
        rbar    <- rank(Rbar)

        # (4e) Determine the element n ∗ such that r n c ∗ ∈ Ω ∗ n ∗
        Nstar    <- which.max(R) 
        # Nstarbar <- which.max(Rbar)

        # (4f) Define the relative rank of r̄ n c ∗
        omegabar <- rbar[Nstar] / (N + 1) # note +1

        # (4g) We define the logit λ c = ln (1−ω)
        lambda <- log(omegabar / (1 - omegabar)) # high lambda == low over-fit

        # return values
        result <- list(
            R       = R, 
            Rbar    = Rbar, 
            Nstar   = Nstar, 
            lambda  = lambda
        )
        return(result)
    }
    
    # (5) compute the distribution of ranks OOS
    pairs <- t(sapply(results, function(x) 
        c(x$R[[x$Nstar]], x$Rbar[[x$Nstar]], x$lambda)))
    colnames(pairs) <- c("R", "Rbar", "lambda") 

    # Inf/-Inf occurs when FUN returns Inf and cscv cannot rank or 'find'
    # appropiate pair
    data    <- pairs[is.finite(rowSums(pairs)), ]
    if(length(data) == 0)
        stop("Algorithm could not correctly rank appropriate pairs; change 'FUN'
             or increase/decrease 'S'")

    # compute distributions
    # It can be verified visually by checking that the cumulative distribution 
    # function of RbarR is not above the cumdist function of R for all outcomes
    # (worse); one would prefer the criterion used to produce RbarR vs random
    # sampling of Rbar
    distRbarR <- ecdf(pairs[,"Rbar"]) # optimized
    distRbar  <- ecdf(do.call(rbind, lapply(results, function(x) x$Rbar)))

    # (5) Define relative frequency
    rel_freq <- sum(as.integer(pairs[ ,"lambda"] <= 0L)) 
    phi      <- signif(rel_freq / length(Cs), digits)

    # fit linear: An intuitive explanation for this negative
    # slope is that overfit backtests minimize future performance    
    # linear  <- lm(R ~ Rbar, as.data.frame(data))
    # alpha   <- signif(linear$coefficients["(Intercept)"], digits)
    # beta    <- signif(linear$coefficients["Rbar"], digits)
    beta <- cov(data[ ,"Rbar"], data[ ,"R"]) / var(data[ ,"Rbar"]) # same+faster

    # A particularly useful statistic is the proportion of combinations 
    # with negative performance,
    insample_neg  <- signif(
        length(which(pairs[ ,"R"] < 0L)) / length(Cs), digits)
    outsample_neg <- signif(
        length(which(pairs[ ,"Rbar"] < 0L)) / length(Cs), digits)

    cscv <- list(
        # model               = linear,
        # alpha               = as.numeric(alpha),
        cumdistf_Rbar_rank  = distRbarR,
        cumdistf_Rbar_all   = distRbar,
        pairs               = pairs,
        insample_neg        = insample_neg,
        outsample_neg       = outsample_neg,
        num_submatrices     = as.integer(S),
        beta                = as.numeric(beta),
        phi                 = phi
    )
    class(cscv) <- "cscv"
    return(cscv)
}

#' Graphical analysis of CSCV results

#' This function supports the plotting of multiple cscv runs viewed in a
#' aggregate.

#' cscv.plot() will return:
#' (1)  the frequency distribution of the rank logits as described in Bailey 
#'      et al (2015);
#' (2)  the out-of-sample performance degradation when compared to in-sample
#'      results given a previously defined evaluation function; 
#' (3)  the first and second order plots of stochastic dominance present among the
#'      attempted trials.
#'
#' The legend includes the attributed 'phi' and 'beta' which
#' correspond to the probability of overfit and the slope of OOS performance 
#' degradation line, respectively.

#' @param cscv.objects  A list containing the results of a particular cscv run. 
#'                      This plotting supports multiple cscv runs so one may 
#'                      view the results in aggregate.

#' @param summarize     Whether to summarize the R/Rbar pairs. Helpful when 
#'                      S >> 16 and one has many pairs across many runs; 
#'                      default 50.      

#' @keywords cscv
#' @seealso cscv
#' @examples
#' @export

plot.cscv <- function(cscv.objects, summarize = 50){

    stopifnot(is.list(cscv.objects))

    if(isTRUE(summarize) && !is.numeric(summarize)) 
        stop("Please specify number of samples or set to FALSE")

    if(!exists("titles") || is.null(titles) || titles == 'none')
        titles <- names(cscv.objects)

    if(length(titles) == 0)
        stop("You must name each cscv result")

    long <- do.call(rbind, lapply(titles, # convert to 'long' format
        function(x) 
            as.data.table(
                cbind.data.frame(
                name                = x, 
                phi                 = cscv.objects[[x]]$phi,
                beta                = cscv.objects[[x]]$beta,
                outsample_neg       = cscv.objects[[x]]$outsample_neg,
                cscv.objects[[x]]$pairs
        ))))

    for(j in 1:ncol(long)) 
        set(long, which(is.infinite(long[[j]])), j, NA)

    # make legend names
    labels <- long[ ,unique(paste(name, 
                     signif(as.numeric(phi), 2), 
                     signif(as.numeric(beta), 2)),
                     sep = "| ")]   

    # frequency historgram 
    histo_plot <- ggplot(
            data = long,
            aes(
                x       = lambda,
                color   = name),
            ) +
            geom_freqpoly(binwidth  = 0.7) +
            scale_fill_discrete(labels = labels, l = 40) +
            labs(
                color   = paste0("Name |", "Phi", "| Beta"),
                title   = "Histogram of Rank Logits",
                x       = "Logits",
                y       = "Denisty"
            ) +
            annotate("text", x =  1, y = -7, label = "Less overfit") +
            annotate("text", x = -1, y = -7, label = "More overfit") +
            geom_vline(
                xintercept  = 0L, 
                linetype    = "dotted", 
                color       = 'darkgrey'
            ) +
            scale_x_continuous(
                limits = c(-4L, 4L), 
                breaks = seq(-4L, 4L, 2L)
            ) +
        theme_minimal()

    .get_legend <- function(gplot){
        tmp     <- ggplot_gtable(ggplot_build(gplot)) 
        leg     <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box") 
        legend  <- tmp$grobs[[leg]] 
        return(legend)
    } 

    legend <- .get_legend(histo_plot) # grab and store the legend

    # stohastic dominance
    sto_dom <- list()
    titles  <- as.character(unique(long[["name"]]))
    for(title in titles){

        optimized   <- long[name == title, Rbar]
        optimized   <- optimized[is.finite(optimized)]
        rng         <- seq(min(optimized), max(optimized), 0.01)
        fitfun_optm <- ecdf(optimized)
        fitfun_non  <- cscv.objects[[title]]$cumdistf_Rbar_all
        
        wide <- data.table(
            name        = title,
            performance = rng,
            optimized   = sort(fitfun_optm(rng)), 
            naive       = sort(fitfun_non(rng))
        )[ ,second_order := naive - optimized][]

        long2 <- melt(wide, 
            id.vars      = c("performance"), 
            measure.vars = c("optimized", "naive", "second_order")
        )

        sto_dom[[title]]$data <- long2
        sto_dom[[title]]$plot <- ggplot(data = long2, 
            aes(x       = performance, 
                y       = value, 
                group   = variable, 
                color   = variable
            )) +
            geom_line() +                   
            scale_color_manual(
                values = c(
                    "optimized"     = "blueviolet", 
                    "naive"         = "red3",
                    "second_order"  = "green4"
            ))  + 
            geom_hline(
                yintercept  = 0L, 
                linetype    = "dotted", 
                color       = 'red'
            ) +                     
            labs(
                variable = "Derivative",                            
                title    = paste0(
                    unique(wide[["name"]])),
                x        = "Performance",
                y        = "Frequency"
            ) +     
        theme_minimal()
    }

    # average stohastic dominance of strategy 
    average_sto <- do.call(rbind, lapply(sto_dom, function(x) x$data))

    avg_sto_plot <- 
            ggplot(data = average_sto, 
                aes(x       = performance, 
                    y       = value, 
                    group   = variable, 
                    color   = variable
                )) +
                # geom_line() + 
                stat_smooth(
                    method  = "lm", 
                    formula = y ~ poly(x, 4), 
                    se      = TRUE,
                    size    = 1
                ) +             
                scale_color_manual(
                    values = c(
                        "optimized"     = "blueviolet", 
                        "naive"         = "red3",
                        "second_order"  = "green4"
                ))  + 
                geom_hline(
                    yintercept  = 0L, 
                    linetype    = "dotted", 
                    color       = 'red'
                ) +                     
                labs(
                    variable = "Derivative",                            
                    title    = "Average Stohastic Dominance",
                    x        = "Performance",
                    y        = "Frequency"
                ) +     
    theme_minimal()

    if(!isTRUE(summarize) && is.numeric(summarize))
        long <- long[ ,.SD[sample(.N, summarize, replace = TRUE)], by = name]

    # OOS loss
    oos_loss <- ggplot(
            data = long, aes(x = Rbar, y = R)) + 
            geom_point(shape = 2, 
                aes(color = long[["name"]])) +
            scale_color_discrete(labels = labels) +
            xlim(
                quantile(long[["Rbar"]], 0.005, na.rm = TRUE), 
                quantile(long[["Rbar"]], 0.995, na.rm = TRUE))      
            if(summarize)
                oos_loss <- oos_loss + geom_smooth(
                    method  = "lm", 
                    formula = y ~ x,
                    se      = FALSE,
                    lwd     = 0.4,
                    aes(color = long[["name"]])
                )
            oos_loss <- oos_loss + geom_vline(
                xintercept  = 0L, 
                linetype    = "dotted", 
                color       = 'black'
            ) +
            geom_hline(
                yintercept  = 0L, 
                linetype    = "dotted", 
                color       = 'black'
            ) +
            labs(
                color   = paste0("Name |", "Phi", "| Beta"),
                title    = "OOS Performance Degradation",
                x        = "Performance (Out of Sample)",
                y        = "Performance (In sample)"
            ) +                     
        theme_minimal()

    .arrange_sto_dom <- function(plots){

        require(ggpubr)
        first_legend <- .get_legend(plots[[1]])
        plots    <- lapply(plots, # remove the legend
            function(x) x + theme(legend.position = 'none'))

        multi_page <- ggpubr::ggarrange(
            plotlist    = plots, 
            nrow        = 2, 
            ncol        = 3
        )
        return(multi_page)
    }

    return(
        list(
            legend              = legend,
            stohastic_dom       = 
                .arrange_sto_dom(lapply(sto_dom, function(x) x$plot)),
            mean_stohastic_dom  = avg_sto_plot,
            oos_loss            = oos_loss + theme(legend.position = 'none'),
            logit_freq          = histo_plot + theme(legend.position = 'none')
        )
    )           
}

summarize.cscv <- function(cscv.objects){

    .integrate_sto_dom <- function(output){

        optimized   <- output$pairs[ ,"Rbar"]
        optimized   <- optimized[is.finite(optimized)]
        rng         <- seq(min(optimized), max(optimized), 0.01)
        fitfun_optm <- ecdf(optimized)
        fitfun_non  <- output$cumdistf_Rbar_all

        # integrate areas and subtract // positive means optm is lower than
        # naive so good, negative means optm is above so bad

        area <- try({
                    integrate(fitfun_non, 
                     lower = 0L, upper = 1L, subdivisions = 10e4)$value - 
                    integrate(fitfun_optm, 
                         lower = 0L, upper = 1L, subdivisions = 10e4)$value
                }, silent = TRUE)

        if(inherits(area, "try-error"))
            area = NA_real_

        return(area)
    }

    summary_table <- as.data.table(do.call(rbind, 
        lapply(names(cscv.objects), function(x) 
            if(!is.null(cscv.objects[[x]]))
                data.frame( 
                    Name                = x,
                    Phi                 = cscv.objects[[x]]$phi, 
                    Beta                = cscv.objects[[x]]$beta, 
                    OOS.Negative        = cscv.objects[[x]]$outsample_neg, 
                    Stohastic.Dominance = .integrate_sto_dom(cscv.objects[[x]])
                )
            else NULL
            )))

    return(summary_table)
}