################################################################################
##
## $Id: portfolioMatch.R 1139 2008-08-29 14:22:48Z zhou $
##
## Methods for portfolioMatch object.
##
################################################################################

## Summarizes the matching.

setMethod("summary",
          signature(object = "portfolioMatch"),
          function(object,
                   ...){
            
            ## Header. Show the periods of the portfolio, matching
            ## variables, type of the portfolio (long only or long
            ## short), number of securities in the long, short, and
            ## entire portfolio, and universe, respectively, threshold
            ## set for random matching, and whether matching was done
            ## with replacement.

            cat(sprintf("Matching conducted with:"), "\n",
                sprintf("Periods:                                    %-s",
                        paste(unique(as.character(object@universe[[object@date.var]])),
                              collapse = ", ")), "\n",
                sprintf("match.var:                                  %-s",
                        paste(object@match.var,
                              collapse = ", ")), "\n",
                sprintf("Type of the matching:                       %-s",
                        ifelse(all(object@weights.orig >= 0),
                               "Long-only matching",
                               "Long-short matching")), "\n",
                sprintf("No. observations in the original portfolio: %-s",
                        length(which(object@weights.orig != 0))), "\n",
                sprintf("No. long securities:                        %-s",
                        length(which(object@weights.orig > 0))), "\n",
                sprintf("No. short securities:                       %-s",
                        length(which(object@weights.orig < 0))), "\n",
                sprintf("No. observations in the universe:           %-s",
                        nrow(object@universe)), "\n",
                sprintf("No. matches:                                %-s",
                        ncol(object@weights.match)), "\n",
                sprintf("Threshold:                                  %-s",
                        object@thresh), "\n",
                sprintf("With replacement:                           %-s",
                        object@replace), "\n\n",
                sep = ""
                )

            ## Exposures. Show the exposures of the original portfolio
            ## stored in the object.
            
            cat("Exposures of the original portfolio:", "\n")
            lapply(names(object@exposures.orig),
                   function(cur.var){
                     cat(cur.var, "\n")
                     show(object@exposures.orig[[cur.var]][[1]])
                     cat("\n")
                   })
            cat("\n")

            ## Show the mean exposures of the matched portfolios
            ## stored in the object. Use helper function .expo.comp to
            ## calculate the mean.
            
            cat("Mean exposures of the matched portfolios:", "\n")
            lapply(names(object@exposures.match),
                   function(cur.var){
                     cat(cur.var, "\n")
                     expo <- .expo.comp(object, cur.var, style = "lst")[, c(2,4,6)]
                     names(expo) <- c("long", "short", "total")
                     show(expo)
                     cat("\n")
                   })
            cat("\n")
            
            ## Returns. Show the returns of the original portfolio
            ## stored in the object.
            
            cat("Returns of the original portfolio:", "\n")
            show(object@return.orig[[1]])
            cat("\n\n")

            ## Show the mean returns of the matched portfolios stored
            ## in the object.
            
            cat("Mean returns of the matched portfolios:", "\n")
            show(data.frame(long = mean(sapply(object@return.match,
                              function(x) x$long)),
                            short = mean(sapply(object@return.match,
                              function(x) x$short)),
                            total = mean(sapply(object@return.match,
                              function(x) x$total))))
            cat("\n\n")

            ## Show the standard deviations of the returns of the
            ## matched portfolios stored in the object.
            
            cat("Standard deviations of returns of the matched portfolios:", "\n")
            show(data.frame(long = sd(sapply(object@return.match,
                              function(x) x$long)),
                            short = sd(sapply(object@return.match,
                              function(x) x$short)),
                            total = sd(sapply(object@return.match,
                              function(x) x$total))))
            cat("\n")
}
)

## Method that calculates the exposures of portfolios specified by var
## as a set of categories. Returns a list where each element of the
## list contains the exposures for an element of var. port specifies
## the portfolios to get exposures from, either the original one or
## the matched ones.

setMethod("exposures",
          signature(object = "portfolioMatch",
                    var = "character"),
          function(object,
                   var,
                   port = "match",
                   ...){
            
            ## If var is greater than 1, perform a recursive call of
            ## exposures and put the results of each element of var in
            ## a matrix with sapply.

            if(length(var) > 1)
              return(sapply(var, USE.NAMES = FALSE, function(cur.var){
                exposures(object, cur.var, port, ...)}))
            
            ## We convert the indices to a matrix instead of a vector,
            ## because apply will not work for a single vector.

            weights.calc <- as.matrix(switch(port,
                                             "orig" = object@weights.orig,
                                             "match" = object@weights.match))
            ind.calc     <- apply(weights.calc, 2, function(x) which(x != 0))

            ## ind.calc will be a list if matching allows for replacement.

            if(!is.list(ind.calc)){
              weights.calc <- sapply(1:ncol(ind.calc),
                                     function(i) weights.calc[ind.calc[,i],i])

              ## For each column in ind.calc, find the exposure by
              ## taking the sum of all weights for each factor in
              ## var. NOTE: this section of the code is only executed
              ## when var == 1

              temp <- list(lapply(1:ncol(ind.calc), function(i){
                data.frame(long  = .na.to.zero(tapply(.pos(weights.calc[,i]),
                             object@universe[[var]][ind.calc[,i]], sum)),
                           short = .na.to.zero(tapply(.neg(weights.calc[,i]),
                             object@universe[[var]][ind.calc[,i]], sum)),
                           total = .na.to.zero(tapply(weights.calc[,i],
                             object@universe[[var]][ind.calc[,i]], sum)))
              }))
              
            }else{
              weights.calc <- lapply(1:length(ind.calc),
                                     function(i) weights.calc[ind.calc[[i]],i])

              temp <- list(lapply(1:length(ind.calc), function(i){
                data.frame(long  = .na.to.zero(tapply(.pos(weights.calc[[i]]),
                             object@universe[[var]][ind.calc[[i]]], sum)),
                           short = .na.to.zero(tapply(.neg(weights.calc[[i]]),
                             object@universe[[var]][ind.calc[[i]]], sum)),
                           total = .na.to.zero(tapply(weights.calc[[i]],
                             object@universe[[var]][ind.calc[[i]]], sum)))
              }))
            }

            names(temp) <- var
            return(temp)
          }
          )

## Method that calculates the returns of portfolios. port specifies
## the portfolios to get exposures from, either the original one or
## the matched ones.

setMethod("returns",
          signature(object = "portfolioMatch"),
          function(object,
                   port = "match",
                   ...){

            ## We convert the indices to a matrix instead of a vector,
            ## because apply will not work for a single vector.

            weights.calc <- as.matrix(switch(port,
                                             "orig" = object@weights.orig,
                                             "match" = object@weights.match))
            ind.calc     <- apply(weights.calc, 2, function(x) which(x != 0))

            ## ind.calc will be a list if matching allows for replacement.

            if(!is.list(ind.calc)){
              weights.calc <- sapply(1:ncol(ind.calc),
                                     function(i) weights.calc[ind.calc[,i],i])

              ## For each column in ind.calc, find the total portfolio
              ## return by multiplying return by the weights

              ret <- lapply(1:ncol(ind.calc), function(i){
                data.frame(long  = sum(.pos(weights.calc[,i]) * object@universe[[object@ret.var]][ind.calc[,i]], na.rm = TRUE),
                           short = sum(.neg(weights.calc[,i]) * object@universe[[object@ret.var]][ind.calc[,i]], na.rm = TRUE),
                           total = sum(weights.calc[,i] * object@universe[[object@ret.var]][ind.calc[,i]], na.rm = TRUE))
              })
              
            }else{
              weights.calc <- sapply(1:length(ind.calc),
                                     function(i) weights.calc[ind.calc[[i]],i])

              ret <- lapply(1:length(ind.calc), function(i){
                data.frame(long  = sum(.pos(weights.calc[[i]]) * object@universe[[object@ret.var]][ind.calc[[i]]], na.rm = TRUE),
                           short = sum(.neg(weights.calc[[i]]) * object@universe[[object@ret.var]][ind.calc[[i]]], na.rm = TRUE),
                           total = sum(weights.calc[[i]] * object@universe[[object@ret.var]][ind.calc[[i]]], na.rm = TRUE))
              })
            }              

            return(ret)
          })


## Method that plots a comparson graph on original and matched
## portfolfios' exposures of one category.

setMethod("expo.plot",
          signature(object = "portfolioMatch",
                    var = "character"),
          function(object,
                   var,
                   col = NULL,
                   legend = FALSE,
                   legend.text = NULL,
                   ...){

            ## Check whether var exists in the object.

            stopifnot(var %in% names(object@exposures.match))
            
            ## Get exposures comparison from the object. Take the
            ## absolute value of the exposures if the original
            ## portfolio is long and short.
            
            e <- abs(.expo.comp(object, var, ...))

            ## Set legend texts.
            
            if (is.null(legend.text))
              legend.text <- if (ncol(e) == 4) {
                c("Original Long", "Matched Long", "Original Short", "Matched Short")
              } else if (ncol(e) == 2) {
                c("Original", "Matched")
              }

            ## Define colors.
            
            if (is.null(col)) {
              col.n <- if (ncol(e) == 4) {
                c(.75, .65, .35, .25)
              } else if (ncol(e) == 2) {
                c(.75, .25)
              }
              col <- gray(col.n)
            }

            ## Add legends if legend is TRUE.

            key <- NULL
            if (legend) {
              key <- c(simpleKey(legend.text,
                                 rectangles = TRUE, points = FALSE),
                       space = "right")
              key$rectangles$col <- rev(col)
            }

            ## Add a column specifying the name of each exposure
            ## segment.
            
            e.df <- cbind(Value = rownames(e), data.frame(e))

            ## Make the plot.
            
            print(barchart(.formula.make("Value", rev(names(e.df)[-1])), e.df,
                           xlab = "Exposure", ylab = var, stack = FALSE,
                           scales = list(relation = "free"),
                           col = col, key = key, ...))
          }
          )

## Helper function that compares exposures of original and matched
## portfolios. Returns a data frame comparing the exposures in an
## obvious way.

.expo.comp <- function(object,
                       var,
                       style = "l"){

  match <- object@exposures.match[[var]]
  
  ## Calculate the mean exposures of multiple matching. Attach
  ## original and match exposures.
  
  rst <- cbind(object@exposures.orig[[var]][[1]],
               apply(sapply(match, function(x) x$long), 1, mean),
               apply(sapply(match, function(x) x$short), 1, mean),
               apply(sapply(match, function(x) x$total), 1, mean))
  
  ## Choose columns for long-only, long-short, or long-short-total.
  
  switch(style,
         l   = rst[,c(1,4)],
         ls  = rst[,c(1,4,2,5)],
         lst = rst[,c(1,4,2,5,3,6)])
}

## Methods that plots a cumulative bias comparison graph on original
## and randomly matched portfolios' exposures of one category.

setMethod("rp.plot",
          signature(object = "portfolioMatch",
                    var = "character"),
          function(object,
                   var,
                   main = paste(var, "exposures of random portfolios"),
                   ...){
            
            ## Check whether var exists in the object.

            stopifnot(var %in% names(object@exposures.match))
            
            ## Check whether the number of matches is greater than 1.

            stopifnot(ncol(object@weights.match) > 1)
            
            ## Get exposures from the object.
            
            orig.expo   <- object@exposures.orig[[var]][[1]]$total
            match.expo  <- sapply(object@exposures.match[[var]], function(x) x$total)

            ## Calculate bias.
            
            match.bias  <- colSums(abs(match.expo - as.vector(orig.expo)))

            ## Sort original exposures.
            
            orig.expo.s <- sort(orig.expo, decreasing = TRUE)
            
            ## Make the plot.

            barplot(match.expo[order(orig.expo,  decreasing = TRUE),
                                     order(match.bias, decreasing = FALSE)],
                          col = gray((cumsum(orig.expo.s) - orig.expo.s[1] + .1) /
                            (1.2 - orig.expo.s[1])),
                          xlab = "Total Absolute Bias",
                          ylab = "Sector Exposure (cumulative)",
                          main = main,
                          yaxt = "n")

            axis(2, at = c(0, unique(round(cumsum(sort(orig.expo, decreasing = TRUE)), 2))))
            axis(1, at = seq(0, 120, length = 6),
                 labels = round(quantile(match.bias, seq(0, 1, by = .2)), 2))
            
          }
          )
          
## Methods that plots a distribution of returns on randomly matched
## portfolios and mark the return of the original portfolio.

setMethod("rp.hist",
          signature(object = "portfolioMatch"),
          function(object, adjust = 0, ...){ 
            
            ## Check whether the number of matches is greater than 1.

            stopifnot(ncol(object@weights.match) > 1)
            
            ## Get returns from the object.
            
            match.ret <- sapply(object@return.match, function(x) x$total)
            perf.port <- object@return.orig[[1]]$total

            ## Print histogram.
            
            print(histogram(~ I(match.ret + adjust), nint = 12, type = "count",
                            xlab = "Matched Portfolio Return", ylab = "Number",
                            main = "Distribution of Random Portfolios' Returns",
                            panel = function(...) {
                              panel.histogram(...)
                              llines(x = rep(perf.port, 2), y = c(-100, 100),
                                     lwd = 2)
                              ltext(x = perf.port, y = 15,
                                    labels = "Original Portfolio", adj = c(0.49, 1))
                            }))
          }
          )

.pos <- function(vec) vec * (vec > 0)
.neg <- function(vec) vec * (vec < 0)
  
## Replace NA values in vec with 0. NOTE: should this even be a
## function?

.na.to.zero <- function(vec){
  replace(vec, is.na(vec), 0)
}
