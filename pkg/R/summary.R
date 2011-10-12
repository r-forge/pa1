## the summary method for regression class

## Yang Lu Yang.Lu@williams.edu

## A method to display a summary for the regression class
## object. The summary has three parts. First, the essential info
## about regression; second, the exposures based on each matching
## variable; third, the returns of both the original, the matching
## portfolios, and their differences.

setMethod("summary",
          signature(object = "regression"),
          function(object,
                   ...){
            
            cat(sprintf("Name:                                       %-s",
                        paste(object@name)), "\n",
                sprintf("Period:                                     %-s",
                        paste(unique(as.character(object@universe[[object@date.var]])),
                              collapse = ", ")), "\n",
                sprintf("Methodology:                                %-s",
                        paste("Regression-Based")), "\n",
                sprintf("Securities in the original portfolio:       %-s",
                        length(which(object@weights != 0))), "\n",
                sprintf("Securities in the universe:                 %-s",
                        nrow(object@universe)), "\n",
                sep = ""
                )
            
            cat("\n")

            ## WHAT TO SHOW?
            
            cat("Exposures", "\n")
            print(exposures(object, object@match.var))
            cat("\n")
            
            cat("Returns", "\n")
            if(length(object@return.match) == 1){
              .rt <- do.call(cbind, lapply(1:3, function(i){returns(object)[[i]]}))
              .rt <- as.vector(.rt)
              .rt <- data.frame(orig = .rt[1], match = .rt[2],
                                diff = .rt[3])
            }   else {
              .rt <- returns(object)[4, ]
            }
            print(.rt)
            cat("\n")
          }
          )

## The show method displays the first part of the summary, which
## contains only the essential info about the matching.

setMethod("show",
          signature(object = "regression"),
          function(object){
            
            cat(sprintf("Name:                                       %-s",
                        paste(object@name)), "\n",
                sprintf("Period:                                     %-s",
                        paste(unique(as.character(object@universe[[object@date.var]])),
                              collapse = ", ")), "\n",
                sprintf("Methodology:                                %-s",
                        paste("Regression-Based")), "\n",
                sprintf("Securities in the original portfolio:       %-s",
                        length(which(object@weights != 0))), "\n",
                sprintf("Securities in the universe:                 %-s",
                        nrow(object@universe)), "\n",
                sep = ""
                )
            
            cat("\n")
          }
          )




############################################

## A method to display a summary for the regressiones class
## object. The summary has three parts. First, the essential info
## about regression; second, the exposures based on each matching
## variable; third, the returns of both the original, the matching
## portfolios, and their differences.

setMethod("summary",
          signature(object = "regressiones"),
          function(object,
                   ...){
            
            cat(sprintf("Name:                                       %-s",
                        paste(object@name)), "\n",
                sprintf("Periods (%-s", paste(length(object@date.var))),
                sprintf("):                                %-s",
                        paste(c(min(object@date.var), max(object@date.var)),
                              collapse = ", ")), "\n",
                sprintf("Methodology:                                %-s",
                        paste("Regression-Based")), "\n",
                sprintf("Avg securities in the original portfolio:    %-s",
                        do.call(mean, lapply(1:length(object@date.var),
                                             function(i){length(which(object@universe[[i]]@weights > 0))}))), "\n",
                sprintf("Avg securities in the universe:              %-s",
                        do.call(mean, lapply(1:length(object@date.var),
                                             function(i){
                                               nrow(object@universe[[i]]@universe)}))), "\n",
             
                sep = ""
                )
            
            cat("\n")

            ## WHAT TO SHOW?
            
            cat("Exposures", "\n")
            print(exposures(object, object@match.var))
            cat("\n")
            
            cat("Returns", "\n")
            if(length(object@return.match) == 1){
              .rt <- do.call(cbind, lapply(1:3, function(i){returns(object)[[i]]}))
              .rt <- as.vector(.rt)
              .rt <- data.frame(orig = .rt[1], match = .rt[2],
                                diff = .rt[3])
            }   else {
              .rt <- returns(object)[4, ]
            }
            print(.rt)
            cat("\n")
          }
          )

## The show method displays the first part of the summary, which
## contains only the essential info about the regression.

setMethod("show",
          signature(object = "regressiones"),
          function(object){
            
            cat(sprintf("Name:                                       %-s",
                        paste(object@name)), "\n",
                sprintf("Periods (%-s", paste(length(object@date.var))),
                sprintf("):                                %-s",
                        paste(c(min(object@date.var), max(object@date.var)),
                              collapse = ", ")), "\n",
                sprintf("Methodology:                                %-s",
                        paste("Regression-Based")), "\n",
                sprintf("Avg securities in the original portfolio:    %-s",
                        do.call(mean, lapply(1:length(object@date.var),
                                             function(i){length(which(object@universe[[i]]@weights > 0))}))), "\n",
                sprintf("Avg securities in the universe:              %-s",
                        do.call(mean, lapply(1:length(object@date.var),
                                             function(i){
                                               nrow(object@universe[[i]]@universe)}))), "\n",
                
                sep = ""
                )
            
            cat("\n")
          }
          )
