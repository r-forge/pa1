## Yang Lu Yang.Lu@williams.edu

## the summary method for brinson class

## The summary has three parts. First, the essential info about the
## portfolio; second, the exposures; third, the returns of both the
## portfolio and the benchmark, and the attribution.

setMethod("summary",
          signature(object = "brinson"),
          function(object,
                   ...){
            
            cat(sprintf("Period:                                     %-s",
                        paste(unique(as.character(object@universe[[object@date.var]])),
                              collapse = ", ")), "\n",
                sprintf("Methodology:                                %-s",
                        paste("Brinson")), "\n",
                sprintf("Securities in the original portfolio:       %-s",
                        length(which(object@universe[[object@portfolio.weight]] > 0))), "\n",
                sprintf("Securities in the universe:                 %-s",
                        nrow(object@universe)), "\n",
                sep = ""
                )
            
            cat("\n")
            
            cat("Exposures", "\n")
            print(exposure(object))
            cat("\n")

            cat("Returns", "\n")
            print(returns(object))
            cat("\n")
          }
          )

## The show method displays the first part of the summary, which
## contains only the essential info about the brinson model.

setMethod("show",
          signature(object = "brinson"),
          function(object){
            
            cat(sprintf("Period:                                     %-s",
                        paste(unique(as.character(object@universe[[object@date.var]])),
                              collapse = ", ")), "\n",
                sprintf("Methodology:                                %-s",
                        paste("Brinson")), "\n",
                sprintf("Securities in the original portfolio:       %-s",
                        length(which(object@universe[[object@portfolio.weight]] > 0))), "\n",
                sprintf("Securities in the universe:                 %-s",
                        nrow(object@universe)), "\n",
                sep = ""
                )
            
            cat("\n")
          }
          )




##############################################

## For brinsonMulti class
## The summary has three parts. First, the essential info about the
## portfolio; second, the exposures; third, the returns of both the
## portfolio and the benchmark, and the attribution.

setMethod("summary",
          signature(object = "brinsonMulti"),
          function(object,
                   ...){
            
            cat(sprintf("Period:                                     %-s",
                        paste(as.character(object@date.var),
                              collapse = ", ")), "\n",
                sprintf("Methodology:                                %-s",
                        paste("Brinson")), "\n",
                sprintf("Securities in the original portfolio:       %-s",
                        do.call(mean, lapply(1:length(object@date.var),
                                             function(i){length(which(object@universe[[i]]@universe[[object@portfolio.weight]] > 0))}))), "\n",
                sprintf("Securities in the universe:                 %-s",
                        do.call(mean, lapply(1:length(object@date.var),
                                             function(i){nrow(object@universe[[i]]@universe)}))), "\n",
                sep = ""
                )
            
            cat("\n")
            
            cat("Exposures", "\n")
            print(exposure(object))
            cat("\n")

            cat("Returns", "\n")
            print(returns(object))
            cat("\n")
          }
          )

## The show method displays the first part of the summary, which
## contains only the essential info about the brinson model.

setMethod("show",
          signature(object = "brinsonMulti"),
          function(object){
            
            cat(sprintf("Period:                                     %-s",
                        paste(as.character(object@date.var),
                              collapse = ", ")), "\n",
                sprintf("Methodology:                                %-s",
                        paste("Brinson")), "\n",
                sprintf("Securities in the original portfolio:       %-s",
                        do.call(mean, lapply(1:length(object@date.var),
                                             function(i){length(which(object@universe[[i]]@universe[[object@portfolio.weight]] > 0))}))), "\n",
                sprintf("Securities in the universe:                 %-s",
                        do.call(mean, lapply(1:length(object@date.var),
                                             function(i){nrow(object@universe[[i]]@universe)}))), "\n",
                sep = ""
                )
            
            cat("\n")
          }
          )

