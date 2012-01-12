## Yang Lu Yang.Lu@williams.edu

## the summary method for brinson class

## The summary has three parts. First, the essential info about the
## portfolio; second, the exposures; third, the returns of both the
## portfolio and the benchmark, and the attribution.

setMethod("summary",
          signature(object = "brinson"),
          function(object,
                   ...){
            
            cat(sprintf("Period:                              %-s",
                        paste(unique(as.character(object@universe[[object@date.var]])),
                              collapse = ", ")), "\n",
                sprintf("Methodology:                         %-s",
                        paste("Brinson")), "\n",
                sprintf("Securities in the portfolio:         %-s",
                        length(which(object@universe[[object@portfolio.weight]] > 0))), "\n",
                sprintf("Securities in the universe:          %-s",
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

##############################################

## For brinsonMulti class
## The summary has three parts. First, the essential info about the
## portfolio; second, the exposures; third, the returns of both the
## portfolio and the benchmark, and the attribution.

setMethod("summary",
          signature(object = "brinsonMulti"),
          function(object,
                   ...){
            
            cat(sprintf("Period:                              %-s",
                        paste(c(min(unique(as.character(object@date.var))),
                                max(unique(as.character(object@date.var)))),
                              collapse = ", ")), "\n",
                sprintf("Methodology:                         %-s",
                        paste("Brinson")), "\n",
                sprintf("Avg securities in the portfolio:     %-s",
                        do.call(mean, lapply(1:length(object@date.var),
                                             function(i){length(which(object@universe[[i]]@universe[[object@portfolio.weight]] > 0))}))), "\n",
                sprintf("Avg securities in the universe:      %-s",
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




## summary for regression class object

## The summary has two parts. First, the essential info about the
## portfolio; second, the returns.

setMethod("summary",
          signature(object = "regression"),
          function(object,
                   ...){
            
            cat(sprintf("Period:                              %-s",
                        paste(unique(as.character(object@universe[[object@date.var]])),
                              collapse = ", ")), "\n",
                sprintf("Methodology:                         %-s",
                        paste("Regression")), "\n",
                sprintf("Securities in the portfolio:         %-s",
                        length(which(object@universe[[object@portfolio.weight]] > 0))), "\n",
                sprintf("Securities in the universe:          %-s",
                        nrow(object@universe)), "\n",
                sep = ""
                )
            
            cat("\n")
            
            cat("Returns", "\n")
            print(returns(object))
            cat("\n")
          }
          )


## summary for regressionMulti class object

## The summary has two parts. First, the essential info about the
## portfolio; second, the returns.

setMethod("summary",
          signature(object = "regressionMulti"),
          function(object,
                   ...){
            
            cat(sprintf("Period:                              %-s",
                        paste(c(min(unique(as.character(object@date.var))),
                                max(unique(as.character(object@date.var)))),
                              collapse = ", ")), "\n",
                sprintf("Methodology:                         %-s",
                        paste("Regression")), "\n",
                sprintf("Avg securities in the portfolio:     %-s",
                        do.call(mean, lapply(1:length(object@date.var),
                                             function(i){length(which(object@universe[[i]]@universe[[object@portfolio.weight]] > 0))}))), "\n",
                sprintf("Avg securities in the universe:      %-s",
                        do.call(mean, lapply(1:length(object@date.var),
                                             function(i){nrow(object@universe[[i]]@universe)}))), "\n",
                sep = ""
                )
            
            cat("\n")
                  
            cat("Returns", "\n")
            print(returns(object))
            cat("\n")
          }
          )



## summary for csvol class object

setMethod("summary",
          signature(object = "csvol"),
          function(object,
                   ...){
            
            cat(sprintf("Period:                              %-s",
                        paste(unique(as.character(object@universe[[object@date.var]])),
                              collapse = ", ")), "\n",
                sprintf("Methodology:                         %-s",
                        paste("Cross-sectional Volatility")), "\n",
                sprintf("Securities in the universe:          %-s",
                        nrow(object@universe)), "\n",
                sep = ""
                )
            
            cat("\n")
            
            cat("CSV", "\n")
            print(returns(object))
            cat("\n")
          }
          )


## summary for csvolMulti class object

setMethod("summary",
          signature(object = "csvolMulti"),
          function(object,
                   ...){
            
            cat(sprintf("Period:                              %-s",
                        paste(c(min(unique(as.character(object@date.var))),
                                max(unique(as.character(object@date.var)))),
                              collapse = ", ")), "\n",
                sprintf("Methodology:                         %-s",
                        paste("Cross-sectional Volatility")), "\n",
                sprintf("Avg securities in the universe:      %-s",
                        do.call(mean, lapply(1:length(object@date.var),
                                             function(i){nrow(object@universe[[i]]@universe)}))), "\n",
    
                sep = ""
                )
            
            cat("\n")
            
            cat("CSV", "\n")
            print(returns(object))
            cat("\n")
          }
          )
