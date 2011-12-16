## show method
## Yang Lu Yang.Lu@williams.edu

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

