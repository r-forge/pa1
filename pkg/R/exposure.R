## Yang Lu Yang.Lu@williams.edu

## exposure method for brinson class
## post: matrix

setMethod("exposure",
          signature(object = "brinson"),
          function(object,
                   ...){
            
            ## round to certain digits
            options(digits = 3)
            
            expo.mat <- cbind(object@weight.port,
                              object@weight.bench)

            colnames(expo.mat) <- c("Portfolio", "Benchmark")
            return(expo.mat)
          }
        )

## exposure method for brinsonMulti class
## post: list

setMethod("exposure",
          signature(object = "brinsonMulti"),
          function(object,
                   ...){
            
            ## round to certain digits
            options(digits = 3)

            expo.list <- list()
            ## portfolio exposures
            expo.list[[1]] <- object@weight.port

            ## benchmark exposures
            expo.list[[2]] <- object@weight.bench
            
            names(expo.list) <- c("Portfolio", "Benchmark")
            return(expo.list)
          }
          )


## exposure based on regression class object

setMethod("exposure",
          signature(object = "regression"),
          function(object,
                   var,
                   ...){
            
            ## round to certain digits
            options(digits = 3)
            expo.mat <- cbind(tapply(object@universe[[object@portfolio.weight]],
                                     object@universe[[var]],
                                     sum),
                              tapply(object@universe[[object@benchmark.weight]],
                                     object@universe[[var]],
                                     sum))
            
            colnames(expo.mat) <- c("Portfolio", "Benchmark")
            return(expo.mat)
          }
        )



## regressionMulti exposure


setMethod("exposure",
          signature(object = "regressionMulti"),
          function(object,
                   var,
                   ...){
            
            ## round to certain digits
            options(digits = 3)
            expo.list <- list()
            
            port.mat <- sapply(1:2,
                               function(i){tapply(object@universe[[i]]@universe[[object@portfolio.weight]],
                                                  object@universe[[i]]@universe[[var]],
                                                  sum)})
            colnames(port.mat) <- object@date.var
            expo.list[[1]] <- port.mat
            
            bench.mat <- sapply(1:2, function(i){tapply(object@universe[[i]]@universe[[object@benchmark.weight]],
                                                        object@universe[[i]]@universe[[var]],
                                                        sum)})
            colnames(bench.mat) <- object@date.var
            expo.list[[2]] <- bench.mat
            names(expo.list) <- c("Portfolio", "Benchmark")
            return(expo.list)
          }
        )
