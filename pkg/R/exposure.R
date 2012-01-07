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

            ## decide whether it's categorical or continuous (split
            ## into 5 quantiles)
            if (class(object@universe[[var]]) != "numeric"){
              ## categorical
              expo.mat <- cbind(tapply(object@universe[[object@portfolio.weight]],
                                       object@universe[[var]],
                                       sum),
                                tapply(object@universe[[object@benchmark.weight]],
                                       object@universe[[var]],
                                       sum))
              
              colnames(expo.mat) <- c("Portfolio", "Benchmark")
              return(expo.mat)
            } else {

              ## continuous (5 quantiles)
              temp <- object@universe
              temp$q <- as.factor(ceiling(rank(temp[[var]]) / nrow(temp) * 5))
              expo.mat <- cbind(tapply(temp[[object@portfolio.weight]],
                                       temp$q,
                                       sum),
                                tapply(temp[[object@benchmark.weight]],
                                       temp$q,
                                       sum))
              colnames(expo.mat) <- c("Portfolio", "Benchmark")
              rownames(expo.mat)[1] <- "low"
              rownames(expo.mat)[5] <- "high"
              return(expo.mat)
            }
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

            if (class(object@universe[[var]]) != "numeric"){
              ## categorical
              expo.list <- list()
              no.date <- length(object@date.var)
              port.mat <- sapply(1:no.date,
                                 function(i){tapply(object@universe[[i]]@universe[[object@portfolio.weight]],
                                                    object@universe[[i]]@universe[[var]],
                                                    sum)})
              colnames(port.mat) <- object@date.var
              expo.list[[1]] <- port.mat
              
              bench.mat <- sapply(1:no.date, function(i){tapply(object@universe[[i]]@universe[[object@benchmark.weight]],
                                                          object@universe[[i]]@universe[[var]],
                                                          sum)})
              colnames(bench.mat) <- object@date.var
              expo.list[[2]] <- bench.mat
              names(expo.list) <- c("Portfolio", "Benchmark")
              return(expo.list)
            } else {
              ## continous (5 quantiles)

              expo.list <- list()
              no.date <- length(object@date.var)

              for (i in 1:no.date){
                object@universe[[i]]@universe$q <- as.factor(ceiling(rank(object@universe[[i]]@universe[[var]]) / nrow(object@universe[[i]]@universe) * 5))
              }

              port.mat <- sapply(1:no.date,
                                 function(i){tapply(object@universe[[i]]@universe[[object@portfolio.weight]],
                                                    object@universe[[i]]@universe$q,
                                                    sum)})
              colnames(port.mat) <- object@date.var
              rownames(port.mat)[1] <- "low"
              rownames(port.mat)[5] <- "high"
              expo.list[[1]] <- port.mat
              
              bench.mat <- sapply(1:no.date,
                                  function(i){tapply(object@universe[[i]]@universe[[object@benchmark.weight]],
                                                     object@universe[[i]]@universe$q,
                                                     sum)})
              
              
              colnames(bench.mat) <- object@date.var
              rownames(bench.mat)[1] <- "low"
              rownames(bench.mat)[5] <- "high"
              expo.list[[2]] <- bench.mat
              names(expo.list) <- c("Portfolio", "Benchmark")
              return(expo.list)
            }
          }
          )
