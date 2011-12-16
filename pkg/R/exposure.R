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
