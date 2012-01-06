## Yang Lu Yang.Lu@williams.edu

## returns method for brinson class

setMethod("returns",
          signature(object = "brinson"),
          function(object,
                   ...){
            
            ## round to certain digits
            options(digits = 3)

            q1 <- object@q1
            q2 <- object@q2
            q3 <- object@q3
            q4 <- object@q4
            
            asset.allocation <- q2 - q1
            stock.selection <- q3 - q1
            interaction <- q4 - q3 - q2 + q1
            active.ret <- q4 - q1

            ret.mat <- matrix(NA, nrow = 4, ncol = 1)
            ret.mat[1, 1] <- asset.allocation
            ret.mat[2, 1] <- stock.selection
            ret.mat[3, 1] <- interaction
            ret.mat[4, 1] <- active.ret

            colnames(ret.mat) <- as.character(unique(object@universe[[object@date.var]]))
            rownames(ret.mat) <- c("Allocation Effect",
                                   "Selection Effect",
                                   "Interaction Effect",
                                   "Active Return")
            return(ret.mat)
          }
          )

## returns method for brinsonMulti class

setMethod("returns",
          signature(object = "brinsonMulti"),
          function(object,
                   ...){
            
            ## round to certain digits
            options(digits = 3)

            temp.mat <- apply(object@brinson.mat + 1, 1, prod) - 1
            
            asset.allocation <- temp.mat[3] - temp.mat[4] ## q2 - q1
            stock.selection <- temp.mat[2] - temp.mat[4] ## q3 - q1

            ## q4 - q3 - q2 + q1
            interaction <- temp.mat[1] - temp.mat[2] - temp.mat[3] + temp.mat[4]

            active.ret <- temp.mat[1] - temp.mat[4] ## q4 - q1

            ret.mat <- matrix(NA, nrow = 4, ncol = 1)
            ret.mat[1, 1] <- asset.allocation
            ret.mat[2, 1] <- stock.selection
            ret.mat[3, 1] <- interaction
            ret.mat[4, 1] <- active.ret
            
            colnames(ret.mat) <- paste(c(min(unique(as.character(object@date.var))),
                                         max(unique(as.character(object@date.var)))),
                                       collapse = ", ")
            rownames(ret.mat) <- c("Allocation Effect",
                                   "Selection Effect",
                                   "Interaction Effect",
                                   "Active Return")
            return(ret.mat)
          }
          )



## returns for regression class object


setMethod("returns",
          signature(object = "regression"),
          function(object,
                   ...){
            
            ## round to certain digits
            options(digits = 3)

            no.row <- length(object@reg.var) + 4
            ret.mat <- matrix(NA, nrow = no.row, ncol = 1)
            
            ret.mat[1:(no.row - 4), 1] <- object@contrib
            ret.mat[no.row - 3, 1] <- object@portfolio.ret - sum(object@contrib)
            ret.mat[no.row - 2, 1] <- object@portfolio.ret
            ret.mat[no.row - 1, 1] <- object@benchmark.ret
            ret.mat[no.row, 1] <- object@act.ret

            colnames(ret.mat) <- as.character(unique(object@universe[[object@date.var]]))
            rownames(ret.mat) <- c(object@reg.var,
                                   "Residual",
                                   "Portfolio Return",
                                   "Benchmark Return",
                                   "Active Return")
            return(ret.mat)
          }
          )
