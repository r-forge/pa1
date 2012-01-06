## Yang Lu Yang.Lu@williams.edu

## plot methods for brinson class

setMethod("plot",
          signature(object = "brinson"),
          function(object,
                   type = "exposure", ...){

            ## type == "exposure"
            if (type == "exposure"){

              df <- data.frame(Name = rep(names(object@weight.port), 2),
                               Value = c(object@weight.port, object@weight.bench),
                               Type = c(rep("Portfolio", length(object@weight.port)),
                                 rep("Benchmark", length(object@weight.bench))))
              
              ## plot
              .bar.plot(df = df,
                        type = "Exposure",
                        title = "Exposure -- Portfolio vs. Benchmark")
              
            } else {
              ## type == "returns"
              df <- data.frame(Name = rep(names(object@ret.port), 2),
                               Value = c(object@ret.port, object@ret.bench),
                               Type = c(rep("Portfolio", length(object@ret.port)),
                                 rep("Benchmark", length(object@ret.bench))))

              .bar.plot(df = df,
                        type = "Return",
                        title = "Return -- Portfolio vs. Benchmark")
              
            }
          }
          
          )

## plot methods for brinsonMulti class


setMethod("plot",
          signature(object = "brinsonMulti"),
          function(object,
                   type = "exposure", ...){

            if (type == "exposure"){
              
              .df <- list()
              dates <- object@date.var
              len <- length(dates)
              len.var <- nrow(object@weight.port)
              for (k in 1:len){
                .df[[k]] <- data.frame(Date = rep(as.character(dates[k]), len.var),
                                       Name = rep(rownames(object@weight.port), 2),
                                       Value = c(object@weight.port[, k], object@weight.bench[, k]),
                                       Type = c(rep("Portfolio", len.var),
                                         rep("Benchmark", len.var)))
              }
              .df <- do.call(rbind,.df)
              ## plot

              .facet.plot(df = .df,
                          type = "Exposure",
                          title = "Exposure across periods")
              
            } else {
              ## type == "returns"
              
              .df <- list()
              dates <- object@date.var
              len <- length(dates)
              len.var <- nrow(object@ret.port)
              for (k in 1:len){
                .df[[k]] <- data.frame(Date = rep(as.character(dates[k]), len.var),
                                       Name = rep(rownames(object@ret.port), 2),
                                       Value = c(object@ret.port[, k], object@ret.bench[, k]),
                                       Type = c(rep("Portfolio", len.var),
                                         rep("Benchmark", len.var)))
              }
              .df <- do.call(rbind,.df)
              ## plot

              .facet.plot(df = .df,
                          type = "Return",
                          title = "Return across periods")

            }
          }
          
          )



## plot for regression class object

setMethod("plot",
          signature(object = "regression"),
          function(object,
                   type = "exposure",
                   var,
                   ...){

            ## type == "exposure"
            if (type == "exposure"){

              temp <- exposure(object, var = var)
              len.row <- nrow(temp)
              df <- data.frame(Name = rep(rownames(temp), 2),
                               Value = c(temp[ , 1], temp[ , 2]),
                               Type = c(rep("Portfolio", len.row),
                                 rep("Benchmark", len.row)))
              
              ## plot
              .bar.plot(df = df,
                        type = "Exposure",
                        title = "Exposure -- Portfolio vs. Benchmark")
              
            } else {
              ## type == "returns"
              ## not sure I need this return plot -- what to show?
              
              ## df <- data.frame(Name = rep(names(object@ret.port), 2),
              ##                  Value = c(object@ret.port, object@ret.bench),
              ##                  Type = c(rep("Portfolio", length(object@ret.port)),
              ##                    rep("Benchmark", length(object@ret.bench))))

              
              ## .bar.plot(df = df,
              ##           type = "Return",
              ##           title = "Return -- Portfolio vs. Benchmark")
              
            }
          }
          )

## plot for regressionMulti class object

setMethod("plot",
          signature(object = "regressionMulti"),
          function(object,
                   type = "exposure",
                   var,
                   ...){

            if (type == "exposure"){
              .df <- list()
              dates <- object@date.var
              len <- length(dates)
              temp <- exposure(object, var = var)
              len.var <- nrow(temp[[1]])
              
              for (k in 1:len){
                .df[[k]] <- data.frame(Date = rep(as.character(dates[k]), 2 * len.var),
                                       Name = rep(rownames(temp[[1]], 2)),
                                       Value = c(temp[[1]][ , k], temp[[2]][ , k]),
                                       Type = c(rep("Portfolio", len.var),
                                         rep("Benchmark", len.var)))
              }
              .df <- do.call(rbind,.df)
              
              ## plot
              .facet.plot(df = .df,
                          type = "Exposure",
                          title = "Exposure across periods")
              
            } else {
              ## type == "returns"
              ## what to show?
              ## .df <- list()
              ## dates <- object@date.var
              ## len <- length(dates)
              ## len.var <- nrow(object@ret.port)
              ## for (k in 1:len){
              ##   .df[[k]] <- data.frame(Date = rep(as.character(dates[k]), len.var),
              ##                          Name = rep(rownames(object@ret.port), 2),
              ##                          Value = c(object@ret.port[, k], object@ret.bench[, k]),
              ##                          Type = c(rep("Portfolio", len.var),
              ##                            rep("Benchmark", len.var)))
              ## }
              ## .df <- do.call(rbind,.df)
              ## ## plot

              ## .facet.plot(df = .df,
              ##             type = "Return",
              ##             title = "Return across periods")

            }
          }
          
          )
