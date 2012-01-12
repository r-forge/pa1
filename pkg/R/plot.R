## Yang Lu Yang.Lu@williams.edu

## plot methods for brinson class

setMethod("plot",
          signature(object = "brinson"),
          function(object,
                   var = "sector",
                   type = "exposure", ...){

            ## type == "exposure"
            if (type == "exposure"){
              temp <- exposure(object, var = var)
              len.row <- nrow(temp)
              df <- data.frame(Name = rep(rownames(temp), 2),
                               Value = c(temp[ , 1], temp[ , 2]),
                               Type = c(rep("Portfolio", len.row),
                                 rep("Benchmark", len.row)))
              if (class(object@universe[[var]])[1] != "numeric"){
                df <- within(df,
                             Name <- factor(Name, levels = rev(levels(object@universe[[var]]))))} else {
                               df <- within(df,
                                            Name <- factor(Name, levels = unique(as.factor(df$Name))))}
              
              ## plot
              temp.plot <- .bar.plot(df = df,
                                     type = "Exposure",
                                     title = "Exposure -- Portfolio vs. Benchmark")
              print(temp.plot)
              
            } else {
              ## type == "returns"
              df <- data.frame(Name = rep(names(object@ret.port), 2),
                               Value = c(object@ret.port, object@ret.bench),
                               Type = c(rep("Portfolio", length(object@ret.port)),
                                 rep("Benchmark", length(object@ret.bench))))

              if (class(object@universe[[var]])[1] != "numeric"){
                df <- within(df,
                             Name <- factor(Name, levels = rev(levels(object@universe[[var]]))))}else {
                               df <- within(df,
                                            Name <- factor(Name, levels = unique(as.factor(df$Name))))}
      
              
              temp.plot <- .bar.plot(df = df,
                                     type = "Return",
                                     title = "Return -- Portfolio vs. Benchmark")
              print(temp.plot)
            }
          }
          
          )

## plot methods for brinsonMulti class

setMethod("plot",
          signature(object = "brinsonMulti"),
          function(object,
                   var = "sector",
                   type = "exposure", ...){
            
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

              if (class(object@universe[[1]]@universe[[var]])[1] != "numeric"){
                .df <- within(.df,
                              Name <- factor(Name, levels = rev(levels(object@universe[[1]]@universe[[var]]))))}else {
                                .df <- within(.df,
                                              Name <- factor(Name, levels = unique(as.factor(.df$Name))))}
              
              ## plot
              temp.plot <- .facet.plot(df = .df,
                                       type = "Exposure",
                                       title = "Exposure across Periods")
              print(temp.plot)
              
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

              if (class(object@universe[[1]]@universe[[var]])[1] != "numeric"){
                .df <- within(.df,
                              Name <- factor(Name, levels = rev(levels(object@universe[[1]]@universe[[var]]))))}else {
                                .df <- within(.df,
                                              Name <- factor(Name, levels = unique(as.factor(.df$Name))))}

              ## plot

              temp.plot <- .facet.plot(df = .df,
                                       type = "Return",
                                       title = "Return across Periods")
              print(temp.plot)
            }
          }
          )

## plot for regression class object

setMethod("plot",
          signature(object = "regression"),
          function(object,
                   var,
                   type = "exposure",
                   ...){

            ## type == "exposure"
            if (type == "exposure"){
              temp <- exposure(object, var = var)
              len.row <- nrow(temp)
              df <- data.frame(Name = rep(rownames(temp), 2),
                               Value = c(temp[ , 1], temp[ , 2]),
                               Type = c(rep("Portfolio", len.row),
                                 rep("Benchmark", len.row)))
           
              if (class(object@universe[[var]])[1] != "numeric"){
                df <- within(df,
                             Name <- factor(Name, levels = rev(levels(object@universe[[var]]))))} else {
                               df <- within(df,
                                            Name <- factor(Name, levels = unique(as.factor(df$Name))))}
              

              ## plot
              temp.plot <- .bar.plot(df = df,
                                     type = "Exposure",
                                     title = "Exposure -- Portfolio vs. Benchmark")
              print(temp.plot)
              
            } else {
              ## type == "returns"
              df <- as.data.frame(returns(object)[c(-nrow(df)-2, -nrow(df) - 1, -nrow(df))])
              names(df) <- "ret"
              df$Name <- c(CapLeading(object@reg.var), "Residual")
              df <- within(df,
                           Name <- factor(Name, levels = rev(df$Name)))
              
              
              temp.plot <- ggplot(df, aes(x = Name, y = ret)) +
                geom_bar(width = 0.5, position = "identity") + coord_flip() +
                  scale_y_continuous()+ geom_hline(yintercept = 0) + 
                    ylab("Return") + xlab("Attribution") +
                      opts(panel.background = theme_blank(),
                           title = "Portfolio Attribution",
                           axis.line = theme_blank(),
                           panel.grid.minor = theme_blank(),
                           panel.grid.major = theme_blank(),
                           plot.background = theme_rect(fill = NA, colour = NA))
              print(temp.plot)
            }
          }
          )

## plot for regressionMulti class object

setMethod("plot",
          signature(object = "regressionMulti"),
          function(object,
                   var,
                   type = "exposure",
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

              
              if (class(object@universe[[1]]@universe[[var]])[1] != "numeric"){
                .df <- within(.df,
                              Name <- factor(Name, levels = rev(levels(object@universe[[1]]@universe[[var]]))))}else {
                                .df <- within(.df,
                                              Name <- factor(Name, levels = unique(as.factor(.df$Name))))}
              
              ## plot
              temp.plot <- .facet.plot(df = .df,
                                       type = "Exposure",
                                       title = "Exposure across periods")
              print(temp.plot)
              
            } else {
              ## type == "returns"
              len.row <- length(object@date.var)
              df <- data.frame(Date = rep(as.character(object@date.var), 2),
                               Value = c(object@portfolio.ret[1, ],
                                 object@benchmark.ret[1, ]),
                               Type = c(rep("Portfolio", len.row),
                                 rep("Benchmark", len.row)))
              temp.plot <- ggplot(df, aes(x = Date, y = Value, col = Type, group = Type)) +
                geom_line(aes(linetype = Type)) +
                  scale_y_continuous()+ geom_hline(yintercept = 0) + 
                    ylab("Return") + xlab("Date") +
                      opts(panel.background = theme_blank(),
                           title = "Portfolio Performance",
                           axis.line = theme_blank(),
                           panel.grid.minor = theme_blank(),
                           panel.grid.major = theme_blank(),
                           axis.text.x = theme_text(angle = 90, hjust = 0.5),
                           plot.background = theme_rect(fill = NA, colour = NA))
              print(temp.plot)

            }
          }
          
          )



## csvol plot
setMethod("plot",
          signature(object = "csvol"),
          function(object,
                   ...){
            print("No plot avaiable for a single period")
          }
          )

## csvolMulti plot
setMethod("plot",
          signature(object = "csvolMulti"),
          function(object,
                   ...){
            
            df <- as.data.frame(object@csv)
            df$date <- object@date.var

            temp.plot <- ggplot(df, aes(x = date, y = csv)) +
              geom_point() + geom_line(aes(group = 1)) + 
                scale_y_continuous()+ 
                  ylab("CSV") + xlab("Date") +
                    opts(panel.background = theme_blank(),
                         title = "CSV across Periods",
                         axis.line = theme_blank(),
                         panel.grid.minor = theme_blank(),
                         panel.grid.major = theme_blank(),
                         axis.text.x = theme_text(angle = 90, hjust = 0.5),
                         plot.background = theme_rect(fill = NA, colour = NA))
            
            print(temp.plot)
            
          }
          
          )
