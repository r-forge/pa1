################################################################################
##
## $Id: portfolioMatches.R 1136 2008-08-29 13:35:30Z satopaa $
##
## portfolioMatches methods
##
################################################################################

## Reports the periods on which the matching was conducted.

setMethod("summary",
          signature(object = "portfolioMatches"),
          function(object,
                   ...){

            matches <- object@matches
            
            cat(sprintf("Multiple periods matching conducted with:"), "\n",
                sprintf("Periods:     %-s",
                        paste(unique(lapply(1:length(matches),
                                            function(i){
                                              unique(as.character(matches[[i]]@universe[[matches[[i]]@date.var]]))})),
                              collapse = ", ")), "\n")
          }
          )

## aggregateMatches takes a portfolioMatches object as its only
## argument. By aggregating the weights, exposures and returns,
## aggregateMatches makes a single portfolioMatch object that
## represents the aggregateMatches. This aggregateMatch object can
## then be used for analysing the matching done over the given period.

setMethod("aggregateMatches",
          signature(object = "portfolioMatches"),
          function(object,
                   ...){

            matches <- object@matches
           
           ## The first part of this function deals with weight
           ## aggregation.
           
           ## Make a matrix where each column represents the weights
           ## of the original portfolio. There is one column for each
           ## portfolioMatch object in matches.
           
           weights.orig <- sapply(matches, FUN = function(i){
             i@weights.orig
           })
           
           ## Make a matrix where each column represents the weights
           ## of a single matched portfolio.
           
            weights.match <- do.call(cbind,
                                     lapply(matches,
                                            FUN = function(i){
                                              i@weights.match}))

           ## Calculate the average weight of each stock of the
           ## original portfolio over the period and normalize the
           ## averages.
           
           weights.orig.avg <- rowMeans(weights.orig)
           weights.orig.avg <- weights.orig.avg/sum(weights.orig.avg)
           
           ## Calculate the average weight of each stock that was used
           ## as a match over the period and normalize the averages.
           
           weights.match.avg <- rowMeans(weights.match)
           weights.match.avg <- matrix(weights.match.avg/sum(weights.match.avg))
           
           ## The second part of this function deals with the
           ## exposures aggregation.
           
           ## Extract the variables on which the matching was done.

           var.match <- names(matches[[1]]@exposures.orig)

           ## Get the average exposures of the original portfolio on
           ## each level of var.match and store these exposures into a
           ## list (exposures.orig) that has one element per each
           ## variable in var.match. Each element has the long, short
           ## and total exposures for all of its levels.
           
           exposures.orig <- lapply(var.match, FUN = function(i){
             list(.calc.expo(matches, "exposures.orig", i))
           })
           names(exposures.orig) <- var.match

           ## Get the average exposures of the matched portfolios on
           ## each level of var.match and store these exposures into a
           ## list (exposures.match) that has one element per each
           ## variable in var.match. Each element has the long, short
           ## and total exposures for all of its levels.
           
           exposures.match <- lapply(var.match, FUN = function(i){
             list(.calc.expo(matches, "exposures.match", i))
           })
           names(exposures.match) <- var.match
            
           ## The third part of this function deals with the returns
           ## aggregation.

           ## Make a matrix with one column per each day in the
           ## period. Each column has three rows: long returns, short
           ## returns and total returns.
            
           return.orig <- sapply(matches, FUN = function(i){
             unlist(i@return.orig)
           })
           
           ## Compound the returns in return.orig row-wise.
           
           return.orig <- .matrix.compound(return.orig)

           ## Make a matrix with one column per each day in the
           ## period. Each column has three rows: average long returns
           ## of the matched portfolios for that day, average short
           ## returns of the matched portfolios for that day and average total
           ## returns of the mactched portfolios for that day.
           
           return.match <- sapply(matches, FUN = function(i){
             n <- length(i@return.match)
             match.ret <- .list.sum(i@return.match)
             match.ret/n
            })

           ## Compound the returns in return.match row-wise.
           
           return.match <- .matrix.compound(matrix(unlist(return.match),
                                                   nrow = 3))

            names(return.match[[1]]) <- c("long", "short", "total")
            
            ## The last part of this function make the portfolioMatch
            ## object using all the aggregated values calculated in
            ## earlier sections of this function.
            
            return.pm <- new("portfolioMatch",
                             match.var = matches[[1]]@match.var,
                             ret.var = matches[[1]]@ret.var,
                             date.var = matches[[1]]@date.var,
                             weights.orig = weights.orig.avg,
                             weights.match = weights.match.avg,
                             exposures.orig = exposures.orig,
                             exposures.match = exposures.match,
                             return.orig = return.orig,
                             return.match = return.match,
                             thresh = matches[[1]]@thresh,
                             replace = matches[[1]]@replace,
                             universe = do.call(rbind, lapply(1:length(matches),
                               function(i) matches[[i]]@universe))
                             )
          }
          )

## Using an expression calculates the compounded value row-wise.

.matrix.compound <- function(matrix){  
  matrix <- matrix + 1
  expr = paste("matrix[,",seq(along = matrix[1,]),"]", collapse="*")
  returns <- eval(parse(text = expr))-1
  returns <- data.frame(t(returns))
  return(list(returns))
}

## Calculates the mean exposures and returns a single data frame of
## long, short and total exposures in var.

.calc.expo <- function(matches, exposure, var){

  ## Sum up each matched portfolio in a single day. expo is a list of
  ## sums with one element per each day of the period.
  
  expo <- lapply(matches, FUN = function(i){
   .list.sum(slot(i, exposure)[[var]])
  })
  
  ## The mean exposures of the original portfolio can be calculated by
  ## dividing the total sum by the number of days in the period. The
  ## mean exposures of the matched portfolios, however, need to be
  ## divided by the total number of matched portfolios over the time
  ## period.
  
  if(exposure == "exposures.orig"){
    expo.avg <- .list.sum(expo)/length(matches)
  } else if (exposure == "exposures.match"){
    expo.avg <- .list.sum(expo)/(length(matches)*length(matches[[1]]@weights.match))
  }

  ## Normalize the mean exposure. The transpose is necessary as this
  ## call used the recycling method of R.
  
  s <- t(expo.avg)/colSums(expo.avg)
  s[s == "NaN"] <- 0

  return(data.frame(t(s)))
}

## A helper function to sum up each element of a list.

.list.sum <- function(list){
  expr = paste("list[[",seq(along = list),"]]", collapse="+")
  eval(parse(text = expr)) 
}

