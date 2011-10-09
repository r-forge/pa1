################################################################################
##
## $Id: portfolio.match.R 1137 2008-08-29 13:46:38Z satopaa $
##
## Convenience function to perform portfolio matching.
##
################################################################################

## This function performs portfolio matching. If matching is conducted
## over a period including multiple unique dates, this function
## returns a portfolioMatches object. If matching on the other hand is
## done only on a single data, this function returns a portfolioMatch
## object. It takes in eight arguments: the universe of stocks (x),
## the variables on which the matching is done (match.var), the name
## of the weight column to be used (weight.var), the name of the
## return column to be used (ret.var), the naem of the date column to
## be used (date.var), threshold (thresh), number of matches per each
## period (n) and a logical determining whether matching should be
## done with or without replacement (replace)

portfolio.match <- function(x,
                            match.var,
                            weight.var = "weight",
                            ret.var    = "fwd.ret.1m",
                            date.var   = "date",
                            thresh     = 0,
                            n          = 1,
                            replace    = TRUE,
                            within     = FALSE){
  
  ## x must be a data frame.

  stopifnot(is.data.frame(x))

  ## weight.var must have length 1.

  stopifnot(length(weight.var) == 1)

  ## ret.var must have length 1.

  stopifnot(length(ret.var) == 1)

  ## date.var must have length 1.

  stopifnot(length(date.var) == 1)

  ## Obtain unique datas. In case of multiple-date-matching.
  
  dates <- unique(x[[date.var]])

  ## If the universe extends over multiple dates, matching needs to be
  ## done on each data separately. This recursive loop run
  ## portfolio.match on each data separately, stores each
  ## portfolioMatch into a list. Using this list a portfolioMatches
  ## object can be created.
  
  if(length(dates) > 1){
    
    matches <- lapply(dates, FUN = function(i){portfolio.match(x[x[[date.var]] %in% i,],
                               match.var,
                               weight.var,
                               ret.var,
                               date.var,
                               thresh,
                               n,
                               replace
                               )})
    pm.multi <- new ("portfolioMatches",
                     matches = matches)
    return(pm.multi)
    
  } else{
    
    ## If match.var is null, matching portfolios are found on randomly
    ## with replacement. index.match is a matrix of indeces of the
    ## matching portfolios. At this point all indeces are characters.
    
    if(is.null(match.var)){
      index.match <- sapply(1:n,
                            function(i){
                              sample(rownames(x[x[[weight.var]] == 0,]),
                                     nrow(x[x[[weight.var]] != 0,]),
                                     replace = replace)})
      
    } else{
       
      ## match.var and weight.var must have class character.
      
      stopifnot(all(sapply(c(match.var, weight.var), is.character)))
      
      ## match.var and weight.var must exist in x.
      
      stopifnot(all(c(match.var, weight.var) %in% names(x)))
      
      ## Check whether the column specified by weight.var is numeric.
      
      stopifnot(is.numeric(x[[weight.var]]))
      
      ## Makes a linear model for propensity score calculations.
      
      prop <- prop.calc(x, match.var, weight.var)

      ## Do we need to compute distances? This is for MD matching within
      ## propensity score calipers.

      dist <- NULL
      if (within) {
        mm <- model.matrix(formula(paste("~",paste(match.var,collapse="+"))), x)
        mm.e <- eigen(cov(mm))
        mm.s <- mm.e$vectors %*%
          diag(replace(1 / mm.e$values, mm.e$values == 0, 0)) %*% t(mm.e$vectors)
        dist <- apply(mm[x[[weight.var]] != 0,], 1,
                      function(row) mahalanobis(mm, row, mm.s, inverted = TRUE))
      }
      
      ## Choose a matched portfolio.

      index.match <- sapply(1:n, function(i){
        prop.match(prop$fitted,
                   prop$model[[1]] != 0,
                   thresh = thresh,
                   replace = replace,
                   distance = dist)
      })
    }
    
    ## Create a portfolioMatch object.
    
    pm <- new("portfolioMatch",
              date.var        = date.var,
              match.var       = match.var,
              ret.var         = ret.var,
              weights.orig    = x[[weight.var]],
              weights.match   = .create.weight.mat(index.match, x, weight.var),
              thresh          = thresh,
              replace         = replace,
              universe        = x
              )
    
    ## Calculate exposures.
    
    pm@exposures.orig <- exposures(pm, match.var, "orig")
    pm@exposures.match <- exposures(pm, match.var, "match")
    
    ## Calculate returns.
    
    pm@return.orig <- returns(pm, "orig")
    pm@return.match <- returns(pm, "match")
  
    return(pm)
  }
}

## This function creates a matrix of weights based on a given matrix
## (or vector) of indices and a universe data frame. The indices are
## assumed to match the stocks in the original portfolio (that is,
## those stocks with non-zero weights in universe x) in the same order
## (That is, the first index of a column in index.match should match
## to the first non-zero weight stock in universe x).

.create.weight.mat <- function(index.match, x, weight.var){

  ## We convert the indices to a matrix instead of a vector, because
  ## apply will not work for a single vector.
  
  index.match <- as.matrix(index.match)

  ## Find the weights of the stocks in the original portfolio
  
  orig.weights <- x[[weight.var]][!x[[weight.var]] == 0]

  ## Create a weights matrix where each column of the weights matrix
  ## corresponds to a column of indices in index.match
  
  weight.mat <- apply(index.match, MARGIN = 2, FUN =
                      function(vec){
                        
                        ## Create a vector of 0s with length = the
                        ## number of rows of universe x
                        
                        matched.weights <- rep(0, length(x[[weight.var]]))
                        
                        ## For each index (element in vec) set the
                        ## weights using orig,weights. If an index is
                        ## selected twice (appears in vec twice) sum
                        ## the weights from orig.weights
                        
                        for(i in 1:length(vec)){
                          ifelse(matched.weights[vec[i]] == 0,
                                 matched.weights[vec[i]] <- orig.weights[i],
                                 matched.weights[vec[i]] <-
                                 sum(matched.weights[vec[i]], orig.weights[i]))
                        }
                        return(matched.weights)
                      })
  return(weight.mat)
}

