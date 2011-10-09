################################################################################
##
## $Id: prop.match.R 909 2008-08-07 15:05:24Z satopaa $
##
## Returns matched portfolios based on a vector of propensity scores
##
################################################################################

## prop.match returns the indices of a matched portfolio. prop.match
## always matches a treated security (a security with non-negative
## weight) with a control security (a security with a weight of 0). It
## looks for a match within a specified threshold. Within the
## threshold a match is randomly chosen if distance is NULL;
## otherwise, the minimum-distance neighbor is pulled out of the
## distance matrix. If no match is found in the threshold then the
## nearest control security outside the threshold is chosen. In this
## way setting thresh = 0 results in nearest neighbor matching while
## setting thresh = Inf results in completely random portfolios being
## formed.

prop.match <- function(prop.scores,
                       treatment,
                       thresh,
                       replace = TRUE,
                       distance = NULL,
                       tol = .Machine$double.eps){

  ## tr.scores are the propensity scores for the treated securities
  
  tr.scores <- prop.scores[treatment]
  match.indices <-  c()

  ## Loop through the treated propensity scores to find matches
  for(i in tr.scores){

    ## Find control indices within the threshold if possible
    
    pos.indices <- which(prop.scores >= (i - thresh - tol) &
                         prop.scores <= (i + thresh + tol) &
                         !treatment)

    ## If no control indices within the threshold are found then find
    ## the nearest neighbor indices
    
    if(length(pos.indices) == 0){
      prop.diffs <- abs(prop.scores - i)
      pos.indices <- which(abs(prop.diffs - min(prop.diffs[!treatment])) <
                           tol & !treatment)     
    }

    ## Sample from the possible indices and return the choosen index
    if (!is.null(distance)) {
      # Choose the minimum-distance unit from the group
      d <- distance[names(pos.indices), names(which(tr.scores == i)[1])]
      picked.index <- pos.indices[which.min(d)]
    } else {
      # Choose randomly from the group
      picked.index <- .resample(pos.indices, 1, replace = FALSE)
    }
    match.indices <- c(match.indices, picked.index)

    ## If replace is TRUE, take out the control value picked to be
    ## matched (We do this by setting treatment to TRUE for that the
    ## matched index)
    
    if(replace == FALSE){
      treatment[picked.index] <- TRUE
    }
  }
  
  return(match.indices)
}


## Sample from a vector, or if the data is of length 1 then return the
## data

.resample <- function(x, size, ...){
  if(length(x) == 1){
    x
  } else{
    sample(x, size, ...)
  }
}
