actshare <- function(x,
                     y = NULL, 
                     type = "orig"){

  stopifnot(type %in% c("orig", "within"))
  
  ## only 1 portfolioMatch object is given
  
  if (is.null(y)){

    ## if type is orig, compare the matching portfolios with the
    ## original portfolio
    
    if (type == "orig") {

      ## but if we use matched weights against the original weights,
      ## everytime we will get the same answer as no original holdings
      ## will appear in the matching portfolios, and the sum of
      ## matched weights is always 1 or close to 1
      
      act <- sapply(1:(dim(x@weights.match)[2]),
                    function(i){sum(abs(x@weights.match[,i]))/2})
      return(act)
      
    } else {

      ## compare amongst n samples

      act <- c()

      for (i in 1:(dim(x@weights.match)[2]-1)){

        a <- c()
        
        for (j in (i):(dim(x@weights.match)[2]-1)){

          ## ignore the diagonal as it will give 0 each time
          
          a <- c(sum(abs(x@weights.match[,j+1] - x@weights.match[,i]))/2, a)}

        act <- c(a, act)}

      return(act)
    }
    
  } else {

    ## make sure that both objects have the same number of samples
    ## drawn
    
    stopifnot(dim(x@weights.match)[2] == dim(y@weights.match)[2])

    ## pair-wise comparison
    
    act <- sapply(1:(dim(x@weights.match)[2]),
                  function(i){sum(abs(y@weights.match[,i] - x@weights.match[,i]))/2})

    return(act)
  }
}


