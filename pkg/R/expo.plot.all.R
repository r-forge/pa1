################################################################################
##
## $Id:$
##
## A convenience function that uses expo.plot to plot exposures on
## different variables, e.g. sector, country and cap.
##
################################################################################

## object is a portfolioMatch object and match.var defines the
## variables on which the exposures are measured.

expo.plot.all <- function(object, match.var, ...){
  
  ## Set up the plot list.

  rst <- list(expo.plot(object, match.var[[1]], ...),
              expo.plot(object, match.var[[2]], ...),
              expo.plot(object, match.var[[3]], legend = TRUE, ...))

  ## Print the plots.
  
  print(rst[[1]], position = c(0, 0.45, 0.5, 0.95), more = TRUE)
  print(rst[[2]], position = c(0.5, 0.45, 1, 0.95), more = TRUE)
  print(rst[[3]], position = c(0, 0, 1, 0.45))
  grid.text("Exposures of Original and Matched Portfolio", .5, 1, vjust = 1,
            gp = gpar(fontsize = 18))
}
