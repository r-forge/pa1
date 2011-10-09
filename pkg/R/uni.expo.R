################################################################################
##
## $Id:$
##
## Function that helps the user to compare the exposures of the
## portfolio and universe by plotting separate barcharts.
##
################################################################################

## x is an universe of stocks that contains at least sector, country,
## cap.usd.bucket, weight.uni and weight.smi.

uni.expo <- function(x){

  ## cols defines the columns for which the exposures are being
  ## compared.
  
  cols = c("sector", "country", "cap.usd.bucket")
  col.names = c("Sector", "Country", "Cap")
  
  ## First makes separate dataframes of portfolio and universe
  ## exposures in different areas specified by cols, e.g. sector,
  ## country and cap.usd.bucket. Then rbinds each of these data frames
  ## together into a single data frame.
  
  p.exp.lf <- do.call("rbind",
                      sapply(seq(along = cols),
                             function(i){
                               rbind(data.frame(
                                                por = "Portfolio",
                                                var = col.names[i],
                                                lev = names(tapply(x$weight.smi,
                                                                   x[[cols[i]]],
                                                                   sum)),
                                                wei = tapply(x$weight.smi,
                                                             x[[cols[i]]],
                                                             sum)),
                                     data.frame(
                                                por = "Universe",
                                                var = col.names[i],
                                                lev = names(tapply(x$weight.uni,
                                                                   x[[cols[i]]],
                                                                   sum)),
                                                wei = tapply(x$weight.uni,
                                                             x[[cols[i]]],
                                                             sum)))
                             }, simplify = FALSE))
  
  ## This makes separate horizontal barcharts of levels (y-axis)
  ## versus weights (x-axis) on the condition of var. 
  
  print(barchart(
                 lev ~ wei | var,
                 p.exp.lf,
                 groups = p.exp.lf$por,
                 as.table = TRUE,
                 xlab = "Weight",
                 scales = list(relation = "free"),
                 auto.key = TRUE,
                 main = "Exposures of Portfolio, Universe"))
}
