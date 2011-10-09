################################################################################
##
## $Id:$
##
## Function that plots the characteristics of the universe of
## stocks. This includes frequencies of stocks in each sector and
## country, and distributions of returns, scores and Log10(Market
## Cap).
##
################################################################################

## x is an universe of stocks.

uni.histograms <- function(x){

  ## x.summ.lf is a list that has two data frames. The first one
  ## contains the distribution of stocks amongst different sectors and
  ## countries. The second contains the returns, scores and
  ## Log10(Market Cap) of the stocks.
  
  final.output <- list()

  ## A single data frame is formed by rbinding together the data
  ## frames of the stocks frequencies in sector and country.
  
  final.output$str <-
    rbind(data.frame(category = "Sector",  table(x$sector)),
          data.frame(category = "Country", table(x$country)))

  ## A single data frame is formed by rbinding together the data
  ## frames of return, score and Log10(Market Cap).
  
  final.output$num <-
    rbind(data.frame(category = "Return", val = x$fwd.ret.1m),
          data.frame(category = "Score", val = x$smi),
          data.frame(category = "Log10(Market Cap)", val = log10(x$cap.usd)))

  ## The top part of the final graphical output has two separate
  ## horizontal barcharts of levels versus frequency: one for sector
  ## and one for country.
  
  final.output$top <- barchart(Var1 ~ Freq | category,
                               final.output$str,
                               xlab = "Number of Stocks",
                               scales = list(relation = "free"),
                               main = "Characteristics of Universe Stocks")
  
  ## The bottom part of the final graphical output has three different
  ## histograms of returns, scores and Log10(Market Cap).
  
  final.output$bottom <- histogram(~ val | category,
                                   final.output$num,
                                   breaks = NULL,
                                   as.table = TRUE,
                                   type = "count", ylab = "Number of Stocks",
                                   xlab = "Value",
                                   scales = list(relation = "free"))
  
  ## Print the top and bottom part of the final graphical output in
  ## their correct spots.
  
  print(final.output$top, split = c(1, 1, 1, 2), more = TRUE)
  print(final.output$bottom, split = c(1, 2, 1, 2))
}
