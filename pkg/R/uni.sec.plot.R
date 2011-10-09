################################################################################
##
## $Id: uni.sec.plot.R 906 2008-08-07 14:06:20Z satopaa $
##
## Function that makes a distinction between the holdings and
## non-holdings of the portfolio by plotting a set of 2-dimensional
## cross-sections by country, each showing securities by sector and
## market cap.
##
################################################################################

## x is an universe of stocks. match.var defines the sections that are
## shown in the final graphical output. weight.var defines the column
## of weights in x.

uni.sec.plot <- function(x,
                         match.var,
                         weight.var){

  ## match.use contains the names of the four most populated levels of
  ## match.var[[1]].
  
  match.use <- names(sort(table(x[[match.var[1]]]), decreasing = TRUE)[1:4])

  ## Take a subset of all the rows that are associated with any of the
  ## four character values of match.use.
  
  rows <- x[[match.var[1]]] %in% match.use
  subset <- subset(x, rows)

  ## Take a subset of all the rows that are associated with any of the
  ## four values of match.use and is a long holding in the portfolio.
  
  groups <- subset[[weight.var]] > 0

  ## The final graphical output will be divided into four rectangular
  ## dotplots. Each name in match.use will have a single rectangular
  ## that represents a dotplot of levels versus market cap. Portfolio
  ## holdings are black dots and non-holdings are grey.
  
  print(stripplot(formula(paste(match.var[2],
                                "~", match.var[3],
                                "|", match.var[1])),
                  subset,
                  jitter = TRUE,
                  col = c("grey", "black"),
                  pch = 20,
                  scales =  list(x      = list(log = TRUE,
                                 at     = 10^(8:11),
                                 labels = c("$100M", "$1B", "$10B", "$100B"))),
                  xlab = "Market Cap in USD",
                  ylab = "Sector",
                  main = "Universe: Holdings and Non-holdings",
                  groups = groups))
}
