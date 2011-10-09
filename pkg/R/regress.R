## regress method to apply regression-based approach for portfolio
## attribution

## Yang Lu Yang.Lu@williams.edu

## setwd("~/pa1/pkg/data")
load("starmine.RData")
names(starmine)


## a single date date set first
oneMonth <- subset(starmine, date == "1995-01-31")
x <- oneMonth
regress <- function(x,
                    ret.var = "ret.0.6.m",
                    weight.var,
                    date.var = "date",
                    reg.var ## variables on the right hand side of the
                            ## regression formula
                    ){
  
  ## x must be a data frame
  stopifnot(is.data.frame(x))

  ## weight.var must have length 1.
  stopifnot(length(weight.var) == 1)
  
  ## ret.var must have length 1.
  stopifnot(length(ret.var) == 1)

  ## date.var must have length 1.
  stopifnot(length(date.var) == 1)

  dates <- unique(x[[date.var]])

  if (length(dates) > 1){
    ## call the regress function recursively

  } else {
    
    ## single-date data regression
    
    ## match.var and weight.var must be characters
    stopifnot(all(sapply(c(reg.var, weight.var), is.character)))
    
    ## reg.var and weight.var must be in the column names of the
    ## input data frame
    stopifnot(all(c(reg.var, weight.var) %in% names(x)))
    
    ## the weight.var column must be numeric
    stopifnot(is.numeric(x[[weight.var]]))
    
    ## Create a regression object with slots date.var, ret.var, 

    reg <- new("regression",
               date.var = date.var,
               ret.var = ret.var,
               

    
    
  }
  
}
