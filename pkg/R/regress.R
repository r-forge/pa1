## regress method to apply regression-based approach for portfolio
## attribution


## THE MULTIPLE PERIOD IS UNDER CONSTRUCTION

## Yang Lu Yang.Lu@williams.edu

setwd("~/pa1/pkg/data")
load("s1.RData")
names(s1)

## a single date date set first
## oneMonth <- subset(starmine, date == "1995-01-31")
##x <- oneMonth
x <- s1
regress <- function(x,
                    ret.var = "fwd.ret.1m",
                    weight.var = "weight.smi",
                    date.var = "date",
                    reg.var = c("sector", "cap.usd", "country"),
                    ## variables on the right hand side of the
                    ## regression formula
                    name = "Regression-Based"
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


  ## HOW TO DEAL WITH MULTIPLE DATES?
  if (length(dates) > 1){
    ## call the regress function recursively
    .fun <- function(i){regress(x[x[[date.var]] %in% i, ],
                                ret.var,
                                weight.var,
                                date.var,
                                reg.var,
                                name
                                )
                      }

    multiples <- lapply(dates, .fun)
    reg.multi <- new("regressiones",
                     name = name,
                     date.var = as.character(dates),
                     ret.var = ret.var,
                     reg.var = reg.var,
                     coefficients = coefficients(lm.obj),
                     universe = multiples
                     )
    ## 
    reg.multi@weights
    reg.multi@ret.orig
    reg.multi@ret.reg

  } else {
    
    ## single-date data regression
    
    ## match.var and weight.var must be characters
    stopifnot(all(sapply(c(reg.var, weight.var), is.character)))
    
    ## reg.var and weight.var must be in the column names of the
    ## input data frame
    stopifnot(all(c(reg.var, weight.var) %in% names(x)))
    
    ## the weight.var column must be numeric
    stopifnot(is.numeric(x[[weight.var]]))

    ## store weights
    weights <- matrix(0)
    weights <- x[[weight.var]]
    
    ## Create the regression formula.
    ## The left hand side is the ret.var
    ## The right hand side is the ret.var -- user-defined factors
    f <- .formula.make(ret.var, reg.var)
    lm.obj <- lm(f, data = x) ## weights = weights should i do weighted regression?
    

    ## Create a regression object with slots date.var, ret.var,
    ## reg.var, weights, ret.orig, ret.reg, coefficients,
    ## universe(data).

    ##ret.orig and ret.reg here are individual results
    reg <- new("regression",
               name = name,
               date.var = date.var,
               ret.var = ret.var,
               reg.var = reg.var,
               weights = x[[weight.var]],
               coefficients = coefficients(lm.obj),
               universe = x
               )
    
    reg@ret.orig <- x[[ret.var]] %*% weights
    reg@ret.reg <- predict(lm.obj) %*% weights
  }
  
}
