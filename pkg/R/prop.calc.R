################################################################################
##
## $Id: prop.calc.R 909 2008-08-07 15:05:24Z satopaa $
##
##  This function makes a linear model using match.var and
##  weight.var.
##
################################################################################

## weight.var defines weight column in x, which is the universe of
## stocks. match.var defines the variables on which the matching
## should be done.

prop.calc <- function (x, match.var, weight.var) {

  ## Choose original portfolio. 

  orig <- x[x[[weight.var]] != 0, ]
  
  ## Construct the matching formula and regress propensity on
  ## covariates. Treat long-only and long-short differently.

  if(all(orig[[weight.var]] > 0)){
    f <- .formula.make(paste(weight.var, "> 0"), match.var)
    prop <- glm(f, x, family = quasibinomial)
  }else{
    f <- .formula.make(weight.var, match.var)
    prop <- lm(f, x)
  }

  return(prop)
}

## This helper function uses two arguments, vdep and vexp, to make a
## string representing a formula format appropriate for a linear
## model: "vdep ~ vexp[[1]] + vexp[[2]] + ... + vexp[[n]]"

.formula.make <- function(vdep, vexp) {
  formula(paste(vdep, "~", paste(vexp, collapse = " + ")))
}
