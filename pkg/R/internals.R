## Yang Lu Yang.Lu@williams.edu

## a collection of internal functions used in the package


## This helper function uses two arguments, vdep and vexp, to make a
## string representing a formula format appropriate for a linear
## model: "vdep ~ vexp[[1]] + vexp[[2]] + ... + vexp[[n]]"
.formula.make <- function(vdep, vexp) {
  formula(paste(vdep, "~", paste(vexp, collapse = " + ")))
}

