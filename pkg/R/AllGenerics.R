################################################################################
##
## Generics for the pa package.
##
################################################################################

## portfolioMatch class methods.

if(!isGeneric("exposure"))
  setGeneric("exposure", function(object,
                                  ...) standardGeneric("exposure"))
if(!isGeneric("returns"))
  setGeneric("returns", function(object,
                                 ...) standardGeneric("returns"))

if(!isGeneric("plot"))
  setGeneric("plot", function(object,
                              ...) standardGeneric("plot"))
