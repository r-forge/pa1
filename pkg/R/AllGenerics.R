################################################################################
##
## $Id: AllGenerics.R 943 2008-08-13 13:15:17Z satopaa $
##
## Generics for the portfolioMatch package.
##
################################################################################

## portfolioMatch class methods.

if(!isGeneric("exposures"))
  setGeneric("exposures", function(object,
                                   var,
                                 ...) standardGeneric("exposures"))
if(!isGeneric("returns"))
  setGeneric("returns", function(object,
                                 ...) standardGeneric("returns"))
if(!isGeneric("expo.plot"))
  setGeneric("expo.plot", function(object,
                                   var,
                                   ...) standardGeneric("expo.plot"))
if(!isGeneric("rp.plot"))
  setGeneric("rp.plot", function(object,
                                 var,
                                 ...) standardGeneric("rp.plot"))
if(!isGeneric("rp.hist"))
  setGeneric("rp.hist", function(object,
                                 ...) standardGeneric("rp.hist"))

## portfolioMatches class methods.

if(!isGeneric("aggregateMatches"))
  setGeneric("aggregateMatches", function(object,
                                          ...) standardGeneric("aggregateMatches"))
