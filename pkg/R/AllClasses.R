################################################################################
##
## $Id: AllClasses.R 1121 2008-08-28 16:30:55Z zhou $
##
## Class definitions for the portfolioMatch package.
##
################################################################################

setClass("portfolioMatch",
         representation = representation(
           date.var        = "character",
           match.var       = "character",
           ret.var         = "character",
           weights.orig    = "numeric",
           weights.match   = "matrix",
           exposures.orig  = "list",
           exposures.match = "list",
           return.orig     = "list",
           return.match    = "list",
           thresh          = "numeric",
           replace         = "logical",
           universe        = "data.frame"
           ),
         prototype = prototype(
           date.var        = character(),
           match.var       = character(),
           ret.var         = character(),
           weights.orig    = numeric(),
           weights.match   = matrix(nrow = 0, ncol = 0),
           exposures.orig  = list(),
           exposures.match = list(),
           return.orig     = list(),
           return.match    = list(),
           thresh          = numeric(),
           replace         = logical(),
           universe        = data.frame()
           ),
         validity = function(object){

           ## The length of the original portfolio should be the
           ## same as the length of each matching portfolio.
           
           weights.orig  <- object@weights.orig
           weights.match <- object@weights.match
           
           if(!nrow(weights.match) %in% length(weights.orig))
             return(paste("Each column of \"weights.match\" must have",
                          "the same length as \"weights.orig\"."))

           return(TRUE)
         }
         )

setClass("portfolioMatches",
         representation = representation(
           matches = "list"
           ),
         prototype = prototype(
           matches = list()
           ),
         validity = function(object){
           return(TRUE)
         })
