################################################################################
##
## ## Class definitions for the pa package.
##
################################################################################

setClass("brinson",
         representation = representation(
           date.var        = "character",
           sector.var      = "character", 
           bench.weight    = "character",
           portfolio.weight= "character",
           ret.var         = "character", 
           weight.port     = "array",
           weight.bench    = "array",
           ret.port        = "array",
           ret.bench       = "array",
           q4              = "numeric",
           q3              = "numeric",
           q2              = "numeric",
           q1              = "numeric",
           universe        = "data.frame"
           ),
         prototype = prototype(
           date.var        = character(),
           sector.var      = character(), 
           bench.weight    = character(),
           portfolio.weight= character(),
           ret.var         = character(), 
           weight.port     = array(),
           weight.bench    = array(),
           ret.port        = array(),
           ret.bench       = array(),
           q4              = numeric(),
           q3              = numeric(),
           q2              = numeric(),
           q1              = numeric(),
           universe        = data.frame()
           ),
         validity = function(object){

           ## The length of the original portfolio should be the
           ## same as the length of each matching portfolio.
           
           ## weights.orig  <- object@weights.orig
           ## weights.match <- object@weights.match
           
           ## if(!nrow(weights.match) %in% length(weights.orig))
           ##   return(paste("Each column of \"weights.match\" must have",
           ##                "the same length as \"weights.orig\"."))

           ## return(TRUE)
         }
         )

setClass("brinsonMulti",
         representation = representation(
           date.var         = "character",
           sector.var       = "character",
           bench.weight     = "character",
           portfolio.weight = "character",
           ret.var          = "character",
           weight.port      = "matrix",
           weight.bench     = "matrix",
           ret.port         = "matrix",
           ret.bench        = "matrix",
           brinson.mat      = "matrix",
           universe         = "list"
           ),
         prototype = prototype(
           date.var        = character(),
           sector.var      = character(), 
           bench.weight    = character(),
           portfolio.weight= character(),
           ret.var         = character(), 
           weight.port     = matrix(),
           weight.bench    = matrix(),
           ret.port        = matrix(),
           ret.bench       = matrix(),
           brinson.mat     = matrix(),
           universe        = list()
           ),
         validity = function(object){
           return(TRUE)
         })
