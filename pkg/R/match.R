## Yang Lu Yang.Lu@williams.edu

## matching methodology for portfolio attribution

## The essential function that matches portfolio. It creates either
## the portfolioMatch or the portfolioMatches class object. The input
## match var must have at least one continuous variable. x is a data
## frame which contains a collection of stocks and some of their
## attributes. match.var option is for the user to provide the
## matching variable(s) to creat the matching portfolio(s). The
## weight.var refers to the name of the column which contains the
## weight infomation for each stock. ret.var refers to the name of the
## column which contains the momentum information for each
## stock. date.var specifies the name of the column with date
## information. bench.var requires the name of the column containing
## the benchmark weight info. zero.var gives users the option to set
## the exposure of one or more variables to be zero when creating
## matching portfolios(s). name option gives users the option of
## assigning a name to the matching portfolio created. breaks
## specifies the quantiles into which each continuous variable should
## be split. Breaks must be integer. If only 1 number is provided, all
## the continuous variables will be split into the same number of
## quantiles. If multiple numbers are provided, each corresponds to a
## continuous variable in the match.var in order. n refers to the
## number of samples to be drawn.

portfolio.match <- function(x,
                            match.var  = NULL,
                            weight.var = "weight.smi",
                            ret.var    = "fwd.ret.1m",
                            date.var   = "date",
                            bench.var  = "benchmark",
                            zero.var   = NULL,
                            name       = NULL, 
                            breaks     = 10, ## do we really want
                                             ## breaks? we can just
                                             ## internally do a decile
                                             ## match.
                            n          = 1){
  ## DO I REALLY NEED THIS?
  options(digits = 4)
  
  ## x must be a data frame.
  stopifnot(is.data.frame(x))

  ## weight.var must have length 1.
  stopifnot(length(weight.var) == 1)

  ## ret.var must have length 1.
  stopifnot(length(ret.var) == 1)

  ## breaks must be numeric
  stopifnot(is.numeric(breaks))

  ## date.var must have length 1.
  stopifnot(length(date.var) == 1)

  dates <- unique(x[[date.var]])

  ## If the universe extends over multiple dates, matching needs to be
  ## done on each data separately. This recursive loop run
  ## portfolio.match on each data separately, stores each
  ## portfolioMatch into a list. Using this list a portfolioMatches
  ## object can be created.

  if(length(dates) > 1){

    .fun <- function(i){portfolio.match(x[x[[date.var]] %in% i, ],
                                        match.var,
                                        weight.var,
                                        ret.var,
                                        date.var,
                                        bench.var,
                                        zero.var,
                                        name, 
                                        breaks,
                                        n)}

    matches <- lapply(dates, .fun)

    ## set the slots for portfolioMatches class

    pm.multi <- new ("portfolioMatches",
                     date.var    = as.character(dates),
                     ret.var     = ret.var,
                     universe    = matches)

    ## the name slot for portfolioMatch object where users can assign
    ## a name to the matching done for easy reference
    
    if (is.null(name)){
      pm.multi@name <- "NA"
    } else {
      pm.multi@name <- name}

    ## if match.var is NULL, a random matching portfolio will be
    ## generated. The display for match.var will be "random"
    
    if (is.null(match.var)){
      pm.multi@match.var <- "random"
    } else {
      pm.multi@match.var <- match.var}
    
    ## a list contains the original weight for each stock

    .w.o.list <- lapply(1:length(dates), function(i){matches[[i]]@weights.orig})
    names(.w.o.list) <- dates

    ## a list contains the matched weight for each stock

    .w.m.list <- lapply(1:length(dates), function(i){matches[[i]]@weights.match})
    names(.w.m.list) <- dates

    ## orignal exposure for each match.var

    .e.o.list <- lapply(1:length(dates), function(i){matches[[i]]@exposures.orig})
    names(.e.o.list) <- dates

    ## matched exposure for each match.var

    .e.m.list <- lapply(1:length(dates), function(i){matches[[i]]@exposures.match})
    names(.e.m.list) <- dates

    ## returns orig and match slots

    .r.o <- matrix(sapply(c(1:length(dates)),
                          function(i){matches[[i]]@return.orig}),
                   nrow = length(dates), ncol = 1)
    rownames(.r.o) <- as.character(dates)

    .r.m <- matrix(sapply(c(1:length(dates)),
                          function(i){matches[[i]]@return.match}),
                   nrow = length(dates), ncol = n, byrow = TRUE)
    rownames(.r.m) <- as.character(dates)


    pm.multi@weights.orig         <- .w.o.list
    pm.multi@weights.match        <- .w.m.list
    pm.multi@exposures.orig       <- .e.o.list
    pm.multi@exposures.match      <- .e.m.list
    pm.multi@return.orig          <- .r.o
    pm.multi@return.match         <- .r.m

    return(pm.multi)

  } else {

    set.seed(n)
    
    ## match.var and weight.var must be characters
    
    stopifnot(all(sapply(c(match.var, weight.var), is.character)))
    
    ## match.var and weight.var must be in the column names of the
    ## input data frame
    
    stopifnot(all(c(match.var, weight.var) %in% names(x)))
    
    ## the weight.var column must be numeric
    
    stopifnot(is.numeric(x[[weight.var]]))
    
    ## the length of the breaks must be less than or equal to the
    ## match.var
    
    stopifnot(length(breaks) <= length(match.var) | length(match.var) == 0)
    
    ## Create a portfolioMatch object.
    
    pm <- new("portfolioMatch",
              date.var        = date.var,
              ret.var         = ret.var,
              weights.orig    = x[[weight.var]],
              universe        = x
              )
    
    if (is.null(name)){
      pm@name <- "NA"
    } else {
      pm@name <- name}
    
    ## if match.var is not NULL, matching will be done based on the
    ## match.var provided.
    
    if (length(match.var) > 0){
      
      ## sort match.var based on alphabetical order
      
      match.var <- sort(match.var)
      .mv <- NULL
      for (i in 1:length(match.var)){
        .mv[i] <- class(x[[match.var[i]]])[1]}
      
      pm@match.var <- match.var[order(.mv, decreasing = TRUE)]
      
      ## separate continuous and categorical variables
      
      match.var.d <- c()
      match.var.c <- c()
      match.var.d <- match.var[.mv != "numeric"]
      match.var.c <- match.var[.mv == "numeric"]
      
      ## stop if breaks and the combination of cont. match var. and
      ## zero.var are not one-to-one or there is only one breaks
      ## number for all continuous variables
      
      stopifnot((length(match.var.c) + length(zero.var)) == length(breaks) | length(breaks) == 1)
      
      ## make breaks numbers and continuous match.var one-to-one
      
      breaks[1:(length(match.var.c)+length(zero.var))] <- breaks
      
      ## create a new column to contain all the quantile info in
      ## the input data frame
      
      if(!is.null(zero.var)){
        if (class(x[[zero.var]])[1] == "numeric"){
          match.var.c <- c(match.var.c, zero.var)
            for (k in 1:length(match.var.c)){
              .col.na <- paste(match.var.c[k], breaks[k], sep = "")
              x[[.col.na]] <- .quantile(x, match.var.c[k], breaks[k])
              match.var.d <- c(match.var.d, .col.na)}
        } else {

          ## if zerovar is discrete
          
          for (k in 1:length(match.var.c)){
            .col.na <- paste(match.var.c[k], breaks[k], sep = "")
            x[[.col.na]] <- .quantile(x, match.var.c[k], breaks[k])
            match.var.d <- c(match.var.d, .col.na)}
          
          match.var.d <- c(match.var.d, zero.var)}
      } else {
        for (k in 1:length(match.var.c)){
          .col.na <- paste(match.var.c[k], breaks[k], sep = "")
          x[[.col.na]] <- .quantile(x, match.var.c[k], breaks[k])
          match.var.d <- c(match.var.d, .col.na)}
      }
      
      ## separate original weight matching and active weight matching
      
      positive.act.weight.no <- which(x$actweight > 0)
      negative.act.weight.no <- which(x$actweight < 0)
      
      ## a weights.match matrix
      
      weights.match.mat <- matrix(NA, nrow(x), n)
      
      ## generate n samples of matching portfolios
      
      for (j in 1:n){
        
        aaa <- x
        aaa$new <-0
        aaa[positive.act.weight.no,]$new <- x[positive.act.weight.no,]$actweight
        
        nR <- nrow(aaa)
        nM <- length(match.var.d)
        exp.mtx.list <-  vector(nM, mode = 'list')
        names(exp.mtx.list) <-  match.var.d
        
        for(i in 1:nM){
          col.d             <- match.var.d[i]
          fac               <- aaa[[col.d]]
                exp.mtx.list[[i]] <- t(dummy(fac))
        }
        
        exposures.mtx.d0 <- do.call(rbind, exp.mtx.list)
        
        ## redundant constraint, remove the one category from each
        ## discrete var.
        
        for(i in 1:nM) exp.mtx.list[[i]] <- exp.mtx.list[[i]][-1, ]
        exposures.mtx.d <- do.call(rbind, exp.mtx.list)
              
        ## set target exposures
        
        weights.orig <- aaa$new
        targets.d <- exposures.mtx.d %*% weights.orig
        
        ## if numeric, quintiles; if not, count

        if(!is.null(zero.var)){
          if (class(x[[zero.var]])[1] == "numeric"){
            targets.d[(length(targets.d) - 8):(length(targets.d))] <- 0
          } else {
            targets.d[(length(targets.d) -
                       length(unique(x[[zero.var]])) + 2):(length(targets.d))] <- 0}
        } else {}
        
        ## set up a matrix with the first row with all 1s to ensure
        ## the sum of all matching weights to be 1, the follwing
        ## rows contain the information about continuous variables,
        ## and dummies for categorical variables.
        
        ones     <- rep(1, nR)
        Amat     <- rbind(ones, exposures.mtx.d)
        bvec     <- c(sum(aaa$new), targets.d)
        
        ## a process to generate random coefficients for an expression
        ## to be maximized
              
        score <- rnorm(dim(Amat)[2])
        
        ## run LP on positive act weight matching
        
        slp <- Rglpk_solve_LP(obj = score,
                              mat = Amat,
                              dir = rep("==", length(bvec)),
                              rhs = bvec,
                              bounds = list(lower = list(ind = 1:length(score),
                                              val = rep(0, length(score))),
                                upper = list(ind = 1:length(score),
                                  val = rep(max(aaa$new)/4, length(score)))))
        
        ## the solutions from the linear programming package
        
        weights.match <- slp$solution
        
        ## portfolios without the original holdings, match.weights > 0
        ## indicate the matching portfolio holding. aaa is the matched
        ## portfolio.
          
        aaa$new <- weights.match

        ## negative active weights matching

        bbb <- x
        bbb$new <-0
        bbb[negative.act.weight.no,]$new <- -(x[negative.act.weight.no,]$actweight)
        
        for(i in 1:nM){
          col.d             <- match.var.d[i]
          fac               <- bbb[[col.d]]
          exp.mtx.list[[i]] <- t(dummy(fac))
        }
        
        exposures.mtx.d0 <- do.call(rbind, exp.mtx.list)
        
        ## redundant constraint, remove the one category from each
        ## discrete var.
        
        for(i in 1:nM) exp.mtx.list[[i]] <- exp.mtx.list[[i]][-1, ]
        exposures.mtx.d <- do.call(rbind, exp.mtx.list)
        
        ## set target exposures
        
        weights.orig <- bbb$new
        targets.d <- exposures.mtx.d %*% weights.orig
        
        if(!is.null(zero.var)){
          if (class(x[[zero.var]])[1] == "numeric"){
            targets.d[(length(targets.d) - 8):(length(targets.d))] <- 0
          } else {
            targets.d[(length(targets.d) -
                           length(unique(x[[zero.var]])) + 2):(length(targets.d))] <- 0}
        } else {}
        
        ## set up a matrix with the first row with all 1s to ensure
        ## the sum of all matching weights to be 1, the follwing
        ## rows contain the information about continuous variables,
        ## and dummies for categorical variables.
        
        Amat     <- rbind(ones, exposures.mtx.d)
        bvec     <- c(sum(bbb$new), targets.d)
        
        score <- rnorm(dim(Amat)[2])
        
        ## run LP for negative active weights. bbb is a matrix
        ## for negative active weight matching
                        
        slp <- Rglpk_solve_LP(obj = score,
                              mat = Amat,
                              dir = rep("==", length(bvec)),
                              rhs = bvec,
                              bounds = list(lower = list(ind = 1:length(score),
                                              val = rep(0, length(score))),
                                upper = list(ind = 1:length(score),
                                  val = rep(max(bbb$new)/4, length(score)))))
        
        ## the solutions from the linear programming package for
        ## negative active weight matching
        
        weights.match2 <- -slp$solution
              
        ## portfolios without the original holdings, match.weights > 0
        ## indicate the matching portfolio holding. aaa is the matched
        ## portfolio.
        
        bbb$new <- weights.match2
        match.weight <- aaa$new + bbb$new
        
        weights.match.mat[ , j] <- match.weight
      }

      pm@weights.match <- weights.match.mat
      
      ## exposures.orig slot, actual exposures.
      
      exposures.orig <- lapply(1:length(match.var.d),
                               function(i){as.matrix(tapply(x[[weight.var]], x[[match.var.d[i]]], sum))})
      names(exposures.orig) <- match.var.d
      pm@exposures.orig <- exposures.orig
      
      ## exposures.match slot, showing actual exposures instead of active exposures
      
      exposures.match <- list()
      
      for (j in 1:length(match.var.d)){
        .b <- sapply(1:dim(weights.match.mat)[2],
                     function(i){as.matrix(tapply((weights.match.mat[ , i]), x[[match.var.d[j]]], sum))})
        rownames(.b) <- levels(x[[match.var.d[j]]])
        exposures.match[[j]] <- .b}
            
      names(exposures.match) <- match.var.d
      
      pm@exposures.match <- exposures.match
      
      ## Calculate orig and match active returns
      
      .re           <- matrix(NA, nrow = 1, ncol = 1)
      .returns      <- x[[ret.var]] %*% x[[weight.var]]
      .date         <- unique(as.character(x[[date.var]]))
      .re[1, 1]     <- .returns
      rownames(.re) <- .date
      
      pm@return.orig <- .re
      
      .re.mat <- t(as.matrix(sapply(1:n,
                                    function(i){x[[ret.var]] %*% weights.match.mat[ , i]})))
            rownames(.re.mat) <- .date
      
      pm@return.match <- .re.mat
      
      return(pm)
      
    } else {
      
      ## when match.var == NULL, generate a random portfolio with
      ## the same number of stocks in the original portfolio.
      
      ## weights.match slot is a matrix of nrow(x) times n (the number of samples drawn)
      
      weights.match.mat <- matrix(0, nrow(x), n)
      
      for (i in 1:n){
        
        ## the random portfolio has the same number of holdings as
        ## the original portfolio with the same weight attached.
        
        .random <- sample(c(1:nrow(x))[-which(x[[weight.var]] > 0)], length(which(x[[weight.var]] > 0)))
        weights.match.mat[, i][.random] <- x[[weight.var]][which(x[[weight.var]] > 0)]}
      
      pm@weights.match <- weights.match.mat
      
      ## display the match.var as country, sector, size, value,
      ## grwoth, quality, and technical. This is just for show only
      ## as some of the other functions require a positive length of
      ## match.var to work
      
      pm@match.var <- "random"

      match.var.d <- c("country", "sector")
      match.var.c <- c("size", "beta")
      
      breaks[1:length(match.var.c)] <- breaks
      
      ## exposures for continuous variables are displayed in
      ## deciles.
      
      for (k in 1:length(match.var.c)){
        .col.na <- paste(match.var.c[k], breaks[k], sep = "")
        x[[.col.na]] <- .quantile(x, match.var.c[k], breaks[k])
        match.var.d <- c(match.var.d, .col.na)}
      
      nM <- length(match.var.d)
      
      for(i in 1:nM){
        col.d             <- match.var.d[i]
        fac               <- x[[col.d]]
      }
      
      exposures.orig <- lapply(1:length(match.var.d),
                               function(i){as.matrix(tapply(x[[weight.var]], x[[match.var.d[i]]], sum))})
      names(exposures.orig) <- match.var.d
      pm@exposures.orig <- exposures.orig
      
      ## exposures.match slot
      
      exposures.match <- list()
      
      for (j in 1:length(match.var.d)){
            .b <- sapply(1:dim(weights.match.mat)[2],
                         function(i){as.matrix(tapply(weights.match.mat[ , i], x[[match.var.d[j]]], sum))})
            rownames(.b) <- levels(x[[match.var.d[j]]])
            exposures.match[[j]] <- .b}
      
      names(exposures.match) <- match.var.d
        
      pm@exposures.match <- exposures.match
      
      ## Calculate orig and match returns
      
      .re           <- matrix(NA, nrow = 1, ncol = 1)
      .returns      <- x[[ret.var]] %*% x[[weight.var]]
      .date         <- unique(as.character(x[[date.var]]))
      .re[1, 1]     <- .returns
      rownames(.re) <- .date
        
      pm@return.orig <- .re
      
      .re.mat <- t(as.matrix(sapply(1:n,
                                    function(i){x[[ret.var]] %*% weights.match.mat[ , i]})))
      rownames(.re.mat) <- .date
      
      pm@return.match <- .re.mat
      
      return(pm)
      
    }
    
  }
}
