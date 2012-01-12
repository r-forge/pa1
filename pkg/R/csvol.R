## cross-sectional volatility calculation

## form a class object csvol or csvolMulti


csvol <- function(x,
                  date.var = "date",
                  ret.var = "return",
                  uni.var = "benchmark"){
  ## checks

  dates <- unique(x[[date.var]])
  len <- length(dates)
  
  if (len > 1){
    .fun <- function(i){csvol(x[x[[date.var]] %in% i, ],
                              date.var,
                              ret.var,
                              uni.var)}
    multiples <- lapply(dates, .fun)
    csvol.multi <- new("csvolMulti",
                       date.var = as.character(dates),
                       ret.var = ret.var,
                       uni.var = uni.var,
                       universe = multiples)
    csv.mat <- matrix(NA, nrow = len, ncol = 1)
    csv.mat[ ,1] <- sapply(1:len, function(i){multiples[[i]]@csv})
    rownames(csv.mat) <- as.character(dates)
    colnames(csv.mat) <- "csv"

    csvol.multi@csv <- csv.mat
    return(csvol.multi)
    
  } else {
    object <- new("csvol",
                  date.var = date.var,
                  ret.var = ret.var,
                  uni.var = uni.var,
                  universe = x)

    object@csv <- 
      (sum(x[[uni.var]] * (x[[ret.var]] - mean(x[[ret.var]]))^2))^0.5

    return(object)
  }
  }                
