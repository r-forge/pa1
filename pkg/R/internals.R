## Yang Lu Yang.Lu@williams.edu

## a collection of internal functions used in the package


## This helper function uses two arguments, vdep and vexp, to make a
## string representing a formula format appropriate for a linear
## model: "vdep ~ vexp[[1]] + vexp[[2]] + ... + vexp[[n]]"
.formula.make <- function(vdep, vexp) {
  formula(paste(vdep, "~", paste(vexp, collapse = " + ")))
}

## a convenience function to get the weight of each sector 
.sector.weight <- function(x,
                           sector.var,
                           all.sector,
                           ret.var,
                           var){
  weight <- sapply(1:length(all.sector),
                   function(i){x[x[[sector.var]] == all.sector[i], ][[ret.var]] %*%
                                 x[x[[sector.var]] == all.sector[i], ][[var]]})
  weight <- as.array(weight)
  
  names(weight) <- all.sector

  return(weight)
}



## An internal function to abstract the plotting of a bar plot for
## either exposure or return in a single period.

.bar.plot <- function(df,
                      type,  ## for ylab, either exposure or return
                      title
                      ){
  bar.plot <- ggplot(df, aes(x = Name, y = Value, fill = Type)) +
    geom_bar(width = 0.5, position = position_dodge()) + coord_flip() +
      ylab(type) + xlab("Sector") +
        geom_hline(yintercept = 0) +
          opts(panel.background = theme_blank(),
               title = title, 
               axis.line = theme_blank(),
               panel.grid.minor = theme_blank(),
               panel.grid.major = theme_blank(),
               plot.background = theme_rect(fill = NA, colour = NA))
  
  return(bar.plot)
  
}



## faceted plot

.facet.plot <- function(df,
                        type,
                        title){
  facet.plot <- ggplot(df, aes(Name, Value, fill = Type)) +
    geom_bar(position = position_dodge()) + coord_flip() + theme_bw()+
      facet_wrap( ~ Date) + scale_x_discrete(name = "Sector") + ylab(type) +
        opts(title = title)
  return(facet.plot)
}
