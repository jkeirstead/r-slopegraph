##' @title R script for creating slopegraphs
##' @author James Keirstead
##' 12 December 2013
##' http://www.jameskeirstead.ca/r/slopegraphs-in-r/ 

##' Build a slopegraph data set
##'
##' Modifies a data frame so that it can be used for plotting by
##' \code{plot_slopegraph}.  The general structure of a slopegraph is
##' \itemize{
##' \item a factor giving the group labels
##' \item an ordered factor giving the x intervals
##' \item a numeric giving the y values
##' }
##' 
##' @param df the raw data frame
##' @param x a character giving the name of the x-axis column.  This
##' column must be an ordered factor.
##' @param y a character giving the name of the y-axis column.  This
##' column must be a numeric.
##' @param group a character giving the name of the group column.
##' This column must be a factor.
##' @param method a character string indicating which method to use to
##' calculate the position of elements.  Values include "tufte"
##' (default), "spaced", "rank", "none".
##' @param min.space fraction of total data range to leave as a
##' minimum gap (default = 0.05, only used by methods \code{spaced}
##' and \code{tufte})
##' @details The \code{method} option allows the y-position of the
##' elements to be calculated using different assumptions.  These are:
##' \itemize{ \item \code{tufte} Values in the first x-column are
##' sorted based on their numeric value.  Subsequent group lines are
##' then shifted to ensure that the lines for two adjacent groups
##' never cross.  Vertical positions in subsequent columns are only
##' meaningful relative to the first entry in that group.  \item
##' \code{spaced} The vertical position of each element is chosen to
##' ensure a minimum spacing between all elements and preserving the
##' rank order within columns.  Group lines can cross.  \item
##' \code{rank} The vertical position of each element represents its
##' rank within the column.  \item \code{none} The vertical position
##' of each element is based solely on its value }
##' @return a data frame with labelled columns, group, x, y, and ypos
build_slopegraph <- function(df, x, y, group, method="tufte", min.space=0.05) {

    ## First rename the columns for consistency
    ids <- match(c(x, y, group), names(df))
    df <- df[,ids]
    names(df) <- c("x", "y", "group")

    ## Expand grid to ensure every combination has a defined value
    tmp <- expand.grid(x=unique(df$x), group=unique(df$group))
    tmp <- merge(df, tmp, all.y=TRUE)
    df <- mutate(tmp, y=ifelse(is.na(y), 0, y))

    ## Then select and apply the appropriate method
    if (method=="spaced") {
        df <- spaced_sort(df, min.space=min.space)
        return(df)
    } else if (method=="none") {
        df <- mutate(df, ypos=y)               
        return(df)
    } else if (method=="rank") {
        df <- ddply(df, .(x), summarize, x=x, y=y, group=group, ypos=rank(y))
        return(df)
    } else if (method=="tufte") {
        df <- tufte_sort(df, min.space=min.space)
        return(df)
    } else {
        template <- "Method '%s' currently unsupported."
        warning(sprintf(template, method))
    }
}

##' Spaced sort for slopegraphs
##'
##' Calculates the position of each element to ensure a minimum
##' space between adjacent entries within a column, while preserving
##' rank order.  Group lines can cross
##' @param df the raw data frame
##' @param min.space fraction of total data range to leave as a
##' minimum gap
##' @return a data frame with the ypos column added
spaced_sort <- function(df, min.space=0.05) {
    ## Define a minimum spacing (5% of full data range)
    min.space <- min.space*diff(range(df$y))

    ## Transform the data
    df <- ddply(df, .(x), calc_spaced_offset, min.space)
    return(df)
}

##' Calculates the vertical offset between successive data points
##' 
##' @param df a data frame representing a single year of data
##' @param min.space the minimum spacing between y values
##' @return a data frame
calc_spaced_offset <- function(df, min.space) {

    ## Sort by value
    ord <- order(df$y, decreasing=T)
    ## Calculate the difference between adjacent values
    delta <- -1*diff(df$y[ord])
    ## Adjust to ensure that minimum space requirement is met 
    offset <- (min.space - delta)
    offset <- replace(offset, offset<0, 0)
    ## Add a trailing zero for the lowest value
    offset <- c(offset, 0)
    ## Calculate the offset needed to be added to each point
    ## as a cumulative sum of previous values
    offset <- rev(cumsum(rev(offset)))
    ## Assemble and return the new data frame
    df.new <- data.frame(group=df$group[ord],
                         x=df$x[ord],
                         y=df$y[ord],
                         ypos=offset+df$y[ord])
  return(df.new)
}


##' Calculates slope graph positions based on Edward Tufte's layout
##'
##' @param df the raw data frame with named x, y, and group columns
##' @param min.space fraction of total data range to leave as a
##' minimum gap
##' @return a data frame with an additional calculate ypos column
tufte_sort <- function(df, min.space=0.05) {

    ## Cast into a matrix shape and arrange by first column
    require(reshape2)
    tmp <- dcast(df, group ~ x, value.var="y")
    ord <- order(tmp[,2])
    tmp <- tmp[ord,]
    
    min.space <- min.space*diff(range(tmp[,-1]))
    yshift <- numeric(nrow(tmp))
    ## Start at "bottom" row
    ## Repeat for rest of the rows until you hit the top
    for (i in 2:nrow(tmp)) {
        ## Shift subsequent row up by equal space so gap between
        ## two entries is >= minimum
        mat <- as.matrix(tmp[(i-1):i, -1])
        d.min <- min(diff(mat))
        yshift[i] <- ifelse(d.min < min.space, min.space - d.min, 0)
    }

    
    tmp <- cbind(tmp, yshift=cumsum(yshift))

    scale <- 1
    tmp <- melt(tmp, id=c("group", "yshift"), variable.name="x", value.name="y")
    ## Store these gaps in a separate variable so that they can be scaled ypos = a*yshift + y

    tmp <- transform(tmp, ypos=y + scale*yshift)
    return(tmp)
   
}


##' A theme for plotting slopegraphs
##'
##' @param base_size a numeric giving the base font size
##' @param base_family a string giving the base font family
##' @import grid
theme_slopegraph <- function (base_size = 12, base_family = "") {
    require(grid)
    theme(axis.line = element_blank(),
          axis.text = element_text(colour="black"),
          axis.text.x = element_text(size = rel(1), lineheight = 0.9,
              vjust = 1),
          axis.text.y = element_text(size=rel(0.8)),
          axis.ticks = element_blank(),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          axis.ticks.length = unit(0, "lines"),
          axis.ticks.margin = unit(0, "lines"), 
          panel.background = element_blank(),
          panel.border = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.margin = unit(0.25, "lines"), 
          strip.background = element_blank(),
          strip.text.x = element_text(size = rel(0.8)),
          strip.text.y = element_blank(),
          plot.background = element_blank(),
          plot.title = element_text(size = rel(1)),
          plot.margin = unit(c(1, 0.5, 0.5, 0.5), "lines"),
          complete=FALSE)
}


##' Plots a slopegraph
##'
##' @param df a data frame giving the data
##' @return a ggplot object
##' @import ggplot2
plot_slopegraph <- function(df) {
    ylabs <- subset(df, x==head(x,1))$group
    yvals <- subset(df, x==head(x,1))$ypos
    fontSize <- 2.5
    gg <- ggplot(df,aes(x=x,y=ypos)) +
        geom_line(aes(group=group),colour="grey80") +
        geom_point(colour="white",size=8) +
        geom_text(aes(label=y),size=fontSize) +
        scale_y_continuous(name="", breaks=yvals, labels=ylabs)
    gg.form <- gg + theme_slopegraph()
    return(gg.form)
}
    

