### R script for creating slopegraphs
### James Keirstead
### 16 July 2011
### http://www.jameskeirstead.ca/r/slopegraphs-in-r/ 


##' Calculates the vertical offset between successive data points
##' 
##' @param df a data frame representing a single year of data
##' @param x a character giving the name of the x-axis column
##' @param y a character giving the name of the y-axis column
##' @param group a character giving the name of the group column
##' @param min.space the minimum spacing between y values
##' @return a data frame
calcOffset <- function(df, x, y, group, min.space) {

    ## Sort by value
    ord <- order(df[[y]], decreasing=T)
    ## Calculate the difference between adjacent values
    delta <- -1*diff(df[[y]][ord])
    ## Adjust to ensure that minimum space requirement is met 
    offset <- (min.space - delta)
    offset <- replace(offset, offset<0, 0)
    ## Add a trailing zero for the lowest value
    offset <- c(offset, 0)
    ## Calculate the offset needed to be added to each point
    ## as a cumulative sum of previous values
    offset <- rev(cumsum(rev(offset)))
    ## Assemble and return the new data frame
    df.new <- data.frame(group=df[[group]][ord],
                         x=df[[x]][ord],
                         y=df[[y]][ord],
                         offset)
  return(df.new)
}


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
##' @param x a character giving the name of the x-axis column
##' @param y a character giving the name of the y-axis column
##' @param group a character giving the name of the group column
##' @param method a character string indicating which method to use to
##' calculate the position of elements.  Values include "rank", "tufte", "spaced", "none".  
##' @note The \code{method} option allows the y-position of the elements to be calculated using different assumptions.  These are:
##' \itemize{
##' \item \code{rank} the vertical position of each element represents its rank within the column
##' \item \code{tufte} the vertical position of each element in the first column only is sorted based on the numeric value.  There is no overlapping lines between adjacent groups.  Vertical positions in subsequent columns are only meaningful relative to the first entry in that group.
##' \item \code{spaced} the vertical position of each element is chosen to ensure a minimum spacing between all elements and preserving the rank order within columns.  Group lines can cross.
##' \item \code{none} the vertical position of each element is based solely on its value
##' }
##' @return a data frame with labelled columns, group, x, y, and offset
build_slopegraph <- function(df, x, y, group, method="spaced") {

    if (method=="spaced") {
        ## Define a minimum spacing (5% of full data range)
        min.space <- 0.05*diff(range(df[[y]]))

        ## Transform the data
        df <- ddply(df, c(x), calcOffset, x, y, group, min.space)

        ## Return the tidied result
        return(df)
    } else if (method=="none") {
        ids <- match(c(x, y, group), names(df))
        names(df)[ids] <- c("x", "y", "group")
        df <- cbind(df, offset=0)               
        return(df)
    } else {
        template <- "Method '%s' currently unsupported."
        warning(sprintf(template, method))
    }
}

##' A theme for plotting slopegraphs
##'
##' @param base_size a numeric giving the base font size
##' @param base_family a string giving the base font family
##' @import grid
theme_slopegraph <- function (base_size = 12, base_family = "") {
    require(grid)
    theme(axis.line = element_blank(),
                   axis.text.x = element_text(family = base_family, 
                     size = base_size * 0.8, lineheight = 0.9, vjust = 1),
                   axis.text.y = element_blank(),
                   axis.ticks = element_blank(),
                   axis.title.x = element_blank(),
                   axis.title.y = element_blank(),
                   axis.ticks.length = unit(0, "lines"),
                   axis.ticks.margin = unit(0, "lines"), 
                   legend.background = element_rect(colour = NA),
                   legend.key = element_rect(colour = "grey80"), 
                   legend.key.size = unit(1.2, "lines"),
                   legend.key.height = NULL, 
                   legend.key.width = NULL,
                   legend.text = element_text(family = base_family, 
                     size = base_size * 0.8),
                   legend.text.align = NULL, 
                   legend.title = element_text(family = base_family,
                     size = base_size * 0.8, face = "bold", hjust = 0),
                   legend.title.align = NULL, 
                   legend.position = "right",
                   legend.direction = "vertical", 
                   legend.box = NULL,
                   panel.background = element_blank(),
                   panel.border = element_blank(),
                   panel.grid.major = element_blank(),
                   panel.grid.minor = element_blank(),
                   panel.margin = unit(0.25, "lines"), 
                   strip.background = element_blank(),
                   strip.text.x = element_text(family = base_family,
                     size = base_size * 0.8),
                   strip.text.y = element_blank(),
                   plot.background = element_blank(),
                   plot.title = element_text(family = base_family,
                     size = base_size * 1.2),
                   plot.margin = unit(c(1, 0.5, 0.5, 0.5), "lines"),
          complete=FALSE)
}


##' Plots a slopegraph
##'
##' @param data a data frame giving the data
##' @return a ggplot object
##' @import ggplot2
plot_slopegraph <- function(data) {
    xvals <- sort(unique(data[["x"]]))
    xlim <- range(as.numeric(data[["x"]])) 
    fontSize <- 2.5
    gg <- ggplot(data,aes(x=x,y=y+offset)) +
        geom_line(aes(group=group),colour="grey80") +
        geom_point(colour="white",size=8) +
        geom_text(aes(label=round(y)),size=fontSize) +
        geom_text(aes(label=group, x=as.numeric(x)-0.5),
                      dat=subset(data, x==head(x, 1)),
                      hjust=1, size=fontSize) 
    gg.form <- gg + theme_slopegraph()
    return(gg.form)
}
    

