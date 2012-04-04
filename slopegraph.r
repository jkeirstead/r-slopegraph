### R script for creating slopegraphs
### James Keirstead
### 16 July 2011
### http://www.jameskeirstead.ca/r/slopegraphs-in-r/ 

### 0. Load necessary libraries
require(ggplot2)
require(grid)

### 1. Load in the health care data
data <- read.csv("cancer_survival_rates.csv")

### 2. Define a minimum spacing (5% of full data range)
min.space <- 0.05*diff(range(data$value))

### 3. Calculate the necessary offset between values.
### This function assumes that the input data frame
### represents a single year's worth of data
calcOffset <- function(df) {
  # Sort by value
  ord <- order(df$value,decreasing=T)
  # Calculate the difference between adjacent values
  delta <- -1*diff(df$value[ord])
  # Adjust to ensure that minimum space requirement is met 
  offset <- (min.space-delta)
  offset <- replace(offset,offset<0,0)
  # Add a trailing zero for the lowest value
  offset <- c(offset,0)
  # Calculate the offset needed to be added to each point
  # as a cumulative sum of previous values
  offset <- rev(cumsum(rev(offset)))
  # Assemble and return the new data frame
  df.new <- with(df,data.frame(group=group[ord],
                               year=year[ord],
                               value=value[ord],
                               offset))
  return(df.new)
}

### 4. Split the raw data by year and calculate the offsets
data <- ddply(data,.(year),calcOffset)

### 5. Define an empty plot theme
theme_empty <- function (base_size = 12, base_family = "") 
{
    structure(list(axis.line = theme_blank(),
                   axis.text.x = theme_text(family = base_family, 
                     size = base_size * 0.8, lineheight = 0.9, vjust = 1),
                   axis.text.y = theme_blank(),
                   axis.ticks = theme_blank(),
                   axis.title.x = theme_blank(),
                   axis.title.y = theme_blank(),
                   axis.ticks.length = unit(0, "lines"),
                   axis.ticks.margin = unit(0, "lines"), 
                   legend.background = theme_rect(colour = NA),
                   legend.key = theme_rect(colour = "grey80"), 
                   legend.key.size = unit(1.2, "lines"),
                   legend.key.height = NA, 
                   legend.key.width = NA,
                   legend.text = theme_text(family = base_family, 
                     size = base_size * 0.8),
                   legend.text.align = NA, 
                   legend.title = theme_text(family = base_family,
                     size = base_size * 0.8, face = "bold", hjust = 0),
                   legend.title.align = NA, 
                   legend.position = "right",
                   legend.direction = "vertical", 
                   legend.box = NA,
                   panel.background = theme_blank(),
                   panel.border = theme_blank(),
                   panel.grid.major = theme_blank(),
                   panel.grid.minor = theme_blank(),
                   panel.margin = unit(0.25, "lines"), 
                   strip.background = theme_blank(),
                   strip.text.x = theme_text(family = base_family,
                     size = base_size * 0.8),
                   strip.text.y = theme_blank(),
                   plot.background = theme_blank(),
                   plot.title = theme_text(family = base_family,
                     size = base_size * 1.2),
                   plot.margin = unit(c(1, 0.5, 0.5, 0.5), "lines")), 
        class = "options")
}

### 6. Make the plot
xvals <- sort(unique(data$year))
fontSize <- 2.5
gg <- ggplot(data,aes(x=year,y=value+offset)) +
  geom_line(aes(group=group),colour="grey80") +
  geom_point(colour="white",size=8) +
  geom_text(aes(label=round(value)),size=fontSize) +
  geom_text(aes(label=group,x=year-0.5),
            dat=subset(data,year==min(year)),
            hjust=1,size=fontSize) +
  scale_x_continuous(lim=range(data$year)-c(min(data$year),0),
                     breaks=xvals,
                     labels=paste(xvals,"years"))
gg.form <- gg + theme_empty()

### 7. Save the plot to PDF and PNG
w <- 6  	# width in inches
h <- 1.6*w	# height in inches
ggsave("slopegraph.pdf",gg.form,w=w,h=h)
dpi <- 150  	# resolution for png
ggsave("slopegraph.png",gg.form,w=w,h=h,dpi=dpi)

