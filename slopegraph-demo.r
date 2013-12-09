##' R script for creating slopegraphs
##' James Keirstead
##' 16 July 2011
##' http://www.jameskeirstead.ca/r/slopegraphs-in-r/ 

##' Load in the health care data and required functions
data <- read.csv("cancer_survival_rates.csv")
source("slopegraph.r")

##' Convert raw data to right format
df <- build_slopegraph(data, x="year", y="value", group="group")

##' Generate the raw plot
xvals <- unique(df$x)
gg.form <- plot_slopegraph(df) +
    scale_x_continuous(lim=range(df$x)-c(min(df$x),0),
                       breaks=xvals,
                       labels=paste(xvals, "years"))

##' Save the results
w <- 6  	# width in inches
h <- 1.6*w	# height in inches
ggsave("slopegraph.pdf",gg.form,w=w,h=h)
dpi <- 150  	# resolution for png
ggsave("slopegraph.png",gg.form,w=w,h=h,dpi=dpi)

