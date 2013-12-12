##' R script for creating slopegraphs
##' James Keirstead
##' 16 July 2011
##' http://www.jameskeirstead.ca/r/slopegraphs-in-r/ 

##' Load in the health care data and required functions
data <- read.csv("cancer_survival_rates.csv")
source("slopegraph.r")

##' Convert raw data to right format
df <- build_slopegraph(data, x="year", y="value", group="group", method="tufte", min.space=0.05)

## Refactor the x-axis to get the right labels, round the y values for presentation
df <- transform(df, x=factor(x, levels=c(5,10,15,20),
                        labels=c("5 years", "10 years", "15 years", "20 years")),
                y=round(y))
##' Generate the raw plot
gg.form <- plot_slopegraph(df) +
    labs(title="Estimates of % survival rates") 

##' Save the results
w <- 6  	# width in inches
h <- 1.6*w	# height in inches
ggsave("slopegraph.pdf",gg.form,w=w,h=h)
dpi <- 150  	# resolution for png
ggsave("slopegraph.png",gg.form,w=w,h=h,dpi=dpi)

