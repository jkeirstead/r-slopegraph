### R script for creating slopegraphs
### James Keirstead
### 16 July 2011
### http://www.jameskeirstead.ca/r/slopegraphs-in-r/ 

### 1. Load in the health care data
data <- read.csv("cancer_survival_rates.csv")

## Convert into the right format
df <- build_slopegraph(data, x="year", y="value", group="group")


gg.form <- plot_slopegraph(df)

### 7. Save the plot to PDF and PNG
w <- 6  	# width in inches
h <- 1.6*w	# height in inches
ggsave("slopegraph.pdf",gg.form,w=w,h=h)
dpi <- 150  	# resolution for png
ggsave("slopegraph.png",gg.form,w=w,h=h,dpi=dpi)

