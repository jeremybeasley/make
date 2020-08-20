# load data 
subreddits <- read.csv("data/subreddits.tsv", stringsAsFactors=FALSE, sep="\t")

# plot regions 
# There are two main steps:
# 1. Start a blank plot region.
# 2. Add new plots to the region, specifying the space it should occupy.

# Start a blank plot region.
par(mar=c(0,0,0,0), bg="white")
plot(0:1, 0:1, type="n", xlab="", ylab="", axes=FALSE, asp=1)

par(new=TRUE, plt=c(0.1, 0.9, 0.1, .25))
plot(0, 0, type="n", xlab="", ylab="", main="Bottom Plot", bty="n")

# add another plot and change the y-axis 
# Top plot
par(new=TRUE, plt=c(0.1, 0.9, 0.75, .9))
plot(0, 0, type="n", xlab="", ylab="", main="Top Plot", bty="n")

# middle plot 
# Middle plot
par(new=TRUE, plt=c(0.1, 0.9, 0.4, .55))
plot(0, 0, type="n", xlab="", ylab="", main="Middle Plot", bty="n")


# Overlap charts 

# Start a blank plot region.
par(mar=c(0,0,0,0), bg="white")
plot(0:1, 0:1, type="n", xlab="", ylab="", axes=FALSE, asp=1)

par(new=TRUE, plt=c(0.1, 0.9, 0.1, .5))
plot(0, 0, type="n", xlab="", ylab="", main="Bottom Plot", bty="n", col.axis="#f0f0f0", col.main="#f0f0f0")

# Middle plot 
par(new=TRUE, plt=c(0.1, 0.9, 0.3, .7))
plot(0, 0, type="n", xlab="", ylab="", main="Middle Plot", bty="n", col.axis="#777777", col.main="#777777")

# Top plot 
par(new=TRUE, plt=c(0.1, 0.9, 0.5, .9))
plot(0, 0, type="n", xlab="", ylab="", main="Top Plot", bty="n")


# calculating the level of overlap — proof of concept 
# Knowing how to make plots that overlap on each other, the key is to calculate the amount of overlap. Or rather, you need to calculate what to pass to the plt parameter, given the amount of overlap you want for each plot and the number of plots.

# set the number of plots and overlap 
num_plots <- 2
overlap <- 2

# The overlap value refers to the relative amount one plot will go over the other compared to no overlapping. For example, a value of 1 means a single plot will maintain its initial height. It would look like that first layout of plots you made. No overlap. However, a value of 2 means a plot is twice the vertical height of its initial. Fifty percent of the plot’s area will overlap.

# set the initial height of each row
# the blank region spans 0 to 1 vertically, and you need to divide that area equally for each plot you add.
initheight <- 1 / (num_plots + overlap)

# Start a blank plot region.
par(mar=c(0,0,0,0), bg="white")
plot(0:1, 0:1, type="n", xlab="", ylab="", axes=FALSE, asp=1)

i <- 1

par(new=TRUE, plt=c(0, 1, initheight*(i-1), initheight*(i-1)+initheight*overlap) )

plot(subreddits[,i], xlim=c(0, 180), ylim=c(0, max(subreddits[,i])),
     xlab="", ylab="", type="n", axes=FALSE)

# You won’t see anything yet, because type is set to “n”, which means don’t draw anything. Just set up the space.

polygon(c(1:154, 154:1), c(subreddits[,i],rep(0, 154)), col="gray", border=NA)

#draw border line
lines(subreddits[,i], lwd=.8, col="black")

# add text label
text(154, max(subreddits[,i])/(overlap+2), colnames(subreddits)[i], pos=4, cex=.7)

# now wrap all of this in a for-loop 

for (i in 1:num_plots) {
  par(new=TRUE, plt=c(0, 1, initheight*(i-1), initheight*(i-1)+initheight*overlap) )
  plot(subreddits[,i], xlim=c(0, 180), ylim=c(0, max(subreddits[,i])),
       xlab="", ylab="", type="n", axes=FALSE)
  
  polygon(c(1:154, 154:1), c(subreddits[,i],rep(0, 154)), col="gray", border=NA)
  lines(subreddits[,i], lwd=.8, col="black")
  text(154, max(subreddits[,i])/(overlap+2), colnames(subreddits)[i], pos=4, cex=.7)
}

# reverse the iteration order so bottom plots are drawn last (and show on top)


for (i in num_plots:1) {
  par(new=TRUE, plt=c(0, 1, initheight*(i-1), initheight*(i-1)+initheight*overlap) )
  plot(subreddits[,i], xlim=c(0, 180), ylim=c(0, max(subreddits[,i])),
       xlab="", ylab="", type="n", axes=FALSE)
  
  polygon(c(1:154, 154:1), c(subreddits[,i],rep(0, 154)), col="gray", border=NA)
  lines(subreddits[,i], lwd=.8, col="black")
  text(154, max(subreddits[,i])/(overlap+2), colnames(subreddits)[i], pos=4, cex=.7)
}


## Iterate on this. Refine. 

# You can expand this to all of the data. Bump up the overlap to 4 and set the initHeight based on the total number of columns in the dataset:

# Specify initial height and row overlap
overlap <- 4
initheight <- 1 / (dim(subreddits)[2]+overlap)

# Start a blank plot region.
par(mar=c(0,0,0,0), bg="white")
plot(0:1, 0:1, type="n", xlab="", ylab="", axes=FALSE, asp=1)

# Draw each multiple.
for (i in (dim(subreddits)[2]):1) {
  
  par(new=TRUE, plt=c(0, 1, initheight*(i-1), initheight*(i-1)+initheight*overlap) )
  plot(subreddits[,i], xlim=c(0, 180), ylim=c(0, max(subreddits[,i])),
       xlab="", ylab="", type="n", axes=FALSE)
  
  polygon(c(1:154, 154:1), c(subreddits[,i],rep(0, 154)), col="gray", border=NA)
  lines(subreddits[,i], lwd=.8, col="black")
  text(154, max(subreddits[,i])/(overlap+2), colnames(subreddits)[i], pos=4, cex=.7)
}


## Color encoding 

# Color scale based on quantiles.
nColors <- 10
pal <- colorRampPalette(c("#fdfd35", "#6364a9"))  # Purple
colors <- pal(nColors)
splits <- quantile(colSums(subreddits), probs=seq(0, 1, 1/(nColors-1)))

# color based on quantiles 
# TODO: play around with different color combinations (also update after you've done color scale tutorials)

# Draw each multiple.
for (i in (dim(subreddits)[2]):1) {
  
  par(new=TRUE, plt=c(0, 1, initheight*(i-1), initheight*(i-1)+initheight*overlap) )
  plot(subreddits[,i], xlim=c(0, 180), ylim=c(0, max(subreddits[,i])),
       xlab="", ylab="", type="n", axes=FALSE)
  
  # Get color.
  colIndex <- which(sum(subreddits[,i]) <= splits)[1]
  polygon(c(1:154, 154:1), c(subreddits[,i],rep(0, 154)), col=colors[colIndex], border=NA)
  
  # Line and label
  lines(subreddits[,i], lwd=.8, col="white")
  text(154, max(subreddits[,i])/(overlap+2), colnames(subreddits)[i], pos=4, cex=.7)
}

## Change to absolute scale — same y-axis across rows / subreddits 

# Specify initial height and maximum overlap. Actual overlap will vary by series.
max_all <- max(subreddits)
max_overlap <- 10
initheight <- 1 / (dim(subreddits)[2]+max_overlap)

# Start a blank plot region.
par(mar=c(0,0,0,0), bg="white")
plot(0:1, 0:1, type="n", xlab="", ylab="", axes=FALSE, asp=1)

# Draw each multiple but set ylim differently
for (i in (dim(subreddits)[2]):1) {
  
  par(new=TRUE, plt=c(0, 1, initheight*(i-1), initheight*(i-1)+initheight*overlap) )
  plot(subreddits[,i], xlim=c(0, 180), ylim=c(0, max_all),
       xlab="", ylab="", type="n", axes=FALSE)
  
  # Get color.
  colIndex <- which(sum(subreddits[,i]) <= splits)[1]
  polygon(c(1:154, 154:1), c(subreddits[,i],rep(0, 154)), col=colors[colIndex], border=NA)
  
  # Line and label
  lines(subreddits[,i], lwd=.8, col="white")
  text(154, max(subreddits[,i])/(overlap+2), colnames(subreddits)[i], pos=4, cex=.7)
}


# Sort based on the totals so that big areas in the middle don't overwhelm the chart
subreddits.o <- subreddits[,order(colSums(subreddits))]

# Start a blank plot region.
par(mar=c(0,0,0,0), bg="white")
plot(0:1, 0:1, type="n", xlab="", ylab="", axes=FALSE, asp=1)

# Draw each multiple.
for (i in (dim(subreddits.o)[2]):1) {
  
  curr_max <- max(subreddits.o[,i])
  
  par(new=TRUE, plt=c(0, 1, initheight*(i-1), initheight*(i-1)+initheight*max_overlap) ) 
  plot(subreddits.o[,i], xlim=c(0, 180), ylim=c(0, max_all), 
       xlab="", ylab="", type="n", axes=FALSE)
  
  colIndex <- which(sum(subreddits.o[,i]) <= splits)[1]
  polygon(c(1:154, 154:1), c(subreddits.o[,i],rep(0, 154)), col=colors[colIndex], border=NA)
  lines(subreddits.o[,i], lwd=.8, col="#f0f0f0")
  text(154, max(subreddits.o[,i])/(max_overlap+2), colnames(subreddits.o)[i], pos=4, cex=.7)
}


#### Flip axis and labels orientation ####

# Start a blank plot region.
par(mar=c(0,0,0,0), bg="white", las=1)
plot(0:1, 0:1, type="n", xlab="", ylab="", axes=FALSE, asp=1)

# Draw each multiple.
for (i in (dim(subreddits.o)[2]):1) {
  
  curr_max <- max(subreddits.o[,i])
  
  par(new=TRUE, plt=c(0, 1, initheight*(i-1), initheight*(i-1)+initheight*max_overlap) ) 
  plot(subreddits.o[,i], xlim=c(-24, 160), ylim=c(0, max_all), 
       xlab="", ylab="", type="n", axes=FALSE)
  
  # Draw an axis for the first (largest) plot.
  if (i == dim(subreddits.o)[2]) {
    axis(4, at=seq(0, 5000000, by=1000000), labels=paste(0:5, "M", sep=""), 
         line=-2.5, lwd = 0, lwd.ticks=.5, cex.axis=.7)
  }
  
  colIndex <- which(sum(subreddits.o[,i]) <= splits)[1]
  polygon(c(1:154, 154:1), c(subreddits.o[,i],rep(0, 154)), col=colors[colIndex], border=NA)
  lines(subreddits.o[,i], lwd=.8, col="#f0f0f0")
  text(0, 0, colnames(subreddits.o)[i], pos=2, cex=.7)
}


##  Wrap up 

## The main challenge in this tutorial was to figure out how to overlap multiple charts. Once you got that, you had to figure out how to calculate the amount of overlap and placement. You started with two and then generalized for the full dataset.

## This is typically my strategy for any new chart type. I start with a single category or series and then two, three, so on and so forth. After I figure out the geometry, I work out the kinks, adjust, and try to generalize for more datasets. Sometimes I work on optimization, but I rarely find it necessary for my purposes when I’m working in R.

## Finally, as with all charts, this isn’t the only way to make frequency trails in R. I tend to go with base R, because that’s what I’m comfortable with, but if you want a ggplot solution, check out Claus Wilke’s ggridges package.

## TODO: play around w/ GG Ridges