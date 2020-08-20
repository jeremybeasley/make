#
# How plt parameter works to layout several charts in a single space.
#

# Start a blank plot region.
par(mar=c(0,0,0,0), bg="white")
plot(0:1, 0:1, type="n", xlab="", ylab="", axes=FALSE, asp=1)


### Stacked Charts ###

par(new=TRUE, plt=c(0.1, 0.9, 0.1, .25))
plot(0, 0, type="n", xlab="", ylab="", main="Bottom Plot", bty="n")

par(new=TRUE, plt=c(0.1, 0.9, 0.75, .9))
plot(0, 0, type="n", xlab="", ylab="", main="Top Plot", bty="n")

par(new=TRUE, plt=c(0.1, 0.9, 0.4, .55))
plot(0, 0, type="n", xlab="", ylab="", main="Middle Plot", bty="n")



### Three overlapping plots. ###

# Start a blank plot region.
par(mar=c(0,0,0,0), bg="white")
plot(0:1, 0:1, type="n", xlab="", ylab="", axes=FALSE, asp=1)

par(new=TRUE, plt=c(0.1, 0.9, 0.1, .5))
plot(0, 0, type="n", xlab="", ylab="", main="Bottom Plot", bty="n", col.axis="#f0f0f0", col.main="#f0f0f0")

par(new=TRUE, plt=c(0.1, 0.9, 0.3, .7))
plot(0, 0, type="n", xlab="", ylab="", main="Middle Plot", bty="n", col.axis="#777777", col.main="#777777")

par(new=TRUE, plt=c(0.1, 0.9, 0.5, .9))
plot(0, 0, type="n", xlab="", ylab="", main="Top Plot", bty="n")



#
# Load the data.
#
subreddits <- read.csv("data/subreddits.tsv", stringsAsFactors = FALSE, sep="\t")
subreddits[1:5, 1:5]



#
# Frequency trails
#


#### Plot two trails for proof of concept ####

num_plots <- 2
overlap <- 2
initheight <- 1 / (num_plots + overlap)

# Start a blank plot region.
par(mar=c(0,0,0,0), bg="white")
plot(0:1, 0:1, type="n", xlab="", ylab="", axes=FALSE, asp=1)

for (i in num_plots:1) {
    par(new=TRUE, plt=c(0, 1, initheight*(i-1), initheight*(i-1)+initheight*overlap) ) 
    plot(subreddits[,i], xlim=c(0, 180), ylim=c(0, max(subreddits[,i])), 
         xlab="", ylab="", type="n", axes=FALSE)
    
    polygon(c(1:154, 154:1), c(subreddits[,i],rep(0, 154)), col="gray", border=NA)
    lines(subreddits[,i], lwd=.8, col="black")
    text(154, max(subreddits[,i])/(overlap+2), colnames(subreddits)[i], pos=4, cex=.7)
}




#### Relative Scale ####

# Specify initial height and row overlap
overlap <- 4
initheight <- 1 / (dim(subreddits)[2]+overlap)

# Start a blank plot region.
par(mar=c(0,0,0,0), bg="white")
plot(0:1, 0:1, type="n", xlab="", ylab="", axes=FALSE, asp=1)

# Draw each multiple.
for (i in (dim(subreddits)[2]):1) {   # Draw in reverse order, so that bottom series appear in front.
    
    par(new=TRUE, plt=c(0, 1, initheight*(i-1), initheight*(i-1)+initheight*overlap) ) 
    plot(subreddits[,i], xlim=c(0, 180), ylim=c(0, max(subreddits[,i])), 
         xlab="", ylab="", type="n", axes=FALSE)
    
    polygon(c(1:154, 154:1), c(subreddits[,i],rep(0, 154)), col="gray", border=NA)
    lines(subreddits[,i], lwd=.8, col="black")
    text(154, max(subreddits[,i])/(overlap+2), colnames(subreddits)[i], pos=4, cex=.7)
}



#### Relative Scale, with Color Encoding ####

# Color scale based on quantiles.
nColors <- 10
pal <- colorRampPalette(c("#fdfd35", "#6364a9"))  # Purple
colors <- pal(nColors)
splits <- quantile(colSums(subreddits), probs=seq(0, 1, 1/(nColors-1)))

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
    
    # Get color.
    colIndex <- which(sum(subreddits[,i]) <= splits)[1]
    polygon(c(1:154, 154:1), c(subreddits[,i],rep(0, 154)), col=colors[colIndex], border=NA)
    
    # Line and label
    lines(subreddits[,i], lwd=.8, col="white")
    text(154, max(subreddits[,i])/(overlap+2), colnames(subreddits)[i], pos=4, cex=.7)
}




#### Absolute Scale ####

# Specify initial height and maximum overlap. Actual overlap will vary by series.
max_all <- max(subreddits)
max_overlap <- 10
initheight <- 1 / (dim(subreddits)[2]+max_overlap)

# Start a blank plot region.
par(mar=c(0,0,0,0), bg="white")
plot(0:1, 0:1, type="n", xlab="", ylab="", axes=FALSE, asp=1)

# Draw each multiple.
for (i in (dim(subreddits)[2]):1) {
    
    par(new=TRUE, plt=c(0, 1, initheight*(i-1), initheight*(i-1)+initheight*max_overlap) ) 
    plot(subreddits[,i], xlim=c(0, 180), ylim=c(0, max_all), 
         xlab="", ylab="", type="n", axes=FALSE)
    
    colIndex <- which(sum(subreddits[,i]) <= splits)[1]
    polygon(c(1:154, 154:1), c(subreddits[,i],rep(0, 154)), col=colors[colIndex], border=NA)
    lines(subreddits[,i], lwd=.8, col="#f0f0f0")
    text(154, max(subreddits[,i])/(max_overlap+2), colnames(subreddits)[i], pos=4, cex=.7)
}




#### Absolute Scale, Sorted in Decreasing Order ####

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




#### Make it a function. ####

source("src/plotFrequencyTrails.R")
plotFrequencyTrails(subreddits, showRelativeScale = TRUE)
plotFrequencyTrails(subreddits.o, overlap=10, showRelativeScale = FALSE)



