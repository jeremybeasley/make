##########################
# Gradient line tutorial
##########################



####### Basic gradient line. #######

# Basic line.
plot(NA, xlim=c(0,1), ylim=c(0,1))
lines(x = c(0, 1), y = c(.5, .5))

# Break it into two pieces.
plot(NA, xlim=c(0,1), ylim=c(0,1))
segments(x0=0, x1=.5, y0=.5, y1=.5, col="blue", lwd=2)
segments(x0=.5, x1=1, y0=.5, y1=.5, col="orange", lwd=2)

# One call to segments()
plot(NA, xlim=c(0,1), ylim=c(0,1))
segments(x0=c(0, .5), x1=c(.5, 1), 
         y0=c(.5, .5), y1=c(.5, .5),
         col=c("blue", "orange"),
         lwd=2)
segments(x0=c(0, .25, .5, .75), x1=c(.25, .5, .75, 1), 
         y0=c(.8, .8, .8, .8), y1=c(.8, .8, .8, .8),
         col=c("blue", "orange", "cyan", "black"),
         lwd=2)
segments(x0=c(0, .25, .5, .75), x1=c(.25, .5, .75, 1), 
         y0=c(.2, .2, .2, .2), y1=c(.2, .2, .2, .2),
         col=c("#f0f0f0", "#aaaaaa", "#777777", "#000000"),
         lwd=4)



####### Finer gradient #######


#
# Define a color scheme.
#

breaks <- c(-100, -50, 0, 50, 100)
shades <- c("#f0f0f0", "#aaaaaa", "#777777", "#000000")
symbols(c(1,2,3,4), c(1,1,1,1), 
        squares=c(1,1,1,1), bg=shades, 
        xlim=c(0,5),
        inches=FALSE)


# Find a shade for a given value.
val <- 10
i <- min(which(breaks > val)) - 1
shades[i]

# Shades for several values.
values <- c(-60, -50, -30, 0, 10, 20, 50, -60)
segcols <- sapply(values, function(val) {
    i <- min(which(breaks > val)) - 1
    return(shades[i])
})

# Draw the line with segments.
plot(values, type="n")
segments(x0=1:(length(values)-1),
         x1=2:length(values),
         y0=values[-length(values)],
         y1=values[-1],
         col=segcols, lwd=4)


#
# Define a color gradient with colorRampPalette()
#

#
# Gradient for one segment.
#

plot(values, type="n")

# In between coordinates.
nterp <- 50
x <- seq(1, 2, length.out=nterp)
y <- seq(values[1], values[2], length.out=nterp)

# Start and finish colors.
shade_start <- segcols[1]
shade_end <- segcols[2]

# Color palette
pal <- colorRampPalette(c(shade_start, shade_end))
inbetween_colors <- pal(nterp)

# Segments.
segments(x0=x[1:(nterp-1)],
         x1=x[2:nterp],
         y0=y[1:(nterp-1)],
         y1=y[2:nterp],
         col=inbetween_colors, lwd=4)




#
# All the segments.
#
plot(values, type="n")
for (i in 2:length(values)) {
    
    # In between coordinates.
    x <- seq((i-1), i, length.out=nterp)
    y <- seq(values[i-1], values[i], length.out=nterp)

    # Start and finish colors.
    shade_start <- segcols[i-1]
    shade_end <- segcols[i]
    
    # Color palette
    pal <- colorRampPalette(c(shade_start, shade_end))
    inbetween_colors <- pal(nterp)
    
    # Segments.
    segments(x0=x[1:(nterp-1)],
             x1=x[2:nterp],
             y0=y[1:(nterp-1)],
             y1=y[2:nterp],
             col=inbetween_colors, lwd=4)
}




#
# Generalize.
#


gradientLine <- function(x, y, breaks, shades, nterp, lwd=2) {
    
    segcols <- sapply(y, function(val) {
        i <- min(which(breaks > val))-1
        return(shades[i])
    })
        
    for (i in 2:length(x)) {
        
        # In between coordinates.
        x_curr <- seq(x[i-1], x[i], length.out=nterp)
        y_curr <- seq(y[i-1], y[i], length.out=nterp)
        
        # Start and finish colors.
        shade_start <- segcols[i-1]
        shade_end <- segcols[i]
        
        # Color palette
        pal <- colorRampPalette(c(shade_start, shade_end))
        inbetween_colors <- pal(nterp)
        
        # Segments.
        segments(x0=x_curr[1:(nterp-1)],
                 x1=x_curr[2:nterp],
                 y0=y_curr[1:(nterp-1)],
                 y1=y_curr[2:nterp],
                 col=inbetween_colors, lwd=lwd)
    }
    
}

# Try the function.
breaks <- c(-100, -50, 0, 50, 100)
shades <- c("#f0f0f0", "#aaaaaa", "#777777", "#000000")
plot(1:length(values), values, type="n")
gradientLine(1:length(values), values, 
             breaks=breaks, shades=shades, nterp=40, lwd=10)





####### Try it with real data. #######

#
# Load data.
#
births <- read.csv("natality-2003-2021.csv", stringsAsFactors = FALSE)
births$monthnum <- c(rep(1:12, 18), 1:6)

#
# Calculations for baseline chart.
#

# 2003 base.
births2003 <- births$Births[births$Year == 2003]

# Calculate change since 2003 during same month.
births$change_since2003 <- apply(births, 1, function(x) {
    curr_births <- as.numeric(x[3])
    from2003 <- births2003[as.numeric(x[4])]
    return( (curr_births - from2003) / from2003 )
})


#
# Plot it.
#
shades <- c('#00aab7', '#77c2ca', '#b6d9dd', '#f0f0f0', '#f0bda6', '#e48b5f', '#d05716')
breaks <- c(-20, -7.5, -5, -2.5, 2.5, 5, 7.5, 20)
x <- 13:dim(births)[1]
y <- 100*births$change_since2003[13:dim(births)[1]]
plot(x, y, xlab="Months Since January 2003", ylab="% Difference", type="n")
gradientLine(x, y, breaks=breaks, shades=shades, nterp=20, lwd=3)


