# basic line
plot(NA, xlim=c(0,1), ylim=c(0,1))
lines(x=c(0,1), y=c(0.5,0.5))

# line as segments 
plot(NA, xlim=c(0,1), ylim=c(0,1))
segments(x0=0, x1=.5, y0=.5, y1=.5, col="blue", lwd=2)
segments(x0=.5, x1=1, y0=.5, y1=.5, col="orange", lwd=2)

# multiple segments with one call 

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



# The last group of gray segments is key. We just have to figure out how to do that with a bunch of segments and actual data. Ideally we wouldn’t have to manually specify the color of every segment.

# Define a color scheme 

breaks <- c(-100, -50, 0, 50, 100)
shades <- c("#f0f0f0", "#aaaaaa", "#777777", "#000000")

# find a shade given a value 
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

# want smoother transitions so use interpolations between the coordinates
plot(values, type="n")
# In between coordinates.
nterp <- 50
x <- seq(1, 2, length.out=nterp)
y <- seq(values[1], values[2], length.out=nterp)

# This gives you 50 x-y coordinate pairs. You want to draw segments in between these pairs and set the colors to smoothly transition from one color to another.
# Start and finish colors.
shade_start <- segcols[1]
shade_end <- segcols[2]

# Creeate color palette
# Color palette
pal <- colorRampPalette(c(shade_start, shade_end))

# This gives you a function that lets you generate a vector of colors. In this case, you want 50 shades from shade_start to shade_end, so you enter the following:
inbetween_colors <- pal(nterp)

# Pass these colors to segments along with the coordinates you generated with seq():
segments(x0=x[1:(nterp-1)],
         x1=x[2:nterp],
         y0=y[1:(nterp-1)],
         y1=y[2:nterp],
         col=inbetween_colors, lwd=4)

# do the same for all segments 

for (i in 2:length(values)) {
  
  nterp <- 50   
  
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


