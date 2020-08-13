# load data 
temp <- read.csv("data/global-temperature.txt", sep="", stringsAsFactors=FALSE)

# using plotrix 
# source helper - returns the month in degree 
source("lib/getAngle.R")

# create a 12-wide vector for each year in the data set (167 times)
months <- rep(1:12, (2017-1850))

# months to angles 
angles <- sapply(months, getAngle)

# default polar plot 
polar.plot(temp$median_val[1:length(months)], angles[1:length(months)], rp.type="p")

## Adjust labels
polar.plot(temp$median_val[1:length(months)], angles, rp.type="polygon", lwd=.5, label.pos = unique(angles), label=c("Jan.", "Feb.", "March", "April", "May", "June", "July", "Aug.", "Sept.", "Oct.", "Nov.", "Dec."), start=90, clockwise=TRUE)

## adjust color to tell which line is closer to the present versus older
# recent years darker, older years grey
pal <- colorRampPalette(c("#f0f0f0", "black"))
shades <- pal(length(months))

# set grid color
grid_col <- "#cccccc"

# set grid lines
# radial grid line spots 
radial_range <- pretty(range(temp$median_val))

# instead of sinle line, let create one for each year 
# logic
# create a blank plot 
# make a call to a function to add lines 

# create a blank plot by setting lines to 0
polar.plot(0, 0, rp.type="polygon", lwd=.1, label.pos = unique(angles), label=c("Jan.", "Feb.", "March", "April", "May", "June", "July", "Aug.", "Sept.", "Oct.", "Nov.", "Dec."), start=90, radial.lim=radial_range, grid.col=grid_col, clockwise=TRUE)

# start a counter to understand how much we've already plotted 
sofar <- 0 

# iterate over the montly data using a while - loop (why not use a for-loop)
while(sofar < length(months)) {
  # draw a link for a year
  # Values for current year.
  start_i <- sofar + 1
  end_i <- start_i + 11
  curr_vals <- temp$median_val[start_i:end_i]
  curr_angles <- angles[start_i:end_i]
  
  # Increment the counter.
  sofar <- sofar + (end_i-start_i+1)
  
  # Line width
  lwd <- max(1.2 * sofar / length(months), .1)
  
  # Line color
  linecol <- shades[sofar]
  
  # Draw plot.
  polar.plot(curr_vals, curr_angles, rp.type="p", lwd=lwd, start=90, add=TRUE, radial.lim=radial_range, line.col=linecol, clockwise=TRUE)
}


# animate 
num_years <- length(months) / 12
max_lwd <- 3
years_so_far <- 1

saveGIF({
  while(years_so_far <= num_years) {
    
    sofar <- 0
    polar.plot(0, 0, rp.type="polygon", lwd=.1, label.pos = unique(angles), label=c("Jan.", "Feb.", "March", "April", "May", "June", "July", "Aug.", "Sept.", "Oct.", "Nov.", "Dec."), start=90, radial.lim=radial_range, grid.col=grid_col, main=1850+years_so_far-1, clockwise=TRUE)
    while(sofar < (years_so_far*12)) {
      
      # Get indicies and then values for current year.
      start_i <- sofar + 1
      end_i <- start_i + 11
      curr_vals <- temp$median_val[start_i:end_i]
      curr_angles <- angles[start_i:end_i]
      sofar <- sofar + (end_i-start_i+1)
      
      # Using a bivariate color scheme.
      if (sum(curr_vals) < 0) {
        basecol <- "#233a7e"
      } else {
        basecol <- "#913110"
      }
      
      # Line width, thinner for "older" years
      lwd <- max_lwd * ( sofar / (years_so_far*12))^10 + .0001
      lwd <- max(.1, lwd)
      
      # Alpha transparency, less visible for "older" years
      transparency <- round(100 * ( (sofar) / (years_so_far*12))^10)
      transparency <- max(transparency, 50)
      if (transparency == 100) {
        linecol <- basecol
      } else if (transparency < 10) {
        linecol <- paste(basecol, "0", transparency, sep="")
      } else {
        linecol <- paste(basecol, transparency, sep="")
      }
      
      # Add line for current year.   
      polar.plot(curr_vals, curr_angles, rp.type="p", lwd=lwd, start=90, add=TRUE, radial.lim=radial_range, line.col=linecol, clockwise=TRUE)
      
    }   
    years_so_far <- years_so_far + 1
    
  } # @end while years_so_far
  
}, movie.name="Time-Circle.gif", interval=0.1, ani.width=720, ani.height=500)
