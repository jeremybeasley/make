#
# Load data.
#

temp <- read.csv("data/global-temperature.txt", sep="", stringsAsFactors = FALSE)


#
# Default line chart
#

plot(temp$median_val, type="l")



#
# Time series plot with polar coordinates.
#

source("lib/getAngle.R")

# install.packages("plotrix", dependencies = TRUE)
library(plotrix)

# Months to angles
months <- rep(1:12, (2017-1850))
# months <- c(months, 1:9)
angles <- sapply(months, getAngle)

# Default polar plot
polar.plot(temp$median_val[1:length(months)], angles[1:length(months)], rp.type="p")

# Other types for refernce
par(mfrow=c(1,3))
polar.plot(temp$median_val[1:length(months)], angles[1:length(months)], rp.type="p", main="Polygon")
polar.plot(temp$median_val[1:length(months)], angles[1:length(months)], rp.type="r", main="Lines")
polar.plot(temp$median_val[1:length(months)], angles[1:length(months)], rp.type="s", main="Symbols")

# Fix labels.
polar.plot(temp$median_val[1:length(months)], angles, rp.type="polygon", lwd=.5, label.pos = unique(angles), label=c("Jan.", "Feb.", "March", "April", "May", "June", "July", "Aug.", "Sept.", "Oct.", "Nov.", "Dec."), start=90, clockwise=TRUE)



# Adjust colors by plotting piece-wise
pal <- colorRampPalette(c("#f0f0f0", "black"))
shades <- pal(length(months))
grid_col <- "#cccccc"

# Radial grid line spots.
radial_range <- pretty(range(temp$median_val))

sofar <- 0
polar.plot(0, 0, rp.type="polygon", lwd=.1, label.pos = unique(angles), label=c("Jan.", "Feb.", "March", "April", "May", "June", "July", "Aug.", "Sept.", "Oct.", "Nov.", "Dec."), start=90, radial.lim=radial_range, grid.col=grid_col, clockwise=TRUE)
while(sofar < length(months)) {
    
    # Values for current year.
    start_i <- sofar + 1
    end_i <- start_i + 11
    curr_vals <- temp$median_val[start_i:end_i]
    curr_angles <- angles[start_i:end_i]
    
    # Increment the counter.
    sofar <- sofar + (end_i-start_i+1)
    
    # Line width
    lwd <- max(1 * sofar / length(months), .1)
    
    # Draw plot.
    polar.plot(curr_vals, curr_angles, rp.type="p", lwd=lwd, start=90, add=TRUE, radial.lim=radial_range, line.col=shades[sofar], clockwise=TRUE)
}



#
# Animate it, using a basic color scheme.
#

# install.packages("animation", dependencies = TRUE)
library(animation)

saveGIF({
    
    num_years <- length(months) / 12
    years_so_far <- 0
    while(years_so_far <= num_years) {
        
        sofar <- 0
        polar.plot(0, 0, rp.type="polygon", lwd=.1, label.pos = unique(angles), label=c("Jan.", "Feb.", "March", "April", "May", "June", "July", "Aug.", "Sept.", "Oct.", "Nov.", "Dec."), start=90, radial.lim=radial_range, grid.col=grid_col, clockwise=TRUE)
        while(sofar < (years_so_far*12)) {
            start_i <- sofar + 1
            end_i <- start_i + 11
            curr_vals <- temp$median_val[start_i:end_i]
            curr_angles <- angles[start_i:end_i]
            
            if (end_i > length(months)) {
                end_i <- length(months)
                curr_vals <- c(-1, curr_vals, -1)
                curr_angles <- c(curr_angles[1], curr_angles, curr_angles[length(curr_angles)])
                the_end <- TRUE
            }
            
            lwd <- max(1 * sofar / length(months), .00005)
            
            polar.plot(curr_vals, curr_angles, rp.type="p", lwd=lwd, start=90, add=TRUE, radial.lim=radial_range, line.col=shades[sofar], clockwise=TRUE)
            
            sofar <- sofar + (end_i-start_i+1)
        }
        
        years_so_far <- years_so_far + 1
    }
    
}, movie.name="Time-Circle-Basic.gif", interval=0.1, ani.width=720, ani.height=500 )






#
# Animate it, using a more intuitive color scheme.
#


saveGIF({

    num_years <- length(months) / 12
    max_lwd <- 3
    years_so_far <- 1
    
    while(years_so_far <= num_years) {
        
        par(bg="#e2e2e2")
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

}, movie.name="Time-Circle-Featured.gif", interval=0.12, ani.width=720, ani.height=500)

