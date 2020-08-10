# Source helper files
source("lib/shot-matrix-helpers.R")

# load data 
all_agg <- read.csv("data/binned_shots_2003-2018.tsv", sep="\t", stringsAsFactors=FALSE)
seasons <- unique(all_agg$season)

## HEAMAP 

# w/ image() function 

# Heatmap color scale
col <- c('#10506c','#4b758c','#7c9aad','#adc2cf','#e1ebf0',
         '#feeae4','#f8c2ae','#ed987a','#dd6f48','#c94214')

# Then define the breaks that define what each color represents. The length of this vector must be one greater than the length of the color vector. In this case, the breaks vector is 11.
breaks <- c(0, 1/3, 2/5, .5, 2/3, 1,
            1.5, 2, 2.5, 3, 100)

# The goal of the animated heatmap in this tutorial is to compare shot selection each season against the 2003-04 season. That is, the the 2003-04 season is the baseline comparison, so subset that out:

# Baseline comparison.
baseline_agg <- all_agg[all_agg$season == "2003-04",]

# To reduce the noise, limit it to spots on the floor with at least five shots and in the first 35 feet of the playing area:
baseline_agg <- baseline_agg[baseline_agg$n >= 5 & baseline_agg$bin_y <= 35,]

#  instead of comparing counts, you want to compare proportions to account of varying pace over the years. 
baseline_agg$prop <- baseline_agg$n / sum(baseline_agg$n)

baseline_agg_mat <- makeMatrixFromAgg(baseline_agg, nrow = 51, ncol=51, valcol = "prop")


## 2017-2018
curr_season <- "2017-18"
curr_agg <- all_agg[all_agg$season == curr_season, ]
curr_agg$prop <- curr_agg$n / sum(curr_agg$n)
curr_agg_mat <- makeMatrixFromAgg(curr_agg)

# Calculate change of current against baseline
curr_diff_mat <- curr_agg_mat / baseline_agg_mat

image(0:50, 0:50, z=curr_diff_mat, col = col, breaks = breaks, asp=1,
      axes=FALSE, useRaster=TRUE, xlab="", ylab="")

# labels and court lines 
text(25, 45, "NBA Shot Selection", cex=2.5)
text(25, 41, paste(curr_season, "compared against 2003-04"), cex=1.4)
addCourtLines("white", 3)

# legend
rect(20:29, rep(38, 10), 21:30, rep(39,10), col = col)
text(25, 37.25, "Even", cex=.9)
text(21, 37.25, "-3x", cex=.9)
text(29, 37.25, "+3x", cex=.9)

# Draw multiples (one for each season)

par(mfrow=c(4,4), mar=c(2,0,0,0))
for (i in 2:length(seasons)) {
  
  # Draw a heatmap for each season.
  
  # Current season.
  curr_season <- seasons[i]
  curr_agg <- all_agg[all_agg$season == curr_season, ]
  curr_agg$prop <- curr_agg$n / sum(curr_agg$n)
  curr_agg_mat <- makeMatrixFromAgg(curr_agg)
  
  # Calculate change of current against baseline
  curr_diff_mat <- curr_agg_mat / baseline_agg_mat
  
  # Draw the heatmap.
  image(0:50, 0:50, z=curr_diff_mat, col = col, breaks = breaks, asp=1,
        axes=FALSE, useRaster=TRUE, xlab="", ylab="")
  text(25, 41, curr_season, cex=1.4)
  addCourtLines("white", 2)
  
}

# Animate 

# There are various ways to go about this, but regardless of the solution you choose, there are two main steps.

# Create images, one for each frame (in this case, each season).
# String them together to make a GIF. 

library(animation)
saveGIF({
  for (i in 2:length(seasons)) {
    
    # Current season.
    curr_season <- seasons[i]
    curr_agg <- all_agg[all_agg$season == curr_season, ]
    curr_agg$prop <- curr_agg$n / sum(curr_agg$n)
    curr_agg_mat <- makeMatrixFromAgg(curr_agg)
    
    # Calculate change of current against baseline.
    curr_diff_mat <- curr_agg_mat / baseline_agg_mat
    
    # Draw the heatmap.
    par(mar=c(3,3,0,3))
    image(0:50, 0:50, z=curr_diff_mat, col = col, breaks = breaks, asp=1, 
          axes=FALSE, useRaster=TRUE, xlab="", ylab="")
    text(25, 45, "NBA Shot Selection", cex=2.5)
    text(25, 41, paste(curr_season, "compared against 2003-04"), cex=1.4)
    addCourtLines("white", 3)
    
    
    # Add legend.
    rect(20:29, rep(38, 10), 21:30, rep(39,10), col = col)
    text(25, 37.25, "Even", cex=.9)
    text(21, 37.25, "-3x", cex=.9)
    text(29, 37.25, "+3x", cex=.9)
  }
  
}, movie.name = "shot-selection-heatmap.gif", ani.width=700, ani.height=600, interval=0.15)


#
# Generate frames for animation.
#
frames_so_far <- 0
for (i in 2:length(seasons)) {
  
  # Current season.
  curr_season <- seasons[i]
  curr_agg <- all_agg[all_agg$season == curr_season, ]
  curr_agg$prop <- curr_agg$n / sum(curr_agg$n)
  curr_agg_mat <- makeMatrixFromAgg(curr_agg)
  
  # Calculate change of current against baseline
  curr_diff_mat <- curr_agg_mat / baseline_agg_mat
  
  # Open graphics device.
  png(paste("images/frame", frames_so_far, ".png", sep=""), 
      width=700, height=600)
  
  # Draw the heatmap.
  par(mar=c(3,3,0,3))
  image(0:50, 0:50, z=curr_diff_mat, col = col, breaks = breaks, asp=1, 
        axes=FALSE, useRaster=TRUE, xlab="", ylab="")
  text(25, 45, "NBA Shot Selection", cex=2.5)
  text(25, 41, paste(curr_season, "compared against 2003-04"), cex=1.4)
  addCourtLines("white", 3)
  
  
  # Add legend.
  rect(20:29, rep(38, 10), 21:30, rep(39,10), col = col)
  text(25, 37.25, "Even", cex=.9)
  text(21, 37.25, "-3x", cex=.9)
  text(29, 37.25, "+3x", cex=.9)
  
  # Done generating frame.
  dev.off()
  
  # Increment frame count.
  frames_so_far <- frames_so_far + 1
}

