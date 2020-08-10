source("lib/shot-matrix-helpers.R")

# Load data.
all_agg <- read.csv("data/binned_shots_2003-2018.tsv", sep="\t", stringsAsFactors = FALSE)
seasons <- unique(all_agg$season)

# Heatmap color scale
col <- c('#10506c','#4b758c','#7c9aad','#adc2cf','#e1ebf0',
         '#feeae4','#f8c2ae','#ed987a','#dd6f48','#c94214')
breaks <- c(0, 1/3, 2/5, .5, 2/3, 1, 
            1.5, 2, 2.5, 3, 100)

# Baseline comparison.
baseline_agg <- all_agg[all_agg$season == "2003-04",]
baseline_agg <- baseline_agg[baseline_agg$n >= 5 & baseline_agg$bin_y <= 35,]
baseline_agg$prop <- baseline_agg$n / sum(baseline_agg$n)
baseline_agg_mat <- makeMatrixFromAgg(baseline_agg, nrow = 51, ncol=51, valcol = "prop")



#
# Small multiples.
#

par(mfrow=c(4,4), mar=c(2,0,0,0))
for (i in 2:length(seasons)) {
    
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


#
# If you have ImageMagick installed and want to,
# convert the .png files to an animated GIF using the following:
#
# convert -delay 10 $(for i in $(seq 0 13); do echo frame${i}.png; done) -delay 200 frame14.png -loop 0 shot-selection2003-18.gif





#
# Using the animation package
#

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








