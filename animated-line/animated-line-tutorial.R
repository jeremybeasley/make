install.packages("animation", dependencies = TRUE)
library(animation)

# Load data
mar_w_age_all <- read.csv("data/mar_w_age_all.tsv", sep="\t", stringsAsFactors = FALSE)



#
# Plot with multiple lines
#

plot(0, 0, type="n", xlim=c(0, 100), ylim=c(0, 1), main=6, xlab="", ylab="")
curr <- mar_w_age_all[mar_w_age_all$MARST == 6,]
years <- unique(curr$YEAR)
for (i in 1:length(years)) {
    curr_year <- curr[curr$YEAR == years[i] & curr$AGE <= 100,]
    lines(curr_year$AGE, curr_year$prop, col="#000000")
}


#
# Plot lines for never married with varying line width, no animation yet
#

max_lwd = 3
plot(0, 0, type="n", xlim=c(0, 100), ylim=c(0, 1), main=6, xlab="", ylab="")
curr <- mar_w_age_all[mar_w_age_all$MARST == 6,]
years <- unique(curr$YEAR)
for (i in 1:length(years)) {
    curr_year <- curr[curr$YEAR == years[i] & curr$AGE <= 100,]
    
    if (years[i] == 2015) { 
        col <- "#000000" 
        lwd <- 2
    } else {
        col <- "#666666"
        lwd <- max_lwd * 1 / ((length(years)-i+.5)^1.5)
    }
    lines(curr_year$AGE, curr_year$prop, col=col, lwd=lwd)
}




#
# Plot lines for all marital statuses, no animation yet
#

par(mfrow=c(2,2))
marstats <- c(6, 1, 4, 5)
max_lwd = 3
for (marst in marstats) {
    plot(0, 0, type="n", xlim=c(0, 100), ylim=c(0, 1), main=marst, xlab="", ylab="")
    curr <- mar_w_age_all[mar_w_age_all$MARST == marst,]
    years <- unique(curr$YEAR)
    for (i in 1:length(years)) {
        curr_year <- curr[curr$YEAR == years[i] & curr$AGE <= 100,]
        
        if (years[i] == 2015) { 
            col <- "#000000" 
            lwd <- 2
        } else {
            col <- "#666666"
            lwd <- max_lwd * 1 / ((length(years)-i+.5)^1.5)
        }
        lines(curr_year$AGE, curr_year$prop, col=col, lwd=lwd)
    }
}


ani.options(outdir = paste(getwd(), "/tmp", sep=""))
mar_w_age_all <- read.csv("mar_w_age_all.tsv", sep="\t", stringsAsFactors = FALSE)
marstats <- c(6, 1, 4, 5)   # Never married, married, divorced, widowed
mardesc <- c("NEVER MARRIED", "MARRIED", "DIVORCED", "WIDOWED")
mar <- data.frame(marst=marstats, desc=mardesc, stringsAsFactors = FALSE)
years <- unique(mar_w_age_all$YEAR)
max_lwd <- 3

#
# All the charts for a marital status.
#

par(mfrow=c(4,4), las=1, mar=c(4,3,2,2))
max_lwd <- 2
for (y in years) {
        
    # Draw blank plot
    main <- mar[mar$marst == marst, "desc"]
    plot(0, 0, type="n", xlim=c(0, 100), ylim=c(0, 100), xlab="", ylab="", bty="n", main=y)
    
    # Add lines
    curr <- mar_w_age_all[mar_w_age_all$MARST == 1 & mar_w_age_all$YEAR <= y & mar_w_age_all$AGE <= 100,]
    years_to_draw <- years[years <= y]
    for (i in 1:length(years_to_draw)) {
        curr_year <- curr[curr$YEAR == years_to_draw[i],]
        if (i == length(years_to_draw)) {
            col <- "#b31dc2" 
            lwd <- max_lwd
        } else {
            col <- "#cd24de"
            lwd <- max_lwd * 1 / ((length(years_to_draw)-i+.5)^1.5)
        }
        
        lines(curr_year$AGE, 100*curr_year$prop, col=col, lwd=lwd)
    }
}





#
# Animate
#

saveGIF({
    # One frame for each year
    for (y in years) {
        
        par(mfrow=c(2,2), las=1, mar=c(4, 4, 3, 4))
        for (marst in marstats) {
            
            # Draw blank plot
            # plot(0, 0, type="n", xlim=c(0, 100), ylim=c(0, 100), xlab="", ylab="", bty="n")
            plot(0, 0, type="n", xlim=c(0, 100), ylim=c(0, 100), xlab="", ylab="", bty="n", axes=FALSE)
            
            # Axes and ticks
            xticks <- c(0, 20, 40, 60, 80, 100)
            yticks <- c(0, 25, 50, 75, 100)
            axis(1, c(-10, xticks, 110), c(-10, xticks, 110), tck=0, pos=0, padj=-.9, cex.axis=1.2)
            axis(2, yticks, paste(yticks, "%", sep=""), line=2, lwd=0, mgp=c(0,-1.5,0), cex.axis=1.2)
            abline(NULL, NULL, lty=1, col="black", lwd=.08, h=yticks)
            clip(-1, 101, 0, 100)
            abline(NULL, NULL, lty=1, col="black", lwd=.08, h=NULL, v=xticks)
            
            # Title
            main <- mar[mar$marst == marst, "desc"]
            # mtext(main, side=3, line=1, cex=1.3, family="Consolas")
            mtext(main, side=3, line=1, cex=1.3)
            
            # Add lines
            curr <- mar_w_age_all[mar_w_age_all$MARST == marst & mar_w_age_all$YEAR <= y & mar_w_age_all$AGE <= 100,]
            years_to_draw <- years[years <= y]
            for (i in 1:length(years_to_draw)) {
                curr_year <- curr[curr$YEAR == years_to_draw[i],]
                if (i == length(years_to_draw)) {
                    col <- "#b31dc2" 
                    lwd <- max_lwd
                } else {
                    col <- "#cd24de"
                    lwd <- max_lwd * 1 / ((length(years_to_draw)-i+.5)^1.5)
                }
                
                lines(curr_year$AGE, 100*curr_year$prop, col=col, lwd=lwd)
            }
        }
    }
}, movie.name = "marital-status-changing-1900-2015-aes.gif", interval=0.1, nmax=100, ani.width=720, ani.height=500)
