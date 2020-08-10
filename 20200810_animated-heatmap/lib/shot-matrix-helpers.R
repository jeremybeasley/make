library(plotrix)

#
# Helper function to turn shots aggregates to a matrix.
#
makeMatrixFromAgg <- function(agg, nrow=51, ncol=51, valcol="prop") {
    newmat <- matrix(0, nrow=nrow, ncol=ncol)
    for (i in 1:dim(agg)[1]) {
        if (agg[i, "bin_x"] <= ncol && agg[i, "bin_y"] <= nrow) {
            newmat[as.numeric(agg[i, "bin_x"]), as.numeric(agg[i, "bin_y"])] <- as.numeric(agg[i, valcol])
        }
    }
    return(newmat)
}




#
# Helper function to draw court lines.
#
addCourtLines <- function(col="#cccccc", lwd=2) {
    draw.arc(25, 4.75, 9/12, angle1=0, angle2=2*pi, col=col, lwd=lwd) # Hoop
    lines(c(22,28), c(4,4), col=col, lwd=lwd)   # Backboard
    
    lines(c(19,19), c(0,19), col=col, lwd=lwd)  # Inside lane
    lines(c(31,31), c(0,19), col=col, lwd=lwd)
    lines(c(19,31), c(19,19), col=col, lwd=lwd) # Free throw
    lines(c(0,50),c(0,0), col=col, lwd=lwd) # Baseline
    lines(c(3, 3), c(0, 14), col=col, lwd=lwd) # Side 3-pt
    lines(c(47, 47), c(0, 14), col=col, lwd=lwd)
    draw.arc(25, 5.25, 23.75, angle1=pi/8.3, angle2=pi/1.138, col=col, lwd=lwd) # 3-pt arc
}






#
# Helper function to get averaged matrix
#

averageMatrix <- function(z, navg) {
    
    x <- 1:(dim(z)[2])
    y <- 1:(dim(z)[1])
    
    newz <- matrix(0, nrow=length(x), ncol=length(y))
    for (i in x) {
        for (j in y) {
            xmin <- max(c(i-navg, 1))
            xmax <- min(c(length(x), i+navg))
            ymin <- max(c(j-navg, 1))
            ymax <- min(c(length(x), j+navg))
            newz[i, j] <- mean(z[xmin:xmax, ymin:ymax])
        }
    }
    
    return(newz)
    
}




