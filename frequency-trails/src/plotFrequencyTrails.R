plotFrequencyTrails <- function(values, overlap=4, showRelativeScale=TRUE) {
    
    # Specify initial height
    initheight <- 1 / (dim(values)[2]+overlap)
    max_all <- max(values)
    
    # Start a layout.
    par(mar=c(0,0,0,0), bg="white")
    plot(0:1, 0:1, type="n", xlab="", ylab="", axes=FALSE, asp=1)
    
    # Draw each multiple.
    for (i in (dim(values)[2]):1) {   # Draw in reverse order, so that bottom series appear in front.
        
        par(new=TRUE, plt=c(0, 1, initheight*(i-1), initheight*(i-1)+initheight*overlap) )
        
        if (showRelativeScale) {
            plot(values[,i], xlim=c(0, dim(values)[1]*1.2), ylim=c(0, max(values[,i])), 
                 xlab="", ylab="", type="n", axes=FALSE)
        } else {
            plot(subreddits[,i], xlim=c(0, dim(values)[1]*1.2), ylim=c(0, max_all), 
                 xlab="", ylab="", type="n", axes=FALSE)
        }
        
        
        polygon(c(1:dim(values)[1], dim(values)[1]:1), 
                c(values[,i],rep(0, dim(values)[1])), col="gray", border=NA)
        lines(values[,i], lwd=.8, col="black")
        text(154, max(values[,i])/(overlap+2), colnames(values)[i], pos=4, cex=.7)
    }
    
}