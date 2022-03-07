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