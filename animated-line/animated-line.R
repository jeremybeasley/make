# install.packages("animation", dependencies = TRUE)

# load data 
mar_w_age_all <- read.csv("data/mar_w_age_all.tsv",sep="\t", stringsAsFactors=FALSE)

# create static plots first 
# blank plot
plot(0, 0, type="n", xlim=c(0, 100), ylim=c(0,1), main=6, xlab="", ylab="")

curr <- mar_w_age_all[mar_w_age_all$MARST == 6,]
years <- unique(curr$YEAR)

# draw a line for each year by iterating through each with a for-loop 
for(i in 1:length(years)) {
  curr_year <- curr[curr$YEAR == years[i] & curr$AGE <= 100,]
  lines(curr_year$AGE, curr_year$prop, col="#000000")
}

# IMPROVEMENT — distinguish years with line width
max_lwd=3

plot(0, 0, type="n", xlim=c(0, 100), ylim=c(0, 1), main=6, xlab="", ylab="")
curr <- mar_w_age_all[mar_w_age_all$MARST == 6,]
years <- unique(curr$YEAR)

for (i in 1:length(years)) {
  curr_year <- curr[curr$YEAR == years[i] & curr$AGE <= 100,]
  
  if (years[i] == 2015) {
    col <- "#000000"
    lwd <- max_lwd
  } else {
    col <- "#666666"
    lwd <- max_lwd * 1 / ((length(years)-i+.5)^1.5)
  }
  lines(curr_year$AGE, curr_year$prop, col=col, lwd=lwd)
}

# Small multiples on maried status
par(mfrow=c(2,2))

# specify the marital codes to use 
marstats <- c(6, 1, 4, 5)   # Never married, married, divorced, widowed
mardesc <- c("NEVER MARRIED", "MARRIED", "DIVORCED", "WIDOWED")
mar <- data.frame(marst=marstats, desc=mardesc, stringsAsFactors = FALSE)


# max line width
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



# create one chart per year for all martial statuses 
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
      col <- "#000000"
      lwd <- max_lwd
    } else {
      col <- "#666666"
      lwd <- max_lwd * 1 / ((length(years_to_draw)-i+.5)^1.5)
    }
    
    lines(curr_year$AGE, 100*curr_year$prop, col=col, lwd=lwd)
  }
}

# set temp directory for animation 
ani.options(outdir = paste(getwd(), "/tmp", sep=""))


saveGIF({
  # One frame for each year
  for (y in years) {
    
    par(mfrow=c(2,2), las=1, mar=c(4, 4, 3, 4))
    for (marst in marstats) {
      
      # Draw blank plot
      plot(0, 0, type="n", xlim=c(0, 100), ylim=c(0, 100), xlab="", ylab="", bty="n")
      
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
}, movie.name = "marital-status-changing-1900-2015.gif", interval=0.1, nmax=100, ani.width=720, ani.height=500)

saveGIF()