# Load data.
births <- read.csv("data/natality-2003-2021.csv", stringsAsFactors = FALSE)
births$monthnum <- c(rep(1:12, 18), 1:6)


# 2003 base.
births2003 <- births$Births[births$Year == 2003]

# Calculate change since 2003 during same month.
births$change_since2003 <- apply(births, 1, function(x) {
  curr_births <- as.numeric(x[3])
  from2003 <- births2003[as.numeric(x[4])]
  return( (curr_births - from2003) / from2003 )
})

shades <- c('#00aab7', '#77c2ca', '#b6d9dd', '#f0f0f0', '#f0bda6', '#e48b5f', '#d05716')
breaks <- c(-20, -7.5, -5, -2.5, 2.5, 5, 7.5, 20)

x <- 13:dim(births)[1]
y <- 100*births$change_since2003[13:dim(births)[1]]

plot(x, y, xlab="Months Since January 2003", ylab="% Difference", type="n")
gradientLine(x, y, breaks=breaks, shades=shades, nterp=20, lwd=3)
