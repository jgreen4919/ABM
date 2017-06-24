# A very basic implementation of the standing ovation problem
# Miller, John H., and Scott E. Page. “The Standing Ovation Problem.” Complexity 9, no. 5 (2004): 8–16.
# Professor Bear Braumoeller, Department of Political Science, Ohio State

# The model implements two rules. In the 0th round, people stand if their estimate
# of the quality of the performance is greater than their threshold for standing.
# Both are measured on a 0-1 interval, so if estimate = 0.9 and threshold = 0.8, for
# example, the person stands. Ranges of estimates and thresholds can be manipulated.

# In the first and subsequent rounds, people stand up if the fraction of people they
# see standing in their own row and the rows ahead of them exceeds their "social
# threshold" for standing.

cols <- c("grey90", "black")

# Handy matrix plot routine from https://www.r-bloggers.com/animating-schellings-segregation-model/
do.plot <- function(state, plot.title) {
  side <- dim(state)[1]
  x <- rep(1:side, side)
  y <- rep(1:side, each = side)
  par(fin=c(4,4), fig=c(0,1,0,1))
  plot(x , y, axes = F, xlab="", ylab="", main=plot.title, col = cols[state+1], pch = 19, cex = (40/side))
}

matdim <- 20 # Set size of matrix

# Draw quality, quality threshold and social threshold at random from runif(min, max)
quality <- matrix(data=runif(matdim^2, min=0.5, max=0.8), nrow=matdim, ncol=matdim)
quality.threshold <- matrix(data=runif(matdim^2, min=0.7, max=1), nrow=matdim, ncol=matdim)
social.threshold <- matrix(data=runif(matdim^2, min=0.05, max=0.8), nrow=matdim, ncol=matdim)

# Stand if quality exceeds threshold
stand <- quality > quality.threshold

# Create plot. Standing people are black, sitting are grey.
do.plot(stand, 0)

# Iterative process: create matrix of percentage of standers that each person
# can see, make that person stand if that percentage exceeds social threshold,
# and repeat. 

for(i in 1:30){
  see.frac <- matrix(data=NA, nrow=matdim, ncol=matdim)
  for(j in 1:matdim){
    see.frac[,j] <- sum(stand[,1:j])/(matdim*j)
  }
  sheeple <- see.frac > social.threshold # People guilted into standing
  stand <- pmax(stand,sheeple)  # New matrix of people standing
  do.plot(stand, i)
  Sys.sleep(0.15) # So that we can see the situation evolve
}

