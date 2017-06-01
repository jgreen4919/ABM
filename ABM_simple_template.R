# Template for running and plotting a very simple agent-based model in R
# Original: Professor Bear Braumoeller, Department of Political Science, Ohio State
# Tweaked by Jon Green, Department of Political Science, Ohio State

# This code creates a 20x20 grid of 0s and 1s, which represent values of some
# variable held by agents in those cells. It then chooses two adjacent cells,
# the first at random and the second at random from among the first cell's
# neighbors, and applies a simple rule -- the first cell takes on the value
# of the second. It iterates this cell selection and rule application 1,000
# times, displays the result, and tracks the fraction of 1s in the matrix
# over time.

# This is not meant to represent a meaningful social process. It's just meant
# to be a template for students and colleagues to use to create more interesting
# agent-based models.

library(spam)

#This function sets the matrix
abm.matrix <- function(dimension = 20){
  mat <- matrix(sample(c(0,1), dimension*dimension, replace=TRUE), nrow=dimension, ncol=dimension)
  return(mat)
}

#This function sets the thing to track (in this case, the ratio of black to white cells)
bin.ratio <- function(mat){
  bin.ratio <- sum(mat)/(nrow(mat)*ncol(mat))
  return(bin.ratio)
}

#This function picks the first cell
select.cell <- function(mat){
  fc.row <- round(runif(1)*(nrow(mat))+0.5)
  fc.col <- round(runif(1)*(ncol(mat))+0.5)
  return(c(fc.row, fc.col))
}

#This function picks a neighboring cell, wrapping around if it winds up out of bounds
select.neighbor <- function(first.cell, mat){
  #Match cells
  sc.row <- first.cell[1]
  sc.col <- first.cell[2]
  
  #Move to neighboring row/column
  while((sc.row == first.cell[1]) & (sc.col == first.cell[2])){
    sc.row <- first.cell[1] + sample(c(-1, 0, 1), 1)
    sc.col <- first.cell[2] + sample(c(-1, 0, 1), 1)
  }
  
  #If out of bounds, wraparound
  sc.row[sc.row==0] <- nrow(mat)
  sc.row[sc.row==nrow(mat)+1] <- 1
  sc.col[sc.col==0] <- ncol(mat)
  sc.col[sc.col==ncol(mat)+1] <- 1
  
  return(c(sc.row, sc.col))
}

#This function makes the first cell match the second cell
copy.values <- function(mat, first.cell, second.cell){
  mat[first.cell[1],first.cell[2]] <- mat[second.cell[1],second.cell[2]]
  return(mat)
}

#This function updates the thing to track
add.to.tracked.series <- function(br, mat){
  br <- c(br, bin.ratio(mat))
  return(br)
}

#This function displays results
display.result <- function(mat, thing.to.track, iteration){
  if(iteration==1){
    image(t(mat), col=c("white", "black"), axes=FALSE)
    par1 <- c(list(mfg=c(1,1,1,2)), par(pars))
    plot(-100, -100, xlim=c(1,1000), ylim=c(0,1), 
         ylab="Fraction of black squares", 
         xlab="Iteration", 
         main = "Fraction of Black Cells",
         type="n", cex.axis=0.8)
    rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4], col="#E6E6E6")
    abline(h=c(0,0.25,0.5,0.75,1), col="white", lwd=0.5)
    abline(v=c(0,200,400,600,800,1000), col="white", lwd=0.5)
    par2 <- c(list(mfg=c(1,2,1,2)), par(pars))
  } else {
    par(par1)
    image(t(mat), col=c("white", "black"), axes=FALSE)
    par(par2)
    segments(iteration-1, ratio[iteration-1], iteration, ratio[iteration], col="black", lwd=1)
  }

}

bin.matrix <- abm.matrix(dimension = 15)
ratio <- bin.ratio(bin.matrix)
par(mfrow=c(1,2))
pars <- c('plt','usr')

for(iteration in 1:1000){
  first.cell <- select.cell(mat = bin.matrix)
  second.cell <- select.neighbor(first.cell = first.cell, mat = bin.matrix)
  bin.matrix <- copy.values(mat = bin.matrix, first.cell = first.cell, second.cell = second.cell)
  ratio <- add.to.tracked.series(br = ratio, mat = bin.matrix)
  display.result(mat = bin.matrix, thing.to.track = bin.ratios, iteration)
}

