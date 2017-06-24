dimension <- 16 #Size of world
characteristics <- 3 #More manageable than the original 6
options(scipen=999)

world <- array(0, dim=c(dimension, dimension, characteristics))

for (i in 1:dimension){
  for (j in 1:dimension){
    for (k in 1:characteristics){
      world[i,j,k] <- sample(c(0,1), 1)
    }
  }
}

#Flatten the world for plotting purposes
debinarize <- function(x){
  sum(x * 2^seq(length(x)-1, 0, -1))
}

# Set up colors for map
library(RColorBrewer)
library(spam)

rgb.palette <- brewer.pal(8, "Pastel1")

layout.mat <- rbind(c(1,1,1,NA,NA),
                    c(1,1,1,2,2),
                    c(1,1,1,NA,NA))

for(i in 0:7){
  name <- paste("num",i,sep="")
  assign(name, NULL)
}

par(mfrow=c(1,2))
pars <- c('plt','usr')

for(iteration in 1:10000){
  # Select two cells to interact. Random number between 1 and dimension
  first.cell.row <- round(runif(1)*(dimension)+0.5)
  first.cell.column <- round(runif(1)*(dimension)+0.5)
  
  second.cell.row <- first.cell.row
  second.cell.column <- first.cell.column
  
  while((second.cell.row == first.cell.row) & (second.cell.column == first.cell.column)){
    second.cell.row <- first.cell.row + sample(c(-1, 0, 1), 1)
    second.cell.column <- first.cell.column + sample(c(-1, 0, 1), 1)
  }
  
  # Make the world wrap around
  second.cell.row[second.cell.row==0] <- dimension
  second.cell.row[second.cell.row==dimension+1] <- 1
  second.cell.column[second.cell.column==0] <- dimension
  second.cell.column[second.cell.column==dimension+1] <- 1
  
  #Calculate probability of successful interaction
  probability.of.interaction <- sum(world[first.cell.row, first.cell.column,] == world[second.cell.row, second.cell.column,])/characteristics
  
  #Change second cell by 1 unit in direction of difference, with given probability
  element.to.change <- min(which(world[first.cell.row, first.cell.column,]!=world[second.cell.row, second.cell.column,]))
  
  if(element.to.change!=Inf & probability.of.interaction > runif(1)){
    world[second.cell.row, second.cell.column,element.to.change] <- world[first.cell.row, first.cell.column,element.to.change]
  }
  
  flatworld <- apply(world, 1:2, debinarize) # Create a 16x16 matrix of types

  num0 <- c(num0, sum(flatworld==0)) # Track the prevalence of each type
  num1 <- c(num1, sum(flatworld==1))
  num2 <- c(num2, sum(flatworld==2))
  num3 <- c(num3, sum(flatworld==3))
  num4 <- c(num4, sum(flatworld==4))
  num5 <- c(num5, sum(flatworld==5))
  num6 <- c(num6, sum(flatworld==6))
  num7 <- c(num7, sum(flatworld==7))
  
  # Plot the result so that we can observe in real time.
  if(iteration==1){
    image(flatworld, col=rgb.palette, axes=FALSE)
    par1 <- c(list(mfg=c(1,1,1,2)), par(pars))
    plot(-100, -100, xlim=c(1,10000), ylim=c(0,256), ylab="Prevalence", xlab="Iteration", type="n", cex.axis=0.8)
    rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4], col="#E6E6E6")
    abline(h=c(0,50,100,150,200,250), col="white", lwd=0.5)
    abline(v=c(0,2000,4000,6000,8000,10000), col="white", lwd=0.5)
    par2 <- c(list(mfg=c(1,2,1,2)), par(pars))
  } else {
    par(par1)
    image(flatworld, col=rgb.palette, axes=FALSE)
    par(par2)
    segments(iteration-1, num0[iteration-1], iteration, num0[iteration], col=rgb.palette[1], lwd=2)
    segments(iteration-1, num1[iteration-1], iteration, num1[iteration], col=rgb.palette[2], lwd=2)
    segments(iteration-1, num2[iteration-1], iteration, num2[iteration], col=rgb.palette[3], lwd=2)
    segments(iteration-1, num3[iteration-1], iteration, num3[iteration], col=rgb.palette[4], lwd=2)
    segments(iteration-1, num4[iteration-1], iteration, num4[iteration], col=rgb.palette[5], lwd=2)
    segments(iteration-1, num5[iteration-1], iteration, num5[iteration], col=rgb.palette[6], lwd=2)
    segments(iteration-1, num6[iteration-1], iteration, num6[iteration], col=rgb.palette[7], lwd=2)
    segments(iteration-1, num7[iteration-1], iteration, num7[iteration], col=rgb.palette[8], lwd=2)
  }
}  