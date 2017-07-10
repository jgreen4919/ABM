# Standing Ovation Problem Replication (from Miller and Page 2004)
# Models a standing ovation. 
# Sets an audience of size x by x. 
# Each agent in the audience perceives the performance to have a certain quality with mean y and variance yvar. 
# Each agent has a quality threshold for standing with mean z and variance zvar. 
# Each agent has a social threshold for standing with mean s and variance svar.
# Each agent has a vision rule for which other audience members they will look at when considering whether to stand.
# When the performance ends, audience members stand if quality > quality threshold. 
# In each round, audience members who are not already standing look around based on a vision rule 
  # (The default randomly selects between the five neighbors to their immediate left/right/front or the entire audience, 
  # but one or the other can be selected for all agents) 
# If the percentage of agents they see standing exceeds their social threshold they stand.

# Function sets audience size and each agent's perceived quality, quality threshold, social threshold, and vision rule
do.aud <- function(dim = 20, qual = .7, qualvar = .2,
                   qualt = .75, qualtvar = .1,
                   soct = .5, soctvar = .25,
                   viz.rule = c("five","cone","global")){
  require(msm)
  require(data.table)

  # Matrices for quality, quality threshold and social threshold
  quality <- matrix(data=rtnorm(dim^2, mean = qual, sd = qualvar, lower = 0, upper = 1), nrow=dim, ncol=dim)
  quality.threshold <- matrix(data=rtnorm(dim^2, mean = qualt, sd = qualtvar, lower = .7, upper = 1), nrow=dim, ncol=dim)
  social.threshold <- matrix(data=rtnorm(dim^2, mean = soct, sd = soctvar, lower = .05, upper = .8), nrow=dim, ncol=dim)

  # Data table giving each audience member an id and x/y coordinates, to fill with characteristics
  aud <<- data.table(id = 1:prod(dim*dim),
                    x = rep(1:dim, dim),
                    y = rep(1:dim, each = dim),
                    q = rep(NA, prod(dim*dim)),
                    qt = rep(NA, prod(dim*dim)),
                    st = rep(NA, prod(dim*dim)),
                    vr = sample(viz.rule, prod(dim*dim), replace = TRUE),
                    stand = rep(NA, prod(dim*dim)))
  
  # Fill data table with characteristics
  for(i in 1:dim){
    for(j in 1:dim){
      aud$q[aud$x == i & aud$y == j] <<- quality[i,j]
      aud$qt[aud$x == i & aud$y == j] <<- quality.threshold[i,j]
      aud$st[aud$x == i & aud$y == j] <<- social.threshold[i,j]
      #Audience member is standing at end of performance if quality > quality threshold
      aud$stand[aud$x == i & aud$y == j] <<- quality[i,j] > quality.threshold[i,j] 
    }
  }
}

# Function to plot the theater
plotStand <- function(title){
  require(data.table)
  require(ggplot2)
  require(RColorBrewer)
  
  # find the dimensions of the grid to get the best dot size
  dims <- c(max(aud$x), max(aud$y))
  
  # plot each agent's status: standing or nah  
  p <- ggplot(data = aud, 
              aes(x = x, y = y, color = stand)) + 
    # resize dots to grid
    geom_point(size = 100/sqrt(prod(dims))) +
    # theme: mostly blank
    theme_bw() + 
    theme(axis.line = element_blank(),
          axis.text.x = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks = element_blank(),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          panel.background = element_blank(),
          panel.border = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          plot.background = element_blank(),
          plot.title = element_text(lineheight=3, 
                                    face="bold", 
                                    color="black", 
                                    size=14)) +
    # fix axes to avoid distortion in the picture
    coord_fixed(ratio = 1) + 
    # add the title
    ggtitle(title)
  
  return(p)
}
do.aud()
# Function governing interaction rules between agents
  # Takes agent's row number, column number and audience dimension

do.stand <- function(row, col, dim = 20){
  if(aud[x == col & y == row, stand] == TRUE){
    return(TRUE) # If agent is standing, they stay standing
  }
  else{
    # Return percent of standing agents in field of vision, depending on selected rule
    if(aud[x == col & y == row, vr] == "five"){
      x_vals <- c(col+1, col, col-1)
      x_vals <- unique(sapply(x_vals, function(x) {
        if(x < 1){x <- 1} 
        if(x > dim){x <- dim} 
        else{x}
        }))

      y_vals <- c(row, row+1)
      y_vals <- unique(sapply(y_vals, function(x) {
        if(x > dim){x <- dim}
        else{x}
        }))

      neighbor.ids <- aud[x %in% x_vals & y %in% y_vals, id]
      neighbor.ids <- neighbor.ids[! neighbor.ids %in% aud[x == col & y == row, id]]
      pct.stand <- sum(aud[id %in% neighbor.ids, stand])/length(unique(neighbor.ids))
      return(pct.stand > aud[x == col & y == row, st])
    }
    if(aud[x == col & y == row, vr] == "cone"){
      coords <- rbind(c(col, row+1), c(col, row-1))
      dis <- 1
      for(i in (row+1):dim){
        y <- i
        x_vals <- c((col-dis):(col+dis))
        for(j in 1:length(x_vals)){
          coords <- rbind(coords, c(x_vals[j], y))
        }
        dis <- dis+1
      }
      coords[,1] <- sapply(coords[,1], function(x) {
        ifelse(x < 1, 1, ifelse(x > dim, dim, x))
      })
      coords <- unique(coords)
      
      see.ids <- aud[x %in% coords[,1] & y %in% coords[,2], id]
      pct.stand <- sum(aud[id %in% see.ids, stand])/length(unique(see.ids))
      return(pct.stand > aud[x == col & y == row, st])
    }
    if(aud[x == col & y == row, vr] == "global"){
      return(mean(aud$stand) > aud[x == col & y == row, st])
    }
  }
}

# Function to iterate through a specified number of rounds. Sets vision rule for do.stand() to inherit
iterate <- function(iterations){
  pctstanding <<- mean(aud$stand) #Calculate initial percent standing
  mst <<- mean(aud$st[aud$stand == FALSE]) #Calculate mean social threshold of sitting audience members
  mq <<- mean(aud$q[aud$stand == FALSE]) #Calculate mean quality perception of sitting audience members
  for(i in 1:iterations){
    aud[,stand := do.stand(row = y, col = x, dim = 20),
        by = 1:nrow(aud)] # Update standing status for each audience member in each iteration
    pctstanding <<- c(pctstanding, mean(aud$stand)) # Update vector of percent standing in each iteration
    mst <<- c(mst, mean(aud$st[aud$stand == FALSE])) # Update vector of mean social threshold of sitting audience members in each iteration
    mq <<- c(mq, mean(aud$q[aud$stand == FALSE])) # Update vector of mean quality perception of sitting audience members in each iteration
    }
  plotStand(paste("Audience after round ", i, sep = "")) #Plot the updated theater after i iterations
}

# Default run
do.aud()
plotStand(title = "Audience Immediately After Performance Ends")
iterate(iterations = 10)

# Plot audience characteristics by round
for(i in 1:length(pctstanding)){
  if(i == 1){
    plot(-100, -100, xlim=c(1,10), ylim=c(0,1), ylab="Value", xlab="Round", type="n", cex.axis=0.8, main = "Audience Characteristics")
  }else{
    segments(i-1, pctstanding[i-1], i, pctstanding[i], col = "green", lwd=2)
    segments(i-1, mst[i-1], i, mst[i], col = "blue", lwd=2)
    segments(i-1, mq[i-1], i, mq[i], col = "red", lwd=2)
  }
}
legend(x = 6, y = .2, legend = c("Percent Standing","Mean ST (Sitting)", "Mean Quality (Sitting)"), fill = c("green","blue","red"))

# Poor quality run with universal cone rule
do.aud(qual = .4, qualvar = .3, viz.rule = "cone")
plotStand(title = "Audience Immediately After Performance Ends")
iterate(iterations = 10)

# Plot audience characteristics by round
for(i in 1:length(pctstanding)){
  if(i == 1){
    plot(-100, -100, xlim=c(1,10), ylim=c(0,1), ylab="Value", xlab="Round", type="n", cex.axis=0.8, main = "Audience Characteristics")
  }else{
    segments(i-1, pctstanding[i-1], i, pctstanding[i], col = "green", lwd=2)
    segments(i-1, mst[i-1], i, mst[i], col = "blue", lwd=2)
    segments(i-1, mq[i-1], i, mq[i], col = "red", lwd=2)
  }
}
legend(x = 6, y = .2, legend = c("Percent Standing","Mean ST (Sitting)", "Mean Quality (Sitting)"), fill = c("green","blue","red"))

