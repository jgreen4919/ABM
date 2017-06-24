# Standing Ovation Problem Replication (from Miller and Page 2004)
# Models a standing ovation. 
# Sets an audience of size x by x. 
# Each agent in the audience perceives the performance to have a certain quality with mean y and variance yvar. 
# Each agent has a quality threshold for standing with mean z and variance zvar. 
# Each agent has a social threshold for standing with mean s and variance svar.
# When the performance ends, audience members stand if quality > quality threshold. 
# In each round, audience members who are not already standing look around based on a vision rule 
  # (The default randomly selects between the five neighbors to their immediate left/right/front or the entire audience, 
  # but one or the other can be selected for all agents) 
# If the percentage of agents they see standing exceeds their social threshold they stand.

# Function sets audience size and each agent's perceived quality, quality threshold and social threshold
do.aud <- function(dim = 20, qual = .7, qualvar = .2,
                   qualt = .75, qualtvar = .1,
                   soct = .5, soctvar = .25){
  require(msm)
  require(data.table)
  pctstanding <<- c(NULL) # Vector to fill later for percent of audience standing
  
  # Matrices for quality, quality threshold and social threshold
  quality <- matrix(data=rtnorm(dim^2, mean = qual, sd = var, lower = 0, upper = 1), nrow=dim, ncol=dim)
  quality.threshold <- matrix(data=rtnorm(dim^2, mean = qualt, sd = qualtvar, lower = .7, upper = 1), nrow=dim, ncol=dim)
  social.threshold <- matrix(data=rtnorm(dim^2, mean = soct, sd = soctvar, lower = .05, upper = .8), nrow=dim, ncol=dim)
  
  # Data table giving each audience member an id and x/y coordinates, to fill with characteristics
  aud <<- data.table(id = 1:prod(dim*dim),
                    x = rep(1:dim, dim),
                    y = rep(1:dim, each = dim),
                    q = rep(NA, prod(dim*dim)),
                    qt = rep(NA, prod(dim*dim)),
                    st = rep(NA, prod(dim*dim)),
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
  pctstanding <<- c(pctstanding, mean(aud$stand)) # Calculate initial percent standing
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

# Function governing interaction rules between agents
  # Takes agent's row number, column number, audience dimension, and vision rule
do.stand <- function(row, col, dim = 20, stand.rule = c("five","global")){
  if(aud[x == row & y == col, stand] == TRUE){
    return(TRUE) # If agent is standing, they stay standing
  }
  else{
    rule <- sample(stand.rule, 1) # Pick a vision rule at random (if one vision rule is specified in function call, will sample from vector of length 1)
    # Return percent of standing agents in field of vision, depending on selected rule
    if(rule == "five"){
      neighbor.ids <- c(NULL)
      if(row %in% 1:(dim-1) & col == 1){
        neighbor.ids[1] <- aud[x == row+1 & y == col, id]
        neighbor.ids[2] <- aud[x == row & y == col+1, id]
        neighbor.ids[3] <- aud[x == row+1 & y == col+1, id]
      }
      if(row %in% 1:(dim-1) & col == dim){
        neighbor.ids[1] <- aud[x == row+1 & y == col, id]
        neighbor.ids[2] <- aud[x == row & y == col-1, id]
        neighbor.ids[3] <- aud[x == row+1 & y == col-1, id]
      }
      if(row == dim & col == 1){
        neighbor.ids[1] <- aud[x == row & y == col+1, id]
      }
      if(row == dim & col == dim){
        neighbor.ids[1] <- aud[x == row & y == col-1, id]
      }
      if(row == dim & col %in% 2:(dim-1)){
        neighbor.ids[1] <- aud[x == row & y == col+1, id]
        neighbor.ids[2] <- aud[x == row & y == col-1, id]
      }
      if(row %in% 1:(dim-1) & col %in% 2:(dim-1)){
        neighbor.ids[1] <- aud[x == row & y == col-1, id]
        neighbor.ids[2] <- aud[x == row & y == col+1, id]
        neighbor.ids[3] <- aud[x == row+1 & y == col-1, id]
        neighbor.ids[4] <- aud[x == row+1 & y == col, id]
        neighbor.ids[5] <- aud[x == row+1 & y == col+1, id]
      }
      pct.stand <- sum(aud[id %in% neighbor.ids, stand])/length(neighbor.ids)
      return(pct.stand > aud[x == row & y == col, st])
    }
    if(rule == "global"){
      return(mean(aud$stand) > aud[x == row & y == col, st])
    }
  }
}

# Function to iterate through a specified number of rounds. Sets vision rule for do.stand() to inherit
iterate <- function(iterations, stand.rule = c("five", "global")){
  for(i in 1:iterations){
    aud[,stand := do.stand(row = x, col = y, dim = 20, stand.rule = stand.rule),
        by = 1:nrow(aud)] # Update standing status for each audience member in each iteration
    pctstanding <<- c(pctstanding, mean(aud$stand)) # Update vector of percent standing in each iteration
  }
  plotStand(paste("Audience after ", i, " rounds", sep = "")) #Plot the updated theater after i iterations
}

do.aud()
plotStand(title = "Audience Immediately After Performance Ends")
iterate(iterations = 1, stand.rule = "five")
pctstanding

sitid <- aud[stand == FALSE, id]
aud[id %in% sitid, 4:6]
