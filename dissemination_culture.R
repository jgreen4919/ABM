# Dissemination of Culture Replication (from Axelrod 1997)
# Model specifies a world of x by x dimensions
# Each cell in the world is a numeric vector of x number of features, and each feature takes on one of x number of traits per feature
# In each step, one cell picked at random interacts with a randomly selected Von Neumann neighbor
# Set the probability of adopting a trait p equal to the % of traits the cells already share
# If adopting a new trait, activated cell picks a trait it doesn't already share with their neighbor and matches it

library(RColorBrewer)
myPalette <- colorRampPalette(rev(brewer.pal(11, "Spectral")))

# Initiate world with a function that specifies number of agents, features per agent, and traits per feature
dc.matrix <- function(dimension = 10, features = 5, traits = 10){
  dimension <<- dimension
  features <<- features
  traits <<- traits
  world <- matrix(NA, nrow=dimension, ncol=dimension)
  for(i in 1:length(world)){
    world[i] <- paste(as.character(sample(0:(traits-1), features, replace = T)), collapse = "")
  }
  return(world)
}

# Within initiated world, set function governing interaction rules
disseminate <- function(mat){
  # Select two cells to interact. Random number between 1 and dimension
  first.cell.row <- round(runif(1)*(dimension)+0.5)
  first.cell.column <- round(runif(1)*(dimension)+0.5)
  
  second.cell.row <- first.cell.row
  second.cell.column <- first.cell.column
  
  while((second.cell.row == first.cell.row) & (second.cell.column == first.cell.column)){
    second.cell.row <- first.cell.row + sample(c(-1, 0, 1), 1)
    second.cell.column <- first.cell.column + sample(c(-1, 0, 1), 1)
  }
  
  #Wrap world (so second cell can't be outside of the universe)
  second.cell.row[second.cell.row==0] <- dimension
  second.cell.row[second.cell.row==dimension+1] <- 1
  second.cell.column[second.cell.column==0] <- dimension
  second.cell.column[second.cell.column==dimension+1] <- 1
  
  # Calculate probability of successful interaction
    # Agents interact with probability p equal to the percentage of traits they already share
  matches <- NULL
  for(i in 1:features){
    matches <- c(matches, substring(mat[first.cell.row, first.cell.column],i,i) == substring(mat[second.cell.row, second.cell.column],i,i))
  }
  probability.of.interaction <- sum(matches)/features
  
  # Select the first element of the neighbor that doesn't match the selected agent, to change if the agents interact
  element.to.change <- NA
      for(i in 1:features){
        if(substring(mat[first.cell.row, first.cell.column],i,i) != substring(mat[second.cell.row, second.cell.column],i,i)){
      element.to.change <- i
      break
        }
      }
  # If there is an element to change, and if the agents interact, first agent gives its selected trait to the second agent
  if(!is.na(element.to.change) & probability.of.interaction > runif(1)){
    substring(mat[second.cell.row, second.cell.column], element.to.change, element.to.change) <- substring(mat[first.cell.row, first.cell.column], element.to.change, element.to.change)
  }
  return(mat)
}

#Set function to track cultural similarity and prevalence of types
aps <- function(dimension){
  require(data.table)
  # Initiate data table giving each agent an id and x/y coordinate
  space <<- data.table(id = 1:prod(dimension*dimension),
                       x = rep(1:dimension, dimension),
                       y = rep(1:dimension, each = dimension),
                       cult = rep(NA, prod(dimension*dimension)),
                       pctsim = rep(NA, prod(dimension*dimension)),
                       color = rep(NA, prod(dimension*dimension)))
  
  # Copy agent's cultural tags over from world matrix
  for(i in 1:10){
    for(j in 1:10){
      space$cult[space$x == i & space$y == j] <<- culture[i,j]
    }
  }
  # Set agent's "type" (color) as being their first feature/trait
  space$color <<- substring(space$cult, 1,1)
  
  # Subfunction to return percentage of cultural similarity between an agent and its neighbors
  sims <- function(x_value, y_value){
    cur_id <- space[x == x_value & y == y_value, id]
    
    # Sub-subfunction to identify agent's neighbors (wrapped)
    select.neighbors <- function(dimension){
      neighbors <- as.data.frame(matrix(NA, nrow = 8, ncol = 2))
      
      # Fill neighbor matrix
      if(x_value == 1 & y_value == 1){
        neighbors[1,] <- space[x == dimension & y == y_value+1, 2:3]
        neighbors[2,] <- space[x == x_value & y == y_value+1, 2:3]
        neighbors[3,] <- space[x == x_value+1 & y == y_value+1, 2:3]
        neighbors[4,] <- space[x == dimension & y == y_value, 2:3]
        neighbors[5,] <- space[x == x_value+1 & y == y_value, 2:3]
        neighbors[6,] <- space[x == dimension & y == dimension, 2:3]
        neighbors[7,] <- space[x == x_value & y == dimension, 2:3]
        neighbors[8,] <- space[x == x_value+1 & y == dimension, 2:3]
      }
      if(x_value == 1 & y_value %in% c(2:(dimension-1))){
        neighbors[1,] <- space[x == dimension & y == y_value+1, 2:3]
        neighbors[2,] <- space[x == x_value & y == y_value+1, 2:3]
        neighbors[3,] <- space[x == x_value+1 & y == y_value+1, 2:3]
        neighbors[4,] <- space[x == dimension & y == y_value, 2:3]
        neighbors[5,] <- space[x == x_value+1 & y == y_value, 2:3]
        neighbors[6,] <- space[x == dimension & y == y_value-1, 2:3]
        neighbors[7,] <- space[x == x_value & y == y_value-1, 2:3]
        neighbors[8,] <- space[x == x_value+1 & y == y_value-1, 2:3]
      }
      if(x_value == 1 & y_value == dimension){
        neighbors[1,] <- space[x == dimension & y == 1, 2:3]
        neighbors[2,] <- space[x == x_value & y == 1, 2:3]
        neighbors[3,] <- space[x == x_value+1 & y == 1, 2:3]
        neighbors[4,] <- space[x == dimension & y == y_value, 2:3]
        neighbors[5,] <- space[x == x_value+1 & y == y_value, 2:3]
        neighbors[6,] <- space[x == dimension & y == y_value-1, 2:3]
        neighbors[7,] <- space[x == x_value & y == y_value-1, 2:3]
        neighbors[8,] <- space[x == x_value+1 & y == y_value-1, 2:3]
      }
      if(x_value %in% c(2:(dimension-1)) & y_value == 1){
        neighbors[1,] <- space[x == x_value-1 & y == y_value+1, 2:3]
        neighbors[2,] <- space[x == x_value & y == y_value+1, 2:3]
        neighbors[3,] <- space[x == x_value+1 & y == y_value+1, 2:3]
        neighbors[4,] <- space[x == x_value-1 & y == y_value, 2:3]
        neighbors[5,] <- space[x == x_value+1 & y == y_value, 2:3]
        neighbors[6,] <- space[x == x_value-1 & y == dimension, 2:3]
        neighbors[7,] <- space[x == x_value & y == dimension, 2:3]
        neighbors[8,] <- space[x == x_value+1 & y == dimension, 2:3]
      }
      if(x_value %in% c(2:(dimension-1)) & y_value %in% c(2:(dimension-1))){
        neighbors[1,] <- space[x == x_value-1 & y == y_value+1, 2:3]
        neighbors[2,] <- space[x == x_value & y == y_value+1, 2:3]
        neighbors[3,] <- space[x == x_value+1 & y == y_value+1, 2:3]
        neighbors[4,] <- space[x == x_value-1 & y == y_value, 2:3]
        neighbors[5,] <- space[x == x_value+1 & y == y_value, 2:3]
        neighbors[6,] <- space[x == x_value-1 & y == y_value-1, 2:3]
        neighbors[7,] <- space[x == x_value & y == y_value-1, 2:3]
        neighbors[8,] <- space[x == x_value+1 & y == y_value-1, 2:3] 
      }
      if(x_value %in% c(2:(dimension-1)) & y_value == dimension){
        neighbors[1,] <- space[x == x_value-1 & y == 1, 2:3]
        neighbors[2,] <- space[x == x_value & y == 1, 2:3]
        neighbors[3,] <- space[x == x_value+1 & y == 1, 2:3]
        neighbors[4,] <- space[x == x_value-1 & y == y_value, 2:3]
        neighbors[5,] <- space[x == x_value+1 & y == y_value, 2:3]
        neighbors[6,] <- space[x == x_value-1 & y == y_value-1, 2:3]
        neighbors[7,] <- space[x == x_value & y == y_value-1, 2:3]
        neighbors[8,] <- space[x == x_value+1 & y == y_value-1, 2:3]
      }
      if(x_value == dimension & y_value == 1){
        neighbors[1,] <- space[x == x_value-1 & y == y_value+1, 2:3]
        neighbors[2,] <- space[x == x_value & y == y_value+1, 2:3]
        neighbors[3,] <- space[x == 1 & y == y_value+1, 2:3]
        neighbors[4,] <- space[x == x_value-1 & y == y_value, 2:3]
        neighbors[5,] <- space[x == 1 & y == y_value, 2:3]
        neighbors[6,] <- space[x == x_value-1 & y == dimension, 2:3]
        neighbors[7,] <- space[x == x_value & y == dimension, 2:3]
        neighbors[8,] <- space[x == 1 & y == dimension, 2:3]
      }
      if(x_value == dimension & y_value %in% c(2:(dimension-1))){
        neighbors[1,] <- space[x == x_value-1 & y == y_value+1, 2:3]
        neighbors[2,] <- space[x == x_value & y == y_value+1, 2:3]
        neighbors[3,] <- space[x == 1 & y == y_value+1, 2:3]
        neighbors[4,] <- space[x == x_value-1 & y == y_value, 2:3]
        neighbors[5,] <- space[x == 1 & y == y_value, 2:3]
        neighbors[6,] <- space[x == x_value-1 & y == y_value-1, 2:3]
        neighbors[7,] <- space[x == x_value & y == y_value-1, 2:3]
        neighbors[8,] <- space[x == 1 & y == y_value-1, 2:3]
      }
      if(x_value == dimension & y_value == dimension){
        neighbors[1,] <- space[x == x_value-1 & y == 1, 2:3]
        neighbors[2,] <- space[x == x_value & y == 1, 2:3]
        neighbors[3,] <- space[x == 1 & y == 1, 2:3]
        neighbors[4,] <- space[x == x_value-1 & y == y_value, 2:3]
        neighbors[5,] <- space[x == 1 & y == y_value, 2:3]
        neighbors[6,] <- space[x == x_value-1 & y == y_value-1, 2:3]
        neighbors[7,] <- space[x == x_value & y == y_value-1, 2:3]
        neighbors[8,] <- space[x == 1 & y == y_value-1, 2:3]
      }
      names(neighbors) <- c("x","y") 
      
      # Extract ids and discard agent's id
      ids <- space[x %in% neighbors$x & y %in% neighbors$y, id]
      ids <- ids[! ids %in% space[x == x_value & y == y_value, id]]
      
      return(ids)
    }
    
    neighbor.ids <- select.neighbors(dimension = dimension)
    
    # Set empty vector to fill
    sim <- NULL
    # Check each feature of each neighbor and return TRUE if match and FALSE if mismatch
    for(i in 1:length(neighbor.ids)){
      for(j in 1:features){
        sim <- c(sim, substring(culture[x_value, y_value],j,j) == substring(space[neighbor.ids[i], cult],j,j))
      }
    }
    # Return percentage of matches
    return(sum(sim)/length(sim))
  }
  
  # Calculate percent similar for each agent
  for(i in 1:length(space$pctsim)){
    space$pctsim[i] <<- sims(x_value = space$x[i], y_value = space$y[i])
  }
  
  # Return average cultural similarity across world
  return(mean(space$pctsim))
}

# Plot the world
plotCulture <- function(title){
  require(data.table)
  require(ggplot2)
  require(RColorBrewer)
  # get number of existing traits to get the right number of colors
  traits <- as.numeric(unique(substring(space$cult, 1,1)))
  
  # find the dimensions of the grid to get the best dot size
  dims <- c(max(space$x), max(space$y))

  # plot percent similarity (gradiented by color)
  p <- ggplot(data = space, 
              aes(x = x, y = y, color = pctsim)) + 
    # resize dots to grid
    geom_point(size = 100/sqrt(prod(dimension*dimension))) +  
    scale_colour_gradientn(colours = myPalette(100), limits=c(0,1)) + 
    # theme: mostly blank
    theme_bw() + 
    theme(axis.line = element_blank(),
          axis.text.x = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks = element_blank(),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          #legend.position = "none",
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

culture <- dc.matrix()
aps(dimension = 10)
culture1 <- culture

plotCulture(title = "First")

# Iterate
for(iterations in 1:10000){
  culture <- disseminate(culture)
}
aps(dimension = 10)
plotCulture(title = "10000 iterations later")

for(iterations in 1:10000){
  culture <- disseminate(culture)
}
aps(dimension = 10)
plotCulture(title = "Another 10000 iterations later")


