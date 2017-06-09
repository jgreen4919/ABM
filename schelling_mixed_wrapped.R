#Schelling Segregation Model with uneven preferences (wrapped)
# Casts a wrapped universe with variable dimensions, number of races, pct empty, and dominant race share
# Iterates allowing the majority and minority races to have different preferences for neighbor homogeneity
# Based on David Zimmerman's example at: https://www.r-bloggers.com/agent-based-modelling-with-data-table-or-how-to-model-urban-migration-with-r/

initiateSchelling_wrapped <- function(dimensions = c(10, 10), n_races = 4, perc_empty = 0.2, perc_maj = .75){
  require(data.table)
  # create "races" based on colors
  dims <<- dimensions
  races <<- colours()[1:n_races]
  min_races <- races[2:length(races)]
  
  # get number of homes (equal to number of cells)
  n_homes = prod(dimensions)
  # calculates the number of agents (cells minus the percent of empty homes)
  count_agents <- floor(n_homes * (1 - perc_empty))
  
  # the characteristics that a home can have -- either empty or a particular race
  races <- c("empty", races)
  # the probabilities of a home having each characteristic
  probabilities <- c(perc_empty, 
                     (1-perc_empty)*perc_maj, 
                     rep((1 - perc_empty - (1-perc_empty)*perc_maj)/(n_races-1), times = n_races-1)
  )
  
  # creates the global schelling data.table
  schelling <<- data.table(id = 1:prod(dimensions),
                           x = rep(1:dimensions[1], 
                                   dimensions[2]),
                           y = rep(1:dimensions[2], 
                                   each = dimensions[1]),
                           race = sample(x = races, 
                                         size = n_homes, 
                                         prob = probabilities,
                                         replace = TRUE),
                           # used to find the satisfaction of each home
                           unsatisfied = rep(NA, prod(dimensions)))
}

plotSchelling <- function(title){
  require(data.table)
  require(ggplot2)
  require(RColorBrewer)
  # get races to get the right number of colors
  races <- unique(schelling$race)
  
  # find the dimensions of the grid to get the best dot size
  dims <- c(max(schelling$x), max(schelling$y))
  
  # create colors
  # check if there are 3 or fewer races, 
  # this would create issues with brewer.pal 
  # RColorBrewer otherwise
  if (length(races) <= 3) {
    colors <- brewer.pal(3, "Dark2")
  } else {
    colors <- brewer.pal(length(races), "Dark2")
  }
  
  # plot the graph  
  p <- ggplot(data = schelling[race != "empty"], 
              aes(x = x, y = y, color = race)) + 
    # resize dots to grid
    geom_point(size = 100/sqrt(prod(dims))) +  
    scale_colour_manual(values = colors) + 
    # theme: mostly blank
    theme_bw() + 
    theme(axis.line = element_blank(),
          axis.text.x = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks = element_blank(),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          legend.position = "none",
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

iterate_wrapped <- function(n = 10, dom_sim_threshold = .9, min_sim_threshold = .1){
  require(data.table)
  # subfunction that checks for a given x and y value if the agent is 
  # unsatisfied (returns TRUE or FALSE)
  is.unsatisfied_wrapped <- function(x_value, y_value, 
                             dom_sim_threshold = dom_sim_threshold,
                             min_sim_threshold = min_sim_threshold){
    # gets the race and id for the agent
  cur_race <- schelling[x == x_value & y == y_value, race]
  cur_id <- schelling[x == x_value & y == y_value, id]

    #sub-subfunction to identify agent's neighbors
  select.neighbors <- function(dimensions){

    #Identify coordinates of neighbors (adjusts wrapping based on position of agent)
    neighbors <- as.data.frame(matrix(NA, nrow = 8, ncol = 2))
    if(x_value == 1 & y_value == 1){
      neighbors[1,] <- schelling[x == dimensions[1] & y == y_value+1, 2:3]
      neighbors[2,] <- schelling[x == x_value & y == y_value+1, 2:3]
      neighbors[3,] <- schelling[x == x_value+1 & y == y_value+1, 2:3]
      neighbors[4,] <- schelling[x == dimensions[1] & y == y_value, 2:3]
      neighbors[5,] <- schelling[x == x_value+1 & y == y_value, 2:3]
      neighbors[6,] <- schelling[x == dimensions[1] & y == dimensions[2], 2:3]
      neighbors[7,] <- schelling[x == x_value & y == dimensions[2], 2:3]
      neighbors[8,] <- schelling[x == x_value+1 & y == dimensions[2], 2:3]
    }
    if(x_value == 1 & y_value %in% c(2:(dimensions[2]-1))){
      neighbors[1,] <- schelling[x == dimensions[1] & y == y_value+1, 2:3]
      neighbors[2,] <- schelling[x == x_value & y == y_value+1, 2:3]
      neighbors[3,] <- schelling[x == x_value+1 & y == y_value+1, 2:3]
      neighbors[4,] <- schelling[x == dimensions[1] & y == y_value, 2:3]
      neighbors[5,] <- schelling[x == x_value+1 & y == y_value, 2:3]
      neighbors[6,] <- schelling[x == dimensions[1] & y == y_value-1, 2:3]
      neighbors[7,] <- schelling[x == x_value & y == y_value-1, 2:3]
      neighbors[8,] <- schelling[x == x_value+1 & y == y_value-1, 2:3]
    }
    if(x_value == 1 & y_value == dimensions[2]){
      neighbors[1,] <- schelling[x == dimensions[1] & y == 1, 2:3]
      neighbors[2,] <- schelling[x == x_value & y == 1, 2:3]
      neighbors[3,] <- schelling[x == x_value+1 & y == 1, 2:3]
      neighbors[4,] <- schelling[x == dimensions[1] & y == y_value, 2:3]
      neighbors[5,] <- schelling[x == x_value+1 & y == y_value, 2:3]
      neighbors[6,] <- schelling[x == dimensions[1] & y == y_value-1, 2:3]
      neighbors[7,] <- schelling[x == x_value & y == y_value-1, 2:3]
      neighbors[8,] <- schelling[x == x_value+1 & y == y_value-1, 2:3]
    }
    if(x_value %in% c(2:(dimensions[1]-1)) & y_value == 1){
      neighbors[1,] <- schelling[x == x_value-1 & y == y_value+1, 2:3]
      neighbors[2,] <- schelling[x == x_value & y == y_value+1, 2:3]
      neighbors[3,] <- schelling[x == x_value+1 & y == y_value+1, 2:3]
      neighbors[4,] <- schelling[x == x_value-1 & y == y_value, 2:3]
      neighbors[5,] <- schelling[x == x_value+1 & y == y_value, 2:3]
      neighbors[6,] <- schelling[x == x_value-1 & y == dimensions[2], 2:3]
      neighbors[7,] <- schelling[x == x_value & y == dimensions[2], 2:3]
      neighbors[8,] <- schelling[x == x_value+1 & y == dimensions[2], 2:3]
    }
    if(x_value %in% c(2:(dimensions[1]-1)) & y_value %in% c(2:(dimensions[2]-1))){
      neighbors[1,] <- schelling[x == x_value-1 & y == y_value+1, 2:3]
      neighbors[2,] <- schelling[x == x_value & y == y_value+1, 2:3]
      neighbors[3,] <- schelling[x == x_value+1 & y == y_value+1, 2:3]
      neighbors[4,] <- schelling[x == x_value-1 & y == y_value, 2:3]
      neighbors[5,] <- schelling[x == x_value+1 & y == y_value, 2:3]
      neighbors[6,] <- schelling[x == x_value-1 & y == y_value-1, 2:3]
      neighbors[7,] <- schelling[x == x_value & y == y_value-1, 2:3]
      neighbors[8,] <- schelling[x == x_value+1 & y == y_value-1, 2:3] 
    }
    if(x_value %in% c(2:(dimensions[1]-1)) & y_value == dimensions[2]){
      neighbors[1,] <- schelling[x == x_value-1 & y == 1, 2:3]
      neighbors[2,] <- schelling[x == x_value & y == 1, 2:3]
      neighbors[3,] <- schelling[x == x_value+1 & y == 1, 2:3]
      neighbors[4,] <- schelling[x == x_value-1 & y == y_value, 2:3]
      neighbors[5,] <- schelling[x == x_value+1 & y == y_value, 2:3]
      neighbors[6,] <- schelling[x == x_value-1 & y == y_value-1, 2:3]
      neighbors[7,] <- schelling[x == x_value & y == y_value-1, 2:3]
      neighbors[8,] <- schelling[x == x_value+1 & y == y_value-1, 2:3]
    }
    if(x_value == dimensions[1] & y_value == 1){
      neighbors[1,] <- schelling[x == x_value-1 & y == y_value+1, 2:3]
      neighbors[2,] <- schelling[x == x_value & y == y_value+1, 2:3]
      neighbors[3,] <- schelling[x == 1 & y == y_value+1, 2:3]
      neighbors[4,] <- schelling[x == x_value-1 & y == y_value, 2:3]
      neighbors[5,] <- schelling[x == 1 & y == y_value, 2:3]
      neighbors[6,] <- schelling[x == x_value-1 & y == dimensions[2], 2:3]
      neighbors[7,] <- schelling[x == x_value & y == dimensions[2], 2:3]
      neighbors[8,] <- schelling[x == 1 & y == dimensions[2], 2:3]
    }
    if(x_value == dimensions[1] & y_value %in% c(2:(dimensions[2]-1))){
      neighbors[1,] <- schelling[x == x_value-1 & y == y_value+1, 2:3]
      neighbors[2,] <- schelling[x == x_value & y == y_value+1, 2:3]
      neighbors[3,] <- schelling[x == 1 & y == y_value+1, 2:3]
      neighbors[4,] <- schelling[x == x_value-1 & y == y_value, 2:3]
      neighbors[5,] <- schelling[x == 1 & y == y_value, 2:3]
      neighbors[6,] <- schelling[x == x_value-1 & y == y_value-1, 2:3]
      neighbors[7,] <- schelling[x == x_value & y == y_value-1, 2:3]
      neighbors[8,] <- schelling[x == 1 & y == y_value-1, 2:3]
    }
    if(x_value == dimensions[1] & y_value == dimensions[2]){
      neighbors[1,] <- schelling[x == x_value-1 & y == 1, 2:3]
      neighbors[2,] <- schelling[x == x_value & y == 1, 2:3]
      neighbors[3,] <- schelling[x == 1 & y == 1, 2:3]
      neighbors[4,] <- schelling[x == x_value-1 & y == y_value, 2:3]
      neighbors[5,] <- schelling[x == 1 & y == y_value, 2:3]
      neighbors[6,] <- schelling[x == x_value-1 & y == y_value-1, 2:3]
      neighbors[7,] <- schelling[x == x_value & y == y_value-1, 2:3]
      neighbors[8,] <- schelling[x == 1 & y == y_value-1, 2:3]
    }
    names(neighbors) <- c("x","y")
    
    ids <- schelling[x %in% neighbors$x & y %in% neighbors$y, id]
    
    return(ids)
  }
    # checks if the home is empty
  if (cur_race == "empty"){
    return(FALSE) # empty houses can't be unsatisfied
  }else{
    neighbor.ids <- select.neighbors(dimensions = dims)

    # counts simliar neighbors
    count_similar <- sum(schelling[id %in% neighbor.ids, 
                                      id != cur_id &
                                      race == cur_race])
    # counts different neighbors
    count_different <- sum(schelling[id %in% neighbor.ids, 
                                        id != cur_id &
                                        race != cur_race & 
                                        race != "empty"])
    
    # calculates the ratio
    ratio <- count_similar/(count_similar + count_different)
    
    # returns TRUE if the ratio is less than or equal to the threshold
    if(cur_race == races[1]){
      return(ratio <= dom_sim_threshold)
    }else{
      return(ratio <= min_sim_threshold)
    }
  }
  }

  # creates a ProgressBar
  pb <- txtProgressBar(min = 0, max = 1, style = 3)
  # and tracks time
  t <- Sys.time()
  
  #sets empty vector of ratios
  ratios.unsatisfied <- c(NULL)
  
  # iterates
  for (iterate in 1:n){
    # fills the boolean vector "unsatisfied" 
    # indicates if the household is unsatisfied 
    schelling[, unsatisfied := is.unsatisfied_wrapped(x_value = x, 
                                              y_value = y, 
                                              dom_sim_threshold = dom_sim_threshold,
                                              min_sim_threshold = min_sim_threshold),
              by = 1:nrow(schelling)]
    
    ratio.unsatisfied <- sum(schelling$unsatisfied)/nrow(schelling)
    ratios.unsatisfied <- c(ratios.unsatisfied, ratio.unsatisfied)
    
    # move unsatisfied agents to an empty house
    # find the IDs that are empty where agents can migrate to
    emptyIDs <- schelling[race == "empty", id] # finds the id of empty houses
    
    # finds the origin of the agents moving, 
    oldIDs <- schelling[(unsatisfied), id] # origin
    
    # generates new IDs for each household moving
    # note that households can move to the house of other moving agents
    # also, agents can (by a small chance) choose to "move" to their 
    # existing home
    newIDs <- sample(x = c(emptyIDs, oldIDs), 
                     size = length(oldIDs), 
                     replace = F) # target
    
    # cast a new data.table showing migration from origin_id to target-id
    transition <- data.table(origin = oldIDs, 
                             oldRace = schelling[id %in% oldIDs, race],
                             target = newIDs)
    
    # move agents to the new homes
    schelling[id %in% transition$origin]$race = "empty"
    schelling[id %in% transition$target]$race = transition$oldRace
    
    # orders and reset schelling
    schelling <<- schelling[order(id)]
    
    # updates the ProgressBar
    setTxtProgressBar(pb, iterate/n)
  }
  close(pb)
  timedif <- Sys.time() - t
  
  # print out statistics for the calculation time
  print(paste0("Time for calculation in seconds: ", round(timedif, 3), " or: ",
               round(n / as.numeric(timedif), 3), " iterations per second"))
  print(ratios.unsatisfied)
  plotSchelling(title = paste0("Schelling Segregation Model after ", n, " Iterations (Wrapped)"))
}

initiateSchelling_wrapped(dimensions = c(50,50), n_races = 2, perc_empty = .2, perc_maj = .6)
plotSchelling(title = "Schelling Segregation Model (Wrapped Environment)")
iterate_wrapped(n = 10, dom_sim_threshold = .5, min_sim_threshold = .5)
