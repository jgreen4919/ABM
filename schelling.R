#Schelling Segregation Model

initiateShelling <- function(dimensions = c(10, 10), n_races = 2, perc_empty = 0.2){
  require(data.table)
  # create "races" based on colours
  races <- colours()[1:n_races]
  
  # how many homes will be simulated
  n_homes = prod(dimensions)
  # calculates the number of agents
  count_agents <- floor(n_homes * (1 - perc_empty))
  
  # the characteristics that a home can have
  races <- c("empty", races)
  # the probabilities of each characteristics
  probabilities <- c(perc_empty, rep((1 - perc_empty)/(length(races) - 1), 
                                     times = length(races) - 1))
  
  # creates the global shelling data.table
  shelling <<- data.table(id = 1:prod(dimensions),
                          x = rep(1:dimensions[1], 
                                  dimensions[2]),
                          y = rep(1:dimensions[2], 
                                  each = dimensions[1]),
                          race = sample(x = races, 
                                        size = n_homes, 
                                        prob = probabilities,
                                        replace = TRUE),
                          # used to find the satisfaction of each home
                          distance = rep(NA, prod(dimensions)),
                          unsatisfied = rep(NA, prod(dimensions)))
}

plotShelling <- function(title = "Shelling-Segregation-Model"){
  require(data.table)
  require(ggplot2)
  require(RColorBrewer)
  # get races to get the right number of colors
  races <- unique(shelling$race)
  
  # find the dimensions of the grid to get the best dot size
  dims <- c(max(shelling$x), max(shelling$y))
  
  # create colours
  # check if there are less than 3 races, 
  # this would create issues with brewer.pal from 
  # RColorBrewer otherwise
  if (length(races) <= 3) {
    colors <- brewer.pal(3, "Dark2")
  } else {
    colors <- brewer.pal(length(races), "Dark2")
  }
  
  # plot the graph  
  p <- ggplot(data = shelling[race != "empty"], 
              aes(x = x, y = y, color = race)) + 
    # workaround to get relatively large dots that 
    # resize with the size of the grid
    geom_point(size = 100/sqrt(prod(dims))) +  
    scale_colour_manual(values = colors) + 
    # create a beatiful and mostly empty chart
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
    # fixes the axis to avoid distortion in the picture
    coord_fixed(ratio = 1) + 
    # lastly adds the title
    ggtitle(title)
  
  return(p)
}

iterate <- function(n = 10, similiarity_threshold = 0.5){
  require(data.table)
  # subfunction that checks for a given x and y value if the agent is 
  # unsatisfied (returns TRUE or FALSE)
  is.unsatisfied <- function(x_value, y_value, 
                             similiarity_threshold = similiarity_threshold){
    # gets the race for the agent
    cur_race <- shelling[x == x_value & y == y_value, race]
    
    # checks if the home is empty to
    if (cur_race == "empty"){
      return(FALSE) # empty houses are not satisfied, therefore will not move!
    }else{
      # creates the square of the distance
      # I avoid to take the squareroot to speed up the code
      shelling[, distance := (x_value - x)^2 + (y_value - y)^2] 
      
      # counts the number of agents that live less than two fields away 
      # (includes all adjacent agents) and that are similar
      count_similar <- nrow(shelling[distance <= 2 & 
                                       race == cur_race & 
                                       distance != 0])
      # same here except that it looks into different agents
      count_different <- nrow(shelling[distance <= 2 & 
                                         race != cur_race & 
                                         race != "empty"])
      
      # calculates the ratio
      ratio <- count_similar/(count_similar + count_different)
      
      # returns TRUE if the ratio is below the threshold
      return(ratio < similiarity_threshold)
    }
  }
  
  # creates a ProgressBar, although this is not necessary, it does look nice..
  pb <- txtProgressBar(min = 0, max = 1, style = 3)
  # for time-keeping-purposes
  t <- Sys.time()
  
  # iterates
  for (iterate in 1:n){
    # fills the boolean vector "unsatisfied" 
    # indicates if the household is unsatisfied 
    shelling[, unsatisfied := is.unsatisfied(x_value = x, 
                                             y_value = y, 
                                            similiarity_threshold = similiarity_threshold),
             by = 1:nrow(shelling)]
    
    # move unsatisfied agents to an empty house
    # find the IDs that are empty where agents can migrate to
    emptyIDs <- shelling[race == "empty", id] # finds the id of empty houses
    
    # finds the origin of the agents moving, 
    # aka. the old ID of each household moving
    oldIDs <- shelling[(unsatisfied), id] # origin
    
    # generates new IDs for each household moving
    # note that households can move to the house of other moving agents
    # also, agents can (by a small chance) choose to "move" to their 
    # existing home
    newIDs <- sample(x = c(emptyIDs, oldIDs), 
                     size = length(oldIDs), 
                     replace = F) # target
    
    # a new data.table that shows 
    # what race migrates from which origin_id to which target-id
    transition <- data.table(origin = oldIDs, 
                             oldRace = shelling[id %in% oldIDs, race],
                             target = newIDs)
    
    # moves the agents to the new homes
    shelling[id %in% transition$origin]$race = "empty"
    shelling[id %in% transition$target]$race = transition$oldRace
    
    # orders the shelling, although this takes some time, 
    # it is necessary for the other operations
    shelling <- shelling[order(id)]
    
    # updates the ProgressBar
    setTxtProgressBar(pb, iterate/n)
  }
  close(pb)
  timedif <- Sys.time() - t
  
  # print out statistics for the calculation time
  print(paste0("Time for calculation in seconds: ", round(timedif, 3), " or: ",
               round(n / as.numeric(timedif), 3), " iterations per second"))
  
  return(shelling)
}

set.seed(4212)
# initiate shelling
initiateShelling(dimensions = c(50, 50), n_races = 4)
# plot shelling
plotShelling(title = "Shelling-Segregation-Model after 0 iterations")

# iterate 10 times
shelling <- iterate(n = 10, similiarity_threshold = .5)
# plot the result after 10 iterations
plotShelling(title = "Shelling-Segregation-Model after 10 iterations")

# iterate another 10 times
shelling <- iterate(n = 10, similiarity_threshold = 0.5)
# plot again after 20 iterations total
plotShelling(title = "Shelling-Segregation-Model after 20 iterations")

# run another 30 times and plot
shelling <- iterate(n = 30, similiarity_threshold = 0.5)
plotShelling(title = "Shelling-Segregation-Model after 50 iterations")

