#Schelling Segregation Model with uneven preferences

initiateSchelling <- function(dimensions = c(10, 10), n_races = 4, perc_empty = 0.2, perc_maj = .75){
  require(data.table)
  # create "races" based on colours
  races <- colours()[1:n_races]
  dom_race <- races[1]
  min_races <- races[2:length(races)]
  
  # how many homes will be simulated
  n_homes = prod(dimensions)
  # calculates the number of agents
  count_agents <- floor(n_homes * (1 - perc_empty))
  
  # the characteristics that a home can have
  races <- c("empty", dom_race, min_races)
  # the probabilities of each characteristics
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
                          distance = rep(NA, prod(dimensions)),
                          unsatisfied = rep(NA, prod(dimensions)))
  schelling$dom_race <<- ifelse(schelling$race == dom_race, 1, 0)
}

plotSchelling <- function(title){
  require(data.table)
  require(ggplot2)
  require(RColorBrewer)
  # get races to get the right number of colors
  races <- unique(schelling$race)
  
  # find the dimensions of the grid to get the best dot size
  dims <- c(max(schelling$x), max(schelling$y))
  
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
  p <- ggplot(data = schelling[race != "empty"], 
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

iterate <- function(n = 10, dom_sim_threshold = .9, min_sim_threshold = .1){
  require(data.table)
  # subfunction that checks for a given x and y value if the agent is 
  # unsatisfied (returns TRUE or FALSE)
  is.unsatisfied <- function(x_value, y_value, 
                             dom_sim_threshold = dom_sim_threshold,
                             min_sim_threshold = min_sim_threshold){
    # gets the race for the agent
    cur_race <- schelling[x == x_value & y == y_value, race]
    dom_race <- schelling[x == x_value & y == y_value, dom_race]
    
    # checks if the home is empty to
    if (cur_race == "empty"){
      return(FALSE) # empty houses are not satisfied, therefore will not move!
    }else{
      # creates the square of the distance
      # I avoid to take the squareroot to speed up the code
      schelling[, distance := (x_value - x)^2 + (y_value - y)^2] 
      
      # counts the number of agents that live less than two fields away 
      # (includes all adjacent agents) and that are similar
      count_similar <- nrow(schelling[distance <= 2 & 
                                       race == cur_race & 
                                       distance != 0])
      # same here except that it looks into different agents
      count_different <- nrow(schelling[distance <= 2 & 
                                         race != cur_race & 
                                         race != "empty"])
      
      # calculates the ratio
      ratio <- count_similar/(count_similar + count_different)
      
      # returns TRUE if the ratio is below the threshold
      if(dom_race == 1){
      return(ratio < dom_sim_threshold)
      }else{
        return(ratio < min_sim_threshold)
      }
    }
  }
  
  # creates a ProgressBar, although this is not necessary, it does look nice..
  pb <- txtProgressBar(min = 0, max = 1, style = 3)
  # for time-keeping-purposes
  t <- Sys.time()
  
  #sets empty vector of ratios
  ratios.unsatisfied <- c(NULL)
  
  # iterates
  for (iterate in 1:n){
    # fills the boolean vector "unsatisfied" 
    # indicates if the household is unsatisfied 
    schelling[, unsatisfied := is.unsatisfied(x_value = x, 
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
    # aka. the old ID of each household moving
    oldIDs <- schelling[(unsatisfied), id] # origin
    
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
                             oldRace = schelling[id %in% oldIDs, race],
                             target = newIDs)
    
    # moves the agents to the new homes
    schelling[id %in% transition$origin]$race = "empty"
    schelling[id %in% transition$target]$race = transition$oldRace
    
    # orders the schelling, although this takes some time, 
    # it is necessary for the other operations
    schelling <- schelling[order(id)]
    
    # updates the ProgressBar
    setTxtProgressBar(pb, iterate/n)
  }
  close(pb)
  timedif <- Sys.time() - t
  
  # print out statistics for the calculation time
  print(paste0("Time for calculation in seconds: ", round(timedif, 3), " or: ",
               round(n / as.numeric(timedif), 3), " iterations per second"))
  print(ratios.unsatisfied)
    return(schelling)
}