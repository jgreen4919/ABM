# Sugarscape replication (from Epstein and Axtell 1996)
# Initiates world of x by x dimensions populated by agents with density d.
# Each cell in the world has a sugar capacity c.
# Each agent has a vision between 1 and maxviz, and a store of sugar.
# In each round, each cell grows sugar at rate grate unless it is at capacity.
# Agents look around, determine which available cell in their field of vision has the most sugar, move there, and harvest the sugar
# Sugar stores are then decremented by the agent's metabolic rate


# The first function initiates world of x by x dimensions, populated by agnets with desity d. 
# Each cell in the world has a sugar capacity c and initially has c amount of sugar. 
# Each agent has an id, vision ranging from 1 to maxviz, and a store of sugar s.
# When world is initialized, agent harvests the sugar in their initial cell

do.sugarscape <- function(dim = 50, popdens = .25, capacity = 4, grate = 1, 
                          maxviz = 3, r_endow = 5:25, metabolism = 3:5,
                          characteristics = c("id","vision","sugar","metabolism")){
  side <<- dim
  characteristics <<- characteristics
  r_endow <<- r_endow
  metabolism <<- metabolism
  
  require(data.table)
  sugar <- matrix(NA, nrow = dim, ncol = dim)
  for(i in 1:length(sugar)){
    sugar[i] <- sample(0:capacity, 1) # Distribute sugar to each cell sampled from 0 to capacity
  }
  
  n_agents <- round(dim*dim*popdens) # Set number of agents

  a <- matrix(NA, nrow = dim, ncol = dim)
  for(i in 1:length(a)){
    a[i] <- sample(1:(dim*dim), 1, replace = F) # Set cells that will be populated by agents
  }
  a[a > n_agents] <- NA
  
  agents <<- array(0, dim=c(dim, dim, length(characteristics))) # Set array of agents
  agcount <- 0
  for (i in 1:dim){
    for (j in 1:dim){
      # If cell is populated by an agent, assign agent characteristics
      if(!is.na(a[i,j])){
        agents[i,j,1] <<- agcount+1
        agcount <- agcount+1
        agents[i,j,2] <<- sample(1:maxviz, 1)
        agents[i,j,3] <<- sample(r_endow, 1)
        agents[i,j,4] <<- sample(metabolism, 1)
      }
      else{
        agents[i,j,1] <<- NA
        agents[i,j,2] <<- NA
        agents[i,j,3] <<- NA
        agents[i,j,4] <<- NA
      }
    }
  }  
  # Initialize the sugarscape. Each cell starts with an id, x value, and y value
  scape <<- data.table(cellid = 1:prod(dim*dim),
                       x = rep(1:dim, dim),
                       y = rep(1:dim, each = dim),
                       cellsugar = rep(NA, prod(dim*dim)),
                       cellcap = rep(NA, prod(dim*dim)),
                       cellgrate = rep(grate, prod(dim*dim)),
                       agid = rep(NA, prod(dim*dim)),
                       agviz = rep(NA, prod(dim*dim)),
                       agmetab = rep(NA, prod(dim*dim)),
                       agsugar = rep(NA, prod(dim*dim))
  )
  # Copy sugar and agent/agent characteristics over to sugarscape
  for(i in 1:dim){
    for(j in 1:dim){
      scape$cellsugar[scape$x == i & scape$y == j] <<- sugar[i,j]
      scape$cellcap[scape$x == i & scape$y == j] <<- sugar[i,j]
      scape$agid[scape$x == i & scape$y == j] <<- agents[i,j,1]
      scape$agviz[scape$x == i & scape$y == j] <<- agents[i,j,2]
      scape$agmetab[scape$x == i & scape$y == j] <<- agents[i,j,4]
      scape$agsugar[scape$x == i & scape$y == j] <<- agents[i,j,3]
    }
  }
  max.agid <<- max(scape$agid, na.rm = T) # Store the max agid to manipulate later
  
  # Agents harvest whatever sugar is in the cell they are populated to
  for(i in 1:length(scape$cellsugar)){
    if(!is.na(scape$agsugar[i])){
      s <- scape$cellsugar[i]
      scape$agsugar[i] <<- scape$agsugar[i] + s
      scape$cellsugar[i] <<- scape$cellsugar[i] - s
    }
  }
}

plotScape <- function(title = "Sugarscape"){
  require(data.table)
  require(ggplot2)
  require(RColorBrewer)
  
  agts <- subset(scape, !is.na(agid), select = c(x,y,agsugar))
  
  # find the dimensions of the grid to get the best dot size
  dims <- c(max(scape$x), max(scape$y))
  
  # plot the landscape gradiented by density of sugar in each cell  
  p <- ggplot() + 
    # resize dots to grid
    geom_point(data = scape, 
               aes(x = x, y = y, color = cellsugar), size = 100/sqrt(prod(dims))) +
    scale_colour_gradient(low = "white",high = "yellow", limits = c(0,4)) +
    geom_point(data = scape[!is.na(agid)], aes(x=x, y=y, shape = shp, fill = agsugar),
               shape = 21, size = 100/sqrt(prod(dims))) +
    scale_fill_gradient(low = "white", high = "red")+
    # add agents
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

# Function specifying interaction rules for each round. Sugar grows back in cells at rate grate.
iterate <- function(iterations){
  require(reldist)
  require(msm)

  mean_vision <- mean(scape$agviz, na.rm = T) # Store initial mean population vision
  mean_metab <- mean(scape$agmetab, na.rm = T) # Store initial mean population metabolism
  gcoef <- gini(scape$agsugar[!is.na(scape$agsugar)]) # Store initial Gini coefficient
  for(i in 1:iterations){
    # Grow sugar
    scape$cellsugar <<- with(scape, cellsugar + cellgrate)
    scape$cellsugar <<- ifelse(scape$cellsugar > scape$cellcap, scape$cellcap, scape$cellsugar)
    
    # Subfunction for agents to pick cell to move to (wrapped world)
    pickcell <- function(x_value, y_value, maxviz, dimension){
      x_vals <- c((x_value+1):(x_value+maxviz), (x_value-1):(x_value-maxviz))
      x_vals <- sapply(x_vals, function(x){
        if(x < 1){x = x + dimension} 
        if(x > dimension){x = x - dimension}
        else{x}})
      
      y_vals <- c((y_value+1):(y_value+maxviz), (y_value-1):(y_value-maxviz))
      y_vals <- sapply(y_vals, function(x){
        if(x < 1){x = x + dimension}
        if(x > dimension){x = x - dimension}
        else{x}})
      
      see.ids <- c(scape[x == x_value & y %in% y_vals, cellid], scape[x %in% x_vals & y == y_value, cellid]) # Cells agents can see
      see.ids <- see.ids[! see.ids %in% transition[,target]] # Minus cells other agents have already decided to move to
      
      # Find the cell(s) with the most sugar within the available cells the agent sees
      if(length(see.ids) > 0){
        newcell <- scape[cellid %in% see.ids & is.na(agid) & cellsugar == max(scape[cellid %in% see.ids, cellsugar]), cellid]
      }else{
        return(scape[x == x_value & y == y_value, cellid]) #If all cells in field of vision are taken, stay put
      }
      
      if(length(newcell) > 1){
        dist <- sapply(1:length(newcell), function(a){
          abs(scape[x == x_value & y == y_value, x] - scape[cellid %in% newcell[a], x]) + 
            abs(scape[x == x_value & y == y_value, y] - scape[cellid %in% newcell[a], y])
        })
        
        newcell <- newcell[which.min(dist)]
        return(newcell) # If multiple available cells with max sugar, return nearest (if equadistant, pick first)
      }
      if(length(newcell) == 0){
        return(scape[x == x_value & y == y_value, cellid]) # If no available cell, stay put
      }
      else{
        return(newcell)
      }
    }

    transition <- subset(scape, !is.na(agid), c(1:3,7:10)) # Placeholder data table to track movement
    transition[,target := rep(NA, nrow(transition))]
    for(t in 1:nrow(transition)){
      transition$target[t] <- pickcell(x_value = transition$x[t], y_value = transition$y[t], 
                                       maxviz = transition$agviz[t], dimension = max(scape$x))
    }
    
    # move agents to the new cells
    for(n in 1:nrow(scape)){
      if(scape$cellid[n] %in% transition[,target]){
        scape[transition[scape$cellid[n] == target, cellid], 7:10] <<- NA
        scape[n,7:10] <<- transition[target == n, 4:7]
      }
    }
    
    # Harvest sugar
    for(s in 1:nrow(scape)){
      if(!is.na(scape$agid[s])){
        su <- scape$cellsugar[s]
        scape$agsugar[s] <<- scape$agsugar[s] + su
        scape$cellsugar[s] <<- scape$cellsugar[s] - su
      }
    }

    scape$agsugar <<- with(scape, ifelse(!is.na(agsugar), agsugar - agmetab, agsugar)) # decrement agent sugar by metabolic rate
    
    turnover <- nrow(scape[agsugar <= 0]) # How many agents have run out of sugar
    scape[agsugar <= 0, 7:10] <<- NA # Agents that run out of sugar die
    
    new.cells <- sample(scape[is.na(agid), cellid], turnover, replace = F) # Find as many new cells to populate with new agents as there are agents who died this round

    # Populate new agents
    for(a in 1:length(new.cells)){
      scape$agid[scape$cellid == new.cells[a]] <<- max.agid+a
      scape$agviz[scape$cellid == new.cells[a]] <<- round(rtnorm(1, mean = mean(scape$agviz, na.rm = T),
                                                       sd = sd(scape$agviz, na.rm = T),
                                                       lower = 1, upper = max(scape$agviz, na.rm = T)))
      scape$agmetab[scape$cellid == new.cells[a]] <<- round(rtnorm(1, mean = mean(scape$agmetab, na.rm = T),
                                                       sd = sd(scape$agmetab, na.rm = T),
                                                       lower = 1, upper = max(scape$agmetab, na.rm = T)))
      scape$agsugar[scape$cellid == new.cells[a]] <<- sample(r_endow, 1)
    }
    max.agid <<- max.agid + turnover # Update new max agent id
    
    n_agents <- c(n_agents, nrow(scape[agid > 0])) # Store new number of agents
    mean_vision <- c(mean_vision, mean(scape$agviz, na.rm = T)) # Store new mean population vision
    mean_metab <- c(mean_metab, mean(scape$agmetab, na.rm = T)) # Store new mean population metabolism
    gcoef <- c(gcoef, gini(scape$agsugar[!is.na(scape$agsugar)])) # Store new gini coefficient
  }
  # Store vectors of characteristics in a list and return them
  returns <- list(n_agents, mean_vision, mean_metab, gcoef)
  names(returns) <- c("agents","mean.vision", "mean.metabolism", "Gini")
  return(returns)
}

do.sugarscape()
plotScape(title = "first")
iterate(iterations = 50)
plotScape(title = "50 later")
max.agid
