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

do.sugarscape <- function(dim = 50, popdens = .25, capacity = 4, maxviz = 3, characteristics = c("id","vision","sugar")){
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
        agents[i,j,3] <<- 0
      }
      else{
        agents[i,j,1] <<- NA
        agents[i,j,2] <<- NA
        agents[i,j,3] <<- NA
      }
    }
  }  
  # Initialize the sugarscape. Each cell starts with an id, x value, and y value
  scape <<- data.table(cellid = 1:prod(dim*dim),
                       x = rep(1:dim, dim),
                       y = rep(1:dim, each = dim),
                       cellsugar = rep(NA, prod(dim*dim)),
                       cellcap = rep(NA, prod(dim*dim)),
                       agid = rep(NA, prod(dim*dim)),
                       agviz = rep(NA, prod(dim*dim)),
                       agsugar = rep(NA, prod(dim*dim))
  )
  # Copy sugar and agent/agent characteristics over to sugarscape
  for(i in 1:dim){
    for(j in 1:dim){
      scape$cellsugar[scape$x == i & scape$y == j] <<- sugar[i,j]
      scape$cellcap[scape$x == i & scape$y == j] <<- sugar[i,j]
      scape$agid[scape$x == i & scape$y == j] <<- agents[i,j,1]
      scape$agviz[scape$x == i & scape$y == j] <<- agents[i,j,2]
      scape$agsugar[scape$x == i & scape$y == j] <<- agents[i,j,3]
    }
  }

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
iterate <- function(iterations, capacity = 4, grate = 1, metabolism = 1){
  for(i in iterations){
    # Grow sugar
    scape$cellsugar <<- scape$cellsugar+1
    scape$cellsugar[scape$cellsugar >= capacity] <<- capacity
    
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
      
      # Find the cell with the most sugar within the available cells the agent sees
      newcell <- scape[cellid %in% see.ids & is.na(agid) & cellsugar %in% max(unlist(scape[cellid %in% see.ids, cellsugar])), cellid]
      
      if(length(newcell) > 1){
        return(newcell[1]) # If multiple cells have max available sugar, pick the first one
      }
      if(length(newcell) == 0){
        return(scape[x == x_value & y == y_value, cellid]) # If no available cell, stay put
      }
      else{
        return(newcell)
      }
    }
    
    transition <- subset(scape, !is.na(agid), c(1:3,6:8)) # Placeholder data table to track movement
    transition[,target := rep(NA, nrow(transition))]
    # Determine where each agent is moving
    for(i in 1:nrow(transition)){
      transition$target[i] <- pickcell(x_value = transition$x[i], y_value = transition$y[i], 
                                       maxviz = transition$agviz[i], dimension = max(scape$x))
    }

    # move agents to the new cells
    for(i in 1:nrow(scape)){
      if(scape$cellid[i] %in% transition[,target]){
        scape[transition[scape$cellid[i] == target, cellid], 6:8] <<- NA
        scape[i,6:8] <<- transition[target == i, 4:6]
      }
    }

    # Harvest sugar
    for(i in 1:nrow(scape)){
      if(!is.na(scape$agid[i])){
        s <- scape$cellsugar[i]
        scape$agsugar[i] <<- scape$agsugar[i] + s
        scape$cellsugar[i] <<- scape$cellsugar[i] - s
      }
    }
    scape$agsugar <<- with(scape, ifelse(!is.na(agsugar), agsugar - metabolism, agsugar)) # decrement agent sugar by metabolic rate
    #Update agent array
    for (i in 1:max(scape$x)){
      for (j in 1:max(scape$y)){
        # If cell is populated by an agent, assign agent characteristics
          agents[i,j,1] <<- scape[x == i & y == j, agid]
          agents[i,j,2] <<- scape[x == i & y == j, maxviz]
          agents[i,j,3] <<- scape[x == i & y == j, agsugar]
      }
    }
  }
}

do.sugarscape()
plotScape()
iterate(iterations = 5)
plotScape(title = "Sugarscape after seven rounds of foraging")

