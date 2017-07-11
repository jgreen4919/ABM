# Sugarscape replication (from Epstein and Axtell 1996)
# Initiates world of x by x dimensions populated by agents with density d.
# Each cell in the world has a sugar capacity c. Sugar is organized into two concentrations at corners of the scape.
# Each agent has a vision between 1 and maxviz, and a store of sugar.
# In each round, each cell grows sugar at rate grate unless it is at capacity.
# Agents look around, determine which available cell in their field of vision has the most sugar, move there, and harvest the sugar
# Sugar stores are then decremented by the agent's metabolic rate
# If sugar stores fall to at or below zero, agent dies

# Function tracks number of agents, mean vision, mean metabolism, and Gini coefficient by round

# The first function initiates world of x by x dimensions, populated by agnets with desity d. 
# Each cell in the world has a sugar capacity c and initially has c amount of sugar. 
# Each agent has an id, vision ranging from 1 to maxviz, and a store of sugar s.
# When world is initialized, agent harvests the sugar in their initial cell

do.sugarscape.organized <- function(dim = 50, popdens = .25, capacity = 4, grate = 1, old.age = 60,
                                    maxviz = 6, r_endow = 5:25, metabolism = 1:4, age = 1:60, gender = c(0,1),
                                    characteristics = c("id","vision","sugar","metabolism","age","gender")){
  require(data.table)
  sugar <- matrix(rep(0, dim^2), nrow=dim, byrow=TRUE)
  sugar[abs(row(sugar)-col(sugar)) < (50-(0.01*row(sugar)*col(sugar)))] <- 0
  sugar[abs((dim-row(sugar)+1)-(dim-col(sugar)+1)) < (50-(0.01*(dim-row(sugar)+1)*(dim-col(sugar)+1)))] <- 0
  sugar[((row(sugar)-25)^2)+((col(sugar)-25)^2) < 500] <- capacity / 2
  sugar[((row(sugar)-25)^2)+((col(sugar)-25)^2) < 250] <- round(capacity*.67)
  sugar[((row(sugar)-25)^2)+((col(sugar)-25)^2) < 100] <- capacity
  sugar[((row(sugar)-75)^2)+((col(sugar)-75)^2) < 500] <- capacity / 2
  sugar[((row(sugar)-75)^2)+((col(sugar)-75)^2) < 250] <- round(capacity*.67)
  sugar[((row(sugar)-75)^2)+((col(sugar)-75)^2) < 100] <- capacity
  
  n_agents <- round(dim*dim*popdens) # Set number of agents
  old.age <<- old.age
  
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
        agents[i,j,5] <<- sample(age, 1)
        agents[i,j,6] <<- sample(gender, 1)
      }
      else{
        agents[i,j,1] <<- NA
        agents[i,j,2] <<- NA
        agents[i,j,3] <<- NA
        agents[i,j,4] <<- NA
        agents[i,j,5] <<- NA
        agents[i,j,6] <<- NA
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
                       agsugar = rep(NA, prod(dim*dim)),
                       agage = rep(NA, prod(dim*dim)),
                       aggender = rep(NA, prod(dim*dim))
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
      scape$agage[scape$x == i & scape$y == j] <<- agents[i,j,5]
      scape$aggender[scape$x == i & scape$y == j] <<- agents[i,j,6]
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

# The second function plots the sugarscape
plotScape <- function(title = "Sugarscape"){
  require(data.table)
  require(ggplot2)
  require(RColorBrewer)
  
  # find the dimensions of the grid to get the best dot size
  dims <- c(max(scape$x), max(scape$y))
  
  # plot the landscape gradiented by density of sugar in each cell  
  p <- ggplot() + 
    # resize dots to grid
    geom_point(data = scape, 
               aes(x = x, y = y, color = cellsugar), size = 100/sqrt(prod(dims))) +
    scale_colour_gradient(low = "white",high = "yellow", limits = c(0,max(scape$cellsugar))) +
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

# The last function specifies interaction rules for each round.
iterate <- function(iterations, store.plots = FALSE){
  require(reldist)
  n_agents <- nrow(scape[agid > 0]) # Store initial number of agents
  mean_vision <- mean(scape$agviz, na.rm = T) # Store initial mean population vision
  mean_metab <- mean(scape$agmetab, na.rm = T) # Store initial mean population metabolism
  gcoef <- gini(scape$agsugar[!is.na(scape$agsugar)]) # Store initial Gini coefficient
  starved <- c(NULL)
  aged <- c(NULL)
  if(store.plots == TRUE){
    viz <<- list(NULL)
    viz[[1]] <<- plotScape(title = "Initial Sugarscape") 
  }
  for(i in 1:iterations){
    # Grow sugar
    scape$cellsugar <<- with(scape, cellsugar + cellgrate)
    scape$cellsugar <<- ifelse(scape$cellsugar > scape$cellcap, scape$cellcap, scape$cellsugar)
    
    # Subfunction for agents to pick cell to move to (unwrapped world)
    pickcell <- function(x_value, y_value, maxviz, dimension){
      x_vals <- c((x_value+1):(x_value+maxviz), (x_value-1):(x_value-maxviz))
      x_vals <- sapply(x_vals, function(x){
        if(x < 1){x = 1} 
        if(x > dimension){x = dimension}
        else{x}})
      
      y_vals <- c((y_value+1):(y_value+maxviz), (y_value-1):(y_value-maxviz))
      y_vals <- sapply(y_vals, function(x){
        if(x < 1){x = 1}
        if(x > dimension){x = dimension}
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
    
    transition <- subset(scape, !is.na(agid), c(1:3,7:ncol(scape))) # Placeholder data table to track movement
    transition[,target := rep(NA, nrow(transition))]
    # Determine where each agent is moving
    for(t in 1:nrow(transition)){
      transition$target[t] <- pickcell(x_value = transition$x[t], y_value = transition$y[t], 
                                       maxviz = transition$agviz[t], dimension = max(scape$x))
    }
    
    # move agents to the new cells
    for(n in 1:nrow(scape)){
      if(scape$cellid[n] %in% transition[,target]){
        scape[transition[scape$cellid[n] == target, cellid], 7:ncol(scape)] <<- NA
        scape[n,7:ncol(scape)] <<- transition[target == n, 4:(ncol(transition)-1)]
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
    starved <- c(starved, scape[agsugar <= 0, agid]) # Update vector of agent IDs for starved agents
    scape[agsugar <= 0, 7:ncol(scape)] <<- NA # Agents that run out of sugar starve
    
    scape$agage <<- scape$agage + 1
    old.ids <- scape[agage > old.age, agid] # List of IDs of old agents
    if(length(old.ids) > 0){
      for(i in 1:length(old.ids)){
        # Old agents have Old.Age/Agent.Age chance of dying of old age
        if(runif(1) > (old.age/scape[agid == old.ids[i], agage])){
          aged <- c(aged, old.ids[i]) # Update vector of agents who have died of old age
          scape[agid == old.ids[i], 7:ncol(scape)] <- NA 
        }
      }
    }
    n_agents <- c(n_agents, nrow(scape[agid > 0])) # Store new number of agents
    mean_vision <- c(mean_vision, mean(scape$agviz, na.rm = T)) # Store new mean population vision
    mean_metab <- c(mean_metab, mean(scape$agmetab, na.rm = T)) # Store new mean population metabolism
    gcoef <- c(gcoef, gini(scape$agsugar[!is.na(scape$agsugar)])) # Store new gini coefficient
    if(store.plots == TRUE){
      viz[[i+1]] <<- plotScape(title = paste("Sugarscape after round ", i, sep = "")) # Store plot for each iteration
    }
  }
  # Store vectors of characteristics in a list and return them
  returns <- list(n_agents, mean_vision, mean_metab, gcoef, starved, aged)
  names(returns) <- c("agents","mean.vision", "mean.metabolism", "Gini", "Starved", "Aged")
  return(returns)
}

# Vary different characteristics over 30-round runs
do.sugarscape.organized()
run1 <- iterate(iterations = 30, store.plots = TRUE)
viz1 <- viz
for(i in 1:length(viz1)){
  dev.copy(png, paste("defaultscape_frame",i,sep=""))
  viz1[[i]]
  dev.off()
}

do.sugarscape.organized(capacity = 6)
run2 <- iterate(iterations = 30, store.plots = TRUE)
viz2 <- viz
for(i in 1:length(viz2)){
  dev.copy(png, paste("highcapscape_frame",i,sep=""))
  viz2[[i]]
  dev.off()
}


do.sugarscape.organized(metabolism = 3:5)
run3 <- iterate(iterations= 30, store.plots = TRUE)
viz3 <- viz
for(i in 1:length(viz3)){
  dev.copy(png, paste("highmetabscape_frame",i,sep=""))
  viz3[[i]]
  dev.off()
}

do.sugarscape.organized(maxviz = 9)
run4 <- iterate(iterations= 30, store.plots = TRUE)
viz4 <- viz
for(i in 1:length(viz4)){
  dev.copy(png, paste("highvizscape_frame",i,sep=""))
  viz4[[i]]
  dev.off()
}

do.sugarscape.organized(maxviz = 3)
run5 <- iterate(iterations= 30, store.plots = TRUE)
viz5 <- viz
for(i in 1:length(viz5)){
  dev.copy(png, paste("lowvizscape_frame",i,sep=""))
  viz5[[i]]
  dev.off()
}

# Track population characteristics over time, by run

# Number of agents (carrying capacity)
for(i in 1:length(run1$agents)){
  if(i == 1){
    plot(-100, -100, xlim=c(1,30), ylim=c(0,700), ylab="Agents", xlab="Iteration", type="n", cex.axis=0.8, main = "Carrying Capacity")
  }else{
    segments(i-1, run1$agents[i-1], i, run1$agents[i], col = "green", lwd=2)
    segments(i-1, run2$agents[i-1], i, run2$agents[i], col = "blue", lwd=2)
    segments(i-1, run3$agents[i-1], i, run3$agents[i], col = "red", lwd=2)
    segments(i-1, run4$agents[i-1], i, run4$agents[i], col = "orange", lwd=2)
    segments(i-1, run5$agents[i-1], i, run5$agents[i], col = "purple", lwd=2)
  }
}
legend(x = 1, y = 200, legend = c("Default","High Capacity", "High Metabolism", "High Vision", "Low Vision"), 
       fill = c("green","blue","red","orange","purple"))

# Mean vision
for(i in 1:length(run1$mean.vision)){
  if(i == 1){
    plot(-100, -100, xlim=c(1,30), ylim=c(1,6), ylab="Mean Vision", xlab="Iteration", type="n", cex.axis=0.8, main = "Selection for Vision")
  }else{
    segments(i-1, run1$mean.vision[i-1], i, run1$mean.vision[i], col = "green", lwd=2)
    segments(i-1, run2$mean.vision[i-1], i, run2$mean.vision[i], col = "blue", lwd=2)
    segments(i-1, run3$mean.vision[i-1], i, run3$mean.vision[i], col = "red", lwd=2)
    segments(i-1, run4$mean.vision[i-1], i, run4$mean.vision[i], col = "orange", lwd=2)
    segments(i-1, run5$mean.vision[i-1], i, run5$mean.vision[i], col = "purple", lwd=2)
  }
}
legend(x = 1, y = 3.1, legend = c("Default","High Capacity", "High Metabolism", "High Vision", "Low Vision"), 
       fill = c("green","blue","red","orange","purple"))

# Mean metabolism
for(i in 1:length(run1$mean.metabolism)){
  if(i == 1){
    plot(-100, -100, xlim=c(1,30), ylim=c(0,5), ylab="Mean Metabolism", xlab="Iteration", type="n", cex.axis=0.8, main = "Selection for Metabolism")
  }else{
    segments(i-1, run1$mean.metabolism[i-1], i, run1$mean.metabolism[i], col = "green", lwd=2)
    segments(i-1, run2$mean.metabolism[i-1], i, run2$mean.metabolism[i], col = "blue", lwd=2)
    segments(i-1, run3$mean.metabolism[i-1], i, run3$mean.metabolism[i], col = "red", lwd=2)
    segments(i-1, run4$mean.metabolism[i-1], i, run4$mean.metabolism[i], col = "orange", lwd=2)
    segments(i-1, run5$mean.metabolism[i-1], i, run5$mean.metabolism[i], col = "purple", lwd=2)
  }
}
legend(x = 1, y = 1.8, legend = c("Default","High Capacity", "High Metabolism", "High Vision", "Low Vision"), 
       fill = c("green","blue","red","orange","purple"))

for(i in 1:length(run1$Gini)){
  if(i == 1){
    plot(-100, -100, xlim=c(1,30), ylim=c(0,1), ylab="Gini Coefficient", xlab="Iteration", type="n", cex.axis=0.8, main = "Wealth Inequality")
  }else{
    segments(i-1, run1$Gini[i-1], i, run1$Gini[i], col = "green", lwd=2)
    segments(i-1, run2$Gini[i-1], i, run2$Gini[i], col = "blue", lwd=2)
    segments(i-1, run3$Gini[i-1], i, run3$Gini[i], col = "red", lwd=2)
    segments(i-1, run4$Gini[i-1], i, run4$Gini[i], col = "orange", lwd=2)
    segments(i-1, run5$Gini[i-1], i, run5$Gini[i], col = "purple", lwd=2)
  }
}
legend(x = 1, y = .8, legend = c("Default","High Capacity", "High Metabolism", "High Vision", "Low Vision"), 
       fill = c("green","blue","red","orange","purple"))

