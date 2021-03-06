# Sugarscape replication (from Epstein and Axtell 1996)
# Initiates world of x by x dimensions populated by agents with density d.
# Each cell in the world has a sugar capacity c. 
  # Sugar is organized such that more sugar is in the center of the scape; the corners are barren.
# Each agent has a vision between 1 and maxviz, a metabolism within a set range, an age, a sex, and an endowment of sugar.
# In each round, each cell grows sugar at rate grate unless it is at capacity.
# Agents look around, determine which available cell in their field of vision has the most sugar, move there, and harvest the sugar
# Sugar stores are then decremented by the agent's metabolic rate
# If sugar stores fall to at or below zero, agent dies
# At end of each round, surviving agents look in their field of vision for members of the opposite sex who are of childbearing health and age
# If they match with another agent who considers them eligible, they have a child with characteristics averaged between them and an endowment of half of each parent's sugar
# Children populate to an empty cell in either parent's field of vision 
# If an agent reaches old age (defined flexibly in the function call) they have an Old.Age/Agent.Age chance of dying
  # Agents who die of old age have their sugar either distributed to their children or taxed by the government, depending on the estate rule defined in the function call

# Function tracks total number of agents, number of agents born, mean vision, mean metabolism and Gini coefficient by round.
# Function tracks which agents have starved to death and which have died.

# The first function initiates world of x by x dimensions, populated by agnets with desity d. 
# Each cell in the world has a sugar capacity c and initially has c amount of sugar. 
# Each agent has an id, vision ranging from 1 to maxviz, a metabolism specified in the function call, an age sampled from 1 to old.age, and a store of sugar s
# When world is initialized, agent harvests the sugar in their initial cell

do.sugarscape.gens <- function(dim = 50, popdens = .25, capacity = 4, grate = 1, old.age = 60,
                               maxviz = 6, r_endow = 5:25, metabolism = 1:4, gender = c(0,1),
                               characteristics = c("id","vision","sugar","metabolism","age","gender"), estate.rule = "tax"){
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
  
  er <<- estate.rule
  if(estate.rule == "tax"){
    grev <<- 0 # If the government will be taxing estates, set a counter for government revenues
  }
  
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
        agents[i,j,5] <<- sample(1:old.age, 1)
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
                       parent.id1 = rep(0, prod(dim*dim)),
                       parent.id2 = rep(0, prod(dim*dim)),
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
iterate.gens <- function(iterations, store.plots = FALSE, estate.rule = er){
  require(data.table)
  require(reldist)
  n_agents <- nrow(scape[agid > 0]) # Store initial number of agents
  mean_vision <- mean(scape$agviz, na.rm = T) # Store initial mean population vision
  mean_metab <- mean(scape$agmetab, na.rm = T) # Store initial mean population metabolism
  gcoef <- gini(scape$agsugar[!is.na(scape$agsugar)]) # Store initial Gini coefficient
  starved <- c(NULL)
  aged <- c(NULL)
  born <- c(NULL)
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
      for(o in 1:length(old.ids)){
        # Old agents have Old.Age/Agent.Age chance of dying of old age
        if(runif(1) > (old.age/scape[agid == old.ids[o], agage])){
          aged <- c(aged, old.ids[o]) # Update vector of agents who have died of old age
          
          if(estate.rule == "inheritance"){
            kids <- scape[parent.id1 == old.ids[o] | parent.id2 == old.ids[o], agid] # Set vector of old agent's children
            if(length(kids)>0){
              # If they had kids, divide old agent's sugar evenly amongst kids, dropping remainder
              will <- floor(scape[agid == old.ids[o], agsugar]/length(kids))
              
              # Add will amount to kids' sugar stores
              scape$agsugar <<- sapply(1:nrow(scape), function(x){
                if(scape$agid[x] %in% kids){
                  scape$agsugar[x] + will
                }else{scape$agsugar[x]}
              }
              )
            }
          }
          if(estate.rule == "tax"){
            grev <<- grev + scape[agid == old.ids[o], agsugar] # Government collects revenue to be distributed later
          }
          scape[agid == old.ids[o], 7:ncol(scape)] <<- NA  # Clear scape of agents who have died of old age
        }
      }
    }
    
    if(estate.rule == "tax"){
      ubi <- floor(grev/length(scape[!is.na(agid), agid])) # Divide collected revenues evenly between agents (not including remainder)
      grev <<- grev - ubi*length(scape[!is.na(agid), agid]) # Subtract distribution from government revenues
      scape$agsugar <<- sapply(1:nrow(scape), function(x){
        if(!is.na(scape$agid[x])){
          scape$agsugar[x] + ubi # Allocate ubi to each agent
        }else{scape$agsugar[x]}
      })
    }
    
    # Subfunction to find potential mates
    
    find.mates <- function(x_value, y_value, maxviz, dimension){
      gender <- scape[x == x_value & y == y_value, aggender] # Store agent's gender
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
      
      # Return eligible agents of the opposite sex the agent can see
      # Eligible: Opposite sex, half of sugar stores > double metabolism, age between 18 and 50
      pot.mates <- c(scape[x == x_value & y %in% y_vals & 
                             aggender != gender & !is.na(agid) & 
                             (agsugar/2) > (agmetab*2) & agage %in% c(18:50), agid], 
                     c(scape[x %in% x_vals & y == y_value & 
                               aggender != gender & !is.na(agid) & 
                               (agsugar/2) > (agmetab*2) & agage %in% c(18:50), agid]))
      if(length(pot.mates) > 0){
        return(pot.mates)
      }else{return(NA)}
    }
    cur_agents <- scape[!is.na(agid)]
    ag_match <- cur_agents$agid
    crushes <- list(NULL)
    # Set list: each index of list is agent in ag_match; elements of that index are that agent's crushes 
    crushes <- sapply(1:nrow(cur_agents), function(c){
      crushes[[c]] <- find.mates(x_value = scape[agid == ag_match[c],x], y_value = scape[agid == ag_match[c],y], 
                                 maxviz = scape[agid == ag_match[c],agviz], dimension = max(scape$x))
    })
    
    # Set list corresponding to crushes that returns whether an agent's crushes like them back
    matches <- list(NULL)
    matches <- sapply(1:length(crushes), function(m){
      if(is.na(crushes[[m]][1])){
        matches[[m]] <- NA
      }else{
        is.match <- c(NULL)
        for(i in 1:length(crushes[[m]])){
          is.match[i] <- ag_match[m] %in% crushes[[which(ag_match == crushes[[m]][i])]]
        }
        if(length(crushes[[m]][which(is.match)]) > 0){
          matches[[m]] <- crushes[[m]][which(is.match)]
        }else{matches[[m]] <- NA}
      }
    }
    )
    
    # Add new column to current agents to fill with matches
    cur_agents[,match.id := rep(NA, nrow(cur_agents))]
    for(a in 1:nrow(cur_agents)){
      mate.ids <- matches[[a]] # ids of agents that matched
      mate.ids <- mate.ids[! mate.ids %in% cur_agents[,match.id]] # remove any agents who are already matched
      
      if(length(mate.ids) == 0){
        cur_agents$match.id[a] <- NA
      }
      else{
        cur_agents$match.id[a] <- mate.ids[1] # Partner with first remaining match
      }
    }
    
    n_babies <- length(cur_agents[!is.na(match.id), agid])/2 # Set number of children that will be conceived
    if(n_babies > 0){
      
      # Set new data table to populate with children
      babies <- data.table(cellid = rep(NA, n_babies),
                           x = rep(NA, n_babies),
                           y = rep(NA, n_babies),
                           agid = (max.agid+1):(max.agid + n_babies),
                           parent.id1 = rep(NA, n_babies),
                           parent.id2 = rep(NA, n_babies),
                           agviz = rep(NA, n_babies),
                           agmetab = rep(NA, n_babies),
                           agsugar = rep(NA, n_babies),
                           agage = rep(0, n_babies),
                           aggender = sample(0:1, n_babies, replace = T))
      
      parents <- cur_agents[!is.na(match.id), agid] # Set vector of parents
      
      # Populate babies data.table with characteristics and subtract the sugar endowment each parent gives them
      for(p in 1:nrow(babies)){
        babies$parent.id1[p] <- parents[1]
        babies$parent.id2[p] <- cur_agents[agid == parents[1], match.id]
        babies$agviz[p] <- round((cur_agents[agid == babies$parent.id1[p], agviz] + 
                                    cur_agents[agid == babies$parent.id2[p], agviz])/2)
        babies$agmetab[p] <- round((cur_agents[agid == babies$parent.id1[p], agmetab] + 
                                      cur_agents[agid == babies$parent.id2[p], agmetab])/2)
        s1 <- round(cur_agents[agid == babies$parent.id1[p], agsugar]/2)
        s2 <- round(cur_agents[agid == babies$parent.id2[p], agsugar]/2)
        babies$agsugar[p] <- s1+s2 # Baby gets endowment of rounded half of each parent's current sugar
        scape$agsugar <<- with(scape, ifelse(agid == babies$parent.id1[p], agsugar - s1, ifelse(
          agid == babies$parent.id2[p], agsugar - s2,
          agsugar))) # decrement agent sugar by amount endowed to child
        parents <- parents[! parents %in% babies[,c(parent.id1, parent.id2)]] 
      }
      
      # Subfunction to pick empty cell within either parent's field of vision for baby to populate to
      pickcell.baby <- function(pid1, pid2, dimension){
        p1x <- scape[agid == pid1, x]
        p1y <- scape[agid == pid1, y]
        p1viz <- scape[agid == pid1, agviz]
        p2x <- scape[agid == pid2, x]
        p2y <- scape[agid == pid2, y]
        p2viz <- scape[agid == pid2, agviz]
        
        x_vals <- c((p1x+1):(p1x+p1viz), (p1x-1):(p1x-p1viz), (p2x+1):(p2x+p2viz), (p2x-1):(p2x-p2viz))
        x_vals <- sapply(x_vals, function(x){
          if(x < 1){x = 1} 
          if(x > dimension){x = dimension}
          else{x}})
        
        y_vals <- c((p1y+1):(p1y+p1viz), (p1y-1):(p1y-p1viz), (p2y+1):(p2y+p2viz), (p2y-1):(p2y-p2viz))
        y_vals <- sapply(y_vals, function(x){
          if(x < 1){x = 1}
          if(x > dimension){x = dimension}
          else{x}})
        
        see.ids <- c(scape[x %in% x_vals & y %in% y_vals & is.na(agid), cellid], scape[x %in% x_vals & y %in% y_vals & is.na(agid), cellid]) # Empty cells agents can see
        see.ids <- see.ids[! see.ids %in% babies[,cellid]] # Minus cells other babies have already been placed in
        
        # Find the cell(s) with the most sugar within the available cells the agent sees
        if(length(see.ids) > 0){
          babycell <- scape[cellid %in% see.ids & is.na(agid) & cellsugar == max(scape[cellid %in% see.ids, cellsugar]), cellid]
        }else{
          return(babies[parent.id1 == pid1] <- NA) #If there is nowhere for the baby to live, it dies
        }
        
        if(length(babycell) > 1){
          return(babycell[1]) # If multiple available cells with max sugar, return first one
        }
        if(length(babycell) == 0){
          return(babies[parent.id1 == pid1] <- NA) #If there is nowhere for the baby to live, it dies
        }
        else{
          return(babycell)
        }
      }
      # Pick cell and populate scape data.table with new agent
      for(b in 1:nrow(babies)){
        babies$cellid[b] <- pickcell.baby(pid1 = babies$parent.id1[b], pid2 = babies$parent.id2[b], dimension = max(scape$x))
        scape[cellid == babies$cellid[b], 7:ncol(scape)] <<- babies[cellid == babies$cellid[b], 4:ncol(babies)]
      }
      born <- c(born, nrow(babies[!is.na(babies$cellid)])) # Update vector of babies born in each round
      max.agid <<- max(scape$agid, na.rm = T) # Update max agent ID
    }
    else{born <- c(born, 0)}
    
    n_agents <- c(n_agents, nrow(scape[agid > 0])) # Store new number of agents
    mean_vision <- c(mean_vision, mean(scape$agviz, na.rm = T)) # Store new mean population vision
    mean_metab <- c(mean_metab, mean(scape$agmetab, na.rm = T)) # Store new mean population metabolism
    gcoef <- c(gcoef, gini(scape$agsugar[!is.na(scape$agsugar)])) # Store new gini coefficient
    if(store.plots == TRUE){
      viz[[i+1]] <<- plotScape(title = paste("Sugarscape after round ", i, sep = "")) # Store plot for each iteration
    }
  }
  # Store vectors of characteristics in a list and return them
  returns <- list(n_agents, born, mean_vision, mean_metab, gcoef, starved, aged)
  names(returns) <- c("agents", "births", "mean.vision", "mean.metabolism", "Gini", "starved", "aged")
  return(returns)
}

# Run 50 iterations under each estate rule, store scape plots and visualize the results
do.sugarscape.gens(estate.rule = "tax")
run1tax <- iterate.gens(iterations = 200, store.plots = T)
viz1tax <- viz

do.sugarscape.gens(estate.rule = "inheritance")
run1inherit <- iterate.gens(iterations = 200, store.plots = T)
viz1inherit <- viz

do.sugarscape.gens(metabolism = 2:4, estate.rule = "tax")
run2tax <- iterate.gens(iterations = 200, store.plots = T)
viz2tax <- viz

do.sugarscape.gens(metabolism = 2:4, estate.rule = "inheritance")
run2inherit <- iterate.gens(iterations = 200, store.plots = T)
viz2inherit <- viz

setwd("~/Desktop/ABM/Sugarscape/Taxation")
for(i in 1:length(viz2tax)){
  dev.copy(png, paste("taxation2_frame",i,".png", sep=""))
  print(viz2tax[[i]])
  dev.off()
}

setwd("~/Desktop/ABM/Sugarscape/Inheritance")
for(i in 1:length(viz2inherit)){
  dev.copy(png, paste("inheritance2_frame",i,".png", sep=""))
  print(viz2inherit[[i]])
  dev.off()
}

# Carrying Capacity
for(i in 1:length(run2tax$agents)){
  if(i == 1){
    plot(-100, -100, xlim=c(1,200), ylim=c(0,1000), ylab="Agents", xlab="Iteration", type="n", cex.axis=0.8, main = "Carrying Capacity")
  }else{
    segments(i-1, run2tax$agents[i-1], i, run2tax$agents[i], col = "blue", lwd=2)
    segments(i-1, run2inherit$agents[i-1], i, run2inherit$agents[i], col = "red", lwd=2)
  }
}
legend(x = 100, y = 200, legend = c("100% Estate Tax","100% Inheritance"), fill = c("blue","red"))


# Births by round
for(i in 1:length(run2tax$births)){
  if(i == 1){
    plot(-100, -100, xlim=c(1,200), ylim=c(0,50), ylab="Births", xlab="Iteration", type="n", cex.axis=0.8, main = "Births by Round")
  }else{
    segments(i-1, run2tax$births[i-1], i, run2tax$births[i], col = "blue", lwd=2)
    segments(i-1, run2inherit$births[i-1], i, run2inherit$births[i], col = "red", lwd=2)  
  }
}
legend(x = 100, y = 40, legend = c("100% Estate Tax","100% Inheritance"), fill = c("blue","red"))

# Mean vision
for(i in 1:length(run2tax$mean.vision)){
  if(i == 1){
    plot(-100, -100, xlim=c(1,200), ylim=c(1,6), ylab="Mean Vision", xlab="Iteration", type="n", cex.axis=0.8, main = "Selection for Vision")
  }else{
    segments(i-1, run2tax$mean.vision[i-1], i, run2tax$mean.vision[i], col = "blue", lwd=2)
    segments(i-1, run2inherit$mean.vision[i-1], i, run2inherit$mean.vision[i], col = "red", lwd=2)
  }
}
legend(x = 1, y = 2, legend = c("100% Estate Tax","100% Inheritance"), fill = c("blue","red"))

# Mean metabolism
for(i in 1:length(run2tax$mean.metabolism)){
  if(i == 1){
    plot(-100, -100, xlim=c(1,200), ylim=c(0,5), ylab="Mean Metabolism", xlab="Iteration", type="n", cex.axis=0.8, main = "Selection for Metabolism")
  }else{
    segments(i-1, run2tax$mean.metabolism[i-1], i, run2tax$mean.metabolism[i], col = "blue", lwd=2)
    segments(i-1, run2inherit$mean.metabolism[i-1], i, run2inherit$mean.metabolism[i], col = "red", lwd=2)
  }
}
legend(x = 1, y = 1, legend = c("100% Estate Tax","100% Inheritance"), fill = c("blue","red"))

for(i in 1:length(run2tax$Gini)){
  if(i == 1){
    plot(-100, -100, xlim=c(1,200), ylim=c(0,1), ylab="Gini Coefficient", xlab="Iteration", type="n", cex.axis=0.8, main = "Wealth Inequality")
  }else{
    segments(i-1, run2tax$Gini[i-1], i, run2tax$Gini[i], col = "blue", lwd=2)
    segments(i-1, run2inherit$Gini[i-1], i, run2inherit$Gini[i], col = "red", lwd=2)
  }
}
legend(x = 1, y = .2, legend = c("100% Estate Tax","100% Inheritance"), fill = c("blue","red"))


# Vision / Metabolism
for(i in 1:length(run2tax$mean.metabolism)){
  if(i == 1){
    plot(-100, -100, xlim=c(1,200), ylim=c(0,4.5), ylab="Value", xlab="Iteration", type="n", cex.axis=0.8, main = "Vision / Metabolism")
  }else{
    segments(i-1, run2tax$mean.metabolism[i-1], i, run2tax$mean.metabolism[i], col = "darkblue", lwd=2)
    segments(i-1, run2tax$mean.vision[i-1], i, run2tax$mean.vision[i], col = "lightblue", lwd=2)
    segments(i-1, run2inherit$mean.metabolism[i-1], i, run2inherit$mean.metabolism[i], col = "darkred", lwd=2)
    segments(i-1, run2inherit$mean.vision[i-1], i, run2inherit$mean.vision[i], col = "pink", lwd=2)
  }
}
legend(x = 1, y = 1.5, legend = c("Tax Metabolism","Tax Vision","Inherit Metabolism","Inherit Vision"), 
       fill = c("darkblue","lightblue","darkred","pink"))
