iterate <- function(iterations, capacity = 4, grate = 1, metabolism = 1){
  for(i in iterations){
    scape$cellsugar <- scape$cellsugar+1
    scape$cellsugar[scape$cellsugar >= capacity] <- capacity
    
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
    for(i in 1:nrow(transition)){
      transition$target[i] <- pickcell(x_value = transition$x[i], y_value = transition$y[i], 
                                       maxviz = transition$agviz[i], dimension = max(scape$x))
    }
    
    # move agents to the new cells
    for(i in 1:nrow(scape)){
      if(scape[i,1] %in% transition[,target]){
        scape[i,6:8] <- transition[target == i, 4:6]
      }
    }
    scape[cellid %in% transition$cellid, 6:8] <- NA
    
    # Harvest sugar
    for(i in 1:nrow(scape)){
      if(!is.na(scape$agid[i])){
        s <- scape$cellsugar[i]
        scape$agsugar[i] <- scape$agsugar[i] + s
        scape$cellsugar[i] <- scape$cellsugar[i] - s
      }
    }
    scape$agsugar <- with(scape, ifelse(!is.na(agid), agsugar - 1, agsugar)) # decrement agent sugar by metabolic rate
  }
}
