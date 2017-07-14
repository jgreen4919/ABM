# Deliberation ABM Replication (based on Lustick and Miodownick 2000)

do.delibspace <- function(dimension = 50, olead.dens = .1, baserep = 3, args = 10, positions = c("for","against")){
  require(msm)
  dim <<- dimension
  
  # Sets data table of agents
  agents <- data.table(agid = 1:(dimension*dimension),
                       x = rep(1:dimension, dimension),
                       y = rep(1:dimension, each = dimension),
                       position = sample(positions, dimension*dimension, replace = T),
                       olead = rep(NA, dimension*dimension),
                       dqual = rep(NA, dimension*dimension),
                       p.rep = rep(NA, dimension*dimension),
                       o.rep = rep(NA, dimension*dimension),
                       p.conf = rep(NA, dimension*dimension)
  )
  
  argspace <- list(NULL)
  # Each agent is assigned an argumentative space, indicating their latent receptiveness to each reason for each position
  # Agents are more receptive to reasons for their position
  argspace <- lapply(1:(dim*dim), function(x){
    argspace[[x]] <- data.table(
      position = rep(c("for","against"), each = args),
      reason = rep(1:args, length(positions)),
      if(agents$position[x] == "for"){
      lreceptive = c(rtnorm(10, mean = .7, sd = .2, lower = 0, upper = 1), rtnorm(10, mean = .3, sd = .2, lower = 0, upper = 1))
      }else{
        lreceptive = c(rtnorm(10, mean = .3, sd = .2, lower = 0, upper = 1), rtnorm(10, mean = .7, sd = .2, lower = 0, upper = 1))
      }
    )
  }
  )
  
  # Distribute opinion leaders
  agents$olead <- sapply(agents$olead, function(x){
    x <- runif(1) < olead.dens # Agent has olead.dens chance of being an opinion leader
  })
  
  # Assign each agent a deliberative quality
  agents$dqual <- sapply(1:nrow(agents), function(x){
    if(agents$olead[x] == TRUE){
      agents$dqual[x] <- rtnorm(1, mean = .8, sd = .1, lower = .5, upper = 1) # Opinion leaders are more skilled deliberators
    }else{
      agents$dqual[x] <- rtnorm(1, mean = .5, sd = .3, lower = 0, upper = .8)
    }
  })
  
  # Give each agent an initial argument repertoire for their position
  agents$p.rep <- sapply(1:nrow(agents), function(x){
    if(agents$olead[x] == TRUE){
      sample(argspace[[x]][argspace[[x]]$position == agents$position[x]]$reason, baserep*2) # Opinion leaders have larger argument repertoires
    }else{
      sample(argspace[[x]][argspace[[x]]$position == agents$position[x]]$reason, baserep)
    }
  })
  
  # Give each agent an initial argument repertoire against their position
  agents$o.rep <- sapply(1:nrow(agents), function(x){
    if(agents$olead[x] == TRUE){
      sample(argspace[[x]][argspace[[x]]$position != agents$position[x]]$reason, baserep) # Opinion leaders have larger argument repertoires 
    }else{
      sample(argspace[[x]][argspace[[x]]$position != agents$position[x]]$reason, 1)
      }
  })
  
  # Give each agent an initial confidence in their position
  # p.conf = sum of receptiveness to arguments in for-repertoire minus sum of receptiveness to arguments in against-repertoire
  agents$p.conf <- sapply(1:nrow(agents), function(x){
    agents$p.conf[x] <- sum(argspace[[x]][,3][argspace[[x]]$position == agents$position[x]][agents$p.rep[x][[1]]]) -
      sum(argspace[[x]][,3][argspace[[x]]$position != agents$position[x]][agents$o.rep[x][[1]]])
  })
  returns <- list(agents, argspace)
  return(returns)
}
test <- do.delibspace()
agents <- test[[1]]
argspace <- test[[2]]

deliberate <- function(iterations){
  for(i in iterations){
    a <- sample(agents$agid, 1) # Pick an agent to deliberate
    
    # Subfunction for agents to pick a deliberative partner (wrapped world)
    pickpartner <- function(x_value, y_value, dimension){
      x_vals <- c(x_value+1, x_value-1) # Look left and right
      x_vals <- sapply(x_vals, function(x){
        if(x < 1){x = dimension} 
        if(x > dimension){x = 1}
        else{x}})
      
      y_vals <- c(y_value+1, y_value-1) # Look up and down
      y_vals <- sapply(y_vals, function(x){
        if(x < 1){x = dimension}
        if(x > dimension){x = 1}
        else{x}})
      
      see.ids <- c(agents[x %in% x_vals & y %in% y_vals, agid], agents[x %in% x_vals & y %in% y_vals, agid]) # Return agent IDs in Moore Neighborhood
      return(sample(see.ids, 1)) # Return one agent from Moore neighborhood
    }
    
    p <- pickpartner(x_value = agents[agid == a, x], y_value = agents[agid == a, y], dimension = dim)
    
    arg.a <- c(agents[agid == a, position], sample(agents[agid == a, p.rep[[1]]], 1)) # Agent states position and gives one reason from repertoire
    arg.p <- c(agents[agid == p, position], sample(agents[agid == p, p.rep[[1]]], 1)) # Partner states position and gives one reason from repertoire
    
    a.force <- agents[agid == a, dqual] * argspace[[p]][argspace[[p]]$position == arg.a[1] & argspace[[p]]$reason == arg.a[2]][,3]
    p.force <- agents[agid == p, dqual] * argspace[[a]][argspace[[a]]$position == arg.p[1] & argspace[[a]]$reason == arg.p[2]][,3]
    
    # If agents agree on position, they reinforce their reasons
    if(arg.a[1] == arg.p[1]){
      
    }
    # If agents disagree, they either move their interlocutor closer or push them away depending on the force of their argument
    if(arg.a[1] != arg.p[1]){
      
    }

    if(argqual.a > argqual.p){
      agents[agid == p, o.rep[[1]]] <<- c(agents[agid == p, o.rep[[1]]], arg.a)
      agents[agid == a, dqual] <<- agents[agid == a, dqual] + .1
      if(agents[agid == a, dqual] > 1){agents[agid == a, dqual] <<- 1}
    }
    if(argqual.a < argqual.p){
      agents[agid == a, o.rep[[1]]] <<- c(agents[agid == a, o.rep[[1]]], arg.p)
      agents[agid == p, dqual] <<- agents[agid == p, dqual] + .1
      if(agents[agid == p, dqual] > 1){agents[agid == p, dqual] <<- 1}
    }
}

