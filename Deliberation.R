# Deliberation ABM Replication (based on Lustick and Miodownick 2000)

# Function to initialize the deliberative space. 
# Takes dimension, opinion leader density, baseline repertoire size, number of arguments per position, and positions
# Creates a list of argumentative spaces for each agent, with latent receptivity to each reason for and against
  # Agents are more receptive to arguments for their position
# Agents are assigned deliberative quality (opinion leaders are more likely to have higher deliberative skill)
# Agents are assigned argument repertoires for and against their position, sampled from their deliberative space
# Agents' confidence in their position is:
  # sum of receptivity to the arguments in their for-repertoire, minus sum of receptivity to arguments in their against-repertoire
do.delibspace <- function(dimension = 20, olead.dens = .1, base.dprop = .3, lead.dprop = .7, 
                          base.dqual = .5, lead.dqual = .8,
                          baserep = 3, args = 10, positions = c("for","against")){
  if((baserep*2) > args){
    return("Error: baseline argument repertoire too large")
  }
  require(data.table)
  require(msm)

  # Sets data table of agents
  agents <- data.table(agid = 1:(dimension*dimension),
                       x = rep(1:dimension, dimension),
                       y = rep(1:dimension, each = dimension),
                       position = sample(positions, dimension*dimension, replace = T),
                       olead = rep(NA, dimension*dimension),
                       dqual = rep(NA, dimension*dimension),
                       dprop = rep(NA, dimension*dimension),
                       p.rep = rep(NA, dimension*dimension),
                       p.repsize = rep(NA, dimension*dimension),
                       o.rep = rep(NA, dimension*dimension),
                       o.repsize = rep(NA, dimension*dimension),
                       p.conf = rep(NA, dimension*dimension),
                       cdelib = rep(NA, dimension*dimension),
                       dpart = rep(NA, dimension*dimension)
  )
  agents$position <- as.factor(agents$position)
  
  argspace <- list(NULL)
  # Each agent is assigned an argumentative space, indicating their latent receptiveness to each reason for each position
  argspace <- lapply(1:(dimension*dimension), function(x){
    argspace[[x]] <- data.table(
      position = rep(c("for","against"), each = args),
      reason = rep(1:args, length(positions)),
      if(agents$position[x] == "for"){
        # Agents are more receptive to reasons for their position
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
      agents$dqual[x] <- rtnorm(1, mean = lead.dqual, sd = .1, lower = .5, upper = 1) # Opinion leaders are more skilled deliberators on average by default, but not by definition
    }else{
      agents$dqual[x] <- rtnorm(1, mean = base.dqual, sd = .3, lower = 0, upper = .8)
    }
  })
  
  # Assign each agent a propensity to deliberate
  agents$dprop <- sapply(1:nrow(agents), function(x){
    if(agents$olead[x] == TRUE){
      agents$dprop[x] <- rtnorm(1, mean = lead.dprop, sd = .1, lower = .5, upper = 1) # Opinion leaders are more likely to deliberate (unless otherwise specified, but see ranges)
    }else{
      agents$dprop[x] <- rtnorm(1, mean = base.dprop, sd = .2, lower = 0, upper = 1)
    }
  })
  
  # Give each agent an initial argument repertoire for their position and store number of arguments
  agents$p.rep <- sapply(1:nrow(agents), function(x){
    if(agents$olead[x] == TRUE){
      sample(argspace[[x]][argspace[[x]]$position == agents$position[x]]$reason, baserep*2) # Opinion leaders have twice as large for-repertoires
    }else{
      sample(argspace[[x]][argspace[[x]]$position == agents$position[x]]$reason, baserep)
    }
  })
  
  agents$p.repsize <- sapply(agents$p.rep, function(x){
    length(x)
  })
  
  # Give each agent an initial argument repertoire against their position and store number of arguments
  agents$o.rep <- sapply(1:nrow(agents), function(x){
    if(agents$olead[x] == TRUE){
      sample(argspace[[x]][argspace[[x]]$position != agents$position[x]]$reason, baserep) # Opinion leaders have larger against-repertoires 
    }else{
      sample(argspace[[x]][argspace[[x]]$position != agents$position[x]]$reason, 1)
      }
  })
  
  agents$o.repsize <- sapply(agents$o.rep, function(x){
    length(x)
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

# Function to plot the deliberative space
plotDelib <- function(title = "Deliberative Space", dat = agents, view = "position"){
  require(data.table)
  require(ggplot2)
  require(RColorBrewer)
  
  # find the dimensions of the grid to get the best dot size
  dims <- c(max(dat$x), max(dat$y))
  
  if(view == "position"){
  # plot each agent's position
  p <- ggplot() + 
    # resize dots to grid
    geom_point(data = dat, 
               aes(x = x, y = y, color = position), 
               size = 100/sqrt(prod(dims)))
  }
  
  if(view == "dqual"){
    p <- ggplot() + 
      # resize dots to grid
      geom_point(data = dat, 
                 aes(x = x, y = y, color = dqual), 
                 size = 100/sqrt(prod(dims)))+
      scale_colour_gradient(low = "white", high = "blue", limits = c(0,1))
  }
  
  if(view == "dprop"){
    p <- ggplot() + 
      # resize dots to grid
      geom_point(data = dat, 
                 aes(x = x, y = y, color = dprop), 
                 size = 100/sqrt(prod(dims)))+
      scale_colour_gradient(low = "white", high = "blue", limits = c(0,1))
  }
  
  if(view == "p.conf"){
    p <- ggplot() + 
      # resize dots to grid
      geom_point(data = dat, 
                 aes(x = x, y = y, color = p.conf), 
                 size = 100/sqrt(prod(dims)))+
      scale_colour_gradient(low = "white", high = "blue", limits = c(0,max(dat$p.conf)))
  }
  if(view == "p.repsize"){
    p <- ggplot() + 
      # resize dots to grid
      geom_point(data = dat, 
                 aes(x = x, y = y, color = p.repsize, size = p.conf))+
      scale_colour_gradient(low = "white", high = "blue", limits = c(0,10))
  }
  if(view == "o.repsize"){
    p <- ggplot() + 
      # resize dots to grid
      geom_point(data = dat, 
                 aes(x = x, y = y, color = o.repsize), 
                 size = 100/sqrt(prod(dims)))+
      scale_colour_gradient(low = "white", high = "blue", limits = c(0,10))
  }
  
    # theme: mostly blank
  p <- p+
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

# Function to govern interaction rules between agents
# In each round, agents choose whether to deliberate with probability dprop
# Agents match with partner in their Moore neighborhood who also wants to deliberate
# Each agent makes an argument, represented by the combination of their position and one reason sampled from their repertoire
  # Their argument's force is determined by their deliberative quality and their partner's receptivity to their stated reason
# If the agents agree:
  # If an argument is powerful:
    # The speaker's propensity to deliberate, deliberative quality, and receptivity to the reason given increase
    # The argument is added to the listener's for-repertoire and their receptivity to the reason given increases
  # If an argument is not powerful:
    # Nothing happens
# If the agents disagree
  # If an argument is powerful:
    # The speaker's propensity to deliberate, deliberative quality, and receptivity to the reason given increase
    # The argument is added to the listener's against-repertoire and their receptivity to the reason given increases
  # If an argument is not powerful:
    # The speaker's propensity to deliberate decreases
    # The listener's receptivity to the reason given decreases, and their receptivity to the reason they gave increases
# At the end of each round, everyone updates their position confidence
  # Agents switch their position if position confidence falls below zero
deliberate <- function(iterations){
  require(data.table)
  for(i in iterations){
    agents$cdelib <<- sapply(1:nrow(agents), function(x){
      agents$dprop[x] > runif(1) # Each agent chooses whether to deliberate in this round
    })

    # Subfunction for agents to pick a deliberative partner (wrapped world)
    pickpartner <- function(x_value, y_value, dimension){
      cur_ag <- agents[x == x_value & y == y_value, agid] # Store current agent ID
      if(cur_ag %in% agents$dpart){
        return(agents[agents$dpart == cur_ag, agid]) # If agent has already been claimed as a deliberative partner, match them with the agent who claimed them
      }else{
      x_vals <- c(x_value+1, x_value, x_value-1) # Look left and right
      x_vals <- sapply(x_vals, function(x){
        if(x < 1){x = dimension} 
        if(x > dimension){x = 1}
        else{x}})
      
      y_vals <- c(y_value+1, y_value, y_value-1) # Look up and down
      y_vals <- sapply(y_vals, function(x){
        if(x < 1){x = dimension}
        if(x > dimension){x = 1}
        else{x}})
      
      # Return agent IDs in Moore Neighborhood who want to deliberate but don't already have a partner
      delib.ids <- agents[x %in% x_vals & y %in% y_vals & cdelib == TRUE & agid != cur_ag, agid] # Deliberating agents in Moore neighborhood
      delib.ids <- delib.ids[! delib.ids %in% agents$dpart] # Subtract agents who are already in list of partners
      delib.ids <- delib.ids[! delib.ids %in% agents$agid[!is.na(agents$dpart)]] # Subtract agents who already have partners
      if(length(delib.ids) == 0){
        return(NA)
      }
      if(length(delib.ids) == 1){
        return(delib.ids)
      }else{
      return(sample(delib.ids, 1)) # Return one agent from Moore neighborhood who isn't already partnered up
      }
      }
    }
    agents$dpart <<- NA
    for(p in 1:nrow(agents)){
      if(agents$cdelib[p] == FALSE) {next}
      # Agents who want to deliberate partner up
        agents$dpart[p] <<- pickpartner(x_value = agents[agid == p, x], y_value = agents[agid == p, y], dimension = max(agents$x))
    } 

    delibs <- agents$agid[!is.na(agents$dpart)] # Vector of agents who are deliberating this round
    
    delibspace <- subset(agents, agid %in% delibs, select = c(1:5)) # Store sub-data table to track reasons and argument forces by round (might use later)
    delibspace$reas <- NA
    delibspace$force <- NA

    for(i in 1:(length(delibs)/2)){
      # Deliberation between first agent in delibs vector and their partner
      a <- delibs[1]
      p <- agents[agid == delibs[1], dpart]
      
      # Agents each state position and reason, sampled from their argument repertoire
      a.pos <- agents[agid == a, position]
      a.reas <- sample(agents[agid == a, p.rep[[1]]], 1)
      p.pos <- agents[agid == p, position]
      p.reas <- sample(agents[agid == p, p.rep[[1]]], 1)

      # Force of agent's argument = agent's deliberative quality * listener's receptivity to the reason they give
      a.force <- agents[agid == a, dqual] * argspace[[p]][argspace[[p]]$position == a.pos & argspace[[p]]$reason == a.reas][,3]
      p.force <- agents[agid == p, dqual] * argspace[[a]][argspace[[a]]$position == p.pos & argspace[[a]]$reason == p.reas][,3]
      
      # Update deliberative space with reasons and forces
      delibspace$reas[delibspace$agid == a] <- a.reas
      delibspace$reas[delibspace$agid == p] <- p.reas
      delibspace$force[delibspace$agid == a] <- a.force
      delibspace$force[delibspace$agid == p] <- p.force
      
      # If the agents agree:
      if(a.pos == p.pos){
        # If an argument is powerful (note: lower threshold for powerful arguments if agents agree):
        if(a.force > (agents[agid == p, dqual]/2)){
          # The speaker's propensity to deliberate, deliberative quality, and receptivity to the reason given increase by 10%, capped at 1
          agents$dprop[a] <<- agents$dprop[a] + .1*agents$dprop[a]
          if(agents$dprop[a] > 1){agents$dprop[a] <<- 1}
          
          agents$dqual[a] <<- agents$dqual[a] + .1*agents$dqual[a]
          if(agents$dqual[a] > 1){agents$dqual[a] <<- 1}
          
          argspace[[a]][argspace[[a]]$position == a.pos & argspace[[a]]$reason == a.reas][,3] <<- 
            argspace[[a]][argspace[[a]]$position == a.pos & argspace[[a]]$reason == a.reas][,3] + 
            .1*argspace[[a]][argspace[[a]]$position == a.pos & argspace[[a]]$reason == a.reas][,3]
          if(argspace[[a]][argspace[[a]]$position == a.pos & argspace[[a]]$reason == a.reas][,3] > 1){
            argspace[[a]][argspace[[a]]$position == a.pos & argspace[[a]]$reason == a.reas][,3] <<- 1
            }
          
          # The argument is added to the listener's for-repertoire and their receptivity to the reason given increases by 10%, capped at 1
          agents$p.rep[p][[1]] <<- unique(as.numeric(c(agents$p.rep[p][[1]], a.reas)))
          argspace[[p]][argspace[[p]]$position == a.pos & argspace[[p]]$reason == a.reas][,3] <<- 
            argspace[[p]][argspace[[p]]$position == a.pos & argspace[[p]]$reason == a.reas][,3] + 
            .1*argspace[[p]][argspace[[p]]$position == a.pos & argspace[[p]]$reason == a.reas][,3]
          if(argspace[[p]][argspace[[p]]$position == a.pos & argspace[[p]]$reason == a.reas][,3] > 1){
            argspace[[p]][argspace[[p]]$position == a.pos & argspace[[p]]$reason == a.reas][,3] <<- 1
          }
        }
        if(p.force > (agents[agid == a, dqual]/2)){
          # The speaker's propensity to deliberate, deliberative quality, and receptivity to the reason given increase by 10%, capped at 1
          agents$dprop[p] <<- agents$dprop[p] + .1*agents$dprop[p]
          if(agents$dprop[p] > 1){agents$dprop[p] <<- 1}
          
          agents$dqual[p] <<- agents$dqual[p] + .1*agents$dqual[p]
          if(agents$dqual[p] > 1){agents$dqual[p] <<- 1}
          
          argspace[[p]][argspace[[p]]$position == p.pos & argspace[[p]]$reason == p.reas][,3] <<- 
            argspace[[p]][argspace[[p]]$position == p.pos & argspace[[p]]$reason == p.reas][,3] + 
            .1*argspace[[p]][argspace[[p]]$position == p.pos & argspace[[p]]$reason == p.reas][,3]
          if(argspace[[p]][argspace[[p]]$position == p.pos & argspace[[p]]$reason == p.reas][,3] > 1){
            argspace[[p]][argspace[[p]]$position == p.pos & argspace[[p]]$reason == p.reas][,3] <<- 1
          }
          
          # The argument is added to the listener's for-repertoire and their receptivity to the reason given increases by 10%, capped at 1
          agents$p.rep[a][[1]] <<- unique(as.numeric(c(agents$p.rep[a][[1]], p.reas)))
          argspace[[a]][argspace[[a]]$position == p.pos & argspace[[a]]$reason == p.reas][,3] <<- 
            argspace[[a]][argspace[[a]]$position == p.pos & argspace[[a]]$reason == p.reas][,3] + 
            .1*argspace[[a]][argspace[[a]]$position == p.pos & argspace[[a]]$reason == p.reas][,3]
          if(argspace[[a]][argspace[[a]]$position == p.pos & argspace[[a]]$reason == p.reas][,3] > 1){
            argspace[[a]][argspace[[a]]$position == p.pos & argspace[[a]]$reason == p.reas][,3] <<- 1
          }
        }else{next} # If an agreeable argument is not powerful, nothing happens
      }
      # If the agents disagree:
      if(a.pos != p.pos){
        # If an argument is powerful:
        if(a.force > (agents[agid == p, dqual])){
          # The speaker's propensity to deliberate, deliberative quality, and receptivity to the reason given increase by 10%, capped at 1
          agents$dprop[a] <<- agents$dprop[a] + .1*agents$dprop[a]
          if(agents$dprop[a] > 1){agents$dprop[a] <<- 1}
          
          agents$dqual[a] <<- agents$dqual[a] + .1*agents$dqual[a]
          if(agents$dqual[a] > 1){agents$dqual[a] <<- 1}
          
          argspace[[a]][argspace[[a]]$position == a.pos & argspace[[a]]$reason == a.reas][,3] <<- 
            argspace[[a]][argspace[[a]]$position == a.pos & argspace[[a]]$reason == a.reas][,3] + 
            .1*argspace[[a]][argspace[[a]]$position == a.pos & argspace[[a]]$reason == a.reas][,3]
          if(argspace[[a]][argspace[[a]]$position == a.pos & argspace[[a]]$reason == a.reas][,3] > 1){
            argspace[[a]][argspace[[a]]$position == a.pos & argspace[[a]]$reason == a.reas][,3] <<- 1
          }
          # The argument is added to the listener's against-repertoire and their receptivity to the reason given increases by 10%, capped at 1
          agents$o.rep[p][[1]] <<- unique(as.numeric(c(agents$o.rep[p][[1]], a.reas)))
          argspace[[p]][argspace[[p]]$position == a.pos & argspace[[p]]$reason == a.reas][,3] <<- 
            argspace[[p]][argspace[[p]]$position == a.pos & argspace[[p]]$reason == a.reas][,3] + 
            .1*argspace[[p]][argspace[[p]]$position == a.pos & argspace[[p]]$reason == a.reas][,3]
          if(argspace[[p]][argspace[[p]]$position == a.pos & argspace[[p]]$reason == a.reas][,3] > 1){
            argspace[[p]][argspace[[p]]$position == a.pos & argspace[[p]]$reason == a.reas][,3] <<- 1
          }
        }
        if(p.force > (agents[agid == a, dqual])){
          # The speaker's propensity to deliberate, deliberative quality, and receptivity to the reason given increase by 10%, capped at 1
          agents$dprop[p] <<- agents$dprop[p] + .1*agents$dprop[p]
          if(agents$dprop[p] > 1){agents$dprop[p] <<- 1}
          
          agents$dqual[p] <<- agents$dqual[p] + .1*agents$dqual[p]
          if(agents$dqual[p] > 1){agents$dqual[p] <<- 1}
          
          argspace[[p]][argspace[[p]]$position == p.pos & argspace[[p]]$reason == p.reas][,3] <<- 
            argspace[[p]][argspace[[p]]$position == p.pos & argspace[[p]]$reason == p.reas][,3] + 
            .1*argspace[[p]][argspace[[p]]$position == p.pos & argspace[[p]]$reason == p.reas][,3]
          if(argspace[[p]][argspace[[p]]$position == p.pos & argspace[[p]]$reason == p.reas][,3] > 1){
            argspace[[p]][argspace[[p]]$position == p.pos & argspace[[p]]$reason == p.reas][,3] <<- 1
          }
          # The argument is added to the listener's against-repertoire and their receptivity to the reason given increases by 10%, capped at 1
          agents$o.rep[a][[1]] <<- unique(as.numeric(c(agents$o.rep[a][[1]], p.reas)))
          argspace[[a]][argspace[[a]]$position == p.pos & argspace[[a]]$reason == p.reas][,3] <<- 
            argspace[[a]][argspace[[a]]$position == p.pos & argspace[[a]]$reason == p.reas][,3] + 
            .1*argspace[[a]][argspace[[a]]$position == p.pos & argspace[[a]]$reason == p.reas][,3]
          if(argspace[[a]][argspace[[a]]$position == p.pos & argspace[[a]]$reason == p.reas][,3] > 1){
            argspace[[a]][argspace[[a]]$position == p.pos & argspace[[a]]$reason == p.reas][,3] <<- 1
          }
        }
        # If an argument is weak:
        if(a.force < (agents[agid == p, dqual]/2)){
          # The speaker's propensity to deliberate decreases by 10%, floored at 0
          agents$dprop[a] <<- agents$dprop[a] - .1*agents$dprop[a]
          if(agents$dprop[a] < 0){agents$dprop[a] <<- 0}
          
          # The listener's receptivity to the reason given decreases, and their receptivity to the reason they gave increases by 10% (floor of 0, cap of 1)
          argspace[[p]][argspace[[p]]$position == a.pos & argspace[[p]]$reason == a.reas][,3] <<- 
            argspace[[p]][argspace[[p]]$position == a.pos & argspace[[p]]$reason == a.reas][,3] - 
            .1*argspace[[p]][argspace[[p]]$position == a.pos & argspace[[p]]$reason == a.reas][,3]
          if(argspace[[p]][argspace[[p]]$position == a.pos & argspace[[p]]$reason == a.reas][,3] <0){
            argspace[[p]][argspace[[p]]$position == a.pos & argspace[[p]]$reason == a.reas][,3] <<- 0
          }
          argspace[[p]][argspace[[p]]$position == p.pos & argspace[[p]]$reason == p.reas][,3] <<- 
            argspace[[p]][argspace[[p]]$position == p.pos & argspace[[p]]$reason == p.reas][,3] + 
            .1*argspace[[p]][argspace[[p]]$position == p.pos & argspace[[p]]$reason == p.reas][,3]
          if(argspace[[p]][argspace[[p]]$position == p.pos & argspace[[p]]$reason == p.reas][,3] > 1){
            argspace[[p]][argspace[[p]]$position == p.pos & argspace[[p]]$reason == p.reas][,3] <<- 1
          }
          if(p.force < (agents[agid == a, dqual]/2)){
            # The speaker's propensity to deliberate decreases
            agents$dprop[p] <<- agents$dprop[p] - .1*agents$dprop[p]
            if(agents$dprop[p] <0){agents$dprop[p] <<- 0}
            
            # The listener's receptivity to the reason given decreases, and their receptivity to the reason they gave increases by 10% (floor of 0, cap of 1)
            argspace[[a]][argspace[[a]]$position == p.pos & argspace[[a]]$reason == p.reas][,3] <<- 
              argspace[[a]][argspace[[a]]$position == p.pos & argspace[[a]]$reason == p.reas][,3] - 
              .1*argspace[[a]][argspace[[a]]$position == p.pos & argspace[[a]]$reason == p.reas][,3]
            if(argspace[[a]][argspace[[a]]$position == p.pos & argspace[[a]]$reason == p.reas][,3] < 0){
              argspace[[a]][argspace[[a]]$position == p.pos & argspace[[a]]$reason == p.reas][,3] <<- 0
            }
            argspace[[a]][argspace[[a]]$position == a.pos & argspace[[a]]$reason == a.reas][,3] <<- 
              argspace[[a]][argspace[[a]]$position == a.pos & argspace[[a]]$reason == a.reas][,3] + 
              .1*argspace[[a]][argspace[[a]]$position == a.pos & argspace[[a]]$reason == a.reas][,3]
            if(argspace[[a]][argspace[[a]]$position == a.pos & argspace[[a]]$reason == a.reas][,3] < 1){
              argspace[[a]][argspace[[a]]$position == a.pos & argspace[[a]]$reason == a.reas][,3] <<- 1
            }
          }
        }
      }
      # Once deliberation has concluded, remove partners from delibs before the next iteration
      delibs <- delibs[-which(delibs %in% c(a,p))]
    }
    # When loop finishes:
    # Update position confidences
    agents$p.conf <<- sapply(1:nrow(agents), function(x){
      agents$p.conf[x] <<- sum(argspace[[x]][,3][argspace[[x]]$position == agents$position[x]][agents$p.rep[x][[1]]]) -
        sum(argspace[[x]][,3][argspace[[x]]$position != agents$position[x]][agents$o.rep[x][[1]]])
    })
    
    # If position confidence falls below zero, flip positions
    for(x in 1:nrow(agents){
      if(agents$position[x] == "for" & agents$p.conf[x] < 0){
        agents$position[x] <<- "against"
        placeholder.prep <- agents$p.rep[x]
        placeholder.orep <- agents$o.rep[x]
        agents$p.rep[x] <<- placeholder.orep
        agents$o.rep[x] <<- placeholder.prep
      }
      if(agents$position[x] == "against" & agents$p.conf[x] < 0){
        agents$position[x] <<- "for"
        placeholder.prep <- agents$p.rep[x]
        placeholder.orep <- agents$o.rep[x]
        agents$p.rep[x] <<- placeholder.orep
        agents$o.rep[x] <<- placeholder.prep
      }
      else{next}
    })
    
    # Update repertoire sizes
    agents$p.repsize <<- sapply(agents$p.rep, function(x){
      length(x)
    })
    agents$o.repsize <<- sapply(agents$o.rep, function(x){
      length(x)
    })
  }
}
test <- do.delibspace()
agents1 <- test[[1]]
argspace1 <- test[[2]]
plotDelib(dat = agents1, view = "p.repsize")

deliberate(100000)
plotDelib(dat = agents1, view = "p.repsize")
plotDelib(dat = agents, view = "p.repsize")
plotDelib(dat = agents1, view = "o.repsize")
plotDelib(dat = agents, view = "o.repsize")
mean(agents1$o.repsize)
mean(agents$o.repsize)

test2 <- do.delibspace(olead.dens = .2, base.dprop = .5, lead.dprop = .8)
agents2 <- test2[[1]]
argspace2 <- test2[[2]]
deliberate(100000)
plotDelib(dat = agents2, view = "p.repsize")
plotDelib(dat = agents, view = "p.repsize")

plotDelib(dat = agents2, view = "o.repsize")
plotDelib(dat = agents, view = "o.repsize")

plotDelib(dat = agents2, view = "p.conf")
plotDelib(dat = agents, view = "p.conf")

mean(agents2$p.repsize)
mean(agents$p.repsize)

table(agents$position)
table(agents2$position)



