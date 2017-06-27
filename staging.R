do.sugarscape <- function(dim = 50, popdens = .25, capacity = 4, maxviz = 3, characteristics = c("id","vision","sugar")){
  require(data.table)
  sugar <- matrix(NA, nrow = dim, ncol = dim)
  for(i in 1:length(sugar)){
    sugar[i] <- sample(0:capacity, 1)
  }
  
  n_agents <- round(dim*dim*popdens)

  a <- matrix(NA, nrow = dim, ncol = dim)
  for(i in 1:length(a)){
    a[i] <- sample(1:(dim*dim), 1, replace = F)
  }
  a[a > n_agents] <- NA
  
  agents <<- array(0, dim=c(dim, dim, length(characteristics)))
  agcount <- 0
  for (i in 1:dim){
    for (j in 1:dim){
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
  scape <<- data.table(cellid = 1:prod(dim*dim),
                       x = rep(1:dim, dim),
                       y = rep(1:dim, each = dim),
                       cellsugar = rep(NA, prod(dim*dim)),
                       cellcap = rep(capacity, prod(dim*dim)),
                       agid = rep(NA, prod(dim*dim)),
                       agviz = rep(NA, prod(dim*dim)),
                       agsugar = rep(NA, prod(dim*dim))
  )
  for(i in 1:dim){
    for(j in 1:dim){
      scape$cellsugar[scape$x == i & scape$y == j] <<- sugar[i,j]
      scape$agid[scape$x == i & scape$y == j] <<- agents[i,j,1]
      scape$agviz[scape$x == i & scape$y == j] <<- agents[i,j,2]
      scape$agsugar[scape$x == i & scape$y == j] <<- agents[i,j,3]
    }
  }
  for(i in 1:length(scape$cellsugar)){
    if(!is.na(scape$agsugar[i])){
      s <- scape$cellsugar[i]
      scape$agsugar[i] <<- scape$agsugar[i] + s
      scape$cellsugar[i] <<- scape$cellsugar[i] - s
    }
  }
}

do.sugarscape()

iterate <- function(iterations, grate = 1){
  scape$cellsugar <<- scape$cellsugar + grate
  see <- function(x_value, y_value, dimension){
    for(i in 1:maxviz){
    x_vals <- c((x_value+1):(x_value+maxviz), (x_value-1):(x_value-maxviz))
    x_vals <- sapply(x_vals, function(x) 
      if(x < 1){x + dimension} 
      if(x > dimension){x - dimension}
      else{x})

    y_vals <- c((y_value+1):(y_value+maxviz), (y_value-1):(y_value-maxviz))
    y_vals <- sapply(y_vals, function(x)
      if(x < 1){x + dimension}
      if(x > dimension){x - dimension}
      else{x})
    see.ids <- scape[x %in% x_vals & y %in% y_vals, id]
    see.ids <- see.ids[! see.ids %in% scape[x == x_value & y == y_value, id]]
    }
  }
}





#######

if(rule == "cone"){
  coords <- rbind(c(col, row+1), c(col, row-1))
  dis <- 1
  for(i in (row+1):dim){
    y <- i
    x_vals <- c((col-dis):(col+dis))
    for(j in 1:length(x_vals)){
      coords <- rbind(coords, c(x_vals[j], y))
    }
    dis <- dis+1
  }
  coords[,1] <- sapply(coords[,1], function(x) {
    ifelse(x < 1, 1, x)
  })
  coords <- unique(coords)
  
  see.ids <- aud[x %in% coords[,1] & y %in% coords[,2], id]
  pct.stand <- sum(aud[id %in% see.ids, stand])/length(unique(see.ids))
  return(pct.stand > aud[x == col & y == row, st])
}