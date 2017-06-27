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
  pickcell <- function(x_value, y_value, maxviz, dimension){
    for(i in 1:maxviz){
      x_vals <- c((x_value+1):(x_value+maxviz), (x_value-1):(x_value-maxviz))
      x_vals <- lapply(x_vals, function(x) 
        if(x < 1){x + dimension} 
        if(x > dimension){x - dimension}
        else{x})
      
      y_vals <- c((y_value+1):(y_value+maxviz), (y_value-1):(y_value-maxviz))
      y_vals <- lapply(y_vals, function(x)
        if(x < 1){x + dimension}
        if(x > dimension){x - dimension}
        else{x})
      see.ids <- scape[x %in% x_vals & y %in% y_vals, id]
      see.ids <- see.ids[! see.ids %in% scape[x == x_value & y == y_value, id]]
    }
    cellvalues <- scape[id %in% see.ids, cellsugar]
    return(max(scape[id %in% see.ids & cellsugar %in% cellvalues, id]))
  }
}
