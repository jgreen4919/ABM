# Sugarscape Replication (from Epstein and Axtell 1996)

dim <- 50
popdens <- .25
capacity <- 4
characteristics <- c("id", "vision", "sugar")
maxviz <- 3
grate <- 1




sugar <- matrix(NA, nrow = dim, ncol = dim)
for(i in 1:length(sugar)){
  sugar[i] <- sample(0:capacity, 1)
}

a <- matrix(NA, nrow = dim, ncol = dim)
for(i in 1:length(a)){
  a[i] <- popdens > runif(1)
}

agents <- array(0, dim=c(dim, dim, length(characteristics)))
agcount <- 0
for (i in 1:dim){
  for (j in 1:dim){
      if(a[i,j] == TRUE){
        agents[i,j,1] <- agcount+1
        agcount <- agcount+1
        agents[i,j,2] <- sample(1:maxviz, 1)
        agents[i,j,3] <- 0
      }
      else{
        agents[i,j,1] <- NA
        agents[i,j,2] <- NA
        agents[i,j,3] <- NA
      }
  }
}

do.sugarscape <- function(dim = 50, popdens = .25, capacity = 4, grate = 1, maxviz = 3, characteristics = c("id","vision","sugar")){
  require(data.table)
  sugar <- matrix(NA, nrow = dim, ncol = dim)
  for(i in 1:length(sugar)){
    sugar[i] <- sample(0:capacity, 1)
  }
  
  a <- matrix(NA, nrow = dim, ncol = dim)
  for(i in 1:length(a)){
    a[i] <- popdens > runif(1)
  }
  agents <- array(0, dim=c(dim, dim, length(characteristics)))
  agcount <- 0
  for (i in 1:dim){
    for (j in 1:dim){
      if(a[i,j] == TRUE){
        agents[i,j,1] <- agcount+1
        agcount <- agcount+1
        agents[i,j,2] <- sample(1:maxviz, 1)
        agents[i,j,3] <- 0
      }
      else{
        agents[i,j,1] <- NA
        agents[i,j,2] <- NA
        agents[i,j,3] <- NA
      }
    }
  }  
  scape <- data.table(cellid = 1:prod(dim*dim),
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
      scape$cellsugar[scape$x == i & scape$y == j] <- sugar[i,j]
      scape$agid[scape$x == i & scape$y == j] <- agents[i,j,1]
      scape$agviz[scape$x == i & scape$y == j] <- agents[i,j,2]
      scape$agsugar[scape$x == i & scape$y == j] <- agents[i,j,3]
    }
  }
  scape$agsugar <- with(scape, agsugar+cellsugar)
  scape$cellsugar <- with(scape, cellsugar - agsugar)
}

View(scape)
