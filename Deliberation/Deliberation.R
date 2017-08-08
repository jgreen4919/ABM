rm(list = ls())
source("~/Desktop/ABM/Deliberation/delib_functions.R")
setwd("~/Desktop/ABM/Deliberation/")
load("delib_runs.RData")

initl <- do.delibspace()
agents <- initl[[1]]
agents1l <- agents
argspace <- initl[[2]]
argspace1l <- argspace
run1l <- deliberate(100, interaction.rule = "local")
agents1lc <- agents
argspace1lc <- argspace

initl_2 <- do.delibspace()
agents <- initl_2[[1]]
agents1l_2 <- agents
argspace <- initl_2[[2]]
argspace1l_2 <- argspace
run1l_2 <- deliberate(100, interaction.rule = "local")
agents1lc_2 <- agents
argspace1lc_2 <- argspace

initl_3 <- do.delibspace()
agents <- initl_3[[1]]
agents1l_3 <- agents
argspace <- initl_3[[2]]
argspace1l_3 <- argspace
run1l_3 <- deliberate(100, interaction.rule = "local")
agents1lc_3 <- agents
argspace1lc_3 <- argspace

initl_4 <- do.delibspace()
agents <- initl_4[[1]]
agents1l_4 <- agents
argspace <- initl_4[[2]]
argspace1l_4 <- argspace
run1l_4 <- deliberate(100, interaction.rule = "local")
agents1lc_4 <- agents
argspace1lc_4 <- argspace

initl_5 <- do.delibspace()
agents <- initl_5[[1]]
agents1l_5 <- agents
argspace <- initl_5[[2]]
argspace1l_5 <- argspace
run1l_5 <- deliberate(100, interaction.rule = "local")
agents1lc_5 <- agents
argspace1lc_5 <- argspace

initg <- do.delibspace()
agents <- initg[[1]]
agents1g <- agents
argspace <- initg[[2]]
argspace1g <- argspace
run1g <- deliberate(100, interaction.rule = "global")
agents1gc <- agents
argspace1gc <- argspace

initg_2 <- do.delibspace()
agents <- initg_2[[1]]
agents1g_2 <- agents
argspace <- initg_2[[2]]
argspace1g_2 <- argspace
run1g_2 <- deliberate(100, interaction.rule = "global")
agents1gc_2 <- agents
argspace1gc_2 <- argspace

initg_3 <- do.delibspace()
agents <- initg_3[[1]]
agents1g_3 <- agents
argspace <- initg_3[[2]]
argspace1g_3 <- argspace
run1g_3 <- deliberate(100, interaction.rule = "global")
agents1gc_3 <- agents
argspace1gc_3 <- argspace

initg_4 <- do.delibspace()
agents <- initg_4[[1]]
agents1g_4 <- agents
argspace <- initg_4[[2]]
argspace1g_4 <- argspace
run1g_4 <- deliberate(100, interaction.rule = "global")
agents1gc_4 <- agents
argspace1gc_4 <- argspace

initg_5 <- do.delibspace()
agents <- initg_5[[1]]
agents1g_5 <- agents
argspace <- initg_5[[2]]
argspace1g_5 <- argspace
run1g_5 <- deliberate(100, interaction.rule = "global")
agents1gc_5 <- agents
argspace1gc_5 <- argspace

init2l <- do.delibspace(olead.dens = .2)
agents <- init2l[[1]]
agents2l <- agents
argspace <- init2l[[2]]
argspace2l <- argspace
run2l <- deliberate(100, interaction.rule = "local")
agents2lc <- agents
argspace2lc <- argspace

init2l_2 <- do.delibspace(olead.dens = .2)
agents <- init2l_2[[1]]
agents2l_2 <- agents
argspace <- init2l_2[[2]]
argspace2l_2 <- argspace
run2l_2 <- deliberate(100, interaction.rule = "local")
agents2lc_2 <- agents
argspace2lc_2 <- argspace

init2l_3 <- do.delibspace(olead.dens = .2)
agents <- init2l_3[[1]]
agents2l_3 <- agents
argspace <- init2l_3[[2]]
argspace2l_3 <- argspace
run2l_3 <- deliberate(100, interaction.rule = "local")
agents2lc_3 <- agents
argspace2lc_3 <- argspace

init2l_4 <- do.delibspace(olead.dens = .2)
agents <- init2l_4[[1]]
agents2l_4 <- agents
argspace <- init2l_4[[2]]
argspace2l_4 <- argspace
run2l_4 <- deliberate(100, interaction.rule = "local")
agents2lc_4 <- agents
argspace2lc_4 <- argspace

init2l_5 <- do.delibspace(olead.dens = .2)
agents <- init2l_5[[1]]
agents2l_5 <- agents
argspace <- init2l_5[[2]]
argspace2l_5 <- argspace
run2l_5 <- deliberate(100, interaction.rule = "local")
agents2lc_5 <- agents
argspace2lc_5 <- argspace

init2g <- do.delibspace(olead.dens = .2)
agents <- init2g[[1]]
agents2g <- agents
argspace <- init2g[[2]]
argspace2g <- argspace
run2g <- deliberate(100, interaction.rule = "global")
agents2gc <- agents
argspace2gc <- argspace

init2g_2 <- do.delibspace(olead.dens = .2)
agents <- init2g_2[[1]]
agents2g_2 <- agents
argspace <- init2g_2[[2]]
argspace2g_2 <- argspace
run2g_2 <- deliberate(100, interaction.rule = "global")
agents2gc_2 <- agents
argspace2gc_2 <- argspace

init2g_3 <- do.delibspace(olead.dens = .2)
agents <- init2g_3[[1]]
agents2g_3 <- agents
argspace <- init2g_3[[2]]
argspace2g_3 <- argspace
run2g_3 <- deliberate(100, interaction.rule = "global")
agents2gc_3 <- agents
argspace2gc_3 <- argspace

init2g_4 <- do.delibspace(olead.dens = .2)
agents <- init2g_4[[1]]
agents2g_4 <- agents
argspace <- init2g_4[[2]]
argspace2g_4 <- argspace
run2g_4 <- deliberate(100, interaction.rule = "global")
agents2gc_4 <- agents
argspace2gc_4 <- argspace

init2g_5 <- do.delibspace(olead.dens = .2)
agents <- init2g_5[[1]]
agents2g_5 <- agents
argspace <- init2g_5[[2]]
argspace2g_5 <- argspace
run2g_5 <- deliberate(100, interaction.rule = "global")
agents2gc_5 <- agents
argspace2gc_5 <- argspace

init3l <- do.delibspace(olead.dens = .2, polarization = .3, base.dprop = .5, lead.dprop = .8)
agents <- init3l[[1]]
agents3l <- agents
argspace <- init3l[[2]]
argspace3l <- argspace
run3l <- deliberate(100, interaction.rule = "local")
agents3lc <- agents
argspace3lc <- argspace

init3l_2 <- do.delibspace(olead.dens = .2, polarization = .3, base.dprop = .5, lead.dprop = .8)
agents <- init3l_2[[1]]
agents3l_2 <- agents
argspace <- init3l_2[[2]]
argspace3l_2 <- argspace
run3l_2 <- deliberate(100, interaction.rule = "local")
agents3lc_2 <- agents
argspace3lc_2 <- argspace

init3l_3 <- do.delibspace(olead.dens = .2, polarization = .3, base.dprop = .5, lead.dprop = .8)
agents <- init3l_3[[1]]
agents3l_3 <- agents
argspace <- init3l_3[[2]]
argspace3l_3 <- argspace
run3l_3 <- deliberate(100, interaction.rule = "local")
agents3lc_3 <- agents
argspace3lc_3 <- argspace

init3l_4 <- do.delibspace(olead.dens = .2, polarization = .3, base.dprop = .5, lead.dprop = .8)
agents <- init3l_4[[1]]
agents3l_4 <- agents
argspace <- init3l_4[[2]]
argspace3l_4 <- argspace
run3l_4 <- deliberate(100, interaction.rule = "local")
agents3lc_4 <- agents
argspace3lc_4 <- argspace

init3l_5 <- do.delibspace(olead.dens = .2, polarization = .3, base.dprop = .5, lead.dprop = .8)
agents <- init3l_5[[1]]
agents3l_5 <- agents
argspace <- init3l_5[[2]]
argspace3l_5 <- argspace
run3l_5 <- deliberate(100, interaction.rule = "local")
agents3lc_5 <- agents
argspace3lc_5 <- argspace

init3g <- do.delibspace(olead.dens = .2, polarization = .3, base.dprop = .5, lead.dprop = .8)
agents <- init3g[[1]]
agents3g <- agents
argspace <- init3g[[2]]
argspace3g <- argspace
run3g <- deliberate(100, interaction.rule = "global")
agents3gc <- agents
argspace3gc <- argspace

init3g_2 <- do.delibspace(olead.dens = .2, polarization = .3, base.dprop = .5, lead.dprop = .8)
agents <- init3g_2[[1]]
agents3g_2 <- agents
argspace <- init3g_2[[2]]
argspace3g_2 <- argspace
run3g_2 <- deliberate(100, interaction.rule = "global")
agents3gc_2 <- agents
argspace3gc_2 <- argspace

init3g_3 <- do.delibspace(olead.dens = .2, polarization = .3, base.dprop = .5, lead.dprop = .8)
agents <- init3g_3[[1]]
agents3g_3 <- agents
argspace <- init3g_3[[2]]
argspace3g_3 <- argspace
run3g_3 <- deliberate(100, interaction.rule = "global")
agents3gc_3 <- agents
argspace3gc_3 <- argspace

init3g_4 <- do.delibspace(olead.dens = .2, polarization = .3, base.dprop = .5, lead.dprop = .8)
agents <- init3g_4[[1]]
agents3g_4 <- agents
argspace <- init3g_4[[2]]
argspace3g_4 <- argspace
run3g_4 <- deliberate(100, interaction.rule = "global")
agents3gc_4 <- agents
argspace3gc_4 <- argspace

init3g_5 <- do.delibspace(olead.dens = .2, polarization = .3, base.dprop = .5, lead.dprop = .8)
agents <- init3g_5[[1]]
agents3g_5 <- agents
argspace <- init3g_5[[2]]
argspace3g_5 <- argspace
run3g_5 <- deliberate(100, interaction.rule = "global")
agents3gc_5 <- agents
argspace3gc_5 <- argspace

init4l <- do.delibspace(olead.dens = 0)
agents <- init4l[[1]]
agents4l <- agents
argspace <- init4l[[2]]
argspace4l <- argspace
run4l <- deliberate(100, interaction.rule = "local")
agents4lc <- agents
argspace4lc <- argspace

init4l_2 <- do.delibspace(olead.dens = 0)
agents <- init4l_2[[1]]
agents4l_2 <- agents
argspace <- init4l_2[[2]]
argspace4l_2 <- argspace
run4l_2 <- deliberate(100, interaction.rule = "local")
agents4lc_2 <- agents
argspace4lc_2 <- argspace

init4l_3 <- do.delibspace(olead.dens = 0)
agents <- init4l_3[[1]]
agents4l_3 <- agents
argspace <- init4l_3[[2]]
argspace4l_3 <- argspace
run4l_3 <- deliberate(100, interaction.rule = "local")
agents4lc_3 <- agents
argspace4lc_3 <- argspace

init4l_4 <- do.delibspace(olead.dens = 0)
agents <- init4l_4[[1]]
agents4l_4 <- agents
argspace <- init4l_4[[2]]
argspace4l_4 <- argspace
run4l_4 <- deliberate(100, interaction.rule = "local")
agents4lc_4 <- agents
argspace4lc_4 <- argspace

init4l_5 <- do.delibspace(olead.dens = 0)
agents <- init4l_5[[1]]
agents4l_5 <- agents
argspace <- init4l_5[[2]]
argspace4l_5 <- argspace
run4l_5 <- deliberate(100, interaction.rule = "local")
agents4lc_5 <- agents
argspace4lc_5 <- argspace

init4g <- do.delibspace(olead.dens = 0)
agents <- init4g[[1]]
agents4g <- agents
argspace <- init4g[[2]]
argspace4g <- argspace
run4g <- deliberate(100, interaction.rule = "global")
agents4gc <- agents
argspace4gc <- argspace

init4g_2 <- do.delibspace(olead.dens = 0)
agents <- init4g_2[[1]]
agents4g_2 <- agents
argspace <- init4g_2[[2]]
argspace4g_2 <- argspace
run4g_2 <- deliberate(100, interaction.rule = "global")
agents4gc_2 <- agents
argspace4gc_2 <- argspace

init4g_3 <- do.delibspace(olead.dens = 0)
agents <- init4g_3[[1]]
agents4g_3 <- agents
argspace <- init4g_3[[2]]
argspace4g_3 <- argspace
run4g_3 <- deliberate(100, interaction.rule = "global")
agents4gc_3 <- agents
argspace4gc_3 <- argspace

init4g_4 <- do.delibspace(olead.dens = 0)
agents <- init4g_4[[1]]
agents4g_4 <- agents
argspace <- init4g_4[[2]]
argspace4g_4 <- argspace
run4g_4 <- deliberate(100, interaction.rule = "global")
agents4gc_4 <- agents
argspace4gc_4 <- argspace

init4g_5 <- do.delibspace(olead.dens = 0)
agents <- init4g_5[[1]]
agents4g_5 <- agents
argspace <- init4g_5[[2]]
argspace4g_5 <- argspace
run4g_5 <- deliberate(100, interaction.rule = "global")
agents4gc_5 <- agents
argspace4gc_5 <- argspace

init5l <- do.delibspace(pct.for = .7)
agents <- init5l[[1]]
agents5l <- agents
argspace <- init5l[[2]]
argspace5l <- argspace
run5l <- deliberate(100, interaction.rule = "local")
agents5lc <- agents
argspace5lc <- argspace

init5l_2 <- do.delibspace(pct.for = .7)
agents <- init5l_2[[1]]
agents5l_2 <- agents
argspace <- init5l_2[[2]]
argspace5l_2 <- argspace
run5l_2 <- deliberate(100, interaction.rule = "local")
agents5lc_2 <- agents
argspace5lc_2 <- argspace

init5l_3 <- do.delibspace(pct.for = .7)
agents <- init5l_3[[1]]
agents5l_3 <- agents
argspace <- init5l_3[[2]]
argspace5l_3 <- argspace
run5l_3 <- deliberate(100, interaction.rule = "local")
agents5lc_3 <- agents
argspace5lc_3 <- argspace

init5l_4 <- do.delibspace(pct.for = .7)
agents <- init5l_4[[1]]
agents5l_4 <- agents
argspace <- init5l_4[[2]]
argspace5l_4 <- argspace
run5l_4 <- deliberate(100, interaction.rule = "local")
agents5lc_4 <- agents
argspace5lc_4 <- argspace

init5l_5 <- do.delibspace(pct.for = .7)
agents <- init5l_5[[1]]
agents5l_5 <- agents
argspace <- init5l_5[[2]]
argspace5l_5 <- argspace
run5l_5 <- deliberate(100, interaction.rule = "local")
agents5lc_5 <- agents
argspace5lc_5 <- argspace

init5g <- do.delibspace(pct.for = .7)
agents <- init5g[[1]]
agents5g <- agents
argspace <- init5g[[2]]
argspace5g <- argspace
run5g <- deliberate(100, interaction.rule = "global")
agents5gc <- agents
argspace5gc <- argspace

init5g_2 <- do.delibspace(pct.for = .7)
agents <- init5g_2[[1]]
agents5g_2 <- agents
argspace <- init5g_2[[2]]
argspace5g_2 <- argspace
run5g_2 <- deliberate(100, interaction.rule = "global")
agents5gc_2 <- agents
argspace5gc_2 <- argspace

init5g_3 <- do.delibspace(pct.for = .7)
agents <- init5g_3[[1]]
agents5g_3 <- agents
argspace <- init5g_3[[2]]
argspace5g_3 <- argspace
run5g_3 <- deliberate(100, interaction.rule = "global")
agents5gc_3 <- agents
argspace5gc_3 <- argspace

init5g_4 <- do.delibspace(pct.for = .7)
agents <- init5g_4[[1]]
agents5g_4 <- agents
argspace <- init5g_4[[2]]
argspace5g_4 <- argspace
run5g_4 <- deliberate(100, interaction.rule = "global")
agents5gc_4 <- agents
argspace5gc_4 <- argspace

init5g_5 <- do.delibspace(pct.for = .7)
agents <- init5g_5[[1]]
agents5g_5 <- agents
argspace <- init5g_5[[2]]
argspace5g_5 <- argspace
run5g_5 <- deliberate(100, interaction.rule = "global")
agents5gc_5 <- agents
argspace5gc_5 <- argspace

run1l_agg <- list(NULL)
for(i in 1:length(run1l)){
  run1l_agg[[i]] <- (run1l[[i]] + run1l_2[[i]] + run1l_3[[i]] + run1l_4[[i]] + run1l_5[[i]])/5
}
names(run1l_agg) <- names(run1l)

run1g_agg <- list(NULL)
for(i in 1:length(run1g)){
  run1g_agg[[i]] <- (run1g[[i]] + run1g_2[[i]] + run1g_3[[i]] + run1g_4[[i]] + run1g_5[[i]])/5
}
names(run1g_agg) <- names(run1g)

run2l_agg <- list(NULL)
for(i in 1:length(run2l)){
  run2l_agg[[i]] <- (run2l[[i]] + run2l_2[[i]] + run2l_3[[i]] + run2l_4[[i]] + run2l_5[[i]])/5
}
names(run2l_agg) <- names(run2l)

run2g_agg <- list(NULL)
for(i in 1:length(run2g)){
  run2g_agg[[i]] <- (run2g[[i]] + run2g_2[[i]] + run2g_3[[i]] + run2g_4[[i]] + run2g_5[[i]])/5
}
names(run2g_agg) <- names(run2g)

run3l_agg <- list(NULL)
for(i in 1:length(run3l)){
  run3l_agg[[i]] <- (run3l[[i]] + run3l_2[[i]] + run3l_3[[i]] + run3l_4[[i]] + run3l_5[[i]])/5
}
names(run3l_agg) <- names(run3l)

run3g_agg <- list(NULL)
for(i in 1:length(run3g)){
  run3g_agg[[i]] <- (run3g[[i]] + run3g_2[[i]] + run3g_3[[i]] + run3g_4[[i]] + run3g_5[[i]])/5
}
names(run3g_agg) <- names(run3g)

run4l_agg <- list(NULL)
for(i in 1:length(run4l)){
  run4l_agg[[i]] <- (run4l[[i]] + run4l_2[[i]] + run4l_3[[i]] + run4l_4[[i]] + run4l_5[[i]])/5
}
names(run4l_agg) <- names(run4l)

run4g_agg <- list(NULL)
for(i in 1:length(run4g)){
  run4g_agg[[i]] <- (run4g[[i]] + run4g_2[[i]] + run4g_3[[i]] + run4g_4[[i]] + run4g_5[[i]])/5
}
names(run4g_agg) <- names(run4g)

run5l_agg <- list(NULL)
for(i in 1:length(run5l)){
  run5l_agg[[i]] <- (run5l[[i]] + run5l_2[[i]] + run5l_3[[i]] + run5l_4[[i]] + run5l_5[[i]])/5
}
names(run5l_agg) <- names(run5l)

run5g_agg <- list(NULL)
for(i in 1:length(run5g)){
  run5g_agg[[i]] <- (run5g[[i]] + run5g_2[[i]] + run5g_3[[i]] + run5g_4[[i]] + run5g_5[[i]])/5
}
names(run5g_agg) <- names(run5g)

save(list = ls(all.names = TRUE), file = "delib_runs.RData")

