source("~/Desktop/ABM/Deliberation/delib_functions.R")

initl <- do.delibspace()
agents <- initl[[1]]
agents1l <- agents
argspace <- initl[[2]]
argspace1l <- argspace
run1l <- deliberate(100, interaction.rule = "local")
agents1lc <- agents
argspace1lc <- argspace

initg <- do.delibspace()
agents <- initg[[1]]
agents1g <- agents
argspace <- initg[[2]]
argspace1g <- argspace
run1g <- deliberate(100, interaction.rule = "global")
agents1gc <- agents
argspace1gc <- argspace

init2 <- do.delibspace(olead.dens = .2)
agents <- init2[[1]]
agents2 <- agents
argspace <- init2[[2]]
argspace2 <- argspace
run2 <- deliberate(100)
agents2c <- agents
argspace2c <- argspace

init3 <- do.delibspace(olead.dens = .2, polarization = .3, base.dprop = .5, lead.dprop = .8)
agents <- init3[[1]]
agents3 <- agents
argspace <- init3[[2]]
argspace3 <- argspace
run3 <- deliberate(100)
agents3c <- agents
argspace3c <- argspace

init4 <- do.delibspace(olead.dens = 0)
agents <- init4[[1]]
agents4 <- agents
argspace <- init4[[2]]
argspace4 <- argspace
run4 <- deliberate(100)
agents4c <- agents
argspace4c <- argspace

plot(density(agents1l$dprop), main = "Initial Propensity to Deliberate (Local Vision Rule)", xlab = "Value", ylab = "Density")
plot(density(agents1lc$dprop), main = "Propensity to Deliberate after 100 Rounds (Local Vision Rule)", xlab = "Value", ylab = "Density")
plot(density(agents1g$dprop), main = "Initial Propensity to Deliberate (Global Vision Rule)", xlab = "Value", ylab = "Density")
plot(density(agents1gc$dprop), main = "Propensity to Deliberate after 100 Rounds (Global Vision Rule)", xlab = "Value", ylab = "Density")

plot(density(agents1l$dqual), main = "Initial Deliberative Skill (Local Vision Rule)", xlab = "Value", ylab = "Density")
plot(density(agents1lc$dqual), main = "Deliberative Skill after 100 Rounds (Local Vision Rule)", xlab = "Value", ylab = "Density")
plot(density(agents1g$dqual), main = "Initial Deliberative Skill (Global Vision Rule)", xlab = "Value", ylab = "Density")
plot(density(agents1gc$dqual), main = "Deliberative Skill after 100 Rounds (Global Vision Rule)", xlab = "Value", ylab = "Density")

plot(density(agents1l$p.conf), main = "Initial Position Confidence (Local Vision Rule)", xlab = "Value", ylab = "Density")
plot(density(agents1lc$p.conf), main = "Position Confidence after 100 Rounds (Local Vision Rule)", xlab = "Value", ylab = "Density")
plot(density(agents1g$p.conf), main = "Initial Position Confidence (Global Vision Rule)", xlab = "Value", ylab = "Density")
plot(density(agents1gc$p.conf), main = "Position Confidence after 100 Rounds (Global Vision Rule)", xlab = "Value", ylab = "Density")

plot(density(agents1l$o.repsize), main = "Initial Against-Repertoire Size (Local Vision Rule)", xlab = "Arguments in Against-Repertoire", ylab = "Density")
plot(density(agents1lc$o.repsize), main = "Against-Repertoire Size after 100 Rounds (Local Vision Rule)", xlab = "Arguments in Against-Repertoire", ylab = "Density")
plot(density(agents1g$o.repsize), main = "Initial Against-Repertoire Size (Global Vision Rule)", xlab = "Arguments in Against-Repertoire", ylab = "Density")
plot(density(agents1gc$o.repsize), main = "Against-Repertoire Size after 100 Rounds (Global Vision Rule)", xlab = "Arguments in Against-Repertoire", ylab = "Density")

plot(density(agents1l$p.repsize), main = "Initial For-Repertoire Size (Local Vision Rule)", xlab = "Arguments in For-Repertoire", ylab = "Density")
plot(density(agents1lc$p.repsize), main = "For-Repertoire Size after 100 Rounds (Local Vision Rule)", xlab = "Arguments in For-Repertoire", ylab = "Density")
plot(density(agents1g$p.repsize), main = "Initial For-Repertoire Size (Global Vision Rule)", xlab = "Arguments in For-Repertoire", ylab = "Density")
plot(density(agents1gc$p.repsize), main = "For-Repertoire Size after 100 Rounds (Global Vision Rule)", xlab = "Arguments in For-Repertoire", ylab = "Density")

hist(agents1lc$o.repsize)
hist(agents1gc$o.repsize)

plot(density(agents2$dprop))
plot(density(agents2c$dprop))

plot(density(agents3$dprop))
plot(density(agents3c$dprop))

plot(density(agents1$dqual))
plot(density(agents1c$dqual))

plot(density(agents2$dqual))
plot(density(agents2c$dqual))

plot(density(agents3$dqual))
plot(density(agents3c$dqual))

plotDelib(title = "Initial Deliberative Space (Local Vision Rule)", dat = agents1l, view = "p.repsize")
plotDelib(title = "Deliberative Space after 100 Rounds (Local Vision Rule)", dat = agents1lc, view = "p.repsize")
plotDelib(title = "Initial Deliberative Space (Global Vision Rule)", dat = agents1g, view = "p.repsize")
plotDelib(title = "Deliberative Space after 100 Rounds (Global Vision Rule)", dat = agents1gc, view = "p.repsize")

plotDelib(title = "Initial Deliberative Space (Local Vision Rule)", dat = agents1l, view = "position")
plotDelib(title = "Initial Deliberative Space (Local Vision Rule)", dat = agents1lc, view = "position")
table(agents1l$position)
table(agents1lc$position)
plotDelib(title = "Initial Deliberative Space (Local Vision Rule)", dat = agents1g, view = "position")
plotDelib(title = "Initial Deliberative Space (Local Vision Rule)", dat = agents1gc, view = "position")
table(agents1g$position)
table(agents1gc$position)


plotDelib(title = "Initial Deliberative Space (20% Leaders)", dat = agents2, view = "p.repsize")
plotDelib(title = "Deliberative Space after 100 Rounds (20% Leaders)", dat = agents2c, view = "p.repsize")

plotDelib(title = "Initial Deliberative Space (20% Leaders/High Prop+Polar)", dat = agents3, view = "p.repsize")
plotDelib(title = "Deliberative Space after 100 Rounds (20% Leaders/High Prop+Polar)", dat = agents3c, view = "p.repsize")

plotDelib(title = "Initial Deliberative Space (Local Vision Rule)", dat = agents1l, view = "o.repsize")
plotDelib(title = "Deliberative Space after 100 Rounds (Local Vision Rule)", dat = agents1lc, view = "o.repsize")
plotDelib(title = "Initial Deliberative Space (Global Vision Rule)", dat = agents1g, view = "o.repsize")
plotDelib(title = "Deliberative Space after 100 Rounds (Global Vision Rule)", dat = agents1gc, view = "o.repsize")

plotDelib(title = "Initial Deliberative Space (20% Leaders)", dat = agents2, view = "o.repsize")
plotDelib(title = "Deliberative Space after 100 Rounds (20% Leaders)", dat = agents2c, view = "o.repsize")

plotDelib(title = "Initial Deliberative Space (20% Leaders/High Prop+Polar)", dat = agents3, view = "o.repsize")
plotDelib(title = "Deliberative Space after 100 Rounds (20% Leaders/High Prop+Polar)", dat = agents3c, view = "o.repsize")

plotDelib(title = "Initial Deliberative Space (Local Vision Rule)", dat = agents1l, view = "dqual")
plotDelib(title = "Deliberative Space after 100 Rounds (Local Vision Rule)", dat = agents1lc, view = "dqual")
plotDelib(title = "Initial Deliberative Space (Global Vision Rule)", dat = agents1g, view = "dqual")
plotDelib(title = "Deliberative Space after 100 Rounds (Global Vision Rule)", dat = agents1gc, view = "dqual")

plotDelib(title = "Initial Deliberative Space (20% Leaders)", dat = agents2, view = "dqual")
plotDelib(title = "Deliberative Space after 100 Rounds (20% Leaders)", dat = agents2c, view = "dqual")
plotDelib(title = "Initial Deliberative Space (20% Leaders/High Prop+Polar)", dat = agents3, view = "dqual")
plotDelib(title = "Deliberative Space after 100 Rounds (20% Leaders/High Prop+Polar)", dat = agents3c, view = "dqual")

plotDelib(title = "Initial Deliberative Space (Local Vision Rule)", dat = agents1l, view = "dprop")
plotDelib(title = "Deliberative Space after 100 Rounds (Local Vision Rule)", dat = agents1lc, view = "dprop")
plotDelib(title = "Initial Deliberative Space (Global Vision Rule)", dat = agents1g, view = "dprop")
plotDelib(title = "Deliberative Space after 100 Rounds (Global Vision Rule)", dat = agents1gc, view = "dprop")

plotDelib(title = "Initial Deliberative Space (20% Leaders)", dat = agents2, view = "dprop")
plotDelib(title = "Deliberative Space after 100 Rounds (20% Leaders)", dat = agents2c, view = "dprop")
plotDelib(title = "Initial Deliberative Space (20% Leaders/High Prop+Polar)", dat = agents3, view = "dprop")
plotDelib(title = "Deliberative Space after 100 Rounds (20% Leaders/High Prop+Polar)", dat = agents3c, view = "dprop")

# Position Confidence
for(i in 1:length(run1l$mean.pconf)){
  if(i == 1){
    plot(-100, -100, xlim=c(1,100), ylim=c(0,1), ylab="Value", xlab="Iteration", type="n", cex.axis=0.8, main = "Position Confidence")
  }else{
    segments(i-1, run1l$mean.pconf[i-1], i, run1l$mean.pconf[i], col = "blue", lwd=2)
    segments(i-1, run1g$mean.pconf[i-1], i, run1g$mean.pconf[i], col = "red", lwd=2)
  }
}

# Percent in favor
for(i in 1:length(run1l$pct.for)){
  if(i == 1){
    plot(-100, -100, xlim=c(1,100), ylim=c(.4, .55), ylab="Percent", xlab="Iteration", type="n", cex.axis=0.8, main = "Percent in Favor")
  }else{
    segments(i-1, run1l$pct.for[i-1], i, run1l$pct.for[i], col = "blue", lwd=2)
    segments(i-1, run1g$pct.for[i-1], i, run1g$pct.for[i], col = "red", lwd=2)
  }
}

# Mean Deliberative Quality
for(i in 1:length(run1l$pct.for)){
  if(i == 1){
    plot(-100, -100, xlim=c(1,100), ylim=c(.4,.7), ylab="Value", xlab="Iteration", type="n", cex.axis=0.8, main = "Mean Deliberative Quality")
  }else{
    segments(i-1, run1l$mean.dqual[i-1], i, run1l$mean.dqual[i], col = "blue", lwd=2)
    segments(i-1, run1g$mean.dqual[i-1], i, run1g$mean.dqual[i], col = "red", lwd=2)
  }
}

# Mean Propensity to Deliberate
for(i in 1:length(run1l$pct.for)){
  if(i == 1){
    plot(-100, -100, xlim=c(1,100), ylim=c(.3, .5), ylab="Value", xlab="Round", type="n", cex.axis=0.8, main = "Mean Propensity to Deliberate")
  }else{
    segments(i-1, run1l$mean.dprop[i-1], i, run1l$mean.dprop[i], col = "blue", lwd=2)
    segments(i-1, run1g$mean.dprop[i-1], i, run1g$mean.dprop[i], col = "red", lwd=2)
  }
}
legend(x = 10, y = .45, legend = c("Local Vision Rule","Global Vision Rule"), fill = c("blue","red"))

# Mean For-Repertoire Size
for(i in 1:length(run1l$pct.for)){
  if(i == 1){
    plot(-100, -100, xlim=c(1,100), ylim=c(3,10), ylab="Value", xlab="Iteration", type="n", cex.axis=0.8, main = "Mean For-Repertoire Size")
  }else{
    segments(i-1, run1l$mean.prepsize[i-1], i, run1l$mean.prepsize[i], col = "blue", lwd=2)
    segments(i-1, run1g$mean.prepsize[i-1], i, run1g$mean.prepsize[i], col = "red", lwd=2)
  }
}

# Mean Against-Repertoire Size
for(i in 1:length(run1l$pct.for)){
  if(i == 1){
    plot(-100, -100, xlim=c(1,100), ylim=c(1,3), ylab="Value", xlab="Iteration", type="n", cex.axis=0.8, main = "Mean Against-Repertoire Size")
  }else{
    segments(i-1, run1l$mean.orepsize[i-1], i, run1l$mean.orepsize[i], col = "blue", lwd=2)
    segments(i-1, run1g$mean.orepsize[i-1], i, run1g$mean.orepsize[i], col = "red", lwd=2)
  }
}

# Number deliberating each round
for(i in 1:length(run1l$pct.for)){
  if(i == 1){
    plot(-100, -100, xlim=c(1,100), ylim=c(100,300), ylab="Number", xlab="Iteration", type="n", cex.axis=0.8, main = "Number Deliberating Each Round")
  }else{
    segments(i-1, run1l$num.delib[i-1], i, run1l$num.delib[i], col = "blue", lwd=2)
    segments(i-1, run1g$num.delib[i-1], i, run1g$num.delib[i], col = "red", lwd=2)
  }
}

# Flips each round
for(i in 1:length(run1l$pct.for)){
  if(i == 1){
    plot(-100, -100, xlim=c(1,100), ylim=c(0,10), ylab="Number", xlab="Iteration", type="n", cex.axis=0.8, main = "Number of Position Changes Each Round")
  }else{
    segments(i-1, run1l$flips[i-1], i, run1l$flips[i], col = "blue", lwd=2)
    segments(i-1, run1g$flips[i-1], i, run1g$flips[i], col = "red", lwd=2)
  }
}

# Tension each round
for(i in 1:length(run1l$tension)){
  if(i == 1){
    plot(-100, -100, xlim=c(1,100), ylim=c(0,1), ylab="Tension Level", xlab="Iteration", type="n", cex.axis=0.8, main = "Tension Level Each Round")
  }else{
    segments(i-1, run1l$tension[i-1], i, run1l$tension[i], col = "blue", lwd=2)
    segments(i-1, run1g$tension[i-1], i, run1g$tension[i], col = "red", lwd=2)
  }
}
legend(x = 10, y = .2, legend = c("Local Vision Rule","Global Vision Rule"), fill = c("blue","red"))

