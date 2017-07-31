source("~/Desktop/ABM/Deliberation/delib_functions.R")

initl <- do.delibspace()
agentsl <- init[[1]]
agents1l <- agents
argspacel <- init[[2]]
argspace1l <- argspace
run1l <- deliberate(100, interaction.rule = "local")
agents1l <- agents
argspace1l <- argspace

initg <- do.delibspace()
agentsg <- init[[1]]
agents1g <- agents
argspaceg <- init[[2]]
argspace1g <- argspace
run1g <- deliberate(100, interaction.rule = "global")
agents1g <- agents
argspace1g <- argspace

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


plot(density(agents1$dprop))
plot(density(agents1c$dprop))

plot(density(agents1$p.conf))
plot(density(agents1c$p.conf))

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

plotDelib(title = "Initial Deliberative Space (10% Leaders)", dat = agents1, view = "p.repsize")
plotDelib(title = "Deliberative Space after 100 Rounds (10% Leaders)", dat = agents1c, view = "p.repsize")

plotDelib(title = "Initial Deliberative Space (20% Leaders)", dat = agents2, view = "p.repsize")
plotDelib(title = "Deliberative Space after 100 Rounds (20% Leaders)", dat = agents2c, view = "p.repsize")

plotDelib(title = "Initial Deliberative Space (20% Leaders/High Prop+Polar)", dat = agents3, view = "p.repsize")
plotDelib(title = "Deliberative Space after 100 Rounds (20% Leaders/High Prop+Polar)", dat = agents3c, view = "p.repsize")

plotDelib(title = "Initial Deliberative Space (10% Leaders)", dat = agents1, view = "o.repsize")
plotDelib(title = "Deliberative Space after 100 Rounds (10% Leaders)", dat = agents1c, view = "o.repsize")

plotDelib(title = "Initial Deliberative Space (20% Leaders)", dat = agents2, view = "o.repsize")
plotDelib(title = "Deliberative Space after 100 Rounds (20% Leaders)", dat = agents2c, view = "o.repsize")

plotDelib(title = "Initial Deliberative Space (20% Leaders/High Prop+Polar)", dat = agents3, view = "o.repsize")
plotDelib(title = "Deliberative Space after 100 Rounds (20% Leaders/High Prop+Polar)", dat = agents3c, view = "o.repsize")

plotDelib(title = "Initial Deliberative Space (10% Leaders)", dat = agents1, view = "dqual")
plotDelib(title = "Deliberative Space after 100 Rounds (10% Leaders)", dat = agents1c, view = "dqual")
plotDelib(title = "Initial Deliberative Space (20% Leaders)", dat = agents2, view = "dqual")
plotDelib(title = "Deliberative Space after 100 Rounds (20% Leaders)", dat = agents2c, view = "dqual")
plotDelib(title = "Initial Deliberative Space (20% Leaders/High Prop+Polar)", dat = agents3, view = "dqual")
plotDelib(title = "Deliberative Space after 100 Rounds (20% Leaders/High Prop+Polar)", dat = agents3c, view = "dqual")

plotDelib(title = "Initial Deliberative Space (10% Leaders)", dat = agents1, view = "dprop")
plotDelib(title = "Deliberative Space after 100 Rounds (10% Leaders)", dat = agents1c, view = "dprop")
plotDelib(title = "Initial Deliberative Space (20% Leaders)", dat = agents2, view = "dprop")
plotDelib(title = "Deliberative Space after 100 Rounds (20% Leaders)", dat = agents2c, view = "dprop")
plotDelib(title = "Initial Deliberative Space (20% Leaders/High Prop+Polar)", dat = agents3, view = "dprop")
plotDelib(title = "Deliberative Space after 100 Rounds (20% Leaders/High Prop+Polar)", dat = agents3c, view = "dprop")

# Position Confidence
for(i in 1:length(run3$mean.pconf)){
  if(i == 1){
    plot(-100, -100, xlim=c(1,100), ylim=c(1,10), ylab="Value", xlab="Iteration", type="n", cex.axis=0.8, main = "Position Confidence")
  }else{
    segments(i-1, run1$mean.pconf[i-1], i, run1$mean.pconf[i], col = "blue", lwd=2)
    segments(i-1, run2$mean.pconf[i-1], i, run2$mean.pconf[i], col = "red", lwd=2)
    segments(i-1, run3$mean.pconf[i-1], i, run3$mean.pconf[i], col = "green", lwd=2)
  }
}

# Percent in favor
for(i in 1:length(run3$pct.for)){
  if(i == 1){
    plot(-100, -100, xlim=c(1,100), ylim=c(.4, .55), ylab="Percent", xlab="Iteration", type="n", cex.axis=0.8, main = "Percent in Favor")
  }else{
    segments(i-1, run1$pct.for[i-1], i, run1$pct.for[i], col = "blue", lwd=2)
    segments(i-1, run2$pct.for[i-1], i, run2$pct.for[i], col = "red", lwd=2)
    segments(i-1, run3$pct.for[i-1], i, run3$pct.for[i], col = "green", lwd=2)
  }
}

# Mean Deliberative Quality
for(i in 1:length(run3$pct.for)){
  if(i == 1){
    plot(-100, -100, xlim=c(1,100), ylim=c(.4,.7), ylab="Value", xlab="Iteration", type="n", cex.axis=0.8, main = "Mean Deliberative Quality")
  }else{
    segments(i-1, run1$mean.dqual[i-1], i, run1$mean.dqual[i], col = "blue", lwd=2)
    segments(i-1, run2$mean.dqual[i-1], i, run2$mean.dqual[i], col = "red", lwd=2)
    segments(i-1, run3$mean.dqual[i-1], i, run3$mean.dqual[i], col = "green", lwd=2)
  }
}

# Mean Propensity to Deliberate
for(i in 1:length(run3$pct.for)){
  if(i == 1){
    plot(-100, -100, xlim=c(1,100), ylim=c(.35, .6), ylab="Value", xlab="Round", type="n", cex.axis=0.8, main = "Mean Propensity to Deliberate")
  }else{
    segments(i-1, run1$mean.dprop[i-1], i, run1$mean.dprop[i], col = "blue", lwd=2)
    segments(i-1, run2$mean.dprop[i-1], i, run2$mean.dprop[i], col = "red", lwd=2)
    segments(i-1, run3$mean.dprop[i-1], i, run3$mean.dprop[i], col = "green", lwd=2)
  }
}
legend(x = 60, y = .44, legend = c("10% Leaders","20% Leaders","30% Leaders"), fill = c("blue","red","green"))

# Mean For-Repertoire Size
for(i in 1:length(run3$pct.for)){
  if(i == 1){
    plot(-100, -100, xlim=c(1,100), ylim=c(3,10), ylab="Value", xlab="Iteration", type="n", cex.axis=0.8, main = "Mean For-Repertoire Size")
  }else{
    segments(i-1, run1$mean.prepsize[i-1], i, run1$mean.prepsize[i], col = "blue", lwd=2)
    segments(i-1, run2$mean.prepsize[i-1], i, run2$mean.prepsize[i], col = "red", lwd=2)
    segments(i-1, run3$mean.prepsize[i-1], i, run3$mean.prepsize[i], col = "green", lwd=2)
  }
}

# Mean Against-Repertoire Size
for(i in 1:length(run3$pct.for)){
  if(i == 1){
    plot(-100, -100, xlim=c(1,100), ylim=c(1,3), ylab="Value", xlab="Iteration", type="n", cex.axis=0.8, main = "Mean Against-Repertoire Size")
  }else{
    segments(i-1, run1$mean.orepsize[i-1], i, run1$mean.orepsize[i], col = "blue", lwd=2)
    segments(i-1, run2$mean.orepsize[i-1], i, run2$mean.orepsize[i], col = "red", lwd=2)
    segments(i-1, run3$mean.orepsize[i-1], i, run3$mean.orepsize[i], col = "green", lwd=2)
  }
}

# Number deliberating each round
for(i in 1:length(run3$pct.for)){
  if(i == 1){
    plot(-100, -100, xlim=c(1,100), ylim=c(100,300), ylab="Number", xlab="Iteration", type="n", cex.axis=0.8, main = "Number Deliberating Each Round")
  }else{
    segments(i-1, run1$num.delib[i-1], i, run1$num.delib[i], col = "blue", lwd=2)
    segments(i-1, run2$num.delib[i-1], i, run2$num.delib[i], col = "red", lwd=2)
    segments(i-1, run3$num.delib[i-1], i, run3$num.delib[i], col = "green", lwd=2)
  }
}

# Flips each round
for(i in 1:length(run3$pct.for)){
  if(i == 1){
    plot(-100, -100, xlim=c(1,100), ylim=c(0,10), ylab="Number", xlab="Iteration", type="n", cex.axis=0.8, main = "Number of Position Changes Each Round")
  }else{
    segments(i-1, run1$flips[i-1], i, run1$flips[i], col = "blue", lwd=2)
    segments(i-1, run2$flips[i-1], i, run2$flips[i], col = "red", lwd=2)
    segments(i-1, run3$flips[i-1], i, run3$flips[i], col = "green", lwd=2)
  }
}
