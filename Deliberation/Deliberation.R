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

initg <- do.delibspace()
agents <- initg[[1]]
agents1g <- agents
argspace <- initg[[2]]
argspace1g <- argspace
run1g <- deliberate(100, interaction.rule = "global")
agents1gc <- agents
argspace1gc <- argspace

init2l <- do.delibspace(olead.dens = .2)
agents <- init2l[[1]]
agents2l <- agents
argspace <- init2l[[2]]
argspace2l <- argspace
run2l <- deliberate(100, interaction.rule = "local")
agents2lc <- agents
argspace2lc <- argspace

init2g <- do.delibspace(olead.dens = .2)
agents <- init2g[[1]]
agents2g <- agents
argspace <- init2g[[2]]
argspace2g <- argspace
run2g <- deliberate(100, interaction.rule = "global")
agents2gc <- agents
argspace2gc <- argspace

init3l <- do.delibspace(olead.dens = .2, polarization = .3, base.dprop = .5, lead.dprop = .8)
agents <- init3l[[1]]
agents3l <- agents
argspace <- init3l[[2]]
argspace3l <- argspace
run3l <- deliberate(100, interaction.rule = "local")
agents3lc <- agents
argspace3lc <- argspace

init3g <- do.delibspace(olead.dens = .2, polarization = .3, base.dprop = .5, lead.dprop = .8)
agents <- init3g[[1]]
agents3g <- agents
argspace <- init3g[[2]]
argspace3g <- argspace
run3g <- deliberate(100, interaction.rule = "global")
agents3gc <- agents
argspace3gc <- argspace

init4l <- do.delibspace(olead.dens = 0)
agents <- init4l[[1]]
agents4l <- agents
argspace <- init4l[[2]]
argspace4l <- argspace
run4l <- deliberate(100, interaction.rule = "local")
agents4lc <- agents
argspace4lc <- argspace

init4g <- do.delibspace(olead.dens = 0)
agents <- init4g[[1]]
agents4g <- agents
argspace <- init4g[[2]]
argspace4g <- argspace
run4g <- deliberate(100, interaction.rule = "global")
agents4gc <- agents
argspace4gc <- argspace

init5l <- do.delibspace(pct.for = .7)
agents <- init5l[[1]]
agents5l <- agents
argspace <- init5l[[2]]
argspace5l <- argspace
run5l <- deliberate(100, interaction.rule = "local")
agents5lc <- agents
argspace5lc <- argspace

init5g <- do.delibspace(pct.for = .7)
agents <- init5g[[1]]
agents5g <- agents
argspace <- init5g[[2]]
argspace5g <- argspace
run5g <- deliberate(100, interaction.rule = "global")
agents5gc <- agents
argspace5gc <- argspace

plot(density(agents1l$dprop), main = "Initial Propensity to Deliberate (Local Vision Rule)", xlab = "Value", ylab = "Density")
plot(density(agents1lc$dprop), main = "Propensity to Deliberate after 100 Rounds (Local Vision Rule)", xlab = "Value", ylab = "Density")
plot(density(agents1g$dprop), main = "Initial Propensity to Deliberate (Global Vision Rule)", xlab = "Value", ylab = "Density")
plot(density(agents1gc$dprop), main = "Propensity to Deliberate after 100 Rounds (Global Vision Rule)", xlab = "Value", ylab = "Density")

plot(density(agents2l$dprop), main = "Initial Propensity to Deliberate (Local Vision Rule)", xlab = "Value", ylab = "Density")
plot(density(agents2lc$dprop), main = "Propensity to Deliberate after 100 Rounds (Local Vision Rule)", xlab = "Value", ylab = "Density")
plot(density(agents2g$dprop), main = "Initial Propensity to Deliberate (Global Vision Rule)", xlab = "Value", ylab = "Density")
plot(density(agents2gc$dprop), main = "Propensity to Deliberate after 100 Rounds (Global Vision Rule)", xlab = "Value", ylab = "Density")

plot(density(agents1l$dqual), main = "Initial Deliberative Skill (Local Vision Rule)", xlab = "Value", ylab = "Density")
plot(density(agents1lc$dqual), main = "Deliberative Skill after 100 Rounds (Local Vision Rule)", xlab = "Value", ylab = "Density")
plot(density(agents1g$dqual), main = "Initial Deliberative Skill (Global Vision Rule)", xlab = "Value", ylab = "Density")
plot(density(agents1gc$dqual), main = "Deliberative Skill after 100 Rounds (Global Vision Rule)", xlab = "Value", ylab = "Density")

plot(density(agents2l$dqual), main = "Initial Deliberative Skill (Local Vision Rule)", xlab = "Value", ylab = "Density")
plot(density(agents2lc$dqual), main = "Deliberative Skill after 100 Rounds (Local Vision Rule)", xlab = "Value", ylab = "Density")
plot(density(agents2g$dqual), main = "Initial Deliberative Skill (Global Vision Rule)", xlab = "Value", ylab = "Density")
plot(density(agents2gc$dqual), main = "Deliberative Skill after 100 Rounds (Global Vision Rule)", xlab = "Value", ylab = "Density")

plot(density(agents1l$p.conf), main = "Initial Position Confidence (Local Vision Rule)", xlab = "Value", ylab = "Density")
plot(density(agents1lc$p.conf), main = "Position Confidence after 100 Rounds (Local Vision Rule)", xlab = "Value", ylab = "Density")
plot(density(agents1g$p.conf), main = "Initial Position Confidence (Global Vision Rule)", xlab = "Value", ylab = "Density")
plot(density(agents1gc$p.conf), main = "Position Confidence after 100 Rounds (Global Vision Rule)", xlab = "Value", ylab = "Density")

plot(density(agents2l$p.conf), main = "Initial Position Confidence (Local Vision Rule)", xlab = "Value", ylab = "Density")
plot(density(agents2lc$p.conf), main = "Position Confidence after 100 Rounds (Local Vision Rule)", xlab = "Value", ylab = "Density")
plot(density(agents2g$p.conf), main = "Initial Position Confidence (Global Vision Rule)", xlab = "Value", ylab = "Density")
plot(density(agents2gc$p.conf), main = "Position Confidence after 100 Rounds (Global Vision Rule)", xlab = "Value", ylab = "Density")

plot(density(agents1l$o.repsize), main = "Initial Against-Repertoire Size (Local Vision Rule)", xlab = "Arguments in Against-Repertoire", ylab = "Density")
plot(density(agents1lc$o.repsize), main = "Against-Repertoire Size after 100 Rounds (Local Vision Rule)", xlab = "Arguments in Against-Repertoire", ylab = "Density")
plot(density(agents1g$o.repsize), main = "Initial Against-Repertoire Size (Global Vision Rule)", xlab = "Arguments in Against-Repertoire", ylab = "Density")
plot(density(agents1gc$o.repsize), main = "Against-Repertoire Size after 100 Rounds (Global Vision Rule)", xlab = "Arguments in Against-Repertoire", ylab = "Density")

plot(density(agents2l$o.repsize), main = "Initial Against-Repertoire Size (Local Vision Rule)", xlab = "Arguments in Against-Repertoire", ylab = "Density")
plot(density(agents2lc$o.repsize), main = "Against-Repertoire Size after 100 Rounds (Local Vision Rule)", xlab = "Arguments in Against-Repertoire", ylab = "Density")
plot(density(agents2g$o.repsize), main = "Initial Against-Repertoire Size (Global Vision Rule)", xlab = "Arguments in Against-Repertoire", ylab = "Density")
plot(density(agents2gc$o.repsize), main = "Against-Repertoire Size after 100 Rounds (Global Vision Rule)", xlab = "Arguments in Against-Repertoire", ylab = "Density")

plot(density(agents1l$p.repsize), main = "Initial For-Repertoire Size (Local Vision Rule)", xlab = "Arguments in For-Repertoire", ylab = "Density")
plot(density(agents1lc$p.repsize), main = "For-Repertoire Size after 100 Rounds (Local Vision Rule)", xlab = "Arguments in For-Repertoire", ylab = "Density")
plot(density(agents1g$p.repsize), main = "Initial For-Repertoire Size (Global Vision Rule)", xlab = "Arguments in For-Repertoire", ylab = "Density")
plot(density(agents1gc$p.repsize), main = "For-Repertoire Size after 100 Rounds (Global Vision Rule)", xlab = "Arguments in For-Repertoire", ylab = "Density")

plot(density(agents2l$p.repsize), main = "Initial For-Repertoire Size (Local Vision Rule)", xlab = "Arguments in For-Repertoire", ylab = "Density")
plot(density(agents2lc$p.repsize), main = "For-Repertoire Size after 100 Rounds (Local Vision Rule)", xlab = "Arguments in For-Repertoire", ylab = "Density")
plot(density(agents2g$p.repsize), main = "Initial For-Repertoire Size (Global Vision Rule)", xlab = "Arguments in For-Repertoire", ylab = "Density")
plot(density(agents2gc$p.repsize), main = "For-Repertoire Size after 100 Rounds (Global Vision Rule)", xlab = "Arguments in For-Repertoire", ylab = "Density")

plotDelib(title = "Initial Deliberative Space (Local Vision Rule)", dat = agents1l, view = "p.repsize")
plotDelib(title = "Deliberative Space after 100 Rounds (Local Vision Rule)", dat = agents1lc, view = "p.repsize")
plotDelib(title = "Initial Deliberative Space (Global Vision Rule)", dat = agents1g, view = "p.repsize")
plotDelib(title = "Deliberative Space after 100 Rounds (Global Vision Rule)", dat = agents1gc, view = "p.repsize")

plotDelib(title = "Initial Deliberative Space (Local Vision Rule)", dat = agents2l, view = "p.repsize")
plotDelib(title = "Deliberative Space after 100 Rounds (Local Vision Rule)", dat = agents2lc, view = "p.repsize")
plotDelib(title = "Initial Deliberative Space (Global Vision Rule)", dat = agents2g, view = "p.repsize")
plotDelib(title = "Deliberative Space after 100 Rounds (Global Vision Rule)", dat = agents2gc, view = "p.repsize")

plotDelib(title = "Initial Deliberative Space (Local Vision Rule)", dat = agents1l, view = "position")
plotDelib(title = "Initial Deliberative Space (Local Vision Rule)", dat = agents1lc, view = "position")
table(agents1l$position)
table(agents1lc$position)
plotDelib(title = "Initial Deliberative Space (Local Vision Rule)", dat = agents1g, view = "position")
plotDelib(title = "Initial Deliberative Space (Local Vision Rule)", dat = agents1gc, view = "position")
table(agents1g$position)
table(agents1gc$position)

plotDelib(title = "Initial Deliberative Space (Local Vision Rule)", dat = agents2l, view = "position")
plotDelib(title = "Initial Deliberative Space (Local Vision Rule)", dat = agents2lc, view = "position")
table(agents2l$position)
table(agents2lc$position)
plotDelib(title = "Initial Deliberative Space (Local Vision Rule)", dat = agents2g, view = "position")
plotDelib(title = "Initial Deliberative Space (Local Vision Rule)", dat = agents2gc, view = "position")
table(agents2g$position)
table(agents2gc$position)

plotDelib(title = "Initial Deliberative Space (Local Vision Rule)", dat = agents1l, view = "o.repsize")
plotDelib(title = "Deliberative Space after 100 Rounds (Local Vision Rule)", dat = agents1lc, view = "o.repsize")
plotDelib(title = "Initial Deliberative Space (Global Vision Rule)", dat = agents1g, view = "o.repsize")
plotDelib(title = "Deliberative Space after 100 Rounds (Global Vision Rule)", dat = agents1gc, view = "o.repsize")

plotDelib(title = "Initial Deliberative Space (Local Vision Rule)", dat = agents2l, view = "o.repsize")
plotDelib(title = "Deliberative Space after 100 Rounds (Local Vision Rule)", dat = agents2lc, view = "o.repsize")
plotDelib(title = "Initial Deliberative Space (Global Vision Rule)", dat = agents2g, view = "o.repsize")
plotDelib(title = "Deliberative Space after 100 Rounds (Global Vision Rule)", dat = agents2gc, view = "o.repsize")

plotDelib(title = "Initial Deliberative Space (Local Vision Rule)", dat = agents1l, view = "dqual")
plotDelib(title = "Deliberative Space after 100 Rounds (Local Vision Rule)", dat = agents1lc, view = "dqual")
plotDelib(title = "Initial Deliberative Space (Global Vision Rule)", dat = agents1g, view = "dqual")
plotDelib(title = "Deliberative Space after 100 Rounds (Global Vision Rule)", dat = agents1gc, view = "dqual")

plotDelib(title = "Initial Deliberative Space (Local Vision Rule)", dat = agents2l, view = "dqual")
plotDelib(title = "Deliberative Space after 100 Rounds (Local Vision Rule)", dat = agents2lc, view = "dqual")
plotDelib(title = "Initial Deliberative Space (Global Vision Rule)", dat = agents2g, view = "dqual")
plotDelib(title = "Deliberative Space after 100 Rounds (Global Vision Rule)", dat = agents2gc, view = "dqual")

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
    plot(-100, -100, xlim=c(1,100), ylim=c(.65,.9), ylab="Value", xlab="Iteration", type="n", cex.axis=0.8, main = "Position Confidence")
  }else{
    segments(i-1, run1l$mean.pconf[i-1], i, run1l$mean.pconf[i], col = "pink", lwd=2)
    segments(i-1, run1g$mean.pconf[i-1], i, run1g$mean.pconf[i], col = "red", lwd=2)
    segments(i-1, run2l$mean.pconf[i-1], i, run2l$mean.pconf[i], col = "lightblue", lwd=2)
    segments(i-1, run2g$mean.pconf[i-1], i, run2g$mean.pconf[i], col = "blue", lwd=2)
    segments(i-1, run3l$mean.pconf[i-1], i, run3l$mean.pconf[i], col = "green", lwd=2)
    segments(i-1, run3g$mean.pconf[i-1], i, run3g$mean.pconf[i], col = "darkgreen", lwd=2)
    segments(i-1, run4l$mean.pconf[i-1], i, run4l$mean.pconf[i], col = "grey", lwd=2)
    segments(i-1, run4g$mean.pconf[i-1], i, run4g$mean.pconf[i], col = "black", lwd=2)
    segments(i-1, run5l$mean.pconf[i-1], i, run5l$mean.pconf[i], col = "tan", lwd=2)
    segments(i-1, run5g$mean.pconf[i-1], i, run5g$mean.pconf[i], col = "brown", lwd=2)
  }
}
legend(x = 0, y = .905, 
       legend = c("Local Vision Rule (10% Leaders)","Global Vision Rule (10% Leaders)",
                  "Local Vision Rule (20% Leaders)","Global Vision Rule (20% Leaders)",
                  "Local Vision Rule (20% Leaders/High Pol+Prop)","Global Vision Rule (20% Leaders/High Pol+Prop)"), 
       fill = c("pink","red","lightblue","blue","green","darkgreen"))

# Percent in favor
for(i in 1:length(run1l$pct.for)){
  if(i == 1){
    plot(-100, -100, xlim=c(1,100), ylim=c(.45, .8), ylab="Percent", xlab="Iteration", type="n", cex.axis=0.8, main = "Percent in Favor")
  }else{
    segments(i-1, run1l$pct.for[i-1], i, run1l$pct.for[i], col = "pink", lwd=2)
    segments(i-1, run1g$pct.for[i-1], i, run1g$pct.for[i], col = "red", lwd=2)
    segments(i-1, run2l$pct.for[i-1], i, run2l$pct.for[i], col = "lightblue", lwd=2)
    segments(i-1, run2g$pct.for[i-1], i, run2g$pct.for[i], col = "blue", lwd=2)
    segments(i-1, run3l$pct.for[i-1], i, run3l$pct.for[i], col = "green", lwd=2)
    segments(i-1, run3g$pct.for[i-1], i, run3g$pct.for[i], col = "darkgreen", lwd=2)
    segments(i-1, run4l$pct.for[i-1], i, run4l$pct.for[i], col = "grey", lwd=2)
    segments(i-1, run4g$pct.for[i-1], i, run4g$pct.for[i], col = "black", lwd=2)
    segments(i-1, run5l$pct.for[i-1], i, run5l$pct.for[i], col = "tan", lwd=2)
    segments(i-1, run5g$pct.for[i-1], i, run5g$pct.for[i], col = "brown", lwd=2)
  }
}

# Mean Deliberative Quality
for(i in 1:length(run1l$mean.dqual)){
  if(i == 1){
    plot(-100, -100, xlim=c(1,100), ylim=c(.4,.7), ylab="Value", xlab="Iteration", type="n", cex.axis=0.8, main = "Mean Deliberative Quality")
  }else{
    segments(i-1, run1l$mean.dqual[i-1], i, run1l$mean.dqual[i], col = "pink", lwd=2)
    segments(i-1, run1g$mean.dqual[i-1], i, run1g$mean.dqual[i], col = "red", lwd=2)
    segments(i-1, run2l$mean.dqual[i-1], i, run2l$mean.dqual[i], col = "lightblue", lwd=2)
    segments(i-1, run2g$mean.dqual[i-1], i, run2g$mean.dqual[i], col = "blue", lwd=2)
    segments(i-1, run3l$mean.dqual[i-1], i, run3l$mean.dqual[i], col = "green", lwd=2)
    segments(i-1, run3g$mean.dqual[i-1], i, run3g$mean.dqual[i], col = "darkgreen", lwd=2)
    segments(i-1, run4l$mean.dqual[i-1], i, run4l$mean.dqual[i], col = "grey", lwd=2)
    segments(i-1, run4g$mean.dqual[i-1], i, run4g$mean.dqual[i], col = "black", lwd=2)
    segments(i-1, run5l$mean.dqual[i-1], i, run5l$mean.dqual[i], col = "tan", lwd=2)
    segments(i-1, run5g$mean.dqual[i-1], i, run5g$mean.dqual[i], col = "brown", lwd=2)
  }
}

# Mean Propensity to Deliberate
for(i in 1:length(run1l$mean.dprop)){
  if(i == 1){
    plot(-100, -100, xlim=c(1,100), ylim=c(.3, .7), ylab="Value", xlab="Round", type="n", cex.axis=0.8, main = "Mean Propensity to Deliberate")
  }else{
    segments(i-1, run1l$mean.dprop[i-1], i, run1l$mean.dprop[i], col = "pink", lwd=2)
    segments(i-1, run1g$mean.dprop[i-1], i, run1g$mean.dprop[i], col = "red", lwd=2)
    segments(i-1, run2l$mean.dprop[i-1], i, run2l$mean.dprop[i], col = "lightblue", lwd=2)
    segments(i-1, run2g$mean.dprop[i-1], i, run2g$mean.dprop[i], col = "blue", lwd=2)
    segments(i-1, run3l$mean.dprop[i-1], i, run3l$mean.dprop[i], col = "green", lwd=2)
    segments(i-1, run3g$mean.dprop[i-1], i, run3g$mean.dprop[i], col = "darkgreen", lwd=2)
    segments(i-1, run4l$mean.dprop[i-1], i, run4l$mean.dprop[i], col = "grey", lwd=2)
    segments(i-1, run4g$mean.dprop[i-1], i, run4g$mean.dprop[i], col = "black", lwd=2)
    segments(i-1, run5l$mean.dprop[i-1], i, run5l$mean.dprop[i], col = "tan", lwd=2)
    segments(i-1, run5g$mean.dprop[i-1], i, run5g$mean.dprop[i], col = "brown", lwd=2)
  }
}
legend(x = 0, y = .68, 
       legend = c("Local Vision Rule (10% Leaders)","Global Vision Rule (10% Leaders)",
                  "Local Vision Rule (20% Leaders)","Global Vision Rule (20% Leaders)",
                  "Local Vision Rule (20% Leaders/High Pol+Prop)","Global Vision Rule (20% Leaders/High Pol+Prop)"), 
       fill = c("pink","red","lightblue","blue","green","darkgreen"))

# Mean For-Repertoire Size
for(i in 1:length(run1l$mean.prepsize)){
  if(i == 1){
    plot(-100, -100, xlim=c(1,100), ylim=c(3,10), ylab="Value", xlab="Iteration", type="n", cex.axis=0.8, main = "Mean For-Repertoire Size")
  }else{
    segments(i-1, run1l$mean.prepsize[i-1], i, run1l$mean.prepsize[i], col = "pink", lwd=2)
    segments(i-1, run1g$mean.prepsize[i-1], i, run1g$mean.prepsize[i], col = "red", lwd=2)
    segments(i-1, run2l$mean.prepsize[i-1], i, run2l$mean.prepsize[i], col = "lightblue", lwd=2)
    segments(i-1, run2g$mean.prepsize[i-1], i, run2g$mean.prepsize[i], col = "blue", lwd=2)
    segments(i-1, run3l$mean.prepsize[i-1], i, run3l$mean.prepsize[i], col = "green", lwd=2)
    segments(i-1, run3g$mean.prepsize[i-1], i, run3g$mean.prepsize[i], col = "darkgreen", lwd=2)
    segments(i-1, run4l$mean.prepsize[i-1], i, run4l$mean.prepsize[i], col = "grey", lwd=2)
    segments(i-1, run4g$mean.prepsize[i-1], i, run4g$mean.prepsize[i], col = "black", lwd=2)
    segments(i-1, run5l$mean.prepsize[i-1], i, run5l$mean.prepsize[i], col = "tan", lwd=2)
    segments(i-1, run5g$mean.prepsize[i-1], i, run5g$mean.prepsize[i], col = "brown", lwd=2)
  }
}
legend(x = 40, y = 4.5, 
       legend = c("Local Vision Rule (10% Leaders)","Global Vision Rule (10% Leaders)",
                  "Local Vision Rule (20% Leaders)","Global Vision Rule (20% Leaders)",
                  "Local Vision Rule (20% Leaders/High Pol+Prop)","Global Vision Rule (20% Leaders/High Pol+Prop)"), 
       fill = c("pink","red","lightblue","blue","green","darkgreen"))

# Mean Against-Repertoire Size
for(i in 1:length(run1l$mean.orepsize)){
  if(i == 1){
    plot(-100, -100, xlim=c(1,100), ylim=c(1,3), ylab="Value", xlab="Iteration", type="n", cex.axis=0.8, main = "Mean For-Repertoire Size")
  }else{
    segments(i-1, run1l$mean.orepsize[i-1], i, run1l$mean.orepsize[i], col = "pink", lwd=2)
    segments(i-1, run1g$mean.orepsize[i-1], i, run1g$mean.orepsize[i], col = "red", lwd=2)
    segments(i-1, run2l$mean.orepsize[i-1], i, run2l$mean.orepsize[i], col = "lightblue", lwd=2)
    segments(i-1, run2g$mean.orepsize[i-1], i, run2g$mean.orepsize[i], col = "blue", lwd=2)
    segments(i-1, run3l$mean.orepsize[i-1], i, run3l$mean.orepsize[i], col = "green", lwd=2)
    segments(i-1, run3g$mean.orepsize[i-1], i, run3g$mean.orepsize[i], col = "darkgreen", lwd=2)
    segments(i-1, run4l$mean.orepsize[i-1], i, run4l$mean.orepsize[i], col = "grey", lwd=2)
    segments(i-1, run4g$mean.orepsize[i-1], i, run4g$mean.orepsize[i], col = "black", lwd=2)
    segments(i-1, run5l$mean.orepsize[i-1], i, run5l$mean.orepsize[i], col = "tan", lwd=2)
    segments(i-1, run5g$mean.orepsize[i-1], i, run5g$mean.orepsize[i], col = "brown", lwd=2)
  }
}

# Number deliberating each round
for(i in 1:length(run1l$num.delib)){
  if(i == 1){
    plot(-100, -100, xlim=c(1,100), ylim=c(100,300), ylab="Number", xlab="Iteration", type="n", cex.axis=0.8, main = "Number Deliberating Each Round")
  }else{
    segments(i-1, run1l$num.delib[i-1], i, run1l$num.delib[i], col = "pink", lwd=2)
    segments(i-1, run1g$num.delib[i-1], i, run1g$num.delib[i], col = "red", lwd=2)
    segments(i-1, run2l$num.delib[i-1], i, run2l$num.delib[i], col = "lightblue", lwd=2)
    segments(i-1, run2g$num.delib[i-1], i, run2g$num.delib[i], col = "blue", lwd=2)
    segments(i-1, run3l$num.delib[i-1], i, run3l$num.delib[i], col = "green", lwd=2)
    segments(i-1, run3g$num.delib[i-1], i, run3g$num.delib[i], col = "darkgreen", lwd=2)
    segments(i-1, run4l$num.delib[i-1], i, run4l$num.delib[i], col = "grey", lwd=2)
    segments(i-1, run4g$num.delib[i-1], i, run4g$num.delib[i], col = "black", lwd=2)
    segments(i-1, run5l$num.delib[i-1], i, run5l$num.delib[i], col = "tan", lwd=2)
    segments(i-1, run5g$num.delib[i-1], i, run5g$num.delib[i], col = "brown", lwd=2)
  }
}

# Flips each round
for(i in 1:length(run1l$flips)){
  if(i == 1){
    plot(-100, -100, xlim=c(1,100), ylim=c(0,10), ylab="Number", xlab="Iteration", type="n", cex.axis=0.8, main = "Number of Position Changes Each Round")
  }else{
    segments(i-1, run1l$flips[i-1], i, run1l$flips[i], col = "pink", lwd=2)
    segments(i-1, run1g$flips[i-1], i, run1g$flips[i], col = "red", lwd=2)
    segments(i-1, run2l$flips[i-1], i, run2l$flips[i], col = "lightblue", lwd=2)
    segments(i-1, run2g$flips[i-1], i, run2g$flips[i], col = "blue", lwd=2)
    segments(i-1, run3l$flips[i-1], i, run3l$flips[i], col = "green", lwd=2)
    segments(i-1, run3g$flips[i-1], i, run3g$flips[i], col = "darkgreen", lwd=2)
    segments(i-1, run4l$flips[i-1], i, run4l$flips[i], col = "grey", lwd=2)
    segments(i-1, run4g$flips[i-1], i, run4g$flips[i], col = "black", lwd=2)
    segments(i-1, run5l$flips[i-1], i, run5l$flips[i], col = "tan", lwd=2)
    segments(i-1, run5g$flips[i-1], i, run5g$flips[i], col = "brown", lwd=2)
  }
}

# Tension each round
for(i in 1:length(run1l$tension)){
  if(i == 1){
    plot(-100, -100, xlim=c(1,100), ylim=c(0,.7), ylab="Tension Level", xlab="Iteration", type="n", cex.axis=0.8, main = "Tension Level Each Round")
  }else{
    segments(i-1, run1l$tension[i-1], i, run1l$tension[i], col = "pink", lwd=2)
    segments(i-1, run1g$tension[i-1], i, run1g$tension[i], col = "red", lwd=2)
    segments(i-1, run2l$tension[i-1], i, run2l$tension[i], col = "lightblue", lwd=2)
    segments(i-1, run2g$tension[i-1], i, run2g$tension[i], col = "blue", lwd=2)
    segments(i-1, run3l$tension[i-1], i, run3l$tension[i], col = "green", lwd=2)
    segments(i-1, run3g$tension[i-1], i, run3g$tension[i], col = "darkgreen", lwd=2)
    segments(i-1, run4l$tension[i-1], i, run4l$tension[i], col = "grey", lwd=2)
    segments(i-1, run4g$tension[i-1], i, run4g$tension[i], col = "black", lwd=2)
    segments(i-1, run5l$tension[i-1], i, run5l$tension[i], col = "tan", lwd=2)
    segments(i-1, run5g$tension[i-1], i, run5g$tension[i], col = "brown", lwd=2)
  }
}
legend(x = 10, y = .2, 
       legend = c("Local Vision Rule (10% Leaders)","Global Vision Rule (10% Leaders)",
                  "Local Vision Rule (20% Leaders)","Global Vision Rule (20% Leaders)"), 
       fill = c("lightblue","pink","blue","red"))


save(agents1l, agents1g, agents1lc, agents1gc, agents2l, agents2g, agents2lc, agents2gc, agents3l, agents3g, agents3lc, agents3gc,
     agents4l, agents4g, agents4lc, agents4gc, agents5l, agents5g, agents5lc, agents5gc,
     argspace1l, argspace1g, argspace1lc, argspace1gc, argspace2l, argspace2g, argspace2lc, argspace2gc, 
     argspace3l, argspace3g, argspace3lc, argspace3gc, argspace4l, argspace4g, argspace4lc, argspace4gc,
     argspace5l, argspace5g, argspace5lc, argspace5gc, initl, initg, init2l, init2g, init3l, init3g,
     init4l, init4g, init5l, init5g, run1l, run1g, run2l, run2g, run3l, run3g, 
     run4l, run4g, run5l, run5g, file = "delib_runs.RData")
