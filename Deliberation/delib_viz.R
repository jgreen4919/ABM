# Deliberation Viz
source("~/Desktop/ABM/Deliberation/delib_functions.R")
setwd("~/Desktop/ABM/Deliberation/Viz")
load("delib_runs.RData")

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
legend(x = 20, y = .7, 
       legend = c("Default Local","20% Leaders Local", "High Prop Local", 
                  "0% Leaders Local", "70% Init. For Local",
                  "Default Global","20% Leaders Global", "High Prop Global", 
                  "0% Leaders Global", "70% Init. For Global"), 
       fill = c("pink","lightblue","green","grey","tan",
                "red","blue","darkgreen","black","brown"),
       ncol = 2)

# Percent in favor
for(i in 1:length(run1l_agg$pct.for)){
  if(i == 1){
    plot(-100, -100, xlim=c(1,100), ylim=c(.45, .8), ylab="Percent", xlab="Iteration", type="n", cex.axis=0.8, main = "Percent in Favor")
  }else{
    segments(i-1, run1l_agg$pct.for[i-1], i, run1l_agg$pct.for[i], col = "pink", lwd=2)
    segments(i-1, run1g_agg$pct.for[i-1], i, run1g_agg$pct.for[i], col = "red", lwd=2)
    segments(i-1, run2l_agg$pct.for[i-1], i, run2l_agg$pct.for[i], col = "lightblue", lwd=2)
    segments(i-1, run2g_agg$pct.for[i-1], i, run2g_agg$pct.for[i], col = "blue", lwd=2)
    segments(i-1, run3l_agg$pct.for[i-1], i, run3l_agg$pct.for[i], col = "green", lwd=2)
    segments(i-1, run3g_agg$pct.for[i-1], i, run3g_agg$pct.for[i], col = "darkgreen", lwd=2)
    segments(i-1, run4l_agg$pct.for[i-1], i, run4l_agg$pct.for[i], col = "grey", lwd=2)
    segments(i-1, run4g_agg$pct.for[i-1], i, run4g_agg$pct.for[i], col = "black", lwd=2)
    segments(i-1, run5l_agg$pct.for[i-1], i, run5l_agg$pct.for[i], col = "tan", lwd=2)
    segments(i-1, run5g_agg$pct.for[i-1], i, run5g_agg$pct.for[i], col = "brown", lwd=2)
  }
}

# Mean Deliberative Quality
for(i in 1:length(run1l_agg$mean.dqual)){
  if(i == 1){
    plot(-100, -100, xlim=c(1,100), ylim=c(.3, .7), ylab="Percent", xlab="Iteration", type="n", cex.axis=0.8, main = "Mean Deliberative Quality")
  }else{
    segments(i-1, run1l_agg$mean.dqual[i-1], i, run1l_agg$mean.dqual[i], col = "pink", lwd=2)
    segments(i-1, run1g_agg$mean.dqual[i-1], i, run1g_agg$mean.dqual[i], col = "red", lwd=2)
    segments(i-1, run2l_agg$mean.dqual[i-1], i, run2l_agg$mean.dqual[i], col = "lightblue", lwd=2)
    segments(i-1, run2g_agg$mean.dqual[i-1], i, run2g_agg$mean.dqual[i], col = "blue", lwd=2)
    segments(i-1, run3l_agg$mean.dqual[i-1], i, run3l_agg$mean.dqual[i], col = "green", lwd=2)
    segments(i-1, run3g_agg$mean.dqual[i-1], i, run3g_agg$mean.dqual[i], col = "darkgreen", lwd=2)
    segments(i-1, run4l_agg$mean.dqual[i-1], i, run4l_agg$mean.dqual[i], col = "grey", lwd=2)
    segments(i-1, run4g_agg$mean.dqual[i-1], i, run4g_agg$mean.dqual[i], col = "black", lwd=2)
    segments(i-1, run5l_agg$mean.dqual[i-1], i, run5l_agg$mean.dqual[i], col = "tan", lwd=2)
    segments(i-1, run5g_agg$mean.dqual[i-1], i, run5g_agg$mean.dqual[i], col = "brown", lwd=2)
  }
}
legend(x = 20, y = .4, 
       legend = c("Default Local","20% Leaders Local", "High Prop Local", 
                  "0% Leaders Local", "70% Init. For Local",
                  "Default Global","20% Leaders Global", "High Prop Global", 
                  "0% Leaders Global", "70% Init. For Global"), 
       fill = c("pink","lightblue","green","grey","tan",
                "red","blue","darkgreen","black","brown"),
       ncol = 2)

# Mean Propensity to Deliberate
for(i in 1:length(run1l$mean.dprop)){
  if(i == 1){
    plot(-100, -100, xlim=c(1,100), ylim=c(.2, .6), ylab="Value", xlab="Round", type="n", cex.axis=0.8, main = "Mean Propensity to Deliberate")
  }else{
    segments(i-1, run1l_agg$mean.dprop[i-1], i, run1l_agg$mean.dprop[i], col = "pink", lwd=2)
    segments(i-1, run2l_agg$mean.dprop[i-1], i, run2l_agg$mean.dprop[i], col = "lightblue", lwd=2)
    segments(i-1, run3l_agg$mean.dprop[i-1], i, run3l_agg$mean.dprop[i], col = "green", lwd=2)
    segments(i-1, run4l_agg$mean.dprop[i-1], i, run4l_agg$mean.dprop[i], col = "grey", lwd=2)
    segments(i-1, run5l_agg$mean.dprop[i-1], i, run5l_agg$mean.dprop[i], col = "tan", lwd=2)
    segments(i-1, run1g_agg$mean.dprop[i-1], i, run1g_agg$mean.dprop[i], col = "red", lwd=2)
    segments(i-1, run2g_agg$mean.dprop[i-1], i, run2g_agg$mean.dprop[i], col = "blue", lwd=2)
    segments(i-1, run3g_agg$mean.dprop[i-1], i, run3g_agg$mean.dprop[i], col = "darkgreen", lwd=2)
    segments(i-1, run4g_agg$mean.dprop[i-1], i, run4g_agg$mean.dprop[i], col = "black", lwd=2)
    segments(i-1, run5g_agg$mean.dprop[i-1], i, run5g_agg$mean.dprop[i], col = "brown", lwd=2)
  }
}
legend(x = 20, y = .3, 
       legend = c("Default Local","20% Leaders Local", "High Prop Local", 
                  "0% Leaders Local", "70% Init. For Local",
                  "Default Global","20% Leaders Global", "High Prop Global", 
                  "0% Leaders Global", "70% Init. For Global"), 
       fill = c("pink","lightblue","green","grey","tan",
                "red","blue","darkgreen","black","brown"),
       ncol = 2)

# Mean For-Repertoire Size
for(i in 1:length(run1l$mean.prepsize)){
  if(i == 1){
    plot(-100, -100, xlim=c(1,100), ylim=c(3,10), ylab="Value", xlab="Iteration", type="n", cex.axis=0.8, main = "Mean For-Repertoire Size")
  }else{
    segments(i-1, run1l_agg$mean.prepsize[i-1], i, run1l_agg$mean.prepsize[i], col = "pink", lwd=2)
    segments(i-1, run1g_agg$mean.prepsize[i-1], i, run1g_agg$mean.prepsize[i], col = "red", lwd=2)
    segments(i-1, run2l_agg$mean.prepsize[i-1], i, run2l_agg$mean.prepsize[i], col = "lightblue", lwd=2)
    segments(i-1, run2g_agg$mean.prepsize[i-1], i, run2g_agg$mean.prepsize[i], col = "blue", lwd=2)
    segments(i-1, run3l_agg$mean.prepsize[i-1], i, run3l_agg$mean.prepsize[i], col = "green", lwd=2)
    segments(i-1, run3g_agg$mean.prepsize[i-1], i, run3g_agg$mean.prepsize[i], col = "darkgreen", lwd=2)
    segments(i-1, run4l_agg$mean.prepsize[i-1], i, run4l_agg$mean.prepsize[i], col = "grey", lwd=2)
    segments(i-1, run4g_agg$mean.prepsize[i-1], i, run4g_agg$mean.prepsize[i], col = "black", lwd=2)
    segments(i-1, run5l_agg$mean.prepsize[i-1], i, run5l_agg$mean.prepsize[i], col = "tan", lwd=2)
    segments(i-1, run5g_agg$mean.prepsize[i-1], i, run5g_agg$mean.prepsize[i], col = "brown", lwd=2)
  }
}
legend(x = 0, y = 9.5, 
       legend = c("Default Local","20% Leaders Local", "High Prop Local", 
                  "0% Leaders Local", "70% Init. For Local",
                  "Default Global","20% Leaders Global", "High Prop Global", 
                  "0% Leaders Global", "70% Init. For Global"), 
       fill = c("pink","lightblue","green","grey","tan",
                "red","blue","darkgreen","black","brown"),
       ncol = 2)

# Mean Against-Repertoire Size
for(i in 1:length(run1l$mean.orepsize)){
  if(i == 1){
    plot(-100, -100, xlim=c(1,100), ylim=c(1,3), ylab="Value", xlab="Iteration", type="n", cex.axis=0.8, main = "Mean Against-Repertoire Size")
  }else{
    segments(i-1, run1l_agg$mean.orepsize[i-1], i, run1l_agg$mean.orepsize[i], col = "pink", lwd=2)
    segments(i-1, run1g_agg$mean.orepsize[i-1], i, run1g_agg$mean.orepsize[i], col = "red", lwd=2)
    segments(i-1, run2l_agg$mean.orepsize[i-1], i, run2l_agg$mean.orepsize[i], col = "lightblue", lwd=2)
    segments(i-1, run2g_agg$mean.orepsize[i-1], i, run2g_agg$mean.orepsize[i], col = "blue", lwd=2)
    segments(i-1, run3l_agg$mean.orepsize[i-1], i, run3l_agg$mean.orepsize[i], col = "green", lwd=2)
    segments(i-1, run3g_agg$mean.orepsize[i-1], i, run3g_agg$mean.orepsize[i], col = "darkgreen", lwd=2)
    segments(i-1, run4l_agg$mean.orepsize[i-1], i, run4l_agg$mean.orepsize[i], col = "grey", lwd=2)
    segments(i-1, run4g_agg$mean.orepsize[i-1], i, run4g_agg$mean.orepsize[i], col = "black", lwd=2)
    segments(i-1, run5l_agg$mean.orepsize[i-1], i, run5l_agg$mean.orepsize[i], col = "tan", lwd=2)
    segments(i-1, run5g_agg$mean.orepsize[i-1], i, run5g_agg$mean.orepsize[i], col = "brown", lwd=2)
  }
}
legend(x = 0, y = 2.75, 
       legend = c("Default Local","20% Leaders Local", "High Prop Local", 
                  "0% Leaders Local", "70% Init. For Local",
                  "Default Global","20% Leaders Global", "High Prop Global", 
                  "0% Leaders Global", "70% Init. For Global"), 
       fill = c("pink","lightblue","green","grey","tan",
                "red","blue","darkgreen","black","brown"),
       ncol = 2)

# Number deliberating each round
for(i in 1:length(run1l$num.delib)){
  if(i == 1){
    plot(-100, -100, xlim=c(1,100), ylim=c(100,300), ylab="Number", xlab="Iteration", type="n", cex.axis=0.8, main = "Number Deliberating Each Round")
  }else{
    segments(i-1, run1l_agg$num.delib[i-1], i, run1l_agg$num.delib[i], col = "pink", lwd=2)
    segments(i-1, run1g_agg$num.delib[i-1], i, run1g_agg$num.delib[i], col = "red", lwd=2)
    segments(i-1, run2l_agg$num.delib[i-1], i, run2l_agg$num.delib[i], col = "lightblue", lwd=2)
    segments(i-1, run2g_agg$num.delib[i-1], i, run2g_agg$num.delib[i], col = "blue", lwd=2)
    segments(i-1, run3l_agg$num.delib[i-1], i, run3l_agg$num.delib[i], col = "green", lwd=2)
    segments(i-1, run3g_agg$num.delib[i-1], i, run3g_agg$num.delib[i], col = "darkgreen", lwd=2)
    segments(i-1, run4l_agg$num.delib[i-1], i, run4l_agg$num.delib[i], col = "grey", lwd=2)
    segments(i-1, run4g_agg$num.delib[i-1], i, run4g_agg$num.delib[i], col = "black", lwd=2)
    segments(i-1, run5l_agg$num.delib[i-1], i, run5l_agg$num.delib[i], col = "tan", lwd=2)
    segments(i-1, run5g_agg$num.delib[i-1], i, run5g_agg$num.delib[i], col = "brown", lwd=2)
  }
}
legend(x = 20, y = 290, 
       legend = c("Default Local","20% Leaders Local", "High Prop Local", 
                  "0% Leaders Local", "70% Init. For Local",
                  "Default Global","20% Leaders Global", "High Prop Global", 
                  "0% Leaders Global", "70% Init. For Global"), 
       fill = c("pink","lightblue","green","grey","tan",
                "red","blue","darkgreen","black","brown"),
       ncol = 2)

# Flips each round
for(i in 1:length(run1l$flips)){
  if(i == 1){
    plot(-100, -100, xlim=c(1,100), ylim=c(0,2), ylab="Number", xlab="Iteration", type="n", cex.axis=0.8, main = "Number of Position Changes Each Round")
  }else{
    segments(i-1, run1l_agg$flips[i-1], i, run1l_agg$flips[i], col = "pink", lwd=2)
    segments(i-1, run1g_agg$flips[i-1], i, run1g_agg$flips[i], col = "red", lwd=2)
    segments(i-1, run2l_agg$flips[i-1], i, run2l_agg$flips[i], col = "lightblue", lwd=2)
    segments(i-1, run2g_agg$flips[i-1], i, run2g_agg$flips[i], col = "blue", lwd=2)
    segments(i-1, run3l_agg$flips[i-1], i, run3l_agg$flips[i], col = "green", lwd=2)
    segments(i-1, run3g_agg$flips[i-1], i, run3g_agg$flips[i], col = "darkgreen", lwd=2)
    segments(i-1, run4l_agg$flips[i-1], i, run4l_agg$flips[i], col = "grey", lwd=2)
    segments(i-1, run4g_agg$flips[i-1], i, run4g_agg$flips[i], col = "black", lwd=2)
    segments(i-1, run5l_agg$flips[i-1], i, run5l_agg$flips[i], col = "tan", lwd=2)
    segments(i-1, run5g_agg$flips[i-1], i, run5g_agg$flips[i], col = "brown", lwd=2)
  }
}

# Tension each round
for(i in 1:length(run1l$tension)){
  if(i == 1){
    plot(-100, -100, xlim=c(1,100), ylim=c(0,.7), ylab="Tension Level", xlab="Iteration", type="n", cex.axis=0.8, main = "Tension Level Each Round")
  }else{
    segments(i-1, run1l_agg$tension[i-1], i, run1l_agg$tension[i], col = "pink", lwd=2)
    segments(i-1, run1g_agg$tension[i-1], i, run1g_agg$tension[i], col = "red", lwd=2)
    segments(i-1, run2l_agg$tension[i-1], i, run2l_agg$tension[i], col = "lightblue", lwd=2)
    segments(i-1, run2g_agg$tension[i-1], i, run2g_agg$tension[i], col = "blue", lwd=2)
    segments(i-1, run3l_agg$tension[i-1], i, run3l_agg$tension[i], col = "green", lwd=2)
    segments(i-1, run3g_agg$tension[i-1], i, run3g_agg$tension[i], col = "darkgreen", lwd=2)
    segments(i-1, run4l_agg$tension[i-1], i, run4l_agg$tension[i], col = "grey", lwd=2)
    segments(i-1, run4g_agg$tension[i-1], i, run4g_agg$tension[i], col = "black", lwd=2)
    segments(i-1, run5l_agg$tension[i-1], i, run5l_agg$tension[i], col = "tan", lwd=2)
    segments(i-1, run5g_agg$tension[i-1], i, run5g_agg$tension[i], col = "brown", lwd=2)
  }
}
legend(x = 0, y = .15, 
       legend = c("Default Local","20% Leaders Local", "High Prop Local", 
                  "0% Leaders Local", "70% Init. For Local",
                  "Default Global","20% Leaders Global", "High Prop Global", 
                  "0% Leaders Global", "70% Init. For Global"), 
       fill = c("pink","lightblue","green","grey","tan",
                "red","blue","darkgreen","black","brown"),
       ncol = 2)

plot(density(c(agents1lc$dprop, agents1lc_2$dprop, agents1lc_3$dprop, agents1lc_4$dprop, agents1lc_5$dprop)),
     main = "Deliberation Propensity after 100 Rounds (Default Run / Local Vision Rule)")
plot(density(c(agents1gc$dprop, agents1gc_2$dprop, agents1gc_3$dprop, agents1gc_4$dprop, agents1gc_5$dprop)),
     main = "Deliberation Propensity after 100 Rounds (Default Run / Global Vision Rule)")

plot(density(c(agents2lc$dprop, agents2lc_2$dprop, agents2lc_3$dprop, agents2lc_4$dprop, agents2lc_5$dprop)))
plot(density(c(agents2gc$dprop, agents2gc_2$dprop, agents2gc_3$dprop, agents2gc_4$dprop, agents2gc_5$dprop)))

plot(density(c(agents3lc$dprop, agents3lc_2$dprop, agents3lc_3$dprop, agents3lc_4$dprop, agents3lc_5$dprop)))
plot(density(c(agents3gc$dprop, agents3gc_2$dprop, agents3gc_3$dprop, agents3gc_4$dprop, agents3gc_5$dprop)))

plot(density(c(agents4lc$dprop, agents4lc_2$dprop, agents4lc_3$dprop, agents4lc_4$dprop, agents4lc_5$dprop)))
plot(density(c(agents4gc$dprop, agents4gc_2$dprop, agents4gc_3$dprop, agents4gc_4$dprop, agents4gc_5$dprop)))

plot(density(c(agents5lc$dprop, agents5lc_2$dprop, agents5lc_3$dprop, agents5lc_4$dprop, agents5lc_5$dprop)))
plot(density(c(agents5gc$dprop, agents5gc_2$dprop, agents5gc_3$dprop, agents5gc_4$dprop, agents5gc_5$dprop)))
