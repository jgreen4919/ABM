# Deliberation Viz
source("~/Desktop/ABM/Deliberation/delib_functions.R")
setwd("~/Desktop/ABM/Deliberation/")
load("delib_runs.RData")
setwd("~/Desktop/ABM/Deliberation/Viz")

plotDelib(title = "Initial Deliberative Space (Default Run / Local Vision Rule)", dat = agents1l, view = "p.repsize")
plotDelib(title = "Deliberative Space after 100 Rounds (Default Run / Local Vision Rule)", dat = agents1lc, view = "p.repsize")
plotDelib(title = "Initial Deliberative Space (Default Run / Global Vision Rule)", dat = agents1g, view = "p.repsize")
plotDelib(title = "Deliberative Space after 100 Rounds (Default Run / Global Vision Rule)", dat = agents1gc, view = "p.repsize")

plotDelib(title = "Initial Deliberative Space (Default Run / Local Vision Rule)", dat = agents1l, view = "o.repsize")
plotDelib(title = "Deliberative Space after 100 Rounds (Default Run / Local Vision Rule)", dat = agents1lc, view = "o.repsize")
plotDelib(title = "Initial Deliberative Space (Default Run / Global Vision Rule)", dat = agents1g, view = "o.repsize")
plotDelib(title = "Deliberative Space after 100 Rounds (Default Run / Global Vision Rule)", dat = agents1gc, view = "o.repsize")

plotDelib(title = "Initial Deliberative Space (Local Vision Rule)", dat = agents1l, view = "position")
plotDelib(title = "Deliberative Space After 100 Rounds (Default Run / Local Vision Rule)", dat = agents1lc, view = "position")
table(agents1l$position)
table(agents1lc$position)
plotDelib(title = "Initial Deliberative Space (Default Run / Global Vision Rule)", dat = agents1g, view = "position")
plotDelib(title = "Deliberative Space After 100 Rounds (Default Run / Global Vision Rule)", dat = agents1gc, view = "position")
table(agents1g$position)
table(agents1gc$position)

plotDelib(title = "Initial Deliberative Space (Unbalanced Run / Local Vision Rule)", dat = agents5l, view = "position")
plotDelib(title = "Deliberative Space After 100 Rounds (Unbalanced Run / Local Vision Rule)", dat = agents5lc, view = "position")
table(agents5l$position)
table(agents5lc$position)
plotDelib(title = "Initial Deliberative Space (Unbalanced Run / Global Vision Rule)", dat = agents5g, view = "position")
plotDelib(title = "Deliberative Space After 100 Rounds (Unbalanced Run / Global Vision Rule)", dat = agents5gc, view = "position")
table(agents5g$position)
table(agents5gc$position)

plotDelib(title = "Initial Deliberative Space (Default Run / Local Vision Rule)", dat = agents1l, view = "dqual")
plotDelib(title = "Deliberative Space after 100 Rounds (Default Run / Local Vision Rule)", dat = agents1lc, view = "dqual")
plotDelib(title = "Initial Deliberative Space (Default Run / Global Vision Rule)", dat = agents1g, view = "dqual")
plotDelib(title = "Deliberative Space after 100 Rounds (Default Run / Global Vision Rule)", dat = agents1gc, view = "dqual")

plotDelib(title = "Initial Deliberative Space (Default Run / Local Vision Rule)", dat = agents1l, view = "dprop")
plotDelib(title = "Deliberative Space after 100 Rounds (Default Run / Local Vision Rule)", dat = agents1lc, view = "dprop")
plotDelib(title = "Initial Deliberative Space (Default Run / Global Vision Rule)", dat = agents1g, view = "dprop")
plotDelib(title = "Deliberative Space after 100 Rounds (Default Run / Global Vision Rule)", dat = agents1gc, view = "dprop")

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
    plot(-100, -100, xlim=c(1,100), ylim=c(.45, .55), ylab="Percent", xlab="Iteration", type="n", cex.axis=0.8, main = "Percent in Favor: Balanced Runs")
  }else{
    segments(i-1, run1l_agg$pct.for[i-1], i, run1l_agg$pct.for[i], col = "pink", lwd=2)
    segments(i-1, run1g_agg$pct.for[i-1], i, run1g_agg$pct.for[i], col = "red", lwd=2)
    segments(i-1, run2l_agg$pct.for[i-1], i, run2l_agg$pct.for[i], col = "lightblue", lwd=2)
    segments(i-1, run2g_agg$pct.for[i-1], i, run2g_agg$pct.for[i], col = "blue", lwd=2)
    segments(i-1, run3l_agg$pct.for[i-1], i, run3l_agg$pct.for[i], col = "green", lwd=2)
    segments(i-1, run3g_agg$pct.for[i-1], i, run3g_agg$pct.for[i], col = "darkgreen", lwd=2)
    segments(i-1, run4l_agg$pct.for[i-1], i, run4l_agg$pct.for[i], col = "grey", lwd=2)
    segments(i-1, run4g_agg$pct.for[i-1], i, run4g_agg$pct.for[i], col = "black", lwd=2)
  }
}
legend(x = 20, y = .47, 
       legend = c("Default Local","20% Leaders Local", "High Prop Local", 
                  "0% Leaders Local",
                  "Default Global","20% Leaders Global", "High Prop Global", 
                  "0% Leaders Global"), 
       fill = c("pink","lightblue","green","grey",
                "red","blue","darkgreen","black"),
       ncol = 2)


for(i in 1:length(run1l_agg$pct.for)){
  if(i == 1){
    plot(-100, -100, xlim=c(1,100), ylim=c(.65,.75), ylab="Percent", xlab="Iteration", type="n", cex.axis=0.8, main = "Percent in Favor: Unbalanced Runs")
  }else{
    segments(i-1, run5l_agg$pct.for[i-1], i, run5l_agg$pct.for[i], col = "tan", lwd=2)
    segments(i-1, run5g_agg$pct.for[i-1], i, run5g_agg$pct.for[i], col = "brown", lwd=2)
  }
}
legend(x = 20, y = .68, 
       legend = c("Local Vision Rule","Global Vision Rule"), 
       fill = c("tan","brown"),
       ncol = 2)

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
legend(x = 20, y = 1.75, 
       legend = c("Default Local","20% Leaders Local", "High Prop Local", 
                  "0% Leaders Local", "70% Init. For Local",
                  "Default Global","20% Leaders Global", "High Prop Global", 
                  "0% Leaders Global", "70% Init. For Global"), 
       fill = c("pink","lightblue","green","grey","tan",
                "red","blue","darkgreen","black","brown"),
       ncol = 2)

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

plot(density(c(agents1l$dprop, agents1l_2$dprop, agents1l_3$dprop, agents1l_4$dprop, agents1l_5$dprop)),
     main = "Initial Deliberation Propensity (Default Run / Local Vision Rule)")
plot(density(c(agents1g$dprop, agents1g_2$dprop, agents1g_3$dprop, agents1g_4$dprop, agents1g_5$dprop)),
     main = "Initial Deliberation Propensity (Default Run / Global Vision Rule)")
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


hist(c(agents1l$p.repsize, agents1l_2$p.repsize, agents1l_3$p.repsize, agents1l_4$p.repsize, agents1l_5$p.repsize),
     main = "Initial For-Repertoire Size (Default Run / Local Vision Rule)")
hist(c(agents1g$p.repsize, agents1g_2$p.repsize, agents1g_3$p.repsize, agents1g_4$p.repsize, agents1g_5$p.repsize),
     main = "Initial For-Repertoire Size (Default Run / Global Vision Rule)")
hist(c(agents1lc$p.repsize, agents1lc_2$p.repsize, agents1lc_3$p.repsize, agents1lc_4$p.repsize, agents1lc_5$p.repsize),
     main = "For-Repertoire Size after 100 Rounds (Default Run / Local Vision Rule)")
hist(c(agents1gc$p.repsize, agents1gc_2$p.repsize, agents1gc_3$p.repsize, agents1gc_4$p.repsize, agents1gc_5$p.repsize),
     main = "For-Repertoire Size after 100 Rounds (Default Run / Global Vision Rule)")

hist(c(agents2l$p.repsize, agents2l_2$p.repsize, agents2l_3$p.repsize, agents2l_4$p.repsize, agents2l_5$p.repsize),
     main = "Initial For-Repertoire Size (20% Leaders / Local Vision Rule)")
hist(c(agents2g$p.repsize, agents2g_2$p.repsize, agents2g_3$p.repsize, agents2g_4$p.repsize, agents2g_5$p.repsize),
     main = "Initial For-Repertoire Size (20% Leaders / Global Vision Rule)")
hist(c(agents2lc$p.repsize, agents2lc_2$p.repsize, agents2lc_3$p.repsize, agents2lc_4$p.repsize, agents2lc_5$p.repsize),
     main = "For-Repertoire Size after 100 Rounds (20% Leaders / Local Vision Rule)")
hist(c(agents2gc$p.repsize, agents2gc_2$p.repsize, agents2gc_3$p.repsize, agents2gc_4$p.repsize, agents2gc_5$p.repsize),
     main = "For-Repertoire Size after 100 Rounds (20% Leaders / Global Vision Rule)")


hist(c(agents1l$o.repsize, agents1l_2$o.repsize, agents1l_3$o.repsize, agents1l_4$o.repsize, agents1l_5$o.repsize),
     main = "Initial Against-Repertoire Size (Default Run / Local Vision Rule)")
hist(c(agents1g$o.repsize, agents1g_2$o.repsize, agents1g_3$o.repsize, agents1g_4$o.repsize, agents1g_5$o.repsize),
     main = "Initial Against-Repertoire Size (Default Run / Global Vision Rule)")
hist(c(agents1lc$o.repsize, agents1lc_2$o.repsize, agents1lc_3$o.repsize, agents1lc_4$o.repsize, agents1lc_5$o.repsize),
     main = "Against-Repertoire Size after 100 Rounds (Default Run / Local Vision Rule)")
hist(c(agents1gc$o.repsize, agents1gc_2$o.repsize, agents1gc_3$o.repsize, agents1gc_4$o.repsize, agents1gc_5$o.repsize),
     main = "Against-Repertoire Size after 100 Rounds (Default Run / Global Vision Rule)")

hist(c(agents2l$o.repsize, agents2l_2$o.repsize, agents2l_3$o.repsize, agents2l_4$o.repsize, agents2l_5$o.repsize),
     main = "Initial Against-Repertoire Size (20% Leaders / Local Vision Rule)")
hist(c(agents2g$o.repsize, agents2g_2$o.repsize, agents2g_3$o.repsize, agents2g_4$o.repsize, agents2g_5$o.repsize),
     main = "Initial Against-Repertoire Size (20% Leaders / Global Vision Rule)")
hist(c(agents2lc$o.repsize, agents2lc_2$o.repsize, agents2lc_3$o.repsize, agents2lc_4$o.repsize, agents2lc_5$o.repsize),
     main = "Against-Repertoire Size after 100 Rounds (20% Leaders / Local Vision Rule)")
hist(c(agents2gc$o.repsize, agents2gc_2$o.repsize, agents2gc_3$o.repsize, agents2gc_4$o.repsize, agents2gc_5$o.repsize),
     main = "Against-Repertoire Size after 100 Rounds (20% Leaders / Global Vision Rule)")

