# Kayla Blincow 2/18/2018
# Adapted from: Brice Semmens, Oct 30, 2017

# this script does a binomial logistic regression on the detections of pinging tags by a VR2w
# as a function of distance from the hydrophone. This data was collected on Jan 31 2005

# tags were deployed off of a boat that navigated around a moored hydrohpone (rec 15 on LC)
# all pings were recorded by a VR100 deployed off the boat. The VR100 records were then 
# compared with the recorded detections on the VR2w. All VR100 detected pings were scored
# with a 1 if the VR2w also recorded a ping, otherwise they were scored 0. The coordinates
# of each VR100 were used to calculate the distance (in KM) of each tag ping from the VR2w hydrohpone.
# In this analysis, we use distance from the VR2w as an explanatory variable in the probability
# of detection at the VR2w.

setwd("C:/Users/kmbli/Documents/Grouper/KaylaAnalyses")

# Bring in the data
range_dat<-as.data.frame(read.csv("detradiusdata.csv",header=TRUE))
bodysize<-sort(range_dat$Dist_KM)
#quartz(title="Distance vs. Tag Detection") # creates a quartz window with title

#KMB- quartz doesn't work on my PC.. need to use windows()
windows(title="Distance vs. Tag Detection")

# run a logistic regression model (in this case, generalized linear model with logit link). see ?glm
# g=glm(vr2_detect~Dist_KM,family=binomial,range_dat) 

#vr2_detect isn't a thing... I'm very confused, do you mean (vr2???--but that is 1s and 2s not 0s and 1s)
# Used excel to convert vr2 to 0s and 1s and calling it vr2_detect--not sure if it's right, but I'm giving it a go

g <- glm(vr2_detect2~Dist_KM, family=binomial, range_dat) 



# At what distance is detection probability 50%?
newdata = data.frame(Dist_KM = .32)
predict(g, newdata, type="response")
# So, @ 320m away from the hydrohpone, the detection probability is 50%!
newdata1 = data.frame(Dist_KM = .90)
predict(g, newdata1, type="response")


#Need to change curve line to black... which apparently is a whole situation..
#Going to paste the underlying code for the logi.hist.plot function and switch
#"red" to "black"

logi.hist.plot <- function (independ, depend, logi.mod = 1, type = "dit", boxp = TRUE, 
                            rug = FALSE, ylabel = "Probability", ylabel2 = "Frequency", 
                            xlabel = "", mainlabel = "", las.h = 1, counts = FALSE, ...) 
{
  logi.scater <- function(independ, depend, scater = "n", x.lab = xlabel, 
                          las = las.h) {
    plot(independ, depend, cex = 1, type = scater, ylab = ylabel, 
         xlab = x.lab, main = mainlabel, cex.lab = 1.2, las = las)
  }
  logi.rug <- function(independ, depend, pch.rug = 16, cex.rug = 1) {
    points(independ, depend, pch = pch.rug, cex = cex.rug)
  }
  logi.box <- function(independ, depend, col.box = "gray", 
                       x.lab = xlabel, las = las.h) {
    plot(independ, depend, cex = 1, type = "n", ylim = c(-0.1, 1.1), 
         ylab = ylabel, xlab = x.lab, cex.lab = 1.2, 
         las = las)
    indep.1 <- independ[depend == 1]
    indep.0 <- independ[depend == 0]
    boxplot(indep.1, horizontal = TRUE, add = TRUE, at = 1.05, 
            boxwex = 0.1, col = col.box, notch = TRUE)
    boxplot(indep.0, horizontal = TRUE, add = TRUE, at = -0.05, 
            boxwex = 0.1, col = col.box, notch = TRUE)
  }
  logi.curve <- function(independ, depend, mod = logi.mod, 
                         col.cur = "black", lwd.cur = 4) {
    if (mod == 1) 
      mod3 <- glm(depend ~ independ, family = binomial)
    if (mod == 2) 
      mod3 <- glm(depend ~ independ + I(independ^2), family = binomial)
    x.new <- seq(min(independ), max(independ), len = 100)
    y.new <- predict(mod3, data.frame(independ = x.new), 
                     type = "response")
    lines(x.new, y.new, lwd = lwd.cur, col = col.cur)
  }
  logi.dit <- function(independ, depend, cex.p = 1, pch.dit = 1, 
                       incre = 0.02) {
    indep.0 <- independ[depend == 0]
    indep.1 <- independ[depend == 1]
    uni.plot.0 <- function(x) length(which(indep.0 == x))
    uni.plot.1 <- function(x) length(which(indep.1 == x))
    cosa.0 <- apply(as.matrix(unique(indep.0)), 1, uni.plot.0)
    cosa.1 <- apply(as.matrix(unique(indep.1)), 1, uni.plot.1)
    points(independ, depend, pch = pch.dit, cex = cex.p)
    for (i in 1:max(cosa.0)) {
      for (j in 1:i) {
        points(unique(indep.0)[which(cosa.0 == i + 1)], 
               rep(0 + incre * j, length(which(cosa.0 == i + 1))), 
               pch = pch.dit, cex = cex.p)
      }
    }
    for (i in 1:max(cosa.1)) {
      for (j in 1:i) {
        points(unique(indep.1)[which(cosa.1 == i + 1)], 
               rep(1 - incre * j, length(which(cosa.1 == i + 1))), 
               pch = pch.dit, cex = cex.p)
      }
    }
  }
  logi.hist <- function(independ, depend, scale.hist = 5, col.hist = "blue", 
                        count.hist = TRUE, intervalo = 0, las.h1 = las.h) {
    h.br <- hist(independ, plot = FALSE)$br
    if (intervalo > 0) 
      h.br <- seq(from = range(h.br)[1], to = range(h.br)[2], 
                  by = intervalo)
    h.x <- hist(independ[depend == 0], breaks = h.br, plot = FALSE)$mid
    h.y0 <- hist(independ[depend == 0], breaks = h.br, plot = FALSE)$counts
    h.y1 <- hist(independ[depend == 1], breaks = h.br, plot = FALSE)$counts
    h.y0n <- h.y0/(max(c(h.y0, h.y1)) * scale.hist)
    h.y1n <- 1 - h.y1/(max(c(h.y0, h.y1)) * scale.hist)
    for (i in 1:length(h.y0n)) {
      if (h.y0n[i] > 0) 
        polygon(c(rep(h.br[i], 2), rep(h.br[i + 1], 2)), 
                c(0, rep(h.y0n[i], 2), 0), col = col.hist)
    }
    for (i in 1:length(h.y1n)) {
      if (h.y1n[i] < 1) 
        polygon(c(rep(h.br[i], 2), rep(h.br[i + 1], 2)), 
                c(h.y1n[i], 1, 1, h.y1n[i]), col = col.hist)
    }
    if (counts == TRUE) 
      for (i in 1:length(h.x)) {
        text(h.x[i], h.y1n[i], h.y1[i], cex = 1, pos = 1)
        text(h.x[i], h.y0n[i], h.y0[i], cex = 1, pos = 3)
      }
    axis.hist <- function(h.y0, h.y1, scale.hist, las = las.h1) {
      tope <- max(c(h.y0, h.y1))
      label.down <- c(0, (ceiling(tope/10)) * 5, (ceiling(tope/10)) * 
                        10)
      label.up <- c((ceiling(tope/10)) * 10, (ceiling(tope/10)) * 
                      5, 0)
      at.down <- label.down/(tope * scale.hist)
      at.up <- 1 - (label.up/(tope * scale.hist))
      at.hist <- c(at.down, at.up)
      label.hist <- c(label.down, label.up)
      axis(side = 4, at = at.hist, labels = label.hist, 
           las = las)
      mtext(ylabel2, side = 4, line = 2, cex = 1.2)
    }
    axis.hist(h.y0, h.y1, scale.hist)
    axis(side = 2, las = las.h1)
  }
  old.mar <- par()$mar
  par(mar = c(5.1, 4.1, 4.1, 4.1))
  if (boxp == TRUE) 
    logi.box(independ, depend)
  if (boxp == FALSE) 
    logi.scater(independ, depend)
  if (type != "dit") 
    logi.hist(independ, depend, ...)
  if (rug == TRUE) 
    logi.rug(independ, depend)
  logi.curve(independ, depend)
  if (type == "dit") 
    logi.dit(independ, depend)
  par(mar = old.mar)
}


# plot with distance (KM) on x-axis and Detection (0 or 1) on y-axis
#library(popbio)
logi.hist.plot(range_dat$Dist_KM, range_dat$vr2_detect2, xlabel = "Distance (km)", boxp=FALSE,type="hist",col="gray")





#Export figure in proper size
jpeg(filename="detectionradius.jpg", 
    units="in", 
    width=6, 
    height=5.5, 
    pointsize=12, 
    res=400)
logi.hist.plot(range_dat$Dist_KM,range_dat$vr2_detect2,xlabel = "Distance (km)", boxp=FALSE,type="hist",col="gray")
dev.off()

