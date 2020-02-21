#Kayla Blincow
#UPDATE: 10/30/2017--10/20/2017
#Making depth/tag/condition plots
#Looking into length and weight relationships on their own also, then doing
#AIC to figure out the best model



setwd("C:/Users/kmbli/Documents/Grouper/KaylaAnalyses")
library(rethinking)

d <- read.table(file = "cond.txt", header = TRUE)
d2 <- read.table(file = "lmdata.txt", header = TRUE)
 
library(ggplot2)

head(d)



#want column with median of depth in d
med <- function (x){
  df <- d2[d2$tag==x,]
  median(df$depth)
}

d$med <- c(med(41), med(42), med(43), med(59), med(61), med(63))

#want to take variance of the log of depth to see if that influences the tail of the plots. Shallow fish doen't have the opportunity to move upward, so obvi less.
d$varlog <- c(0.007001273, 0.004454931, 0.011170445, 0.005111682, 0.002126577, 0.000761915)


#plotting functions

lm_eqn <- function(df, z, zz ){
  m <- lm(z ~ zz, df);
  eq <- substitute(italic(y) == a + b %.% italic(x)*","~~italic(r)^2~"="~r2, 
                   list(a = format(coef(m)[1], digits = 2), 
                        b = format(coef(m)[2], digits = 2), 
                        r2 = format(summary(m)$r.squared, digits = 3)))
  as.character(as.expression(eq));                 
}

#pop up window for my plots
windows(title="Condition/Depth Linear Model")

#condition plots
pcond <- ggplot(d, aes(x = condition, y = mean)) + scale_y_reverse() + 
  scale_x_continuous(position = "top") + theme_classic() +
  labs(x = "Condition", y = "mean(Depth)") +
  geom_point() +
  geom_smooth(method='lm',formula=y~x, col = "black") +
  theme(axis.title = element_text(color="black", size=15)) +
  theme(axis.text.x = element_text(color = "black", size = 13, margin = margin(r = 1))) +
  theme(axis.text.y = element_text(color = "black", size = 13)) +
  theme(axis.ticks.length = unit(0.25, "cm")) +
  geom_text(x = 2.1, y = 0, label = lm_eqn(d, z = d$mean, zz = d$condition), parse = TRUE)
pcond

#export my plot
jpeg(filename="lmconddepth.jpg", 
     units="in", 
     width=6, 
     height=5.5, 
     pointsize=12, 
     res=400)
pcond
dev.off()



gv #varlog test
p5cond <- ggplot(d, aes(x = condition, y = varlog)) + scale_y_reverse() + 
  scale_x_continuous(position = "top") + theme_classic() + 
  labs(x = "Condition", y = "var(Depth)") +
  geom_point() +
  geom_smooth(method='lm',formula=y~x) + 
  geom_text(x = 2.1, y = 0, label = lm_eqn(d, z = d$varlog, zz = d$condition), parse = TRUE)
p5cond

p2cond <- ggplot(d, aes(x = condition, y = var)) + scale_y_reverse() + 
  scale_x_continuous(position = "top") + theme_classic() + 
  labs(x = "Condition", y = "var(Depth)") +
  geom_point() +
  geom_smooth(method='lm',formula=y~x) + 
  geom_text(x = 2.1, y = 0, label = lm_eqn(d, z = d$var, zz = d$condition), parse = TRUE)
p2cond

p3cond <- ggplot(d, aes(x = condition, y = med)) + scale_y_reverse() + 
  scale_x_continuous(position = "top") + theme_classic() + 
  labs(x = "Condition", y = "median(Depth)") +
  geom_point() +
  geom_smooth(method='lm',formula=y~x) + 
  geom_text(x = 2.1, y = 0, label = lm_eqn(d, z = d$med, zz = d$condition), parse = TRUE)
p3cond


#length plots
plength <- ggplot(d, aes(x = length_cm, y = mean)) + scale_y_reverse() + 
  scale_x_continuous(position = "top") + theme_classic() +
  labs(x = "Fish Length", y = "mean(Depth)") +
  geom_point() +
  geom_smooth(method='lm',formula=y~x) +
  geom_text(x = 75, y = 0, label = lm_eqn(d, z = d$mean, zz = d$length_cm), parse = TRUE)
plength

p2length <- ggplot(d, aes(x = length_cm, y = var)) + scale_y_reverse() + 
  scale_x_continuous(position = "top") + theme_classic() + 
  labs(x = "Fish Length", y = "var(Depth)") +
  geom_point() +
  geom_smooth(method='lm',formula=y~x) + 
  geom_text(x = 75, y = 0, label = lm_eqn(d, z = d$var, zz = d$length_cm), parse = TRUE)
p2length

p3length <- ggplot(d, aes(x = length_cm, y = med)) + scale_y_reverse() + 
  scale_x_continuous(position = "top") + theme_classic() + 
  labs(x = "Length", y = "median(Depth)") +
  geom_point() +
  geom_smooth(method='lm',formula=y~x) + 
  geom_text(x = 75, y = 0, label = lm_eqn(d, z = d$med, zz = d$length_cm), parse = TRUE)
p3length


#weight plots
pweight <- ggplot(d, aes(x = weight_kg, y = mean)) + scale_y_reverse() + 
  scale_x_continuous(position = "top") + theme_classic() +
  labs(x = "Fish Weight", y = "mean(Depth)") +
  geom_point() +
  geom_smooth(method='lm',formula=y~x) +
  geom_text(x = 10, y = 0, label = lm_eqn(d, z = d$mean, zz = d$weight_kg), parse = TRUE)
pweight

p2weight <- ggplot(d, aes(x = weight_kg, y = var)) + scale_y_reverse() + 
  scale_x_continuous(position = "top") + theme_classic() + 
  labs(x = "Fish Length", y = "var(Depth)") +
  geom_point() +
  geom_smooth(method='lm',formula=y~x) + 
  geom_text(x = 10, y = 0, label = lm_eqn(d, z = d$var, zz = d$weight_kg), parse = TRUE)
p2weight

p3weight <- ggplot(d, aes(x = weight_kg, y = med)) + scale_y_reverse() + 
  scale_x_continuous(position = "top") + theme_classic() + 
  labs(x = "Fish Weight", y = "median(Depth)") +
  geom_point() +
  geom_smooth(method='lm',formula=y~x) + 
  geom_text(x = 10, y = 0, label = lm_eqn(d, z = d$med, zz = d$weight_kg), parse = TRUE)
p3weight



#Build linear models
#condition
mC <- lm(mean ~ condition, data = d)
summary(mC)

vC <- lm(var ~ condition, data = d)
summary(vC)

mdC <- lm(med ~ condition, data = d)
summary(mdC)



#length
mL <- lm(mean ~ length_cm, data = d)
summary(mL)

vL <- lm(var ~ length_cm, data = d)
summary(vL)

mdL <- lm(med ~ length_cm, data = d)
summary(mdL)


#weight
mW <- lm(mean ~ weight_kg, data = d)
summary(mW)

vW <- lm(var ~ weight_kg, data = d)
summary(vW)

mdW <- lm(med ~ weight_kg, data = d)
summary(mdW)

#Null models (no size factor)
nullm <- lm(mean ~ 1, data = d)

nullv <- lm(var ~ 1, data = d)

nullmd <- lm(var ~ 1, data = d)

# perform information criteria analysis
AIC(mC, mL, mW)
results <- compare(mC, mL, mW)
AICc(mC, mL, mW, nullm)
anova(mC, nullm)
anova(mL, nullm)
anova(mW, nullm)

AIC(vC, vL, vW)
compare(vC, vL, vW)

AIC(mdC, mdL, mdW)
compare(mdC, mdL, mdW)

compare(mC, mL, mW, vC, vL, vW, mdC, mdW, mdL, nullm, nullv, nullmd)

#AICc to account for small sample size
AICc(mC, mL, mW, vC, vL, vW, mdC, mdW, mdL, nullm, nullv, nullmd)

# plot tag #'s against depth
# REORGANIZE SO RANKED BY CONDITION??
d2$tag <- as.factor(d2$tag)

d2$tag <- factor(d2$tag, levels = c("41", "59", "61", "63", "43", "42"))
Tlabs <- c("41", "59", "61", "63", "43", "42")


d2$condS <- format(round(d2$condition, 4), nsmall = 2)
condS <- c("1.7784", "1.8519", "1.9877", "2.0094", "2.0979", "2.1861")
#pop up window for my plots
windows(title="Condition/Depth Over Time")


p3 <- ggplot(d2, aes(x = tag, y = depth)) + scale_y_reverse() +
  scale_x_discrete(position = "top") + 
  theme_classic() + labs(x = "Fish ID", y = "Depth (m)", size = 4) +
  theme(
    axis.title = element_text(color="black", size=15), 
    axis.text.x = element_text(color = "black", size = 13, margin = margin(r = 1)),
    axis.text.y = element_text(color = "black", size = 13),
    axis.ticks.length = unit(0.25, "cm")) +
  geom_jitter(alpha = 0.1) +
  geom_violin(alpha = 0.9, bw = 0.45)

p3


#Export figure in proper size
jpeg(filename="violin.jpg", 
     units="in", 
     width=6, 
     height=5.5, 
     pointsize=12, 
     res=400)
p3
dev.off()


p4 <- ggplot(d2, aes(x = as.factor(condition), y = depth)) + scale_y_reverse() +
  scale_x_discrete( position = "top") + 
  theme_classic() +labs(x = "Condition", y = "Depth") +
  geom_jitter( factor) 
p4




#Brice's whiteboard models ---PSEUDOREPLICATION

dF <- lm(depth ~ tag, data = d2)

dC <- lm(depth ~ condition, data = d2)

dnull <- lm(depth ~ 1, data = d2)

AIC(dF, dC, dnull)
