#Kayla Blincow 3/15/2016
#Generating violin plots of depths over D/C/N time periods

setwd("C:/Users/kmbli/Documents/Grouper/KaylaAnalyses")
library(rethinking)
library(lubridate)
library(ggplot2)

d2 <- read.table(file = "lmdata.txt", header = TRUE)
d2$Date <- as.Date(d2$Date, "%m/%d/%Y")

d2$month <- format(d2$Date, "%b")

Dnonsp <- d2[!(d2$month=="Jan" | d2$month=="Feb"),]

#generate the violin plot
p1<-ggplot(Dnonsp,aes(x=DNC, y=depth))
p1 + geom_violin()

p2 <- ggplot(Dnonsp,aes(x=DNC, y=depth)) + scale_y_reverse() +
  scale_x_discrete(position = "top") + 
  theme_classic() + labs(x = "Time Period", y = "Depth (m)", size = 4) +
  theme(
    axis.title = element_text(color="black", size=15), 
    axis.text.x = element_text(color = "black", size = 13, margin = margin(r = 1)),
    axis.text.y = element_text(color = "black", size = 13),
    axis.ticks.length = unit(0.25, "cm")) +
  geom_jitter(alpha = 0.1) +
  geom_violin(alpha = 0.9, bw = 0.45)

p2
