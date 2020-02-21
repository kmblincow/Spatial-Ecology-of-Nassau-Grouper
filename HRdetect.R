# Kayla Blincow
#10/20/2017
#Recreating Sierra's Excel Plots to look nicer and display information that works with my rewriting of the paper.

setwd("C:/Users/kmbli/Documents/Grouper/KaylaAnalyses")

d <- read.table(file = "HRdetect.txt", header = TRUE)
b <- read.table(file = "BRACdetect.txt", header = TRUE)
c <- read.table(file = "tag61detect.txt", header = TRUE)

#combine data and time into one column
d$dt <- as.POSIXct(paste(d$date, d$time), format="%d-%b-%y %H:%M:%S")
d$date <- as.Date(d$date, "%d-%b-%y")
b$date <- as.Date(b$date, "%m/%d/%y")
c$date <- as.Date(c$date, "%m/%d/%y")

head(d)


library(plyr)

#create functions to make plots

#set up rolling average information
f21 <- rep(1/21,21)

p <- function(x){
  d2 <- d[d$tag==x,]
  t <- plyr::count(d2, "date")
  plot(t$date, t$freq, xlab = "Date", ylab = "Daily Detection Frequency", 
       xaxt='n', pch = 20)
  axis.Date(1, at=seq(min(t$date), max(t$date), by="1 mon"), format="%m-%Y")
}

p2 <- function(x){
  d2 <- b[b$Tag==x,]
  t <- plyr::count(d2, "date")
  plot(t$date, t$freq, xlab = "Date", ylab = "Daily Detection Frequency", 
       xaxt='n', pch = 20)
  axis.Date(1, at=seq(min(t$date), max(t$date), by="1 mon"), format="%m-%Y")
}

p3 <- function(x){
  d2 <- c[c$tag==x,]
  t <- plyr::count(d2, "date")
  plot(t$date, t$freq, xlab = "Date", ylab = "Daily Detection Frequency", 
       xaxt='n', pch = 20)
  axis.Date(1, at=seq(min(t$date), max(t$date), by="1 mon"), format="%m-%Y")
}

Corner_text <- function(text, location="topleft"){
  legend(location,legend=text, bty ="n", pch=NA) 
}


#Make plots (Little Cayman)

p(41)
Corner_text(text="Tag 41")

p(42)
Corner_text(text="Tag 42")

p(3943)
Corner_text(text="Tag 3943")

p(3912)
Corner_text(text="Tag 3912")

p(3917)
Corner_text(text="Tag 3917", location = "topright")

p(3941)
Corner_text(text = "Tag 3941")

p(3916)
Corner_text(text = "Tag 3941", location = "topright")

p(3909)
Corner_text(text = "Tag 3909")

p(3928) 
Corner_text(text = "Tag 3928", location = "topright")

p(3918)
Corner_text(text = "Tag 3918")

p(3921)
Corner_text(text = "Tag 3921")

p(3925)
Corner_text(text = "Tag 3925", location = "topright")

p(3927)
Corner_text(text = "Tag 3927", "topright")

p(3936)
Corner_text(text = "Tag 3936", "topright")

p(43)
Corner_text(text = "Tag 43", "topright")

p(3926)
Corner_text(text = "Tag 3926")

#Make plots (Cayman Brac)

p2(63)
Corner_text(text = "Tag 63")

p2(4051)
Corner_text(text = "Tag 4051")

p2(13172)
Corner_text(text = "Tag 13172", location = "topright")

p2(59)
Corner_text(text = "Tag 59")

#Make plot Tag 61

p3(61)
Corner_text(text = "Tag 61")


#Make 4 panel sample plot
par(mfrow = c(2,2), mar=c(4,4,1,1))

p3(61)
Corner_text(text = "Tag 61", location = "topright")

p(3912)
Corner_text(text = "Tag 3912", location = "topright")

p(3943)
Corner_text(text = "Tag 3943", location = "topright")

p(3916)
Corner_text(text = "Tag 3916", location = "topright")
