# Kayla Blincow
# 10/25/2017
# Making abacus plots to determine which tags have home ranges

#set up my space
setwd("C:/Users/kmbli/Documents/Grouper/KaylaAnalyses")
library(tidyverse)

d <- read.table(file = "abacusdata.txt", header = TRUE)

# Combine date and time, and make it into a time object

d$dta <- paste(d$Date, d$Time)
d$dt <- as.POSIXct(d$dta, tz = "EST", 
                           format = "%m/%d/%Y %H:%M:%OS")
d$Date <- as.Date(d$Date, format = "%m/%d/%Y")

tag63 <- d[d$tag==63,]
max(tag63$dt)

levels(d$Name) <- list("LC1" = "REC3",
                       "LC2" = "REC4",
                       "LC3" = "REC5",
                       "LC4" = "REC6",
                       "LC5" = "REC7",
                       "LC6" = "REC8",
                       "LC7" = "REC9",
                       "LC8" = "REC10",
                       "LC9" = "REC11",
                       "LC10" = "REC12",
                       "LC11" = "REC13",
                       "LC12" = "REC14",
                       "LC13" = "REC15",
                       "LC14" = "REC2",
                       "LC15" = "REC1",
                       "CB1" = "CBWEREC",
                       "CB2" = "CBNWREC",
                       "CB3" = "CB7REC",
                       "CB4" = "CBNEREC",
                       "CB5" = "CBEEREC",
                       "CB6" = "CBSEREC",
                       "CB7" = "CB8REC",
                       "CB8" = "CBSWREC"
                       )



# make ze plots
p <- function(x){
  d1<- d[d$tag==x,]
  d1$dt <- as.Date(d1$dt, format = "%m/%d/%Y %H:%M:%S")
  ggplot(d1, aes(x = dt, y = Name)) +
    geom_point() +
    labs(y = "Receiver", x = "Time", size = 4)+
    theme_classic()+
    #scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
    theme(
      axis.title = element_text(color="black", size=15),
      axis.text.x = element_text(color = "black", size = 13, margin = margin(r = 1)),
      axis.text.y = element_text(color = "black", size = 13),
      axis.ticks.length = unit(0.25, "cm"))
}

pc <- function(x){
  d2 <- d[d$tag==x,]
  t <- count(d2, "Date")
  plot(t$Date, t$freq, xlab = "Date", ylab = "Daily Detection Frequency",
       xaxt='n', pch = 20)
  axis.Date(1, at=seq(min(t$Date), max(t$Date), by="1 mon"), format="%m-%Y")
}



#Tag Plots
p(40) #no home hydro
p(41) #home hydro at 19 (REC5)
pc(41)
p(42) #home hydro at 19 (REC5)
p(43) #home hydro at 15 (REC15)
p(44) #no home hydro
p(54) #no home hydro--He Dead!
pc(54)
p(55) #no home hydro
p(57) #no home hydro
p(59) #home hydro at 7 (CBSWREC)
p(60) #no home hydro
p(61) #home hydro at 4 (CBNEREC)
pc(61)
p(62) #no home hydro
p(63) #home hydro at 3 (CBEEREC)--CONFIRMED
pc(63)
p(3900) #no home hydro
p(3901) #no home hydro
p(3902) #no home hydro
p(3903) #no home hydro
p(3904) #no home hydro
p(3905) #no home hydro, lots of hydros
p(3906) #no home hydro, lots of hydros
p(3907) #no home hydro
p(3908) #no home hydro
p(3909) #home hydro at 23 (REC9)--not as frequent
p(3910) #no home hydro
p(3911) #no home hydro
p(3912) #home hydro at 19 (REC5)
p(3913) #no home hydro
p(3914) #no home hydro
p(3915) #no home hydro
p(3916) #home hydro at 22 (REC8)
p(3917) #home hydro at 19 (REC5)
p(3918) #home hydro at 10 (REC10)
p(3919) #no home hydro
p(3920) #home hydro at 17 (REC3)
pc(3920)
p(3921) #home hydro at 12 (REC12) --tapers off in last year
p(3923) #no home hydro
p(3924) #no home hydro
p(3925) #home hydro at 12 (REC12)
p(3926) #no home hydro
pc(3926)
p(3927) #home hydro at 14 (REC14)
p(3928) #no home hydro
pc(3928)
p(3929) #no home hydro
p(3930) #no home hydro
p(3931) #no home hydro
p(3932) #no home hydro--appears to be inbetween 18 and 19
p(3933) #no home hydro
p(3934) #no home hydro--detected consistently but infrequently at same hydro
p(3935) #no home hydro
p(3936) #home hydro at 14 (REC14)
p(3937) #no home hydro
p(3938) #no home hydro
p(3939) #no home hydro--only detected two days
p(3940) #no home hydro
p(3941) #home hydro at 19 (REC5)--ish
p(3942) #no home hydro
p(3943) #home hydro at 18 (REC4)
p(3944) #no home hydro
p(4041) #no home hydro
pc(4041)
p(4042) #no home hydro
p(4043) #no home hydro
p(4046) #no home hydro
p(4047) #no home hydro
p(4049) #no home hydro
p(4051) #home hydro at 8 (CBWEREC)
pc(4051)
p(4052) #no home hydro
p(4053) #no home hydro
p(4054) #home hydro at 8 (CBWEREC)
pc(4054)
p(4056) #no home hydro
p(9753) #no home hydro
p(9754) #no home hydro
p(9755) #no home hydro
p(9756) #no home hydro
p(9757) #no home hydro
p(9758) #no home hydro
p(9759) #no home hydro
p(9760) #no home hydro
p(9761) #no home hydro
p(9762) #no home hydro


#needs to be detected outside of the spawning months of Jan/Feb for at least three months to qualify as home hydro.


#4 panel plot for Tag 3912, Tag 61, Tag 3916, and Tag 3943

#pop up window for my plot
windows(title="Abacus")

p(3912)

#Export figure in proper size
jpeg(filename="3912ab.jpg",
     units="in",
     width=6,
     height=5.5,
     pointsize=12,
     res=400)
p(3912)
dev.off()


#pop up window for my plot
windows(title="Abacus")
p(61)
#Export figure in proper size
jpeg(filename="61ab.jpg",
     units="in",
     width=6,
     height=5.5,
     pointsize=12,
     res=400)
p(61)
dev.off()


#pop up window for my plot
windows(title="Abacus")
p(3916)
#Export figure in proper size
jpeg(filename="3916ab.jpg",
     units="in",
     width=6,
     height=5.5,
     pointsize=12,
     res=400)
p(3916)
dev.off()


#pop up window for my plot
windows(title="Abacus")
p(3943)
#Export figure in proper size
jpeg(filename="3943ab.jpg",
     units="in",
     width=6,
     height=5.5,
     pointsize=12,
     res=400)
p(3943)
dev.off()

