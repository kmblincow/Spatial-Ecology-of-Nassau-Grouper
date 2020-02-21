# Kayla Blincow
# 10/20/2017
# Making the depth over time plots 

setwd("C:/Users/kmbli/Documents/Grouper/KaylaAnalyses")

d <- read.table(file = "HRdetect.txt", header = TRUE)
b <- read.table(file = "BRACdetect.txt", header = TRUE)
c <- read.table(file = "tag61detect.txt", header = TRUE)

#prep ze data! 
d$date <- as.Date(d$date, "%d-%b-%y")
b$date <- as.Date(b$date, "%m/%d/%y")
c$date <- as.Date(c$date, "%m/%d/%y")

d$dt <- as.POSIXct(paste(d$date, d$time), tz = "EST", format="%Y-%m-%d %H:%M:%S")
b$dt <- as.POSIXct(paste(b$date, b$time), tz = "EST", format="%Y-%m-%d %H:%M:%S")
c$dt <- as.POSIXct(paste(c$date, c$time), tz = "EST", format="%Y-%m-%d %H:%M:%S")



#Add Tag# label
ct <- function(text, location="topright"){
  legend(location,legend=text, bty ="n", pch=NA) 
}

# average of current sample, 10 future samples, and 10 past samples (blue)
f21 <- rep(1/21,21)


#Export figure in proper size
jpeg(filename="depthtime.jpg", 
     units="in", 
     width=7, 
     height=8, 
     pointsize=12, 
     res=400)

par(mfrow = c(3, 2), mar = c(2, 4, 2, 1))

# Tag41
t41 <- d[d$tag==41,]
t41 <- t41[order(t41$date),,drop=FALSE]
p41 <- plot(t41$date, t41$depth, xlab = "", ylab = "Depth (m)", xaxt = 'n', pch = 20, col = "gray60", ylim = (c(max(t41$depth), 0)))
ax41 <- seq(floor_date(min(t41$dt), "month"), 
            ceiling_date(max(t41$dt), "month"), by="1 mon")

axis.Date(1, at=ax41, format = "%b-%y") 
# axis.Date(1, at=seq(min(t41$date), max(t41$date) + 10, by="1 mon"), labels = FALSE) 
# axis.Date(1, at=seq(min(t41$date), max(t41$date) +10, by="6 mon"), format="%b-%y")
sym_41 <- stats::filter(t41$depth, f21, sides = 2)
lines(t41$date, sym_41, col = "black")
ct("Tag 41")

# Tag42
t42 <- d[d$tag==42,]
t42 <- t42[order(t42$date),,drop=FALSE]


p42 <- plot(t42$date, t42$depth, xlab = "", ylab = "", xaxt = 'n', pch = 20, col = "gray60", ylim = (c(max(t42$depth), 0)))
ax42 <- seq(floor_date(min(t42$dt), "month"), 
            ceiling_date(max(t42$dt), "month"), by="1 mon")

axis.Date(1, at=ax42, format = "%b-%y") 
# axis.Date(1, at=seq(min(t42$date), max(t42$date) + 10, by="1 mon"), labels = FALSE) 
# axis.Date(1, at=seq(min(t42$date), max(t42$date) +10, by="3 mon"), format="%b-%y")
sym_42 <- stats::filter(t42$depth, f21, sides = 2)
lines(t42$date, sym_42, col = "black")

ct("Tag 42")


#Tag43
t43 <- d[d$tag==43,]
t43 <- t43[order(t43$date),,drop=FALSE]

p43 <- plot(t43$date, t43$depth, xlab = "", ylab = "Depth (m)", xaxt = 'n', pch = 20, col = "gray60", ylim = (c(max(t43$depth), 0)))
ax43 <- seq(floor_date(min(t43$dt), "month"), 
             ceiling_date(max(t43$dt), "month"), by="1 mon")

axis.Date(1, at=ax43, format = "%b-%y") 


# axis.Date(1, at=seq(min(t43$date), max(t43$date) + 10, by="1 mon"), labels = FALSE) 
# axis.Date(1, at=seq(min(t43$date), max(t43$date) +10, by="5 mon"), format="%b-%y")
sym_43 <- stats::filter(t43$depth, f21, sides = 2)
lines(t43$date, sym_43, col = "black")

ct("Tag 43")

#Tag59
t59 <- b[b$Tag==59,]
t59 <- t59[order(t59$date),,drop=FALSE]

p59 <- plot(t59$date, t59$depth, xlab = "", ylab = "", xaxt = 'n', pch = 20, col = "gray60", ylim = (c(max(t59$depth), 0)))

ax59 <- seq(floor_date(min(t59$dt), "month"), 
             ceiling_date(max(t59$dt), "month"), by="1 mon")
axis.Date(1, at=ax59, format = "%b-%y") 

# axis.Date(1, at=seq(min(t59$date), max(t59$date) + 30, by="1 mon"), labels = FALSE) 
# axis.Date(1, at=seq(min(t59$date), max(t59$date) +30, by="1 mon"), format="%b-%y")
sym_59 <- stats::filter(t59$depth, f21, sides = 2)
lines(t59$date, sym_59, col = "black")

ct("Tag 59")



#Tag61
t61 <- c[c$tag==61,]
t61 <- t61[order(t61$date),,drop=FALSE]

axtck <- seq(floor_date(min(t61$dt), "month"), 
           ceiling_date(max(t61$dt), "month"), by="1 mon")

p61 <- plot(t61$date, t61$depth, xlab = "", ylab = "Depth (m)", xaxt = 'n', pch = 20, col = "gray60", ylim = (c(max(t61$depth), 0)))
axis.Date(1, at=axtck, format = "%b-%y") 

# axis.Date(1, at=seq(min(t61$date), max(t61$date) + 30, by="1 mon"), labels = FALSE) 
# axis.Date(1, at=seq(min(t61$date), max(t61$date) +30, by="1 mon"), format="%b-%y")
sym_61 <- stats::filter(t61$depth, f21, sides = 2)
lines(t61$date, sym_61, col = "black")

ct("Tag 61")


#Tag63
t63 <- b[b$Tag==63,]
t63 <- t63[order(t63$date),,drop=FALSE]
ax63 <- seq(floor_date(min(t63$dt), "month"), 
            ceiling_date(max(t63$dt), "month"), by="1 mon")

p63 <- plot(t63$date, t63$depth, xlab = "", ylab = "",  pch = 20, 
            xaxt = "n", col = "gray60", ylim = (c(max(t63$depth), 0)))
axis.Date(1, at= ax63, format = "%b-%y") 
sym_63 <- stats::filter(t63$depth, f21, sides = 2)
lines(t63$date, sym_63, col = "black")

ct("Tag 63")



dev.off()

