#Kayla Blincow
#3/6/2020

#EDIT (5/15/2020): Mapping the Caymans Receiver Array in R as well

#Caribbean Line Map

#Need to make a map of the Caribbean to give context of where the Cayman Islands
#are

#clear my workspace
rm(list = ls())

#set the working directory
setwd("C:/Users/kmbli/OneDrive - UC San Diego/Nassau Depth/Data/MappingData")

#load the libraries
library(ggplot2)
library(ggmap)
library(maps)
library(mapdata)
library(dplyr)
library(sf)
library(ggsn)



#make the larger Caribbean map for context
world <- map_data("mapdata::worldHires")

carib <- world %>% 
  filter(long > -92 & long < -65 & lat > 16 & lat < 28.5)
  
xlabs <- c(90, 85, 80, 75, 70, 65)
ylabs <- c(15, 20, 25, 30)
map <- ggplot() + geom_polygon(data = carib, aes(x=long, y = lat,
                                          group = group),
                        fill = "gray60", color = "black") +
  labs(x = "Longitude", y = "Latitude")+
  scale_x_continuous(limits = c(-92, -68),
    labels = paste0(xlabs,'°W'),
    breaks = c(-90, -85, -80, -75, -70, -65)) +
  scale_y_continuous(
    labels = paste0(ylabs,'°N'),
    breaks = c(15, 20, 25, 30)  
  )+
  theme_classic()+
  theme(text = element_text(size=20),
        panel.border = element_rect(colour = "black", fill=NA, size=2))+
  coord_fixed(1.3, expand = FALSE)


jpeg(filename="CaribMap.jpg", 
     units="in", 
     width=12, 
     height=6, 
     pointsize=8, 
     res=400)

map

dev.off()


#Now let's make a map of just the Caymans and the Receiver Arrays
cayman <- st_read("gadm36_CYM_1.shp")
receivers <- read.table("RecLocs.txt")

caymap <- ggplot() + geom_sf(data = cayman, fill = "gray60", color = "black") +
  geom_point(data = receivers, aes(x = Long, y = Lat), size = 2) + 
  xlim(-80.13, -79.7) +
  scale_y_continuous(
    limits = c(19.62, 19.78),
    breaks = c(19.65, 19.7, 19.75)  
  ) +
  labs(NULL) +
  theme_classic()+
  theme(text = element_text(size=18, color = "black"),
        panel.border = element_rect(colour = "black", fill=NA, size=2),
        axis.title = element_blank())+
  scalebar(x.min = -79.8, x.max = -79.7,
           y.min = 19.63, y.max = 19.64,
           dist = 5, dist_unit = "km", height = 0.3, st.bottom = F,
           st.dist = 1, st.size = 5,
           transform = TRUE, model = "WGS84") +
  coord_sf()


jpeg(filename="CaymanMap.jpg", 
     units="in", 
     width=12, 
     height=6, 
     pointsize=8, 
     res=400)

caymap

dev.off()
