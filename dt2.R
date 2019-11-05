library(lubridate)
library(tidyr)
library(plyr)
library(ggplot2)
library(ggmap)
library(sp)
library(raster)
library(grid)
library(gridExtra)
library(RColorBrewer)

register_google(key="XXX")

dt3 <- read.csv("Traffic_Collision_Data_from_2010_to_Present.csv")
dt3$Date.Occurred <- mdy(as.character(dt3$Date.Occurred))
dt3$year <- year(dt3$Date.Occurred)
dt3 <- dt3[dt3$year %in% c(2015,2016,2017,2018),]

temp <- gsub(".*\\((.*)\\).*", "\\1", dt3$Location)
temp <- data.frame(temp)
temp <- temp %>% separate(temp, c("lat", "lon"), ", ")
dt3 <- cbind(dt3,temp)
dt3$lat <- as.numeric(dt3$lat)
dt3$lon <- as.numeric(dt3$lon)





dt4 <- rbind(read.csv("ad_viz_plotval_data_2018.csv"),read.csv("ad_viz_plotval_data_2017.csv"),read.csv("ad_viz_plotval_data_2016.csv"),read.csv("ad_viz_plotval_data_2015.csv"))
  
dt4$Date <- mdy(as.character(dt4$Date))


temp <- aggregate(dt4$DAILY_AQI_VALUE, list(dt4$Date), mean)
temp$car <- ddply(dt3, .(Date.Occurred), c("nrow"))[,2]
# temp.2 <- ddply(dt3, .(Date.Occurred), c("nrow"))
# temp<- rbind(temp[1:622,])
# temp[622:625,]
# which(temp$Group.1!=temp.2$Date.Occurred)
names(temp)<-c("day","aqi","car")
temp$month <- month(temp$day)
temp$month <- month.abb[temp$month]
temp$month <- factor(temp$month, levels=month.abb[1:12])
temp$aqi <- temp$aqi/max(temp$aqi)
temp$car <- temp$car/max(temp$car)


ggplot(temp)+
  geom_line(aes(day,aqi))+
  geom_line(aes(day,car,col="red"))

p.1 <-
ggplot(temp,aes(aqi,car))+
  geom_point(aes(col=factor(month)))+
  geom_smooth(method = "lm")+
  labs(title = "Car accident as a function of Air Quality Index", x = "Normalzied AQI", y ="Normalzied Car Accident")+
  theme_bw()+
  theme(plot.title=element_text(family="Times", face="bold", size=12),axis.text.x =element_text(family="Times", size=8),axis.text.y =element_text(family="Times", size=8),legend.position="None",aspect.ratio = 0.8,panel.spacing = unit(0.5, "lines"),text=element_text(size=16,  family="Times"))

p.2 <-
  ggplot(temp,aes(aqi,car))+
  geom_point(aes(col=factor(month)))+
  geom_smooth(method = "lm")+
  labs(title = "", x = "Normalzied AQI", y ="Normalzied Car Accident")+
  theme_bw()+
  theme(plot.title=element_text(family="Times", face="bold", size=12),axis.text.x =element_text(family="Times", size=8),axis.text.y =element_text(family="Times", size=8),legend.position="None",aspect.ratio = 0.8,panel.spacing = unit(0.5, "lines"),text=element_text(size=12,  family="Times"))+
  facet_wrap(~month)

pdf("p1.pdf")
grid.arrange(p.1,p.2,nrow=1)
dev.off()


# geospatial plot
m <- get_map("Los Angels",source="stamen")
dt3.tp <- dt3[month(dt3$Date.Occurred)==9,]
temp <- ddply(dt3.tp, .(Location), c("nrow"))
names(temp) <- c("Location","car")
temp$car <- temp$car/max(temp$car)
cor <- gsub(".*\\((.*)\\).*", "\\1", temp$Location)
cor <- data.frame(cor)
cor <- cor %>% separate(cor, c("lat", "lon"), ", ")
temp <- cbind(temp,cor)
temp$lat <- as.numeric(temp$lat)
temp$lon <- as.numeric(temp$lon)







# car accident map
# ggplot()+
p.3 <- ggmap(m, base_layer = ggplot(aes(x = lon, y = lat, size = car), data = temp))+ 
  stat_density_2d(data = temp, aes(x = lon, y = lat, fill = stat(level)), alpha = .2, bins = 25, geom = "polygon") +
  labs(title = 'Car Accident Map in LA',x="Longitude",y="Latitude")+
  scale_fill_gradientn(colors = brewer.pal(7, "YlOrRd"))+theme(plot.title=element_text(family="Times", face="bold", size=12),axis.text.x =element_text(family="Times", size=8),axis.text.y =element_text(family="Times", size=8),legend.position = 'none',text=element_text(size=12,  family="Times"))



# AQI map
dt4 <- dt4[ as.numeric(dt4$SITE_LATITUDE)<34.3 & as.numeric(dt4$SITE_LONGITUDE) < -118.2,]

p.4<- ggmap(m, base_layer = ggplot(aes(x = SITE_LONGITUDE, y = SITE_LATITUDE, size = DAILY_AQI_VALUE), data = dt4[month(dt4$Date)==1:12,]))+
  stat_density_2d(data = dt4[month(dt4$Date)==1:12,], aes(x = SITE_LONGITUDE, y = SITE_LATITUDE, fill = stat(level)), alpha = .2, bins = 25, geom = "polygon") +
  scale_fill_gradientn(colors = brewer.pal(7, "YlOrRd"))+
  labs(title = 'AQI index Weather Map in LA',x="Longitude",y="Latitude")+
  theme(plot.title=element_text(family="Times", face="bold", size=12),axis.text.x =element_text(family="Times", size=8),axis.text.y =element_text(family="Times", size=8),legend.position = 'none',text=element_text(size=12,  family="Times"))



pdf("p2.pdf")
grid.arrange(p.3,p.4,nrow=1)
dev.off()


