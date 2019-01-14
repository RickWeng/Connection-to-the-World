### Task: Mapping Julia's flight routes since 2010
### Author: Ricky

# Libraries
library(tidyverse)
library(jpeg)
library(maps)
library(geosphere)
library(grid)
library(ggrepel)
install.packages("extrafont")
library(extrafont)
font_import()

# Download NASA night lights image
download.file("https://www.nasa.gov/specials/blackmarble/2016/globalmaps/BlackMarble_2016_01deg.jpg", 
              destfile = "BlackMarble_2016_01deg.jpg", mode = "wb")
# Load picture and render
earth <- readJPEG("BlackMarble_2016_01deg.jpg", native = TRUE)
earth <- rasterGrob(earth, interpolate = TRUE)

# Set work space
setwd=("U:/)

attach(ConnectionFlights_Julia)
# Count how many times we have each unique connexion + order by importance
summary=ConnectionFlights_Julia %>% 
  dplyr::count(HomeLat,HomeLon,Home, TravelLat,TravelLon,Travel) %>%
  arrange(n)
# A function that makes a dateframe per connection (we will use these connections to plot each lines)
data_for_connection=function(dep_lon, dep_lat, arr_lon, arr_lat,group){
  inter <- gcIntermediate(c(dep_lon, dep_lat), c(arr_lon, arr_lat), n=50, addStartEnd=TRUE, breakAtDateLine=F)             
  inter=data.frame(inter)
  inter$group=NA
  diff_of_lon=abs(dep_lon) + abs(arr_lon)
  diff_of_lon=abs(dep_lon) + abs(arr_lon)
  if(diff_of_lon > 180){
    inter$group[ which(inter$lon>=0)]=paste(group, "A",sep="")
    inter$group[ which(inter$lon<0)]=paste(group, "B",sep="")
  }else{
    inter$group=group
  }
  return(inter)
}

# Create ready to plot data frame
data_ready_plot=data.frame()
for(i in c(1:nrow(ConnectionFlights_Julia))){
  tmp=data_for_connection(ConnectionFlights_Julia$HomeLon[i], ConnectionFlights_Julia$HomeLat[i], ConnectionFlights_Julia$TravelLon[i], ConnectionFlights_Julia$TravelLat[i],i )
  tmp$Time=ConnectionFlights_Julia$Time[i]
  tmp$n=summary$n[i]
  data_ready_plot=rbind(data_ready_plot, tmp)
}
data_ready_plot$Time=factor(data_ready_plot$Time, levels=c("2010","2014","2015","2016","2018"))

# Plot
p=ggplot() +
  annotation_custom(earth, xmin = -180, xmax = 180, ymin = -90, ymax = 90) +
  geom_line(data=data_ready_plot, aes(x=lon, y=lat, group=group,colour=Time),size=1) +
  
  geom_point(data = placetravel, aes(Lon, Lat), alpha = 0.5, size = 0.1, colour = "white") +
  geom_text(data=placetravel, aes(Lon,Lat,label=Place),color="white",alpha=1,hjust=1,vjust=0.5,fontface="bold")+
  scale_color_manual(values = c( "#899DA4","#66C2A5","#f9ba00","#075aaa","#ff0000"))+
  theme_void() +
  theme(
    legend.position="none",
    panel.background = element_rect(fill = "black", colour = "black"), 
    panel.spacing=unit(c(0,0,0,0), "null"),
    plot.margin=grid::unit(c(0,0,0,0), "cm")
  ) +
  geom_text_repel()+
  annotate("text", x = -150, y = 3, hjust = 0, size = 14, 
           label = paste("2010"), color ="#899DA4" , family = "Baker Signet Std") +
  annotate("text", x = -150, y = -4, hjust = 0, size = 14, 
           label = paste("2014"), color = "#66C2A5", family = "Baker Signet Std") + 
  annotate("text", x = -150, y = -11, hjust = 0, size = 14, 
           label = paste("2015"), color = "#f9ba00", family = "Baker Signet Std") +
  annotate("text", x = -150, y = -18, hjust = 0, size = 14, 
           label = paste("2016"), color = "#075aaa", family = "Baker Signet Std") + 
  annotate("text", x = -150, y = -25, hjust = 0, size = 14, 
           label = paste("2018"), color = "#ff0000", family = "Baker Signet Std") +
    
  ggplot2::annotate("text", x = -150, y = -45, hjust = 0, size = 11, label = paste("Julia's Flight Routes 2010-2018"), color = "white") +
  ggplot2::annotate("text", x = -150, y = -51, hjust = 0, size = 8, label = paste("Ricky Weng || NASA.gov"), color = "white", alpha = 0.5) +
  ggplot2::annotate("text", x = -150, y = -60, hjust = 0, size = 6, label = paste("Air Canada || American Airlines || Beijing Capital Airlines || China Eastern Airlines || Delta Air Lines || WestJet"), color = "white", alpha = 0.5)+
  xlim(-180,180) +
  ylim(-60,80) +
  scale_x_continuous(expand = c(0.006, 0.006)) +
  coord_equal() 
p
# Save 
ggsave("Flightstransparent.png", width = 36, height = 18,units = "in", dpi = 500, bg="transparent")
ggsave("Flightsblack.png", width = 36, height = 18,units = "in", dpi = 500, bg="black")
dev.copy2pdf(file = paste0("ff",".pdf"), height=4, width=6)
dev.copy(png, file = paste0("ff",".png"), width=36, height=18, units="in", res=600)
