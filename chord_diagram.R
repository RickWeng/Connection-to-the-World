### Task: Chord diagram of Julia's flight routes
### Author: Ricky

# Libraries
library(tidyverse)
library(viridis)
library(patchwork)
library(hrbrthemes)
library(circlize)

# Read data
data=read.csv(file = "Chord_flights.csv",sep =",",header=TRUE,row.names = 1)

colnames(data) <- c("BNE", "HKG", "HNL", "INC",  "LAS","LAX",   "MIA", "NKG", "PVG", "SFO", "SJC","SJO","SYD","TAO","XNN","YVR","YYZ")
rownames(data) <- colnames(data)

# I need a long format
data_long <- data %>%
  rownames_to_column %>%
  gather(key = 'key', value = 'value', -1)
data_long=data.frame(data_long)

# parameters

circos.clear()
circos.par(start.degree = 90, gap.degree = 4, track.margin = c(-0.1, 0.1), points.overflow.warning = FALSE)

par(mar = rep(0, 4),bg="black")

# color palette
mycolor <- viridis(17, alpha = 1, begin = 1, end = 0, option = "D")

# Base plot
chordDiagram(
  x = data_long,
  order=rownames(data),
  grid.col = mycolor,
  transparency = 0.1,
  directional = 1,
  direction.type = c("arrows", "diffHeight"), 
  diffHeight  = -0.04,
  annotationTrack = "grid", 
  annotationTrackHeight = c(0.05, 0.1),
  link.arr.type = "big.arrow", 
  link.sort = TRUE, 
  link.largest.ontop = TRUE)

# Add text and axis
circos.trackPlotRegion(
  track.index = 1, 
  bg.border = NA, 
  panel.fun = function(x, y) {
    
    xlim = get.cell.meta.data("xlim")
    sector.index = get.cell.meta.data("sector.index")
    
    # Add names to the sector. 
    circos.text(
      x = mean(xlim), 
      y = 3.2, 
      labels = sector.index, 
      facing = "bending", 
      cex = 1,
      col="white"
      
    )
    
    # Add graduation on axis
    circos.axis( 
      h = "top", 
      major.at = seq(from = 0, to = xlim[2], by = ifelse(test = xlim[2]>10, yes = 1, no = 1)), 
      minor.ticks = 0, 
      major.tick.percentage = 0.5,
      labels.niceFacing = FALSE,
      labels.col="white",
      col="white")
  }
)
text(-1,1,"Julia's Flight Routes 2010-2018", col="white", cex=2, pos=4)
text(-1,0.94,"Ricky Weng", col="grey", cex=1.5, pos=4)

dev.off()
ggsave("chord.png", width = 36, height = 18, units = "in", dpi = 100, bg="transparent")
dev.copy2pdf(file = paste0("chordvertical",".pdf"), height=18, width=36)
dev.copy(png, file = paste0("chord",".png"), width=36, height=18, units="in", res=500)

