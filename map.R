# work directory
setwd("/Users/xizhanxu/Learning/Tongren/Projects/AK/map")

library(ggplot2)
library(plyr)
library(maptools)
library(rgdal)

## import world map data
world_map <-readShapePoly("World_region.shp")
x <- world_map@data    #读取行政信息
xs <- data.frame(x,id=seq(0:250)-1)  #含岛屿共251个形状
world_map1 <- fortify(world_map)       #转化为数据框
world_map_data <- join(world_map1, xs, type = "full") #合并两个数据框

# import epidemo data
mydata <- read.csv("Region_map_new.csv", encoding = "UTF-8")   #读取指标数据，csv格式
world_data <- join(world_map_data, mydata, type="full")   #合并两个数据框

library(ggsci)
mypal = pal_lancet("lanonc", alpha = 0.6)(3)
library("scales")
show_col(mypal)#可视化色号
mypal#输出色号

p <- ggplot(world_data, aes(x = long, y = lat, group = group,fill = cut(zhibiao1, c(0,129,248,399,497),include.lowest = TRUE))) +
  geom_polygon(colour="white") +
  scale_fill_lancet(alpha = 0.6)+
  guides(fill=guide_legend(title="Incidence"))+
  theme(               #清除不需要的元素
    panel.grid = element_blank(),
    panel.background = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    legend.position = c(0.2,0.3)
  )#平面地图

ggsave("map.pdf")
