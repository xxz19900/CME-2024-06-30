
setwd("/Users/xizhanxu/Learning/Tongren/Projects/Pengyan")

#下载绘制中国地图所需的各种R包
BiocManager::install("maps")
BiocManager::install("mapdata")
BiocManager::install("maptools")
BiocManager::install("sf")

#加载R包
library(maps)
library(mapdata)
library(maptools)
library(sf)
library(rgdal)

#读取中国行政区地图数据
china_map<-sf::st_read('map_data/bou2_4p.shp')
plot(china_map$geometry)

#给地图加点中国红
plot(china_map$geometry,col="red")

#china_map数据中包含了925条记录，每条记录中都含有，面积（AREA），周长（PERIMETER），BOU2_4M、BOU2_4M_ID、ADCODE93、ADCODE99、中文名（NAME),几何形状(geometry)等字段，其中中文名（NAME）字段是以GBK编码的，需要iconv函数转化一下才能正常显示。
length(china_map$geometry)

names(china_map)

#利用iconv 格式转换函数来转换各省名称
table(iconv(china_map$NAME, from = "GBK"))

iconv(china_map$NAME, from = "GBK")

#河北行政区化图
plot(china_map[7,]$geometry,col="red")

#### 
library(ggplot2)
library(sf)
library(geojsonsf)
library(RColorBrewer)
## 通过阿里云获得中国地图
# 地图选择器网址 http://datav.aliyun.com/tools/atlas/index.html

map_china = read_sf("https://geo.datav.aliyun.com/areas_v2/bound/100000_full.json")
# 或
# map_china = read_sf("https://geo.datav.aliyun.com/areas_v3/bound/geojson?code=100000_full")

###
library(geojsonsf)
library(sf)
library(ggplot2)
library(RColorBrewer)
library(openxlsx)
library(showtext)

API_pre = "http://xzqh.mca.gov.cn/data/"
## 1.全国
China = st_read(dsn = paste0(API_pre, "quanguo.json"), 
                stringsAsFactors=FALSE) 
st_crs(China) = 4326

# 2.国境线
China_line = st_read(dsn = paste0(API_pre, "quanguo_Line.geojson"), 
                     stringsAsFactors=FALSE) 
st_crs(China_line) = 4326

gjx <- China_line[China_line$QUHUADAIMA == "guojiexian",]

# 3.读取省份地理中心
# 地图中心坐标：基于st_centroid和省会坐标以及部分调整值
province_mid <- read.csv("map_data/province.csv",encoding = "UTF-8")

# 4.着色数据+全国地图
zhuose_data <- read.xlsx("map_data/your_data.xlsx")
zhuose_data$QUHUADAIMA <- as.character(zhuose_data$QUHUADAIMA) # 因China数据中QUHUADAIMA是chr类型
CHINA <- dplyr::left_join(China,zhuose_data,by= "QUHUADAIMA")

###----全国地图完整（无右下角小地图）----------###

ggplot()+
  # 绘制主图
  geom_sf(data = CHINA,aes(fill = factor(yanse))) +
  scale_fill_manual("class", values=c("#FFCCCC", "#FF9333", "#FF6660","#FF5111","#CC0070"),
                    breaks = c("0~200","200~400","400~600","600~1000","1000+"),
                    labels = c("0~200","200~400","400~600","600~1000","1000+"))+
  # 绘制国境线及十/九段线
  geom_sf(data = gjx)+
  geom_text(data = province_mid,aes(x=dili_Jd,y=dili_Wd,label=省市),
            position = "identity",size=3,check_overlap = TRUE) +
  labs(title="中国地图",subtitle="随机着色",caption = "reference")+
  theme(
    plot.title = element_text(color="red", size=16, face="bold",vjust = 0.1,hjust = 0.5),
    plot.subtitle = element_text(size=10,vjust = 0.1,hjust = 0.5),
    legend.title=element_blank(),
    legend.position = c(0.2,0.2),
    panel.grid=element_blank(),
    panel.background=element_blank(),
    axis.text=element_blank(),
    axis.ticks=element_blank(),
    axis.title=element_blank()
  )

ggplot()+
  # 绘制主图
  geom_sf(data = CHINA,aes(fill = factor(yanse))) +
  scale_fill_manual("class", values=c("#FFCCCC", "#FF9333", "#FF6660","#FF5111","#CC0070"),
                    breaks = c("0~200","200~400","400~600","600~1000","1000+"),
                    labels = c("0~200","200~400","400~600","600~1000","1000+"))+
  # 绘制国境线及十/九段线
  geom_sf(data = gjx)+
  geom_text(data = province_mid,aes(x=dili_Jd,y=dili_Wd,label=省市),
            position = "identity",size=3,check_overlap = TRUE) +
  theme(
    plot.title = element_text(color="red", size=16, face="bold",vjust = 0.1,hjust = 0.5),
    plot.subtitle = element_text(size=10,vjust = 0.1,hjust = 0.5),
    legend.title=element_blank(),
    legend.position = c(0.2,0.2),
    panel.grid=element_blank(),
    panel.background=element_blank(),
    axis.text=element_blank(),
    axis.ticks=element_blank(),
    axis.title=element_blank()
  )


########## hchinamap ###################

download.file('https://mdniceczx.oss-cn-beijing.aliyuncs.com/chinadf.rda', file.path(dir, 'chinadf.rda'))
load(file.path("./map_data", 'chinadf.rda'), verbose = TRUE)
chinadf

write.csv(chinadf,"map_data/chinadf.csv")###读取excel数据，该地图数据可以根据自己数据修改
Rdata<-read.csv("map_data/chinadf.csv")

library(hchinamap)
#绘制基础中国地图
hchinamap(name = Rdata$name, #要绘制的省份或城市 
          value = Rdata$value,#要可视化的数据
          width = "100%", #地图的宽度
          height = "400px",#地图的高度 
          title = "R语言绘制中国地图", #地图标题 
          minColor = "#458B74",#最小值的颜色 
          maxColor = "#8B2323",#最大值的颜色 
          region = "China"#要绘制的区域
)

#绘制基础中国地图
hchinamap(name = Rdata$name, #要绘制的省份或城市
          value = Rdata$value,#要可视化的数据  
          width = "100%", #地图的宽度  
          height = "400px",#地图的高度  
          title = "R语言绘制中国地图", #地图标题 
          minColor = "#FAEBD7",#最小值的颜色 
          maxColor = "#8B8378",#最大值的颜色  
          region = "China"#要绘制的区域
)

#绘制基础中国地图
hchinamap(name = Rdata$name, #要绘制的省份或城市 
          value = Rdata$value,#要可视化的数据  
          width = "100%", #地图的宽度  
          height = "400px",#地图的高度 
          title = "R语言绘制中国地图", #地图标题 
          minColor = "#8DEEEE",#最小值的颜色  
          maxColor = "#2F4F4F",#最大值的颜色  
          region = "China"#要绘制的区域
)

devtools::install_github("xmc811/mapchina", ref = "dev")

if(!requireNamespace('remotes',quietly = T)) install.packages('remotes',update=F)
if(!requireNamespace('onekeyMap',quietly = T)){
  tryCatch({
    remotes::install_github('biomarble/onekeyMap',dependencies=T,upgrade = F)
  },error = function(e){
    remotes::install_url('https://download.fastgit.org/biomarble/onekeyMap/archive/main.zip',dependencies = T,upgrade = F) 
  })
}

library(onekeyMap)
data <- read.csv("map_data/genotype.csv")
mapChina_pie_province(data)



