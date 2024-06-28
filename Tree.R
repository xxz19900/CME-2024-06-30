
setwd("/Users/xizhanxu/Learning/Tongren/Projects/AK")

rm(list = ls())
library(pacman)
pacman::p_load(ggplot2,ggstar,ggtreeExtra,ggtree,
               treeio,ggnewscale,ggsci); options(warn = -1)

library(tidytree)
library(ggtree)
library(ggtreeExtra)

tree <- read.newick("data/ITS_filtered.nwk",node.label = "support")

#node.label = "support"将node label解析为support value，另存为树注释数据
group <- read.table("data/ITS_group.txt",header = T,sep="\t")

row.names(group) <- group$Sample

#读入分组文件用于绘制条行图，并按分组添加颜色
group_file <- read.table("data/ITS_group.txt",header = T, sep = "\t")

group_file$Sample <- gsub(" ","_", group_file$Sample)
row.names(group_file) <- group_file$Sample
group_file$Group <- 
groupInfo <- split(row.names(group_file), group_file$Group)

p <- groupOTU(tree, groupInfo) %>%
  ggtree(branch.length = "none", linetype=1, size=0.8,ladderize = T, aes(color=group))

p

MRCA(tree, "Aspergillus_flavus|FKCB-044", "Cordyceps_bassiana|FKCB-036")
MRCA(tree, "Fusarium_proliferatum|FKCB-018", "Fusarium_moniliforme|FKCB-001")
MRCA(tree, "Fusarium_oxysporum|FKCB-032", "Fusarium_solani|FKCB-010")
MRCA(tree, "Fusarium_solani|FKCB-022", "Fusarium_solani|FKCB-029")

MRCA(tree, "Fusarium_solani|FKCB-022", "Fusarium_solani|FKCB-012")
MRCA(tree, "Fusarium_solani|FKCB-027", "Fusarium_solani|FKCB-025")
MRCA(tree, "Fusarium_solani|FKCB-021", "Fusarium_solani|FKCB-023")
MRCA(tree, "Fusarium_solani|FKCB-016", "Fusarium_solani|FKCB-014")
MRCA(tree, "Fusarium_solani|FKCB-029", "Fusarium_solani|FKCB-014")

p1 <- p + geom_text(aes(label=node), hjust=-3,size=3)+
  scale_color_nejm()+labs(color="")+
  geom_hilight(node=58,fill="#1E90FF",type="rect",alpha=0.6)+
  geom_hilight(node=14:17,fill="#FF8C00",type="rect",alpha=0.6)+
  geom_hilight(node=67,fill="#4DAF4A",type="rect",alpha=0.6)+
  geom_hilight(node=c(83,77,78,79,80,35),fill="#984EA3",type="rect",alpha=0.6)

p1

p2 <- p1 + geom_point2(aes(subset=!isTip,
                           fill=support),shape=21,size=2)+
  scale_fill_continuous(low='green',high='red')


p2

p3 <- p2 + geom_strip(10,11,label = "Group I", align = T, alpha=.8,family="Arial",
                      fontsize=5,offset = 7, color = "#1E90FF",offset.text = 2 ,
                      hjust="center",barsize = 5,extend = 1)+
  geom_strip(14,17,label = "Group II", align = T, alpha=.8,family="Arial",
             fontsize=5,offset = 7, color ="#FF8C00",offset.text = 2 ,
             hjust="center",barsize = 5,extend = 1,angle=66)+
  geom_strip(30,20,label = "Group III", align = T, alpha=.8,family="Arial",
             fontsize=5,offset = 7, color = "#4DAF4A",offset.text = 2 ,
             hjust="center",barsize = 5,extend = 1,angle=0)+
  geom_strip(52,35,label = "Group V", align = T, alpha=.8,family="Arial",
             fontsize=5,offset = 7, color = "#984EA3",offset.text = 2 ,
             hjust="center",barsize = 5,extend = 1,angle=-50)

p3

pp3 <- groupOTU(tree,groupInfo) %>%
  ggtree(branch.length = "none",layout = "circular",
         linetype=1,size=0.8,ladderize = T,aes(color=group))+
  #geom_text(aes(label=node), hjust=-3,size=3) +
  geom_tiplab(size=2,hjust = -0.1)+
  scale_color_nejm()+labs(color="")+
  geom_hilight(node=58,fill="#1E90FF",type="rect",alpha=0.6)+
  geom_hilight(node=14:17,fill="#FF8C00",type="rect",alpha=0.6)+
  geom_hilight(node=67,fill="#4DAF4A",type="rect",alpha=0.6)+
  geom_hilight(node=c(83,77,78,79,80,35),fill="#984EA3",type="rect",alpha=0.6)+
  geom_point2(aes(subset=!isTip,fill=support),shape=21,size=2)+
  scale_fill_continuous(low='green',high='red')+
  geom_strip(10,11,label = "Group I", align = T, alpha=.8,
             fontsize=5,offset = 18, color = "#1E90FF",offset.text = 2 ,
             hjust="center",barsize = 5,extend = 1,angle = 45)+
  geom_strip(14,17,label = "Group II", align = T, alpha=.8,
             fontsize=5,offset = 18, color ="#FF8C00",offset.text = 2 ,
             hjust="center",barsize = 5,extend = 1,angle=-15)+
  geom_strip(30,20,label = "Group III", align = T, alpha=.8,
             fontsize=5,offset = 18, color = "#4DAF4A",offset.text = 2 ,
             hjust="center",barsize = 5,extend = 1,angle=-70)+
  geom_strip(52,35,label = "Group IV", align = T, alpha=.8,
             fontsize=5,offset = 18, color = "#984EA3",offset.text = 2.5 ,
             hjust="center",barsize = 5,extend = 1,angle=-10)+
  labs(fill = "Bootstrap")

pp3

ggsave(pp3,file="Figure/ITS_tree.pdf",width =8.08,height =6.49,units="in",dpi=300)


ggtree(tree,branch.length = "none",layout = "circular",
       linetype=1,size=1,ladderize = F,aes(color=group))+
  #geom_text(aes(label=node), hjust=-3,size=1) +
  #展示节点信息，并根据节点信息添加外圈分组
  scale_color_npg()+guides(color=FALSE)+
  #guides(color=FALSE) 移除图例
  geom_fruit(data=group,
             geom=geom_bar,
             mapping=aes(y=Sample, x=Length,fill=Group),
             orientation="y",stat="identity",color="white")+
  scale_fill_npg()+labs(fill = "")+new_scale_fill()+
  geom_strip(53,116, #geom_strip()根据节点添加外部条带，后跟节点位置信息
             label = "Group I", align = T, alpha=.8,family="Arial",
             fontsize=4,offset = 3, color = "orange",offset.text = 2 ,
             hjust="center",barsize = 4,extend = 0.5)+
  #offset.text调整label位置；extend调整条带之间间距；offset设置距离节点的位置；
  #hjust="center"将lable居中放置
  geom_strip(35,52,
             label = "Group II", align = T, alpha=.8,family="Arial",
             offset = 3, color = "green",offset.text = 2 ,fontsize=4,
             hjust="center",barsize = 4,extend = 0.5,angle = 45)+
  geom_strip(20,34,
             label = "Group III", align = T, alpha=.8,family="Arial",
             offset = 3, color = "red",offset.text = 2 ,fontsize=4,
             hjust="center",barsize = 4,extend = 0.5)+
  geom_strip(1,19,
             label = "Group IV", align = T, alpha=.8,family="Arial",
             offset = 3, color = "blue",offset.text = 2 ,fontsize=4,
             hjust="center",barsize = 4,extend = 0.5,angle=-60)+
  #geom_tiplab(hjust = -.5,size=3,fontface="plain")+
  #设置标签显示
  geom_point2(aes(subset=!isTip,fill=support),
              shape=21,size=2)+
  scale_fill_continuous(low='green', high='red')+
  labs(fill = "bootstrap") 

############################### AK Tree ############################### 
############################## AK 18S tree ################################
library(ape)
library(tidytree)

tree <- read.newick("data/AK_DF3_3.tree",node.label = "support")

#node.label = "support"将node label解析为support value，另存为树注释数据
group <- read.table("data/genotype.txt",header = T,sep="\t")
row.names(group) <- group$Sample
group$Source <- factor(group$Source, levels = c("Cornea", "Contact lens case", "Contact lens solution"))
group$Subgenotype <- factor(group$Subgenotype, levels = c("T1", "T2", "T3", "T4A", "T4B",
                                                        "T4C", "T4D","T4E", "T4F", "T4Neff",
                                                        "T5","T10", "T11", "T12", "T15"))

group$Time <- factor(group$Time, levels = unique(group$Time)[order(unique(group$Time), decreasing = T)])

groupInfo <- split(row.names(group), group$Subgenotype)

tr <- groupOTU(tree, groupInfo, "Genotype")

### library(ggsci)
library(scales)
pal <- pal_npg("nrc")(10)
show_col(pal)


genotytecolors <- c("0" = "grey50","T1" = "#DC0000FF","T2" = "#800080", "T3" = "#9ACD32","T4A" = "#D15FEE","T4B" = "#FFC0CB",
              "T4C" = "#EE6A50","T4D" = "#7E6148FF", "T4E" = "#006400","T4F" = "#800000",
              "T4Neff" = "#B0171F","T5" = "#191970", "T10" = "#FFC125","T11" = "#87CEFA","T12" = "#7B68EE","T15" = "#808080")


p <- ggtree(tr=tr, layout="circular", open.angle = 10, branch.length='none', size= 0.2, aes(colour=Genotype))+ 
  scale_colour_manual(
    values=genotytecolors,
    labels=c("","T1", "T2", "T3","T4A", "T4B", "T4C",
             "T4D","T4E", "T4F", "T4Neff", "T5", "T10", "T11", "T12", "T15"),
    guide=guide_legend(keywidth=0.6,
                       keyheight=0.6,
                       ncol = 2,
                       order=1,
                       override.aes=list(linetype=c("0"=NA,
                                                    "T1"=1,
                                                    "T2"=1,
                                                    "T3"=1,
                                                    "T4A"=1,
                                                    "T4B"=1,
                                                    "T4C"=1,
                                                    "T4D"=1,
                                                    "T4E"=1,
                                                    "T4F"=1,
                                                    "T4Neff"=1,
                                                    "T5"=1,
                                                    "T10"=1,
                                                    "T11"=1,
                                                    "T12"=1,
                                                    "T15"=1))
                               
    )
  )+
  theme(
    legend.title=element_text(size=10), 
    legend.text=element_text(size=8),
    legend.spacing.y = unit(0.02, "cm")
  )

p2 <- flip(p, 1036, 1128) %>%
  flip(1033, 1002) %>%
  flip(1162, 1176) %>%
  flip(1201, 1252) %>%
  flip(1200,1158)

p3 <- p2 + new_scale_fill()+
  geom_fruit(data = group,
    geom=geom_tile,
    mapping=aes(y=Sample, fill=Subgenotype),
    width=2,
    offset=0.05
  ) +
  scale_fill_manual(
    name="Genotype",
    values=genotytecolors,
    guide=guide_legend(keywidth=0.3, keyheight=0.3, ncol=2, order=2)
  ) +
  theme(
    legend.title=element_text(size=10), 
    legend.text=element_text(size=8),
    legend.spacing.y = unit(0.02, "cm")
  )

p3

p4 <- p3 + new_scale_fill()+
  geom_fruit(data = group,
             geom=geom_tile,
             mapping=aes(y=Sample, fill=Continent),
             width=2,
             offset=0.05
  ) +
  scale_fill_manual(
    name="Continent",
    values=countrycolors$country__colour[30:35],
    guide=guide_legend(keywidth=0.3, keyheight=0.3, ncol=2, order=3)
  ) +
  theme(
    legend.title=element_text(size=10), 
    legend.text=element_text(size=8),
    legend.spacing.y = unit(0.02, "cm")
  )

p5 <- p4 + new_scale_fill()+
  geom_fruit(data = group,
             geom=geom_tile,
             mapping=aes(y=Sample, fill=Time),
             width=2,
             offset=0.05
  ) +
  scale_fill_manual(
    name="Year",
    values=yearcolors$year__colour[1:26],
    guide=guide_legend(keywidth=0.3, keyheight=0.3, ncol=2, order=4)
  ) +
  theme(
    legend.title=element_text(size=10), 
    legend.text=element_text(size=8),
    legend.spacing.y = unit(0.02, "cm")
  )


## Time genotype
time_genotype <- table(group$Time, group$Subgenotype) %>% 
  as.data.frame()

colnames(time_genotype) <- c("Time", "Genotype", "Count")
time_genotype$Time <- as.numeric(as.character(time_genotype$Time))
time_genotype$Category <- ifelse(time_genotype$Genotype %in% c("T4A", "T4B", "T4C","T4D","T4E","T4F", "T4Neff"), "Genotype T4", "Other genotype")

library(cowplot)

genotytecolors2 <- c("T1" = "#DC0000FF","T2" = "#800080", "T3" = "#9ACD32","T4A" = "#D15FEE","T4B" = "#FFC0CB",
                     "T4C" = "#EE6A50","T4D" = "#7E6148FF", "T4E" = "#006400","T4F" = "#800000",
                     "T4Neff" = "#B0171F","T5" = "#191970", "T10" = "#FFC125","T11" = "#87CEFA","T12" = "#7B68EE","T15" = "#808080")

gtime <- ggplot(time_genotype, aes(x=Time, y=Count, fill=Genotype)) + 
  geom_area(alpha=0.6 , size=0.2, colour="black")+
  scale_fill_manual(values = genotytecolors2)+
  scale_y_continuous(expand = c(0,0))+
  scale_x_continuous(breaks=seq(1994,2021,4))+
  theme_cowplot()+
  facet_wrap(~Category, scales = "free", ncol = 1)+
  xlab("Year of isolation")+
  ylab("Number of isolates")


### Continent-genotype
continent_genotype <- table(group$Continent, group$Subgenotype) %>% 
  as.data.frame()

colnames(continent_genotype) <- c("Continent", "Genotype", "Count")

ggplot(continent_genotype, aes(x=Genotype, y=Count, fill=Genotype)) + 
  geom_bar(position="dodge", stat="identity")+
  scale_fill_manual(values = genotytecolors2)+
  lims(y=c(0,102))+
  scale_y_continuous(expand = c(0,0))+
  facet_wrap(~Continent, scales = "fixed")+
  theme_cowplot()+
  ylab("Number of isolates")+
  xlab(NULL)+
  theme(
    legend.title=element_text(size=10), 
    legend.text=element_text(size=8),
    legend.spacing.y = unit(0.02, "cm"),
    axis.text.x = element_text(size = 6, angle = 30, hjust = 0.6)
  )

library(treemap)
gt <- table(group$Subgenotype) %>%
  as.data.frame()

colnames(gt) <- c("Genotype", "Count")
gt <- gt %>% arrange(desc(Count),Genotype)

# treemap(gt,
#         index=c("Genotype"),
#         vSize="Count",
#         type="index"
# ) 


# library
library(tidyverse)

# Create dataset
data <- gt

# Set a number of 'empty bar'
empty_bar <- 15

# Add lines to the initial dataset
to_add <- matrix(NA, empty_bar, ncol(data))
colnames(to_add) <- colnames(data)
data <- rbind(data, to_add)
data$id <- seq(1, nrow(data))
data = data %>% arrange(desc(Count))

# Get the name and the y position of each label
label_data <- data
number_of_bar <- nrow(label_data)
angle <- 90 - 360 * (label_data$id-0.5) /number_of_bar     # I substract 0.5 because the letter must have the angle of the center of the bars. Not extreme right(1) or extreme left (0)
label_data$hjust <- ifelse( angle < -90, 1, 0)
label_data$angle <- ifelse(angle < -90, angle+180, angle)

# Make the plot
ggplot(data, aes(x=as.factor(id), y=Count, fill= Genotype)) +       # Note that id is a factor. If x is numeric, there is some space between the first bar
  geom_bar(stat="identity", width = 0.8) +
  scale_fill_manual(values = genotytecolors2)+
  ylim(-250,326) +
  theme_minimal() +
  theme(
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    plot.margin = unit(rep(-1,4), "cm") 
  ) +
  coord_polar(start = 30, theta = "x") + 
  geom_text(data=label_data, aes(x=id, y=Count+10, label=Count, hjust=hjust), color="black", fontface="bold",alpha=0.6, size=2.5, angle= label_data$angle, inherit.aes = FALSE )+
  geom_segment(aes(x = 1, y = -5, xend = 15, yend = -5), colour = "black", alpha=0.8, size=0.6 , inherit.aes = FALSE )

p

# library
library(tidyverse)

# Create dataset
data <- table(group$Continent, group$Subgenotype) %>%
  as.data.frame()
colnames(data) <- c("Continent", "Genotype", "Count")
data$Continent <- factor(data$Continent)
# Set a number of 'empty bar' to add at the end of each group
empty_bar <- 4
to_add <- data.frame( matrix(NA, empty_bar*nlevels(data$Continent), ncol(data)) )
colnames(to_add) <- colnames(data)
to_add$Continent <- rep(levels(data$Continent), each=empty_bar)
data <- rbind(data, to_add)
data <- data %>% arrange(Continent,desc(Count))
data <- data[data$Count != 0, ]
data$id <- seq(1, nrow(data))

# Get the name and the y position of each label
label_data <- data
number_of_bar <- nrow(label_data)
angle <- 90 - 360 * (label_data$id-0.5) /number_of_bar     # I substract 0.5 because the letter must have the angle of the center of the bars. Not extreme right(1) or extreme left (0)
label_data$hjust <- ifelse( angle < -90, 1, 0)
label_data$angle <- ifelse(angle < -90, angle+180, angle)

# prepare a data frame for base lines
base_data <- data %>% 
  group_by(Continent) %>% 
  summarize(start=min(id), end=max(id)) %>% 
  rowwise() %>% 
  mutate(title=mean(c(start, end))) %>%
  na.omit()

# angle_label <- c(6,-95,35.1,100,8,60)

# Make the plot
p <- ggplot(data, aes(x=as.factor(id), y=Count, fill=Genotype)) +       # Note that id is a factor. If x is numeric, there is some space between the first bar
  scale_fill_manual(values = genotytecolors2)+
  geom_bar(stat="identity") +
  ylim(-100,105) +
  theme_minimal() +
  theme(
    legend.position = "none",
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    plot.margin = unit(rep(-1,4), "cm") 
  ) +
  coord_polar() + 
  geom_text(data=label_data, aes(x=id, y=Count+2, label=Count, hjust=hjust), color="black", fontface="bold",alpha=0.6, size=2.5, angle= label_data$angle, inherit.aes = FALSE )+
  geom_segment(data=base_data, aes(x = start, y = -5, xend = end, yend = -5), colour = "black", alpha=0.8, size=0.6 , inherit.aes = FALSE )+
  geom_text(data=base_data, aes(x = title, y = -7, label=Continent),hjust="inward",vjust="inward", colour = "black", alpha=0.8, size=3, fontface="bold", inherit.aes = FALSE)

p

library(ggsci)

################################################# AK genotype distribution  #############################
# library
library(tidyverse)

# Create dataset
data <- read.csv("data/AK-distribution.csv")
colnames(data) <- c("Continent", "Genotype", "Count")
data$Continent <- factor(data$Continent)
# Set a number of 'empty bar' to add at the end of each group
empty_bar <- 4
to_add <- data.frame( matrix(NA, empty_bar*nlevels(data$Continent), ncol(data)) )
colnames(to_add) <- colnames(data)
to_add$Continent <- rep(levels(data$Continent), each=empty_bar)
data <- rbind(data, to_add)
data <- data %>% arrange(Continent,desc(Count))
data <- data[data$Count != 0, ]
data$id <- seq(1, nrow(data))

# Get the name and the y position of each label
label_data <- data
number_of_bar <- nrow(label_data)
angle <- 90 - 360 * (label_data$id-0.5) /number_of_bar     # I substract 0.5 because the letter must have the angle of the center of the bars. Not extreme right(1) or extreme left (0)
label_data$hjust <- ifelse( angle < -90, 1, 0)
label_data$angle <- ifelse(angle < -90, angle+180, angle)

# prepare a data frame for base lines
base_data <- data %>% 
  group_by(Continent) %>% 
  summarize(start=min(id), end=max(id)) %>% 
  rowwise() %>% 
  mutate(title=mean(c(start, end))) %>%
  na.omit()

# angle_label <- c(6,-95,35.1,100,8,60)

# Make the plot
p <- ggplot(data, aes(x=as.factor(id), y=Count, fill=Genotype)) +       # Note that id is a factor. If x is numeric, there is some space between the first bar
  scale_fill_manual(values = genotytecolors2)+
  geom_bar(stat="identity") +
  ylim(-80,20) +
  theme_minimal() +
  theme(
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    plot.margin = unit(rep(-0.5,4), "cm") 
  ) +
  coord_polar() + 
  geom_text(data=label_data, aes(x=id, y=Count+2, label=Count, hjust=hjust), color="black", fontface="bold",alpha=0.6, size=2.5, angle= label_data$angle, inherit.aes = FALSE )+
  geom_segment(data=base_data, aes(x = start, y = -5, xend = end, yend = -5), colour = "black", alpha=0.8, size=0.6 , inherit.aes = FALSE )+
  geom_text(data=base_data, aes(x = title, y = -7, label=Continent),hjust="inward",vjust="inward", colour = "black", alpha=0.8, size=3, fontface="bold", inherit.aes = FALSE)

p

## Group
library(ggalluvial)
allu <- read.csv("data/Genotype_alluvial.csv")
allu$Region <- factor(allu$Region)
allu$Genotype <- factor(allu$Genotype)
allu$Group <- factor(allu$Group)

ggplot(allu,
       aes(y = Count, axis1 = Region, axis2 = Group)) +
  scale_x_discrete(limits = c("Region", "Group"), expand = c(.2, .05)) +
  geom_alluvium(aes(fill = Genotype), width = 1/5) +
  geom_stratum() +
  geom_text(stat = "stratum", aes(label = after_stat(stratum))) +
  scale_fill_manual(values = c("#6485b0","#f36563","#84cc82","#69bacd","#b495be","#fdc8b6","#c66375"))+
  ggtitle("Association between region and genotype")+
  theme_minimal()

### Pie chart
pie <- read.csv("data/Genotype_pie.csv")
pie$Region <- as.factor(pie$Region)
dt_north <- pie[pie$Region %in% c("North"), ]
dt_south <- pie[pie$Region %in% c("South"), ]

dt_north$Fraction <- dt_north$Count/sum(dt_north$Count)*100
dt_south$Fraction <- dt_south$Count/sum(dt_south$Count)*100

dt_north$Fraction <- round(dt_north$Fraction, 2)
dt_north$Fraction[2] <- 9.83
dt_south$Fraction <- round(dt_south$Fraction, 2)

dt_north$Genotype <- as.factor(dt_north$Genotype)

pie$Genotype[8:14] <- paste0(pie$Genotype[8:14], "1")
pie$Genotype <- as.factor(pie$Genotype)

# ggpubr
library(ggpubr)

ggdotchart(pie, y = "Count", x = "Genotype",
           color = "Genotype",                                # Color by groups
           palette = c("#6485b0","#f36563","#84cc82","#69bacd","#b495be","#fdc8b6","#c66375",
                       "#6485b0","#f36563","#84cc82","#69bacd","#b495be","#fdc8b6","#c66375"), # Custom color palette
           sorting = "descending",                       # Sort value in descending order
           add = "segments",                             # Add segments from y = 0 to dots
           rotate = TRUE,                                # Rotate vertically
           group = "Region",                                # Order by groups
           dot.size = 6,                                 # Large dot size
           label = "Count",                        # Add mpg values as dot labels
           font.label = list(color = "white", size = 9, 
                             vjust = 0.5),               # Adjust label parameters
           ggtheme = theme_pubr()                        # ggplot2 theme
)


# Get the names, sorted first by lg, then by avg
pie <- pie[order(pie$Region, pie$Count), ]

# Turn name into a factor, with levels in the order of nameorder
pie$Genotype <- factor(pie$Genotype, levels = pie$Genotype)

ggplot(pie, aes(x = Count, y = Genotype)) +
  geom_segment(aes(yend = Genotype), xend = 0, colour = "grey50") +
  geom_point(size = 3, aes(colour = Region)) +
  scale_colour_brewer(palette = "Set1", limits = c("North", "South"), guide = "none") +
  theme_bw() +
  theme(panel.grid.major.y = element_blank()) +
  facet_grid(Region ~ ., scales = "free_y", space = "free_y")


## donut plot
lab1 <- paste0(dt_north$Genotype, " (", dt_north$Fraction, "%)")

ggdonutchart(dt_north, "Fraction", label = lab1,
             lab.pos = "in",
             fill = "Genotype", color = "white",
             palette = c("#6485b0","#f36563","#84cc82","#69bacd","#b495be","#fdc8b6","#c66375"),
             orientation = "horizontal")

lab2 <- paste0(dt_south$Genotype, " (", dt_south$Fraction, "%)")
ggdonutchart(dt_south, "Fraction", label = lab2,
             lab.pos = "in",
             fill = "Genotype", color = "white",
             palette = c("#6485b0","#f36563","#84cc82","#69bacd","#b495be","#fdc8b6","#c66375"),
             orientation = "horizontal")



