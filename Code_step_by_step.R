
df <- read.csv("TableS1.csv", na.strings = "NA")

tab <- data.frame(lapply(df, rep, df$count))

tab$count <- NULL

tab$Date <- as.Date(tab$Date)

tab <- tab[order(tab$Date),]  #sort by date


# plague_tab <- subset(tab, tab$Death_cause == "Plague")
# no_plague_tab <- subset(tab, tab$Death_cause != "Plague")
library(tidyverse)
library(ggplot2)

fig1 <- tab %>% 
  group_by(Date,Death_cause) %>% 
  summarize(count = n()) %>%  
  ggplot(aes(x = Date, y = count, col = Death_cause)) + 
  geom_line(size = 0.7) +
  geom_point(size = 2) +
  theme_bw() +
  scale_color_manual(values=c("grey70","red")) +
  theme(axis.text.x = element_text(angle=90, size = 10),
        axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15)) +
  labs(y = 'Number of Deaths')

pdf("Figures/Fig1.pdf", 24,9)
fig1
dev.off()



library(tidyverse)
library(reshape2)
library(factoextra)
library(ade4)
library(vegan)
library(RColorBrewer)
library(inflection)


tab_1630_peste <- droplevels(subset(tab, tab$Death_cause == "Plague"))

head(tab_1630_peste)


t_1630_peste <- as.matrix(table(tab_1630_peste$Date, tab_1630_peste$Parish))

days1630 <- seq(from=as.Date("1630-01-01"), to=as.Date("1630-12-31"), by=1)

absent_days <- as.Date(setdiff(days1630, as.Date(row.names(t_1630_peste))), origin="1970-01-01")

t_1630_peste_absent <- matrix(ncol=ncol(t_1630_peste), nrow=length(absent_days))

t_1630_peste_absent[is.na(t_1630_peste_absent)] <- 0

row.names(t_1630_peste_absent) <- as.character(absent_days)

t_1630_peste_all <- rbind(t_1630_peste, t_1630_peste_absent)

t_1630_peste_all_ord <- t_1630_peste_all[order(row.names(t_1630_peste_all)),]

peste_cum <- matrix(ncol=ncol(t_1630_peste_all_ord), nrow=0)

for (i in 2:nrow(t_1630_peste_all_ord)) {
  tmp <- colSums(t_1630_peste_all_ord[1:i,])
  peste_cum <- rbind(peste_cum, tmp)
}

peste_cum <- rbind(t_1630_peste_all_ord[1,], peste_cum)

row.names(peste_cum) <-row.names(t_1630_peste_all_ord)

peste_cum_norm <- apply(peste_cum, 2, function(x) x/max(x))

peste_cum_norm_m <- melt(peste_cum_norm)
colnames(peste_cum_norm_m) <-  (c("Date", "Parish", "Count"))
peste_cum_norm_m$Date <- as.Date(peste_cum_norm_m$Date)


Cumulative_curves <- ggplot(peste_cum_norm_m, aes(x=Date, y=Count*100, group = Parish)) + 
  geom_line() +
  theme_bw() +
  ylab("Cumulative relative frequency of plague deaths (%)")

pdf("Figures/Fig2.pdf", 10,10)
Cumulative_curves
dev.off()

weeks_of_epidemic <- 42

death_count_thr <- weeks_of_epidemic/2

parr_sel <- peste_cum[nrow(peste_cum),] > death_count_thr

peste_cum_norm_sel <- peste_cum_norm[,parr_sel]
# 
# peste_cum_norm_sel_2 <- peste_cum_norm_sel[,which(apply(peste_cum_norm_sel, 2, var) != 0)]

peste_cum_norm_melt <- melt(peste_cum_norm_sel)


colnames(peste_cum_norm_melt) <- c("Date", "Parish", "Count")

peste_cum_norm_melt$Date <- as.Date(peste_cum_norm_melt$Date)

cumulative_curves_sel <- ggplot(peste_cum_norm_melt, aes(x=Date, y=Count*100, group = Parish)) + 
  geom_line() +  
  scale_color_manual(values = c("1" =  "#1B9E77", "2" = "#D95F02")) + 
  theme_bw() +
  ylab("Cumulative relative frequency of plague deaths (%)")


pdf("Figures/Fig3.pdf", 10,10)
cumulative_curves_sel
dev.off()


dist <- dist(t(peste_cum_norm_sel))

pca <- cmdscale(dist, eig = T)

pdf("Figures/Fig4.pdf", 10,10)
plot(pca$eig)
dev.off()

pca <- cmdscale(dist, 3)


silhouette <- fviz_nbclust(peste_cum_norm_sel, kmeans, method = "silhouette", k.max=10) 

pdf("Figures/Fig5.pdf", 12,7)
silhouette
dev.off()


f <- kmeans(pca, 2)

clusters <- as.matrix(f$cluster)

clus <- clusters[as.matrix(row.names(pca)),1]




a <- adonis2(pca ~ clus, method='eu')
p_value <- a$`Pr(>F)`[1]



pdf("Figures/Fig6.pdf", 12,7)
s.class(pca, 
        as.factor(clus), 
        col = c("#42a4cf","#ed4e4e"),
        cellipse = 0,
        sub = paste("PCoA Axis1-Axis2 Permanova p-value < ", 
                    p_value, 
                    sep = ""),
        xlim = c(-3.8, 3))
dev.off()


library(RColorBrewer)
library(inflection)

peste_cum_norm_melt <- melt(peste_cum_norm_sel)
colnames(peste_cum_norm_melt) <- c("Date", "Parish", "Count")

clusters <- as.matrix(f$cluster)

peste_cum_norm_melt$Date <- as.Date(peste_cum_norm_melt$Date)
peste_cum_norm_melt$Cluster <- as.character(clusters[as.matrix(peste_cum_norm_melt$Parish), 1])

Cumulative_curves_sel_clusters <- ggplot(peste_cum_norm_melt, aes(x=Date, y=Count*100, group = Parish, color = Cluster)) + 
  geom_line() +  
  scale_color_manual(values = c("1" =  "#42a4cf", "2" = "#ed4e4e")) + 
  theme_bw() +
  ylab("Cumulative relative frequency of plague deaths (%)")


pdf("Figures/Fig7.pdf", 10,10)
Cumulative_curves_sel_clusters
dev.off()


palette <- c("#42a4cf", "#ed4e4e")

tab_clusters <- data.frame(Parish = row.names(clusters), Cluster = as.data.frame(clusters)$V1, Color = palette[as.matrix(clusters)])
head(tab_clusters)


write.csv(tab_clusters, file = "Clusters.csv", row.names = F)

#first plague case for each parish
peste_cum_norm_melt_first <- peste_cum_norm_melt[peste_cum_norm_melt$Count > 0,]
peste_cum_norm_melt_first_nodup <- peste_cum_norm_melt_first[!duplicated(peste_cum_norm_melt_first$Parish),]
peste_cum_norm_melt_first_nodup[,3:4]<-NULL
colnames(peste_cum_norm_melt_first_nodup)<-c("Date_first_death","Parish")


#Cumulative curve - 25%
peste_cum_norm_melt_25 <- peste_cum_norm_melt[peste_cum_norm_melt$Count >= 0.25,]
peste_cum_norm_melt_25_nodup <- peste_cum_norm_melt_25[!duplicated(peste_cum_norm_melt_25$Parish),]
peste_cum_norm_melt_25_nodup[,3:4]<-NULL
colnames(peste_cum_norm_melt_25_nodup)<-c("Date_25_death","Parish")


#Cumulative curve - 50%
peste_cum_norm_melt_50 <- peste_cum_norm_melt[peste_cum_norm_melt$Count >= 0.5,]
peste_cum_norm_melt_50_nodup <- peste_cum_norm_melt_50[!duplicated(peste_cum_norm_melt_50$Parish),]
peste_cum_norm_melt_50_nodup[,3:4]<-NULL
colnames(peste_cum_norm_melt_50_nodup)<-c("Date_50_death","Parish")

#Cumulative curve - 75%
peste_cum_norm_melt_75 <- peste_cum_norm_melt[peste_cum_norm_melt$Count >= 0.75,]
peste_cum_norm_melt_75_nodup <- peste_cum_norm_melt_75[!duplicated(peste_cum_norm_melt_75$Parish),]
peste_cum_norm_melt_75_nodup[,3:4]<-NULL
colnames(peste_cum_norm_melt_75_nodup)<-c("Date_75_death","Parish")

#Cumulative curve - 100%
peste_cum_norm_melt_100 <- peste_cum_norm_melt[peste_cum_norm_melt$Count >= 1,]
peste_cum_norm_melt_100_nodup <- peste_cum_norm_melt_100[!duplicated(peste_cum_norm_melt_100$Parish),]
peste_cum_norm_melt_100_nodup[,3:4]<-NULL
colnames(peste_cum_norm_melt_100_nodup)<-c("Date_100_death","Parish")

#Inflection points

infl_date_tab <- matrix(ncol=2, nrow=ncol(peste_cum_norm))
colnames(infl_date_tab) <- c("Inflection_date", "Parish")


for (i in 1:ncol(peste_cum_norm)){
  col = colnames(peste_cum_norm)[i]
  infl_date <- as.Date(bede(as.numeric(as.Date(row.names(peste_cum_norm))), peste_cum_norm[,as.matrix(col)],0)$iplast, origin = "1970-01-01")
  infl_date_tab[i, "Inflection_date"] <- as.character(infl_date)
  infl_date_tab[i, "Parish"] <- col 
}



all_tab_tmp <- merge(clusters, peste_cum_norm_melt_first_nodup, by.x="row.names", by.y="Parish")
colnames(all_tab_tmp)[1:2] <- c("Parish", "Cluster")

all_tab_tmp1 <- merge(all_tab_tmp, peste_cum_norm_melt_25_nodup, by="Parish")
all_tab_tmp2 <- merge(all_tab_tmp1, peste_cum_norm_melt_50_nodup, by="Parish")
all_tab_tmp3 <- merge(all_tab_tmp2, peste_cum_norm_melt_75_nodup, by="Parish")
all_tab_tmp4 <- merge(all_tab_tmp3, peste_cum_norm_melt_100_nodup, by="Parish")
all_tab <- merge(all_tab_tmp4, infl_date_tab, by="Parish")

all_tab$Inflection_date <- as.Date(all_tab$Inflection_date )

all_tab2 <- melt(all_tab,  id.vars = c("Parish", "Cluster"))



all_tab2$Cluster <- factor(all_tab2$Cluster, levels = c("1","2"), ordered = TRUE)

my_comparisons <- list(c("1","2"))

labels <- list("First death", "25%", "50%", "75%","100%", "Inflection date")

#label name of facet
labels <- list("First_death" = "First death",
               "Date_25_death" = "25%",  
               "Date_50_death" =  "50%",  
               "Date_75_death" =  "75%",  
               "Date_100_death" =  "100%",  
               "Inflection_date" = "Inflection date")


facet_labeller <- function(variable,value){
  return(labels[value])
}

library(ggpubr)

Clusters_boxplot <- ggboxplot(all_tab2, x = "Cluster", y = "value", fill = "Cluster") + 
  scale_fill_manual(values=as.matrix(palette)) + 
  geom_jitter(alpha=0.5, position = position_jitter(width = 0.3)) + 
  facet_wrap(~all_tab2$variable, ncol=6, labeller = facet_labeller ) + 
  stat_compare_means(comparisons = my_comparisons, method = "wilcox.test", size=4, label = "p.signif", hide.ns = TRUE) + 
  theme(legend.position = "none") +
  ylab( "Date") +
  theme(strip.text.x = element_text(size = 8))

pdf("Figures/Fig8.pdf", 15,10)
Clusters_boxplot
dev.off()


df <- read.csv("TableS1.csv", na.strings = "NA")
gps <- read.csv("TableS2.csv", na.strings = "NA")

clusters <- read.csv("Clusters.csv")

df2 <- data.frame(lapply(df, rep, df$count))
df3 <- df2 %>%  select(-count)
tab <- left_join(df2, gps, by = "Parish")
tab$Date <- as.Date(tab$Date)

tab_cluster <- left_join(tab, clusters, by = "Parish")
tab_cluster_2 <- droplevels(subset(tab_cluster, !is.na(tab_cluster$Cluster)))  
#drop rows without cluster total number of cases in the 2 clusters = 7002


tab_cluster_2$Weeks <- as.numeric(format(tab_cluster_2$Date, "%W"))
tab_cluster_2$Cluster <- factor(tab_cluster_2$Cluster, levels=c(1,2))

tab_cluster_3 <- tab_cluster_2 %>% filter(Death_cause == "Plague")


cc_clusters <- tab_cluster_3 %>% 
  group_by(Weeks,Cluster) %>% 
  summarize(count = n()) %>%  
  ggplot(aes(x = Weeks, y = count, col = Cluster)) + 
  geom_line(size = 0.7) +
  geom_point(size = 2) +
  theme_bw() +
  scale_color_manual(values=c("#42a4cf","#ed4e4e")) +
  theme(axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12)) +
  labs(y = 'Number of Deaths')


pdf("Figures/Fig9.pdf", 15,10)
cc_clusters
dev.off()

################################################################################ 4
library(png)
library(grid)

head(tab_cluster)

df <- tab_cluster %>% filter(!is.na(Latitude))   

df$Cluster[is.na(df$Cluster)] <- 0
df$Color[is.na(df$Color)] <- "grey"

df2 <- df %>% group_by(Parish, Cluster,Color, Death_cause, Latitude, Longitude) %>%  
  summarize(Count = n())

head(df2)


df2$Cluster <- factor(df2$Cluster, levels = c( "0","1","2"),ordered = TRUE)

peste_clus_gps <- df2 %>%  filter(Death_cause == "Plague")

peste_clus_gps$Death_cause <- NULL

peste_clus_gps <- peste_clus_gps[order(peste_clus_gps$Cluster),]
peste_clus_gps$Latitude <- as.numeric(peste_clus_gps$Latitude)
peste_clus_gps$Longitude <- as.numeric(peste_clus_gps$Longitude)
peste_clus_gps$Count <- as.numeric(peste_clus_gps$Count)


map <- readPNG("positron_darker_2023.png")
map_2_plot <- rasterGrob(map, interpolate=TRUE)


gps_map <-data.frame(X = c(9.145413, 9.228967),
                     Y = c(45.43978, 45.49275),
                     fid = c(1,2),
                     crop = c("BottomLeft", "TopRight"))
#fig margins
xmin <- gps_map[gps_map$crop=="BottomLeft","X"]	#Bottom Left xmin
ymin <- gps_map[gps_map$crop=="BottomLeft","Y"]	#Bottom Right ymin
xmax <- gps_map[gps_map$crop=="TopRight","X"]		#Top Right xmax
ymax <- gps_map[gps_map$crop=="TopRight","Y"]		#Top Right ymax


img_width <- ncol(map)
img_height <- nrow(map)
aspect_ratio <- img_width/img_height 
zoom <- 15

p <-	ggplot(peste_clus_gps, aes(Latitude,Longitude))+
  annotation_custom(map_2_plot, xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax)+
  geom_point(data=peste_clus_gps, aes( size = Count, color = Cluster))+
  scale_color_manual(values=c("grey30", "#42a4cf","#ed4e4e"))+
  labs(size="Plague deaths", color="Cluster") + 
  xlim(xmin,xmax)+
  ylim(ymin,ymax)+
  theme_classic()+
  theme(axis.line=element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        legend.text=element_text(size=zoom/2),
        legend.title=element_text(size=zoom/2),
        axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        legend.position = c(.92, .50) )


ggsave(p, 
       filename = "Map_Clustering_plague_+21_deaths.png",
       device="png", units = "cm", 
       width = aspect_ratio*zoom,
       height = zoom)




