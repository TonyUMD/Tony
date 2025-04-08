###
library(raster)
library(sp)
#library(rgdal)
library(sf)
library(stringr)
library(ggplot2)
library(ggtext)
library(wesanderson)
library(zyp)
library(gghalves)
library(trend)
library(zyp)

atl08_spatial <- read.csv('~/Desktop/atl08_spatial_deciduous_extracted_final.csv')
atl08_spatial <- SpatialPointsDataFrame(coords = cbind(atl08_spatial$lon,atl08_spatial$lat),data = atl08_spatial,proj4string  = CRS("+init=epsg:4326"))

#Distribution of SWI0 slope
atl08_spatial_significant <- atl08_spatial[which(atl08_spatial@data$swi0_p < 0.05),]


quantile(atl08_spatial$swi0_slope[which(atl08_spatial@data$swi0_p < 0.05 &
                                          atl08_spatial@data$swi0_slope > 0)])


atl08_spatial_significant@data$warming_interval <- NA

atl08_spatial_significant@data$warming_interval[which(atl08_spatial_significant@data$swi0_slope >=0 &
                                                        atl08_spatial_significant@data$swi0_slope < 0.4896709 &
                                                        atl08_spatial_significant@data$swi0_p < 0.05)] <- "Slight Warming"

atl08_spatial_significant@data$warming_interval[which(atl08_spatial_significant@data$swi0_slope > 0.4896709 &
                                                        atl08_spatial_significant@data$swi0_p < 0.05)] <- "Rapid Warming"


ggplot(data =atl08_spatial@data[which(atl08_spatial@data$swi0_p > 0.05),],aes(x=swi0_slope,color="Non-significant Warming"))+
  geom_histogram(fill='#00A08A',bins =64)+xlab(expression('Annual Change of SWI'[0]*' (p > 0.05)'))+
  scale_color_manual(values = c("Non-significant Warming" = 'white'))+
  scale_fill_manual(values = c("Non-significant Warming" = '#00A08A'))+
  theme(axis.text.y= element_text(size=15,face = "bold"),axis.title   = element_text(size= 15),
        axis.text.x= element_text(size=15,face = "bold"),
        legend.background =  element_rect(fill = "transparent"),legend.text = element_text(size=8),legend.title = element_blank(),legend.position = c(0.8,0.85))
ggsave("//gssto1.umd.edu/gsdata1/duncansongp/fengt/icesat2_boreal_fire/figure/Manuscript_figures/annual_change_swi0_insignificant.png", width = 6, height = 3, units = "in", dpi = 300)

##Significant with negative included
ggplot(data =atl08_spatial@data[which(atl08_spatial@data$swi0_p <= 0.05),],aes(x=swi0_slope))+
  geom_histogram(color='white',fill='tomato',bins =64)+xlab(expression('Annual Change of SWI'[0]*' (p < 0.05)'))+
  theme(axis.text.y= element_text(size=15,face = "bold"),axis.title   = element_text(size= 15),
        axis.text.x= element_text(size=15,face = "bold"),
        legend.background =  element_rect(fill = "transparent"),legend.text = element_text(size=8),legend.title = element_blank(),legend.position = c(0.8,0.85))
ggsave("//gssto1.umd.edu/gsdata1/duncansongp/fengt/icesat2_boreal_fire/figure/Manuscript_figures/annual_change_swi0_significant.png", width = 6, height = 3, units = "in", dpi = 300)

atl08_spatial_significant$warming_interval <- factor(atl08_spatial_significant$warming_interval, levels=c("Slight Warming","Rapid Warming") )

ggplot(data =atl08_spatial_significant@data[which(atl08_spatial_significant@data$warming_interval == "Slight Warming" |
                                                    atl08_spatial_significant@data$warming_interval == "Rapid Warming"  ),],aes(x=swi0_slope,
                                                                                                                                fill=factor(warming_interval)))+
  geom_histogram(color='white',bins =64)+scale_fill_manual(values = c("#F2AD00","red"))+
  xlab(expression('Annual Change of SWI'[0]*' (p < 0.05)'))+geom_vline(xintercept = 0,col='#F2AD00',linetype='dashed',size=1)+
  geom_vline(xintercept = 0.4896709,col='red',linetype='dashed',size=1)+
  theme(axis.text.y= element_text(size=15,face = "bold"),axis.title   = element_text(size= 15),
        axis.text.x= element_text(size=15,face = "bold"),legend.key = element_rect(colour = NA, fill = NA),
        legend.background =  element_rect(fill = "transparent"),legend.text = element_text(size=8),legend.title = element_blank(),legend.position = c(0.8,0.85))
ggsave("//gssto1.umd.edu/gsdata1/duncansongp/fengt/icesat2_boreal_fire/figure/Manuscript_figures/annual_significant_change_swi0_warming_only.png", width = 6, height = 3, units = "in", dpi = 300)



atl08_spatial_tmean_group1 <- atl08_spatial[which(atl08_spatial@data$swi0_slope > 0 &
                                                    atl08_spatial@data$swi0_slope < 0.4896709    &
                                                    atl08_spatial@data$swi0_p < 0.05),]
max_ysd <- max(atl08_spatial_tmean_group1@data$year_since_disturbance)
min_ysd <- min(atl08_spatial_tmean_group1@data$year_since_disturbance)
growth_rate_table_by_ysd <- array(data = NA,dim = c((max_ysd - min_ysd + 1),8))
z<-1
for(i in seq(from = min_ysd,to = max_ysd,by = 1)){
  atl08_spatial_ysd_specific <- atl08_spatial_tmean_group1@data[which(atl08_spatial_tmean_group1@data$year_since_disturbance >= i & atl08_spatial_tmean_group1@data$year_since_disturbance < i + 1),]
  median_growth_rate <- median(atl08_spatial_ysd_specific$growth_rate)
  growth_rate_25_percentile <- quantile(atl08_spatial_ysd_specific$growth_rate,0.25)
  growth_rate_75_percentile <- quantile(atl08_spatial_ysd_specific$growth_rate,0.75)
  growth_rate_table_by_ysd[z,1] <- i
  growth_rate_table_by_ysd[z,2] <- median_growth_rate
  growth_rate_table_by_ysd[z,3] <- growth_rate_25_percentile
  growth_rate_table_by_ysd[z,4] <- growth_rate_75_percentile
  growth_rate_table_by_ysd[z,5] <- nrow(atl08_spatial_ysd_specific)
  growth_rate_table_by_ysd[z,6] <- median(atl08_spatial_ysd_specific$swi0_year_before_after_disturbance_smooth)
  growth_rate_table_by_ysd[z,7] <- quantile(atl08_spatial_ysd_specific$swi0_year_before_after_disturbance_smooth,0.25)
  growth_rate_table_by_ysd[z,8] <- quantile(atl08_spatial_ysd_specific$swi0_year_before_after_disturbance_smooth,0.75)
  
  
  z <- z+1
}
growth_rate_table_by_ysd
growth_rate_table_by_ysd <- data.frame(growth_rate_table_by_ysd)
names(growth_rate_table_by_ysd) <- c("Year since Disturbance","Median","25%","75%","Num of Observations","SWI0 Year After Disturbance","p25 SWI0 Year After Disturbance","p75 SWI0 Year After Disturbance")
growth_rate_table_by_ysd <- growth_rate_table_by_ysd[which(growth_rate_table_by_ysd$`Num of Observations` > 100),]
growth_rate_table_by_ysd1 <- growth_rate_table_by_ysd

###
atl08_spatial_tmean_group2 <- atl08_spatial[which(atl08_spatial@data$swi0_slope > 0.4896709    &
                                                    atl08_spatial@data$swi0_p < 0.05),]
max_ysd <- max(atl08_spatial_tmean_group2@data$year_since_disturbance)
min_ysd <- min(atl08_spatial_tmean_group2@data$year_since_disturbance)
growth_rate_table_by_ysd <- array(data = NA,dim = c((max_ysd - min_ysd + 1),8))
z<-1
for(i in seq(from = min_ysd,to = max_ysd,by = 1)){
  atl08_spatial_ysd_specific <- atl08_spatial_tmean_group2@data[which(atl08_spatial_tmean_group2@data$year_since_disturbance >= i & atl08_spatial_tmean_group2@data$year_since_disturbance < i + 1),]
  median_growth_rate <- median(atl08_spatial_ysd_specific$growth_rate)
  growth_rate_25_percentile <- quantile(atl08_spatial_ysd_specific$growth_rate,0.25)
  growth_rate_75_percentile <- quantile(atl08_spatial_ysd_specific$growth_rate,0.75)
  growth_rate_table_by_ysd[z,1] <- i
  growth_rate_table_by_ysd[z,2] <- median_growth_rate
  growth_rate_table_by_ysd[z,3] <- growth_rate_25_percentile
  growth_rate_table_by_ysd[z,4] <- growth_rate_75_percentile
  growth_rate_table_by_ysd[z,5] <- nrow(atl08_spatial_ysd_specific)
  growth_rate_table_by_ysd[z,6] <- median(atl08_spatial_ysd_specific$swi0_year_before_after_disturbance_smooth)
  growth_rate_table_by_ysd[z,7] <- quantile(atl08_spatial_ysd_specific$swi0_year_before_after_disturbance_smooth,0.25)
  growth_rate_table_by_ysd[z,8] <- quantile(atl08_spatial_ysd_specific$swi0_year_before_after_disturbance_smooth,0.75)
  
  
  z <- z+1
}
growth_rate_table_by_ysd
growth_rate_table_by_ysd <- data.frame(growth_rate_table_by_ysd)
names(growth_rate_table_by_ysd) <- c("Year since Disturbance","Median","25%","75%","Num of Observations","SWI0 Year After Disturbance","p25 SWI0 Year After Disturbance","p75 SWI0 Year After Disturbance")
growth_rate_table_by_ysd <- growth_rate_table_by_ysd[which(growth_rate_table_by_ysd$`Num of Observations` > 100),]
growth_rate_table_by_ysd2 <- growth_rate_table_by_ysd

###
atl08_spatial_tmean_group3 <- atl08_spatial[which(atl08_spatial@data$swi0_p >= 0.05),]
max_ysd <- max(atl08_spatial_tmean_group3@data$year_since_disturbance)
min_ysd <- min(atl08_spatial_tmean_group3@data$year_since_disturbance)
growth_rate_table_by_ysd <- array(data = NA,dim = c((max_ysd - min_ysd + 1),8))
z<-1
for(i in seq(from = min_ysd,to = max_ysd,by = 1)){
  atl08_spatial_ysd_specific <- atl08_spatial_tmean_group3@data[which(atl08_spatial_tmean_group3@data$year_since_disturbance >= i & atl08_spatial_tmean_group3@data$year_since_disturbance < i + 1),]
  median_growth_rate <- median(atl08_spatial_ysd_specific$growth_rate)
  growth_rate_25_percentile <- quantile(atl08_spatial_ysd_specific$growth_rate,0.25)
  growth_rate_75_percentile <- quantile(atl08_spatial_ysd_specific$growth_rate,0.75)
  growth_rate_table_by_ysd[z,1] <- i
  growth_rate_table_by_ysd[z,2] <- median_growth_rate
  growth_rate_table_by_ysd[z,3] <- growth_rate_25_percentile
  growth_rate_table_by_ysd[z,4] <- growth_rate_75_percentile
  growth_rate_table_by_ysd[z,5] <- nrow(atl08_spatial_ysd_specific)
  growth_rate_table_by_ysd[z,6] <- median(atl08_spatial_ysd_specific$swi0_year_before_after_disturbance_smooth)
  growth_rate_table_by_ysd[z,7] <- quantile(atl08_spatial_ysd_specific$swi0_year_before_after_disturbance_smooth,0.25)
  growth_rate_table_by_ysd[z,8] <- quantile(atl08_spatial_ysd_specific$swi0_year_before_after_disturbance_smooth,0.75)
  
  
  z <- z+1
}
growth_rate_table_by_ysd
growth_rate_table_by_ysd <- data.frame(growth_rate_table_by_ysd)
names(growth_rate_table_by_ysd) <- c("Year since Disturbance","Median","25%","75%","Num of Observations","SWI0 Year After Disturbance","p25 SWI0 Year After Disturbance","p75 SWI0 Year After Disturbance")
growth_rate_table_by_ysd <- growth_rate_table_by_ysd[which(growth_rate_table_by_ysd$`Num of Observations` > 100),]
growth_rate_table_by_ysd3 <- growth_rate_table_by_ysd

lat_dis_category <- data.frame(array(data = NA,dim = c((nrow(atl08_spatial_tmean_group1@data)+
                                                          nrow(atl08_spatial_tmean_group2@data)+
                                                          nrow(atl08_spatial_tmean_group3@data)),2)))
lat_dis_category$X1 <- c(atl08_spatial_tmean_group1@data$lat,
                         atl08_spatial_tmean_group2@data$lat,
                         atl08_spatial_tmean_group3@data$lat)
lat_dis_category$X2 <- c(rep("Slight Warming",nrow(atl08_spatial_tmean_group1@data)),
                         rep("Rapid Warming",nrow(atl08_spatial_tmean_group2@data)),
                         rep("Non-significant Warming",nrow(atl08_spatial_tmean_group3@data)))
lat_dis_category$X2 <- factor(lat_dis_category$X2, levels=c("Non-significant Warming",
                                                            "Slight Warming",
                                                            "Rapid Warming") )
ggplot(lat_dis_category, aes(x=factor(lat_dis_category$X2),y=lat_dis_category$X1,fill=factor(lat_dis_category$X2))) +
  scale_fill_manual(values = c("#00A08A",'#F2AD00',"#FF0000"))+
  geom_boxplot(width = 0.07,outlier.shape = NA,coef=0,notch = FALSE,alpha=0.75)+
  geom_violin(trim = FALSE,width=0.8,alpha=0.35)+ guides(fill="none")+
  ylab("Latitude (Degree)")+ylim(45,72)+
  geom_hline(yintercept = median(lat_dis_category$X1[which(lat_dis_category$X2=="Non-significant Warming")]),col='#00A08A',linetype='dashed',size=1)+
  geom_hline(yintercept = median(lat_dis_category$X1[which(lat_dis_category$X2=="Slight Warming")]),col='#F2AD00',linetype='dashed',size=1)+
  geom_hline(yintercept = median(lat_dis_category$X1[which(lat_dis_category$X2=="Rapid Warming")]),col='#FF0000',linetype='dashed',size=1)+
  theme(axis.text.y= element_text(size=15,face = "bold"),axis.title.x = element_blank(),
        axis.text.x= element_text(size=12,face = "bold"),axis.title.y = element_text(size=15))
ggsave("//gssto1.umd.edu/gsdata1/duncansongp/fengt/icesat2_boreal_fire/figure/Manuscript_figures/lat_dis_group_swi0.png", width = 4, height = 3, units = "in", dpi = 300)

median_gr1 <- data.frame(cbind(growth_rate_table_by_ysd1$`Year since Disturbance`,growth_rate_table_by_ysd1$Median,growth_rate_table_by_ysd1$`SWI0 Year After Disturbance`)) 
median_gr1$X4 <- "Slight Warming***"
median_gr2 <- data.frame(cbind(growth_rate_table_by_ysd2$`Year since Disturbance`,growth_rate_table_by_ysd2$Median,growth_rate_table_by_ysd2$`SWI0 Year After Disturbance`))
median_gr2$X4 <- "Rapid Warming***"
median_gr3 <- data.frame(cbind(growth_rate_table_by_ysd3$`Year since Disturbance`,growth_rate_table_by_ysd3$Median,growth_rate_table_by_ysd3$`SWI0 Year After Disturbance`)) 
median_gr3$X4 <- "Non-significant Warming***"


atl08_spatial_filtered <- atl08_spatial@data[-which(atl08_spatial@data$swi0_slope < 0 &
                                                      atl08_spatial@data$swi0_p < 0.05),]

atl08_spatial_filtered$Warming_trend <- NA


atl08_spatial_filtered$Warming_trend[which(atl08_spatial_filtered$swi0_p > 0.05 &
                                             atl08_spatial_filtered$swi0_slope > 0)] <- 0
atl08_spatial_filtered$Warming_trend[which(atl08_spatial_filtered$swi0_p < 0.05 &
                                             atl08_spatial_filtered$swi0_slope >0.1 &
                                             atl08_spatial_filtered$swi0_slope <= 0.2)] <- 1

atl08_spatial_filtered$Warming_trend[which(atl08_spatial_filtered$swi0_p < 0.05 &
                                             atl08_spatial_filtered$swi0_slope >0.2 &
                                             atl08_spatial_filtered$swi0_slope <= 0.3)] <- 2

atl08_spatial_filtered$Warming_trend[which(atl08_spatial_filtered$swi0_p < 0.05 &
                                             atl08_spatial_filtered$swi0_slope >0.3 &
                                             atl08_spatial_filtered$swi0_slope <= 0.4)] <- 3

atl08_spatial_filtered$Warming_trend[which(atl08_spatial_filtered$swi0_p < 0.05 &
                                             atl08_spatial_filtered$swi0_slope >0.4 &
                                             atl08_spatial_filtered$swi0_slope <= 0.5)] <- 4

atl08_spatial_filtered$Warming_trend[which(atl08_spatial_filtered$swi0_p < 0.05 &
                                             atl08_spatial_filtered$swi0_slope >0.5 &
                                             atl08_spatial_filtered$swi0_slope <= 0.6)] <- 5

atl08_spatial_filtered$Warming_trend[which(atl08_spatial_filtered$swi0_p < 0.05 &
                                             atl08_spatial_filtered$swi0_slope >0.6 &
                                             atl08_spatial_filtered$swi0_slope <= 0.7)] <- 6

atl08_spatial_filtered$Warming_trend[which(atl08_spatial_filtered$swi0_p < 0.05 &
                                             atl08_spatial_filtered$swi0_slope >0.7 &
                                             atl08_spatial_filtered$swi0_slope <= 0.8)] <- 7

atl08_spatial_filtered$Warming_trend[which(atl08_spatial_filtered$swi0_p < 0.05 &
                                             atl08_spatial_filtered$swi0_slope >0.8)] <- 8






atl08_spatial_filtered$Warming_trend <- factor(atl08_spatial_filtered$Warming_trend, levels=c(0,
                                                                                              1,
                                                                                              2,
                                                                                              3,
                                                                                              4,
                                                                                              5,
                                                                                              6,
                                                                                              7,
                                                                                              8))


atl08_spatial_filtered0 <- atl08_spatial_filtered[which(atl08_spatial_filtered$Warming_trend == 0),]
atl08_spatial_filtered1 <- atl08_spatial_filtered[which(atl08_spatial_filtered$Warming_trend == 1),]
atl08_spatial_filtered2 <- atl08_spatial_filtered[which(atl08_spatial_filtered$Warming_trend == 2),]
atl08_spatial_filtered3 <- atl08_spatial_filtered[which(atl08_spatial_filtered$Warming_trend == 3),]
atl08_spatial_filtered4 <- atl08_spatial_filtered[which(atl08_spatial_filtered$Warming_trend == 4),]
atl08_spatial_filtered5 <- atl08_spatial_filtered[which(atl08_spatial_filtered$Warming_trend == 5),]
atl08_spatial_filtered6 <- atl08_spatial_filtered[which(atl08_spatial_filtered$Warming_trend == 6),]
atl08_spatial_filtered7 <- atl08_spatial_filtered[which(atl08_spatial_filtered$Warming_trend == 7),]
atl08_spatial_filtered8 <- atl08_spatial_filtered[which(atl08_spatial_filtered$Warming_trend == 8),]

data_list <- list(atl08_spatial_filtered0,
                  atl08_spatial_filtered1,
                  atl08_spatial_filtered2,
                  atl08_spatial_filtered3,
                  atl08_spatial_filtered4,
                  atl08_spatial_filtered5,
                  atl08_spatial_filtered6,
                  atl08_spatial_filtered7,
                  atl08_spatial_filtered8)

group_data <- rbind(atl08_spatial_filtered0,atl08_spatial_filtered6)
result1 <- aov(growth_rate ~ year_since_disturbance*Warming_trend,data = group_data)
summary(result1)

cor_data <- data.frame(array(data = NA,dim = c(9,9)))
cor_data <- cor(cor_data)
rownames(cor_data) <- c("[0,0]",
                        "[0.1,0.2]",
                        "[0.2,0.3]",
                        "[0.3,0.4]",
                        "[0.4,0.5]",
                        "[0.5,0.6]",
                        "[0.6,0.7]",
                        "[0.7,0.8]",
                        "[0.8,]")
colnames(cor_data) <- c("[0,0]",
                        "[0.1,0.2]",
                        "[0.2,0.3]",
                        "[0.3,0.4]",
                        "[0.4,0.5]",
                        "[0.5,0.6]",
                        "[0.6,0.7]",
                        "[0.7,0.8]",
                        "[0.8,]")

cor_data2 <- cor_data

for(i in 0:8){
  for(j in 0:8){
    if(i != j){
      data_group <- atl08_spatial_filtered[which(atl08_spatial_filtered$Warming_trend == i | atl08_spatial_filtered$Warming_trend == j),]
      result1 <- aov(growth_rate ~ year_since_disturbance*Warming_trend,data =data_group)
      if(summary(result1)[[1]][["Pr(>F)"]][3] <= 0.05){
        cor_data[i+1,j+1] <- 1
      }
      else{cor_data[i+1,j+1] <- 0}
      
      slope_i <- -summary(lm(growth_rate ~ year_since_disturbance,data = data_group[which(data_group$Warming_trend == i),]))$coefficients[2,1]
      coefficient_slope_i <- summary(lm(growth_rate ~ year_since_disturbance,data = data_group[which(data_group$Warming_trend == i),]))$coefficients[2,4]
      slope_j <- -summary(lm(growth_rate ~ year_since_disturbance,data = data_group[which(data_group$Warming_trend == j),]))$coefficients[2,1]
      coefficient_slope_j <- summary(lm(growth_rate ~ year_since_disturbance,data = data_group[which(data_group$Warming_trend == j),]))$coefficients[2,4]
      if(isTRUE(coefficient_slope_i < 0.05 & coefficient_slope_j < 0.05)==TRUE){
        slope_diff <- slope_i - slope_j
        if(slope_diff > 0){
          cor_data2[i+1,j+1] <-  1
        } else{
          cor_data2[i+1,j+1] <-  0
        }
        
      }else{cor_data2[i+1,j+1] <-  NA
      
      }
      
      print(j)}
    else{cor_data[i+1,j+1] <- NA}
    
  }
}

for(i in 0:8){
  for(j in 0:8){
    if(i != j){
      data_group <- atl08_spatial_filtered[which(atl08_spatial_filtered$Warming_trend == i | atl08_spatial_filtered$Warming_trend == j),]
      result1 <- aov(growth_rate ~ year_since_disturbance*Warming_trend,data =data_group)
      if(summary(result1)[[1]][["Pr(>F)"]][3] <= 0.05){
        cor_data[i+1,j+1] <- 1
      }
      else{cor_data[i+1,j+1] <- 0}
      
      slope_i <- -summary(lm(growth_rate ~ year_since_disturbance,data = data_group[which(data_group$Warming_trend == i),]))$coefficients[2,1]
      coefficient_slope_i <- summary(lm(growth_rate ~ year_since_disturbance,data = data_group[which(data_group$Warming_trend == i),]))$coefficients[2,4]
      slope_j <- -summary(lm(growth_rate ~ year_since_disturbance,data = data_group[which(data_group$Warming_trend == j),]))$coefficients[2,1]
      coefficient_slope_j <- summary(lm(growth_rate ~ year_since_disturbance,data = data_group[which(data_group$Warming_trend == j),]))$coefficients[2,4]
      slope_diff <- slope_i - slope_j
      if(slope_diff > 0){
        cor_data2[i+1,j+1] <-  1
      } else{
        cor_data2[i+1,j+1] <-  0
      }
      
    }else{
      cor_data[i+1,j+1] <- NA
      cor_data2[i+1,j+1] <-  NA
      
    }
    
    print(j)}
}



get_lower_tri<-function(cormat){
  cormat[upper.tri(cormat)] <- NA
  return(cormat)
}
get_upper_tri <- function(cormat){
  cormat[lower.tri(cormat)]<- NA
  return(cormat)
}

library(reshape2)
#ggcorr(cor_data,nbreaks = 3)
cor_data_upper <- get_upper_tri(cor_data)
cor_data2_upper <- get_upper_tri(cor_data2)

melted_cor_data <- melt(cor_data_upper,na.rm = TRUE)
melted_cor_data2 <- melt(cor_data2_upper,na.rm = TRUE)
melted_cor_data$value2 <- melted_cor_data2$value

melted_cor_data$value2 <- factor(melted_cor_data$value2,levels = c(0,1))
melted_cor_data$Var2 <- factor(melted_cor_data$Var2,levels = names(c("[0,0]",
                                                                     "[0.1,0.2]",
                                                                     "[0.2,0.3]",
                                                                     "[0.3,0.4]",
                                                                     "[0.4,0.5]",
                                                                     "[0.5,0.6]",
                                                                     "[0.6,0.7]",
                                                                     "[0.7,0.8]",
                                                                     "[0.8,]")))
melted_cor_data$value <- factor(melted_cor_data$value,levels = c(0,1))

ggplot(data = melted_cor_data, aes(Var2, Var1,fill = value2))+
  geom_tile(col='white')+
  scale_fill_manual(values = c("tomato","grey"))+
  geom_point(aes(size=value))+
  scale_size_manual(values = c("1" = 1, "0"=-1))+
  theme_minimal()+ xlab(expression('Annual Increase of SWI'[0]))+ylab(expression('Annual Increase of SWI'[0]))+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 12, hjust = 1,colour = c("red", "black","black","black","black","black","black","black")),
        axis.text.y = element_text(angle = 45, vjust = 1, 
                                   size = 12, hjust = 1,colour=c("black","red", "black","black","black","black","black","black")),
        axis.title.x = element_text(size = 18),
        axis.title.y = element_text(size = 18))+
  coord_fixed()

#median_gr <- rbind(median_gr1,median_gr2,median_gr3,median_gr4)
median_gr <- rbind(median_gr3,median_gr1,median_gr2)

names(median_gr) <- c("Year Since Disturbance","Annual Growth Rate","SWI0 Year of Disturbance","Warming Trend")

median_gr$`Warming Trend` <- factor(median_gr$`Warming Trend`, levels=c("Non-significant Warming***",
                                                                        "Slight Warming***",
                                                                        "Rapid Warming***") )
median_gr$SWI0_y <- NA
median_gr$SWI0_y[which(median_gr$`Warming Trend` == "Non-significant Warming***")] <-  -0.00296372 * median_gr$`Year Since Disturbance`[which(median_gr$`Warming Trend` == "Non-significant Warming***")] + 0.22916359
median_gr$SWI0_y[which(median_gr$`Warming Trend` == "Slight Warming***")] <-  -0.0034770 * median_gr$`Year Since Disturbance`[which(median_gr$`Warming Trend` == "Slight Warming***")] + 0.2201453
median_gr$SWI0_y[which(median_gr$`Warming Trend` == "Rapid Warming***")] <-  -0.007721  * median_gr$`Year Since Disturbance`[which(median_gr$`Warming Trend` == "Rapid Warming***")] + 0.306794

gr_group1 <- atl08_spatial_tmean_group1@data$growth_rate
ysd_group1 <- atl08_spatial_tmean_group1@data$year_since_disturbance
summary(lm(gr_group1~ysd_group1))

gr_group2 <- atl08_spatial_tmean_group2@data$growth_rate
ysd_group2 <- atl08_spatial_tmean_group2@data$year_since_disturbance
summary(lm(gr_group2~ysd_group2))

gr_group3 <- atl08_spatial_tmean_group3@data$growth_rate
ysd_group3 <- atl08_spatial_tmean_group3@data$year_since_disturbance
summary(lm(gr_group3~ysd_group3))


ggplot(data = median_gr,aes(x=`Year Since Disturbance` ,
                            y=SWI0_y,
                            color=`Warming Trend`
                            
))+geom_point(aes(size=median_gr$`SWI0 Year of Disturbance`),alpha=0.8)+     #geom_smooth(method = lm,size=1,linetype="dashed",se=0)+scale_y_continuous(limits = c(0.05,0.18))+
  geom_abline(slope = -0.00296372,intercept = 0.22916359 ,col='#00A08A',lty='dashed',size=1)+
  geom_abline(slope = -0.0034770,intercept = 0.2201453 ,col='#F2AD00',lty='dashed',size=1)+
  geom_abline(slope = -0.007721 ,intercept = 0.306794  ,col='#FF0000',lty='dashed',size=1)+
  scale_color_manual(values = c("#00A08A","#F2AD00","#FF0000","black"))+
  scale_size(range = c(2,10))+ylim(c(0.02,0.2))+
  scale_size_continuous(expression('SWI'[0]))+
  ylab("Annual Growth Rate (m / year)")+xlab("Year since Disturbance")+
  theme(legend.box = "horizontal",
        axis.text.y  = element_text(size = 15,face = "bold"),axis.title.x  = element_text(size = 15),axis.title.y = element_text(size= 15) ,
        axis.text.x= element_text(size = 15,face = "bold"),
        #axis.ticks.x=element_blank(),
        legend.key = element_rect(colour = NA, fill = NA),
        legend.background =  element_rect(fill = "transparent"),legend.text = element_text(size=10),legend.title = element_text(size=15),legend.position = c(0.32,0.18))



p25_gr1 <- data.frame(cbind(growth_rate_table_by_ysd1$`Year since Disturbance`,growth_rate_table_by_ysd1$`25%`,growth_rate_table_by_ysd1$`p25 SWI0 Year After Disturbance`)) 
p25_gr1$X4 <- "Slight Warming ***"
p25_gr2 <- data.frame(cbind(growth_rate_table_by_ysd2$`Year since Disturbance`,growth_rate_table_by_ysd2$`25%`,growth_rate_table_by_ysd2$`p25 SWI0 Year After Disturbance`))
p25_gr2$X4 <- "Rapid Warming *"
p25_gr3 <- data.frame(cbind(growth_rate_table_by_ysd3$`Year since Disturbance`,growth_rate_table_by_ysd3$`25%`,growth_rate_table_by_ysd3$`p25 SWI0 Year After Disturbance`)) 
p25_gr3$X4 <- "Non-significant Warming ***"

p25_gr <- rbind(p25_gr3,p25_gr1,p25_gr2)
names(p25_gr) <- c("Year Since Disturbance","Annual Growth Rate","Smmothed SWI0 Year of Disturbance","Warming Trend")
p25_gr$`Warming Trend` <- factor(p25_gr$`Warming Trend`, levels=c("Non-significant Warming ***","Slight Warming ***",
                                                                  "Rapid Warming *") )
ggplot(data = p25_gr,aes(x=`Year Since Disturbance`,
                         y=`Annual Growth Rate`,
                         color=`Warming Trend`
                         
))+geom_point(aes(size=`Smmothed SWI0 Year of Disturbance`))+geom_line()+geom_smooth(method = lm,size=1,linetype="dashed",se=0)+scale_y_continuous(limits = c(0.025,0.107))+
  scale_color_manual(values = c("#00A08A","#F2AD00","#FF0000","black"))+
  scale_size(range = c(2,10))+
  scale_size_continuous(expression('SWI'[0]))+
  ylab("25th Percentile of Annual Growth Rate (m / year)")+xlab("Year Since Disturbance")+
  theme(legend.box = "horizontal",
        axis.text.y  = element_text(size = 15,face = "bold"),axis.title.x  = element_text(size = 15),axis.title.y = element_text(size= 15) ,
        axis.text.x= element_text(size = 15,face = "bold"),
        #axis.ticks.x=element_blank(),
        legend.key = element_rect(colour = NA, fill = NA),
        legend.background =  element_rect(fill = "transparent"),legend.text = element_text(size=10),legend.title = element_text(size=15),legend.position = c(0.27,0.15))

p75_gr1 <- data.frame(cbind(growth_rate_table_by_ysd1$`Year since Disturbance`,growth_rate_table_by_ysd1$`75%`,growth_rate_table_by_ysd1$`p75 SWI0 Year After Disturbance`)) 
p75_gr1$X4 <- "Slight Warming "
p75_gr2 <- data.frame(cbind(growth_rate_table_by_ysd2$`Year since Disturbance`,growth_rate_table_by_ysd2$`75%`,growth_rate_table_by_ysd2$`p75 SWI0 Year After Disturbance`))
p75_gr2$X4 <- "Rapid Warming "
p75_gr3 <- data.frame(cbind(growth_rate_table_by_ysd3$`Year since Disturbance`,growth_rate_table_by_ysd3$`75%`,growth_rate_table_by_ysd3$`p75 SWI0 Year After Disturbance`)) 
p75_gr3$X4 <- "Non-significant Warming ***"

p75_gr <- rbind(p75_gr3,p75_gr1,p75_gr2)
names(p75_gr) <- c("Year Since Disturbance","Annual Growth Rate","Smmothed SWI0","Warming Trend")
p75_gr$`Warming Trend` <- factor(p75_gr$`Warming Trend`, levels=c("Non-significant Warming ***","Slight Warming ",
                                                                  "Rapid Warming "))
ggplot(data = p75_gr,aes(x=`Year Since Disturbance`,
                         y=`Annual Growth Rate`,
                         color=`Warming Trend`
                         
))+geom_point(aes(size=`Smmothed SWI0`))+geom_line()+geom_smooth(method = lm,size=1,linetype="dashed",se=0)+scale_y_continuous(limits = c(0.07,0.275))+
  scale_color_manual(values = c("#00A08A","#F2AD00","#FF0000","black"))+
  #scale_size(limits = 3)+
  scale_size_continuous(name = expression('SWI'[0]),limits = c(40,70))+
  ylab("75th Percentile of Annual Growth Rate (m / year)")+xlab("Year Since Disturbance")+
  theme(legend.box = "horizontal",
        axis.text.y  = element_text(size = 15,face = "bold"),axis.title.x  = element_text(size = 15),axis.title.y = element_text(size= 15) ,
        axis.text.x= element_text(size = 15,face = "bold"),
        #axis.ticks.x=element_blank(),
        legend.key = element_rect(colour = NA, fill = NA),
        legend.background =  element_rect(fill = "transparent"),legend.text = element_text(size=10),legend.title = element_text(size=15),legend.position = c(0.27,0.15))



atl08_spatial_updated <-  atl08_spatial@data

deciduousness <- atl08_spatial_updated[which(atl08_spatial_updated$deciduous_fraction_absolute_change > 0),]

evergreeness <- atl08_spatial_updated[which(atl08_spatial_updated$deciduous_fraction_absolute_change < 0),]


evergreeness_group1 <- evergreeness[which(evergreeness$swi0_slope >  0 &
                                            evergreeness$swi0_p < 0.05),]

max_ysd <- max(evergreeness_group1$year_since_disturbance)
min_ysd <- min(evergreeness_group1$year_since_disturbance)

growth_rate_table_by_ysd <- array(data = NA,dim = c((max_ysd - min_ysd + 1),6))
z<-1

for(i in seq(from = min_ysd,to = max_ysd,by = 1)){
  atl08_spatial_ysd_specific <- evergreeness_group1[which(evergreeness_group1$year_since_disturbance >= i & evergreeness_group1$year_since_disturbance < i + 1),]
  median_growth_rate <- median(atl08_spatial_ysd_specific$growth_rate)
  growth_rate_25_percentile <- quantile(atl08_spatial_ysd_specific$growth_rate,0.25)
  growth_rate_75_percentile <- quantile(atl08_spatial_ysd_specific$growth_rate,0.75)
  growth_rate_table_by_ysd[z,1] <- i
  growth_rate_table_by_ysd[z,2] <- median_growth_rate
  growth_rate_table_by_ysd[z,3] <- growth_rate_25_percentile
  growth_rate_table_by_ysd[z,4] <- growth_rate_75_percentile
  growth_rate_table_by_ysd[z,5] <- nrow(atl08_spatial_ysd_specific)
  growth_rate_table_by_ysd[z,6] <- median(atl08_spatial_ysd_specific$deciduousfraction_2015_prediction)
  
  z <- z+1
}

growth_rate_table_by_ysd

growth_rate_table_by_ysd <- data.frame(growth_rate_table_by_ysd)
names(growth_rate_table_by_ysd) <- c("Year since Disturbance","Median","25%","75%","Num of Observations","Mean Deciduous Fraction")


growth_rate_table_by_ysd <- growth_rate_table_by_ysd[which(growth_rate_table_by_ysd$`Num of Observations` > 100),]
growth_rate_table_by_ysd1 <- growth_rate_table_by_ysd


evergreeness_group2 <- evergreeness[which(evergreeness$swi0_p > 0.05),]
max_ysd <- max(evergreeness_group2$year_since_disturbance)
min_ysd <- min(evergreeness_group2$year_since_disturbance)
growth_rate_table_by_ysd <- array(data = NA,dim = c((max_ysd - min_ysd + 1),6))
z<-1
for(i in seq(from = min_ysd,to = max_ysd,by = 1)){
  atl08_spatial_ysd_specific <- evergreeness_group2[which(evergreeness_group2$year_since_disturbance >= i & evergreeness_group2$year_since_disturbance < i + 1),]
  median_growth_rate <- median(atl08_spatial_ysd_specific$growth_rate)
  growth_rate_25_percentile <- quantile(atl08_spatial_ysd_specific$growth_rate,0.25)
  growth_rate_75_percentile <- quantile(atl08_spatial_ysd_specific$growth_rate,0.75)
  growth_rate_table_by_ysd[z,1] <- i
  growth_rate_table_by_ysd[z,2] <- median_growth_rate
  growth_rate_table_by_ysd[z,3] <- growth_rate_25_percentile
  growth_rate_table_by_ysd[z,4] <- growth_rate_75_percentile
  growth_rate_table_by_ysd[z,5] <- nrow(atl08_spatial_ysd_specific)
  growth_rate_table_by_ysd[z,6] <- median(atl08_spatial_ysd_specific$deciduousfraction_2015_prediction)
  
  z <- z+1
}
growth_rate_table_by_ysd
growth_rate_table_by_ysd <- data.frame(growth_rate_table_by_ysd)
names(growth_rate_table_by_ysd) <- c("Year since Disturbance","Median","25%","75%","Num of Observations","Mean Deciduous Fraction")
growth_rate_table_by_ysd <- growth_rate_table_by_ysd[which(growth_rate_table_by_ysd$`Num of Observations` > 100),]
growth_rate_table_by_ysd2 <- growth_rate_table_by_ysd

#Warming & Deciduoueness 
deciduousness_group1 <- deciduousness[which(deciduousness$swi0_slope > 0 &
                                              deciduousness$swi0_p < 0.05),]
max_ysd <- max(deciduousness_group1$year_since_disturbance)

min_ysd <- min(deciduousness_group1$year_since_disturbance)
growth_rate_table_by_ysd <- array(data = NA,dim = c((max_ysd - min_ysd + 1),6))
z<-1
for(i in seq(from = min_ysd,to = max_ysd,by = 1)){
  atl08_spatial_ysd_specific <- deciduousness_group1[which(deciduousness_group1$year_since_disturbance >= i & deciduousness_group1$year_since_disturbance < i + 1),]
  median_growth_rate <- median(atl08_spatial_ysd_specific$growth_rate)
  growth_rate_25_percentile <- quantile(atl08_spatial_ysd_specific$growth_rate,0.25)
  growth_rate_75_percentile <- quantile(atl08_spatial_ysd_specific$growth_rate,0.75)
  growth_rate_table_by_ysd[z,1] <- i
  growth_rate_table_by_ysd[z,2] <- median_growth_rate
  growth_rate_table_by_ysd[z,3] <- growth_rate_25_percentile
  growth_rate_table_by_ysd[z,4] <- growth_rate_75_percentile
  growth_rate_table_by_ysd[z,5] <- nrow(atl08_spatial_ysd_specific)
  growth_rate_table_by_ysd[z,6] <- median(atl08_spatial_ysd_specific$deciduousfraction_2015_prediction)
  
  z <- z+1
}

growth_rate_table_by_ysd
growth_rate_table_by_ysd <- data.frame(growth_rate_table_by_ysd)
names(growth_rate_table_by_ysd) <- c("Year since Disturbance","Median","25%","75%","Num of Observations","Mean Deciduous Fraction")
growth_rate_table_by_ysd <- growth_rate_table_by_ysd[which(growth_rate_table_by_ysd$`Num of Observations` > 100),]
growth_rate_table_by_ysd3 <- growth_rate_table_by_ysd

deciduousness_group2 <- deciduousness[which(deciduousness$swi0_p > 0.05 ),]
max_ysd <- max(deciduousness_group2$year_since_disturbance)
min_ysd <- min(deciduousness_group2$year_since_disturbance)

growth_rate_table_by_ysd <- array(data = NA,dim = c((max_ysd - min_ysd + 1),6))

z<-1
for(i in seq(from = min_ysd,to = max_ysd,by = 1)){
  atl08_spatial_ysd_specific <- deciduousness_group2[which(deciduousness_group2$year_since_disturbance >= i & deciduousness_group2$year_since_disturbance < i + 1),]
  median_growth_rate <- median(atl08_spatial_ysd_specific$growth_rate)
  growth_rate_25_percentile <- quantile(atl08_spatial_ysd_specific$growth_rate,0.25)
  growth_rate_75_percentile <- quantile(atl08_spatial_ysd_specific$growth_rate,0.75)
  growth_rate_table_by_ysd[z,1] <- i
  growth_rate_table_by_ysd[z,2] <- median_growth_rate
  growth_rate_table_by_ysd[z,3] <- growth_rate_25_percentile
  growth_rate_table_by_ysd[z,4] <- growth_rate_75_percentile
  growth_rate_table_by_ysd[z,5] <- nrow(atl08_spatial_ysd_specific)
  growth_rate_table_by_ysd[z,6] <- median(atl08_spatial_ysd_specific$deciduousfraction_2015_prediction)
  
  z <- z+1
}
growth_rate_table_by_ysd
growth_rate_table_by_ysd <- data.frame(growth_rate_table_by_ysd)
names(growth_rate_table_by_ysd) <- c("Year since Disturbance","Median","25%","75%","Num of Observations","Mean Deciduous Fraction")
growth_rate_table_by_ysd <- growth_rate_table_by_ysd[which(growth_rate_table_by_ysd$`Num of Observations` > 100),]

growth_rate_table_by_ysd4 <- growth_rate_table_by_ysd

median_gr1 <- data.frame(cbind(growth_rate_table_by_ysd1$`Year since Disturbance`,growth_rate_table_by_ysd1$Median,growth_rate_table_by_ysd1$`Mean Deciduous Fraction`)) 
median_gr1$X4 <- "Warming & No Deciduous Shift"
median_gr2 <- data.frame(cbind(growth_rate_table_by_ysd2$`Year since Disturbance`,growth_rate_table_by_ysd2$Median,growth_rate_table_by_ysd2$`Mean Deciduous Fraction`))
median_gr2$X4 <- "No Warming & No Deciduous Shift **"
median_gr3 <- data.frame(cbind(growth_rate_table_by_ysd3$`Year since Disturbance`,growth_rate_table_by_ysd3$Median,growth_rate_table_by_ysd3$`Mean Deciduous Fraction`)) 

median_gr3$X4 <- "Warming & Deciduous Shift ***"

median_gr4 <- data.frame(cbind(growth_rate_table_by_ysd4$`Year since Disturbance`,growth_rate_table_by_ysd4$Median,growth_rate_table_by_ysd4$`Mean Deciduous Fraction`)) 
median_gr4$X4 <- "No Warming & Deciduous Shift ***"
median_gr <- rbind(median_gr1,
                   median_gr2,
                   median_gr3,
                   median_gr4)
names(median_gr) <- c("Year Since Disturbance","Annual Growth Rate","Mean Deciduous Fraction","Warming Trend")
median_gr$`Warming Trend` <- factor(median_gr$`Warming Trend`, levels=c("No Warming & No Deciduous Shift **",
                                                                        "No Warming & Deciduous Shift ***",
                                                                        "Warming & No Deciduous Shift",
                                                                        "Warming & Deciduous Shift ***") )

ggplot(data = median_gr,aes(x=`Year Since Disturbance`,
                            y=`Annual Growth Rate`,
                            color=`Warming Trend`
))+geom_line(alpha=0.3,lwd=1)+
  geom_smooth(method = lm,size=2,linetype="dashed",se=0)+scale_y_continuous(limits = c(0.05,0.22))+
  #scale_color_manual(values = c("grey3","darkgreen",'tomato',"red"))+
  scale_color_manual(values = c("#F2AD00", "#72D8FF","#F76D5E","darkgreen"))+ 
  ylab("Median Annual Growth Rate (m / year)")+xlab("Year Since Disturbance")+
  ylim(c(0.025,0.25))+
  theme(axis.text.y  = element_text(size = 15,face = "bold"),axis.title.x  = element_text(size = 15),axis.title.y = element_text(size= 15) ,
        axis.text.x= element_text(size = 15,face = "bold"),
        #axis.ticks.x=element_blank(),
        legend.key = element_rect(colour = NA, fill = NA),
        legend.background =  element_rect(fill = "transparent"),legend.text = element_text(size=15),legend.title = element_blank(),legend.position = c(0.35,0.15))
ggsave("//gssto1.umd.edu/gsdata1/duncansongp/fengt/icesat2_boreal_fire/figure/Manuscript_figures/gr_deciduousness_comparison_col_updated_swi0.png", width = 10, height = 10, units = "in", dpi = 500)

lat_group1 <- data.frame(array(data = NA,dim  = c(nrow(deciduousness_group1),2)))
lat_group2 <- data.frame(array(data = NA,dim  = c(nrow(deciduousness_group2),2)))
lat_group3 <- data.frame(array(data = NA,dim  = c(nrow(evergreeness_group1),2)))
lat_group4 <- data.frame(array(data = NA,dim  = c(nrow(evergreeness_group2),2)))
lat_group <- rbind(lat_group1,lat_group2,lat_group3,lat_group4)
lat_group[,1] <- c(deciduousness_group1$lat,
                   deciduousness_group2$lat,
                   evergreeness_group1$lat,
                   evergreeness_group2$lat)
lat_group[,2] <- c(rep("Warming & Deciduous Shift",nrow(lat_group1)),
                   rep("No Warming & Deciduous Shift",nrow(lat_group2)),
                   rep("Warming & No Deciduous Shift",nrow(lat_group3)),
                   rep("No Warming & No Deciduous Shift",nrow(lat_group4)))
lat_group$X2 <- factor(lat_group$X2, levels=c("No Warming & No Deciduous Shift",
                                              "No Warming & Deciduous Shift",
                                              "Warming & No Deciduous Shift",
                                              "Warming & Deciduous Shift") )
cols <- c("#F2AD00", "#72D8FF","#F76D5E","darkgreen")
ggplot(lat_group, aes(y = X1, colour = X2)) +
  geom_density(lwd = 1.2, linetype = 1,show_guide=FALSE) + 
  stat_density(geom="line",position="identity")+
  scale_color_manual(values = cols)+
  geom_hline(yintercept = median(lat_group$X1[which(lat_group$X2 == "No Warming & No Deciduous Shift")]),col="#F2AD00",linetype='dashed', lwd = 1.3)+
  geom_hline(yintercept = median(lat_group$X1[which(lat_group$X2 == "No Warming & Deciduous Shift")]),col="#72D8FF",linetype='dashed',size=1.3)+
  geom_hline(yintercept = median(lat_group$X1[which(lat_group$X2 == "Warming & No Deciduous Shift")]),col="#F76D5E",linetype='dashed',size=1.3)+
  geom_hline(yintercept = median(lat_group$X1[which(lat_group$X2 == "Warming & Deciduous Shift")]),col="darkgreen",linetype='dashed',size=1.3)+
  xlab("Probability Density (#%)")+
  ylab("Latitude (Degree)")+
  theme(axis.text.y  = element_text(size = 15,face = "bold"),axis.title.x  = element_text(size = 15),axis.title.y = element_text(size= 15) ,
        axis.text.x= element_text(size = 15,face = "bold"),
        #axis.ticks.x=element_blank(),
        legend.key = element_rect(colour = NA, fill = NA),
        legend.background =  element_rect(fill = "transparent"),legend.text = element_text(size=15),legend.title = element_blank(),legend.position = c(0.72,0.9))

ggsave("//gssto1.umd.edu/gsdata1/duncansongp/fengt/icesat2_boreal_fire/figure/Manuscript_figures/gr_deciduousness_lat_swi0.png", width = 10, height = 10, units = "in", dpi = 500)



##p25
p25_gr1 <- data.frame(cbind(growth_rate_table_by_ysd1$`Year since Disturbance`,growth_rate_table_by_ysd1$`25%`,growth_rate_table_by_ysd1$`Mean Deciduous Fraction`)) 
p25_gr1$X4 <- "Warming & No Deciduous Shift"
p25_gr2 <- data.frame(cbind(growth_rate_table_by_ysd2$`Year since Disturbance`,growth_rate_table_by_ysd2$`25%`,growth_rate_table_by_ysd2$`Mean Deciduous Fraction`))
p25_gr2$X4 <- "No Warming & No Deciduous Shift ***"
p25_gr3 <- data.frame(cbind(growth_rate_table_by_ysd3$`Year since Disturbance`,growth_rate_table_by_ysd3$`25%`,growth_rate_table_by_ysd3$`Mean Deciduous Fraction`)) 
p25_gr3$X4 <- "Warming & Deciduous Shift ***"
p25_gr4 <- data.frame(cbind(growth_rate_table_by_ysd4$`Year since Disturbance`,growth_rate_table_by_ysd4$`25%`,growth_rate_table_by_ysd4$`Mean Deciduous Fraction`)) 
p25_gr4$X4 <- "No Warming & Deciduous Shift ***"
p25_gr <- rbind(p25_gr1,
                p25_gr2,
                p25_gr3,
                p25_gr4)
names(p25_gr) <- c("Year Since Disturbance","Annual Growth Rate","Mean Deciduous Fraction","Warming Trend")
p25_gr$`Warming Trend` <- factor(p25_gr$`Warming Trend`, levels=c("No Warming & No Deciduous Shift ***",
                                                                  "No Warming & Deciduous Shift ***",
                                                                  "Warming & No Deciduous Shift",
                                                                  "Warming & Deciduous Shift ***") )

ggplot(data = p25_gr,aes(x=`Year Since Disturbance`,
                         y=`Annual Growth Rate`,
                         color=`Warming Trend`
))+geom_line(alpha=0.3,lwd=1)+
  geom_smooth(method = lm,size=2,linetype="dashed",se=0)+scale_y_continuous(limits = c(0,0.16),breaks = seq(0,0.16,by=0.04))+
  #scale_color_manual(values = c("grey3","darkgreen",'tomato',"red"))+
  scale_color_manual(values = c("#F2AD00", "#72D8FF","#F76D5E","darkgreen"))+ 
  ylab("25th Percentile of Annual Growth Rate (m / year)")+xlab("Year Since Disturbance")+
  #ylim(c(0.025,0.15))+
  theme(axis.text.y  = element_text(size = 15,face = "bold"),axis.title.x  = element_text(size = 15),axis.title.y = element_text(size= 15) ,
        axis.text.x= element_text(size = 15,face = "bold"),
        #axis.ticks.x=element_blank(),
        legend.key = element_rect(colour = NA, fill = NA),
        legend.background =  element_rect(fill = "transparent"),legend.text = element_text(size=15),legend.title = element_blank(),legend.position = c(0.37,0.15))
ggsave("//gssto1.umd.edu/gsdata1/duncansongp/fengt/icesat2_boreal_fire/figure/Manuscript_figures/p25_gr_deciduousness_comparison_col_updated_swi0.png", width = 10, height = 10, units = "in", dpi = 500)

##p75
p75_gr1 <- data.frame(cbind(growth_rate_table_by_ysd1$`Year since Disturbance`,growth_rate_table_by_ysd1$`75%`,growth_rate_table_by_ysd1$`Mean Deciduous Fraction`)) 
p75_gr1$X4 <- "Warming & No Deciduous Shift"
p75_gr2 <- data.frame(cbind(growth_rate_table_by_ysd2$`Year since Disturbance`,growth_rate_table_by_ysd2$`75%`,growth_rate_table_by_ysd2$`Mean Deciduous Fraction`))
p75_gr2$X4 <- "No Warming & No Deciduous Shift ***"
p75_gr3 <- data.frame(cbind(growth_rate_table_by_ysd3$`Year since Disturbance`,growth_rate_table_by_ysd3$`75%`,growth_rate_table_by_ysd3$`Mean Deciduous Fraction`)) 
p75_gr3$X4 <- "Warming & Deciduous Shift ***"
p75_gr4 <- data.frame(cbind(growth_rate_table_by_ysd4$`Year since Disturbance`,growth_rate_table_by_ysd4$`75%`,growth_rate_table_by_ysd4$`Mean Deciduous Fraction`)) 
p75_gr4$X4 <- "No Warming & Deciduous Shift ***"
p75_gr <- rbind(p75_gr1,
                p75_gr2,
                p75_gr3,
                p75_gr4)
names(p75_gr) <- c("Year Since Disturbance","Annual Growth Rate","Mean Deciduous Fraction","Warming Trend")
p75_gr$`Warming Trend` <- factor(p75_gr$`Warming Trend`, levels=c("No Warming & No Deciduous Shift ***",
                                                                  "No Warming & Deciduous Shift ***",
                                                                  "Warming & No Deciduous Shift",
                                                                  "Warming & Deciduous Shift ***") )

ggplot(data = p75_gr,aes(x=`Year Since Disturbance`,
                         y=`Annual Growth Rate`,
                         color=`Warming Trend`
))+geom_line(alpha=0.3,lwd=1)+
  geom_smooth(method = lm,size=2,linetype="dashed",se=0)+scale_y_continuous(limits = c(0.1,0.35))+
  #scale_color_manual(values = c("grey3","darkgreen",'tomato',"red"))+
  scale_color_manual(values = c("#F2AD00", "#72D8FF","#F76D5E","darkgreen"))+ 
  ylab("75th Percentile of Annual Growth Rate (m / year)")+xlab("Year Since Disturbance")+
  ylim(c(0.05,0.32))+
  theme(axis.text.y  = element_text(size = 15,face = "bold"),axis.title.x  = element_text(size = 15),axis.title.y = element_text(size= 15) ,
        axis.text.x= element_text(size = 15,face = "bold"),
        #axis.ticks.x=element_blank(),
        legend.key = element_rect(colour = NA, fill = NA),
        legend.background =  element_rect(fill = "transparent"),legend.text = element_text(size=15),legend.title = element_blank(),legend.position = c(0.37,0.15))
ggsave("//gssto1.umd.edu/gsdata1/duncansongp/fengt/icesat2_boreal_fire/figure/Manuscript_figures/p75_gr_deciduousness_comparison_col_updated_swi0.png", width = 10, height = 10, units = "in", dpi = 500)



evergreeness_group1$climate_group <- 1
evergreeness_group2$climate_group <- 2
deciduousness_group1$climate_group <-3
deciduousness_group2$climate_group <-4
composition_data <- rbind(evergreeness_group1,
                          evergreeness_group2,
                          deciduousness_group1,
                          deciduousness_group2)

summary(lm(deciduousness_group2$growth_rate~deciduousness_group2$year_since_disturbance))


cor_data <- data.frame(array(data = NA,dim = c(4,4)))
cor_data <- cor(cor_data)
rownames(cor_data) <- c("Warming & No Deciduous Shift",
                        "No Warming & No Deciduous Shift",
                        "Warming & Deciduous Shift",
                        "No Warming & Deciduous Shift"
)
colnames(cor_data) <- c("Warming & No Deciduous Shift",
                        "No Warming & No Deciduous Shift",
                        "Warming & Deciduous Shift",
                        "No Warming & Deciduous Shift"
)


cor_data2 <- cor_data

for(i in 1:4){
  for(j in 1:4){
    if(i != j){
      data_group <- composition_data[which(composition_data$climate_group == i | composition_data$climate_group == j),]
      result1 <- aov(growth_rate ~ year_since_disturbance*climate_group,data =data_group)
      if(summary(result1)[[1]][["Pr(>F)"]][1] <= 0.05){
        cor_data[i,j] <- 1
      }
      else{cor_data[i,j] <- 0}
      
      slope_i <- -summary(lm(growth_rate ~ year_since_disturbance,data = data_group[which(data_group$climate_group == i),]))$coefficients[2,1]
      coefficient_slope_i <- summary(lm(growth_rate ~ year_since_disturbance,data = data_group[which(data_group$climate_group == i),]))$coefficients[2,4]
      slope_j <- -summary(lm(growth_rate ~ year_since_disturbance,data = data_group[which(data_group$climate_group == j),]))$coefficients[2,1]
      coefficient_slope_j <- summary(lm(growth_rate ~ year_since_disturbance,data = data_group[which(data_group$climate_group == j),]))$coefficients[2,4]
      slope_diff <- slope_i - slope_j
      if(slope_diff > 0){
        cor_data2[i,j] <-  1
      } else{
        cor_data2[i,j] <-  0
      }
      
    }
    else{
      cor_data[i,j] <- NA
      cor_data2[i,j] <- NA
      
    }
    #print(coefficient_slope_i)
    #print(coefficient_slope_j)
    
  }
  
  print(j)}




get_lower_tri<-function(cormat){
  cormat[upper.tri(cormat)] <- NA
  return(cormat)
}
get_upper_tri <- function(cormat){
  cormat[lower.tri(cormat)]<- NA
  return(cormat)
}

library(reshape2)
#ggcorr(cor_data,nbreaks = 3)
cor_data_upper <- get_upper_tri(cor_data)
cor_data2_upper <- get_upper_tri(cor_data2)

melted_cor_data <- melt(cor_data_upper,na.rm = TRUE)
melted_cor_data2 <- melt(cor_data2_upper,na.rm = TRUE)

melted_cor_data$value2 <- melted_cor_data2$value

melted_cor_data$value <- factor(melted_cor_data$value,levels = c(0,1))
melted_cor_data$value2 <- factor(melted_cor_data$value2,levels = c(0,1))

ggplot(data = melted_cor_data, aes(Var2, Var1,fill = value2))+
  geom_tile(col='white')+
  scale_fill_manual(values = c("tomato","grey"))+
  geom_point(aes(size=value2))+
  scale_size_manual(values = c("1" = -1, "0"=1))+
  theme_minimal()+ 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 10, hjust = 1,colour = c( "black","black","black","black","black","black","black")),
        axis.text.y = element_text(angle = 45, vjust = 1, 
                                   size = 10, hjust = 1,colour=c("black", "black","black","black","black","black","black")),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.position = "none")+
  coord_fixed()
group_data <- deciduousness_group1
result1 <- aov(growth_rate ~ year_since_disturbance*climate_group,data = group_data)
summary(result1)

gr_group1 <- atl08_spatial_tmean_group1@data$growth_rate
ysd_group1 <- -atl08_spatial_tmean_group1@data$year_since_disturbance
summary(lm(gr_group1~ysd_group1))

gr_group2 <- atl08_spatial_tmean_group2@data$growth_rate
ysd_group2 <- -atl08_spatial_tmean_group2@data$year_since_disturbance
summary(lm(gr_group2~ysd_group2))

gr_group3 <- atl08_spatial_tmean_group3@data$growth_rate
ysd_group3 <- -atl08_spatial_tmean_group3@data$year_since_disturbance
summary(lm(gr_group3~ysd_group3))


###
deciduousness_group1$climate_group <- 1
deciduousness_group2$climate_group <- 2
evergreeness_group1$climate_group <- 3
evergreeness_group2$climate_group <- 4

output_data <- rbind(deciduousness_group1,deciduousness_group2,
                     evergreeness_group1,evergreeness_group2) 
output_data <- data.frame(cbind(output_data$lon,output_data$lat,output_data$climate_group))
output_spatial <- SpatialPointsDataFrame(data = output_data,coords = cbind(output_data$X1,output_data$X2),proj4string = CRS("+init=epsg:4326")) %>% spTransform(CRSobj = 5070)
output_spatial <- spTransform(output_spatial,CRSobj = "+proj=aea +lat_0=23 +lon_0=-96 +lat_1=29.5 +lat_2=45.5 +x_0=0 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs +type=crs")

raster_template <- raster(ext=extent(output_spatial),res=30,crs=output_spatial@proj4string)

output_spatial_grid <- rasterize(output_spatial,raster_template,output_spatial$X3)



gr_group1 <- evergreeness_group1$growth_rate
ysd_group1 <- floor(evergreeness_group1$year_since_disturbance)
ysd_list1 <- unique(ysd_group1)
summary(lm(gr_group1~ysd_group1))

gr_group2 <- evergreeness_group2$growth_rate
ysd_group2 <- floor(evergreeness_group2$year_since_disturbance)
ysd_list2 <- unique(ysd_group2)
summary(lm(gr_group2~ysd_group2))

gr_group3 <- deciduousness_group1$growth_rate
ysd_group3 <- floor(deciduousness_group1$year_since_disturbance)
ysd_list3 <- unique(ysd_group3)
summary(lm(gr_group3~ysd_group3))

gr_group4 <- deciduousness_group2$growth_rate
ysd_group4 <- floor(deciduousness_group2$year_since_disturbance)
ysd_list4 <- unique(ysd_group4)
summary(lm(gr_group4~ysd_group4))

ysd_list <- c(ysd_list1,ysd_list2,ysd_list3,ysd_list4)
gr_list <- c(-0.0018374 * ysd_list1 + 0.1914512,
             -0.00304877 * ysd_list2 + 0.25107666,
             -0.0041021 * ysd_list3 + 0.2302334,
             -0.00337091 * ysd_list4 + 0.23087999
)

group_list <- c(rep("Warming & No Deciduous Shift ***",length(ysd_list1)),
                rep("No Warming & No Deciduous Shift ***",length(ysd_list2)),
                rep("Warming & Deciduous Shift ***",length(ysd_list3)),
                rep("No Warming & Deciduous Shift ***",length(ysd_list4)))

gr_table <- data.frame(cbind(ysd_list,gr_list,group_list))
names(gr_table) <- c("Year Since Disturbance","Annual Growth Rate","Condition")
gr_table$`Year Since Disturbance` <- as.numeric(gr_table$`Year Since Disturbance`)
gr_table$`Annual Growth Rate` <- as.numeric(gr_table$`Annual Growth Rate`)
gr_table$Condition <- factor(gr_table$Condition, levels=c("No Warming & No Deciduous Shift ***",
                                                          "No Warming & Deciduous Shift ***",
                                                          "Warming & No Deciduous Shift ***",
                                                          "Warming & Deciduous Shift ***") )


ggplot(data = median_gr,aes(x=`Year Since Disturbance` * -1,
                            y=SWI0_y,
                            color=`Warming Trend`
                            
))+geom_point(aes(size=median_gr$`SWI0 Year of Disturbance`),alpha=0.8)+     #geom_smooth(method = lm,size=1,linetype="dashed",se=0)+scale_y_continuous(limits = c(0.05,0.18))+
  geom_abline(slope = 0.00296372,intercept = 0.22916359 ,col='#00A08A',lty='dashed',size=1)+
  geom_abline(slope = 0.0034770,intercept = 0.2201453 ,col='#F2AD00',lty='dashed',size=1)+
  geom_abline(slope = 0.007721 ,intercept = 0.306794  ,col='#FF0000',lty='dashed',size=1)+
  geom_abline(slope = 3.049e-03  ,intercept = 0.306794  ,col='darkgreen',lty='dashed',size=1)+
  scale_color_manual(values = c("#00A08A","#F2AD00","#FF0000","black"))+
  scale_size(range = c(2,10))+ylim(c(0.02,0.2))+
  scale_size_continuous(expression('SWI'[0]))+
  ylab("Annual Growth Rate (m / year)")+xlab("\nYear of Disturbance\nYear of ATL08 Acquisition - Year Since Disturbance")+
  theme(legend.box = "horizontal",
        axis.text.y  = element_text(size = 15,face = "bold"),axis.title.x  = element_text(size = 15),axis.title.y = element_text(size= 15) ,
        axis.text.x= element_blank(),
        axis.ticks.x=element_blank(),
        legend.key=element_rect(fill="white"),
        legend.text = element_text(size=22.5),legend.title = element_blank(),legend.position = c(0.72,0.9),
        legend.background =  element_blank())

