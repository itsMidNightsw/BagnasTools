#RF_validation_2021_2023_4bands_V2


library(raster)
library(sf)
library(terra)
library(caret)
library(sp)
library(plotrix)
#library(verification)


data_terrain_2022 = read.csv(paste(getwd(),"/Data/Data_bagnas_validation_terrain/06-2022/releves_macrophytes_gb_2022-06-14.csv", sep = ""),
                             sep = ",")

data_terrain_03 = read.csv(paste(getwd(),"/Data/Data_bagnas_validation_terrain/03-2023/releves_macrophytes_gb_2023-03-23.csv", sep = ""),
                           sep = ";")

data_terrain_04 = read.csv(paste(getwd(),"/Data/Data_bagnas_validation_terrain/04-2023/releves_macrophytes_gb_2023-04.csv", sep = ""),
                           sep = ";")

data_terrain_05 = read.csv(paste(getwd(),"/Data/Data_bagnas_validation_terrain/05-2023/releves_macrophytes_gb_2023-05-23.csv", sep = ""),
                           sep = ";")

data_terrain_06 = read.csv(paste(getwd(),"/Data/Data_bagnas_validation_terrain/06-2023/releves_macrophytes_gb_2023-06-22.csv", sep = ""),
                           sep = ";")


#a mettre en forme pour faire un point data raster truc


data_terrain_2022 = data_terrain_2022[,c(5,6,12,14)]
data_terrain_2022[,4] = as.factor(data_terrain_2022[,4])
data_terrain_2022 = na.omit(data_terrain_2022)
data_terrain_2022 = data_terrain_2022[-c(1, 15, 18, 19, 23, 75, 104),] #hors GB
names(data_terrain_2022) = c("x", "y","couv1" ,"recouvrement")
summary(data_terrain_2022)

data_terrain_03 = data_terrain_03[,c(2,3,4,5)]
data_terrain_03[,3] = as.factor(data_terrain_03[,3])
data_terrain_03[,4] = as.factor(data_terrain_03[,4])
summary(data_terrain_03)

data_terrain_04 = data_terrain_04[,c(3,4,5,6)]
data_terrain_04[,3] = as.factor(data_terrain_04[,3])
data_terrain_04[,4] = as.factor(data_terrain_04[,4])
data_terrain_04 = na.omit(data_terrain_04)
summary(data_terrain_04)

data_terrain_05 = data_terrain_05[,c(3,4,5,6)]
data_terrain_05[,3] = as.factor(data_terrain_05[,3])
data_terrain_05[,4] = as.factor(data_terrain_05[,4])
summary(data_terrain_05)

data_terrain_06 = data_terrain_06[,c(3,4,5,6)]
data_terrain_06[,3] = as.factor(data_terrain_06[,3])
data_terrain_06[,4] = as.factor(data_terrain_06[,4])
summary(data_terrain_06)


datapoints_2022 = SpatialPointsDataFrame(coords = data_terrain_2022[,c(1,2)], data = data_terrain_2022[,c(3,4)],
                                    proj4string=CRS("EPSG:2154"))

datapoints_03 = SpatialPointsDataFrame(coords = data_terrain_03[,c(1,2)], data = data_terrain_03[,c(3,4)],
                                       proj4string=CRS("EPSG:2154"))

datapoints_04 = SpatialPointsDataFrame(coords = data_terrain_04[,c(1,2)], data = data_terrain_04[,c(3,4)],
                                       proj4string=CRS("EPSG:2154"))

datapoints_05 = SpatialPointsDataFrame(coords = data_terrain_05[,c(1,2)], data = data_terrain_05[,c(3,4)],
                                       proj4string=CRS("EPSG:2154"))

datapoints_06 = SpatialPointsDataFrame(coords = data_terrain_06[,c(1,2)], data = data_terrain_06[,c(3,4)],
                                       proj4string=CRS("EPSG:2154"))

######################

#raster_2021 = raster("~/Thèse/Resultats/2021_RF_4bands_V2/rasters/20210527.tif")
#raster_2021 = raster("~/Thèse/Resultats/2021_RF_4bands_V2/rasters/20210621.tif")
#raster_2021 = raster("~/Thèse/Resultats/2022_RF_4bands_V2/rasters/20220606.tif")
raster_2022 = raster("~/Thèse/Resultats/2022_RF_4bands_V2/rasters/20220611.tif")

crs(raster_2022) <- "+proj=utm +zone=31 +datum=WGS84 +units=m +no_defs"
raster_2022_lambert93 = projectRaster(raster_2022, crs = "+init=epsg:2154", method = "ngb")

raster_03 = raster("~/Thèse/Resultats/2023_RF_4bands_V2/rasters/20230328.tif")
crs(raster_03) <- "+proj=utm +zone=31 +datum=WGS84 +units=m +no_defs"
raster_03_lambert93 = projectRaster(raster_03, crs = "+init=epsg:2154", method = "ngb")

raster_04 = raster("~/Thèse/Resultats/2023_RF_4bands_V2/rasters/20230502.tif")
crs(raster_04) <- "+proj=utm +zone=31 +datum=WGS84 +units=m +no_defs"
raster_04_lambert93 = projectRaster(raster_04, crs = "+init=epsg:2154", method = "ngb")

raster_05 = raster("~/Thèse/Resultats/2023_RF_4bands_V2/rasters/20230601.tif")
crs(raster_05) <- "+proj=utm +zone=31 +datum=WGS84 +units=m +no_defs"
raster_05_lambert93 = projectRaster(raster_05, crs = "+init=epsg:2154", method = "ngb")

raster_06 = raster("~/Thèse/Resultats/2023_RF_4bands_V2/rasters/20230626.tif")
crs(raster_06) <- "+proj=utm +zone=31 +datum=WGS84 +units=m +no_defs"
raster_06_lambert93 = projectRaster(raster_06, crs = "+init=epsg:2154", method = "ngb")

##########################

raster::plot(raster_2022_lambert93, colNA="gray")
raster::plot(datapoints_2022[datapoints_2022$recouvrement == 0,], add = T, col = "black")
raster::plot(datapoints_2022[datapoints_2022$recouvrement == 1,], add = T, col = "black")
raster::plot(datapoints_2022[datapoints_2022$recouvrement== 2,], add = T, col = "red")
raster::plot(datapoints_2022[datapoints_2022$recouvrement == 3,], add = T, col = "orange")
raster::plot(datapoints_2022[datapoints_2022$recouvrement == 4,], add = T, col = "yellow3")
raster::plot(datapoints_2022[datapoints_2022$recouvrement == 5,], add = T, col = "blue")


raster::plot(raster_03_lambert93, colNA="gray")
raster::plot(datapoints_03[datapoints_03$recouvrement == 0,], add = T, col = "black")
raster::plot(datapoints_03[datapoints_03$recouvrement == 1,], add = T, col = "red")
raster::plot(datapoints_03[datapoints_03$recouvrement == 2,], add = T, col = "orange")


raster::plot(raster_04_lambert93, colNA="gray")
raster::plot(datapoints_04[datapoints_04$recouvrement == 0,], add = T, col = "black")
raster::plot(datapoints_04[datapoints_04$recouvrement == 1,], add = T, col = "red")
raster::plot(datapoints_04[datapoints_04$recouvrement == 2,], add = T, col = "orange")
raster::plot(datapoints_04[datapoints_04$recouvrement == 3,], add = T, col = "yellow3")


raster::plot(raster_05_lambert93, colNA="gray")
raster::plot(datapoints_05[datapoints_05$recouvrement == 0,], add = T, col = "black")
raster::plot(datapoints_05[datapoints_05$recouvrement == 1,], add = T, col = "red")
raster::plot(datapoints_05[datapoints_05$recouvrement == 2,], add = T, col = "orange2")
raster::plot(datapoints_05[datapoints_05$recouvrement == 3,], add = T, col = "yellow3")
raster::plot(datapoints_05[datapoints_05$recouvrement == 4,], add = T, col = "black")
raster::plot(datapoints_05[datapoints_05$recouvrement == 5,], add = T, col = "blue1")


raster::plot(raster_06_lambert93, colNA="gray")
raster::plot(datapoints_06[datapoints_06$recouvrement == 0,], add = T, col = "black")
raster::plot(datapoints_06[datapoints_06$recouvrement == 1,], add = T, col = "red")
raster::plot(datapoints_06[datapoints_06$recouvrement == 2,], add = T, col = "orange2")
raster::plot(datapoints_06[datapoints_06$recouvrement == 3,], add = T, col = "yellow3")
raster::plot(datapoints_06[datapoints_06$recouvrement == 4,], add = T, col = "black")
raster::plot(datapoints_06[datapoints_06$recouvrement == 5,], add = T, col = "blue1")

#############################

veccat = c(0,1,2,3,4,5)
abs_count_03 = c(11,18,2,0,0,0)
pre_count_03 = c(0,0,0,0,0,0)

valid_03_4bands = data.frame(veccat,abs_count_03,pre_count_03)


abs_count_04 = c(25,73,33,9,0,0)
pre_count_04 = c(0,0,0,0,0,0)


valid_04_4bands = data.frame(veccat,abs_count_04,pre_count_04)


abs_count_05 = c(4,4,6,2,4,1)
pre_count_05 = c(1,1,0,0,5,11)


valid_05_4bands = data.frame(veccat,abs_count_05,pre_count_05)


abs_count_06 = c(7,3,0,2,0,0)
pre_count_06 = c(1,2,5,2,3,32)


valid_06_4bands = data.frame(veccat,abs_count_06,pre_count_06)
##########################################

lvs <- c("pre", "abs")
truth <- factor(rep(lvs, times = c(93, 10)),
                levels = rev(lvs))
pred <- factor(
  c(
    rep(lvs, times = c(87, 6)),
    rep(lvs, times = c(2, 8))),               
  levels = rev(lvs))

xtab <- table(pred, truth)
confusionMatrix(xtab)


############# CIRCLE COUNTING ##############


#raster::plot(raster_05_lambert93, colNA="gray")
#raster::plot(datapoints_05[datapoints_05$recouvrement == 0,], add = T, col = "black")
#identifier l'image

#image = raster_05_lambert93
#points = datapoints_05

field_validation = function(image, points, width_value){
  
  abs_count = rep(0,6)
  pre_count = rep(0,6)
  total = rep(0,6)
  veccat = c(0,1,2,3,4,5)
  valid_table = data.frame(veccat,abs_count,pre_count, total)
  
  
  max_recouv = as.numeric(max(levels(points$recouvrement))) #identifier le recouvrement max
  
  for (l in c(0:max_recouv)) {
    
    
    points_subset = points[points$recouvrement == l,]
    length_subset = length(points_subset) #max of subset dataset
    
    
    
    for (m in c(1:length_subset)) {
      
      one_point = points_subset[m,]
      #buffer <- gBuffer(one_point, byid = T, width = width_value, capStyle = 'round')  OLD, was working with rgeos and rgdal
      
      buffer <- terra::buffer(one_point, width = width_value, capstyle="round") 
     
      #raster::plot(image, colNA="gray")
      #raster::plot(buffer, add = T, col = "black")
      
      #break
      #raster::plot(image, colNA="gray")
      #raster::plot(one_point, add = T, col = "black")
      #draw.circle(one_point@coords[1],one_point@coords[2],radius = 20, border = "red")
      
      
      
      count_extract = raster::extract(image,buffer, cellnumber = F, df = T)
      non_na_pxl_n = length(na.omit(count_extract[,2]))
      count_sum = sum(na.omit(count_extract[,2]))
      
      test_pre = count_sum>(non_na_pxl_n/2) # true = pre
      
      if (test_pre == T) {valid_table[l+1,3] = valid_table[l+1,3]+1}
      if (test_pre == F) {valid_table[l+1,2] = valid_table[l+1,2]+1} # coord dans le tableau a actualiser
      
    }
    
    valid_table[l+1,4] = valid_table[l+1,2] + valid_table[l+1,3]
    
  }
  
  return(valid_table)
}


field_validation(raster_2022_lambert93, datapoints_2022, width_value = 20)
field_validation(raster_03_lambert93, datapoints_03, width_value = 20)
field_validation(raster_04_lambert93, datapoints_04, width_value = 20)
field_validation(raster_05_lambert93, datapoints_05, width_value = 20)
field_validation(raster_06_lambert93, datapoints_06, width_value = 20)

################ TEST ACCURACY PROFILE ################

compute_accuracy_profile = function(table_in){
  
  accuracy_profile = data.frame(c("0%+","0-5%+","5-25%+","25-50%+","50-75%+","75-100%+"),rep(0,6),rep(0,6),rep(0,6),rep(0,6),rep(0,6))
  names(accuracy_profile) = c("cat_seuil", "accuracy", "sensitivity", "specificity","fall_out", "F1_score")
  #accuracy_profile
  conf_mat_list = list()
  for (s in c(0:5)) {
    
    cat_seuil = s # on ira jusqu'a 5
    
    
    table_down_seuil = table_in[table_in$veccat<=cat_seuil,] #abscences seuil
    table_up_seuil = table_in[table_in$veccat>cat_seuil,] #presences seuil
    
    sums_abs = colSums(table_down_seuil)
    sums_abs = sums_abs[-c(1,4)]
    names(sums_abs) = c("TN","FN") #c("hits", "misses", "total")
    #print(sums_abs)
    
    sums_pre = colSums(table_up_seuil)
    sums_pre = sums_pre[-c(1,4)]
    names(sums_pre) = c("FP","TP") #c("misses", "hits", "total")
    sums_pre <- sums_pre[c(2, 1)]
    #print(sums_pre)
    
    
    TN = sum(table_down_seuil$abs_count)
    FN = sum(table_down_seuil$pre_count)
    
    FP = sum(table_up_seuil$abs_count)
    TP = sum(table_up_seuil$pre_count)
    
    
    conf_mat_list[[s+1]] = matrix(c(TN,FN,
                        FP,TP), nrow = 2, ncol = 2)
    
    
    
    #accuracy_tab = sums_abs + sums_pre
    #print(accuracy_tab)
    
    #accuracy = accuracy_tab[1]/accuracy_tab[3] #final accuracy for this threshold
    
    Accuracy = (TP+TN)/(TP+TN+FP+FN)
    
    Sensitivity = TP/(TP+FN)
    Specificity = TN/(TN+FP)
    Fall_out = FP/(FP+TN)
    
    
    F1_score = (2*TP)/((2*TP)+FP+FN)
    
    accuracy_profile[s+1,2] = Accuracy
    accuracy_profile[s+1,3] = Sensitivity
    accuracy_profile[s+1,4] = Specificity
    accuracy_profile[s+1,5] = Fall_out
    
    accuracy_profile[s+1,6] = F1_score
  }
  
  return(accuracy_profile)
  #return(conf_mat_list)
}

table_2022 = field_validation(raster_2022_lambert93, datapoints_2022, width_value = 20)
accuracy_table2022 = compute_accuracy_profile(table_in = table_2022)

table_03 = field_validation(raster_03_lambert93, datapoints_03, width_value = 20)
accuracy_table03 = compute_accuracy_profile(table_in = table_03)

table_04 = field_validation(raster_04_lambert93, datapoints_04, width_value = 20)
accuracy_table04 = compute_accuracy_profile(table_in = table_04)

table_05 = field_validation(raster_05_lambert93, datapoints_05, width_value = 20)
accuracy_table05 = compute_accuracy_profile(table_in = table_05)

table_06 = field_validation(raster_06_lambert93, datapoints_06, width_value = 20)
accuracy_table06 = compute_accuracy_profile(table_in = table_06)



############# TO PLOT #############

library(ggplot2)
library(reshape2)

############ ALL ##############

#### ACCURACY ####

data_to_plot = data.frame(accuracy_table03$cat_seuil,
                          accuracy_table2022$accuracy,
                          accuracy_table03$accuracy,
                          accuracy_table04$accuracy,
                          accuracy_table05$accuracy,
                          accuracy_table06$accuracy)

names(data_to_plot) = c("Seuils", "2022-06-14", "2023-03-28", "2023-05-02", "2023-06-01", "2023-06-26")
#data_to_plot$Seuils = as.factor(data_to_plot$Seuils)
data_to_plot$Seuils = factor(data_to_plot$Seuils, levels = c("0%+","0-5%+","5-25%+","25-50%+","50-75%+","75-100%+"))
data_to_plot$Mean = 0
data_to_plot$Mean = rowMeans(data_to_plot[, 2:6])

mdf <- melt(data_to_plot,id.vars="Seuils")
names(mdf) = c("Threshold", "Date", "Accuracy")

ggplot(mdf, aes(x=Threshold, y=Accuracy, colour=Date, group=Date)) + 
  ggtitle("Accuracy")+
  geom_line() +
  scale_color_manual(values=c("2022-06-14"="purple","2023-03-28"="red","2023-05-02"="orange",
                              "2023-06-01"="green", "2023-06-26"="blue", "Mean"="black"))#+




#### SENSITIVITY ####

data_to_plot = data.frame(accuracy_table03$cat_seuil,
                          accuracy_table2022$sensitivity,
                          accuracy_table03$sensitivity,
                          accuracy_table04$sensitivity,
                          accuracy_table05$sensitivity,
                          accuracy_table06$sensitivity)

names(data_to_plot) = c("Seuils","2022-06-14", "2023-03-28", "2023-05-02", "2023-06-01", "2023-06-26")
#data_to_plot$Seuils = as.factor(data_to_plot$Seuils)
data_to_plot$Seuils = factor(data_to_plot$Seuils, levels = c("0%+","0-5%+","5-25%+","25-50%+","50-75%+","75-100%+"))
data_to_plot$Mean = 0
data_to_plot$Mean = rowMeans((data_to_plot[, 2:6]), na.rm = T)

mdf <- melt(data_to_plot,id.vars="Seuils")
names(mdf) = c("Threshold", "Date", "Sensitivity")

ggplot(mdf, aes(x=Threshold, y=Sensitivity, colour=Date, group=Date)) + 
  ggtitle("Sensitivity (True positive rate)")+
  geom_line() +
  scale_color_manual(values=c("2022-06-14"="purple","2023-03-28"="red","2023-05-02"="orange",
                              "2023-06-01"="green", "2023-06-26"="blue", "Mean"="black"))#+


#### SPECIFICITY ####

data_to_plot = data.frame(accuracy_table03$cat_seuil,
                          accuracy_table2022$specificity,
                          accuracy_table03$specificity,
                          accuracy_table04$specificity,
                          accuracy_table05$specificity,
                          accuracy_table06$specificity)

names(data_to_plot) = c("Seuils","2022-06-14", "2023-03-28", "2023-05-02", "2023-06-01", "2023-06-26")
#data_to_plot$Seuils = as.factor(data_to_plot$Seuils)
data_to_plot$Seuils = factor(data_to_plot$Seuils, levels = c("0%+","0-5%+","5-25%+","25-50%+","50-75%+","75-100%+"))
data_to_plot$Mean = 0
data_to_plot$Mean = rowMeans(data_to_plot[, 2:6])

mdf <- melt(data_to_plot,id.vars="Seuils")
names(mdf) = c("Threshold", "Date", "Specificity")

ggplot(mdf, aes(x=Threshold, y=Specificity, colour=Date, group=Date)) + 
  ggtitle("Specificity (True negative rate)")+
  geom_line() +
  scale_color_manual(values=c("2022-06-14"="purple","2023-03-28"="red","2023-05-02"="orange",
                              "2023-06-01"="green", "2023-06-26"="blue", "Mean"="black"))#+


#### F1 score ####

data_to_plot = data.frame(accuracy_table03$cat_seuil,
                          accuracy_table2022$F1_score,
                          accuracy_table03$F1_score,
                          accuracy_table04$F1_score,
                          accuracy_table05$F1_score,
                          accuracy_table06$F1_score)

names(data_to_plot) = c("Seuils","2022-06-14", "2023-03-28", "2023-05-02", "2023-06-01", "2023-06-26")
#data_to_plot$Seuils = as.factor(data_to_plot$Seuils)
data_to_plot$Seuils = factor(data_to_plot$Seuils, levels = c("0%+","0-5%+","5-25%+","25-50%+","50-75%+","75-100%+"))
data_to_plot$Mean = 0
data_to_plot$Mean = rowMeans(data_to_plot[, 2:6],na.rm = T)

mdf <- melt(data_to_plot,id.vars="Seuils")
names(mdf) = c("Threshold", "Date", "F1_score")

ggplot(mdf, aes(x=Threshold, y=F1_score, colour=Date, group=Date)) + 
  ggtitle("F1_score")+
  geom_line() +
  scale_color_manual(values=c("2022-06-14"="purple","2023-03-28"="red","2023-05-02"="orange",
                              "2023-06-01"="green", "2023-06-26"="blue", "Mean"="black"))#+






############ 3 DATES ##############

#### ACCURACY ####

data_to_plot = data.frame(accuracy_table2022$cat_seuil,
                          accuracy_table2022$accuracy,
                          accuracy_table05$accuracy,
                          accuracy_table06$accuracy)

names(data_to_plot) = c("Seuils", "2022-06-14", "2023-06-01", "2023-06-26")
#data_to_plot$Seuils = as.factor(data_to_plot$Seuils)
data_to_plot$Seuils = factor(data_to_plot$Seuils, levels = c("0%+","0-5%+","5-25%+","25-50%+","50-75%+","75-100%+"))
data_to_plot$Mean = 0
data_to_plot$Mean = rowMeans(data_to_plot[, 2:4])

mdf <- melt(data_to_plot,id.vars="Seuils")
names(mdf) = c("Threshold", "Date", "Accuracy")

ggplot(mdf, aes(x=Threshold, y=Accuracy, colour=Date, group=Date)) + 
  ggtitle("Accuracy")+
  geom_line() +
  scale_color_manual(values=c("2022-06-14"="purple", "2023-06-01"="green", "2023-06-26"="blue", "Mean"="black"))#+




#### SENSITIVITY ####

data_to_plot = data.frame(accuracy_table2022$cat_seuil,
                          accuracy_table2022$sensitivity,
                          accuracy_table05$sensitivity,
                          accuracy_table06$sensitivity)

names(data_to_plot) = c("Seuils","2022-06-14", "2023-06-01", "2023-06-26")
#data_to_plot$Seuils = as.factor(data_to_plot$Seuils)
data_to_plot$Seuils = factor(data_to_plot$Seuils, levels = c("0%+","0-5%+","5-25%+","25-50%+","50-75%+","75-100%+"))
data_to_plot$Mean = 0
data_to_plot$Mean = rowMeans((data_to_plot[, 2:4]), na.rm = T)

mdf <- melt(data_to_plot,id.vars="Seuils")
names(mdf) = c("Threshold", "Date", "Sensitivity")

ggplot(mdf, aes(x=Threshold, y=Sensitivity, colour=Date, group=Date)) + 
  ggtitle("Sensitivity (True positive rate)")+
  geom_line() +
  scale_color_manual(values=c("2022-06-14"="purple", "2023-06-01"="green", "2023-06-26"="blue", "Mean"="black"))#+

#### SPECIFICITY ####

data_to_plot = data.frame(accuracy_table2022$cat_seuil,
                          accuracy_table2022$specificity,
                          accuracy_table05$specificity,
                          accuracy_table06$specificity)

names(data_to_plot) = c("Seuils","2022-06-14", "2023-06-01", "2023-06-26")
#data_to_plot$Seuils = as.factor(data_to_plot$Seuils)
data_to_plot$Seuils = factor(data_to_plot$Seuils, levels = c("0%+","0-5%+","5-25%+","25-50%+","50-75%+","75-100%+"))
data_to_plot$Mean = 0
data_to_plot$Mean = rowMeans(data_to_plot[, 2:4])

mdf <- melt(data_to_plot,id.vars="Seuils")
names(mdf) = c("Threshold", "Date", "Specificity")

ggplot(mdf, aes(x=Threshold, y=Specificity, colour=Date, group=Date)) + 
  ggtitle("Specificity (True negative rate)")+
  geom_line() +
  scale_color_manual(values=c("2022-06-14"="purple", "2023-06-01"="green", "2023-06-26"="blue", "Mean"="black"))#+

#### F1 score ####

data_to_plot = data.frame(accuracy_table2022$cat_seuil,
                          accuracy_table2022$F1_score,
                          accuracy_table05$F1_score,
                          accuracy_table06$F1_score)

names(data_to_plot) = c("Seuils","2022-06-14", "2023-06-01", "2023-06-26")
#data_to_plot$Seuils = as.factor(data_to_plot$Seuils)
data_to_plot$Seuils = factor(data_to_plot$Seuils, levels = c("0%+","0-5%+","5-25%+","25-50%+","50-75%+","75-100%+"))
data_to_plot$Mean = 0
data_to_plot$Mean = rowMeans(data_to_plot[, 2:4],na.rm = T)

mdf <- melt(data_to_plot,id.vars="Seuils")
names(mdf) = c("Threshold", "Date", "F1_score")

ggplot(mdf, aes(x=Threshold, y=F1_score, colour=Date, group=Date)) + 
  ggtitle("F1_score")+
  geom_line() +
  scale_color_manual(values=c("2022-06-14"="purple", "2023-06-01"="green", "2023-06-26"="blue", "Mean"="black"))#+








#### ROC Curve ####

dataroc2022 = data.frame(accuracy_table2022$cat_seuil,accuracy_table2022$sensitivity,accuracy_table2022$fall_out)
dataroc03 = data.frame(accuracy_table03$sensitivity,accuracy_table03$fall_out)
dataroc04 = data.frame(accuracy_table04$sensitivity,accuracy_table04$fall_out)
dataroc05 = data.frame(accuracy_table05$cat_seuil,accuracy_table05$sensitivity,accuracy_table05$fall_out)
dataroc06 = data.frame(accuracy_table06$cat_seuil, accuracy_table06$sensitivity,accuracy_table06$fall_out)



datatest <- do.call(rbind, list(accuracy_table2022,accuracy_table05,accuracy_table06))
datatest$cat_seuil = factor(datatest$cat_seuil, levels = c("0%+","0-5%+","5-25%+","25-50%+","50-75%+","75-100%+"))
datatest <- aggregate(datatest[-c(1)], list(datatest$cat_seuil), mean)

dataroc_all = data.frame(datatest$Group.1,datatest$sensitivity,datatest$fall_out)

point11 = dataroc06
point11 = point11[1,]
point11[1,1]= "ini"
point11[1,2] = 1
point11[1,3] = 1

names(point11) = names(dataroc2022)
dataroc2022 = rbind(point11,dataroc2022)

names(point11) = names(dataroc05)
dataroc05 = rbind(point11,dataroc05)

names(point11) = names(dataroc06)
dataroc06 = rbind(point11,dataroc06)

names(point11) = names(dataroc_all)
dataroc_all = rbind(point11,dataroc_all)


plot(dataroc2022$accuracy_table2022.sensitivity~dataroc2022$accuracy_table2022.fall_out, ylim = c(0,1), xlim = c(0,1), type = "l")
abline(0,1, lty = 2)
plot(dataroc03$accuracy_table03.sensitivity~dataroc03$accuracy_table03.fall_out, ylim = c(0,1), xlim = c(0,1), type = "l")
abline(0,1, lty = 2)
plot(dataroc04$accuracy_table04.sensitivity~dataroc04$accuracy_table04.fall_out, ylim = c(0,1), xlim = c(0,1), type = "l")
abline(0,1, lty = 2)
plot(dataroc05$accuracy_table05.sensitivity~dataroc05$accuracy_table05.fall_out, ylim = c(0,1), xlim = c(0,1), type = "l")
abline(0,1, lty = 2)
plot(dataroc06$accuracy_table06.sensitivity~dataroc06$accuracy_table06.fall_out, ylim = c(0,1), xlim = c(0,1), type = "l")
abline(0,1, lty = 2)

plot(dataroc_all$datatest.sensitivity~dataroc_all$datatest.fall_out, ylim = c(0,1), xlim = c(0,1), type = "l")
abline(0,1, lty = 2)


#### BETTER PLOT #####

dataroc_all$date = rep("Mean",7)
dataroc2022$date = rep("2022-06-14", 7)
dataroc05$date = rep("2023-06-01", 7)
dataroc06$date = rep("2023-06-26", 7)

names(dataroc2022) = c("cat_seuil", "sensitivity", "fall_out", "date")
names(dataroc05) = c("cat_seuil", "sensitivity", "fall_out", "date")
names(dataroc06) = c("cat_seuil", "sensitivity", "fall_out", "date")
names(dataroc_all) = c("cat_seuil", "sensitivity", "fall_out", "date")

data_to_plot = rbind(dataroc2022, dataroc05, dataroc06, dataroc_all)
data_to_plot$cat_seuil = factor(data_to_plot$cat_seuil, levels = c("ini","0%+","0-5%+","5-25%+","25-50%+","50-75%+","75-100%+"))


ggplot(data_to_plot, aes(x=fall_out, y=sensitivity, colour=date, group=date)) + 
  ggtitle("ROC")+
  geom_path() +
  geom_abline(lty = 3)+
  scale_linetype_manual(values=c("2022-06-14"="dashed", "2023-06-01"="dashed", "2023-06-26"="dashed", "Mean"="solid"))+
  scale_color_manual(values=c("2022-06-14"="purple", "2023-06-01"="green", "2023-06-26"="blue", "Mean"="black"))
  






############### confusion matrixes ##################
#modifer le return de la fonction plus haut
table_2022 = field_validation(raster_2022_lambert93, datapoints_2022, width_value = 20)
conf_mat_list_2022 =  compute_accuracy_profile(table_in = table_2022)

table_05 = field_validation(raster_05_lambert93, datapoints_05, width_value = 20)
conf_mat_list_05 = compute_accuracy_profile(table_in = table_05)

table_06 = field_validation(raster_06_lambert93, datapoints_06, width_value = 20)
conf_mat_list_06 = compute_accuracy_profile(table_in = table_06)


conf_mat_all_2550 = conf_mat_list_2022[[4]] + conf_mat_list_05[[4]] + conf_mat_list_06[[4]]
conf_mat_all_5075 = conf_mat_list_2022[[5]] + conf_mat_list_05[[5]] + conf_mat_list_06[[5]]
