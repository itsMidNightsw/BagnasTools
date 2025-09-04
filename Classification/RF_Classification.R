

####### ENTRAINEMENT RANDOM FOREST ########
source(file = paste(getwd(),"/Classification/rf_refined_full_dataset_2017to2023_4bands.R", sep = ""), local = F, echo = F)


####### LOAD FUNCTION ########
source(file = paste(getwd(),"/Classification/classify_simple_image_4bands_V2.R", sep = ""), local = F, echo = F)


###### CLASS ALL FOLDER IMAGES ######
source(file = paste(getwd(),"/Classification/classify_multi_images_4bands_V2.R", sep = ""), local = F, echo = F)

summary(subset(datarf, datarf$date == 20170707))
