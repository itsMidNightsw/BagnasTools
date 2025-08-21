#classify_multi_images_4bands_V2.r


### data_path_xxxx à completer en fonction de la localisation des données (ici, disque dur externe "my passport")


###################################
############# 2017 ################
###################################

data_path_2017 = "/media/adam/My Passport/DATA_THESE/DATA/2017"

dates_ordered_2017 = c("20170103","20170113","20170222","20170314","20170403","20170423","20170523","20170602",
                       "20170612","20170622","20170707","20170712","20170717","20170722","20170806","20170821",
                       "20170905","20170910","20170920","20171005","20171010","20171025","20171030","20171114",
                       "20171119","20171129","20171209","20171219","20171224")




for (date in dates_ordered_2017) {
  
  classify_simple_image(date = date, data_path = data_path_2017, export = F,
                        export_dir_raster = paste(getwd(),"/Classified_maps/2017_RF_4bands_V2/rasters", sep = ""),
                        export_dir_png = paste(getwd(),"/Classified_maps/2017_RF_4bands_V2/plots", sep = "")
  )
  
}

#classify_simple_image(date = "20170905", data_path = data_path_20172018)

###################################
############# 2018 ################
###################################

data_path_2018 = "/media/adam/My Passport/DATA_THESE/DATA/2018"

dates_ordered_2018 = c("20180103","20180123","20180128","20180202","20180227","20180314","20180319","20180418",
                       "20180503","20180518","20180523","20180627","20180702","20180707","20180717","20180727",
                       "20180801","20180806","20180811","20180816","20180821","20180826","20180905","20180920",
                       "20180925","20180930","20181005","20181020","20181025","20181124","20181204","20181214",
                       "20181224","20181229"
)




for (date in dates_ordered_2018) {
  
  classify_simple_image(date = date, data_path = data_path_2018, export = F,
                        export_dir_raster = paste(getwd(),"/Classified_maps/2018_RF_4bands_V2/rasters", sep = ""),
                        export_dir_png = paste(getwd(),"/Classified_maps/2018_RF_4bands_V2/plots", sep = "")
  )
  
}

###################################
############# 2019 ################
###################################

data_path_2019 = "/media/adam/My Passport/DATA_THESE/DATA/2019"

dates_ordered_2019 = c("20190103","20190212","20190222","20190227","20190304","20190309","20190324","20190329",
                       "20190413","20190513","20190523","20190602","20190617","20190622","20190627","20190707",
                       "20190712","20190717","20190722","20190801","20190811","20190816","20190831","20190905",
                       "20190915","20191015","20191025","20191104","20191229")




for (date in dates_ordered_2019) {
  
  classify_simple_image(date = date, data_path = data_path_2019, export = F,
                        export_dir_raster = paste(getwd(),"/Classified_maps/2019_RF_4bands_V2/rasters", sep = ""),
                        export_dir_png = paste(getwd(),"/Classified_maps/2019_RF_4bands_V2/plots", sep = "")
  )
  
}


###################################
############# 2020 ################
###################################

data_path_2020 = "/media/adam/My Passport/DATA_THESE/DATA/2020"

dates_ordered_2020 = c("20200108","20200222","20200308","20200313","20200318","20200328","20200402","20200407",
                       "20200507","20200512","20200517","20200522","20200527","20200606","20200616","20200621",
                       "20200706","20200711","20200716","20200726","20200731","20200805","20200815","20200825",
                       "20200904","20200914","20201024","20201118","20201123","20201203","20201228")


for (date in dates_ordered_2020) {
  
  classify_simple_image(date = date, data_path = data_path_2020, export = F,
                        export_dir_raster = paste(getwd(),"/Classified_maps/2020_RF_4bands_V2/rasters", sep = ""),
                        export_dir_png = paste(getwd(),"/Classified_maps/2020_RF_4bands_V2/plots", sep = "")
  )
  
}



###################################
############# 2021 ################
###################################


data_path_2021 = "/media/adam/My Passport/DATA_THESE/DATA/2021"

dates_ordered_2021 = c("20210127","20210211","20210226","20210308","20210313","20210323","20210328","20210407",
                       "20210412","20210422","20210527","20210621","20210626","20210701","20210711","20210716",
                       "20210721","20210726","20210731","20210810","20210820","20210825","20210830","20210919",
                       "20210924","20210929","20211009","20211014","20211118","20211128","20211218")



for (date in dates_ordered_2021) {
  
  classify_simple_image(date = date, data_path = data_path_2021, export = F,
                        export_dir_raster = paste(getwd(),"/Classified_maps/2021_RF_4bands_V2/rasters", sep = ""),
                        export_dir_png = paste(getwd(),"/Classified_maps/2021_RF_4bands_V2/plots", sep = "")
  )
  
}


###################################
############# 2022 ################
###################################



data_path_2022 = "/media/adam/My Passport/DATA_THESE/DATA/2022"

dates_ordered_2022 = c("20220112","20220117","20220122","20220127","20220201","20220206","20220211","20220221",
                       "20220226","20220303","20220323","20220328","20220417","20220422","20220427","20220517",
                       "20220527","20220606","20220611","20220701","20220711","20220716","20220721","20220726",
                       "20220805","20220810","20220815","20220820","20220904","20220919","20221004","20221009", 
                       "20221223")



for (date in dates_ordered_2022) {
  
  classify_simple_image(date = date, data_path = data_path_2022, export = F,
                        export_dir_raster = paste(getwd(),"/Classified_maps/2022_RF_4bands_V2/rasters", sep = ""),
                        export_dir_png = paste(getwd(),"/Classified_maps/2022_RF_4bands_V2/plots", sep = "")
  )
  
}


###################################
############# 2023 ################
###################################

data_path_2023 = "/media/adam/My Passport/DATA_THESE/DATA/2023"

# dates_ordered_2023 = c("20230112","20230127","20230201","20230206","20230211","20230226","20230303","20230328",
#                        "20230407","20230417","20230502","20230517","20230527","20230601","20230616","20230626",
#                        "20230716")
# 
# dates_ordered_2023 = c("20230328","20230502","20230527")


dates_ordered_2023 = c("20230112","20230127","20230201","20230206","20230211","20230226","20230303","20230328",
                       "20230407","20230417","20230502","20230517","20230527","20230601","20230616","20230626",
                       "20230716","20230726","20230731","20230810","20230820","20230830","20230904","20230909",
                       "20230914","20230919","20230924","20230929","20231004","20231009","20231108","20231203",
                       "20231218","20231223")


for (date in dates_ordered_2023) {
  
  classify_simple_image(date = date, data_path = data_path_2023, export = F,
                        export_dir_raster = paste(getwd(),"/Classified_maps/2023_RF_4bands_V2/rasters", sep = ""),
                        export_dir_png = paste(getwd(),"/Classified_maps/2023_RF_4bands_V2/plots", sep = "")
  )
  
}

