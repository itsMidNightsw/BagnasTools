#classify_multi_images_4bands_V2.r



data_path = paste(getwd(),"/Images_S2", sep = "")

all_images=dir(data_path, pattern="T31", full.names = F)
all_dates_str = substring(all_images,12,19)

all_dates_str = as.character(sort(as.numeric(all_dates_str)))

for (date in all_dates_str) {
  
  classify_simple_image(date = date, data_path = data_path, export = T,
                        export_dir_raster = paste(getwd(),"/Sorties/Images_classees/rasters", sep = ""),
                        export_dir_png = paste(getwd(),"/Sorties/Images_classees/plots/", sep = "")
  )
  
}



