

classify_simple_image = function(date, data_path, export, export_dir_raster, export_dir_png){
  
  library(sp)
  library(RStoolbox)
  library(matrixStats)
  library(Hmisc)
  library(htmlwidgets)
  library(ggplot2)
  library(devtools)
  library(raster)
  library(terra)
  
  #necessite une image, la date de l'image, un obj random forest, les differents shapefiles
  
  
  print(date)
  
  imagery=dir(data_path, pattern="T31", full.names = F)  #nom des dossiers d'accés aux images brutes
  imagery_fullnames=dir(data_path, pattern="T31", full.names = T)
  
  #'Lire les shapefiles zone d'étude
  
  emprise=shapefile(paste(getwd(),"/Data/Couches_Bagnas/emprise.shp", sep = "")) #shapefile emprise pour cropping
  lagune_elargie=shapefile(paste(getwd(),"/Data/Couches_Bagnas/lagune_elargie.shp", sep = "")) #shapefile lagune grand bagnas elargie avec bassins ouest
  grand_bagnas=shapefile(paste(getwd(),"/Data/Couches_Bagnas/grand_bagnas.shp", sep = "")) #shapefile lagune grand bagnas 
  masque_roseliere = raster(paste(getwd(),"/Data/mask_bagnas/mask_lagune.tif", sep = ""))
  

  pixels_to_class = masque_roseliere == 2
  
  pxlpoints = rasterToPoints(pixels_to_class)
  ind = which(pxlpoints[,3] == 1)
  pxlpoints = pxlpoints[ind,]
  pxlpoints = pxlpoints[,-c(3)]
  
  
  masque_roseliere <- masque_roseliere == 1
  
  ind0 = which(masque_roseliere@data@values == 0)
  masque_roseliere[ind0] = NA
  
  
  
  ## find the sub directory
  
  name=paste(date,"stack", sep="_")
  files = dir(data_path, pattern=as.character(date), full.names = T)
  file = files[1]
  DATA_path<-file.path(file)
  
  
  
  #' Concaténation des bandes et découpage de la zone
  filenameB2=file.path(DATA_path,dir(DATA_path, pattern="FRE_B2"))
  B2=raster(filenameB2[1])
  filenameB3=file.path(DATA_path,dir(DATA_path, pattern="FRE_B3"))
  B3=raster(filenameB3[1])
  filenameB4=file.path(DATA_path,dir(DATA_path, pattern="FRE_B4"))
  B4=raster(filenameB4[1])
  filenameB8=file.path(DATA_path,dir(DATA_path, pattern="FRE_B8"))
  B8=raster(filenameB8[1]) #sélection du premier B8 et pas B8A
  stack<-addLayer(B2,B3,B4,B8)
  
  data<-crop(stack,emprise)
  
  names(data)<-c("bleu", "vert", "rouge", "pir")
  
  
  
  
  #' Passer en réflectance
  data[]<-getValues(data)/10000 #attention vérifier que réflectance jamais inférieure à 0 et pas de NA
  
  
  # 4bandes 
  bleu = data$bleu
  vert = data$vert
  rouge = data$rouge
  pir = data$pir
  

  bleu_extract = raster::extract(bleu, pxlpoints, cellnumber = T ,df = T)
  cell = bleu_extract$cell
  xy = raster::xyFromCell(bleu, cell = cell)
  
  xdf = xy[,1]
  ydf = xy[,2]
  
  datedf = rep(date, length(cell))
  #polyclassdf = rep(polyclassnames[m], length(cell))
  
  part_dataset = data.frame(xdf,ydf,datedf,cell,bleu_extract$bleu)
  colnames(part_dataset) <- c("x","y","date","cell","bleu")
  
  forindicesnames = c(vert, rouge, pir)
  nameofcols = c("vert", "rouge", "pir")
  
  for(l in c(1:3)){ #boucle sur les indices sauf dvi
    #part_dataset$add = rep(NA, length(cell))
    #extract_out = raster::extract(forindicesnames[[l]],grand_bagnas, cellnumber = T, df = T)
    extract_out = raster::extract(forindicesnames[[l]],pxlpoints, cellnumber = T, df = T)
    part_dataset$add= extract_out[,3]
    colnames(part_dataset)[colnames(part_dataset) == 'add'] <- nameofcols[l]
  }
  
  print(summary(part_dataset))
  image_data_all = part_dataset
  image_data_rf = image_data_all[-c(1:4)]

  
  ############# RF
  
  p3pa <- predict(rfpa, image_data_rf)
  image_data_all$pre_abs = p3pa
  
  
  ############ raster reconstruction and plot
  
  xyzfile = image_data_all[, c(1,2,9)]
  xyzfile$pre_abs = ifelse(xyzfile$pre_abs=="pre", 1, 0)
  xyzraster <- rasterFromXYZ(xyzfile)  #Convert first two columns as lon-lat and third as value        
  #xyzraster
  raster::plot(xyzraster, col = c("white","darkgreen"), legend = F)
  raster::plot(masque_roseliere, add = T, col = "gray", legend = F)
  #legend("topleft", legend = date)
  legend("bottomright", inset=.02, title=date,
         c("herbier", "roseliere"), fill=c("darkgreen", "gray"), horiz=F, cex=1)
  
  
  
  #############export
  
  if (export == T){
    #poly = rasterToPolygons(xyzraster)
    exp_file = export_dir_raster
    #raster::shapefile(poly, filename = paste(exp_file, date, sep = "/"))
    raster::writeRaster(xyzraster, format = "GTiff", filename = paste(exp_file, date, sep = "/"))
    
    png(file = paste(export_dir_png, date, ".png", sep = ""), width = 700, height = 550)
    raster::plot(xyzraster, col = c("white","darkgreen"), legend = F)
    raster::plot(masque_roseliere, add = T, col = "gray", legend = F)
    #legend("topleft", legend = date)
    legend("bottomright", inset=.02, title=date,
           c("herbier", "roseliere"), fill=c("darkgreen", "gray"), horiz=F, cex=1)
    dev.off()
  }
}
