# Metriques_herbier



library(lubridate)
library(gridExtra)
library(readr)
library(raster)
library(ggplot2)




raster_path = paste(getwd(),"/Sorties/Images_classees/rasters", sep = "")

all_rasters=dir(raster_path, pattern="2", full.names = T)
raster_dates = dir(raster_path, pattern="2", full.names = F)
raster_dates = substring(raster_dates,1,8)


########## GET AREAS #############

inds = c(1:length(all_rasters))
surface = rep(NA,length(all_rasters))

for (i in inds) {
  r_path_in_file = all_rasters[i]
  r = raster(r_path_in_file)
  s = cellStats(r, stat = "sum")
  surface[i] = s*100
}



########### MAKE DF ############


date = as.Date(raster_dates,format = "%Y%m%d")

df = data.frame(date,surface)



######### SAVE DF ########

write_csv(df, paste(getwd(),"/Sorties/Surface_herbier/Surface_herbier.csv", sep = ""))


############ PLOTS ###########


p1 = ggplot(df)+
  ggtitle("Surface d'herbier detectée (hectares)") +
  xlab("Date") + ylab("Surface")+
  
  geom_point(aes(x=date, y=surface/10000, color = "herbier"), shape = 2)+
  geom_path(aes(x=date, y=surface/10000, color = "herbier"))
#geom_ribbon(aes(x=all_dates, y=all_values, ymax=all_values+(1581800-(1581800*0.99)), ymin=all_values-(1581800-(1581800*0.99))), 
#alpha=0.2)

#geom_errorbar(aes(x=dates,y = method1, ymin=method1-(15818-(15818*0.95)), ymax=method1+(15818-(15818*0.95))), width=5, color = "grey")



plot(p1)


