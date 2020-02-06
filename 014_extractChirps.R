library(raster)
library(stringr)
library(lubridate)

file = "../data/CHIRPS/chirps-v2.0.monthly.nc"
out = "../results/P/chirps_precipitation.tif"
master = raster("../results/CER/2003_Apr_cer_mean.tif")

slave = brick(file)
years = as.numeric(str_sub(names(slave),2,5))
Yindex = which(years >= 2002 & years <= 2019)

slave = slave[[Yindex]]
ext = extent(master)
ext[1] = 56
ext[2] = 68
ext[3] = 41
ext[4] = 48
slave = crop(slave, ext)
slave = resample(slave, master, method="bilinear")
dates = str_sub(names(slave), 2, 11)
dates = as.Date(dates,"%Y.%m.%d")
years = as.character(year(dates))
months = as.character(month(dates, label=T))

file_names = paste("../results/P/chirps_",months,"_",years,".tif", sep="")

writeRaster(slave, file_names, bylayer=T, overwrite = T)



