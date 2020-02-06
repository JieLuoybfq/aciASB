library(raster)
library(stringr)
library(lubridate)
library(ncdf4)

file = "../data/ERA/adaptor.mars.internal-1579094074.302342-2220-14-7f4b2a96-10eb-4d3c-851c-ea6dcad8bac8.nc"
out = "../results/RH/era_rh.tif"
master = raster("../results/CER/2003_Apr_cer_mean.tif")
years = 2003:2018
months = as.character(month(1:12, label=T))
combinations = expand.grid(year=years, month=months)

rh = nc_open(file)
dates = ncvar_get(rh, attributes(rh$dim)$names[4])
dates = as.POSIXct(dates*3600, origin = "1900-01-01 00:00:00.0")
pressure_vals = rh$dim$level$vals
nc_close(rh)


for (l in 1:length(pressure_vals)){
  print(paste0(l, " out of ", length(pressure_vals)))

  slave = brick(file, level=l)
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

  file_names = paste("../results/RH/levels/",years,"_",months,"_rh_",pressure_vals[l],".tif", sep="")

  writeRaster(slave, file_names, bylayer=T, overwrite = T)
}



for (r in 1:nrow(combinations)){
  print(paste0(r, " out of ", nrow(combinations)))
  ls = list.files("../results/RH/levels/", full.names = T)
  ls = ls[grep(combinations$year[r], ls)]
  ls = ls[grep(combinations$month[r], ls)]
  tmp = stack(ls)
  tmp = calc(tmp, median)
  writeRaster(tmp, filename=paste0("../results/RH/",combinations$year[r],"_",
                                   combinations$month[r],"_rh_median.tif"), overwrite = T)
}

seasons = list(s1 = months[3:5], s2 = months[6:8], 
               s3 = months[9:11], s4 =months[c(12,1,2)])
dir.create("../results/season/RH", showWarnings = F)
RHstack = stack(list.files(paste0("../results/RH"), pattern = "median", full.names = TRUE))



tmpLS = lapply(years, function(y){
  # target year
  target = y
  # next year
  nextY = y +1
  # get layer names of target year
  layer_names = names(RHstack)[grep(as.character(target), names(RHstack))]
  # exclude january of target year
  layer_names = layer_names[!str_detect(layer_names, c("_Jan_", "_Feb_"))]
  # get raster layers for target year
  tmpRas = RHstack[[which(names(RHstack) %in% layer_names)]]
  # get january layer for next year
  jan = RHstack[[grep(paste0("X", as.character(nextY),"_Jan_"), names(RHstack))]]
  feb = RHstack[[grep(paste0("X", as.character(nextY),"_Feb_"), names(RHstack))]]
  # add january to tmpRas
  tmpRas = addLayer(tmpRas, jan, feb)
  
  # get charachter abbrevations for months only from layer names
  layer_months = str_split(names(tmpRas), "_")
  layer_months = unlist(lapply(layer_months, function(x) x[[2]]))
  
  # calculate seasonal means per year
  beginCluster(parallel::detectCores()-1)
  s1 = clusterR(tmpRas[[which( layer_months %in% seasons$s1 == TRUE)]], mean, args = list(na.rm = TRUE))
  s2 = clusterR(tmpRas[[which( layer_months %in% seasons$s2 == TRUE)]], mean, args = list(na.rm = TRUE))
  s3 = clusterR(tmpRas[[which( layer_months %in% seasons$s3 == TRUE)]], mean, args = list(na.rm = TRUE))
  s4 = clusterR(tmpRas[[which( layer_months %in% seasons$s4 == TRUE)]], mean, args = list(na.rm = TRUE))
  endCluster()
  
  # stack results and rename
  seasonRas = stack(s1,s2,s3,s4)
  names(seasonRas) = c("s1","s2","s3","s4")
  return(seasonRas)
})

names(tmpLS) = as.character(years)


for (y in 1:length(years)){
  writeRaster(tmpLS[[y]][["s1"]], 
              filename = paste0("../results/season/RH/",
                                years[y], "_s1_RH_mean.tif"), overwrite = TRUE)
  writeRaster(tmpLS[[y]][[2]], 
              filename = paste0("../results/season/RH/",
                                years[y], "_s2_RH_mean.tif"), overwrite = TRUE)
  writeRaster(tmpLS[[y]][[3]],
              filename = paste0("../results/season/RH/",
                                years[y], "_s3_RH_mean.tif"), overwrite = TRUE)
  writeRaster(tmpLS[[y]][[4]],
              filename = paste0("../results/season/RH/",
                                years[y], "_s4_RH_mean.tif"), overwrite = TRUE)
}
