library(ncdf4)
library(lubridate)
library(raster)
library(stringr)

cloudMeans <- function(years, vars=c("cer", "cot", "cwp"), dummy = "dummy.tif", outpath = "results/", inpath = "results/NC/"){
  # value 1 water cloud, value 2 ice cloud
  for (dir in vars){ dir.create(paste0(outpath,"/", str_to_upper(dir)), showWarnings = F)}
  
  dummy =raster(dummy) 
  dummy[] = NA
  
  for (year in years){
    print(year)
    cloud = ncdf4::nc_open(paste0(inpath,"cloud_",as.character(year),".nc"))
    dates = ncvar_get(cloud, attributes(cloud$dim)$names[1])
    dates = as.POSIXct(dates*3600, origin = "2007-01-01 00:00")
    months = month(dates)
    
    for (var in vars){
      data = ncvar_get(cloud, var)
      print(var)
      
      for (m in na.omit(unique(months))){
        
        print(as.character(month(m, label = T)))
        tmp = data[ , , months == m]
        tmp[tmp == 0] = NA
        
        total = apply(tmp, c(1,2), mean, na.rm =T)
        obsvT = apply(tmp, c(1,2), function(x) length(na.omit(x)))
        
        totalDummy = dummy
        obsvDummy = dummy
        
        totalDummy[] = as.vector(total)
        obsvDummy[] = as.vector(obsvT)
        
        totalDummy = aggregate(totalDummy, 10, mean)
        obsvDummy = aggregate(obsvDummy, 10, mean)
        
        
        writeRaster(totalDummy, filename = paste0(outpath, str_to_upper(var), "/",
                                                  year,"_", as.character(month(m, label = T)),
                                                  "_", var,"_mean.tif"), overwrite = TRUE)
        writeRaster(obsvDummy, filename = paste0(outpath, str_to_upper(var), "/",
                                                  year,"_", as.character(month(m, label = T)),
                                                  "_", var,"_obsv.tif"), overwrite = TRUE)
        
        rm(tmp, totalDummy, obsvDummy, obsvT, total)
        gc()
      }
    }
    nc_close(cloud)}
}


cloudMeans(years = c(2003:2019), vars=c("cer", "cot", "cth", "ctp","ctt","cwp","cwv"),  dummy = "../data/dummy.tif", outpath = "../results/", inpath = "../results/NC/")

