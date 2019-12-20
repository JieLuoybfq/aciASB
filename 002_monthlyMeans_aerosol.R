library(ncdf4)
library(lubridate)
library(raster)
library(stringr)

cloudMeans <- function(years, vars=c("aod_550", "ae", "ssa"), dummy = "../data/dummy_10.tif", outpath = "../results/", inpath = "../results/NC/"){
  # value 1 water cloud, value 2 ice cloud
  for (dir in vars){ dir.create(paste0(outpath,"/", str_to_upper(dir)), showWarnings = F)}
  
  dummy =raster(dummy) 
  dummy[] = NA
  
  for (year in years){
    print(year)
    aerosol = ncdf4::nc_open(paste0(inpath,"aerosol_",as.character(year),".nc"))
    dates = ncvar_get(aerosol, attributes(aerosol$dim)$names[1])
    dates = as.POSIXct(dates*3600, origin = "2007-01-01 00:00")
    months = month(dates)
    
    for (var in vars){
      data = ncvar_get(aerosol, var)
      print(var)
      
      for (m in na.omit(unique(months))){
        
        print(as.character(month(m, label = T)))
        tmp = data[ , , months == m]
        tmp[tmp == 0] = NA
        
        tmpMean = apply(tmp, c(1,2), mean, na.rm=T)
        tmpObsv = apply(tmp, c(1,2), function(x) length(na.omit(x)))
        
        meanDummy = dummy
        obsvDummy = dummy
        
        meanDummy[] = as.vector(tmpMean)
        obsvDummy[] = as.vector(tmpObsv)

        
        writeRaster(meanDummy, filename = paste0(outpath, str_to_upper(var), "/",
                                                  year,"_", as.character(month(m, label = T)),
                                                  "_", var,"_mean.tif"), overwrite = TRUE)
        writeRaster(obsvDummy, filename = paste0(outpath, str_to_upper(var), "/",
                                                  year,"_", as.character(month(m, label = T)),
                                                  "_", var,"_obsv.tif"), overwrite = TRUE)

        rm(tmp, tmpMean, tmpObsv, meanDummy, obsvDummy)
        gc()
      }
    }
    nc_close(aerosol)}
}


cloudMeans(years = 2003:2019, vars=c("aod_550", "aod", "ae", "ssa","at"), dummy = "../data/dummy_10.tif", outpath = "../results/", inpath = "../results/NC/")

