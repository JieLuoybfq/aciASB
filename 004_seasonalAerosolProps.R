library(stringr)
library(raster)
library(ggplot2)
library(lubridate)
library(reshape2)

# select relevant parameters
paras = str_to_upper(c("aod_550", "ae", "ssa"))
for (dir in paras){ dir.create(paste0("../results/season/", str_to_upper(dir)), showWarnings = F)}

# read parameters observation number files into raster stack in a list
paraStacks = lapply(paras, function(x){
  stack(list.files(paste0("../results/",x), pattern = "mean", full.names = TRUE))
})

# vector indicating the years for which to calculate mean values
years = 2003:2018

# declarig months abbrevations
months = as.character(month(1:12, label = TRUE))
seasons = list(s1 = months[3:5], s2 = months[6:8], 
               s3 = months[9:11], s4 =months[c(12,1,2)])


# lapply loop through parameters and months to get seasonality for the whole time spawn
seasonalMeans = lapply(paraStacks, function(x){
  
  tmpLS = lapply(years, function(y){
    # target year
    target = y
    # next year
    nextY = y +1
    # get layer names of target year
    layer_names = names(x)[grep(as.character(target), names(x))]
    # exclude january of target year
    layer_names = layer_names[!str_detect(layer_names, c("_Jan_", "_Feb_"))]
    # get raster layers for target year
    tmpRas = x[[which(names(x) %in% layer_names)]]
    # get january layer for next year
    jan = x[[grep(paste0("X", as.character(nextY),"_Jan_"), names(x))]]
    feb = x[[grep(paste0("X", as.character(nextY),"_Feb_"), names(x))]]
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
  # rename years list
  names(tmpLS) = as.character(years)
  return(tmpLS)
})
# rename parameter list
names(seasonalMeans) = paras

# save results to disk
for (p in paras){
  for (y in 1:length(years)){
    writeRaster(seasonalMeans[[p]][[y]][["s1"]], 
                filename = paste0("../results/season/", str_to_upper(p),"/",
                                  years[y], "_s1_", p, "_mean.tif"), overwrite = TRUE)
    writeRaster(seasonalMeans[[p]][[y]][[2]], 
                filename = paste0("../results/season/", str_to_upper(p),"/",
                                  years[y], "_s2_", p, "_mean.tif"), overwrite = TRUE)
    writeRaster(seasonalMeans[[p]][[y]][[3]],
                filename = paste0("../results/season/", str_to_upper(p),"/",
                                  years[y], "_s3_", p, "_mean.tif"), overwrite = TRUE)
    writeRaster(seasonalMeans[[p]][[y]][[4]],
                filename = paste0("../results/season/", str_to_upper(p),"/",
                                  years[y], "_s4_", p, "_mean.tif"), overwrite = TRUE)
  }
}

# calculate yearly means for the seasons
meanRasters = lapply(seasonalMeans, function(p){
  # stack current parameter
  rasterStack = stack(p)
  # split up layers to seasons
  s1 = rasterStack[[seq(1, nlayers(rasterStack)-3, 4)]]
  s2 = rasterStack[[seq(2, nlayers(rasterStack)-2, 4)]]
  s3 = rasterStack[[seq(3, nlayers(rasterStack)-1, 4)]]
  s4 = rasterStack[[seq(4, nlayers(rasterStack), 4)]]
  # calculate total mean
  beginCluster(parallel::detectCores()-1)
  s1 = clusterR(s1, mean, args = list(na.rm=TRUE))
  s2 = clusterR(s2, mean, args = list(na.rm=TRUE))
  s3 = clusterR(s3, mean, args = list(na.rm=TRUE))
  s4 = clusterR(s4, mean, args = list(na.rm=TRUE))
  endCluster()
  # prepare output
  meanRas = stack(s1,s2,s3,s4)
  names(meanRas) = c("s1","s2","s3","s4")
  return(meanRas)
})
# rename elements in list
names(meanRasters) = paras

# write output to disk
for (p in paras){
  writeRaster(meanRasters[[p]][["s1"]], filename = paste0("../results/season/",p,"/",p,"_s1_total.tif"), overwrite = TRUE)
  writeRaster(meanRasters[[p]][["s2"]], filename = paste0("../results/season/",p,"/",p,"_s2_total.tif"), overwrite = TRUE)
  writeRaster(meanRasters[[p]][["s3"]], filename = paste0("../results/season/",p,"/",p,"_s3_total.tif"), overwrite = TRUE)
  writeRaster(meanRasters[[p]][["s4"]], filename = paste0("../results/season/",p,"/",p,"_s4_total.tif"), overwrite = TRUE)
}

