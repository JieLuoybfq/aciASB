scatterPlotMean <- function(parax = "AOD_550", paray = "P", groupv="RH", plot = TRUE, aggrLevel="season") {
  
  if (aggrLevel == "season") aggPath = "../results/season/"
  if(aggrLevel == "monthly") aggPath ="../results/"
  
  para1 = raster::stack(list.files(paste0(aggPath, parax, "/") , pattern = "*mean.tif$", full.names = T))
  para2 = raster::stack(list.files(paste0(aggPath, paray, "/"), pattern = "*mean.tif$", full.names = T))
  paraC = raster::stack(list.files(paste0(aggPath, groupv, "/") , pattern = "*mean.tif$", full.names = T))

  # get masking NA layer
  tmp = raster::stack(para1, para2, paraC)
  raster::beginCluster(parallel::detectCores()-1)
  maskNA = raster::clusterR(tmp, calc, args=list(sum, na.rm=FALSE))
  raster::endCluster()
  rm(tmp)
  gc()
  
  # apply masking layer
  para1 = raster::mask(para1, maskNA)
  para2 = raster::mask(para2, maskNA)
  paraC = raster::mask(paraC, maskNA)
  rm(maskNA)
  gc()
  
  # raster to vector, excluding NAs
  values1 = c(na.exclude(values(para1)))
  values2 = c(na.exclude(values(para2)))
  valuesC = c(na.exclude(values(paraC)))
  
  # stratify plots by group variable
  breaks = classInt::classIntervals(valuesC, 10, style = "fisher", intervalClosure = "left")$brks
  # summVals = summary(valuesC)
  # min_ = summVals[1]
  # qu3_ = summVals[5]  
  # steps = (qu3_ - min_) / 8

  groupC = as.factor(cut(valuesC, breaks, right=F, labels=F, include.lowest=T))
  groupVec = round(breaks,2)
  
  unit = "[dimensionless]"
  if (groupv == "RH") unit = "%"
  if (groupv == "CER") unit = "microns"
  if (groupv == "CWP") unit = "g/m²"
  labels = paste(groupVec,  unit,sep=" ")
  key = sort(unique(groupC))
  names(labels) = key
  
  valuesC = as.factor(groupC)
  data = data.frame(value1 = values1, value2 = values2, valueC = valuesC)
  seasVec = c(rep("S1", length(values1)/4), rep("S2", length(values1)/4),
              rep("S3", length(values1)/4), rep("S4", length(values1)/4))
  data$season = as.factor(seasVec)
  data = data[-which(is.na(data$valueC)),]

  if (plot == TRUE) {
    p <- ggplot2::ggplot(data=data, aes(x=value1, y=value2))+
      ggplot2::geom_point(shape=1)+
      ggplot2::facet_wrap(~valueC, ncol=4, scales = "free", labeller=labeller(valueC=labels))+
      ggplot2::xlab(parax)+
      ggplot2::ylab(paray)+
      ggplot2::geom_smooth(method = lm, aes(x=value1, y=value2))+
      ggplot2::theme_minimal()
    
      lmModels <- data %>% 
      dplyr::group_by(valueC) %>%
      dplyr::group_map(~ lm(value2 ~ value1, data=.))
    
    results = list(p, lmModels)
    
  } else {
    
    results <- data %>% 
      dplyr::group_by(valueC) %>%
      dplyr::group_map(~ lm(value2 ~ value1, data=.))
    
  }
  return(results)
}
library(magrittr)
library(raster)
library(ggplot2)
library(classInt)
library(dplyr)

aod_rh = scatterPlotMean(parax="AOD_550", paray="P", groupv="RH", plot=T, aggrLevel="season")
aod_rh[[1]]
aod_cer = scatterPlotMean(parax="AOD_550", paray="P", groupv="CER", plot=T, aggrLevel="season")
aod_cer[[1]]
aod_cwp = scatterPlotMean(parax="AOD_550", paray="P", groupv="CWP", plot=T, aggrLevel="season")
aod_cwp[[1]]
aod_cot = scatterPlotMean(parax="AOD_550", paray="P", groupv="COT", plot=T, aggrLevel="season")
aod_cot[[1]]
