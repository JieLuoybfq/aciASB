scatterPlotMean <- function(parax = "AOD_550", paray = "P", groupv="RH", plot = TRUE, aggrLevel="season") {
  
  if (aggrLevel == "season") aggPath = "../results/season/"
  if(aggrLevel == "monthly") aggPath ="../results/"
  
  para1 = raster::stack(list.files(paste0(aggPath, parax, "/") , pattern = "*mean.tif$", full.names = T))
  if(parax == "AOD_550") para1[para1 > 1] = NA
  indNA1 = which(is.na(para1[]))
  
  para2 = raster::stack(list.files(paste0(aggPath, paray, "/"), pattern = "*mean.tif$", full.names = T))
  if(paray == "AOD_550") para2[para2 > 1] = NA
  indNA2 = which(is.na(para2[]))
  
  paraC = raster::stack(list.files(paste0(aggPath, groupv, "/") , pattern = "*mean.tif$", full.names = T))
  if(groupv == "AOD_550") paraC[paraC > 1] = NA
  indNAC = which(is.na(paraC[]))
  
  para1[indNA2] = NA
  para1[indNAC] = NA
  para2[indNA1] = NA
  para2[indNAC] = NA
  paraC[is.na(para1)] = NA
  paraC[is.na(para2)] = NA
  
  values1 = c(na.omit(values(para1)))
  values2 = c(na.omit(values(para2)))
  valuesC = c(na.omit(values(paraC)))
  
  groupC = as.factor(cut(valuesC, seq(0, 100, 2.5), right=F, labels=F))
  groupVec = seq(0, 100, 2.5)
  labels = paste(groupVec[as.numeric(levels(groupC))],"%",sep="")
  key = sort(unique(groupC))
  names(labels) = key
  
  valuesC = as.factor(groupC)
  data = data.frame(value1 = values1, value2 = values2, valueC = valuesC)
  seasVec = c(rep("S1", length(values1)/4), rep("S2", length(values1)/4),
              rep("S3", length(values1)/4), rep("S4", length(values1)/4))
  data$season = as.factor(seasVec)
  
  if (plot == TRUE) {
    p <- ggplot2::ggplot(data=data, aes(x=value1, y=value2))+
      ggplot2::geom_point(aes(color=season))+
      ggplot2::facet_wrap(~valueC, ncol=4, scales = "free", labeller=labeller(valueC=labels))+
      ggplot2::xlab(parax)+
      ggplot2::ylab(paray)+
      ggplot2::geom_smooth(method = lm, aes(x=value1, y=values2))+
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

x = scatterPlotMean(parax = "AOD_550", paray = "P", groupv = "RH", plot=TRUE, aggrLevel = "season")

