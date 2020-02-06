scatterPlotMean <- function(parax = "AOD_550", paray = "P", groupv="RH", plot = TRUE, aggrLevel="season") {
  
  if (aggrLevel == "season") aggPath = "../results/season/"
  if(aggrLevel == "monthly") aggPath ="../results/"
  
  para1 = stack(list.files(paste0(aggPath, parax, "/") , pattern = "*mean.tif$", full.names = T))
  indNA1 = which(is.na(para1[]))
  
  para2 = stack(list.files(paste0(aggPath, paray, "/"), pattern = "*mean.tif$", full.names = T))
  indNA2 = which(is.na(para2[]))
  
  paraC = stack(list.files(paste0(aggPath, groupv, "/") , pattern = "*mean.tif$", full.names = T))
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
  
  groupC = as.factor(cut(valuesC, seq(0, 100, 5), right=F, labels=F))
  groupVec = seq(0, 100, 5)
  labels = paste(groupVec[as.numeric(levels(groupC))],"%",sep="")
  key = sort(unique(groupC))
  names(labels) = key
  
  valuesC = as.factor(groupC)
  data = data.frame(value1 = values1, value2 = values2, valueC = valuesC)
  seasVec = c(rep("S1", length(values1)/4), rep("S2", length(values1)/4),
              rep("S3", length(values1)/4), rep("S4", length(values1)/4))
  data$season = as.factor(seasVec)
  
  if (plot == TRUE) {
    p <- ggplot(data=data[data$value1 < 1,], aes(x=value1, y=value2))+
      geom_point(aes(color=season))+
      facet_wrap(~valueC, ncol=4, scales = "free", labeller=labeller(valueC=labels))+
      xlab(parax)+
      ylab(paray)+
      geom_smooth(method = lm, aes(x=value1, y=values2))+
      theme_minimal()
    
      lmModels <- data[data$value1 < 1,] %>% 
      group_by(valueC) %>%
      group_map(~ lm(value2 ~ value1, data=.))
    
    results = list(p, lmModels)
    
  } else {
    
    results <- data %>% 
      group_by(valueC) %>%
      group_map(~ lm(value2 ~ value1, data=.))
    
  }
  return(results)
}

