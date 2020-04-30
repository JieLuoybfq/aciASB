# spatial correlation plots
library(raster)
library(rasterVis)
library(gridExtra)
library(ppcor)

paras = c("AOD_550","P", "RH", "CWP", "CER", "COT")
# function to calculate correlation
getCor = function(x){
  if (!(sum(is.na(x)) / 2) > length(years)/2){ #50% of time series present a cor will be calculated
    para1 = x[1:length(years)]
    para2 = x[(length(years)+1):length(x)]
    result = cor.test(para1, para2, method = "pearson")
    return(c(result$estimate, result$p.value))
  }else{
    return(c(NA,NA))
  }
}

getParCor = function(x){
  if (!(sum(is.na(x)) / 2) > length(years)/2){ #
    #50% of time series present a cor will be calculated
    x = as.vector(na.omit(x))
    index = length(x) / 3
    parax = x[1:index]
    paray = x[(index+1):(2*index)]
    parac = x[(2*index+1):length(x)]
    result = pcor.test(x=parax, y=paray, z=parac, method = "pearson")
    return(c(result$estimate, result$p.value))
  }else{
    return(c(NA,NA))
  }
}

getParCorMulti = function(x){
  if (!(sum(is.na(x)) / 6) > length(years)/2){ #
    #50% of time series present a cor will be calculated
    x = as.vector(na.omit(x))
    index = length(x) / 6
    tmp = matrix(x, nrow = index)
    result = pcor.test(x = tmp[ ,1], y = tmp[ ,2], z = tmp[ ,-c(1,2)], method = "pearson")
    return(c(result$estimate, result$p.value))
  }else{
    return(c(NA,NA))
  }
}

# subsetting NAs
parasRaster = lapply(paras, function(x){
  para = stack(list.files(paste0("../results/season/", x, "/") , pattern = "mean", full.names = T))
  return(para)
})

names(parasRaster) = paras

# KS test per season and parameter
seasons = c("s1","s2","s3","s4")
years = 2003:2018

# since ties are present in the data, results are approximate which means
# no exact p values are calculated. However, when p-values are really small
# the function returns 0 which indicates that H0 of the two sided text is
# to be rejected
ks_test_results = lapply(1:length(paras), function(x){
  tmp = parasRaster[[x]]
  results = data.frame(para = paras[x], season = seasons, pvalue = rep(10, 4))
  for (s in 1:length(seasons)){
    test_results = ks.test(c(values(tmp[[grep(seasons[s], names(tmp))]])), y = "pnorm", alternative = "two.sided")
    results$pvalue[s] = test_results$p.value
  }
  return(results)
})

ks_results = do.call(rbind, ks_test_results)
ks_results
# we can assume normal distribution of all variables

# pearson correlation advises to exclude outliers. Here we define outliers as 
# pixels with AOD higher than 0.4
tmp = parasRaster$AOD_550
tmp[tmp > 0.3] = NA
parasRaster$AOD_550 = tmp

# AOD vs P
seasons = c("s1","s2","s3","s4")
years = 2003:2018
paraX = "AOD_550"
paraY = "P"
print(paste0(paraX, " vs. ", paraY))

valsx = parasRaster[[paraX]]
valsy = parasRaster[[paraY]]
index = is.na(valsx)
valsx[is.na(valsy)] = NA
valsy[index] = NA

seasonal_cor = lapply(seasons, function(s){
  tmpx = valsx[[grep(s, names(valsx))]]
  tmpy = valsy[[grep(s, names(valsy))]]
  tmpvals = stack(tmpx,tmpy)
  tmpResult = calc(tmpvals, fun = getCor)
  names(tmpResult) = c("correlation","pvalue")
  return(tmpResult)
})

names(seasonal_cor) = seasons

mytheme = RdBuTheme()
mytheme$panel.background$col = "grey"

corVals = stack(lapply(seasons, function(s) seasonal_cor[[s]]$correlation))
names(corVals) = seasons
pVals = lapply(seasons, function(s) seasonal_cor[[s]]$pvalue)

pValsPoints = lapply(pVals, function(x){
  x[x>0.05] = NA
  y = rasterToPoints(x, spatial=TRUE)
  return(y)
})
names(pValsPoints) = seasons

corPlotS1 = levelplot(corVals$s1, par.settings=mytheme, margin=FALSE)+ 
  latticeExtra::layer(sp.points(pValsPoints$s1, col="black", lwd=0.4, cex=.3))

corPlotS2 = levelplot(corVals$s2, par.settings=mytheme, margin=FALSE)+ 
  latticeExtra::layer(sp.points(pValsPoints$s2, col="black", lwd=0.4, cex=.3))

corPlotS3 = levelplot(corVals$s3, par.settings=mytheme, margin=FALSE)+ 
  latticeExtra::layer(sp.points(pValsPoints$s3, col="black", lwd=0.4, cex=.3))

corPlotS4 = levelplot(corVals$s4, par.settings=mytheme, margin=FALSE)+ 
  latticeExtra::layer(sp.points(pValsPoints$s4, col="black", lwd=0.4, cex=.3))

grids = c(a = corPlotS1, b= corPlotS2, c=corPlotS3, d=corPlotS4, layout = c(2,2), merge.legends=F)

png(file = paste0("../results/plots/cor_",paraX,"_",paraY,".png"), 
    width = 30, height = 30, units = "cm", res = 600)
print(grids)
dev.off()

totalCorr = do.call("rbind",lapply(1:length(seasons), function(s){
  tmpx = as.vector(extract(valsx[[grep(s, names(valsx))]], pValsPoints[[s]]))
  tmpy = as.vector(extract(valsy[[grep(s, names(valsy))]], pValsPoints[[s]]))
  # tmpx = c(valsx[grep(s, names(valsx))])
  # tmpy = c(valsy[grep(s, names(valsy))])
  mat = data.frame(x=tmpx, y=tmpy)
  mat = na.exclude(mat)
  result = cor.test(x=mat$x, y=mat$y, method = "pearson")
  return(data.frame(estimate=result$estimate, pvalue=result$p.value, season=seasons[s], contVar = "none"))
}))

# AOD vs RH
seasons = c("s1","s2","s3","s4")
years = 2003:2018
paraX = "AOD_550"
paraY = "RH"
print(paste0(paraX, " vs. ", paraY))

valsx = parasRaster[[paraX]]
valsy = parasRaster[[paraY]]
valsy[index] = NA

seasonal_cor = lapply(seasons, function(s){
  tmpx = valsx[[grep(s, names(valsx))]]
  tmpy = valsy[[grep(s, names(valsy))]]
  tmpvals = stack(tmpx,tmpy)
  tmpResult = calc(tmpvals, fun = getCor)
  names(tmpResult) = c("correlation","pvalue")
  return(tmpResult)
})

names(seasonal_cor) = seasons

mytheme = RdBuTheme()
mytheme$panel.background$col = "grey"

corVals = stack(lapply(seasons, function(s) seasonal_cor[[s]]$correlation))
names(corVals) = seasons
pVals = lapply(seasons, function(s) seasonal_cor[[s]]$pvalue)

pValsPoints = lapply(pVals, function(x){
  x[x>0.05] = NA
  y = rasterToPoints(x, spatial=TRUE)
  return(y)
})
names(pValsPoints) = seasons

corPlotS1 = levelplot(corVals$s1, par.settings=mytheme, margin=FALSE)+ 
  latticeExtra::layer(sp.points(pValsPoints$s1, col="black", lwd=0.4, cex=.3))

corPlotS2 = levelplot(corVals$s2, par.settings=mytheme, margin=FALSE)+ 
  latticeExtra::layer(sp.points(pValsPoints$s2, col="black", lwd=0.4, cex=.3))

corPlotS3 = levelplot(corVals$s3, par.settings=mytheme, margin=FALSE)+ 
  latticeExtra::layer(sp.points(pValsPoints$s3, col="black", lwd=0.4, cex=.3))

corPlotS4 = levelplot(corVals$s4, par.settings=mytheme, margin=FALSE)+ 
  latticeExtra::layer(sp.points(pValsPoints$s4, col="black", lwd=0.4, cex=.3))

grids = c(a = corPlotS1, b= corPlotS2, c=corPlotS3, d=corPlotS4, layout = c(2,2), merge.legends=F)

png(file = paste0("../results/plots/cor_",paraX,"_",paraY,".png"), 
    width = 30, height = 30, units = "cm", res = 600)
print(grids)
dev.off()


# Partial Correlation between AOD and P, controlled for RH
seasons = c("s1","s2","s3","s4")
years = 2003:2018
paraX = "AOD_550"
paraY = "P"
paraC = "RH"
print(paste0(paraX, " vs. ", paraC))


valsx = parasRaster[[paraX]]
valsy = parasRaster[[paraY]]
valsc = parasRaster[[paraC]]
valsy[index] = NA
valsc[index] = NA

seasonal_cor = lapply(seasons, function(s){
  tmpx = valsx[[grep(s, names(valsx))]]
  tmpy = valsy[[grep(s, names(valsy))]]
  tmpc = valsc[[grep(s, names(valsc))]]
  tmpvals = stack(tmpx,tmpy, tmpc)
  tmpResult = calc(tmpvals, fun = getParCor)
  names(tmpResult) = c("correlation","pvalue")
  return(tmpResult)
})

names(seasonal_cor) = seasons

mytheme = RdBuTheme()
mytheme$panel.background$col = "grey"

corVals = stack(lapply(seasons, function(s) seasonal_cor[[s]]$correlation))
names(corVals) = seasons
pVals = lapply(seasons, function(s) seasonal_cor[[s]]$pvalue)

pValsPoints = lapply(pVals, function(x){
  x[x>0.05] = NA
  y = rasterToPoints(x, spatial=TRUE)
  return(y)
})
names(pValsPoints) = seasons

corPlotS1 = levelplot(corVals$s1, par.settings=mytheme, margin=FALSE)+ 
  latticeExtra::layer(sp.points(pValsPoints$s1, col="black", lwd=0.4, cex=.3))

corPlotS2 = levelplot(corVals$s2, par.settings=mytheme, margin=FALSE)+ 
  latticeExtra::layer(sp.points(pValsPoints$s2, col="black", lwd=0.4, cex=.3))

corPlotS3 = levelplot(corVals$s3, par.settings=mytheme, margin=FALSE)+ 
  latticeExtra::layer(sp.points(pValsPoints$s3, col="black", lwd=0.4, cex=.3))

corPlotS4 = levelplot(corVals$s4, par.settings=mytheme, margin=FALSE)+ 
  latticeExtra::layer(sp.points(pValsPoints$s4, col="black", lwd=0.4, cex=.3))

grids = c(a = corPlotS1, b= corPlotS2, c=corPlotS3, d=corPlotS4, layout = c(2,2), merge.legends=F)

png(file = paste0("../results/plots/cor_",paraX,"_",paraY,"_by_",paraC,".png"), 
    width = 30, height = 30, units = "cm", res = 600)
print(grids)
dev.off()

totalCorr = rbind(totalCorr, do.call("rbind",lapply(1:length(seasons), function(s){
  tmpx = as.vector(extract(valsx[[grep(s, names(valsx))]], pValsPoints[[s]]))
  tmpy = as.vector(extract(valsy[[grep(s, names(valsy))]], pValsPoints[[s]]))
  tmpc = as.vector(extract(valsc[[grep(s, names(valsc))]], pValsPoints[[s]]))
  # tmpx = c(valsx[grep(s, names(valsx))])
  # tmpy = c(valsy[grep(s, names(valsy))])
  # tmpc = c(valsc[grep(s, names(valsc))])
  mat = data.frame(x=tmpx, y=tmpy, z=tmpc)
  mat = na.exclude(mat)
  result = pcor.test(x=mat$x, y=mat$y, z=mat[,-c(1,2)], method = "pearson")
  return(data.frame(estimate=result$estimate, pvalue=result$p.value, season=seasons[s], contVar = paraC))
})))

# Partial Correlation between AOD and P, controlled for CWP
seasons = c("s1","s2","s3","s4")
years = 2003:2018
paraX = "AOD_550"
paraY = "P"
paraC = "CWP"
print(paste0(paraX, " vs. ", paraY))

valsx = parasRaster[[paraX]]
valsy = parasRaster[[paraY]]
valsc = parasRaster[[paraC]]
valsy[index] = NA
valsc[index] = NA

seasonal_cor = lapply(seasons, function(s){
  tmpx = valsx[[grep(s, names(valsx))]]
  tmpy = valsy[[grep(s, names(valsy))]]
  tmpc = valsc[[grep(s, names(valsc))]]
  tmpvals = stack(tmpx,tmpy, tmpc)
  tmpResult = calc(tmpvals, fun = getParCor)
  names(tmpResult) = c("correlation","pvalue")
  return(tmpResult)
})

names(seasonal_cor) = seasons

mytheme = RdBuTheme()
mytheme$panel.background$col = "grey"

corVals = stack(lapply(seasons, function(s) seasonal_cor[[s]]$correlation))
names(corVals) = seasons
pVals = lapply(seasons, function(s) seasonal_cor[[s]]$pvalue)

pValsPoints = lapply(pVals, function(x){
  x[x>0.05] = NA
  y = rasterToPoints(x, spatial=TRUE)
  return(y)
})
names(pValsPoints) = seasons

corPlotS1 = levelplot(corVals$s1, par.settings=mytheme, margin=FALSE)+ 
  latticeExtra::layer(sp.points(pValsPoints$s1, col="black", lwd=0.4, cex=.3))

corPlotS2 = levelplot(corVals$s2, par.settings=mytheme, margin=FALSE)+ 
  latticeExtra::layer(sp.points(pValsPoints$s2, col="black", lwd=0.4, cex=.3))

corPlotS3 = levelplot(corVals$s3, par.settings=mytheme, margin=FALSE)+ 
  latticeExtra::layer(sp.points(pValsPoints$s3, col="black", lwd=0.4, cex=.3))

corPlotS4 = levelplot(corVals$s4, par.settings=mytheme, margin=FALSE)+ 
  latticeExtra::layer(sp.points(pValsPoints$s4, col="black", lwd=0.4, cex=.3))

grids = c(a = corPlotS1, b= corPlotS2, c=corPlotS3, d=corPlotS4, layout = c(2,2), merge.legends=F)

png(file = paste0("../results/plots/cor_",paraX,"_",paraY,"_by_",paraC,".png"), 
    width = 30, height = 30, units = "cm", res = 600)
print(grids)
dev.off()

totalCorr = rbind(totalCorr, do.call("rbind",lapply(1:length(seasons), function(s){
  tmpx = as.vector(extract(valsx[[grep(s, names(valsx))]], pValsPoints[[s]]))
  tmpy = as.vector(extract(valsy[[grep(s, names(valsy))]], pValsPoints[[s]]))
  tmpc = as.vector(extract(valsc[[grep(s, names(valsc))]], pValsPoints[[s]]))
  # tmpx = c(valsx[grep(s, names(valsx))])
  # tmpy = c(valsy[grep(s, names(valsy))])
  # tmpc = c(valsc[grep(s, names(valsc))])
  mat = data.frame(x=tmpx, y=tmpy, z=tmpc)
  mat = na.exclude(mat)
  result = pcor.test(x=mat$x, y=mat$y, z=mat[,-c(1,2)], method = "pearson")
  return(data.frame(estimate=result$estimate, pvalue=result$p.value, season=seasons[s], contVar = paraC))
})))

# Partial Correlation between AOD and P, controlled for CER
seasons = c("s1","s2","s3","s4")
years = 2003:2018
paraX = "AOD_550"
paraY = "P"
paraC = "CER"
print(paste0(paraX, " vs. ", paraY))

valsx = parasRaster[[paraX]]
valsy = parasRaster[[paraY]]
valsc = parasRaster[[paraC]]
valsy[index] = NA
valsc[index] = NA

seasonal_cor = lapply(seasons, function(s){
  tmpx = valsx[[grep(s, names(valsx))]]
  tmpy = valsy[[grep(s, names(valsy))]]
  tmpc = valsc[[grep(s, names(valsc))]]
  tmpvals = stack(tmpx,tmpy, tmpc)
  tmpResult = calc(tmpvals, fun = getParCor)
  names(tmpResult) = c("correlation","pvalue")
  return(tmpResult)
})

names(seasonal_cor) = seasons

mytheme = RdBuTheme()
mytheme$panel.background$col = "grey"

corVals = stack(lapply(seasons, function(s) seasonal_cor[[s]]$correlation))
names(corVals) = seasons
pVals = lapply(seasons, function(s) seasonal_cor[[s]]$pvalue)

pValsPoints = lapply(pVals, function(x){
  x[x>0.05] = NA
  y = rasterToPoints(x, spatial=TRUE)
  return(y)
})
names(pValsPoints) = seasons

corPlotS1 = levelplot(corVals$s1, par.settings=mytheme, margin=FALSE)+ 
  latticeExtra::layer(sp.points(pValsPoints$s1, col="black", lwd=0.4, cex=.3))

corPlotS2 = levelplot(corVals$s2, par.settings=mytheme, margin=FALSE)+ 
  latticeExtra::layer(sp.points(pValsPoints$s2, col="black", lwd=0.4, cex=.3))

corPlotS3 = levelplot(corVals$s3, par.settings=mytheme, margin=FALSE)+ 
  latticeExtra::layer(sp.points(pValsPoints$s3, col="black", lwd=0.4, cex=.3))

corPlotS4 = levelplot(corVals$s4, par.settings=mytheme, margin=FALSE)+ 
  latticeExtra::layer(sp.points(pValsPoints$s4, col="black", lwd=0.4, cex=.3))

grids = c(a = corPlotS1, b= corPlotS2, c=corPlotS3, d=corPlotS4, layout = c(2,2), merge.legends=F)

png(file = paste0("../results/plots/cor_",paraX,"_",paraY,"_by_",paraC,".png"), 
    width = 30, height = 30, units = "cm", res = 600)
print(grids)
dev.off()

totalCorr = rbind(totalCorr, do.call("rbind",lapply(1:length(seasons), function(s){
  tmpx = as.vector(extract(valsx[[grep(s, names(valsx))]], pValsPoints[[s]]))
  tmpy = as.vector(extract(valsy[[grep(s, names(valsy))]], pValsPoints[[s]]))
  tmpc = as.vector(extract(valsc[[grep(s, names(valsc))]], pValsPoints[[s]]))
  # tmpx = c(valsx[grep(s, names(valsx))])
  # tmpy = c(valsy[grep(s, names(valsy))])
  # tmpc = c(valsc[grep(s, names(valsc))])
  mat = data.frame(x=tmpx, y=tmpy, z=tmpc)
  mat = na.exclude(mat)
  result = pcor.test(x=mat$x, y=mat$y, z=mat[,-c(1,2)], method = "pearson")
  return(data.frame(estimate=result$estimate, pvalue=result$p.value, season=seasons[s], contVar = paraC))
})))

# Partial Correlation between AOD and P, controlled for COT
seasons = c("s1","s2","s3","s4")
years = 2003:2018
paraX = "AOD_550"
paraY = "P"
paraC = "COT"
print(paste0(paraX, " vs. ", paraY))

valsx = parasRaster[[paraX]]
valsy = parasRaster[[paraY]]
valsc = parasRaster[[paraC]]
valsy[index] = NA
valsc[index] = NA

seasonal_cor = lapply(seasons, function(s){
  tmpx = valsx[[grep(s, names(valsx))]]
  tmpy = valsy[[grep(s, names(valsy))]]
  tmpc = valsc[[grep(s, names(valsc))]]
  tmpvals = stack(tmpx,tmpy, tmpc)
  tmpResult = calc(tmpvals, fun = getParCor)
  names(tmpResult) = c("correlation","pvalue")
  return(tmpResult)
})

names(seasonal_cor) = seasons

mytheme = RdBuTheme()
mytheme$panel.background$col = "grey"

corVals = stack(lapply(seasons, function(s) seasonal_cor[[s]]$correlation))
names(corVals) = seasons
pVals = lapply(seasons, function(s) seasonal_cor[[s]]$pvalue)

pValsPoints = lapply(pVals, function(x){
  x[x>0.05] = NA
  y = rasterToPoints(x, spatial=TRUE)
  return(y)
})
names(pValsPoints) = seasons

corPlotS1 = levelplot(corVals$s1, par.settings=mytheme, margin=FALSE)+ 
  latticeExtra::layer(sp.points(pValsPoints$s1, col="black", lwd=0.4, cex=.3))

corPlotS2 = levelplot(corVals$s2, par.settings=mytheme, margin=FALSE)+ 
  latticeExtra::layer(sp.points(pValsPoints$s2, col="black", lwd=0.4, cex=.3))

corPlotS3 = levelplot(corVals$s3, par.settings=mytheme, margin=FALSE)+ 
  latticeExtra::layer(sp.points(pValsPoints$s3, col="black", lwd=0.4, cex=.3))

corPlotS4 = levelplot(corVals$s4, par.settings=mytheme, margin=FALSE)+ 
  latticeExtra::layer(sp.points(pValsPoints$s4, col="black", lwd=0.4, cex=.3))

grids = c(a = corPlotS1, b= corPlotS2, c=corPlotS3, d=corPlotS4, layout = c(2,2), merge.legends=F)

png(file = paste0("../results/plots/cor_",paraX,"_",paraY,"_by_",paraC,".png"), 
    width = 30, height = 30, units = "cm", res = 600)
print(grids)
dev.off()

totalCorr = rbind(totalCorr, do.call("rbind",lapply(1:length(seasons), function(s){
  tmpx = as.vector(extract(valsx[[grep(s, names(valsx))]], pValsPoints[[s]]))
  tmpy = as.vector(extract(valsy[[grep(s, names(valsy))]], pValsPoints[[s]]))
  tmpc = as.vector(extract(valsc[[grep(s, names(valsc))]], pValsPoints[[s]]))
  # tmpx = c(valsx[grep(s, names(valsx))])
  # tmpy = c(valsy[grep(s, names(valsy))])
  # tmpc = c(valsc[grep(s, names(valsc))])
  mat = data.frame(x=tmpx, y=tmpy, z=tmpc)
  mat = na.exclude(mat)
  result = pcor.test(x=mat$x, y=mat$y, z=mat[,-c(1,2)], method = "pearson")
  return(data.frame(estimate=result$estimate, pvalue=result$p.value, season=seasons[s], contVar = paraC))
})))


# Partial Correlation between AOD and P, controlled for RH, CWP, CER, and COT
seasons = c("s1","s2","s3","s4")
years = 2003:2018
paraX = "AOD_550"
paraY = "P"
paraC = names(parasRaster)[-c(1,2)]

valsx = parasRaster[[paraX]]
valsy = parasRaster[[paraY]]
list_index = which(names(parasRaster) %in% paraC)
valsc = lapply(list_index, function(x) return(parasRaster[[x]]))
valsy[index] = NA
# setting to NA for control parameters
result = raster()
for ( i in 1:length(paraC)){
  tmp = valsc[[i]]
  tmp[index] = NA
  result = addLayer(result, tmp)
}
vals = stack(valsx, valsy, result)

seasonal_cor = lapply(seasons, function(s){
  tmpvals = vals[[grep(s, names(vals))]]
  tmpx = tmpvals[[grep(paste0("_",paraX,"_"), names(tmpvals))]]
  tmpy = tmpvals[[grep(paste0("_",paraY,"_"), names(tmpvals))]]
  tmpc1 = tmpvals[[grep(paste0("_",paraC[1],"_"), names(tmpvals))]]
  tmpc2 = tmpvals[[grep(paste0("_",paraC[2],"_"), names(tmpvals))]]
  tmpc3 = tmpvals[[grep(paste0("_",paraC[3],"_"), names(tmpvals))]]
  tmpc4 = tmpvals[[grep(paste0("_",paraC[4],"_"), names(tmpvals))]]
  tmpvals = stack(tmpx, tmpy, tmpc1, tmpc2, tmpc3, tmpc4)
  tmpResult = raster::calc(tmpvals, fun = getParCorMulti)
  names(tmpResult) = c("correlation","pvalue")
  return(tmpResult)
})

# seasonal_cor = lapply(seasons, function(s){
#   tmpvals = vals[[grep(s, names(vals))]]
#   tmpdf = as.data.frame(tmpvals)
#   tmpdf = na.exclude(tmpdf)
#   tmpdf2 = do.call("cbind", lapply(paras, function(i){
#     tmp = data.frame(x = unlist(tmpdf[ ,stringr::str_detect(names(tmpdf), i)]))
#     names(tmp) = i
#     rownames(tmp) = NULL
#     return(tmp)
#   }))
# })
#   n <- dim(tmpdf2)[1]
#   gp <- dim(tmpdf2)[2]-2
#   covMat = cov(tmpdf2)
#   icvx <- solve(covMat)
#   pcor = -cov2cor(icvx)
#   diag(pcor) <- 1
#   statistic <- pcor*sqrt((n-2-gp)/(1-pcor^2))
#   p.value <- 2*pt(-abs(statistic),(n-2-gp))
#   diag(statistic) <- 0
#   diag(p.value) <- 0
#   
#   list(estimate=pcor[1,2],p.value=p.value[1,2],statistic=statistic[1,2],n=n,gp=gp)

names(seasonal_cor) = seasons

mytheme = RdBuTheme()
mytheme$panel.background$col = "grey"

corVals = stack(lapply(seasons, function(s) seasonal_cor[[s]]$correlation))
names(corVals) = seasons
pVals = lapply(seasons, function(s) seasonal_cor[[s]]$pvalue)

pValsPoints = lapply(pVals, function(x){
  x[x>0.05] = NA
  y = rasterToPoints(x, spatial=TRUE)
  return(y)
})
names(pValsPoints) = seasons

corPlotS1 = levelplot(corVals$s1, par.settings=mytheme, margin=FALSE)+ 
  latticeExtra::layer(sp.points(pValsPoints$s1, col="black", lwd=0.4, cex=.3))

corPlotS2 = levelplot(corVals$s2, par.settings=mytheme, margin=FALSE)+ 
  latticeExtra::layer(sp.points(pValsPoints$s2, col="black", lwd=0.4, cex=.3))

corPlotS3 = levelplot(corVals$s3, par.settings=mytheme, margin=FALSE)+ 
  latticeExtra::layer(sp.points(pValsPoints$s3, col="black", lwd=0.4, cex=.3))

corPlotS4 = levelplot(corVals$s4, par.settings=mytheme, margin=FALSE)+ 
  latticeExtra::layer(sp.points(pValsPoints$s4, col="black", lwd=0.4, cex=.3))

grids = c(a = corPlotS1, b= corPlotS2, c=corPlotS3, d=corPlotS4, layout = c(2,2), merge.legends=F)

png(file = paste0("../results/plots/cor_AOD_550_P_by_RH_CWP_CER_COT.png"), 
    width = 30, height = 30, units = "cm", res = 600)
print(grids)
dev.off()


totalCorr = rbind(totalCorr, do.call("rbind",lapply(1:length(seasons), function(s){
  tmpvals = vals[[grep(s, names(vals))]]
  tmpx = tmpvals[[grep(paste0("_",paraX,"_"), names(tmpvals))]]
  tmpx = as.vector(extract(tmpx, pValsPoints[[s]]))
  tmpy = tmpvals[[grep(paste0("_",paraY,"_"), names(tmpvals))]]
  tmpy = as.vector(extract(tmpy, pValsPoints[[s]]))
  tmpc1 = tmpvals[[grep(paste0("_",paraC[1],"_"), names(tmpvals))]]
  tmpc1 = as.vector(extract(tmpc1, pValsPoints[[s]]))
  tmpc2 = tmpvals[[grep(paste0("_",paraC[2],"_"), names(tmpvals))]]
  tmpc2 = as.vector(extract(tmpc2, pValsPoints[[s]]))
  tmpc3 = tmpvals[[grep(paste0("_",paraC[3],"_"), names(tmpvals))]]
  tmpc3 = as.vector(extract(tmpc3, pValsPoints[[s]]))
  tmpc4 = tmpvals[[grep(paste0("_",paraC[4],"_"), names(tmpvals))]]
  tmpc4 = as.vector(extract(tmpc4, pValsPoints[[s]]))
  
  mat = data.frame(x=tmpx, y=tmpy, z1=tmpc1, z2=tmpc2, z3=tmpc3, z4=tmpc4)
  mat = na.exclude(mat)
  result = pcor.test(x=mat$x, y=mat$y, z=mat[,-c(1,2)], method = "pearson")
  return(data.frame(estimate=result$estimate, pvalue=result$p.value, season=seasons[s], contVar = "all"))
})))

rownames(totalCorr) = NULL
saveRDS(totalCorr, file = "../results/totalCorr.rds")
