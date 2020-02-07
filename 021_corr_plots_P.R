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
    result = cor.test(para1, para2, method = "spearman")
    return(c(result$estimate, result$p.value))
  }else{
    return(c(NA,NA))
  }
}

getParCor = function(x){
  if (!(sum(is.na(x)) / 3) > length(years)/2){ #
    #50% of time series present a cor will be calculated
    x = as.vector(na.omit(x))
    index = length(x) / 3
    parax = x[1:index]
    paray = x[(index+1):(2*index)]
    parac = x[(2*index+1):length(x)]
    result = pcor.test(x=parax, y=paray, z=parax, method = "spearman")
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
    result = pcor.test(tmp[,1], tmp[,2], tmp[,-c(1,2)], method = "spearman")
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


# AOD vs P
seasons = c("s1","s2","s3","s4")
years = 2003:2018
parax = "AOD_550"
paray = "P"
print(paste0(parax, " vs. ", paray))

valsx = parasRaster[[parax]]
valsy = parasRaster[[paray]]
valsx[is.na(valsy)] = NA
valsy[is.na(valsx)] = NA

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
pVals = lapply(seasons, function(s) seasonal_cor[[s]]$correlation)

pValsPoints = lapply(pVals, function(x){
  x[x>0.05] = NA
  y = rasterToPoints(x, spatial=TRUE)
  return(y)
})
names(pValsPoints) = seasons

corPlotS1 = levelplot(corVals$s1, par.settings=mytheme, margin=FALSE, xlab.top = "season 1")+ 
  latticeExtra::layer(sp.points(pValsPoints$s1, col="black", lwd=0.4, cex=.3))

corPlotS2 = levelplot(corVals$s2, par.settings=mytheme, margin=FALSE, xlab.top = "season 2")+ 
  latticeExtra::layer(sp.points(pValsPoints$s2, col="black", lwd=0.4, cex=.3))

corPlotS3 = levelplot(corVals$s3, par.settings=mytheme, margin=FALSE, xlab.top = "season 3")+ 
  latticeExtra::layer(sp.points(pValsPoints$s3, col="black", lwd=0.4, cex=.3))

corPlotS4 = levelplot(corVals$s4, par.settings=mytheme, margin=FALSE, xlab.top = "season 4")+ 
  latticeExtra::layer(sp.points(pValsPoints$s4, col="black", lwd=0.4, cex=.3))

grids = c(a = corPlotS1, b= corPlotS2, c=corPlotS3, d=corPlotS4, layout = c(2,2), merge.legends=F)

png(file = paste0("../results/plots/cor_",parax,"_",paray,".png"), 
    width = 30, height = 30, units = "cm", res = 600)
print(grids)
dev.off()

totalCorr = do.call("rbind",lapply(1:length(seasons), function(s){
  tmpx = as.vector(extract(valsx, pValsPoints[[s]]))
  tmpy = as.vector(extract(valsy, pValsPoints[[s]]))
  mat = data.frame(x=tmpx, y=tmpy)
  mat = na.exclude(mat)
  result = cor.test(x=mat$x, y=mat$y, method = "spearman")
  return(data.frame(estimate=result$estimate, pvalue=result$p.value, season=seasons[s], contVar = "none"))
}))

# AOD vs RH
seasons = c("s1","s2","s3","s4")
years = 2003:2018
parax = "AOD_550"
paray = "RH"
print(paste0(parax, " vs. ", paray))

valsx = parasRaster[[parax]]
valsy = parasRaster[[paray]]
valsx[is.na(valsy)] = NA
valsy[is.na(valsx)] = NA

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
pVals = lapply(seasons, function(s) seasonal_cor[[s]]$correlation)

pValsPoints = lapply(pVals, function(x){
  x[x>0.05] = NA
  y = rasterToPoints(x, spatial=TRUE)
  return(y)
})
names(pValsPoints) = seasons

corPlotS1 = levelplot(corVals$s1, par.settings=mytheme, margin=FALSE, xlab.top = "season 1")+ 
  latticeExtra::layer(sp.points(pValsPoints$s1, col="black", lwd=0.4, cex=.3))

corPlotS2 = levelplot(corVals$s2, par.settings=mytheme, margin=FALSE, xlab.top = "season 2")+ 
  latticeExtra::layer(sp.points(pValsPoints$s2, col="black", lwd=0.4, cex=.3))

corPlotS3 = levelplot(corVals$s3, par.settings=mytheme, margin=FALSE, xlab.top = "season 3")+ 
  latticeExtra::layer(sp.points(pValsPoints$s3, col="black", lwd=0.4, cex=.3))

corPlotS4 = levelplot(corVals$s4, par.settings=mytheme, margin=FALSE, xlab.top = "season 4")+ 
  latticeExtra::layer(sp.points(pValsPoints$s4, col="black", lwd=0.4, cex=.3))

grids = c(a = corPlotS1, b= corPlotS2, c=corPlotS3, d=corPlotS4, layout = c(2,2), merge.legends=F)

png(file = paste0("../results/plots/cor_",parax,"_",paray,".png"), 
    width = 30, height = 30, units = "cm", res = 600)
print(grids)
dev.off()


# Partial Correlation between AOD and P, controlled for RH
seasons = c("s1","s2","s3","s4")
years = 2003:2018
parax = "AOD_550"
paray = "P"
parac = "RH"
print(paste0(parax, " vs. ", paray))


valsx = parasRaster[[parax]]
valsy = parasRaster[[paray]]
valsc = parasRaster[[parac]]
xindex = which(is.na(valsx[]))
yindex = which(is.na(valsy[]))
cindex = which(is.na(valsc[]))
valsx[c(yindex,cindex)] = NA
valsy[c(xindex,cindex)] = NA
valsc[c(xindex,yindex)] = NA

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
pVals = lapply(seasons, function(s) seasonal_cor[[s]]$correlation)

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

png(file = paste0("../results/plots/cor_",parax,"_",paray,"_by_",parac,".png"), 
    width = 30, height = 30, units = "cm", res = 600)
print(grids)
dev.off()

totalCorr = rbind(totalCorr, do.call("rbind",lapply(1:length(seasons), function(s){
  tmpx = as.vector(extract(valsx, pValsPoints[[s]]))
  tmpy = as.vector(extract(valsy, pValsPoints[[s]]))
  tmpc = as.vector(extract(valsc, pValsPoints[[s]]))
  mat = data.frame(x=tmpx, y=tmpy, z=tmpc)
  mat = na.exclude(mat)
  result = pcor.test(x=mat$x, y=mat$y, z=mat$z, method = "spearman")
  return(data.frame(estimate=result$estimate, pvalue=result$p.value, season=seasons[s], contVar = parac))
})))

# Partial Correlation between AOD and P, controlled for CWP
seasons = c("s1","s2","s3","s4")
years = 2003:2018
parax = "AOD_550"
paray = "P"
parac = "CWP"
print(paste0(parax, " vs. ", paray))


valsx = parasRaster[[parax]]
valsy = parasRaster[[paray]]
valsc = parasRaster[[parac]]
xindex = which(is.na(valsx[]))
yindex = which(is.na(valsy[]))
cindex = which(is.na(valsc[]))
valsx[c(yindex,cindex)] = NA
valsy[c(xindex,cindex)] = NA
valsc[c(xindex,yindex)] = NA

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
pVals = lapply(seasons, function(s) seasonal_cor[[s]]$correlation)

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

png(file = paste0("../results/plots/cor_",parax,"_",paray,"_by_",parac,".png"), 
    width = 30, height = 30, units = "cm", res = 600)
print(grids)
dev.off()

totalCorr = rbind(totalCorr, do.call("rbind",lapply(1:length(seasons), function(s){
  tmpx = as.vector(extract(valsx, pValsPoints[[s]]))
  tmpy = as.vector(extract(valsy, pValsPoints[[s]]))
  tmpc = as.vector(extract(valsc, pValsPoints[[s]]))
  mat = data.frame(x=tmpx, y=tmpy, z=tmpc)
  mat = na.exclude(mat)
  result = pcor.test(x=mat$x, y=mat$y, z=mat$z, method = "spearman")
  return(data.frame(estimate=result$estimate, pvalue=result$p.value, season=seasons[s], contVar = parac))
})))

# Partial Correlation between AOD and P, controlled for CER
seasons = c("s1","s2","s3","s4")
years = 2003:2018
parax = "AOD_550"
paray = "P"
parac = "CER"
print(paste0(parax, " vs. ", paray))


valsx = parasRaster[[parax]]
valsy = parasRaster[[paray]]
valsc = parasRaster[[parac]]
xindex = which(is.na(valsx[]))
yindex = which(is.na(valsy[]))
cindex = which(is.na(valsc[]))
valsx[c(yindex,cindex)] = NA
valsy[c(xindex,cindex)] = NA
valsc[c(xindex,yindex)] = NA

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
pVals = lapply(seasons, function(s) seasonal_cor[[s]]$correlation)

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

png(file = paste0("../results/plots/cor_",parax,"_",paray,"_by_",parac,".png"), 
    width = 30, height = 30, units = "cm", res = 600)
print(grids)
dev.off()

totalCorr = rbind(totalCorr, do.call("rbind",lapply(1:length(seasons), function(s){
  tmpx = as.vector(extract(valsx, pValsPoints[[s]]))
  tmpy = as.vector(extract(valsy, pValsPoints[[s]]))
  tmpc = as.vector(extract(valsc, pValsPoints[[s]]))
  mat = data.frame(x=tmpx, y=tmpy, z=tmpc)
  mat = na.exclude(mat)
  result = pcor.test(x=mat$x, y=mat$y, z=mat$z, method = "spearman")
  return(data.frame(estimate=result$estimate, pvalue=result$p.value, season=seasons[s], contVar = parac))
})))

# Partial Correlation between AOD and P, controlled for COT
seasons = c("s1","s2","s3","s4")
years = 2003:2018
parax = "AOD_550"
paray = "P"
parac = "COT"
print(paste0(parax, " vs. ", paray))

valsx = parasRaster[[parax]]
valsy = parasRaster[[paray]]
valsc = parasRaster[[parac]]
xindex = which(is.na(valsx[]))
yindex = which(is.na(valsy[]))
cindex = which(is.na(valsc[]))
valsx[c(yindex,cindex)] = NA
valsy[c(xindex,cindex)] = NA
valsc[c(xindex,yindex)] = NA

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
pVals = lapply(seasons, function(s) seasonal_cor[[s]]$correlation)

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

png(file = paste0("../results/plots/cor_",parax,"_",paray,"_by_",parac,".png"), 
    width = 30, height = 30, units = "cm", res = 600)
print(grids)
dev.off()

totalCorr = rbind(totalCorr, do.call("rbind",lapply(1:length(seasons), function(s){
  tmpx = as.vector(extract(valsx, pValsPoints[[s]]))
  tmpy = as.vector(extract(valsy, pValsPoints[[s]]))
  tmpc = as.vector(extract(valsc, pValsPoints[[s]]))
  mat = data.frame(x=tmpx, y=tmpy, z=tmpc)
  mat = na.exclude(mat)
  result = pcor.test(x=mat$x, y=mat$y, z=mat$z, method = "spearman")
  return(data.frame(estimate=result$estimate, pvalue=result$p.value, season=seasons[s], contVar = parac))
})))


# Partial Correlation between AOD and P, controlled for RH, CWP, CER, and COT
seasons = c("s1","s2","s3","s4")
years = 2003:2018
parax = "AOD_550"
paray = "P"
parac = c("RH", "CWP", "CER", "COT")


if(!file.exists("../results/parCor_vals.tif")){
  vals = stack(parasRaster[grep(paste(c(parax, paray, parac), collapse = "|"), names(parasRaster))])
  beginCluster(parallel::detectCores()-1)
  tmp = clusterR(vals, fun=calc, args = list(fun=sum))
  tmp[!is.na(tmp)] = 1
  vals = mask(vals, tmp)
  endCluster()
  writeRaster(vals, filename = "../results/parCor_vals.tif")
  saveRDS(names(vals), file ="../results/parCor_layernames.rds")
}else{
  vals = brick("../results/parCor_vals.tif")
  layernames = readRDS("../results/parCor_layernames.rds")
  names(vals) = layernames
}


seasonal_cor = lapply(seasons, function(s){
  tmpvals = vals[[grep(s, names(vals))]]
  tmpx = tmpvals[[grep(paste0("_",parax,"_"), names(tmpvals))]]
  tmpy = tmpvals[[grep(paste0("_",paray,"_"), names(tmpvals))]]
  tmpc1 = tmpvals[[grep(paste0("_",parac[1],"_"), names(tmpvals))]]
  tmpc2 = tmpvals[[grep(paste0("_",parac[2],"_"), names(tmpvals))]]
  tmpc3 = tmpvals[[grep(paste0("_",parac[3],"_"), names(tmpvals))]]
  tmpc4 = tmpvals[[grep(paste0("_",parac[4],"_"), names(tmpvals))]]
  tmpvals = stack(tmpx, tmpy, tmpc1, tmpc2, tmpc3, tmpc4)
  tmpResult = calc(tmpvals, fun = getParCorMulti)
  names(tmpResult) = c("correlation","pvalue")
  return(tmpResult)
})

names(seasonal_cor) = seasons

mytheme = RdBuTheme()
mytheme$panel.background$col = "grey"

corVals = stack(lapply(seasons, function(s) seasonal_cor[[s]]$correlation))
names(corVals) = seasons
pVals = lapply(seasons, function(s) seasonal_cor[[s]]$correlation)

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
  tmpx = tmpvals[[grep(paste0("_",parax,"_"), names(tmpvals))]]
  tmpx = as.vector(extract(tmpx, pValsPoints[[s]]))
  tmpy = tmpvals[[grep(paste0("_",paray,"_"), names(tmpvals))]]
  tmpy = as.vector(extract(tmpy, pValsPoints[[s]]))
  tmpc1 = tmpvals[[grep(paste0("_",parac[1],"_"), names(tmpvals))]]
  tmpc1 = as.vector(extract(tmpc1, pValsPoints[[s]]))
  tmpc2 = tmpvals[[grep(paste0("_",parac[2],"_"), names(tmpvals))]]
  tmpc2 = as.vector(extract(tmpc2, pValsPoints[[s]]))
  tmpc3 = tmpvals[[grep(paste0("_",parac[3],"_"), names(tmpvals))]]
  tmpc3 = as.vector(extract(tmpc3, pValsPoints[[s]]))
  tmpc4 = tmpvals[[grep(paste0("_",parac[4],"_"), names(tmpvals))]]
  tmpc4 = as.vector(extract(tmpc4, pValsPoints[[s]]))
  
  mat = data.frame(x=tmpx, y=tmpy, z1=tmpc1, z2=tmpc2, z3=tmpc3, z4=tmpc4)
  mat = na.exclude(mat)
  result = pcor.test(x=mat$x, y=mat$y, z=mat[,-c(1,2)], method = "spearman")
  return(data.frame(estimate=result$estimate, pvalue=result$p.value, season=seasons[s], contVar = "all"))
})))

rownames(totalCorr) = NULL
saveRDS(totalCorr, file = "../results/totalCorr.rds")
