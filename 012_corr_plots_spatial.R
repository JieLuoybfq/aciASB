# spatial correlation plots
library(raster)
library(rasterVis)
library(gridExtra)

paras = c("AE", "SSA", "AOD_550","COT","CWP", "CER")
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

# subsetting NAs
parasRaster = lapply(paras, function(x){
  para = stack(list.files(paste0("../results/season/", x, "/") , pattern = "mean", full.names = T))
  return(para)
})

names(parasRaster) = paras


# AOD vs AE
seasons = c("s1","s2","s3","s4")
years = 2003:2018
parax = "AOD_550"
paray = "AE"
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
col_ramp = colorRampPalette(brewer.pal(11, "RdBu"))

corVals = stack(lapply(seasons, function(s) seasonal_cor[[s]]$correlation))
names(corVals) = seasons
pVals = lapply(seasons, function(s) seasonal_cor[[s]]$correlation)

pValsPoints = lapply(pVals, function(x){
  x[x>0.05] = NA
  y = rasterToPoints(x, spatial=TRUE)
  return(y)
})
names(pValsPoints) = seasons
breaks = seq(-1,1,.1)

corPlotS1= levelplot(corVals$s1, par.settings=mytheme, margin=FALSE, breaks=breaks, colorkey = T)+ 
  latticeExtra::layer(sp.points(pValsPoints$s1, col="black", lwd=0.4, cex=.3))

corPlotS2 = levelplot(corVals$s2, par.settings=mytheme, margin=FALSE,  breaks=breaks, colorkey = T)+ 
  latticeExtra::layer(sp.points(pValsPoints$s2, col="black", lwd=0.4, cex=.3))

corPlotS3 = levelplot(corVals$s3, par.settings=mytheme, margin=FALSE,  breaks=breaks, colorkey = T)+ 
  latticeExtra::layer(sp.points(pValsPoints$s3, col="black", lwd=0.4, cex=.3))

corPlotS4 = levelplot(corVals$s4, par.settings=mytheme, margin=FALSE, breaks=breaks, colorkey = T)+ 
  latticeExtra::layer(sp.points(pValsPoints$s4, col="black", lwd=0.4, cex=.3))

grids = grid.arrange(arrangeGrob(corPlotS1, corPlotS2, corPlotS3, corPlotS4),
                     arrangeGrob(draw.colorkey(key, draw=F)), nrow=1)


grids = c(a = corPlotS1, b= corPlotS2, c=corPlotS3, d=corPlotS4, layout = c(2,2), merge.legends=F)

png(file = paste0("../results/plots/cor_",parax,"_",paray,".png"), 
    width = 30, height = 30, units = "cm", res = 600)
print(grids)
dev.off()



# AOD vs. SSA
parax = "AOD_550"
paray = "SSA"
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
breaks = seq(-1,1,.1)

corPlotS1= levelplot(corVals$s1, par.settings=mytheme, margin=FALSE, breaks=breaks, colorkey = T)+ 
  latticeExtra::layer(sp.points(pValsPoints$s1, col="black", lwd=0.4, cex=.3))

corPlotS2 = levelplot(corVals$s2, par.settings=mytheme, margin=FALSE,  breaks=breaks, colorkey = T)+ 
  latticeExtra::layer(sp.points(pValsPoints$s2, col="black", lwd=0.4, cex=.3))

corPlotS3 = levelplot(corVals$s3, par.settings=mytheme, margin=FALSE,  breaks=breaks, colorkey = T)+ 
  latticeExtra::layer(sp.points(pValsPoints$s3, col="black", lwd=0.4, cex=.3))

corPlotS4 = levelplot(corVals$s4, par.settings=mytheme, margin=FALSE, breaks=breaks, colorkey = T)+ 
  latticeExtra::layer(sp.points(pValsPoints$s4, col="black", lwd=0.4, cex=.3))

grids = grid.arrange(arrangeGrob(corPlotS1, corPlotS2, corPlotS3, corPlotS4),
                     arrangeGrob(draw.colorkey(key, draw=F)), nrow=1)


grids = c(a = corPlotS1, b= corPlotS2, c=corPlotS3, d=corPlotS4, layout = c(2,2), merge.legends=F)

png(file = paste0("../results/plots/cor_",parax,"_",paray,".png"), 
    width = 30, height = 30, units = "cm", res = 600)
print(grids)
dev.off()



# AOD vs. CWP
parax = "AOD_550"
paray = "CWP"
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
breaks = seq(-1,1,.1)

corPlotS1= levelplot(corVals$s1, par.settings=mytheme, margin=FALSE, breaks=breaks, colorkey = T)+ 
  latticeExtra::layer(sp.points(pValsPoints$s1, col="black", lwd=0.4, cex=.3))

corPlotS2 = levelplot(corVals$s2, par.settings=mytheme, margin=FALSE,  breaks=breaks, colorkey = T)+ 
  latticeExtra::layer(sp.points(pValsPoints$s2, col="black", lwd=0.4, cex=.3))

corPlotS3 = levelplot(corVals$s3, par.settings=mytheme, margin=FALSE,  breaks=breaks, colorkey = T)+ 
  latticeExtra::layer(sp.points(pValsPoints$s3, col="black", lwd=0.4, cex=.3))

corPlotS4 = levelplot(corVals$s4, par.settings=mytheme, margin=FALSE, breaks=breaks, colorkey = T)+ 
  latticeExtra::layer(sp.points(pValsPoints$s4, col="black", lwd=0.4, cex=.3))

grids = grid.arrange(arrangeGrob(corPlotS1, corPlotS2, corPlotS3, corPlotS4),
                     arrangeGrob(draw.colorkey(key, draw=F)), nrow=1)


grids = c(a = corPlotS1, b= corPlotS2, c=corPlotS3, d=corPlotS4, layout = c(2,2), merge.legends=F)

png(file = paste0("../results/plots/cor_",parax,"_",paray,".png"), 
    width = 30, height = 30, units = "cm", res = 600)
print(grids)
dev.off()


# AOD vs. COT
parax = "AOD_550"
paray = "COT"
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

corVals = stack(lapply(seasons, function(s) seasonal_cor[[s]]$correlation))
names(corVals) = seasons
pVals = lapply(seasons, function(s) seasonal_cor[[s]]$correlation)

pValsPoints = lapply(pVals, function(x){
  x[x>0.05] = NA
  y = rasterToPoints(x, spatial=TRUE)
  return(y)
})
names(pValsPoints) = seasons
breaks = seq(-1,1,.1)

corPlotS1= levelplot(corVals$s1, par.settings=mytheme, margin=FALSE, breaks=breaks, colorkey = T)+ 
  latticeExtra::layer(sp.points(pValsPoints$s1, col="black", lwd=0.4, cex=.3))

corPlotS2 = levelplot(corVals$s2, par.settings=mytheme, margin=FALSE,  breaks=breaks, colorkey = T)+ 
  latticeExtra::layer(sp.points(pValsPoints$s2, col="black", lwd=0.4, cex=.3))

corPlotS3 = levelplot(corVals$s3, par.settings=mytheme, margin=FALSE,  breaks=breaks, colorkey = T)+ 
  latticeExtra::layer(sp.points(pValsPoints$s3, col="black", lwd=0.4, cex=.3))

corPlotS4 = levelplot(corVals$s4, par.settings=mytheme, margin=FALSE, breaks=breaks, colorkey = T)+ 
  latticeExtra::layer(sp.points(pValsPoints$s4, col="black", lwd=0.4, cex=.3))

grids = grid.arrange(arrangeGrob(corPlotS1, corPlotS2, corPlotS3, corPlotS4),
                     arrangeGrob(draw.colorkey(key, draw=F)), nrow=1)


grids = c(a = corPlotS1, b= corPlotS2, c=corPlotS3, d=corPlotS4, layout = c(2,2), merge.legends=F)

png(file = paste0("../results/plots/cor_",parax,"_",paray,".png"), 
    width = 30, height = 30, units = "cm", res = 600)
print(grids)
dev.off()



# AOD vs. CER
parax = "AOD_550"
paray = "CER"
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
breaks = seq(-1,1,.1)

corPlotS1= levelplot(corVals$s1, par.settings=mytheme, margin=FALSE, breaks=breaks, colorkey = T)+ 
  latticeExtra::layer(sp.points(pValsPoints$s1, col="black", lwd=0.4, cex=.3))

corPlotS2 = levelplot(corVals$s2, par.settings=mytheme, margin=FALSE,  breaks=breaks, colorkey = T)+ 
  latticeExtra::layer(sp.points(pValsPoints$s2, col="black", lwd=0.4, cex=.3))

corPlotS3 = levelplot(corVals$s3, par.settings=mytheme, margin=FALSE,  breaks=breaks, colorkey = T)+ 
  latticeExtra::layer(sp.points(pValsPoints$s3, col="black", lwd=0.4, cex=.3))

corPlotS4 = levelplot(corVals$s4, par.settings=mytheme, margin=FALSE, breaks=breaks, colorkey = T)+ 
  latticeExtra::layer(sp.points(pValsPoints$s4, col="black", lwd=0.4, cex=.3))

grids = grid.arrange(arrangeGrob(corPlotS1, corPlotS2, corPlotS3, corPlotS4),
                     arrangeGrob(draw.colorkey(key, draw=F)), nrow=1)


grids = c(a = corPlotS1, b= corPlotS2, c=corPlotS3, d=corPlotS4, layout = c(2,2), merge.legends=F)

png(file = paste0("../results/plots/cor_",parax,"_",paray,".png"), 
    width = 30, height = 30, units = "cm", res = 600)
print(grids)
dev.off()

