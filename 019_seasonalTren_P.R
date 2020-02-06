library(stringr)
library(raster)
library(ggplot2)
library(lubridate)
library(reshape2)
library(rasterVis)
library(gridExtra)

# select relevant parameters
paras = str_to_upper(c("p"))

# read parameters observation number files into raster stack in a list
paraStacks = lapply(paras, function(x){
  stack(list.files(paste0("../results/season/", x), pattern = "mean", full.names = TRUE))
})

seasons = c("s1","s2","s3","s4")

# function to calculate mann-kendall significance of trends - used in trends_raster.R

funTrendsSig <- function(y){
  y <- as.numeric(y)
  idNA <- which(is.na(y)!=T)
  
  if (length(idNA) <  0.8*length(y) ){return(NA) # need at least three elements to calculate mk.test
  } else {
    
    # Mann-Kendall Trend Test for assessing significance
    a <- y[idNA]
    mkmodel <- trend::mk.test(x=a)
    suppressWarnings(mkmodel$p.value)
  }
}

# function to calculate trend slopes - used in trends_raster.R
funTrendsDir <- function(y){
  y <- as.numeric(y)
  idNA <- which(is.na(y)!=T)
  if (length(idNA) < 0.8*length(y)) {return(NA)
  } else {
    x <- as.numeric(1:length(y))
    
    # simple linear regression using ordinary least squares
    lmodel <- lm(y ~ x, na.action=na.omit)
    
    # for lm or mblm
    dummyStats <- summary(lmodel)
    suppressWarnings(as.numeric(lmodel$coefficients[2]))# slope
  }
}

if(length(list.files("../results/trends/", pattern="cloud")) == 0){
  # apply the parallel summation of observation numbers for the whole time period
  trendRasters = lapply(paraStacks, function(x){
    print(x)
    tmpSeasons = lapply(seasons, function(y){
      x[[grep(as.character(y), names(x))]]
    })
    
    seasonTrends = lapply(tmpSeasons, function(x){
      beginCluster(parallel::detectCores()-1)
      slope = clusterR(x, calc, args = list(fun = funTrendsDir))
      pvalue =  clusterR(x, calc, args = list(fun = funTrendsSig))
      endCluster()
      rasterStack = stack(slope,pvalue)
      names(rasterStack) = c("slope","pvalue")
      return(rasterStack)
    })
    
    return(seasonTrends)
  })
  
  names(trendRasters) = paras
  
  # save output to disk
  for (p in paras){
    writeRaster(trendRasters[[p]][[1]][["slope"]], filename = paste0("../results/trends/cloud_s1_slope_",p,".tif"), overwrite = TRUE)
    writeRaster(trendRasters[[p]][[1]][["pvalue"]], filename = paste0("../results/trends/cloud_s1_pvalue_",p,".tif"), overwrite = TRUE)
    
    writeRaster(trendRasters[[p]][[2]][["slope"]], filename = paste0("../results/trends/cloud_s2_slope_",p,".tif"), overwrite = TRUE)
    writeRaster(trendRasters[[p]][[2]][["pvalue"]], filename = paste0("../results/trends/cloud_s2_pvalue_",p,".tif"), overwrite = TRUE)
    
    writeRaster(trendRasters[[p]][[3]][["slope"]], filename = paste0("../results/trends/cloud_s3_slope_",p,".tif"), overwrite = TRUE)
    writeRaster(trendRasters[[p]][[3]][["pvalue"]], filename = paste0("../results/trends/cloud_s3_pvalue_",p,".tif"), overwrite = TRUE)
    
    writeRaster(trendRasters[[p]][[4]][["slope"]], filename = paste0("../results/trends/cloud_s4_slope_",p,".tif"), overwrite = TRUE)
    writeRaster(trendRasters[[p]][[4]][["pvalue"]], filename = paste0("../results/trends/cloud_s4_pvalue_",p,".tif"), overwrite = TRUE)
  }
}

slopes = stack(list.files("../results/trends/", pattern = "cloud_s._slope", full.names = TRUE))
pvalues = stack(list.files("../results/trends/", pattern = "cloud_s._pvalue", full.names = TRUE))

for (p in paras){
  
  mytheme = RdBuTheme()
  mytheme$panel.background$col = "grey"
  
  tmpSlope = slopes[[grep(paste0(p,"$"), names(slopes))]] 
  tmpPvalue = pvalues[[grep(paste0(p,"$"), names(pvalues))]]
  names(tmpSlope) = c("s1","s2","s3","s4")
  names(tmpPvalue) = c("s1","s2","s3","s4")
  
  pValsPoints = lapply(as.list(tmpPvalue), function(x){
    x[x>0.05] = NA
    y = rasterToPoints(x, spatial=TRUE)
    return(y)
  })
  names(pValsPoints) = seasons
  
  trendPlotS1 = levelplot(tmpSlope$s1, par.settings=mytheme, margin=FALSE, xlab.top = "season 1")+ 
    latticeExtra::layer(sp.points(pValsPoints$s1, col="black", lwd=0.4, cex=.3))
  
  trendPlotS2 = levelplot(tmpSlope$s2, par.settings=mytheme, margin=FALSE, xlab.top = "season 2")+ 
    latticeExtra::layer(sp.points(pValsPoints$s2, col="black", lwd=0.4, cex=.3))
  
  trendPlotS3 = levelplot(tmpSlope$s3, par.settings=mytheme, margin=FALSE, xlab.top = "season 3")+ 
    latticeExtra::layer(sp.points(pValsPoints$s3, col="black", lwd=0.4, cex=.3))
  
  trendPlotS4 = levelplot(tmpSlope$s4, par.settings=mytheme, margin=FALSE, xlab.top = "season 4")+ 
    latticeExtra::layer(sp.points(pValsPoints$s4, col="black", lwd=0.4, cex=.3))
  
  
  png(file = paste0("../results/plots/trend_seasonal_",p,".png"), 
      width = 30, height = 30, units = "cm", res = 600)
  print(grid.arrange(trendPlotS1, trendPlotS2, trendPlotS3, trendPlotS4))
  dev.off()
  
}
