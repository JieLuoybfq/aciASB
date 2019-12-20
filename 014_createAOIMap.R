library(rasterVis)
library(rgdal)           # required for readOGR and spTransform
library(RColorBrewer)
library(raster)
library(maptools)

# read shapefile
r = raster("../data/dummy_10.tif")
e = extent(r)
wmap = as(e, "SpatialPolygons")
d = data.frame(id =1)
wmap = SpatialPolygonsDataFrame(wmap, d)
crs(wmap) = crs(r)


writeOGR(wmap, dsn="../data/aral_aoi.shp", layer="aral_aoi", driver="ESRI Shapefile")

rivers = readOGR("../data/shapes/ne_10m_rivers_lake_centerlines.shp")
amu = rivers[rivers$name =="Amu Darya", ]
amu = crop(amu, wmap)
sir = rivers[rivers$name == "Syr Darya", ]
df = sir@data[2, ]
rownames(df) = 1
sir = rgeos::gUnion(sir[1,], sir[2,])
sir = SpatialLinesDataFrame(sir, df)
sir = crop(sir, wmap)
river = rbind(sir,amu)
lake = readOGR("../data/shapes/ne_10m_lakes.shp")
lake = lake[which(lake$name == "Aral Sea"),]



dem1 = getData("alt", country="UZB")
dem2 = getData("alt", country="KAZ")
dem3 = getData("alt", country="TKM")
dem = merge(dem1, dem2, dem3)
dem = crop(dem, wmap)

kaz = getData("GADM", country="KAZ", level=0)
tkm = getData("GADM", country="TKM", level=0)


years = 2003:2018
pmean = lapply(years, function(y){
  tmp = stack(list.files("../results/P/", pattern= as.character(y), full.names=T))
  return(sum(tmp))
})


pmean = mean(stack(pmean))
P_contours = rasterToContour(pmean)


demPal =gray.colors(10)
slP <- list('sp.lineLabel', P_contours, label=label(P_contours, P_contours$level),
            cex=0.7, col='black', lty=2, col.line="black", byid=F,
            position="above")
slA <- list('sp.lineLabel', amu, label=label(amu, amu$name),
            cex=0.7, col='lightblue3', lwd=1.5, col.line="lightblue3", byid=T,
            position="above")
slS <- list('sp.lineLabel', sir, label=label(sir, sir$name),
            cex=0.7, col='lightblue3', lwd=1.5, col.line="lightblue3", byid=F,
            position="above")


png(filename = "../results/plots/aral_aoi.png",width = 30, height = 30, units = "cm", res = 600)
levelplot(dem, par.settings=rasterTheme(demPal),  margin=F, pretty=T )+
  latticeExtra::layer(sp.polygons(lake, fill="lightblue3"))+
  latticeExtra::layer(sp.text(c(64.5,45.5), "Syr Darya", "lightblue3", cex=.8))+
  latticeExtra::layer(sp.text(c(60,42.5), "Amu Darya", "lightblue3", cex=.8))+
  latticeExtra::layer(sp.lines(sir, col="lightblue3", lwd=1.5))+
  latticeExtra::layer(sp.lines(amu, col="lightblue3", lwd=1.5))+
  latticeExtra::layer(sp.lines(kaz, col="red", lty=1, lwd=1.5))+
  latticeExtra::layer(sp.lines(tkm, col="red", lty=1, lwd=1.5))+
  latticeExtra::layer(sppanel(slP))+
  latticeExtra::layer(sp.text(c(62,42.5), "UZB", "red"))+
  latticeExtra::layer(sp.text(c(59,42.2), "TKM", "red"))+
  latticeExtra::layer(sp.text(c(63,45), "KAZ", "red"))
dev.off()
  

