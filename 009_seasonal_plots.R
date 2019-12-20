library(raster)
library(rasterVis)


paras = c("AE", "AOD_550", "CER", "COT", "CWP", "SSA")
seasons = c("s1", "s2", "s3", "s4")

units = c("none","none","micron","none","g/m²","none")

for (p in paras){
  print(p)
  cer = stack(list.files(paste0("../results/season/",p,"/"), full.names = TRUE,
                         pattern = "20.._s"))
  
  
  cer_means = lapply(seasons, function(s){
    tmp = cer[[grep(s, names(cer))]]
    return(mean(tmp))
  }) 
  cer_means = stack(cer_means)
  
  cerTheme = BuRdTheme(region = rev(brewer.pal(10, "RdBu"))[10:0])
  
  png(file = paste0("../results/plots/seasonal_means_",p,".png"), 
      width = 15, height = 15, units = "cm", res = 900)
  print(levelplot(cer_means, par.settings = cerTheme,
            names.attr = c("a", "b", "c", "d")))
  dev.off()
}
