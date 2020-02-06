library(raster)
library(rasterVis)


paras = c("P")
seasons = c("s1", "s2", "s3", "s4")

units = c("mm/season")

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
            main = p, xlab.top = paste0("average seasonal values - unit: ", units[which(paras == p)]),
            names.attr = c("Mar Apr May", "Jun Jul Aug", "Sep Oct Nov", "Dec Jan Feb")))
  
  dev.off()
}
