library(stringr)
library(raster)
library(ggplot2)
library(reshape2)
library(dplyr)
library(purrr)
library(gridExtra)
library(ggpubr)

# select relevant parameters
paras = str_to_upper(c("cer", "cot", "cwp"))

# read parameters observation number files into raster stack in a list
paraStacks = lapply(paras, function(x){
  stack(list.files(paste0("../results/",x), pattern = "mean", full.names = TRUE))
})

# vector indicating the years for which to calculate mean values
years = 2003:2018

if (!file.exists("../results/values/yearly_cloudProps.rds")){
  # nested lapply loop through every parameter and the years to get one mean value per parameter and year 
  yearlyMeans = lapply(paraStacks, function(x){
    print(x)
    tmpLS = lapply(years, function(y){
      tmp = x[[grep(as.character(y), names(x))]]
      beginCluster(parallel::detectCores()-1)
      ymean = clusterR(tmp, calc, args=list(fun=mean, na.rm=TRUE))
      endCluster()
      tmp2 = c(na.omit(values(ymean)))
      df = data.frame(year=rep(y, length(tmp2)), values = tmp2)
      return(df)
    })
    xmean = do.call(rbind, tmpLS)
    return(xmean)
  })
  
  # preparation for plotting
  names(yearlyMeans) = paras
  for (i in 1:length(paras)){
    yearlyMeans[[paras[i]]]$type = paras[i]
  }
  
  df = as_tibble(do.call(rbind, yearlyMeans))
  #df$year = years
  #df = melt(df, id.vars = "year")
  saveRDS(df, file = "../results/values/yearly_cloudProps.rds")
} else {
  df = readRDS("../results/values/yearly_cloudProps.rds")
}


meanVals = df %>% 
  group_by(type, year) %>%
  summarize(mean = mean(values))

regCoef = meanVals %>%
  group_by(type) %>%
  summarize(slope=lm(mean ~ year)$coefficients[2], intercept=lm(mean ~ year)$coefficients[1], r=summary(lm(mean ~ year))$r.squared )


# using ggplot2 to plot every parameter in a facet
meanplot = ggplot(df, aes(x=year))+
  geom_boxplot(aes(y=values, group=year))+
  geom_point(data=meanVals, aes(x=year, y=mean, group=type), color="red")+
  #geom_abline(data=regCoef, aes(intercept=intercept, slope=slope, group=type), col="red")+
  facet_wrap(~type, ncol=3, scales = "free")+
  scale_x_discrete(limits=years)+
  #geom_text(data=regCoef,
  #          mapping= aes(x=2017, y=10, label= paste("slope: ", round(slope, 2), sep="")))+
  #geom_text(data=regCoef,
  #          mapping= aes(x=2014, y=10, label= paste("R²: ", round(r, 4), sep="")))+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45))

# saving plot to disk
ggsave(plot=meanplot, file = "../results/plots/cloudPros_yearly.png", dpi=600, device="png", width = 20, height = 10)

