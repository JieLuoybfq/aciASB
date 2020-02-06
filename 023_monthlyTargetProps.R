library(stringr)
library(raster)
library(ggplot2)
library(lubridate)
library(reshape2)
library(dplyr)

# select relevant parameters
paras = str_to_upper(c("aod_550", "P", "RH"))
# read parameters observation number files into raster stack in a list
paraStacks = lapply(paras, function(x){
  files_list = list.files(paste0("../results/", x), full.names = TRUE)
  stack(files_list[grep("(mean|median|chirps)", files_list)])
})

# vector indicating the years for which to calculate mean values
years = 2003:2018

# declarig months abbrevations
months = as.character(month(1:12, label = TRUE))


if (!file.exists("../results/values/monthly_targetProbs.rds")){
  # lapply loop through parameters and months to get seasonality for the whole time spawn
  monthlyMeans = lapply(paraStacks, function(x){
    print(x)
    tmpLS = lapply(months, function(m){
      tmp = x[[grep(as.character(m), names(x))]]
      beginCluster(parallel::detectCores()-1)
      ymean = clusterR(tmp, calc, args=list(fun=mean, na.rm=TRUE))
      endCluster()
      tmp2 = c(na.omit(values(ymean)))
      df = data.frame(month=rep(m, length(tmp2)), values = tmp2)
      return(df)
    })
    xmean = do.call(rbind, tmpLS)
    return(xmean)
  })
  
  # preparation for plotting
  names(monthlyMeans) = paras
  for (i in 1:length(paras)){
    monthlyMeans[[paras[i]]]$type = paras[i]
  }
  
  df = as_tibble(do.call(rbind, monthlyMeans))
  saveRDS(df, file = "../results/values/monthly_targetProbs.rds")
} else {
  df = readRDS("../results/values/monthly_targetProbs.rds")
}


meanVals = df %>% 
  group_by(type, month) %>%
  summarize(mean = mean(values))

regCoef = meanVals %>%
  group_by(type) %>%
  summarize(slope=lm(mean ~ month)$coefficients[2], intercept=lm(mean ~ month)$coefficients[1], r=summary(lm(mean ~ month))$r.squared )

# using ggplot2 to plot every parameter in a facet
# using ggplot2 to plot every parameter in a facet
meanplot = ggplot(df, aes(x=month))+
  geom_boxplot(aes(y=values, group=month))+
  geom_smooth(aes(y=values, group=type), se=FALSE)+
  geom_point(data=meanVals, aes(x=month, y=mean, group=type), color="red")+
  #geom_abline(data=regCoef, aes(intercept=intercept, slope=slope, group=type), col="red")+
  facet_wrap(~type, ncol=3, scales = "free")+
  scale_x_discrete(limits=months)+
  #geom_text(data=regCoef,
  #          mapping= aes(x=2017, y=10, label= paste("slope: ", round(slope, 2), sep="")))+
  #geom_text(data=regCoef,
  #          mapping= aes(x=2014, y=10, label= paste("R²: ", round(r, 4), sep="")))+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45))

# saving plot to disk
ggsave(plot=meanplot, file = "../results/plots/targetProps_monthly.png", dpi=600, device="png", width = 20, height = 10)
