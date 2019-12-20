library(raster)
library(psych)
library(reshape2)
library(dplyr)
library(tidyr)
library(data.table)

paras = c("AE", "SSA", "AOD_550","COT","CWP","CER")
seasons = c("s1","s2","s3","s4")
season_label = c("season 1", "season 2", "season 3", "season 4")

if (!file.exists("../results/values/corTables.rds")){
  nas = lapply(paras, function(x){
    para = stack(list.files(paste0("../results/season/", x, "/") , pattern = "mean", full.names = T))
    indNA = which(is.na(para[]))
    return(indNA)
  })
  
  nas = unique(unlist(nas))
  
  vals = lapply(paras, function(x){
    para = stack(list.files(paste0("../results/season/", x, "/") , pattern = "mean", full.names = T))
    para[nas] = NA
    return(para)
  })
  
  
  corTables = lapply(seasons, function(s){
    
    seasonVals = lapply(vals, function(x){
      y = x[[grep(s, names(x))]]
      return(c(na.omit((values(y)))))
    })
    
    getCor = function(x){
      if (!(sum(is.na(x)) / 2) > length(years)/2){ #50% of time series present a cor will be calculated
        para1 = x[1:length(years)]
        para2 = x[(length(years)+1):length(x)]
        result = cor.test(para1, para2, method = "spearman")
        return(c(result$p.value))
      }else{
        return(c(NA))
      }
    }
    
    names(seasonVals) = paras
    data = as.data.frame(seasonVals)
    
    corTable = psych::corr.test(data, method="spearman")
    rho = corTable$r
    pvalue = corTable$p
    results = list(rho, pvalue)
    names(results) = c("rho", "pvalue")
    return(results)
    
  })
  
  names(corTables) = seasons
  saveRDS(corTables, file = "../results/values/corTables.rds")
} else {
  corTables = readRDS("../results/values/corTables.rds")
}

combinations = as.data.frame(matrix(c("AE", "SSA", "\ AE vs. SSA",
                        "AE", "AOD_550", "\ AE vs. AOD_550",
                        "SSA", "AOD_550", "SSA vs. AOD_550",
                        "AOD_550", "CER", "AOD_550 vs. CER",
                        "AOD_550", "CWP", "AOD_550 vs. CWP",
                        "AOD_550", "COT", "AOD_550 vs. COT"), ncol=3, byrow=T))

names(combinations) =  c("para1", "para2", "parameters")
combinations['season 1'] = 0
combinations['season 2'] = 0
combinations['season 3'] = 0
combinations['season 4'] = 0
combinations['p1'] = 0
combinations['p2'] = 0
combinations['p3'] = 0
combinations['p4'] = 0

for (s in 1:length(seasons)){
  tmp = corTables[[s]]$rho
  for (i in 1:nrow(combinations)){
  val = tmp[which(rownames(tmp) == combinations$para1[i]) , which(colnames(tmp) == combinations$para2[i])]
  combinations[i, which(names(combinations) == season_label[s])] = round(val,2)
  }
}

for (s in 1:length(seasons)){
  tmp = corTables[[s]]$pvalue
  for (i in 1:nrow(combinations)){
    val = tmp[which(rownames(tmp) == combinations$para1[i]) , which(colnames(tmp) == combinations$para2[i])]
    combinations[i, (which(names(combinations) == season_label[s])+ 4)] = val
  }
}

combinations[,4:7][which(combinations[,8:11] < 0.1 & combinations[,8:11] > 0.05, arr.ind=TRUE)] = paste(combinations[,4:7][which(combinations[,8:11] < 0.1 & combinations[,8:11] > 0.05, arr.ind=TRUE  )],"*\ \ ",sep="")
combinations[,4:7][which(combinations[,8:11] < 0.05 & combinations[,8:11] > 0.01, arr.ind=TRUE)] = paste(combinations[,4:7][which(combinations[,8:11] < 0.05 & combinations[,8:11] > 0.01, arr.ind=TRUE  )],"**\ ",sep="")
combinations[,4:7][which(combinations[,8:11] < 0.01, arr.ind = T)] = paste(combinations[,4:7][which(combinations[,8:11] < 0.01, arr.ind = T)],"***",sep="")
combinations[,4:7][which(combinations[,8:11] > 0.1, arr.ind = T)] = paste(combinations[,4:7][which(combinations[,8:11] > 0.1, arr.ind = T)],"\ \ \ ",sep="")

resultTable = combinations[,c(3:7)]
saveRDS(resultTable, "../results/values/corPrintTable.rds", ascii = T)
