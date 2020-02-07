library(ggplot2)

totalCorr = readRDS("../results/totalCorr.rds")

ggplot(data=totalCorr, aes(y=estimate, x=contVar, fill=contVar)) +
  geom_bar(stat="identity", width = .5) +
  facet_wrap(~season)+
  scale_fill_brewer(palette="Dark2") +
  ylab("Spearman's Rho")+
  xlab("Control Variables")+
  labs(fill = "Control Variables")+
  theme_classic()