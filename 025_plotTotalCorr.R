library(ggplot2)

totalCorr = readRDS("../results/totalCorr.rds")

plt = ggplot(data=totalCorr, aes(y=estimate, x=contVar, fill=contVar)) +
  geom_bar(stat="identity", width = .5) +
  facet_wrap(~season)+
  scale_fill_brewer(palette="Set2") +
  ylab("Spearman's Rho")+
  xlab("Control Variables")+
  labs(fill = "Control Variables")+
  theme_classic()+
  coord_flip()+
  theme(text = element_text(size=15))
ggsave(filename="../results/plots/totalCorr.png", plt)
