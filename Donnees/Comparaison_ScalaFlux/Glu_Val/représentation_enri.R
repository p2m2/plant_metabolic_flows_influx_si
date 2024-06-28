# Réprésentation de l'enrichissement glutamate vers valine
library(ggplot2)

donnees <- read.table("enrichissement_glu_val.tsv" , header = T, sep = "\t" , dec = ",")
donnees$feuilles <- as.factor(donnees$feuilles)
str(donnees)
donnees

figure <- ggplot (donnees , aes (x = feuilles , y = différence.enri , fill = replicat))+
  geom_bar(stat = "identity" ,  position=position_dodge() ) +
  labs (x = "Feuilles",
        y = "Différence d'enrichissement (glu-val)" ,
        title = "Pourcentage d'enrichissement en fonction des feuilles avec les différents réplicats",
        subtitle = "Flux Glutamate vers Valine") +
  theme(
    plot.title = element_text(size = 20),      
    plot.subtitle = element_text(size = 18),
    axis.title = element_text(size = 30),    
    axis.text = element_text(size = 30),
    legend.title = element_text(size = 30),           
    legend.text = element_text(size = 30)
)
figure

