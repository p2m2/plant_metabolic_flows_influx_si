# Réprésentation de l'enrichissement aspartate vers thréonine
library(ggplot2)

donnees <- read.table("enrichissement_asp_thr.tsv" , header = T, sep = "\t" , dec = ",")
donnees$feuilles <- as.factor(donnees$feuilles)
str(donnees)
donnees

figure <- ggplot (donnees , aes (x = feuilles , y = difference_enri , fill = replicat))+
  geom_bar(stat = "identity" ,  position=position_dodge() ) +
  labs (x = "Feuilles",
        y = "Différence d'enrichissement (asp-thr)" ,
        title = "Pourcentage d'enrichissement en fonction des feuilles avec les différents réplicats",
        subtitle = "Flux Aspartate vers Thréonine") +
  theme(
    plot.title = element_text(size = 20),      
    plot.subtitle = element_text(size = 18),
    axis.title = element_text(size = 30),    
    axis.text = element_text(size = 30),
    legend.title = element_text(size = 30),           
    legend.text = element_text(size = 30)
  )
figure