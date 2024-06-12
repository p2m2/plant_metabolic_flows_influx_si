# Réprésentation de l'enrichissement glutamate vers valine
library(ggplot2)

donnees <- read.table("enrichissement_mal.tsv" , header = T, sep = "\t" , dec = ",")
str(donnees)

figure_mal <- ggplot (donnees , aes (x = condition , y = difference_enri , fill = replicat))+
  geom_bar(stat = "identity" ,  position=position_dodge() ) +
  labs (x = "Condition",
        y = "Différence d'enrichissement (suc-mal)" ,
        title = "Pourcentage d'enrichissement en fonction des conditions avec les différents réplicats",
        subtitle = "Flux Succinate vers Malate") +
  theme(
    plot.title = element_text(size = 20),      
    plot.subtitle = element_text(size = 18),
    axis.title = element_text(size = 20),    
    axis.text = element_text(size = 20),
    legend.title = element_text(size = 20),           
    legend.text = element_text(size = 20) 
  )
figure_mal
