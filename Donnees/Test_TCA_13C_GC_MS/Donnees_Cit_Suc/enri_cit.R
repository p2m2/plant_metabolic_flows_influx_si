# Réprésentation de l'enrichissement glutamate vers valine
library(ggplot2)

donnees <- read.table("enrichissement_cit.tsv" , header = T, sep = "\t" , dec = ",")
str(donnees)
donnees$difference_enri_.cit...glu.

figure_suc <- ggplot (donnees , aes (x = condition , y = difference_enri._.cit...suc. , fill = replicat))+
  geom_bar(stat = "identity" ,  position=position_dodge() ) +
  labs (x = "Condition",
        y = "Différence d'enrichissement (cit-suc)" ,
        title = "Pourcentage d'enrichissement en fonction des conditions avec les différents réplicats",
        subtitle = "Flux Citrate vers Succinate") +
  theme(
    plot.title = element_text(size = 20),      
    plot.subtitle = element_text(size = 18),
    axis.title = element_text(size = 20),    
    axis.text = element_text(size = 20),
    legend.title = element_text(size = 20),           
    legend.text = element_text(size = 20) 
  )
figure_suc
figure_suc_glu <- ggplot (donnees , aes (x = condition , y = difference_enri_.cit...glu. , fill = replicat))+
  geom_bar(stat = "identity" ,  position=position_dodge() ) +
  labs (x = "Condition",
        y = "Différence d'enrichissement (cit-glu)" ,
        title = "Pourcentage d'enrichissement en fonction des conditions avec les différents réplicats",
        subtitle = "Flux Citrate vers Glutamate") +
  theme(
    plot.title = element_text(size = 20),      
    plot.subtitle = element_text(size = 18),
    axis.title = element_text(size = 20),    
    axis.text = element_text(size = 20),
    legend.title = element_text(size = 20),           
    legend.text = element_text(size = 20) 
  )
figure_suc_glu