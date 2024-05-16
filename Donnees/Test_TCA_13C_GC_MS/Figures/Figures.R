# Résultat de flux modéliser par Influx_SI, Succinate vers Malate de l'article "Validation of carbon isotopologue distribution measurements by GC-MS and application to 13C-metabolic flux analysis of the tricarboxylic acid cycle in Brassica napus leaves"

library(dplyr)
library(tidyr)
library(ggplot2)

resultat <- read.table("Flux_Succinate_Malate.tsv" , header = TRUE , sep = "\t" , dec = ",")
head(resultat)
resultat

tableau <- resultat %>%
  group_by(conditions) %>%
  summarise( moyenne = mean(Valeurs.de.flux..nmol.poids_sec.min.) , ecart_type = sd(Valeurs.de.flux..nmol.poids_sec.min.)) 
  
visualiser <- ggplot(tableau , aes (x = conditions , y = moyenne , fill = conditions )) + 
  geom_bar(stat = "identity") + 
  geom_errorbar(aes (ymin = moyenne - ecart_type, ymax = moyenne + ecart_type ), width = 0.4)

tableau
visualiser

  

