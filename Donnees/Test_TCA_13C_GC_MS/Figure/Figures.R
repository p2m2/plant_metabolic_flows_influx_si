# Résultat de flux modéliser par Influx_SI, Succinate vers Malate de l'article "Validation of carbon isotopologue distribution measurements by GC-MS and application to 13C-metabolic flux analysis of the tricarboxylic acid cycle in Brassica napus leaves"

library(dplyr)
library(tidyr)
library(ggplot2)

resultat_M <- read.table("Flux_Succinate_Malate.tsv" , header = TRUE , sep = "\t" , dec = ",")
head(resultat_M)
str(resultat_M)


tableau <- resultat_M %>%
  group_by(conditions) %>%
  summarise( moyenne = mean(Valeurs_flux_.nmol.poids_sec.min.) , ecart_type = sd(Valeurs_flux_.nmol.poids_sec.min.)) 
  
visualiser <- ggplot(tableau , aes (x = conditions , y = moyenne , fill = conditions )) + 
  geom_bar(stat = "identity") + 
  geom_errorbar(aes (ymin = moyenne - ecart_type, ymax = moyenne + ecart_type ), width = 0.4 , size = 5)+
  labs (y = "Valeurs de flux (nmol/poidssec/min)" ,
        x = "Moyenne des flux",
        title = "Moyennes des flux en fonction des conditions" ,
        subtitle = "Flux Succinate vers Malate")+  
  theme(
    plot.title = element_text(size = 20),      
    plot.subtitle = element_text(size = 18),
    axis.title = element_text(size = 30),    
    axis.text = element_text(size = 30),
    legend.title = element_text(size = 30),           
    legend.text = element_text(size = 30))

tableau
visualiser

# Représentation avec les valeurs de chi2

visua_chi2 <- ggplot (resultat_M , aes ( x = répétitions ,y = log10(chi2_reduit) , fill = répétitions )) +
  geom_bar(stat = "identity" , position=position_dodge())+
  facet_grid(.~conditions) +
  labs (y = "Valeur de chi2 réduit (log10)" ,
        x = "Répétition",
        title = "Valeurs de chi2 en fonction des répétitions pour les conditions light et dark" ,
        subtitle = "Flux Succinate vers Malate")+  
  theme(
    plot.title = element_text(size = 20),      
    plot.subtitle = element_text(size = 18),
    axis.title = element_text(size = 30),    
    axis.text = element_text(size = 30),
    legend.title = element_text(size = 30),           
    legend.text = element_text(size = 30),
    strip.text = element_text(size = 30))
visua_chi2 
# Résultat de flux modéliser par Influx_SI, Citrate vers Succinate  de l'article "Validation of carbon isotopologue distribution measurements by GC-MS and application to 13C-metabolic flux analysis of the tricarboxylic acid cycle in Brassica napus leaves"

resultat_S <- read.table("Flux_Citrate_Succinate.tsv" , header = TRUE , sep = "\t" , dec = "," , stringsAsFactors = TRUE)
resultat_S$enrichissement <- as.factor(resultat_S$enrichissement)
str(resultat_S)

figure_S <- ggplot(resultat_S , aes (x =  enrichissement , y = log10(chi2_reduit) ,fill = repetition )) +
  geom_bar (stat = "identity", position=position_dodge() , na.rm = TRUE) +
  facet_grid(.~condition) +
  labs (y = "Valeur de chi2 réduit (log10)" ,
        x = "Pourcentage d'enrichissement",
        title = "Valeurs de chi2 reduit en fonction du pourcentage d'enrichissement" ,
        subtitle = "Flux Citrate vers Succinate")+
  theme(
    plot.title = element_text(size = 20),      
    plot.subtitle = element_text(size = 18),
    axis.title = element_text(size = 30),    
    axis.text = element_text(size = 30),
    legend.title = element_text(size = 30),           
    legend.text = element_text(size = 30),
    strip.text = element_text(size = 30))
figure_S

figure_valeur <- ggplot(resultat_S , aes (x =  enrichissement , y = log10(vCit) ,fill = repetition )) +
  geom_bar (stat = "identity", position=position_dodge() , na.rm = TRUE) +
  facet_grid(.~condition) +
  labs (y = "Valeur de chi2 réduit" ,
        x = "Pourcentage d'enrichissement",
        title = "Valeurs de chi2 reduit en fonction du pourcentage d'enrichissement" ,
        subtitle = "Flux Citrate vers Succinate")
figure_valeur

# Résultat de flux modéliser par Influx_SI, Citrate vers Succinate et Glutamate  de l'article "Validation of carbon isotopologue distribution measurements by GC-MS and application to 13C-metabolic flux analysis of the tricarboxylic acid cycle in Brassica napus leaves"

resultat_S_C <- read.table("flux_cit_suc_glu.tsv" , header = T , sep="\t" , dec = ",", stringsAsFactors = T)
resultat_S_C$taux_enrichissement <- as.factor(resultat_S_C$taux_enrichissement)
str(resultat_S_C)

# A partir du graphique du Citrate vers Succinate on prend le pourcentage d'enrichissement associés au chi2 le plus faible 

tableau_filtre <- resultat_S_C %>%
  filter ((condition == "dark" & taux_enrichissement == 20 & replicat == "C") | 
          (condition == "dark" & taux_enrichissement == 20 & replicat == "D") |
            (condition == "dark" & taux_enrichissement == 25 & replicat == "C") |
            (condition == "dark" & taux_enrichissement == 30 & replicat == "C")|
            (condition == "dark" & taux_enrichissement == 35 & replicat == "B") |
            (condition == "dark" & taux_enrichissement == 40 & replicat == "A") |
            (condition == "light" & taux_enrichissement == 25 & replicat == "A") |
            (condition == "light" & taux_enrichissement == 30 & replicat == "A") |
            (condition == "light" & taux_enrichissement == 20 & replicat == "B") |
            (condition == "light" & taux_enrichissement == 20 & replicat == "C") |
            (condition == "light" & taux_enrichissement == 20 & replicat == "D"))
tableau_filtre
str(tableau_filtre)

figure_S_C <- ggplot(tableau_filtre , aes( x = taux_enrichissement , y =  pourcentage_vGlu.vSuc , shape = replicat))+
  geom_point()+
  geom_line(aes (group = replicat , color = replicat))+
  facet_grid(.~ condition)+
  labs (y = "Pourcentage de flux vGlu/vSuc " ,
        x = "Pourcentage d'enrichissement",
        title = "Pourcentage de flux en fonction du pourcentage d'enrichissement" ,
        subtitle = "Flux Citrate vers Succinate")
figure_S_C
# Représentation des valeurs de chi2 réduit avec la sélection d'enrichissent du flux citrate vers succinate

figure_valeurs <- ggplot(resultat_S_C , aes (x =taux_enrichissement, y = chi2_reduit  ,fill = replicat )) +
  geom_bar (stat = "identity", position=position_dodge() , na.rm = TRUE) +
  facet_grid(.~condition) +
  labs (y = "Valeur de chi2 réduit" ,
        x = "Pourcentage d'enrichissement",
        title = "Valeurs de chi2 reduit en fonction du pourcentage d'enrichissement" ,
        subtitle = "Flux Citrate vers Succinate et Glutamate") +
  theme(
    plot.title = element_text(size = 20),      
    plot.subtitle = element_text(size = 18),
    axis.title = element_text(size = 30),    
    axis.text = element_text(size = 30),
    legend.title = element_text(size = 30),           
    legend.text = element_text(size = 30),
    strip.text = element_text(size = 30))
figure_valeurs

# Représentation des valeurs de flux citrate succinate fixé lors de l analyse précédente 
donnees_trie <- tableau_filtre %>%
  pivot_longer(cols = c(vSuc, vCit, vGlu),
               names_to = "variable",
               values_to = "valeur")
# Créer le barplot
ggplot(donnees_trie, aes(x = taux_enrichissement, y = log10(valeur), fill = replicat)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~ variable, scales = "free_y") +
  labs(x = "Taux d'enrichissement",
       y = "Valeur de flux en log10",
       fill = "Répétition",
       title = "Valeurs de vSuc, vCit et vGlu pour les différents taux d'enrichissement et répétitions") +
  theme(
    plot.title = element_text(size = 20),      
    plot.subtitle = element_text(size = 18),
    axis.title = element_text(size = 30),    
    axis.text = element_text(size = 30),
    legend.title = element_text(size = 30),           
    legend.text = element_text(size = 30),
    strip.text = element_text(size = 30))

filtre <- resultat_S_C %>%
  filter ((condition == "dark" & taux_enrichissement == 25 & replicat == "C") | 
            (condition == "dark" & taux_enrichissement == 20 & replicat == "D") |
            (condition == "dark" & taux_enrichissement == 25 & replicat == "D") |
            (condition == "dark" & taux_enrichissement == 30 & replicat == "B") |
            (condition == "dark" & taux_enrichissement == 40 & replicat == "A") |
            (condition == "light" & taux_enrichissement == 30 & replicat == "A") |
            (condition == "light" & taux_enrichissement == 25 & replicat == "B") |
            (condition == "light" & taux_enrichissement == 20 & replicat == "C") |
            (condition == "light" & taux_enrichissement == 25 & replicat == "C") |
            (condition == "light" & taux_enrichissement == 15 & replicat == "D"))
filtre
str(filtre)

pourcentage_trie <- filtre %>%
  pivot_longer(cols = c(ratio_vSuc.vGlu, ratio_vSuc.vCit, ratio_vGlu.vCit),
               names_to = "variable",
               values_to = "valeur")
ggplot(pourcentage_trie, aes(x = taux_enrichissement, y = valeur, fill = replicat)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap( variable~condition, scales = "free_y") +
  labs(x = "Taux d'enrichissement",
       y = "Valeur de flux",
       fill = "Répétition",
       title = "Pourcentage vCit VGlu VSuc pour les différents taux d'enrichissement et répétitions") +
  theme(
    plot.title = element_text(size = 20),      
    plot.subtitle = element_text(size = 18),
    axis.title = element_text(size = 40),    
    axis.text = element_text(size = 40),
    legend.title = element_text(size = 30),           
    legend.text = element_text(size = 30),
    strip.text = element_text(size = 30))
print(pourcentage_trie , n = 36)
