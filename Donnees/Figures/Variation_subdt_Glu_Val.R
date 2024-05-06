library(ggplot2)
library(dplyr)
library(tidyr)

# Données Glutamate vers Valine 

# Comparaison de l'effet des subdt du fichier d'optimisation

donnees <- read.table("Variation_subdt_Glu_Val.tsv" , sep = "\t" , header = TRUE , dec = ",")
head(donnees)

val <- ggplot(donnees, aes(x = log10(sub.dt), y = Valeurs, col = Feuilles)) +
  geom_point() +
  geom_hline(aes(yintercept = 18.75296, color = "Scalaflux feuille A"), show.legend = TRUE) +
  geom_hline(aes(yintercept = 25.82443, color = "Scalaflux feuille B"), show.legend = TRUE) +
  geom_hline(aes(yintercept = 16.17588, color = "Scalaflux feuille C"), show.legend = TRUE) +
  labs(
    x = "Variation sub dt (log10)",
    y = "Valeurs (nmol/g/poid sec /min)",
    title = "Comparaison des valeurs entre Influx et Scalaflux avec variation de subdt sur les données Glutamate vers Valine ",
    subtitle = "Données Glutamate vers Valine")+
  scale_color_discrete(name = "Logiciel", 
                     labels = c("Influx feuille A", "Influx feuille B", "Influx feuille C" , "Scalaflux feuille A" , "Scalaflux feuille B" , "Scalaflux feuille C"))

val

# Comparaison des sd dans le fichier miso avec subdt 10

donnees_sd_10 <- read.table("Variation_sd_subdt10_Glu_Val.tsv" , sep = "\t", header = TRUE  , dec = ",")
head(donnees_sd_10)
# geom_tile permet de faire une "heatmap" avec ggplot
val_sd <- ggplot(donnees_sd_10, aes(x = SD, y = Feuilles, fill = Valeurs)) +
  geom_tile()+
  labs (title = "Comparaison des valeurs de SD sur le fonctionnement du logiciel Influx avec un subdt de 10",
        subtitle = "Données Glutamate vers Valine")
val_sd

# Comparaison des sd dans le fichier miso avec subdt 1

donnees_sd_1 <- read.table("Variation_sd_subdt1_Glu_Val.tsv" , sep = "\t" , header = TRUE )
head(donnees_sd_1)
val_sd_1 <- ggplot (donnees_sd_1 , aes(x = SD , y = Feuilles , fill = Valeurs)) +
  geom_tile()+
  labs(title =  "Comparaison des valeurs de SD sur le fonctionnement du logiciel Influx avec un subdt de 1", 
       subtitle = "Données Glutamate vers Valine")
val_sd_1

# Données Aspartate vers Thréonine 

# Comparaison des sd dans le fichier miso avec subdt 1000

donnee <- read.table("Variation_sd_subdt_1000_Asp_Thr.tsv" , sep = "\t" , header = TRUE)
head(donnee)

thr_subdt_1000 <- ggplot(donnee , aes(x = SD , y = Feuilles , fill = Valeurs))+
  geom_tile()+
  labs(title = "Comparaison des valeurs de SD sur le fonctionnement du logiciel Influx avec un subdt de 1000",
       subtitle = "Données Aspartate vers Thréonine")
thr_subdt_1000