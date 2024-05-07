library(ggplot2)
library(dplyr)
library(tidyr)

# Définition de paramètres
couleurs <- c("Valeur de flux" = "blue",
               "Valeur de flux nulle ou négative" = "green",
               "Valeur de flux incalculable" = "yellow")

# Données Glutamate vers Valine 

# Comparaison de l'effet des subdt du fichier d'optimisation

donnees <- read.table("Variation_subdt_Glu_Val.tsv" , sep = "\t" , header = TRUE , dec = ",")
head(donnees)

donnees_modif <- donnees %>%
  rename(Influx = Valeurs_Influx , Scalaflux = Valeurs_Scalaflux) %>%
  pivot_longer(cols = c(Influx , Scalaflux) , names_to = "Logiciel" , values_to = "Valeurs")

val <- ggplot(donnees_modif, aes(x = log10(sub.dt),y = Valeurs , col = Logiciel , shape = Feuilles)) +
  geom_point() +
  geom_line()+
  labs(
    x = "Variation sub dt (log10)",
    y = "Valeurs (nmol/g/poid sec /min)",
    title = "Comparaison des valeurs de flux entre Influx et Scalaflux avec un SD de -2 et variation de subdt",
    subtitle = "Flux Glutamate vers Valine")

val

# Comparaison des sd dans le fichier miso avec subdt 10

donnees_sd_10 <- read.table("Variation_sd_subdt10_Glu_Val.tsv" , sep = "\t", header = TRUE  , dec = ",")
head(donnees_sd_10)
donnees_sd_10$Feuille <- as.numeric(gsub("[^0-9]", "", donnees_sd_10$Feuilles))
donnees_sd_10$Rep <- gsub("[^A-Za-z]", "", donnees_sd_10$Feuilles)

val_sd <- donnees_sd_10 %>%
  mutate(Feuilles = factor(Feuilles, levels = unique(Feuilles[order(Feuille)]))) %>%
  ggplot(aes(x = SD, y = Feuilles, fill = Valeurs)) +
  geom_tile() +
  labs(
    title = "Comparaison des valeurs de SD sur le fonctionnement du logiciel Influx avec un subdt de 10",
    subtitle = "Flux Glutamate vers Valine"
  )+
  scale_fill_manual(values = couleurs)

val_sd
# Comparaison des sd dans le fichier miso avec subdt 1

donnees_sd_1 <- read.table("Variation_sd_subdt1_Glu_Val.tsv" , sep = "\t" , header = TRUE )
head(donnees_sd_1)
donnees_sd_1$Feuille <- as.numeric(gsub("[^0-9]", "", donnees_sd_1$Feuilles))
donnees_sd_1$Rep <- gsub("[^A-Za-z]", "", donnees_sd_1$Feuilles)

val_sd_1 <- donnees_sd_1 %>%
  mutate(Feuilles = factor(Feuilles, levels = unique(Feuilles[order(Feuille)]))) %>%
  ggplot(aes(x = SD, y = Feuilles, fill = Valeurs)) +
  geom_tile()+
  labs(title =  "Comparaison des valeurs de SD sur le fonctionnement du logiciel Influx avec un subdt de 1", 
       subtitle = "Flux Glutamate vers Valine")+
  scale_fill_manual(values = couleurs)
val_sd_1

# Données Aspartate vers Thréonine 

# Comparaison des sd dans le fichier miso avec subdt 1000

donnee <- read.table("Variation_sd_subdt_1000_Asp_Thr.tsv" , sep = "\t" , header = TRUE)
head(donnee)
donnee$Feuille <- as.numeric(gsub("[^0-9]", "", donnee$Feuilles))
donnee$Rep <- gsub("[^A-Za-z]", "", donnee$Feuilles)


thr_subdt_1000 <- donnee %>%
  mutate(Feuilles = factor(Feuilles, levels = unique(Feuilles[order(Feuille)]))) %>%
  ggplot(aes(x = SD , y = Feuilles , fill = Valeurs))+
  geom_tile()+
  labs(title = "Comparaison des valeurs de SD sur le fonctionnement du logiciel Influx avec un subdt de 1000",
       subtitle = "Flux Aspartate vers Thréonine")+
  scale_fill_manual(values = couleurs)
thr_subdt_1000
