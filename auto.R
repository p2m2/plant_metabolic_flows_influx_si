library(dplyr)
library(tidyr)

# Automatisation de création de fichiers d'entrée pour le logiciel Influx pour des sortie de MS

# Données isotopologique 
donnees_sombre <- read.table ("Table 2 - 13Cpyruvate_isocor_outputs.tsv" , header = TRUE , sep = "\t" , dec = ",")
head(donnees_sombre)

# Données quantités absolus
donnees_sombre_pool <- read.table("13Cpyruvate_poolsize.tsv" , header = TRUE , sep = "\t" , dec = ",")
head(donnees_sombre_pool)

# Quantité absolu de observation à l'obscurité  

# Cette fonction permet de calculer les moyennes pour tout les métabolites d'une condition d'un même pool dans notre cas sombre ou lumineux.Cette fonction prend en argument 1) le tableau avec les valeurs de pools 2) la modalité 

moyen <- function(tableau_pool , modalite) {
  moyenne <- tableau_pool %>%
    filter (condition == modalite) %>%
    group_by(replicate) %>%
    summarise(across(4:(ncol(tableau_pool) -1) , mean))
  return (moyenne)}

# Cette fonction permet de calculer les ecarts-types pour tout les métabolites d'une condition d'un même pool dans notre cas sombre ou lumineux. Cette fonction prend en argument 1) le tableau avec les valeurs de pools 2) la modalité

ecart <- function (tableau_pool , modalite) {
  ecart_type <- tableau_pool %>%
    filter (condition == modalite) %>%
    group_by(replicate) %>%
    summarise(across(4:(ncol(tableau_pool)-1) , sd))
  return (ecart_type)}

# Exemple d'utilisation des fonctions
ecart(donnees_sombre_pool , "dark")
moyen(donnees_sombre_pool , "dark")

# Pour écrire les résultats dans des tableaux en format .tsv
# write.table(moyen file="moyenne_metabolite", sep="\t" , row.names = FALSE , quote = FALSE)
# write.table(ecart, file="SD_matabolite", sep="\t" , row.names = FALSE , quote = FALSE)

# Fonction d'écriture d'un fichier .miso. Cette fonction a pour argument 1) tableau avec les valeurs de MS 2) le ou les métabolites de sortie 3) la répétition choisis

miso <- function (tableau , proteine_sortie , repetition) {
  fichier <- tableau %>%
  filter (metabolite %in% proteine_sortie  , replicate %in% repetition ) %>%
    select(ID , Comments ,metabolite, Fragment , Dataset , Isospecies, isotopologue_fraction , SD , time ,isotopologue , sample) %>%
    ungroup() %>%
    select (- isotopologue  , - sample) %>%
    rename (Specie = metabolite , Value = isotopologue_fraction , Time = time ) %>%
    arrange (Specie) 
  write.table(fichier, file="Metabolite.miso", sep="\t" , row.names = FALSE , quote = FALSE)
  return (fichier)
} 

# Ecriture fichier interpolation linéaire lu par .linp. Cette fonction a pour argument 1) tableau avec les valeurs de MS 2) le métabolite d'entrée 3) la répétition choisis

interpolation <- function(tableau , protein_entree , repetition){
  fichier <- tableau %>%
    filter (metabolite %in% protein_entree , replicate %in% repetition) %>%
    select (time ,Isospecies ,isotopologue_fraction , sample , isotopologue) %>%
    pivot_wider(id_cols = time , names_from = Isospecies , values_from = isotopologue_fraction)
  write.table(fichier, file="Metabolite.tsv", sep="\t" , row.names = FALSE , quote = FALSE)
  return(fichier)
}

# Tableau complet

# Cette fonction est adapté à la sortie MS et au fichier d'entrée du logiciel Influx
# Cette fonction prend en argument : 1) condition souhaité 2) réplicat 3) métabolite entrée 4 ou +) métabolite sortie(s)  

tableau_meta <- function (...) {
  metabol <- c(...)
  modalite <- metabol[1]
  meta_depart <- metabol[3]
  meta_arrive <- metabol[4:length(metabol)]
  repli <- metabol[2]
  tableau <- donnees_sombre %>%
    filter (condition %in% modalite , metabolite %in% metabol,  replicate %in% repli) %>%
    select(time, replicate , metabolite , isotopologue , isotopologue_fraction , sample) %>%
    mutate ( Fragment = max (isotopologue)) %>%
    group_by(isotopologue, time , sample) %>%
    mutate(Isospecies = paste("M" , isotopologue, sep = "") , ID = "" , Comments = "" , Dataset = "MS-1" , SD = 0.001)
  fichier_inter <- interpolation(tableau , meta_depart , repli)
  fichier_miso <- miso (tableau , meta_arrive , repli)
  return(tableau)
}

# Exemple d'utilisation 

tableau_meta("dark","A", "SuccinateC1C4" , "MalateC1C4" , "MalateC2C4" )
