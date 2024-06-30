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
ecart(donnees_sombre_pool , "light")
moyen(donnees_sombre_pool , "light")

# Pour écrire les résultats dans des tableaux en format .tsv
# write.table(moyen file="moyenne_metabolite", sep="\t" , row.names = FALSE , quote = FALSE)
# write.table(ecart, file="SD_matabolite", sep="\t" , row.names = FALSE , quote = FALSE)

# Fonction d'écriture d'un fichier .miso. Cette fonction a pour argument 1) tableau avec les valeurs de MS 2) le ou les métabolites de sortie 3) la répétition choisis

miso <- function (tableau , proteine_sortie) {
  fichier <- tableau %>%
  filter (metabolite %in% proteine_sortie ) %>%
    select(ID , Comments ,metabolite, Fragment , Dataset , Isospecies, isotopologue_fraction , SD , time ,isotopologue , sample) %>%
    ungroup() %>%
    select (- isotopologue  , - sample) %>%
    rename (Specie = metabolite , Value = isotopologue_fraction , Time = time ) %>%
    arrange (Specie) 
  write.table(fichier, file="Metabolite.miso", sep="\t" , row.names = FALSE , quote = FALSE)
  return (fichier)
} 

miso <- function (tableau , proteine_sortie) {
  fichier <- tableau %>%
    filter (metabolite %in% proteine_sortie ) %>%
    select(ID , Comments ,metabolite, Fragment , Dataset , Isospecies, isotopologue_fraction , SD , Time ,isotopologue , sample) %>%
    ungroup() %>%
    select (- isotopologue  , - sample) %>%
    rename (Specie = metabolite , Value = isotopologue_fraction , Time = Time ) %>%
    arrange (Specie) 
  write.table(fichier, file="Metabolite.miso", sep="\t" , row.names = FALSE , quote = FALSE)
  return (fichier)
}

print(miso (tableau_sombre , "GlutamateC2C5") , n = 30)
print(miso (tableau_sombre , "SuccinateC1C4") , n = 30)

# Ecriture fichier interpolation linéaire lu par .linp. Cette fonction a pour argument 1) tableau avec les valeurs de MS 2) le métabolite d'entrée 3) la répétition choisis

interpolation <- function(tableau , protein_entree , enrichissement){
  fichier <- tableau %>%
    filter (metabolite %in% protein_entree) %>%
    select (time ,Isospecies ,isotopologue_fraction , sample , isotopologue) %>%
    pivot_wider(id_cols = time , names_from = Isospecies , values_from = isotopologue_fraction) %>%
    mutate(M_0 = (M0 - (1-enrichissement))/ enrichissement , M_1 = M1 / enrichissement , M_2 = M2 /enrichissement , M_3 = M3 / enrichissement , M_4 = M4 / enrichissement , M_5 = M5 /enrichissement , M_6 = M6 /enrichissement) %>%
    select ( time , M_0 , M_1 , M_2 , M_3 , M_4 , M_5, M_6) %>%
    rename (M0 = M_0 , M1 = M_1 , M2 = M_2 , M3 = M_3 , M4 = M_4 , M5 = M_5 , M6 = M_6 , Time = time ) %>%
    arrange(Time)
  write.table(fichier, file="Metabolite.tsv", sep="\t" , row.names = FALSE , quote = FALSE)
  return(fichier)
}

interpolation(tableau_sombre , "CitrateC1C6" ,1 )
interpolation <- function(tableau , protein_entree , enrichissement){
  fichier <- tableau %>%
    filter (metabolite %in% protein_entree) %>%
    select (Time ,Isospecies ,isotopologue_fraction , sample , isotopologue) %>%
    pivot_wider(id_cols = Time , names_from = Isospecies , values_from = isotopologue_fraction) %>%
    mutate(M_0 = (M0 - (1-enrichissement))/ enrichissement , M_1 = M1 / enrichissement , M_2 = M2 /enrichissement , M_3 = M3 / enrichissement , M_4 = M4 / enrichissement , M_5 = M5 /enrichissement , M_6 = M6 /enrichissement) %>%
    select ( Time , M_0 , M_1 , M_2 , M_3 , M_4 , M_5, M_6) %>%
    rename (M0 = M_0 , M1 = M_1 , M2 = M_2 , M3 = M_3 , M4 = M_4 , M5 = M_5 , M6 = M_6 , Time = Time ) %>%
    arrange(Time)
  write.table(fichier, file="Metabolite.tsv", sep="\t" , row.names = FALSE , quote = FALSE)
  return(fichier)
}

interpolation(tableau_sombre , "CitrateC1C6" ,1 )

# Tableau complet

# Cette fonction est adapté à la sortie MS et au fichier d'entrée du logiciel Influx
# Cette fonction prend en argument : 1) condition souhaité 2) réplicat 3) pourcentage d'enrichissement 4) métabolite entrée 5 ou +) métabolite sortie(s)  

tableau_meta <- function (...) {
  metabol <- c(...)
  modalite <- metabol[1]
  meta_depart <- metabol[4]
  meta_arrive <- metabol[5:length(metabol)]
  repli <- metabol[2]
  pourcentage_en <- as.numeric (metabol[3])
  tableau <- donnees_sombre %>%
    filter (condition %in% modalite , metabolite %in% metabol,  replicate %in% repli) %>%
    select(time, replicate , metabolite , isotopologue , isotopologue_fraction , sample) %>%
    mutate ( Fragment = max (isotopologue), time = time * 60) %>%
    group_by(isotopologue, time , sample) %>%
    mutate(Isospecies = paste("M" , isotopologue, sep = "") , ID = "" , Comments = "" , Dataset = "MS-1" , SD = 0.001) %>%
    ungroup()
  fichier_inter <- interpolation(tableau , meta_depart , pourcentage_en)
  fichier_miso <- miso (tableau , meta_arrive )
  print(fichier_miso)
  print(fichier_inter)
  return(tableau  )
}

# Exemple d'utilisation 

tableau_meta("dark","A",1, "CitrateC1C6" , "GlutamateC2C5" , "SuccinateC1C4" )

tableau_sombre <- donnees_sombre %>%
  filter (condition == "dark" , replicate == "A") %>%
  filter ( metabolite== "SuccinateC1C4" | metabolite == "CitrateC1C6" | metabolite == "GlutamateC2C5") %>%
  select(time, replicate , metabolite , isotopologue , isotopologue_fraction , sample) %>%
  mutate ( Fragment = paste(1 , max (isotopologue) , sep = "-" ), time = time * 60) %>%
  rename(Time = time) %>%
  group_by(isotopologue, Time ) %>%
  mutate(Isospecies = paste("M" , isotopologue, sep = "") , ID = "" , Comments = "" , Dataset = "MS-1" , SD = 0.001)
#write.table(tableau, file=paste("succinate","dark", sep = "."), sep="\t" , row.names = FALSE , quote = FALSE)

interp <- tableau_sombre %>%
  filter (metabolite == "CitrateC1C6") %>%
  select (Time ,Isospecies ,isotopologue_fraction , sample , isotopologue) %>%
  pivot_wider(id_cols = Time , names_from = Isospecies , values_from = isotopologue_fraction) %>%
  mutate(M_0 = (M0 - (1-0.20))/ 0.20 , M_1 = M1 / 0.20 , M_2 = M2 / 0.20 , M_3 = M3 / 0.20 , M_4 = M4 / 0.20 , M_5 = M5 / 0.20 , M_6 = M6 / 0.20) %>%
  select ( Time , M_0 , M_1 , M_2 , M_3 , M_4 , M_5, M_6) %>%
  rename (M0 = M_0 , M1 = M_1 , M2 = M_2 , M3 = M_3 , M4 = M_4 , M5 = M_5 , M6 = M_6) %>%
  arrange(Time)
interp
write.table(interp, file=paste("SuccinateC1C4_D_sombre","tsv", sep = "."), sep="\t" , row.names = FALSE , quote = FALSE)

mis <- tableau_sombre %>%
  filter (metabolite== "SuccinateC1C4" | metabolite == "GlutamateC2C5") %>%
  select(ID , Comments ,metabolite, Fragment , Dataset , Isospecies, isotopologue_fraction , SD , Time ,isotopologue , sample) %>%
  ungroup() %>%
  select (- isotopologue  , - sample) %>%
  rename (Specie = metabolite , Value = isotopologue_fraction) %>%
  arrange(Specie , Time)
mis
write.table(mis, file=paste("Suc_Glu_A_sombre","miso", sep = "."), sep="\t" , row.names = FALSE , quote = FALSE)
zero <- donnees_sombre %>%
  filter(time == 0 , metabolite == "GlutamateC2C5") %>%
  select(time , metabolite , isotopologue_fraction , isotopologue , replicate)
zero
