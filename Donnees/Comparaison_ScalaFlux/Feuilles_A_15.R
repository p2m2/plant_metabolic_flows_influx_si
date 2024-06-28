# Transformation de tableau 
library("readxl") # Package permettant de lire les excels 
library("tidyr") # Package permettant d'acceder à dplyr
library ("dplyr") # Package permettant de remanier le jeu de données
# Ne surtout pas installer plyr il inactive certaines fonction de dyplr comme group by 

donnees_lab <- read_excel ("Scalaflux_labelling.xlsx")
donnees_poo <- read_excel ("Scalaflux_poolsize.xlsx")

# Moyenne feuille Threonine 

moyenne <- donnees_poo %>%
  group_by(leaf , rep ) %>%
  summarise(moyenne = mean(Threonine)) 
moyenne
write.table(moyenne, file="Moyenne", sep="\t" , row.names = FALSE , quote = FALSE) 

# feuille B L15 miso ,

donnes_glu_val <- donnees_lab %>%
  filter (leaf == "L3" , rep == "A") %>% 
  select(time , starts_with("Thr"))%>%
  pivot_longer(cols = c("Thr_M1" , "Thr_M0") , names_to = "Species_Isospecies" , values_to = "Value") %>%
  separate ( col = "Species_Isospecies" , into = c("Specie" , "Isospecies" ) , sep = "_") %>%
  mutate ( ID = "", Comments = "" , Fragment = 1 , Dataset = "MS-1" , SD = 0.01) %>%
  arrange (time , Isospecies)
donnes_glu_val <- donnes_glu_val[, c("ID" , "Comments" , "Specie", "Fragment", "Dataset" , "Isospecies" , "Value" , "SD" , "time" )]
colnames(donnes_glu_val) <- c("ID" , "Comments" , "Specie", "Fragment", "Dataset" , "Isospecies" , "Value" , "SD" ,"Time")
write.table(donnes_glu_val, file="Threoniner_A_3.miso", sep="\t" , row.names = FALSE , quote = FALSE) 
donnes_glu_val


moyenne <- mean (donnes_glu_val[donnes_glu_val$Isospecies != "M0" ,]$Value)

#GLUVAL interpolation lineaire.tsv  

donnees_glu_val_i <- donnees_lab %>%
  filter (leaf == "L3" , rep == "A") %>%
  select(time , starts_with("Asp"))
colnames(donnees_glu_val_i) <- c("Time", "M1", "M0")
donnees_glu_val_i <- donnees_glu_val_i[, c("Time", "M0", "M1"  )]
write.table(donnees_glu_val_i, file="Threoniner_A_3.tsv", sep="\t" , row.names = FALSE , quote = FALSE)
donnees_glu_val_i

variable0 <- approx(df$time , df$M0)
variable1 <- approx(df$time , df$M1)
variable2 <- approx(df$time , df$M2)
variable3 <- approx(df$time , df$M3)
variable4 <- approx(df$time , df$M4)


plot (variable0$x , variable0$y)
plot (variable1$x , variable1$y)
plot (variable2$x , variable2$y)
plot (variable3$x , variable3$y)
plot (variable4$x , variable4$y)
