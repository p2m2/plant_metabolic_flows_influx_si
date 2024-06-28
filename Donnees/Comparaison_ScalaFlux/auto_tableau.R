# Transformation de tableau 
library("readxl") # Package permettant de lire les excels 
library("tidyr") # Package permettant d'acceder à dplyr
library ("dplyr") # Package permettant de remanier le jeu de données 

donnees_lab <- read_excel ("Scalaflux_labelling.xlsx")
donnees_poo <- read_excel ("Scalaflux_poolsize.xlsx")
# head(donnees_poo)
  


df <- donnees_poo
head(df)

#GLUVAL
df1 <- subset.data.frame(df, select=c(leaf,rep,time, Valine))
df1$leaf <- factor(df1$leaf)
df1$rep <- factor(df1$rep)
MEAN <- ddply(df1, .(leaf, rep), summarise, mean=mean(Valine))
MEAN

write.table(MEAN, file="MEAN_pool_size.tsv", sep="\t")                     
                         
df10 <- donnees_lab
df10 <- gather(df10, metabolite_isotopologue, value, 4:13)
df10 <- separate(df10, metabolite_isotopologue, c("metabolite", "isotopologue"))

aa <- "Val"
aa1 <- paste(aa ,"M1" , sep = "_")
aa1
aa0 <- paste(aa ,"M0" , sep = "_")
aa0
aam <- paste (aa , "miso" , sep = ".")
aam

#GLUVAL miso

# Début de fonctions 

miso <- function(aa , nbr_mol_marquer , data ) {
  aa1 <- paste(aa ,"M1" , sep = "_")
  aa0 <- paste(aa ,"M0" , sep = "_")
  aam <- paste (aa , "miso" , sep = ".")
  fichiers <- donnees_lab %>%
    select(time , starts_with(aa))%>%
    pivot_longer(cols = c( aa1, aa0) , names_to = "Species_Isospecies" , values_to = "Value") %>%
    separate ( col = "Species_Isospecies" , into = c("Specie" , "Isospecies" ) , sep = "_") %>%
    mutate ( ID = "", Comments = "" , Fragments = nbr_mol_marquer , Dataset = data , SD = max(Value ,Specie != "M0")/10)
  fichiers <- fichiers[, c("ID" , "Comments" , "Specie", "Fragments", "Dataset" , "Isospecies" , "Value" , "SD" , "time" )]
  write.table(fichiers, file= aam, sep="\t")
  return fichiers
  
}

resultat <- miso( "Val" , 1 , "MS-1")
print(resultat)

# Code qui fonctionne 

donnes_glu_val <- donnees_lab %>%
  select(time , starts_with("Val"))%>%
  pivot_longer(cols = c("Val_M1" , "Val_M0") , names_to = "Species_Isospecies" , values_to = "Value") %>%
  separate ( col = "Species_Isospecies" , into = c("Specie" , "Isospecies" ) , sep = "_") %>%
  mutate ( ID = "", Comments = "" , Fragments = 1 , Dataset = "MS-1" , SD = max(Value , Specie != "M0")/10)
donnes_glu_val <- donnes_glu_val[, c("ID" , "Comments" , "Specie", "Fragments", "Dataset" , "Isospecies" , "Value" , "SD" , "time" )]
write.table(donnes_glu_val, file="Valine.miso", sep="\t") 
donnes_glu_val

#GLUVAL interpolation lineaire.tsv  

donnees_glu_val_i <- donnees_lab %>%
  select(time , starts_with("Glu"))
colnames(donnees_glu_val_i) <- c("Time", "M1", "M0")
donnees_glu_val_i <- donnees_glu_val_i[, c("Time", "M0", "M1"  )]
write.table(donnees_glu_val_i, file="Valine.tsv", sep="\t")
donnees_glu_val_i


            