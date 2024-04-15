# Installation 

# Création d'un environement FLUX 
conda create --name FLUX 
conda activate FLUX

# Sur vcode avec miniconda en local dans l'environnement FLUX
conda install influx_si -c conda-forge -c bioconda 

# Test pour vérifier que le logiciel est bien installé 
influx_s.py --copy_test
cd test/mtf  # création d'un dossier pour tester l'installation de influx 
influx_s.py --prefix e_coli 
diff e_coli_res/e_coli.tvar.sim ../ok/mtf/e_coli_res/e_coli.tvar.sim # permets de comparer les différences entre les données d'un dossier et celle simulé
influx_s.py --prefix e_coli --prefix e_coli_growth # permets de mettre en parallèles plusieurs simulations 
influx_i.py --prefix e_coli_i # permet d'effectuer des simulations plus vite

# Le logiciels influx va prendre les fichiers avec le prefixe "e_coli" afin de simuler les flux.
# Le logiciel influx nous crée comme sortie un dossier avec :
# pdf récapitulatif avec les mesures de MS mesuré et les MS simulé afin de comparer si la simulations est cohérente
# un dossier tmp
# fichier erreur qui indique l'erreur qui s'est produite 
# les fichiers avec les mêmes extensions que les fichiers d'entrées mais avec une extentions .sim pour les données simulées

# Aide pour les fonctions 
# influx_s.py --copy_doc celle ci ne fonctionne pas 
influx_s.py --help
influx_i.py --help