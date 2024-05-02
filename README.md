# Modélisation des flux métaboliques chez les végétaux avec Influx

#C’est un logiciel qui permet de modéliser des flux métaboliques instationnaires.



# Installation:
#Il faut tout d’abord créer un nouvel environnement, afin d’isoler et d’assurer une fiabilité au résultat. Pour se faire on utilise conda qui est un gestionnaire d'environnements et de packages:
conda create --name FLUX
#On installe ce logiciel sur l'environnement qu’on vient de créer : 
conda install influx_si -c conda-forge -c bioconda
#On s’assure du bon fonctionnement du logiciel en effectuant les tests proposés, ces tests sont des données issues de l’article https://www.ncbi.nlm.nih.gov/pmc/articles/PMC8021400/. Le logiciel est composé de deux branches influx_s pour les flux stationnaires et influx_i des flux instationnaires. Nous n'utiliserons et détaillerons que influx_i. 



# Fichiers d’entrées:
#Pour fonctionner, le logiciel a besoin de plusieurs fichiers d’entrées. Ces fichiers doivent avoir le même préfixe mais aussi ces fichiers possèdent des nomenclatures spécifiques (tabulations et espaces) ,  Si ID et Comment sont vides mettre une tabulation, :

# .netw : ou network permet d’indiquer au logiciel les voies à modéliser. Le nombre d’atome suivi doit être équilibré.
#nom_flux:(tab)métabolite_entrée (atome_suivi) -> métabolite_sortie (atome_suivi)

# .miso : ou mesures permet d’indiquer les mesures des métabolites de transitions ou de sorties. Species est le métabolite de sortie, Fragment est le nombre d’atome suivi, Dataset est issue de la même analyse de spectrométrie de masse, Isospecies est l’isotopomère, et SD est le pourcentage d’erreur autoriser 
#Id(tab)Comment(tab)Specie(tab)Fragment(tab)Dataset(tab)Isospecies(tab)Value(tab)SD(tab)Time 

# .opt : ou optimisation permet d’indiquer les paramètres d’optimisations pour la modélisation.
#Id(tab)Comment(tab)Name(tab)Value

# .tvar : permet d’indiquer la nature des flux. Name indique le nom du flux (.netw), Kind indique les flux contraint = C, libre = F ou dépendants = D mais aussi les flux net = NET , les flux d’échanges = XCH et la concentrations des espèces METAB avec pour valeur la moyenne d’un pool. 
#Id(tab)Comment(tab)Name(tab)Kind(tab)Type(tab)Value

# .linp : permet d’indiquer les mesures des métabolites d’entrée. Value permet de mettre une méthode sur les flux d’entrées par exemple linterp qui permet de mettre une fonction linéaire continue sur les métabolismes d’entrée.
#Id(tab)Metabolite(tab)Isotopomer(tab)Value(tab)Comment
#Lorsque le .linp utilise une méthode de lecture, il faut créer d’autre fichier : 

# .tsv : permet d’indiquer les mesures de métabolites d’entrées. M0 et M1 sont les isotopomères.
#Time(tab)M0(tab)M1

# .funlab.R : permet de lire le fichier .tsv 
#df=read.table(file.path(dirw,"préfixe.tsv"), header=TRUE)



# Conseils: 
#Pour vérifier si la nomenclature des fichiers on peut utiliser notepad avec l option Affichage/Symbole spéciaux/Afficher les espaces et tabulations. Cela permet une meilleur visibilité des espaces et tabulations qui sont primordiales pour la bonne lecture des différents fichiers.
#Quand les données d'enrichissement sont trop faibles et que les mesures ne sont pas assez nombreuses il faut modifier les paramètres d’optimisation (.opt) comme le subdt ou les paramètres de mesures (.miso) comme le sd.
#Pour plus de détails sur la création des fichiers : https://influx-si.readthedocs.io/en/latest/index.html



# Exemple:
#Modélisation des flux du glutamate vers la valine et de l’aspartate vers la thréonine  https://www.mdpi.com/2218-1989/10/4/150

