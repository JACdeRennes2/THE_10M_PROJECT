library(sf)
#install.packages("countrycode")
library(countrycode)
library(ggmap)
library("RColorBrewer")
library("classInt")

# Importer les données à partir du fichier CSV
# Modification de l'encoding du fichier ANSI -> utf-8
donnees <-
  read.csv(
    "data/Pays_donnees.csv",
    header = TRUE,
    sep = ",",
    encoding = "UTF-8"
  )
# Supprimer les valeurs manquantes
donnees <- na.omit(donnees)


# Statistiques descriptives
summary(donnees_norm)
# Graphiques
pairs(donnees_norm, main = "Matrice de corrélation")

# Sélection des variables quantitatives
donnees_quant <- donnees[, c(2:10)]
# Centrer et réduire les données
donnees_centrees <- scale(donnees_quant, center = TRUE, scale = TRUE)
# Appliquer l'ACP
pca <- prcomp(donnees_centrees, scale. = TRUE)
# Visualiser le graphique des valeurs propres
plot(pca)
# Interpréter les composantes principales
summary(pca)
# Afficher les charges des variables sur les deux premiers axes
print(pca$rotation[,1:2])


eucl <- dist(donnees_norm) ^ 2
plot(hclust(eucl, method = "ward.D2"))
plot(rev(hclust(eucl, method = "ward.D2")$height), type = 'b')


# K-means clustering avec 4 clusters
set.seed(123)
kmeans_clusters <- kmeans(pca$x[, 1:2], centers = 4)
table(kmeans_clusters$cluster)

# Ajouter les clusters aux données d'origine
donnees$cluster <- as.factor(kmeans_clusters$cluster)


# Comment répartir les 10 M?

# Calcul des besoins moyens par cluster
besoins_cluster <-
  aggregate(donnees[, 2:10], by = list(donnees$cluster), FUN = mean)
# Ajout de la colonne "besoins" pour chaque pays
donnees$besoins <- NA
for (i in 1:nrow(donnees)) {
  donnees[i, "besoins"] <-
    besoins_cluster[donnees[i, "cluster"], names(besoins_cluster) %in% names(donnees)[2:10]]
}
# Répartition des fonds
donnees$montant_alloue <-
  donnees$besoins / sum(donnees$besoins) * 10000000


################################################################################
################################################################################
## Partie polygones ############################################################
################################################################################

countries_code <-
  countrycode(donnees$pays, origin = 'country.name', destination = 'iso3c')

world_polygons <-
  st_read("data/world-administrative-boundaries.shp")
countries_polygons <-
  world_polygons[which(world_polygons$iso3 %in% countries_code), c(1, 4, 9)]
countries_polygons <-
  countries_polygons[order(countries_polygons$name), ]
countries_polygons <-
  countries_polygons[-c(11, 45, 125, 129, 149, 160, 156), ]
pays_carte <- donnees$pays[-c(39, 93, 139, 142, 148, 159)]
montants <- donnees$montant_alloue[-c(39, 93, 139, 142, 148, 159)]
countries_polygons$pays <- pays_carte
countries_polygons$montant_alloue <- round(montants)




plot(countries_polygons$geometry,
     main = "Repartition des dépenses")

# Déterminer les classes
nb_coupures <- 3
classes <-
  classIntervals(donnees$montant_alloue, nb_coupures, style = "kmeans")

# Utiliser les couleurs de RColorBrewer
palette_couleurs <- brewer.pal(4, "YlOrRd")
couleurs <-
  ifelse(
    countries_polygons$montant_alloue == 6467,
    palette_couleurs[1],
    ifelse(
      countries_polygons$montant_alloue == 13416,
      palette_couleurs[2],
      ifelse(
        countries_polygons$montant_alloue == 45238,
        palette_couleurs[3],
        palette_couleurs[4]
      )
    )
  )

# Dessiner la carte avec les couleurs de chaque pays
plot(
  countries_polygons["montant_alloue"],
  main = "Carte du monde de la répartition du budget",
  col = couleurs,
  graticule = st_crs(4326)
)


legend(
  "topright",
  legend = formatC(classes$brks),
  fill = palette_couleurs,
  title = "Montant alloué",
  title.col = "black",
  cex = 0.6,
  bty = "n",
  horiz = FALSE,
  box.lwd = 1,
  box.col = "black",
  bg = "white"
)


#Graphique avec un ggplot
library(ggplot2)

# Convertir la dataframe en sf
countries_sf <- st_as_sf(countries_polygons)

# Dessiner la carte avec les couleurs de chaque pays et les étiquettes
ggplot() +
  geom_sf(data = countries_sf, aes(fill = montant_alloue)) +
  scale_fill_gradient(low = "yellow",
                      high = "red",
                      name = "Montant alloué €") +
  geom_sf_text(
    data = countries_sf,
    aes(label = paste(pays)),
    size = 2,
    color = "black",
    check_overlap = TRUE,
    nudge_y = 0.5
  ) +
  labs(title = "Carte du monde de la répartition du budget") +
  theme_minimal() + theme(plot.title = element_text(size = 20, hjust = 0.5))
################################################################################
################################################################################
