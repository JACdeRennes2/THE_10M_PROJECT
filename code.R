library(sf)
#install.packages("countrycode")
library(countrycode)
library(ggmap)
library("RColorBrewer")
library("classInt")
library(mclust)
library(gridExtra)
library(FactoMineR)
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

#représentation illustrative

# Création du graphique à boîtes

plot1 <- ggplot(donnees, aes(x = "", y = inflation)) +
  geom_boxplot(fill = "#1C3EC7", color = "black") +
  ggtitle("Inflation par pays") +
  ylab("Inflation") +
  theme_bw()+theme(plot.title = element_text(hjust = 0.5, face = 'bold'))

plot2 <- ggplot(donnees, aes(x = "", y = pib_h)) +
  geom_boxplot(fill = "#BD1818DA", color = "black") +
  ggtitle("PIB par habitant par pays") +
  ylab("PIB par habitant") +
  theme_bw()+theme(plot.title = element_text(hjust = 0.5, face = 'bold'))

grid.arrange(plot1, plot2, ncol = 2)
dev.off()

# Sélection des variables quantitatives
donnees_quant <- donnees[, c(2:10)]
# Centrer et réduire les données
donnees_centrees <- scale(donnees_quant, center = TRUE, scale = TRUE)


# Statistiques descriptives
summary(donnees_centrees)
# Graphiques
pairs(donnees_centrees, main = "Matrice de corrélation")

# Appliquer l'ACP
pca <- prcomp(donnees_centrees, scale. = TRUE)
acp = PCA(donnees_norm)
# Visualiser le graphique des valeurs propres
plot(pca)
# Interpréter les composantes principales
summary(pca)
# Afficher les charges des variables sur les deux premiers axes
print(pca$rotation[,1:2])


eucl <- dist(donnees_centrees) ^ 2
dend <- hclust(eucl, method = "ward.D2")
# Plot le dendrogramme avec les titres
plot(dend, main = "Dendrogramme hiérarchique",
     sub = "Méthode de Ward", xlab = "Observations", ylab = "Distance")


plot(rev(hclust(eucl, method = "ward.D2")$height), type = 'b',
     main = "Graphique de coupe du dendrogramme",
     xlab = "Nombre de groupes",
     ylab = "Hauteur du saut")


model <- Mclust(donnees_centrees)
plot(model, what = "BIC")

# Get the number of clusters with the lowest BIC value
best_cluster <- which.min(model$BIC)
cat("The optimal number of clusters is:", best_cluster)



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
palette_couleurs <- brewer.pal(best_cluster, "YlOrRd")
couleurs <- rep(NA, length(countries_polygons$montant_alloue))

for (i in seq_along(countries_polygons$montant_alloue)) {
  montant_alloue <- countries_polygons$montant_alloue[i]
  couleurs[i] <- palette_couleurs[match(montant_alloue, unique(countries_polygons$montant_alloue))]
}


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
  scale_fill_gradient(low = "#FFFF54",
                      high = "red",
                      na.value = "grey50",
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
