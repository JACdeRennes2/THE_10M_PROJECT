library(sf)
#install.packages("countrycode")
library(countrycode)
library(ggmap)
library("RColorBrewer")
library("classInt")

# Importer les données à partir du fichier CSV
# Modification de l'encoding du fichier ANSI -> utf-8
donnees <- read.csv("data/Pays_donnees.csv", header = TRUE, sep = ",", encoding = "UTF-8")
# Supprimer les valeurs manquantes
donnees <- na.omit(donnees)
# Normaliser les variables si nécessaire
donnees_norm <- scale(donnees[2:9])


# Statistiques descriptives
summary(donnees_norm)
# Graphiques
pairs(donnees_norm, main="Matrice de corrélation")

# Analyse en composantes principales (ACP)
pca <- prcomp(donnees_norm, scale = TRUE)
summary(pca)

eucl <- dist(donnees_norm)^2
plot(hclust(eucl, method="ward.D2"))
plot(rev(hclust(eucl, method="ward.D2")$height), type='b')

# K-means clustering avec 3 clusters
set.seed(123)
kmeans_clusters <- kmeans(pca$x[,1:3], centers = 4, nstart = 25)
table(kmeans_clusters$cluster)

# Ajouter les clusters aux données d'origine
donnees$cluster <- as.factor(kmeans_clusters$cluster)
# Identifier les pays avec les plus grands besoins dans chaque cluster
besoins <- aggregate(donnees[,2:9], by = list(donnees$cluster), FUN = mean)
besoins <- besoins[,2:9]
rownames(besoins) <- c("Cluster 1", "Cluster 2", "Cluster 3", "Cluster 4")
besoins

# Recommandations pour chaque cluster
recommandations <- list()
recommandations[[1]] <- c("Améliorer la santé maternelle et infantile", "Investir dans l'éducation", "Développer les infrastructures")
recommandations[[2]] <- c("Stimuler la croissance économique", "Promouvoir l'entrepreneuriat", "Encourager l'investissement étranger")
recommandations[[3]] <- c("Lutter contre la pauvreté", "Améliorer l'accès aux soins de santé", "Investir dans l'agriculture")
recommandations


# Comment répartir les 10 M?

# Calcul des besoins moyens par cluster
besoins_cluster <- aggregate(donnees[,2:9], by = list(donnees$cluster), FUN = mean)
# Ajout de la colonne "besoins" pour chaque pays
donnees$besoins <- NA
for(i in 1:nrow(donnees)) {
  donnees[i,"besoins"] <- besoins_cluster[donnees[i,"cluster"], names(besoins_cluster) %in% names(donnees)[2:9]]
}
# Répartition des fonds
donnees$montant_alloue <- donnees$besoins / sum(donnees$besoins) * 10000000


################################################################################
################################################################################
## Partie polygones ############################################################
################################################################################

countries_code <- countrycode(donnees$pays, origin = 'country.name', destination = 'iso3c')

world_polygons <- st_read("data/world-administrative-boundaries.shp")
countries_polygons <- world_polygons[which(world_polygons$iso3 %in% countries_code), c(1, 4, 9)]
countries_polygons <- countries_polygons[order(countries_polygons$name),]
countries_polygons <- countries_polygons[-c(11, 45, 125, 129, 149, 160, 156),]
pays_carte <- donnees$pays[-c(39, 93, 139, 142, 148, 159)]
montants <- donnees$montant_alloue[-c(39, 93, 139, 142, 148, 159)]
countries_polygons$pays <- pays_carte
countries_polygons$montant_alloue <- round(montants)


plot(countries_polygons$geometry,
     main="Repartition des dépenses")

palette_couleurs <- brewer.pal(4, "YlOrRd")
couleurs <- ifelse(countries_polygons$montant_alloue == 6467, palette_couleurs[1],
                   ifelse(countries_polygons$montant_alloue == 13416, palette_couleurs[2],
                          ifelse(countries_polygons$montant_alloue == 45238, palette_couleurs[3],
                                 palette_couleurs[4])))

plot(countries_polygons["montant_alloue"], 
     main="Taux de chomage dans l'agglomération Rennaise",
     col=couleurs,
     graticule = st_crs(4326)
)
legend("bottomleft",
       legend = formatC(classes$brks),
       fill = palette_couleurs,
       title = "Montant alloué",
       title.col = "black",
       cex = 0.6,
       bty = "n",
       horiz = FALSE)

################################################################################
################################################################################
