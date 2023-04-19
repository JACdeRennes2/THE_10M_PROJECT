library(sf)
#install.packages("countrycode")
library(countrycode)
library(ggmap)
library("RColorBrewer")
library("classInt")
library(mclust)
library(gridExtra)
library(FactoMineR)
library(ggplot2)

# Importation du fichier CSV
# Modification de l'encoding du fichier ANSI -> utf-8
donnees <-read.csv("data/Pays_donnees.csv", header = TRUE, sep = ",", encoding = "UTF-8")
# Supression des valeurs manquantes
donnees <- na.omit(donnees)

# Représentations illustratives

# Création du graphique à boîtes

plot1 <- ggplot(donnees, aes(x = "", y = inflation)) +
  geom_boxplot(fill = "#1C3EC7", color = "black") +
  ggtitle("Inflation par pays") +
  ylab("Inflation") +
  theme_bw() + theme(plot.title = element_text(hjust = 0.5, face = 'bold'))

plot2 <- ggplot(donnees, aes(x = "", y = pib_h)) +
  geom_boxplot(fill = "#BD1818DA", color = "black") +
  ggtitle("PIB par habitant par pays") +
  ylab("PIB par habitant") +
  theme_bw() + theme(plot.title = element_text(hjust = 0.5, face = 'bold'))

grid.arrange(plot1, plot2, ncol = 2)
dev.off()

# Sélection des variables quantitatives
donnees_quant <- donnees[, c(2:10)]
# Centrer et réduire les données
donnees_centrees <- scale(donnees_quant, center = TRUE, scale = TRUE)

# Statistiques descriptives
summary(donnees_centrees)
pairs(donnees_centrees, main = "Matrice de corrélation")

# ACP
PCA(donnees_centrees)
pca <- prcomp(donnees_centrees, scale. = TRUE)
PC1 <- pca$x[,1]
PC2 <-pca$x[,2]
plot(pca)
summary(pca)
# Afficher les charges des variables sur les deux premiers axes
print(pca$rotation[, 1:2])


#CAH
par(mfrow = c(1, 2))
eucl <- dist(donnees_centrees) ^ 2
dend <- hclust(eucl, method = "ward.D2")
# Plot le dendrogramme avec les titres
plot(
  dend,
  main = "Dendrogramme hiérarchique",
  sub = "Méthode de Ward",
  xlab = "Observations",
  ylab = "Distance"
)

plot(
  rev(hclust(eucl, method = "ward.D2")$height),
  type = 'b',
  main = "Graphique de coupe du dendrogramme",
  xlab = "Nombre de groupes",
  ylab = "Hauteur du saut"
)

# K-means clustering avec 7 clusters
set.seed(123)
kmeans_clusters <- kmeans(pca$x[, 1:2], centers = 7)
table(kmeans_clusters$cluster)

# Ajouter les clusters aux données d'origine
donnees$cluster <- as.factor(kmeans_clusters$cluster)

# Créer un graphique en nuage de points coloré par cluster
ggplot(donnees, aes(x = PC1, y = PC2, group = cluster, color = factor(cluster))) +
  geom_point(size = 3) +
  scale_colour_manual(values=c("#FFE0CC", "#FFC7B6", "#FFB5B8", "#F2B2CC", "#E5B5F7", "#B5CCE5", "#B5E5D6")) +
  labs(title = "ACP avec clustering K-means (7 clusters)",
       x = "Première composante principale",
       y = "Deuxième composante principale") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold")) + theme_minimal()


# Comment répartir les 10 M?

# Calcul des besoins moyens par cluster
besoins_cluster <- aggregate(donnees[, 2:10], by = list(donnees$cluster), FUN = mean)
# Ajout de la colonne "besoins" pour chaque pays
donnees$besoins <- NA
for (i in 1:nrow(donnees)) {
  donnees[i, "besoins"] <-
    besoins_cluster[donnees[i, "cluster"], names(besoins_cluster) %in% names(donnees)[2:10]]
}
# Répartition des fonds
donnees$montant_alloue <- donnees$besoins / sum(donnees$besoins) * 10000000




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
  countries_polygons[order(countries_polygons$name),]
countries_polygons <-
  countries_polygons[-c(11, 45, 125, 129, 149, 160, 156),]
pays_carte <- donnees$pays[-c(39, 93, 139, 142, 148, 159)]
montants <- donnees$montant_alloue[-c(39, 93, 139, 142, 148, 159)]
countries_polygons$pays <- pays_carte
countries_polygons$montant_alloue <- round(montants)


# Déterminer les classes
nb_coupures <- 6
classes <-
  classIntervals(donnees$montant_alloue, nb_coupures, style = "kmeans")

# Utiliser les couleurs de RColorBrewer

palette_couleurs <- brewer.pal(7, "YlOrRd")
couleurs <- rep(NA, length(countries_polygons$montant_alloue))

for (i in seq_along(countries_polygons$montant_alloue)) {
  montant_alloue <- countries_polygons$montant_alloue[i]
  couleurs[i] <-
    palette_couleurs[match(montant_alloue, unique(countries_polygons$montant_alloue))]
}


#Graphique avec un ggplot

# Convertir la dataframe en sf
countries_sf <- st_as_sf(countries_polygons)
couleurs_cluster <- c("#FFE0CC", "#FFC7B6", "#FFB5B8", "#F2B2CC", "#E5B5F7", "#B5CCE5", "#B5E5D6")
pays_communs <- intersect(countries_sf$pays, donnees$pays)

################################################################################
## Carte coloriée en fonction des clusters #####################################

ggplot() +
  geom_sf(data = countries_sf[countries_sf$pays %in% pays_communs, ],
          aes(fill = as.factor(donnees$cluster[donnees$pays %in% pays_communs]))) +
  scale_fill_manual(values = couleurs_cluster,
                    na.value = "grey50",
                    name = "Cluster") +
  geom_sf_text(
    data = countries_sf[countries_sf$pays %in% pays_communs, ],
    aes(label = paste(donnees$pays[donnees$pays %in% pays_communs])),
    size = 2,
    color = "black",
    check_overlap = TRUE,
    nudge_y = 0.5
  ) +
  labs(title = "Carte du monde de la répartition du budget") +
  theme_minimal() +
  theme(plot.title = element_text(size = 20, hjust = 0.5))

################################################################################
## Carte coloriée en fonction du montant alloué ################################

ggplot() +
  geom_sf(data = countries_sf[countries_sf$pays %in% pays_communs, ],
          aes(fill = montant_alloue)) +
  scale_fill_continuous(high="#BA2525", 
                        low="#FFF7BD") +
  geom_sf_text(
    data = countries_sf[countries_sf$pays %in% pays_communs, ],
    aes(label = paste(donnees$pays[donnees$pays %in% pays_communs])),
    size = 2,
    color = "black",
    check_overlap = TRUE,
    nudge_y = 0.5
  ) +
  labs(title = "Carte du monde de la répartition du budget") +
  theme_minimal() +
  theme(plot.title = element_text(size = 20, hjust = 0.5))

