# Importer les données à partir du fichier CSV
# Modification de l'encoding du fichier ANSI -> utf-8
donnees <- read.csv("data/Pays_donnees.csv", header = TRUE, sep = ",")
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

# K-means clustering avec 3 clusters
set.seed(123)
kmeans_clusters <- kmeans(pca$x[,1:3], centers = 3, nstart = 25)
table(kmeans_clusters$cluster)

# Ajouter les clusters aux données d'origine
donnees$cluster <- as.factor(kmeans_clusters$cluster)
# Identifier les pays avec les plus grands besoins dans chaque cluster
besoins <- aggregate(donnees[,2:9], by = list(donnees$cluster), FUN = mean)
besoins <- besoins[,2:9]
rownames(besoins) <- c("Cluster 1", "Cluster 2", "Cluster 3")
besoins

# Recommandations pour chaque cluster
recommandations <- list()
recommandations[[1]] <- c("Améliorer la santé maternelle et infantile", "Investir dans l'éducation", "Développer les infrastructures")
recommandations[[2]] <- c("Stimuler la croissance économique", "Promouvoir l'entrepreneuriat", "Encourager l'investissement étranger")
recommandations[[3]] <- c("Lutter contre la pauvreté", "Améliorer l'accès aux soins de santé", "Investir dans l'agriculture")
recommandations
