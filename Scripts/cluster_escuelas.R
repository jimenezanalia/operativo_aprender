rm(list = ls())
options(scipen=999)

library(arrow)
library(tibble)
library(writexl)
library(factoextra)
library(ggplot2)
library(readr)
library(dplyr)


union21 <- read_csv("datos_indice_equipamiento.csv")
OA2021 <- read_parquet("RA_OA_2021.parquet")

# calculamos la proporcion de migrantes de primaria en las escuelas 


temp1 <- union21 %>% select(id, ch_index)
sapply(temp1, function(x) sum(is.na(x)))

temp <- OA2021 %>% select(ID1, jurisdiccion, sector, ambito, mpuntaje, lpuntaje,
                                NSE_puntaje) %>% 
  group_by(ID1, jurisdiccion, sector, ambito) %>% 
  summarise(avg_mpuntaje = mean(mpuntaje, na.rm = TRUE),
            avg_lpuntaje = mean(lpuntaje, na.rm = TRUE),
            avg_NSE = mean(NSE_puntaje, na.rm = TRUE))

sapply(temp, function(x) sum(is.na(x)))


for_cluster <- merge(temp, temp1, by.x = "ID1", by.y = "id", all.x = TRUE)

sapply(for_cluster, function(x) sum(is.na(x)))

for_cluster <- na.omit(for_cluster)

for_cluster$ch_standard <- 
  (for_cluster$ch_index - mean(for_cluster$ch_index)) / sd(for_cluster$ch_index)



# Escribir el data.frame a un archivo Parquet
# write_parquet(for_cluster, "df_clustered.parquet")

# Leer el archivo Parquet
df <- read_parquet("df_clustered.parquet")
for_cluster <- df

rm(list = setdiff(ls(), c("for_cluster","df", "OA2021_final")))

## CODIGOS DE CLUSTER DE ESCUELA

df <- column_to_rownames(df, var = "ID1")
df <- df %>% select(avg_mpuntaje, avg_lpuntaje, avg_NSE, ch_standard)
df <- scale(df)

# set.seed(123)
# 
# fviz_nbclust(df, kmeans, method = "wss") # 3 grupos
# 
# fviz_nbclust(df, kmeans, method = "silhouette") +
#   ggtitle("Número Óptimo de Clusters") + # Cambia el título principal del gráfico
#   labs(x = "Número de Clusters", y = "Promedio de la Silueta") # Cambia los títulos de los ejes
# 3 grupos

# Compute k-means clustering with k = 4
set.seed(124)
final <- kmeans(df, 3, nstart = 25)
print(final)

# grafico de clusters

fviz_cluster(final, data = df, geom = "point", show.clust.cent = TRUE) +
  ggtitle("")

# Reassign cluster labels: cluster 3 -> 1, cluster 1 -> 3, and cluster 2 stays the same
final$cluster <- recode(final$cluster, `3` = 4, `2` = 6, `1` = 5)
final$cluster <- recode(final$cluster, `4` = 1, `5` = 2, `6` = 3)

# grafico de clusters

fviz_cluster(final, data = df, geom = "point", show.clust.cent = TRUE) +
  ggtitle("")


## agregamos los clusters al dato original


cluster <- column_to_rownames(for_cluster, var = "ID1")
cluster <- cluster %>% select(avg_mpuntaje, avg_lpuntaje, avg_NSE, ch_standard)
resumen_cluster <- aggregate(cluster, by=list(cluster=final$cluster), mean)
resumen_cluster


# Export to Excel
write_xlsx(resumen_cluster, "Resultados, tablas e imagenes/Clustering/RESUMEN_cluster.xlsx")


# # grafico 2 clusters
# fviz_cluster(final, data = cluster,
#              # palette = c("#FF0000", "#0000FF", "#000000", "#C870FF"),
#              ellipse.type = "convex", # Concentration ellipse
#              star.plot = TRUE, # Add segments from centroids to items
#              repel = TRUE, # Avoid label overplotting (slow)
#              ggtheme = theme_minimal())

# colocamos los clusters al dataset
for_cluster <- cbind(for_cluster, cluster = final$cluster)

for_cluster$cluster <- as.factor(for_cluster$cluster)

write_parquet(for_cluster, "df_clustered.parquet")

cluster <- for_cluster %>% select(ID1, cluster)

OA2021_final <- read_parquet("RA_OA_2021.parquet")

OA2021_final <- merge(OA2021_final, cluster, by.x = "ID1", by.y = "ID1", all.x = TRUE)

write_parquet(OA2021_final, "RA_OA_2021.parquet")

## Revisamos las proporciones de migrantes por cluster

rm(list = setdiff(ls(), c("resumen_cluster", "OA2021_final")))
sapply(OA2021_final, function(x) sum(is.na(x)))

contingency_table <- table(OA2021_final$cluster, OA2021_final$origen1)

# Convert the table to a dataframe for exporting
df_contingency_table <- as.data.frame.matrix(contingency_table)

# Add row names as a column
df_contingency_table$cluster <- rownames(df_contingency_table)

# Export to Excel
write_xlsx(df_contingency_table, "Resultados, tablas e imagenes/Clustering/contingency_table.xlsx")

## clusters por sector

cluster_sector <- table(OA2021_final$cluster, OA2021_final$sector)
cluster_sector

# Convert the table to a dataframe for exporting
df_cluster_sector <- as.data.frame.matrix(cluster_sector)

# Add row names as a column
df_cluster_sector$cluster <- rownames(df_cluster_sector)

# Export to Excel
write_xlsx(df_cluster_sector, "Resultados, tablas e imagenes/Clustering/cluster_sector.xlsx")

## clusters por ambito

cluster_ambito <- table(OA2021_final$cluster, OA2021_final$ambito)
cluster_ambito

# Convert the table to a dataframe for exporting
df_cluster_ambito <- as.data.frame.matrix(cluster_ambito)

# Add row names as a column
df_cluster_ambito$cluster <- rownames(df_cluster_ambito)

# Export to Excel
write_xlsx(df_cluster_ambito, "Resultados, tablas e imagenes/Clustering/cluster_ambito.xlsx")

## clusters por NSE

cluster_NSE <- table(OA2021_final$cluster, OA2021_final$NSE_nivel)
cluster_NSE

# Convert the table to a dataframe for exporting
df_cluster_NSE <- as.data.frame.matrix(cluster_NSE)

# Add row names as a column
df_cluster_NSE$cluster <- rownames(df_cluster_NSE)

# Export to Excel
write_xlsx(df_cluster_NSE, "Resultados, tablas e imagenes/Clustering/cluster_NSE.xlsx")


