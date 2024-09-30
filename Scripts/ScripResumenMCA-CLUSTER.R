rm(list = ls())
options(scipen=999)


library (ggplot2)
library (arrow)
library(dplyr)
library(FactoMineR)
library(factoextra)
library(ggrepel)
library(ggridges)
require(missMDA)
library(ggthemes)
library(PupillometryR)

# ANALISIS CORRESPONDENCIA MULTIPLE

OA2021_final <- read_parquet("RA_OA_2021.parquet")


## ACM sin los nativos

for_mca <- OA2021_final %>% 
  filter( origen != "native") %>%  
  select(Puntaje_matematica, Puntaje_lengua, sector, ambito, NSE_nivel, Indice_equipamiento,
         origen1, ponder)

imputes <- imputeFAMD(for_mca[,1:7])

res1 <- MCA(for_mca[,1:7],ncp = 12, graph = FALSE, method = "CA", 
            row.w=for_mca$ponder, tab.disj = imputes$tab.disj)


var1 <- get_mca_var(res1)
var1

# data frame with variable coordinates
cats = apply(for_mca[1:7], 2, function(x) nlevels(as.factor(x)))
cats

res1_vars_df = data.frame(var1$coord, Variable = rep(names(cats), cats))
# Recode the Variable column with desired names
res1_vars_df$Variable <- recode(res1_vars_df$Variable,
                                "Puntaje_matematica" = "Puntaje de Matemática",
                                "Puntaje_lengua" = "Puntaje de Lengua",
                                "sector" = "Sector",
                                "ambito" = "Ambito",
                                "NSE_nivel" = "Nivel NSE",
                                "Indice_equipamiento" = "Indice de equipamiento",
                                "origen1" = "Origen"
)

res1_vars_df$for_row <- c("Matematica - Por debajo", "Matematica - Básico", "Matematica - Satisfactorio",
                          "Matematica - Avanzado", "Lengua - Por debajo", "Lengua - Básico",
                          "Lengua - Satisfactorio", "Lengua - Avanzado", "Estatal", "Privado",
                          "Rural", "Urbano", "NSE - Bajo", "NSE - Medio", "NSE - Alto",
                          "Equipamiento - Alto", "Equipamiento - Bajo", "Equipamiento - Medio",
                          "Hijo peruanos", "Peruano", "Hijo paraguayos", "Paraguayo",
                          "Hijo bolivianos", "Boliviano", "Hijo venezolanos", "Venezolano")

# Set row names from an existing column
rownames(res1_vars_df) <- res1_vars_df$for_row
res1_vars_df$for_row <- NULL  # Remove the column if it's no longer needed

ggplot(data = res1_vars_df, aes(x = Dim.1, y = Dim.2, label = rownames(res1_vars_df))) +
  geom_hline(yintercept = 0, colour = "gray70") +
  geom_vline(xintercept = 0, colour = "gray70") +
  geom_text_repel(aes(colour = Variable)) +  # Use geom_text_repel instead of geom_text
  ggtitle("") +
  theme(plot.title = element_text(hjust = 0.5)) + 
  paper_theme()
##  MISMO GRAFICO PERO SIN LA LEYENDA AL COSTADO
ggplot(data = res1_vars_df, aes(x = Dim.1, y = Dim.2, label = rownames(res1_vars_df))) +
  geom_hline(yintercept = 0, colour = "gray70") +
  geom_vline(xintercept = 0, colour = "gray70") +
  geom_text_repel(aes(colour = Variable), size = 5) +  # Increase text label size
  ggtitle("") +
  theme(plot.title = element_text(hjust = 0.5)) + 
  paper_theme() +
  theme(legend.position = "none")  # Remove the legend

## ACM con los nativos

for_mca <- OA2021_final %>% 
  select(Puntaje_matematica, Puntaje_lengua, sector, ambito, NSE_nivel, Indice_equipamiento,
         origen1, ponder)

imputes <- imputeFAMD(for_mca[,1:7])

res2 <- MCA(for_mca[,1:7],ncp = 12, graph = FALSE, method = "CA", 
            row.w=for_mca$ponder, tab.disj = imputes$tab.disj)

var2 <- get_mca_var(res2)
var2

# data frame with variable coordinates
cats = apply(for_mca[1:7], 2, function(x) nlevels(as.factor(x)))
cats

res2_vars_df = data.frame(var2$coord, Variable = rep(names(cats), cats))
# Recode the Variable column with desired names
res2_vars_df$Variable <- recode(res2_vars_df$Variable,
                                "Puntaje_matematica" = "Puntaje de Matemática",
                                "Puntaje_lengua" = "Puntaje de Lengua",
                                "sector" = "Sector",
                                "ambito" = "Ambito",
                                "NSE_nivel" = "Nivel NSE",
                                "Indice_equipamiento" = "Indice de equipamiento",
                                "origen1" = "Origen"
)

res2_vars_df$for_row <- c("Matematica - Por debajo", "Matematica - Básico", "Matematica - Satisfactorio",
                          "Matematica - Avanzado", "Lengua - Por debajo", "Lengua - Básico",
                          "Lengua - Satisfactorio", "Lengua - Avanzado", "Estatal", "Privado",
                          "Rural", "Urbano", "NSE - Bajo", "NSE - Medio", "NSE - Alto",
                          "Equipamiento - Alto", "Equipamiento - Bajo", "Equipamiento - Medio",
                          "Nativo", "Hijo peruanos", "Peruano", "Hijo paraguayos", "Paraguayo",
                          "Hijo bolivianos", "Boliviano", "Hijo venezolanos", "Venezolano")

# Set row names from an existing column
rownames(res2_vars_df) <- res2_vars_df$for_row
res2_vars_df$for_row <- NULL  # Remove the column if it's no longer needed

ggplot(data = res2_vars_df, aes(x = Dim.1, y = Dim.2, label = rownames(res2_vars_df))) +
  geom_hline(yintercept = 0, colour = "gray70") +
  geom_vline(xintercept = 0, colour = "gray70") +
  geom_text_repel(aes(colour = Variable)) +  # Use geom_text_repel instead of geom_text
  ggtitle("") +
  theme(plot.title = element_text(hjust = 0.5)) + 
  paper_theme()

##  MISMO GRAFICO PERO SIN LA LEYENDA AL COSTADO
ggplot(data = res2_vars_df, aes(x = Dim.1, y = Dim.2, label = rownames(res2_vars_df))) +
  geom_hline(yintercept = 0, colour = "gray70") +
  geom_vline(xintercept = 0, colour = "gray70") +
  geom_text_repel(aes(colour = Variable), size = 5) +  # Increase text label size
  ggtitle("") +
  theme(plot.title = element_text(hjust = 0.5)) + 
  paper_theme() +
  theme(legend.position = "none")  # Remove the legend

# GRAFICOS BIVARIADOS

# INDICE DE EQUIPAMIENTO ESCOLAR

JACE_COLOR <- c("#E41A1C", "#E41A1C", "#377EB8", "#377EB8", "#4DAF4A","#4DAF4A",
                "#FF7F00","#FF7F00", "#FFFF33","#FFFF33", "#984EA3")

ggplot(OA2021_final, aes(x = ch_standard, y = origen1, fill = origen1)) +
  # geom_density_ridges_gradient(scale = 3, rel_min_height = 0.01, gradient_lwd = 1.) +
  geom_density_ridges(quantile_lines=TRUE, 
                      quantile_fun=function(ch_standard,...)median(ch_standard)) + 
  scale_y_discrete(expand = c(0.01, 0))  +
  scale_fill_manual(values = rev(JACE_COLOR)) +
  scale_color_manual(values = rev(JACE_COLOR))  +
  labs(x = "Indice de equipamiento",
       y = "",
       fill = "Origen: ",
       title = "") +
  guides(fill = guide_legend(title = "Origen:",
                             direction = "vertical",
                             reverse = T),
         color = guide_legend(title = "Origen:",
                              direction = "vertical",
                              reverse = T))

# PUNTAJE DE LENGUAS

OA2021_final %>%
  ggplot() +
  aes(x = reorder(origen1, desc(origen1)),
      y = lpuntaje,
      fill = origen1) +
  geom_point(aes(color = origen1),
             position = position_jitter(w = .15),
             size = 0.5,
             alpha = 0.05) +
  geom_boxplot(width = .25,
               outlier.shape = NA,
               alpha = 0.5) +
  geom_flat_violin(position = position_nudge(x = .2),
                   alpha = 0.7,
                   adjust = 0.5)  +
  # coord_flip() +
  scale_x_discrete(expand = c(0,0)) +
  scale_fill_manual(values = rev(JACE_COLOR)) +
  scale_color_manual(values = rev(JACE_COLOR)) +
  guides(fill = guide_legend(title = "Origen:",
                             direction = "horizontal",
                             reverse = T),
         color = guide_legend(title = "Origen:",
                              direction = "horizontal",
                              reverse = T)) +
  labs(y = "Puntaje de lengua", x = "",
       title = "")  +
  theme(legend.position = "bottom") 

# PUNTAJE DE MATEMATICA

OA2021_final %>%
  ggplot() +
  aes(x = reorder(origen1, desc(origen1)),
      y = mpuntaje,
      fill = origen1) +
  geom_point(aes(color = origen1),
             position = position_jitter(w = .15),
             size = 0.5,
             alpha = 0.05) +
  geom_boxplot(width = .25,
               outlier.shape = NA,
               alpha = 0.5) +
  geom_flat_violin(position = position_nudge(x = .2),
                   alpha = 0.7,
                   adjust = 0.5)  +
  # coord_flip() +
  scale_x_discrete(expand = c(0,0)) +
  scale_fill_manual(values = rev(JACE_COLOR)) +
  scale_color_manual(values = rev(JACE_COLOR)) +
  guides(fill = guide_legend(title = "Origen:",
                             direction = "horizontal",
                             reverse = T),
         color = guide_legend(title = "Origen:",
                              direction = "horizontal",
                              reverse = T)) +
  labs(y = "Puntaje de matemática", x = "",
       title = "")  +
  theme(legend.position = "bottom") 

# NIVEL SOCIOECONOMICO

ggplot(OA2021_final, aes(x = NSE_puntaje, y = origen1, fill = origen1)) +
  # geom_density_ridges_gradient(scale = 3, rel_min_height = 0.01, gradient_lwd = 1.) +
  geom_density_ridges(quantile_lines=TRUE, quantile_fun=function(ch_standard,...)median(ch_standard)) + 
  scale_y_discrete(expand = c(0.01, 0))  +
  scale_fill_manual(values = rev(JACE_COLOR)) +
  scale_color_manual(values = rev(JACE_COLOR))  +
  labs(x = "Indice de nivel socioeconómico",
       y = "",
       fill = "Origen: ",
       title = "") +
  guides(fill = guide_legend(title = "Origen:",
                             direction = "vertical",
                             reverse = T),
         color = guide_legend(title = "Origen:",
                              direction = "vertical",
                              reverse = T)) 

# GRAFICOS UNIVARIADOS

# distribucion de migrantes por origen

origen <- OA2021_final %>% filter(origen2 != "Otro" & origen2 != "Native") %>% 
  select(origen1) %>% group_by(origen1) %>% 
  count(name = "cantidad") %>%
  arrange(cantidad)
origen$prop <- (origen$cantidad/sum(origen$cantidad))*100
origen <- origen %>% arrange(desc(prop))
origen$freq_acum <- cumsum(origen$prop)
origen


# grafico


ggplot(origen, aes(x = reorder(origen1, -cantidad), y = cantidad)) +
  geom_bar(stat = "identity", fill = "#377EB8", color = "black") +
  geom_text(aes(label = paste0(cantidad, " (", round(prop, 1), "%)")),
            vjust = -0.5, color = "black", fontface = "bold", size = 3) +
  labs(x = "País", y = "Cantidad", title = "") +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_y_continuous(labels = scales::comma)  # Format y-axis labels with commas

# distribucion de migrantes por origen2
origen2 <- OA2021_final %>% filter(origen2 != "Otro" & origen2 != "Native") %>% 
  select(origen2) %>% group_by(origen2) %>% 
  count(name = "cantidad") %>%
  arrange(cantidad)
origen2$prop <- (origen2$cantidad/sum(origen2$cantidad))*100
origen2 <- origen2 %>% arrange(desc(prop))
origen2$freq_acum <- cumsum(origen2$prop)
origen2

# grafico


ggplot(origen2, aes(x = reorder(origen2, -cantidad), y = cantidad)) +
  geom_bar(stat = "identity", fill = "#377EB8", color = "black") +
  geom_text(aes(label = paste0(cantidad, " (", round(prop, 1), "%)")),
            vjust = -0.5, color = "black", fontface = "bold", size = 3) +
  labs(x = "País", y = "Cantidad", title = "") +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_y_continuous(labels = scales::comma)  # Format y-axis labels with commas

## CLUSTERING DE ESCUELAS

# Leer el archivo Parquet
df <- read_parquet("df_clustered.parquet")
for_cluster <- df

rm(list = setdiff(ls(), c("for_cluster","df", "OA2021_final")))

## CODIGOS DE CLUSTER DE ESCUELA

df <- column_to_rownames(df, var = "ID1")
df <- df %>% select(avg_mpuntaje, avg_lpuntaje, avg_NSE, ch_standard)
df <- scale(df)

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


# GRAFICO DE SILUETAS
# Calculate distance matrix
dist_matrix <- dist(df)

# Compute silhouette information
sil <- silhouette(final$cluster, dist_matrix)

fviz_silhouette(sil) + paper_theme()


# Grafico de dispersión

# Scatter plot between mpuntaje and NSE_puntaje, colored by cluster
ggplot(data = for_cluster, aes(x = avg_NSE , y = avg_mpuntaje, color = as.factor(cluster))) +
  geom_point() +  # Scatter plot points with size 3
  labs(title = "",
       x = "Nivel socioeconómico promedio", 
       y = "Puntaje promedio de matemática",
       color = "Cluster") +  # Title for the legend +  # Apply a minimal theme
  theme(plot.title = element_text(hjust = 0.5))  # Center the title
