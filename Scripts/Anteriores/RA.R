# rm(list = ls())
  options(scipen=999)
  
  
  library(readxl)
  library(readr)
  library(dplyr)
  library(haven)
  library(ggfortify)
  library (summarytools)
  library(factoextra)
  library(FactoMineR)
  library(compareGroups)
  library(moments)
  library(viridis)
  library(ggridges)
  library(ggthemes)
  
  
  # guess_encoding ("RelevamientoAnual/01. Bases/Base 5 - Características 2021.csv", n_max = 1000)
  # guess_encoding ("RelevamientoAnual/01. Bases/Base 2 - Matrícula y secciones 2021.csv", n_max = 1000)
  # guess_encoding ("RelevamientoAnual/01. Bases/Base 3 - Trayectoria 2021.csv", n_max = 1000)
  # guess_encoding ("RelevamientoAnual/01. Bases/Base 4 - Cargos 2021.csv", n_max = 1000)
  # guess_encoding ("RelevamientoAnual/01. Bases/Base 4 - Cargos Bis 2021.csv", n_max = 1000)
  # guess_encoding ("RelevamientoAnual/01. Bases/Base 6 - Población, idiomas, etc. 2021.csv", n_max = 1000)
  # guess_encoding ("RelevamientoAnual/01. Bases/Base 7 - Alumnos por edad 2021.csv", n_max = 1000)
  
  # ///////////características del colegio ////////////////
  
  ch21 <- read.csv("RelevamientoAnual/01. Bases/Base 5 - Características 2021.csv", header=TRUE, 
                        row.names=NULL, sep=";", stringsAsFactors=F, check.names = F, fileEncoding = "ISO-8859-1")
  
  ####Para reemplazar las X
  ###Replace Values in a DataFrame
  
  ch21 [ch21 == "X"] <- 1
  
  ##Replace empty cell with NA
  
  ch21 [ch21 == ""] <- NA 
  
  ###Replace NA values with 0
  
  ch21[, 5:64][is.na(ch21[, 5:64])] <- 0
  
  ch21 <- ch21 %>% 
    mutate_at(c(5:64), as.numeric)
  
  # convertimos a factores
  ch21$id <- as.factor(ch21$id)
  ch21$provincia <- as.factor(ch21$provincia)
  ch21$sector <- as.factor(ch21$sector) 
  ch21$ambito <- as.factor(ch21$ambito) 
  
#  stview (dfSummary (ch21))
  
# chequeamos la variable tiene internet
  
ch21$has_internet <- ifelse(ch21$`Internet - Tipo de servicio - Gratuito` == 1 |
                            ch21$`Internet - Tipo de servicio - Pago` == 1, 1, 0)

a<- table(ch21$has_internet)
b <- table(ch21$`Electricidad - Si`)

# la proporcion es significativa para el indice
prop.table(b)

# chequeamos que la variable cuadre con las demas 
# (si no tiene internet, no debe tener ningun tipo de conexion)

for (i in 43:47) {
  column_freq <- table(ch21$has_internet, ch21[, i])
  print(colnames(ch21)[i])
  print(column_freq)
}


  # CONSTRUCCION DE INDICE DE EQUIPAMIENTO

for_index <- ch21[, c(seq(12,15),seq(24,28),31,65, seq(43,47))]

ch_index <-  FactoMineR::PCA(for_index,scale.unit = T, graph = F, ncp = 2)

# obtener la matriz de carga de los componentes principales
loadings<- ch_index$var$coord

# obtener los nombres de las variables originales
variables <- rownames(loadings)

# Obtener los coeficientes de los componentes principales
coeficientes <- loadings[, 1] # Para el primer componente principal

# Formatear la salida
output <- paste(variables, "*", coeficientes, collapse = " + ")

# Mostrar la salida
cat("PC1 =", output)
  
# Print the percentage explained by each component
eigenvalues <- ch_index$eig
eigenvalues[, 1:2]

eigenvalues <- data.frame(eigenvalues[, 1:2])

# library(openxlsx)

# write.xlsx(eigenvalues, "Documentos/autovalores.xlsx")

barplot(eigenvalues[, 2], names.arg=1:nrow(eigenvalues), 
        main = "Variances",
        xlab = "Principal Components",
        ylab = "Percentage of variances",
        col ="steelblue") +
# Add connected line segments to the plot
  lines(x = 1:nrow(eigenvalues), eigenvalues[, 2], 
      type="b", pch=19, col = "red")

ch_index$var$cos2


fviz_pca_var(ch_index, col.var="contrib")
  
ch21$ch_index <-index01
  
  # rm(ch_index)
  # rm(index01)

ch21$ch_standard <- (ch21$ch_index - mean(ch21$ch_index)) / sd(ch21$ch_index)
summary(ch21$ch_standard)

# grafico de densidad del indice

ggplot(ch21, aes(x = ch_standard)) +
  geom_histogram(aes(y = ..density..), colour = "black", fill = "#007A87", bins = 20) +
  geom_density(aes(fill = "Density Plot"), alpha = 0.5) +
  labs(title = "", x = "Indice de Equipamiento", y = "Densidad") +
  scale_fill_manual(values = "#007A87") +  # Change fill color
  theme_few() +
  theme(plot.title = element_text(size = 16, hjust = 0.5)) +
  theme(axis.title = element_text(size = 14)) +
  theme(axis.text = element_text(size = 12)) +
  theme(legend.position = "none") 


  
  # mantenemos solo el indice de equipamiento
  
  ch21_2 <- ch21 %>% select(id, ch_index)
  

  
  # /////////// Matriculados ////////////////
  
  matri21 <- read.csv("RelevamientoAnual/01. Bases/Base 2 - Matrícula y secciones 2021.csv", header=TRUE, 
                      row.names=NULL, sep=";", stringsAsFactors=F, check.names = F, fileEncoding = "ASCII")
  
  matri21 <- matri21 %>% 
    mutate_at(c(1:4), as.factor)
  
  ####Para reemplazar las X
  ###Replace Values in a DataFrame
  
  matri21 [matri21 == "X"] <- 1
  
  ##Replace empty cell with NA
  
  matri21 [matri21 == ""] <- NA 
  
  ###Replace NA values with 0
  
  matri21[, 5:111][is.na(matri21[, 5:111])] <- 0
  
  # filtramos a las escuelas que ofertan primaria
  # esta base se usara como base para identificar estos colegios al unir las bases
  
  matri21 <- matri21 %>% filter(primaria6 == 1 | primaria7 == 1)
  
  to_sum1 <- colnames(matri21[, 23:28])
  to_sum2 <- colnames(matri21[, 23:29])
  
  
  matri21 <- matri21 %>%
    mutate(total_prim6 = rowSums(across(all_of(to_sum1)), na.rm=TRUE),
           total_prim7 = rowSums(across(all_of(to_sum2)), na.rm=TRUE))
  
  matri21$total_primaria <- ifelse(matri21$primaria6 == 1, matri21$total_prim6, matri21$total_prim7)
  
  # colnames(matri21)
  
  matri21 <- matri21[, c(seq(1,4), 28,48, 62, 81, 114)]
  
  # calculamos cantidad de mujeres en el sexto grado
  
  matri21$f_6 <- matri21$`_6` - matri21$v_6
  
  # calculamos la proporcion de recursantes
  matri21$r_6_prop <- ifelse(matri21$`_6` == 0, 0, matri21$r_6 / matri21$`_6`)
  
  # calculamos la proporcion de estudiantes con sobreedad
  matri21$s_6_prop <- ifelse(matri21$`_6` == 0, 0, matri21$s_6 / matri21$`_6`)
  
  
  
  
  # //////////////////777 trayectoria ///////////////
  
  # 
  # trayec21 <- read.csv("RelevamientoAnual/01. Bases/Base 3 - Trayectoria 2021.csv", header=TRUE, 
  #                     row.names=NULL, sep=";", stringsAsFactors=F, check.names = F, fileEncoding = "ASCII")
  # 
  # trayec21 <- trayec21 %>% 
  #   mutate_at(c(1:4), as.factor)
  # 
  # ####Para reemplazar las X
  # ###Replace Values in a DataFrame
  # 
  # trayec21 [trayec21 == "X"] <- 1
  # 
  # ##Replace empty cell with NA
  # 
  # trayec21 [trayec21 == ""] <- NA 
  # 
  # ###Replace NA values with 0
  # 
  # trayec21[, 5:280][is.na(trayec21[, 5:280])] <- 0
  # 
  # # filtramos a las escuelas que ofertan primaria
  # 
  # trayec21 <- trayec21 %>% filter(primaria6 == 1 | primaria7 == 1)
  
  # # /////////// CARGOS ///////////
  # 
  # cargos21 <- read.csv("RelevamientoAnual/01. Bases/Base 4 - Cargos 2021.csv", header=TRUE, 
  #                      row.names=NULL, sep=";", stringsAsFactors=F, check.names = F, fileEncoding = "UTF-8")
  # 
  # cargos21 <- cargos21 %>% 
  #   mutate_at(c(1:4), as.factor)
  # 
  # ####Para reemplazar las X
  # ###Replace Values in a DataFrame
  # 
  # cargos21 [cargos21 == "X"] <- 1
  # 
  # ##Replace empty cell with NA
  # 
  # cargos21 [cargos21 == " "] <- NA 
  # cargos21 [cargos21 == ""] <- NA 
  # 
  # ###Replace NA values with 0
  # 
  # cargos21[, 5:72][is.na(cargos21[, 5:72])] <- 0
  # 
  # # filtramos los colegios que ofertan educacion primaria
  # 
  # cargos21 <- cargos21 %>% 
  #   filter(primaria6 == 1 | primaria7 == 1)
  # 
  # 
  # cargosBis21 <- read.csv("RelevamientoAnual/01. Bases/Base 4 - Cargos Bis 2021.csv", header=TRUE, 
  #                  row.names=NULL, sep=";", stringsAsFactors=F, check.names = F, fileEncoding = "ISO-8859-1")
  # 
  # cargosBis21 <- cargosBis21 %>% 
  #   mutate_at(c(2:9), as.factor)
  # 
  # cargosBis21 <- cargosBis21 %>% filter(nivel == "Primaria")
  # 
  
  # //////////////7 poblacion, idiomas y otros ////////////////
  
  pob21 <- read.csv("RelevamientoAnual/01. Bases/Base 6 - Población, idiomas, etc. 2021.csv", header=TRUE, 
                       row.names=NULL, sep=";", stringsAsFactors=F, check.names = F, fileEncoding = "UTF-8")
  
  pob21 <- pob21 %>% 
    mutate_at(c(1:4), as.factor)
  
  ###Replace Values in a DataFrame
  
  pob21 [pob21 == "X"] <- 1
  
  ##Replace empty cell with NA
  
  pob21 [pob21 == " "] <- NA 
  pob21 [pob21 == ""] <- NA 
  
  
  ###Replace NA values with 0
  
  pob21[, 5:130][is.na(pob21[, 5:130])] <- 0
  
  
  pob21 <- pob21[, c(seq(1:4), seq(54,66))]
  
  colnames(pob21)
  
  to_sum1 <- colnames(pob21[, 5:17])
  to_sum2 <- colnames(pob21[, c(6,7,8,9,12,13,14)])
  
  pob21 <- pob21 %>%
    mutate(total_migrantes = rowSums(across(all_of(to_sum1)), na.rm=TRUE),
           OtrospaísesdeAmérica = rowSums(across(all_of(to_sum2)), na.rm=TRUE))
  
  pob21 <- pob21 %>% select(ID1, BoliviaPrimaria, PerúPrimaria, ParaguayPrimaria, OtrospaísesdeAmérica,
                            EuropaPrimaria, AsiaPrimaria, OtrosPrimaria, total_migrantes)
  
    # # ///////////////// alumnos por edad (primaria) ////////////////
  # 
  # alumnos_edad <- read.csv("RelevamientoAnual/01. Bases/Base 7 - Alumnos por edad 2021.csv", header=TRUE, 
  #                   row.names=NULL, sep=";", stringsAsFactors=F, check.names = F, fileEncoding = "UTF-8")
  # 
  # alumnos_edad <- alumnos_edad %>% 
  #   mutate_at(c(1:5), as.factor)
  # 
  # # filtramos por los estudiantes de sexto grado
  # 
  # alumnos_edad <- alumnos_edad %>% filter(grado == "6°")
  # 
  # ###Replace NA values with 0
  # 
  # alumnos_edad[, 6:43][is.na(alumnos_edad[, 6:43])] <- 0
  
  # /////////// UNION DE LOS DATASETS //////////////
  
  union21 <-   ch21_2 %>% 
    left_join(matri21, by = "id") %>% 
    left_join(pob21, by = join_by(id == ID1))
  

# calculamos la proporcion de migrantes de primaria en las escuelas 

union21$migrante_prop <- 
  ifelse(union21$total_primaria == 0 , 0, union21$total_migrantes / union21$total_primaria)

union21$ch_standard <- (union21$ch_index - mean(union21$ch_index)) / sd(union21$ch_index)
summary(union21$ch_standard)

# chequeamos los NA's del indice de equipamiento

# mean(is.na(union21$ch_standard))

rm(list = setdiff(ls(), c("union21", "OA2021")))


# write.foreign(union21, "RA2021.txt", "RA2021.sps", package = "SPSS")
# write_sav(union21, "ra2021.sav")
# write_csv(union21, "aber.csv")

# GRAFICOS DE BOXPLOT PARA LAS VARIABLES: PROVINCIA, SECTOR Y AMBITO

for (i in 2:4 ) {
  quantiles <- union21 %>% 
    group_by(union21[, i]) %>% 
    summarize(
      median = median(ch_standard),
      q25 = quantile(ch_standard, 0.25),
      q75 = quantile(ch_standard, 0.75),
      mean = mean(ch_standard)
    )
  boxplot <- ggplot(union21, aes(x = union21[, i], y = ch_standard)) +
    geom_boxplot(fill = "lightblue", color = "blue", width = 0.5) +
    labs(x = colnames(union21)[i], y = "Indice de equipamiento",
         title = paste0("Boxplot del Indice de equipamiento según ", colnames(union21)[i])) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  print(boxplot)
}

# distribucion del indice normal y estandarizado


ggplot(union21, aes(x = ch_index)) +
  geom_histogram(fill = "skyblue", color = "black", bins = 20) +  # Customize fill and border colors
  labs(
    title = "Distribución del índice de equipamiento",
    x = "Indice de equipamiento",
    y = "Frecuencia"
  ) +
  theme_minimal()

ggplot(union21, aes(x = ch_standard)) +
  geom_histogram(fill = "skyblue", color = "black", bins = 20) +  # Customize fill and border colors
  labs(
    title = "Distribución del índice de equipamiento",
    x = "Indice de equipamiento",
    y = "Frecuencia"
  ) +
  theme_minimal()

hist(union21$ch_standard)

summary(union21$ch_index)

b <- union21 %>% 
  summarise(min = min(ch_standard),
            q1 = quantile(ch_standard, 0.25),
            median = median(ch_standard),
            q3 = quantile(ch_standard, 0.75),
            max = max(ch_standard),
            mean = mean(ch_standard),
            asimetria = skewness(ch_standard),
            curtosis = kurtosis(ch_standard))
b
# b <- t(b)
# write.xlsx(b, "resumen_indice.xlsx")
# ///////////////////7 COMPARACION CON INFORME DE EDUARDO BOLOGNA //////////////////

# los totales difieren en cantidad de matriculados (diferencia en los ultimos 3 digitos)
tot21 <- union21 %>%
  summarise("Migrantes" = sum(total_migrantes),
            "Paraguay" = sum(ParaguayPrimaria),
            "Peru" = sum(PerúPrimaria),
            "Bolivia" = sum(BoliviaPrimaria),
            "Nativos" = (sum(total_primaria, na.rm = TRUE) - sum(total_migrantes)),
            "Total" = sum(total_primaria, na.rm = TRUE))
tot21

# por provincia
# totales
prov21 <- union21 %>% 
  group_by(provincia) %>% 
  summarise("estudiantes" = sum(total_primaria)) %>% 
  arrange(desc(estudiantes))
prov21$prop <- prov21$estudiantes / sum(prov21$estudiantes)

# View(prov21)

#migrantes


prov21_mig <- union21 %>% 
  group_by(provincia) %>% 
  summarise("estudiantes" = sum(total_migrantes)) %>% 
  arrange(desc(estudiantes))
prov21_mig$prop <- prov21$estudiantes / sum(prov21$estudiantes)

# View(prov21_mig)

# Distribucion de migrantes por sector

sector_mig <- union21 %>% 
  group_by(sector) %>% 
  summarise("estudiantes" = sum(total_migrantes)) %>% 
  arrange(desc(estudiantes))
sector_mig$prop <- sector_mig$estudiantes / sum(sector_mig$estudiantes)

# View(sector_mig)


# //// UNION CON OPERATIVO APRENDER ////////////

union21 <- union21 %>% select(id,ch_index, migrante_prop, total_primaria, total_migrantes)

union21 <- setDT(union21)

OA2021 <- merge(OA2021, union21, by.x = "ID1", by.y = "id", all.x = TRUE)

# OA2021 <- OA2021 %>% left_join(union21, by = join_by(ID1 == id), keep = TRUE)

colnames(OA2021)

OA2021 <- OA2021[, c(seq(1,26),seq(112,120),seq(169,195))]

OA2021$ch_standard <- (OA2021$ch_index - mean(OA2021$ch_index, na.rm = TRUE))/sd(OA2021$ch_index, na.rm = TRUE)
summary(OA2021$ch_standard)
sd(OA2021$ch_standard, na.rm = TRUE)


# Define the desired order for the x-axis categories

# chequeamos NA's de indice de equipamiento

summary(OA2021$ch_standard)
mean(is.na(OA2021$ch_standard))

print("Cantidad de colegios sin indice de equipamiento: ")
OA2021 %>% filter(is.na(OA2021$ch_standard)) %>% 
  distinct(ID1) %>% count()
print("de cantidad total de colegios")
OA2021  %>% 
  distinct(ID1) %>% count()



# boxplots por puntajes e indice de equipamiento


for (i in 39:40 ) {
  quantiles <- OA2021 %>% 
    group_by(OA2021[,i]) %>% 
    summarize(
      median = median(ch_standard, na.rm = TRUE),
      q25 = quantile(ch_standard, 0.25, na.rm = TRUE),
      q75 = quantile(ch_standard, 0.75, na.rm = TRUE),
      mean = mean(ch_standard, na.rm = TRUE)
    )
  boxplot <- ggplot(OA2021, aes(x = OA2021[,i], y = ch_standard)) +
    geom_boxplot(fill = "lightblue", color = "blue", width = 0.5) +
    labs(x = colnames(union21)[i], y = "Indice de equipamiento",
         title = paste0("Boxplot del Indice de equipamiento según ", colnames(OA2021)[i])) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  print(boxplot)
  print(paste0("Datos del Indice de equipamiento según ", colnames(OA2021)[i]))
  print(quantiles)
}

  # write.csv(OA2021, "RA_OA_2021.csv")
 
 ggplot(OA2021, aes(x = ch_standard)) +
   geom_histogram(fill = "skyblue", color = "black", bins = 20) +  # Customize fill and border colors
   labs(
     title = "Distribución del índice de equipamiento",
     x = "Indice de equipamiento",
     y = "Frecuencia"
   ) +
   theme_minimal()

# colegios por proporcion de migrantes

prop_orig1 <- OA2021 %>% 
  group_by(origen1) %>% 
  summarise(prom_mate = mean(mpuntaje, na.rm = TRUE),
            prom_lengua =  mean(lpuntaje, na.rm = TRUE),
            total = n(),
            prom_mig_prop = mean(migrante_prop, na.rm = TRUE))

# FILTRAMOS LOS ORIGENES A TRABAJAR

OA2021_final <- OA2021 %>% 
  filter(origen1 == "Boliviano" | origen1 == "Hijo de bolivianos" | origen1 == "Paraguayo"
         | origen1 == "Hijo de paraguayos" | origen1 == "Peruano"  | origen1 == "Hijo de peruanos"
         | origen1 == "Nativo")

# Remove empty levels
OA2021_final$origen1 <- droplevels(OA2021_final$origen1)

# Check the levels
levels(OA2021_final$origen1)

rm(list = setdiff(ls(), c("OA2021_final")))

# Define the desired order for the x-axis categories

desired_order <- c("Nativo", "Hijo de peruanos", "Peruano", "Hijo de paraguayos", "Paraguayo",
                   "Hijo de bolivianos", "Boliviano")

# Reorder the levels of the ch_cat variable based on the desired order
OA2021_final$origen1 <- factor(OA2021_final$origen1, levels = desired_order)
summary(OA2021_final$origen1)

# /// Categorizamos las variables cuantitativas para el ACM

OA2021_final$ch_cat <- ifelse(OA2021_final$ch_standard <=
                                quantile(OA2021_final$ch_standard, 1/3, na.rm = TRUE), "Bajo",
                        ifelse(OA2021_final$ch_standard <=
                                 quantile(OA2021_final$ch_standard, 2/3, na.rm = TRUE), "Medio", "Alto"))


# OA2021_final$ch_cat <- ifelse(OA2021_final$ch_standard <= 
#                                 quantile(OA2021_final$ch_standard, 0.25, na.rm = TRUE), "Bajo",
#                               ifelse(OA2021_final$ch_standard <= 
#                                        quantile(OA2021_final$ch_standard, 0.5, na.rm = TRUE), "Medio-bajo",
#                                ifelse(OA2021_final$ch_standard <= 
#                                               quantile(OA2021_final$ch_standard, 0.75, na.rm = TRUE),
#                                       "Medio-alto" ,"Alto")))


OA2021_final$ch_cat <- factor(OA2021_final$ch_cat)

summary(OA2021_final$ch_cat)

OA2021_final <- OA2021_final %>%
  rename(
    Indice_equipamiento = ch_cat,
    Puntaje_lengua = ldesemp,
    Puntaje_matematica = mdesemp )


# 
# ggplot(OA2021_final, aes(x = ch_standard, y = origen1, fill = ..x..)) +
#   geom_density_ridges_gradient(scale = 3, rel_min_height = 0.01, gradient_lwd = 1.) +
#   scale_x_continuous(expand = c(0.01, 0)) +
#   scale_y_discrete(expand = c(0.01, 0)) +
#   scale_fill_viridis(name = "Equipamiento", option = "C") +
#   labs(title = 'Indice de equipamiento por origen del estudiante') +
#   theme_ridges(font_size = 13, grid = TRUE) +
#   theme(axis.title.y = element_blank(), axis.title.x = element_blank())





  
