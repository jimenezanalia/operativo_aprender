
options(scipen=999)
# cargamos las librerías

library(dplyr)
library(ggplot2)
library(ggrepel)
library(emmeans)
library(lme4)
library(lmerTest)
library(MuMIn)
library(lattice)
library(tidyverse)
library(gridExtra)
library(forcats)
library(tidyr)
library(Hmisc)

rm(list = setdiff(ls(), c("migrantes","natives","OA2021")))




# nos fijamos en los puntajes de matematica segun origen
# si generalizamos a los migrantes no pareciera haber una diferencia tan grande
# pero al mirar por pais de origen se puede notar las diferencias

avg_score <- OA2021 %>% filter(is.na(migracion) == FALSE) %>% 
  group_by(migracion) %>%
  summarise(mean = mean(mpuntaje, na.rm = TRUE),
            weighted_mean = weighted.mean(mpuntaje, mpondera, na.rm = TRUE),
            sd = sd(mpuntaje, na.rm = TRUE),
            n = n()) %>%
  ungroup() %>%
  mutate(migracion = if_else(is.na(migracion), "Total", as.character(migracion))) %>%
  bind_rows(., summarise(., migracion = "Total",
                         mean = mean(OA2021$mpuntaje, na.rm = TRUE),
                         weighted_mean = weighted.mean(OA2021$mpuntaje, OA2021$mpondera, na.rm = TRUE),
                         sd = sd(OA2021$mpuntaje, na.rm = TRUE),
                         n = nrow(OA2021)))
avg_score

avg_score0 <- OA2021 %>% filter(!is.na(origen)) %>% 
  group_by(origen) %>%
  summarise(weighted_mean = weighted.mean(mpuntaje, mpondera, na.rm = TRUE),
            sd =  sqrt(wtd.var(mpuntaje, weights= mpondera, na.rm = TRUE)),
            n = n()) %>%
  ungroup() %>%
  mutate(origen = if_else(is.na(origen), "Total", as.character(origen))) %>%
  bind_rows(., summarise(., origen = "Total",
                         weighted_mean = weighted.mean(OA2021$mpuntaje, OA2021$mpondera, na.rm = TRUE),
                         sd =  sqrt(wtd.var(OA2021$mpuntaje, weights= OA2021$mpondera, na.rm = TRUE)),
                         n = nrow(OA2021)))
avg_score0

avg_score1 <- OA2021 %>% filter(is.na(origen1) == FALSE) %>% 
  group_by(origen1) %>%
  summarise(weighted_mean = weighted.mean(mpuntaje, mpondera, na.rm = TRUE),
            sd =  sqrt(wtd.var(mpuntaje, weights= mpondera, na.rm = TRUE)),
            n = n()) %>%
  ungroup() %>%
  mutate(origen1 = if_else(is.na(origen1), "Total", as.character(origen1))) %>%
  bind_rows(., summarise(., origen1 = "Total",
                         weighted_mean = weighted.mean(OA2021$mpuntaje, OA2021$mpondera, na.rm = TRUE),
                         sd =  sqrt(wtd.var(OA2021$mpuntaje, weights= OA2021$mpondera, na.rm = TRUE)),
                         n = nrow(OA2021)))
avg_score1

avg_score2 <- OA2021 %>% filter(is.na(origen2) == FALSE) %>% 
  group_by(origen2) %>%
  summarise(weighted_mean = weighted.mean(mpuntaje, mpondera, na.rm = TRUE),
            sd =  sqrt(wtd.var(mpuntaje, weights= mpondera, na.rm = TRUE)),
            n = n()) %>%
  ungroup() %>%
  mutate(origen2 = if_else(is.na(origen2), "Total", as.character(origen2))) %>%
  bind_rows(., summarise(., origen2 = "Total",
                         weighted_mean = weighted.mean(OA2021$mpuntaje, OA2021$mpondera, na.rm = TRUE),
                         sd =  sqrt(wtd.var(OA2021$mpuntaje, weights= OA2021$mpondera, na.rm = TRUE)),
                         n = nrow(OA2021)))
avg_score2


# verificamos el supuesto de normalidad
## con caja y bigotes
# como mencione arriba, solo se nota la diferencia si diferenciamos por paises

win.graph(width = 8, height = 6, pointsize = 10) # abre una pestaña a parte para los gráficos
# colocamos el orden en el que queremos desplegar los datos
desired_order <- c("Boliviano", "Hijo de bolivianos", "Chileno","Hijo de chilenos", "Paraguayo",
                   "Hijo de paraguayos", "Peruano","Hijo de peruanos", "Venezolano",
                   "Hijo de venezolanos","nac_otro","hije_otro","Nativo")

# graficamente se puede ver que los migrantes e hijes tienen diferencias en sus puntajes
OA2021 %>%
  filter(!is.na(origen1)) %>%
  mutate(origen1 = factor(origen1, levels = desired_order)) %>%
  ggplot(aes(x = origen1, y = mpuntaje)) +
  geom_boxplot(color = "steelblue", fill = "lightblue", outlier.shape = NA) +
  geom_text(aes(label = origen1, y = max(mpuntaje), vjust = -0.5), color = "black", fontface = "bold") +
  labs(x = "", y = "Puntaje matemática") +
  theme_bw() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(color = "black"),
    axis.text.x = element_text(color = "black", size = 12, angle = 45, hjust = 1),
    axis.text.y = element_text(color = "black", size = 12),
    axis.title = element_text(color = "black", size = 14, face = "bold"),
    plot.title = element_text(size = 16, face = "bold"),
    plot.subtitle = element_text(size = 14),
    plot.caption = element_text(size = 12),
    legend.position = "none"
  )


# histogramas
# para todos los grupos migrante ocurre que la densidad poblacional es mas grandes para los hijos de migrantes
# a excepcion de venezuela, que son los nacidos alla quien tiene mayor densidad poblacional en comparacion
OA2021 %>% filter(is.na(origen1) == FALSE & origen1 != "native") %>%
  mutate(origen1 = factor(origen1, levels = desired_order)) %>% 
  ggplot( aes(x = mpuntaje, fill = origen1)) +
  geom_histogram(bins = 30) +
  ggtitle("Puntaje de Matemática") +
  xlab("Puntaje") +
  ylab("Densidad") +
  facet_wrap(~ origen1)

# se prueba una prueba de t-student sobre  migrante/native para ver si los putnajes en matematica son distintos
# asumiendo varianzas iguales

ttest1 <- t.test(mpuntaje ~ migracion, data = OA2021, var.equal = TRUE, weights = mpondera)
ttest1

# varianzas distintas
ttest2 <- t.test(mpuntaje ~ migracion, data = OA2021, var.equal = FALSE)
ttest2
# en ambos casos se puede ver que existe diferencia significativa entre migrantes y natives

# haremos un test anova para ambas variables origen
# el primero solo tiene dos clasificaciones, si la prueba es significativa significa que las medias
# son diferentes entre migrantes e hijos y natives
anova1 <- aov(mpuntaje ~ origen, data = OA2021)
summary(anova1)
# chequeamos las diferencias
emmeans(anova1, "origen")

# ahora testeamos sobre el origen que distingue países y nacimiento
# si el test sale significativo, indica que al menos UNA clasificacion es diferente al resto
# no diferencia cual
anova2 <- aov(mpuntaje ~ origen1, data = OA2021)
summary(anova2)
# miramos los valores numericos de las diferencias
emmeans(anova2, "origen1")


# Se dejo en claro que los puntajes difieren entre si según origen del estudiante
# se procede a analizar al estudiante segun su origen y otras variables características


# el 80% de los migrantes se encuentran concentrados en las primeras 5 jurisdicciones que se muestran abajo

# ahora nos fijaremos en la concentracion por region
# Tan solo el centro alberga el 75% de los migrantes
migrantes <- OA2021 %>%
  filter(as.numeric(migracion) != 1) %>%
  select(region, jurisdiccion) %>%
  group_by(region) %>%
  summarise(nro_jurisdiccion = n_distinct(jurisdiccion),
            migrantes = n()) %>%
  arrange(desc(migrantes))
migrantes$prop_migrantes <- (migrantes$migrantes/sum(migrantes$migrantes))*100
migrantes$frec_acum <- cumsum(migrantes$prop_migrantes)
migrantes

# el 75% de los migrantes por región se encuentran dentro de estas jurisdicciones
migrantes_centro <- OA2021 %>% 
  filter(as.numeric(migracion) != 1 & region == "CENTRO") %>% 
  select(jurisdiccion) %>% group_by(jurisdiccion) %>% 
  count(name = "cantidad") %>%
  arrange(cantidad)
migrantes_centro$prop <- (migrantes_centro$cantidad/sum(migrantes_centro$cantidad))*100
migrantes_centro <- migrantes_centro %>% arrange(desc(prop))
migrantes_centro$frec_acumulada <- cumsum(migrantes_centro$prop)
migrantes_centro

# pero si se mira solo por jurisdiccion, los primeros 5 jurisdicciones son...
migrantes <- OA2021 %>% 
  filter(as.numeric(migracion) != 1) %>% 
  select(jurisdiccion) %>% group_by(jurisdiccion) %>% 
  count(name = "cantidad") %>%
  arrange(cantidad)
migrantes$prop <- (migrantes$cantidad/sum(migrantes$cantidad))*100
migrantes <- migrantes %>% arrange(desc(prop))
migrantes$frec_acumulada <- cumsum(migrantes$prop)
head(migrantes)

# El analisis seguira sobre el total de migrantes por el momento de aqui en adelante



# distribucion de migrantes por origen

origen <- OA2021 %>% filter(origen2 != "Otro" & origen2 != "Native" &
                              origen2 != "Chile" & origen2 != "Venezuela") %>% 
  select(origen1) %>% group_by(origen1) %>% 
  count(name = "cantidad") %>%
  arrange(cantidad)
origen$prop <- (origen$cantidad/sum(origen$cantidad))*100
origen <- origen %>% arrange(desc(prop))
origen$freq_acum <- cumsum(origen$prop)
origen


# grafico


ggplot(origen, aes(x = reorder(origen1, -cantidad), y = cantidad)) +
  geom_bar(stat = "identity", fill = "#0c99d6", color = "black") +
  geom_text(aes(label = paste0(cantidad, " (", round(prop, 1), "%)")),
            vjust = -0.5, color = "black", fontface = "bold", size = 3) +
  labs(x = "País", y = "Cantidad", title = "Distribución de migrantes e hijos de migrantes por país de origen") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_y_continuous(labels = scales::comma)  # Format y-axis labels with commas

    # distribucion de migrantes por origen2
origen2 <- OA2021 %>% filter(origen2 != "Otro" & origen2 != "Native" &
                               origen2 != "Chile" & origen2 != "Venezuela") %>% 
  select(origen2) %>% group_by(origen2) %>% 
  count(name = "cantidad") %>%
  arrange(cantidad)
origen2$prop <- (origen2$cantidad/sum(origen2$cantidad))*100
origen2 <- origen2 %>% arrange(desc(prop))
origen2$freq_acum <- cumsum(origen2$prop)
origen2

# grafico


ggplot(origen2, aes(x = reorder(origen2, -cantidad), y = cantidad)) +
  geom_bar(stat = "identity", fill = "#0c99d6", color = "black") +
  geom_text(aes(label = paste0(cantidad, " (", round(prop, 1), "%)")),
            vjust = -0.5, color = "black", fontface = "bold", size = 3) +
  labs(x = "País", y = "Cantidad", title = "Distribución de migrantes e hijos de migrantes por país de origen") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_y_continuous(labels = scales::comma)  # Format y-axis labels with commas

# vemos la distribucion de migrantes por sector de la escual (privado/publico)

sector <- migrantes %>% 
  select(sector, mpuntaje) %>% group_by(sector) %>% 
  summarise(cantidad = n(), mpuntaje_mean = mean(mpuntaje, na.rm=TRUE)) %>%
  arrange(cantidad)
sector$prop <- (sector$cantidad/sum(sector$cantidad))*100
sector <- sector %>% arrange(desc(prop))
sector

#lo graficamos
# El 75% de los migrantes asisten a Escuelas publicas
# Entre migrantes, se puede ver diferencia entre los puntajes de los tipos de escuelas
par(mfrow = c(1, 2))

 pie1 <- ggplot(sector, aes(x = "", y = cantidad, fill = sector)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  labs(title = "Distribución de los migrantes según sector de la Escuela") +
  theme_minimal() +
  theme(legend.position = "right") +
  geom_text(aes(label = paste0(cantidad, " (", scales::percent(cantidad/sum(cantidad)), ")\nPuntaje promedio: ", round(mpuntaje_mean,2))), 
            position = position_stack(vjust = 0.5), color = "gray3", size = 4) +
  scale_fill_brewer(palette = "Set3") +
  scale_fill_manual(values = c("darkviolet", "royalblue"))
 pie1

# chequeamos si la diferencia de puntajes (los que asisten a escuelas privadas tienen mayor puntaje) persiste a nivel global
# se puede ver que la diferencia persiste, así tambien como la distribucion proporcional (aconsejable realizar una prueba de
# hipotesis para comprobar si existe diferencia significativa)
Tsector <- OA2021 %>% 
  filter(as.numeric(migracion)==1) %>% 
  select(sector, mpuntaje) %>% group_by(sector) %>% 
  summarise(cantidad = n(), mpuntaje_mean = mean(mpuntaje, na.rm=TRUE)) %>%
  arrange(cantidad)
Tsector$prop <- (Tsector$cantidad/sum(Tsector$cantidad))*100
Tsector <- Tsector %>% arrange(desc(prop))
Tsector

pie2 <- ggplot(Tsector, aes(x = "", y = cantidad, fill = sector)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  labs(title = "Distribución de los natives según sector de la Escuela") +
  theme_minimal() +
  theme(legend.position = "right") +
  geom_text(aes(label = paste0(cantidad, " (", scales::percent(cantidad/sum(cantidad)),
                               ")\nPuntaje promedio: ", round(mpuntaje_mean,2))), 
            position = position_stack(vjust = 0.5), color = "gray3", size = 4) +
  scale_fill_brewer(palette = "Set3") +
  scale_fill_manual(values = c("darkviolet", "royalblue"))
pie2

# comparamos los graficos
grid.arrange(pie1,pie2, ncol = 2)

# miramos la distribucion de migrantes por Origen y Sector
sector_prop <- OA2021 %>% filter(origen2 != "Otro" & origen2 != "Native" &
                                   origen2 != "Chile" & origen2 != "Venezuela") %>% 
  group_by(origen1, sector) %>%
  summarise(count = n()) %>%
  mutate(prop = count / sum(count)) %>%
  group_by(origen1) %>%
  mutate(total_count = sum(count))

# a excepcion de Chile la proporcion de escuelas privadas es un poco mas alta en hijos de padres extrajeros
# que los estudiantes nacidos fuera del pais
desired_order <- c("Boliviano", "Hijo de bolivianos", "Paraguayo",
                   "Hijo de paraguayos", "Peruano","Hijo de peruanos")

sector_prop %>%
  filter(!is.na(origen1)) %>%
  mutate(origen1 = factor(origen1, levels = desired_order)) %>%
ggplot( aes(x = origen1, y = prop, fill = sector)) +
  geom_col(position = "fill") +
  labs(title = "Distribución de migrantes por Origen y Sector escolar",
       x = "",
       y = "Proporción") +
  scale_y_continuous(labels = scales::percent) +
  theme_minimal() +
  geom_text(aes(label = paste0(scales::percent(prop))), 
            position = position_fill(vjust = 0.5),
            size = 3) +
  geom_text(aes(label = paste0(total_count)),
            position = position_fill(vjust = 1.2),
            size = 4, fontface = "bold") + 
  theme(plot.title = element_text(hjust = 0.5))


  # miramos la distubucion por ambito

ambito <- migrantes %>% 
  select(ambito, mpuntaje) %>% group_by(ambito)%>% 
  summarise(cantidad = n(), mpuntaje_mean = mean(mpuntaje, na.rm=TRUE)) %>%
  arrange(cantidad)
ambito$prop <- (ambito$cantidad/sum(ambito$cantidad))*100
ambito <- ambito %>% arrange(desc(prop))
ambito

#lo graficamos
# El 95% de los migrantes asisten a Escuelas de zonas urbanas
pie1 <- ggplot(ambito, aes(x = "", y = cantidad, fill = ambito)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  labs(title = "Distribución de los migrantes según zona de residencia") +
  theme_minimal() +
  theme(legend.position = "right") +
  geom_text(aes(label = paste0(cantidad, " (", scales::percent(cantidad/sum(cantidad)), ")\nPuntaje promedio: ", round(mpuntaje_mean,2))), 
            position = position_stack(vjust = 0.5), color = "gray3", size = 4) +
  scale_fill_brewer(palette = "Set3") +
  scale_fill_manual(values = c("darkviolet", "royalblue"))
pie1

## verificamos graficamente tambien si los no-migrantes presentan esta distribucion

Tambito <- OA2021 %>% 
  filter(as.numeric(migracion)==1) %>% 
  select(ambito, mpuntaje) %>% group_by(ambito) %>% 
  summarise(cantidad = n(), mpuntaje_mean = mean(mpuntaje, na.rm=TRUE)) %>%
  arrange(cantidad)
Tambito$prop <- (Tambito$cantidad/sum(Tambito$cantidad))*100
Tambito <- Tambito %>% arrange(desc(prop))
Tambito

pie2 <- ggplot(Tambito, aes(x = "", y = cantidad, fill = ambito)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  labs(title = "Distribución de los natives según zona de residencia") +
  theme_minimal() +
  theme(legend.position = "right") +
  geom_text(aes(label = paste0(cantidad, " (", scales::percent(cantidad/sum(cantidad)), ")\nPuntaje promedio: ", round(mpuntaje_mean,2))), 
            position = position_stack(vjust = 0.5), color = "gray3", size = 4) +
  scale_fill_brewer(palette = "Set3") +
  scale_fill_manual(values = c("darkviolet", "royalblue"))
pie2

# comparamos ambos graficos migrantes/no-migrantes
grid.arrange(pie1,pie2, ncol = 2)

  # comparemos los notas por NSE
# El NSE comprende las siguientes variables: nivel educativo padres, hacinamiento en el hogar, tenencia de bienes/servicios

rm(list = setdiff(ls(), c("migrantes","natives","OA2021")))

# migrantes Y puntaje de matematica

nse <- migrantes %>% 
  select(NSE_nivel, mpuntaje) %>% group_by(NSE_nivel)%>% 
  summarise(cantidad = n(), mpuntaje_mean = mean(mpuntaje, na.rm=TRUE)) %>%
  arrange(cantidad)
nse <- nse %>% arrange(NSE_nivel)
nse

# natives y putanje de matematica

natives_nse <- natives %>% 
  select(NSE_nivel, mpuntaje) %>% group_by(NSE_nivel)%>% 
  summarise(cantidad = n(), mpuntaje_mean = mean(mpuntaje, na.rm=TRUE)) %>%
  arrange(cantidad)
natives_nse <- natives_nse %>% arrange(NSE_nivel)
natives_nse

# comparamos los puntajes
# para ambos grupos se puede ver que el NSE y los puntajes tienen una relacion lineal positiva

# tambien se puede ver que para el nivel medio de NSE, los puntajes de ambos grupos practicamente convergen

nse %>% 
  filter(!is.na(NSE_nivel)) %>% 
  ggplot(aes(x = NSE_nivel, y = mpuntaje_mean)) +
  geom_point( aes(color = "Migrantes"), size = 3) +
  geom_point(data = natives_nse %>% filter(!is.na(NSE_nivel)), aes(color = "Natives"), size = 3) +
  labs(x = "Nivel socieconómico", y = "Puntaje promedio de matemáticas",
       title = "Comparación de NSE entre migrantes/natives y sus puntajes promedio en matematica") +
  theme_minimal() +
  scale_color_manual(values = c(Migrantes = "darkviolet", Natives = "royalblue"),
                     labels = c(Migrantes = "Migrantes", Natives = "Natives")) +
  guides(color = guide_legend(title = "Configuración migratoria"))

quartiles <- by(migrantes$mpuntaje, migrantes$NSE_nivel, 
                function(x) round(quantile(x, probs = c(0.25, 0.5, 0.75), na.rm=TRUE),2))
means <- tapply(migrantes$mpuntaje, migrantes$NSE_nivel, function(x) round(mean(x, na.rm = TRUE), 2))



par(mfrow = c(1,1))
  
boxplot(mpuntaje ~ NSE_nivel, data = migrantes,
        main = "Variación de los puntajes de matemática según NSE",
        xlab = "Nivel socioeconómico",
        ylab = "Puntajes",
        col = "lightblue",
        border = "black")

# Add quartile values as text annotations
text(1, quartiles[["Bajo"]][1], paste("Q1 =", quartiles[["Bajo"]][1]), pos = 1)
text(1, quartiles[["Bajo"]][2], paste("Median =", quartiles[["Bajo"]][2]), pos = 1)
text(1, quartiles[["Bajo"]][3], paste("Q3 =", quartiles[["Bajo"]][3]), pos = 3)
text(1, means[["Bajo"]], paste("Mean =", means[["Bajo"]]), pos = 3)
points(1, means[["Bajo"]], pch = 19)

text(2, quartiles[["Medio"]][1], paste("Q1 =", quartiles[["Medio"]][1]), pos = 1)
text(2, quartiles[["Medio"]][2], paste("Median =", quartiles[["Medio"]][2]), pos = 1)
text(2, quartiles[["Medio"]][3], paste("Q3 =", quartiles[["Medio"]][3]), pos = 3)
text(2, means[["Medio"]], paste("Mean =", means[["Medio"]]), pos = 3)
points(2, means[["Medio"]], pch = 19)

text(3, quartiles[["Alto"]][1], paste("Q1 =", quartiles[["Alto"]][1]), pos = 1)
text(3, quartiles[["Alto"]][2], paste("Median =", quartiles[["Alto"]][2]), pos = 1)
text(3, quartiles[["Alto"]][3], paste("Q3 =", quartiles[["Alto"]][3]), pos = 3)
text(3, means[["Alto"]], paste("Mean =", means[["Alto"]]), pos = 3)
points(3, means[["Alto"]], pch = 19)

# Nivel educativo de los padres 
# primero miramos la proporcion de NA's de cada padre
# El nivel del padre tiene muchos más valores perdidos que de las madres.

# madre
mean(is.na(migrantes$Nivel_Ed_Madre))
# padre
mean(is.na(migrantes$Nivel_Ed_Padre))

# madre -- nem: nivel educativo madre
# graficamos 
# Calculamos la cantidad de casos por categoria y su proporcion

category_counts <- table(migrantes$Nivel_Ed_Madre)
category_proportions <- prop.table(category_counts)

# Creamos un dataset de lo anterior y agregamos el % acumulado

nem <- data.frame(Category = names(category_counts),
                   Count = category_counts,
                   Proportion = category_proportions) %>% arrange(desc(Proportion.Freq))
nem$Category <- factor(nem$Category,
                       levels = levels(migrantes$Nivel_Ed_Madre))
nem <- nem %>% arrange(Category)
nem$cumulated <- cumsum(nem$Proportion.Freq)

# en el grafico se puede ver que aprox el 40% de las madres no terminamos el secundario


bar1 <- ggplot(nem, aes(x = Category, y = Count.Freq, fill = Category)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = paste0(Count.Freq, " (", scales::percent(Proportion.Freq), ")")),
            vjust = -0.5, size = 3) +
  geom_text(aes(label = paste0("% acumulado: ", scales::percent(cumulated))),
            position = position_stack(vjust = 0.5), size = 3) +
  labs(x = "", y = "Frecuencia",
       title = "Grafico de barras del Nivel educativo de la madre",
       subtitle = "Frecuencia y proporción de cada categoría") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))
bar1

## repetemis lo mismo para los padres

# Calculamos la cantidad de casos por categoria y su proporcion

category_counts <- table(migrantes$Nivel_Ed_Padre)
category_proportions <- prop.table(category_counts)

# Creamos un dataset de lo anterior y agregamos el % acumulado

nep <- data.frame(Category = names(category_counts),
                  Count = category_counts,
                  Proportion = category_proportions) %>% arrange(desc(Proportion.Freq))
nep$Category <- factor(nep$Category,
                       levels = levels(migrantes$Nivel_Ed_Padre))
nep <- nep %>% arrange(Category)
nep$cumulated <- cumsum(nep$Proportion.Freq)

# en el grafico se puede ver que aprox el 40% de las madres no terminamos el secundario
# aproximadamente el 40% de padres tampoco terminó el secundario pero esta variable tiene mas NA's

bar2 <- ggplot(nep, aes(x = Category, y = Count.Freq, fill = Category)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = paste0(Count.Freq, " (", scales::percent(Proportion.Freq), ")")),
            vjust = -0.5, size = 3) +
  geom_text(aes(label = paste0("% acumulado: ", scales::percent(cumulated))),
            position = position_stack(vjust = 0.5), size = 3) +
  labs(x = "Nivel Educativo", y = "Frecuencia",
       title = "Grafico de barras del Nivel educativo del padre",
       subtitle = "Frecuencia y proporción de cada categoría") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))


# comparamos los graficos
grid.arrange(bar1,bar2, nrow = 2)

## verificamos los valores perdidos de la variabel nivel educativo de los padres
mean(is.na(migrantes$Nivel_Ed_MyP))

# Calculamos la cantidad de casos por categoria y su proporcion

category_counts <- table(migrantes$Nivel_Ed_MyP)
category_proportions <- prop.table(category_counts)

# Creamos un dataset de lo anterior y agregamos el % acumulado

nepm <- data.frame(Category = names(category_counts),
                  Count = category_counts,
                  Proportion = category_proportions) %>% arrange(desc(Proportion.Freq))
nepm$Category <- factor(nepm$Category,
                       levels = levels(migrantes$Nivel_Ed_MyP))
nepm <- nepm %>% arrange(Category)
nepm$cumulated <- cumsum(nepm$Proportion.Freq)

# Exluyendo el 20% de NA's aprox
# aprox el 33% de NINGUN padre termino el secundario

ggplot(nepm, aes(x = Category, y = Count.Freq, fill = Category)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = paste0(Count.Freq, " (", scales::percent(Proportion.Freq), ")")),
            vjust = -0.5, size = 3) +
  geom_text(aes(label = paste0("% acumulado: ", scales::percent(cumulated))),
            position = position_stack(vjust = 0.5), size = 3) +
  labs(x = "Nivel Educativo", y = "Frecuencia",
       title = "Grafico de barras del Nivel educativo de los padres",
       subtitle = "Frecuencia y proporción de cada categoría") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))

# comparemos las notas de matematica con el nivel educativo de los padres


# se puede notar un pequeño aumento en las medias de los putanjes segun el nivel educativo
# la categoria "completo-incompleto" podria no aportar mucho en terminos de diferencias
# para analisis del modelo posterior, podria unificarse

par(mfrow = c(2,1))

means <- tapply(migrantes$mpuntaje, migrantes$Nivel_Ed_MyP, 
                function(x) round(mean(x, na.rm = TRUE), 2))

boxplot(mpuntaje ~ Nivel_Ed_MyP, data = migrantes,
        main = "Puntajes de matemática según nivel educativo de los padres",
        xlab = "",
        ylab = "Puntajes",
        col = "lightblue",
        border = "black")

mtext("Migrantes", line = 0, cex = 1.2)

text(1, means[["No fue a la escuela"]], paste("Mean =", means[["No fue a la escuela"]]), pos = 3)
points(1, means[["No fue a la escuela"]], pch = 19)

text(2, means[["Primario incompleto"]], paste("Mean =", means[["Primario incompleto"]]), pos = 3)
points(2, means[["Primario incompleto"]], pch = 19)

text(3, means[["Primario completo"]], paste("Mean =", means[["Primario completo"]]), pos = 3)
points(3, means[["Primario completo"]], pch = 19)

text(4, means[["Secundario incompleto"]], paste("Mean =", means[["Secundario incompleto"]]), pos = 3)
points(4, means[["Secundario incompleto"]], pch = 19)

text(5, means[["Secundario completo"]], paste("Mean =", means[["Secundario completo"]]), pos = 3)
points(5, means[["Secundario completo"]], pch = 19)

text(6, means[["Universitario incompleto"]], paste("Mean =", means[["Universitario incompleto"]]), pos = 3)
points(6, means[["Universitario incompleto"]], pch = 19)

text(7, means[["Universitario completo"]], paste("Mean =", means[["Universitario completo"]]), pos = 3)
points(7, means[["Universitario completo"]], pch = 19)

text(8, means[["Posgrado"]], paste("Mean =", means[["Posgrado"]]), pos = 3)
points(8, means[["Posgrado"]], pch = 19)

# verificamos si el nivel educativo de los padres tiene el mismo comportamiento entre natives


means <- tapply(natives$mpuntaje, natives$Nivel_Ed_MyP, 
                function(x) round(mean(x, na.rm = TRUE), 2))

# se puede notar un pequeño aumento en las medias de los putanjes segun el nivel educativo
# la categoria "completo-incompleto" podria no aportar mucho en terminos de diferencias
# para analisis del modelo posterior, podria unificarse

## ambos grupos presentan un comportamiento lineal. En el grupo de natives podria verse mas marcado
# el motivo podria deberse a que el grupo tiene una muestra mas grande


boxplot(mpuntaje ~ Nivel_Ed_MyP, data = natives,
        xlab = "Nivel educativo",
        ylab = "Puntajes",
        col = "lightblue",
        border = "black")

mtext("Natives", line = 1, cex = 1.2)


text(1, means[["No fue a la escuela"]], paste("Mean =", means[["No fue a la escuela"]]), pos = 3)
points(1, means[["No fue a la escuela"]], pch = 19)

text(2, means[["Primario incompleto"]], paste("Mean =", means[["Primario incompleto"]]), pos = 3)
points(2, means[["Primario incompleto"]], pch = 19)

text(3, means[["Primario completo"]], paste("Mean =", means[["Primario completo"]]), pos = 3)
points(3, means[["Primario completo"]], pch = 19)

text(4, means[["Secundario incompleto"]], paste("Mean =", means[["Secundario incompleto"]]), pos = 3)
points(4, means[["Secundario incompleto"]], pch = 19)

text(5, means[["Secundario completo"]], paste("Mean =", means[["Secundario completo"]]), pos = 3)
points(5, means[["Secundario completo"]], pch = 19)

text(6, means[["Universitario incompleto"]], paste("Mean =", means[["Universitario incompleto"]]), pos = 3)
points(6, means[["Universitario incompleto"]], pch = 19)

text(7, means[["Universitario completo"]], paste("Mean =", means[["Universitario completo"]]), pos = 3)
points(7, means[["Universitario completo"]], pch = 19)

text(8, means[["Posgrado"]], paste("Mean =", means[["Posgrado"]]), pos = 3)
points(8, means[["Posgrado"]], pch = 19)


  # miramos el idioma que los migrantes mas hablan en sus casas
par(mfrow = c(1,1))

idioma <- migrantes %>% select(ap06, mpuntaje) %>% 
  group_by(ap06) %>% summarise(count = n(),
                               mpuntaje_mean = mean(mpuntaje, na.rm=TRUE),
                               mpuntaje_sd = sd(mpuntaje,na.rm=TRUE))
idioma$prop <- (idioma$count/ sum(idioma$count))
idioma

# el 80% de los migrantes hablan español

idioma %>% filter(!is.na(ap06)) %>% 
  ggplot( aes(x = ap06, y = count, fill = "MyColor")) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = paste0(count, " (", scales::percent(prop), ")")),
            vjust = -0.5, size = 4) +
  labs(x = "Idioma", y = "Frecuencia",
       title = "Idiomas que los migrantes utilizan la mayor parte del tiempo",
       subtitle = "Frecuencia y proporción") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5)) +
  scale_fill_manual(values = "darkviolet")  +
  guides(fill = FALSE) 

quartiles <- by(migrantes$mpuntaje, migrantes$ap06, 
                function(x) quantile(x, probs = c(0.25, 0.5, 0.75), na.rm=TRUE))

# Pareciera haber diferencias en las notas para los que hablan un idioma diferente
# pero 20% podria no ser representativo

boxplot(mpuntaje ~ ap06, data = migrantes,
        main = "Puntajes de matemática de los migrantes por idioma que habla",
        xlab = "Idioma",
        ylab = "Puntajes",
        col = "lightblue",
        border = "black")


text(1, quartiles[["Español"]][2], paste("Median =", quartiles[["Español"]][2]), pos = 1)
text(2, quartiles[["Lenguas indígenas"]][2], paste("Median =", quartiles[["Lenguas indígenas"]][2]), pos = 1)
text(3, quartiles[["Portugués"]][2], paste("Median =", quartiles[["Portugués"]][2]), pos = 1)
text(4, quartiles[["Inglés"]][2], paste("Median =", quartiles[["Inglés"]][2]), pos = 1)
text(5, quartiles[["Otros"]][2], paste("Median =", quartiles[["Otros"]][2]), pos = 1)

rm(list = setdiff(ls(), c("migrantes","natives","OA2021")))


# actividades en la casa

## cuidar familiar
# el 17% aprox no tienen respuesta

print(paste0("El porcentaje aprox de NA's es: ", scales::percent((mean(is.na(migrantes$ap19a))))))

cuidar_familiar <- migrantes %>% select(ap19a, mpuntaje) %>% 
  group_by(ap19a) %>% summarise(count = n(),
                               mpuntaje_mean = mean(mpuntaje, na.rm=TRUE),
                               mpuntaje_sd = sd(mpuntaje,na.rm=TRUE))
cuidar_familiar$prop <- (cuidar_familiar$count/ sum(cuidar_familiar$count))
cuidar_familiar

a <- cuidar_familiar %>% na.omit() %>% 
  ggplot(aes(x = ap19a, y = prop, fill = "MyColor")) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = paste0(scales::percent(prop), " (", count, ")")),
            vjust = -0.5, size = 4) +
  # Add secondary y-axis for mpuntaje_mean variable
  scale_y_continuous(labels = scales::percent, limits = c(0,1),
                     sec.axis = sec_axis(~ (. * 100) + 400, name = "")) +
  geom_point(aes(x = ap19a, y = (mpuntaje_mean - 400) / 100), color = "black", size = 4) +
  geom_text(aes(label = round(mpuntaje_mean, 2), y = (mpuntaje_mean - 400) / 100),
            color = "black", size = 4, vjust = -1.5) +
  labs(x = "¿Con qué frecuencia cuida a un familiar?", y = "Cantidad",
       title = "Cantidad de estudiantes según la frecuencia que cuida a un familiar",
       subtitle = "MIGRANTES") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5)) +
  scale_fill_manual(values = "darkviolet") +
  guides(fill = FALSE)

a

# NATIVES - CUIDADO DE FAMILIARES
print(paste0("El porcentaje aprox de NA's es: ", scales::percent((mean(is.na(natives$ap19a))))))

cuidar_familiar <- natives %>% select(ap19a, mpuntaje) %>% 
  group_by(ap19a) %>% summarise(count = n(),
                                mpuntaje_mean = mean(mpuntaje, na.rm=TRUE),
                                mpuntaje_sd = sd(mpuntaje,na.rm=TRUE))
cuidar_familiar$prop <- (cuidar_familiar$count/ sum(cuidar_familiar$count))
cuidar_familiar

b <- cuidar_familiar %>% na.omit() %>% 
  ggplot(aes(x = ap19a, y = prop, fill = "MyColor")) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = paste0(scales::percent(prop), " (", count, ")")),
            vjust = -0.5, size = 4) +
  # Add secondary y-axis for mpuntaje_mean variable
  scale_y_continuous(labels = scales::percent, limits = c(0,1),
                     sec.axis = sec_axis(~ (. * 100) + 400, name = "")) +
  geom_point(aes(x = ap19a, y = (mpuntaje_mean - 400) / 100), color = "black", size = 4) +
  geom_text(aes(label = round(mpuntaje_mean, 2), y = (mpuntaje_mean - 400) / 100),
            color = "black", size = 4, vjust = -1.5) +
  labs(x = "¿Con qué frecuencia cuida a un familiar?", y = "",
       title = "Cantidad de estudiantes según la frecuencia que cuida a un familiar",
       subtitle = "NATIVES") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5)) +
  scale_fill_manual(values = "royalblue") +
  guides(fill = FALSE)

b
## ambos grupos prensentan comportamientos parecidos
grid.arrange(a,b, ncol = 2)

## tareas del hogar
# MIGRANTES
print(paste0("El porcentaje aprox de NA's es: ", scales::percent((mean(is.na(migrantes$ap19b))))))

tareas_hogar <- migrantes %>% select(ap19b, mpuntaje) %>% 
  group_by(ap19b) %>% summarise(count = n(),
                                mpuntaje_mean = mean(mpuntaje, na.rm=TRUE),
                                mpuntaje_sd = sd(mpuntaje,na.rm=TRUE))
tareas_hogar$prop <- (tareas_hogar$count/ sum(tareas_hogar$count))
tareas_hogar

a <- tareas_hogar %>% na.omit() %>% 
ggplot(aes(x = ap19b, y = prop, fill = "MyColor")) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = paste0(scales::percent(prop), " (", count, ")")),
            vjust = -0.5, size = 4) +
  # Add secondary y-axis for mpuntaje_mean variable
  scale_y_continuous(labels = scales::percent, limits = c(0,1),
                     sec.axis = sec_axis(~ (. * 100) + 400, name = "")) +
  geom_point(aes(x = ap19b, y = (mpuntaje_mean - 400) / 100), color = "black", size = 4) +
  geom_text(aes(label = round(mpuntaje_mean, 2), y = (mpuntaje_mean - 400) / 100),
            color = "black", size = 4, vjust = -1.5) +
  labs(x = "¿Con qué frecuencia realiza tareas del hogar?", y = "Cantidad",
       title = "Cantidad de estudiantes según la frecuencia que realiza tareas del hogar",
       subtitle = "MIGRANTES") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5)) +
  scale_fill_manual(values = "darkviolet") +
  guides(fill = FALSE)

a

# NATIVES
print(paste0("El porcentaje aprox de NA's es: ", scales::percent((mean(is.na(natives$ap19b))))))

tareas_hogar <- natives %>% select(ap19b, mpuntaje) %>% 
  group_by(ap19b) %>% summarise(count = n(),
                                mpuntaje_mean = mean(mpuntaje, na.rm=TRUE),
                                mpuntaje_sd = sd(mpuntaje,na.rm=TRUE))
tareas_hogar$prop <- (tareas_hogar$count/ sum(tareas_hogar$count))
tareas_hogar

b <- tareas_hogar %>% na.omit() %>% 
  ggplot(aes(x = ap19b, y = prop, fill = "MyColor")) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = paste0(scales::percent(prop), " (", count, ")")),
            vjust = -0.5, size = 4) +
  # Add secondary y-axis for mpuntaje_mean variable
  scale_y_continuous(labels = scales::percent, limits = c(0,1),
                     sec.axis = sec_axis(~ (. * 100) + 400, name = "Puntajes promedio de matemática")) +
  geom_point(aes(x = ap19b, y = (mpuntaje_mean - 400) / 100), color = "black", size = 4) +
  geom_text(aes(label = round(mpuntaje_mean, 2), y = (mpuntaje_mean - 400) / 100),
            color = "black", size = 4, vjust = -1.5) +
  labs(x = "¿Con qué frecuencia realiza tareas del hogar?", y = "",
       title = "Cantidad de estudiantes según la frecuencia que realiza tareas del hogar",
       subtitle = "NATIVES") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5)) +
  scale_fill_manual(values = "royalblue") +
  guides(fill = FALSE)

b
# tambien presentan comportamientos parecidos entre grupos
grid.arrange(a,b, ncol=2)

## Trabajo la tierra, cultivo, cosecho en la huerta o cuido animales de granja para consumir en mi casa
# MIGRANTES
print(paste0("El porcentaje aprox de NA's es: ", scales::percent((mean(is.na(migrantes$ap19c))))))

tierra_cultivo <- migrantes %>% select(ap19c, mpuntaje) %>% 
  group_by(ap19c) %>% summarise(count = n(),
                                mpuntaje_mean = mean(mpuntaje, na.rm=TRUE),
                                mpuntaje_sd = sd(mpuntaje,na.rm=TRUE))
tierra_cultivo$prop <- (tierra_cultivo$count/ sum(tierra_cultivo$count))
tierra_cultivo

a <- tierra_cultivo %>% na.omit() %>% 
  ggplot(aes(x = ap19c, y = prop, fill = "MyColor")) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = paste0(scales::percent(prop), " (", count, ")")),
            vjust = -0.5, size = 4) +
  # Add secondary y-axis for mpuntaje_mean variable
  scale_y_continuous(labels = scales::percent, limits = c(0,1),
                     sec.axis = sec_axis(~ (. * 150) + 400, name = "")) +
  geom_point(aes(x = ap19c, y = (mpuntaje_mean - 400) / 150), color = "black", size = 4) +
  geom_text(aes(label = round(mpuntaje_mean, 2), y = (mpuntaje_mean - 400) / 150),
            color = "black", size = 4, vjust = -1.5) +
  labs(x = "¿Con qué frecuencia realiza tareas del hogar?", y = "Cantidad",
       title = "Cantidad de estudiantes según la frecuencia que realiza tareas del hogar",
       subtitle = "MIGRANTES") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5)) +
  scale_fill_manual(values = "darkviolet") +
  guides(fill = FALSE)

a

# NATIVES
print(paste0("El porcentaje aprox de NA's es: ", scales::percent((mean(is.na(natives$ap19c))))))

tierra_cultivo <- natives %>% select(ap19c, mpuntaje) %>% 
  group_by(ap19c) %>% summarise(count = n(),
                                mpuntaje_mean = mean(mpuntaje, na.rm=TRUE),
                                mpuntaje_sd = sd(mpuntaje,na.rm=TRUE))
tierra_cultivo$prop <- (tierra_cultivo$count/ sum(tierra_cultivo$count))
tierra_cultivo

b <- tierra_cultivo %>% na.omit() %>% 
  ggplot(aes(x = ap19c, y = prop, fill = "MyColor")) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = paste0(scales::percent(prop), " (", count, ")")),
            vjust = -0.5, size = 4) +
  # Add secondary y-axis for mpuntaje_mean variable
  scale_y_continuous(labels = scales::percent, limits = c(0,1),
                     sec.axis = sec_axis(~ (. * 150) + 400, name = "Puntaje promedio de matemática")) +
  geom_point(aes(x = ap19c, y = (mpuntaje_mean - 400) / 150), color = "black", size = 4) +
  geom_text(aes(label = round(mpuntaje_mean, 2), y = (mpuntaje_mean - 400) / 150),
            color = "black", size = 4, vjust = -1.5) +
  labs(x = "¿Con qué frecuencia realiza tareas del hogar?", y = "",
       title = "Cantidad de estudiantes según la frecuencia que realiza tareas del hogar",
       subtitle = "NATIVES") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5)) +
  scale_fill_manual(values = "royalblue") +
  guides(fill = FALSE)

b
## tambien se comportan de manera parecidas ambos grupos
grid.arrange(a,b, ncol = 2)

# comparamos los graficos
# la mayoria cuida de un familiar algunas veces, seguido por los que nunca lo hacen
cuidar_familiar

# la mayoria realizas las tareas algunos días de la semana
tareas_hogar

# la mayoria no realiza nunca las tareas de cultivo y demas para el consumo de la casa
tierra_cultivo

# las tres tareas mencionadas arriba coinciden en que los migrantes con frecuencias "algunos dias o nunca"
# tienen puntajes de matematica mas altas con respecto a los que realizas tareas "todos los dias o de L a V"
# como tienen comportamiento parecido, se sugiere utilizar la variable con menos NA's en ambos grupos


# medio de transporte que utiliza

print(paste0("El porcentaje aprox de NA's es: ", scales::percent((mean(is.na(migrantes$ap22))))))

par(mfrow = c(1,1))

medio_transporte <- migrantes %>% select(ap22, mpuntaje) %>% 
  group_by(ap22) %>% summarise(count = n(),
                                mpuntaje_mean = mean(mpuntaje, na.rm=TRUE),
                                mpuntaje_sd = sd(mpuntaje,na.rm=TRUE))
medio_transporte$prop <- (medio_transporte$count/ sum(medio_transporte$count))
medio_transporte

# la mayoria camina, seguido por la utilizacion de autos/motos
# el grafico no parece presentar una relacion linear entre el medio de transporte y las notas

medio_transporte %>% na.omit() %>% 
  ggplot(aes(x = reorder(ap22, desc(count)), y = count, fill = "MyColor")) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = paste0(count, " (", scales::percent(prop), ")")),
            vjust = -0.5, size = 4) +
  # Add secondary y-axis for mpuntaje_mean variable
  scale_y_continuous(sec.axis = sec_axis(~ (. / 100) + 300, name = "Puntaje promedio de matemática")) +
  geom_point(aes(x = ap22, y = (mpuntaje_mean - 300) * 100), color = "black", size = 4) +
  geom_text(aes(label = round(mpuntaje_mean, 2), y = (mpuntaje_mean - 300) * 100),
            color = "black", size = 4, vjust = -1.5) +
  labs(x = "Medio de transporte", y = "Nro de migrantes",
       title = "Cantidad de migrantes según el medio de transporte utilizado",
       subtitle = "Y el puntaje promedio de matemáticas para cada frecuencia") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5)) +
  scale_fill_manual(values = "darkviolet") +
  guides(fill = FALSE)

# Tiempo promedio que tarda en llegar a la escuela

# MIGRANTES
print(paste0("El porcentaje aprox de NA's es: ", scales::percent((mean(is.na(migrantes$ap23))))))

hs_transportandose <- migrantes %>% select(ap23, mpuntaje) %>% 
  group_by(ap23) %>% summarise(count = n(),
                               mpuntaje_mean = mean(mpuntaje, na.rm=TRUE),
                               mpuntaje_sd = sd(mpuntaje,na.rm=TRUE))
hs_transportandose$prop <- (hs_transportandose$count/ sum(hs_transportandose$count))
hs_transportandose

# la mayoria tarda menos de media hora (el 80%)
# Pareciera haber una relacion lineal donde cuando mas tarde el alumno en llegar a la escuela mas bajo sea el puntaje
# pero esos datos solon representan el 20% de los migrantes

a<- hs_transportandose %>% na.omit() %>% 
  ggplot(aes(x = ap23, y = prop, fill = "MyColor")) +
  geom_bar(stat = "identity")  +
  geom_text(aes(label = paste0(scales::percent(prop), " (", count, ")")),
            vjust = -0.5, size = 4) +
  # Add secondary y-axis for mpuntaje_mean variable
  scale_y_continuous(labels = scales::percent, limits = c(0,1),
                     sec.axis = sec_axis(~ (. *150) + 400, name = "")) +
  geom_point(aes(x = ap23, y = (mpuntaje_mean - 400) /150), color = "black", size = 4) +
  geom_text(aes(label = round(mpuntaje_mean, 2), y = (mpuntaje_mean - 400) /150),
            color = "black", size = 4, vjust = -1.5) +
  labs(x = "Hs transportandose", y = "Cantidad",
       title = "Cantidad de estudiantes según el tiempo que tarda en llegar a la escuela",
       subtitle = "MIGRANTES") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5)) +
  scale_fill_manual(values = "darkviolet") +
  guides(fill = FALSE)

a

# NATIVES
print(paste0("El porcentaje aprox de NA's es: ", scales::percent((mean(is.na(natives$ap23))))))

hs_transportandose <- natives %>% select(ap23, mpuntaje) %>% 
  group_by(ap23) %>% summarise(count = n(),
                               mpuntaje_mean = mean(mpuntaje, na.rm=TRUE),
                               mpuntaje_sd = sd(mpuntaje,na.rm=TRUE))
hs_transportandose$prop <- (hs_transportandose$count/ sum(hs_transportandose$count))
hs_transportandose

# la mayoria tarda menos de media hora (el 80%)
# Pareciera haber una relacion lineal donde cuando mas tarde el alumno en llegar a la escuela mas bajo sea el puntaje
# pero esos datos solon representan el 20% de los migrantes

b <- hs_transportandose %>% na.omit() %>% 
  ggplot(aes(x = ap23, y = prop, fill = "MyColor")) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = paste0(scales::percent(prop), " (", count, ")")),
            vjust = -0.5, size = 4) +
  # Add secondary y-axis for mpuntaje_mean variable
  scale_y_continuous(labels = scales::percent, limits = c(0,1),
                     sec.axis = sec_axis(~ (. *150) + 400, name = "Puntaje promedio de matemática")) +
  geom_point(aes(x = ap23, y = (mpuntaje_mean - 400) /150), color = "black", size = 4) +
  geom_text(aes(label = round(mpuntaje_mean, 2), y = (mpuntaje_mean - 400) /150),
            color = "black", size = 4, vjust = -1.5) +
  labs(x = "Hs transportandose", y = "",
       title = "Cantidad de estudiantes según el tiempo que tarda en llegar a la escuela",
       subtitle = "NATIVES") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5)) +
  scale_fill_manual(values = "royalblue") +
  guides(fill = FALSE)

b

  # ambos grupos presentan comportamientos parecidos
grid.arrange(a,b,ncol = 2)


## repite grado?

print(paste0("El porcentaje aprox de NA's es: ", scales::percent((mean(is.na(migrantes$ap25))))))


repite_grado <- migrantes %>% select(ap25, mpuntaje) %>% 
  group_by(ap25) %>% summarise(count = n(),
                               mpuntaje_mean = mean(mpuntaje, na.rm=TRUE),
                               mpuntaje_sd = sd(mpuntaje,na.rm=TRUE))
repite_grado$prop <- (repite_grado$count/ sum(repite_grado$count))
repite_grado

# la mayoria no ha repetido ningún grado
# Pero los que han repetido tienen un puntaje promedio más bajo

repite_grado %>% na.omit() %>% 
  ggplot(aes(x = ap25, y = count, fill = "MyColor")) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = paste0(count, " (", scales::percent(prop), ")")),
            vjust = -0.5, size = 4) +
  # Add secondary y-axis for mpuntaje_mean variable
  scale_y_continuous(sec.axis = sec_axis(~ (. / 100) + 300, name = "Puntaje promedio de matemática")) +
  geom_point(aes(x = ap25, y = (mpuntaje_mean - 300) * 100), color = "black", size = 4) +
  geom_text(aes(label = round(mpuntaje_mean, 2), y = (mpuntaje_mean - 300) * 100),
            color = "black", size = 4, vjust = -1.5) +
  labs(x = "Hs transportandose", y = "Nro de migrantes",
       title = "Cantidad de migrantes según el tiempo que tarda en llegar a la escuela",
       subtitle = "Y el puntaje promedio de matemáticas para cada frecuencia") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5)) +
  scale_fill_manual(values = "darkviolet") +
  guides(fill = FALSE)

rm(list = setdiff(ls(), c("migrantes","natives","OA2021")))


## asistencia 2020
# MIGRANTES
print(paste0("El porcentaje aprox de NA's es: ", scales::percent((mean(is.na(migrantes$ap26a))))))


asistencia2020 <- migrantes %>% select(ap26a, mpuntaje) %>% 
  group_by(ap26a) %>% summarise(count = n(),
                               mpuntaje_mean = mean(mpuntaje, na.rm=TRUE),
                               mpuntaje_sd = sd(mpuntaje,na.rm=TRUE))
asistencia2020$prop <- (asistencia2020$count/ sum(asistencia2020$count))
asistencia2020



a<- asistencia2020 %>% na.omit() %>% 
  ggplot(aes(x = ap26a, y = prop, fill = "MyColor")) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = paste0(scales::percent(prop), " (", count, ")")),
            vjust = -0.5, size = 4) +
  # Add secondary y-axis for mpuntaje_mean variable
  scale_y_continuous(labels = scales::percent, limits = c(0,1),
                     sec.axis = sec_axis(~ (. * 150) + 400, name = "")) +
  geom_point(aes(x = ap26a, y = (mpuntaje_mean - 400) / 150), color = "black", size = 4) +
  geom_text(aes(label = round(mpuntaje_mean, 2), y = (mpuntaje_mean - 400)/ 150),
            color = "black", size = 4, vjust = -1.5) +
  labs(x = "Clases asistidas", y = "Cantidad",
       title = "Cantidad de estudiantes según las clases asistidas en 2020",
       subtitle = "MIGRANTES") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5)) +
  scale_fill_manual(values = "darkviolet") +
  guides(fill = FALSE)

a

# NATIVES

print(paste0("El porcentaje aprox de NA's es: ", scales::percent((mean(is.na(natives$ap26a))))))


asistencia2020 <- natives %>% select(ap26a, mpuntaje) %>% 
  group_by(ap26a) %>% summarise(count = n(),
                                mpuntaje_mean = mean(mpuntaje, na.rm=TRUE),
                                mpuntaje_sd = sd(mpuntaje,na.rm=TRUE))
asistencia2020$prop <- (asistencia2020$count/ sum(asistencia2020$count))
asistencia2020



b<- asistencia2020 %>% na.omit() %>% 
  ggplot(aes(x = ap26a, y = prop, fill = "MyColor")) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = paste0(scales::percent(prop), " (", count, ")")),
            vjust = -0.5, size = 4) +
  # Add secondary y-axis for mpuntaje_mean variable
  scale_y_continuous(labels = scales::percent, limits = c(0,1),
                     sec.axis = sec_axis(~ (. *150) + 400, name = "Puntaje promedio de matemática")) +
  geom_point(aes(x = ap26a, y = (mpuntaje_mean - 400) /150), color = "black", size = 4) +
  geom_text(aes(label = round(mpuntaje_mean, 2), y = (mpuntaje_mean - 400) /150),
            color = "black", size = 4, vjust = -1.5) +
  labs(x = "Clases asistidas", y = "",
       title = "Cantidad de estudiantes según las clases asistidas en 2020",
       subtitle = "NATIVES") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5)) +
  scale_fill_manual(values = "royalblue") +
  guides(fill = FALSE)

b

# comportamientos parecidos
grid.arrange(a,b, ncol = 2)

  ## asistencia 2021

# MIGRANTES

print(paste0("El porcentaje aprox de NA's es: ", scales::percent((mean(is.na(migrantes$ap26b))))))


asistencia2021 <- migrantes %>% select(ap26b, mpuntaje) %>% 
  group_by(ap26b) %>% summarise(count = n(),
                                mpuntaje_mean = mean(mpuntaje, na.rm=TRUE),
                                mpuntaje_sd = sd(mpuntaje,na.rm=TRUE))
asistencia2021$prop <- (asistencia2021$count/ sum(asistencia2021$count))
asistencia2021


a <- asistencia2021 %>% na.omit() %>% 
  ggplot(aes(x = ap26b, y = prop, fill = "MyColor")) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = paste0(scales::percent(prop), " (", count, ")")),
            vjust = -0.5, size = 4) +
  # Add secondary y-axis for mpuntaje_mean variable
  scale_y_continuous(labels = scales::percent, limits = c(0,1),
                     sec.axis = sec_axis(~ (. *150) + 400, name = "")) +
  geom_point(aes(x = ap26b, y = (mpuntaje_mean - 400) /150), color = "black", size = 4) +
  geom_text(aes(label = round(mpuntaje_mean, 2), y = (mpuntaje_mean - 400) /150),
            color = "black", size = 4, vjust = -1.5) +
  labs(x = "Clases asistidas", y = "Cantidad",
       title = "Cantidad de estudiantes según las clases asistidas en 2021",
       subtitle = "MIGRANTES") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5)) +
  scale_fill_manual(values = "darkviolet") +
  guides(fill = FALSE)

a

# NATIVES
print(paste0("El porcentaje aprox de NA's es: ", scales::percent((mean(is.na(natives$ap26b))))))


asistencia2021 <- natives %>% select(ap26b, mpuntaje) %>% 
  group_by(ap26b) %>% summarise(count = n(),
                                mpuntaje_mean = mean(mpuntaje, na.rm=TRUE),
                                mpuntaje_sd = sd(mpuntaje,na.rm=TRUE))
asistencia2021$prop <- (asistencia2021$count/ sum(asistencia2021$count))
asistencia2021


b <- asistencia2021 %>% na.omit() %>% 
  ggplot(aes(x = ap26b, y = prop, fill = "MyColor")) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = paste0(scales::percent(prop), " (", count, ")")),
            vjust = -0.5, size = 4) +
  # Add secondary y-axis for mpuntaje_mean variable
  scale_y_continuous(labels = scales::percent, limits = c(0,1),
                     sec.axis = sec_axis(~ (. *150) + 400, name = "Puntaje promedio de matemática")) +
  geom_point(aes(x = ap26b, y = (mpuntaje_mean - 400) /150), color = "black", size = 4) +
  geom_text(aes(label = round(mpuntaje_mean, 2), y = (mpuntaje_mean - 400) /150),
            color = "black", size = 4, vjust = -1.5) +
  labs(x = "Clases asistidas", y = "",
       title = "Cantidad de estudiantes según las clases asistidas en 2021",
       subtitle = "NATIVES") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5)) +
  scale_fill_manual(values = "royalblue") +
  guides(fill = FALSE)

b

# comportamiento parecido entre grupos
grid.arrange(a,b,ncol= 2)




# ap33: les gusta ira la escuela?

# MIGRANTES

print(paste0("El porcentaje aprox de NA's es: ", scales::percent((mean(is.na(migrantes$ap32))))))


gustar_escuela <- migrantes %>% select(ap32, mpuntaje) %>% 
  group_by(ap32) %>% summarise(count = n(),
                                mpuntaje_mean = mean(mpuntaje, na.rm=TRUE),
                                mpuntaje_sd = sd(mpuntaje,na.rm=TRUE))
gustar_escuela$prop <- (gustar_escuela$count/ sum(gustar_escuela$count))
gustar_escuela

# a la mayoria les gusta ir a la escuela
# no pareciera haber relacion lineal con el gusto con el p untaje en matematica

a<- gustar_escuela %>% na.omit() %>% 
  ggplot(aes(x = ap32, y = prop, fill = "MyColor")) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = paste0(scales::percent(prop), " (", count, ")")),
            vjust = -0.5, size = 4) +
  # Add secondary y-axis for mpuntaje_mean variable
  scale_y_continuous(labels = scales::percent, limits = c(0,1),
                     sec.axis = sec_axis(~ (. *150) + 400, name = "")) +
  geom_point(aes(x = ap32, y = (mpuntaje_mean - 400) /150), color = "black", size = 4) +
  geom_text(aes(label = round(mpuntaje_mean, 2), y = (mpuntaje_mean - 400) /150),
            color = "black", size = 4, vjust = -1.5) +
  labs(x = "", y = "Cantidad",
       title = "Cantidad de estudiantes según su gusto por ir a la Escuela",
       subtitle = "MIGRANTES") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5)) +
  scale_fill_manual(values = "darkviolet") +
  guides(fill = FALSE)

a

# NATIVES

print(paste0("El porcentaje aprox de NA's es: ", scales::percent((mean(is.na(natives$ap32))))))


gustar_escuela <- natives %>% select(ap32, mpuntaje) %>% 
  group_by(ap32) %>% summarise(count = n(),
                               mpuntaje_mean = mean(mpuntaje, na.rm=TRUE),
                               mpuntaje_sd = sd(mpuntaje,na.rm=TRUE))
gustar_escuela$prop <- (gustar_escuela$count/ sum(gustar_escuela$count))
gustar_escuela

# a la mayoria les gusta ir a la escuela
# no pareciera haber relacion lineal con el gusto con el p untaje en matematica

b <- gustar_escuela %>% na.omit() %>% 
  ggplot(aes(x = ap32, y = prop, fill = "MyColor")) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = paste0(scales::percent(prop), " (", count, ")")),
            vjust = -0.5, size = 4) +
  # Add secondary y-axis for mpuntaje_mean variable
  scale_y_continuous(labels = scales::percent, limits = c(0,1),
                     sec.axis = sec_axis(~ (. *150) + 400, name = "Puntaje promedio de matemática")) +
  geom_point(aes(x = ap32, y = (mpuntaje_mean - 400) /150), color = "black", size = 4) +
  geom_text(aes(label = round(mpuntaje_mean, 2), y = (mpuntaje_mean - 400) /150),
            color = "black", size = 4, vjust = -1.5) +
  labs(x = "", y = "",
       title = "Cantidad de estudiantes según su gusto por ir a la Escuela",
       subtitle = "NATIVES") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5)) +
  scale_fill_manual(values = "royalblue") +
  guides(fill = FALSE)

b

# ambos grupos con distribuciones parecidas
grid.arrange(a,b,ncol = 2)

# con cuantos compañeres se lleva bien?
# MIGRANTES

print(paste0("El porcentaje aprox de NA's es: ", scales::percent((mean(is.na(migrantes$ap33))))))


relacion_compañeres <- migrantes %>% select(ap33, mpuntaje) %>% 
  group_by(ap33) %>% summarise(count = n(),
                               mpuntaje_mean = mean(mpuntaje, na.rm=TRUE),
                               mpuntaje_sd = sd(mpuntaje,na.rm=TRUE))
relacion_compañeres$prop <- (relacion_compañeres$count/ sum(relacion_compañeres$count))
relacion_compañeres

# la mayoria se lleva bien con todos o la mayoria de sus compañeros
# capaz se podria analizar si es significativo el puntaje promedio para los que no se llevan bien con nadie

a<- relacion_compañeres %>% na.omit() %>% 
  ggplot(aes(x = ap33, y = prop, fill = "MyColor")) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = paste0(scales::percent(prop), " (", count, ")")),
            vjust = -0.5, size = 4) +
  # Add secondary y-axis for mpuntaje_mean variable
  scale_y_continuous(labels = scales::percent, limits = c(0,1),
                     sec.axis = sec_axis(~ (. *150) + 400, name = "")) +
  geom_point(aes(x = ap33, y = (mpuntaje_mean - 400) /150), color = "black", size = 4) +
  geom_text(aes(label = round(mpuntaje_mean, 2), y = (mpuntaje_mean - 400) /150),
            color = "black", size = 4, vjust = -1.5) +
  labs(x = "", y = "Cantidad",
       title = "Cantidad de estudiantes según su gusto por ir a la Escuela",
       subtitle = "MIGRANTES") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5)) +
  scale_fill_manual(values = "darkviolet") +
  guides(fill = FALSE)

a

# NATIVES
print(paste0("El porcentaje aprox de NA's es: ", scales::percent((mean(is.na(natives$ap33))))))


relacion_compañeres <- natives %>% select(ap33, mpuntaje) %>% 
  group_by(ap33) %>% summarise(count = n(),
                               mpuntaje_mean = mean(mpuntaje, na.rm=TRUE),
                               mpuntaje_sd = sd(mpuntaje,na.rm=TRUE))
relacion_compañeres$prop <- (relacion_compañeres$count/ sum(relacion_compañeres$count))
relacion_compañeres

# la mayoria se lleva bien con todos o la mayoria de sus compañeros
# capaz se podria analizar si es significativo el puntaje promedio para los que no se llevan bien con nadie

b<- relacion_compañeres %>% na.omit() %>% 
  ggplot(aes(x = ap33, y = prop, fill = "MyColor")) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = paste0(scales::percent(prop), " (", count, ")")),
            vjust = -0.5, size = 4) +
  # Add secondary y-axis for mpuntaje_mean variable
  scale_y_continuous(labels = scales::percent, limits = c(0,1),
                     sec.axis = sec_axis(~ (. *150) + 400, name = "Puntaje promedio de matemática")) +
  geom_point(aes(x = ap33, y = (mpuntaje_mean - 400) /150), color = "black", size = 4) +
  geom_text(aes(label = round(mpuntaje_mean, 2), y = (mpuntaje_mean - 400) /150),
            color = "black", size = 4, vjust = -1.5) +
  labs(x = "", y = "",
       title = "Cantidad de estudiantes según su gusto por ir a la Escuela",
       subtitle = "NATIVES") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5)) +
  scale_fill_manual(values = "royalblue") +
  guides(fill = FALSE)

b

# los natives tienden a llevarse mejor con todos pero no es suficientes diferencia
# lo demas es parecido en cuanto a puntajes
grid.arrange(a,b,ncol=2)


rm(list = setdiff(ls(), c("migrantes","natives","OA2021")))

#  AP34: percepcion de bienestar en la escuela

# AP34a: en la escuela tengo amigos

# migrantes

print(paste0("El porcentaje aprox de NA's es: ", scales::percent((mean(is.na(migrantes$ap34a))))))


tiene_amigues <- migrantes %>% select(ap34a, mpuntaje) %>% 
  group_by(ap34a) %>% summarise(count = n(),
                               mpuntaje_mean = mean(mpuntaje, na.rm=TRUE),
                               mpuntaje_sd = sd(mpuntaje,na.rm=TRUE))
tiene_amigues$prop <- (tiene_amigues$count/ sum(tiene_amigues$count))
tiene_amigues

# la mmayoria opina que tiene amigos en la escuela
# En cuanto a puntajes, el promedio baja drasticamente con los que estan en desacuerdo

a <- tiene_amigues %>% na.omit() %>% 
  ggplot(aes(x = ap34a, y = prop, fill = "MyColor")) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = paste0(scales::percent(prop), " (", count, ")")),
            vjust = -0.5, size = 4) +
  # Add secondary y-axis for mpuntaje_mean variable
  scale_y_continuous(labels = scales::percent, limits = c(0,1),
                     sec.axis = sec_axis(~ (. * 100) + 400, name = "")) +
  geom_point(aes(x = ap34a, y = (mpuntaje_mean - 400) / 100), color = "black", size = 4) +
  geom_text(aes(label = round(mpuntaje_mean, 2), y = (mpuntaje_mean - 400) / 100),
  color = "black", size = 4, vjust = -1.5) +
  labs(x = "", y = "Cantidad",
       title = "Cantidad de estudiantes según su si tienen amigos en la escuela",
       subtitle = "MIGRANTES") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5)) +
  scale_fill_manual(values = "darkviolet") +
  guides(fill = FALSE)

a

# NATIVES

print(paste0("El porcentaje aprox de NA's es: ", scales::percent((mean(is.na(natives$ap34a))))))


tiene_amigues <- natives %>% select(ap34a, mpuntaje) %>% 
  group_by(ap34a) %>% summarise(count = n(),
                                mpuntaje_mean = mean(mpuntaje, na.rm=TRUE),
                                mpuntaje_sd = sd(mpuntaje,na.rm=TRUE))
tiene_amigues$prop <- (tiene_amigues$count/ sum(tiene_amigues$count))
tiene_amigues

# la mmayoria opina que tiene amigos en la escuela
# En cuanto a puntajes, el promedio baja drasticamente con los que estan en desacuerdo

b <- tiene_amigues %>% na.omit() %>% 
  ggplot(aes(x = ap34a, y = prop, fill = "MyColor")) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = paste0(scales::percent(prop), " (", count, ")")),
            vjust = -0.5, size = 4) +
  # Add secondary y-axis for mpuntaje_mean variable
  scale_y_continuous(labels = scales::percent, limits = c(0,1),
                     sec.axis = sec_axis(~ (. * 100) + 400, name = "Puntaje promedio de matemática")) +
  geom_point(aes(x = ap34a, y = (mpuntaje_mean - 400) / 100), color = "black", size = 4) +
  geom_text(aes(label = round(mpuntaje_mean, 2), y = (mpuntaje_mean - 400) / 100),
            color = "black", size = 4, vjust = -1.5) +
  labs(x = "", y = "",
       title = "Cantidad de estudiantes según su si tienen amigos en la escuela",
       subtitle = "NATIVES") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5)) +
  scale_fill_manual(values = "royalblue") +
  guides(fill = FALSE)

b

# comparacion
# a nivel proporcional, los natives tienen una unidad porcentual menos que los migrantes
# el comportamiento de puntajes es el mismo
grid.arrange(a,b,ncol=2)

# AP34B: SE SIENTE PARTE DE LA ESCUELA
# MIGRANTES

print(paste0("El porcentaje aprox de NA's es: ", scales::percent((mean(is.na(migrantes$ap34b))))))


parte_escuela <- migrantes %>% select(ap34b, mpuntaje) %>% 
  group_by(ap34b) %>% summarise(count = n(),
                                mpuntaje_mean = mean(mpuntaje, na.rm=TRUE),
                                mpuntaje_sd = sd(mpuntaje,na.rm=TRUE))
parte_escuela$prop <- (parte_escuela$count/ sum(parte_escuela$count))
parte_escuela

# En esta pregunta aumentan la proporcion de estudiantes en desacuerdo
# el puntaje promedio tambien disminuye un poco con respecto a las demas categorias

a <- parte_escuela %>% na.omit() %>% 
  ggplot(aes(x = ap34b, y = prop, fill = "MyColor")) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = paste0(scales::percent(prop), " (", count, ")")),
            vjust = -0.5, size = 4) +
  # Add secondary y-axis for mpuntaje_mean variable
  scale_y_continuous(labels = scales::percent, limits = c(0,1),
                     sec.axis = sec_axis(~ (. * 150) + 400, name = "")) +
  geom_point(aes(x = ap34b, y = (mpuntaje_mean - 400) / 150), color = "black", size = 4) +
  geom_text(aes(label = round(mpuntaje_mean, 2), y = (mpuntaje_mean - 400) / 150),
            color = "black", size = 4, vjust = -1.5) +
  labs(x = "", y = "Cantidad",
       title = "Cantidad de estudiantes según qué tan parte de la escuela se siente",
       subtitle = "MIGRANTES") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5)) +
  scale_fill_manual(values = "darkviolet") +
  guides(fill = FALSE)

a

# NATIVES
print(paste0("El porcentaje aprox de NA's es: ", scales::percent((mean(is.na(natives$ap34b))))))


parte_escuela <- natives %>% select(ap34b, mpuntaje) %>% 
  group_by(ap34b) %>% summarise(count = n(),
                                mpuntaje_mean = mean(mpuntaje, na.rm=TRUE),
                                mpuntaje_sd = sd(mpuntaje,na.rm=TRUE))
parte_escuela$prop <- (parte_escuela$count/ sum(parte_escuela$count))
parte_escuela

# En esta pregunta aumentan la proporcion de estudiantes en desacuerdo
# el puntaje promedio tambien disminuye un poco con respecto a las demas categorias

b <- parte_escuela %>% na.omit() %>% 
  ggplot(aes(x = ap34b, y = prop, fill = "MyColor")) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = paste0(scales::percent(prop), " (", count, ")")),
            vjust = -0.5, size = 4) +
  # Add secondary y-axis for mpuntaje_mean variable
  scale_y_continuous(labels = scales::percent, limits = c(0,1),
                     sec.axis = sec_axis(~ (. * 150) + 400, name = "Puntaje promedio de matemática")) +
  geom_point(aes(x = ap34b, y = (mpuntaje_mean - 400) / 150), color = "black", size = 4) +
  geom_text(aes(label = round(mpuntaje_mean, 2), y = (mpuntaje_mean - 400) / 150),
            color = "black", size = 4, vjust = -1.5) +
  labs(x = "", y = "",
       title = "Cantidad de estudiantes según qué tan parte de la escuela se siente",
       subtitle = "NATIVES") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5)) +
  scale_fill_manual(values = "royalblue") +
  guides(fill = FALSE)

b

# la distribucion porcentual para los estudiantes en desacuerdo tiene una brecha mas grande en esta variable
grid.arrange(a,b,ncol=2)

# AP34C: SIENTE EXCLUSIÓN EN LA ESCUELA
# MIGRANTES
# porcentaje de NA's muy alto
print(paste0("El porcentaje aprox de NA's es: ", scales::percent((mean(is.na(migrantes$ap34c))))))


siente_exclusion <- migrantes %>% select(ap34c, mpuntaje) %>% 
  group_by(ap34c) %>% summarise(count = n(),
                                mpuntaje_mean = mean(mpuntaje, na.rm=TRUE),
                                mpuntaje_sd = sd(mpuntaje,na.rm=TRUE))
siente_exclusion$prop <- (siente_exclusion$count/ sum(siente_exclusion$count))
siente_exclusion

# Tiene un alto porcentaje de NA's
# De los validos, el 20% esta de acuerdo que se le excluye
# pareciera haber una relacion lineal positiva 

a <- siente_exclusion %>% na.omit() %>% 
  ggplot(aes(x = ap34c, y = prop, fill = "MyColor")) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = paste0(scales::percent(prop), " (", count, ")")),
            vjust = -0.5, size = 4) +
  # Add secondary y-axis for mpuntaje_mean variable
  scale_y_continuous(labels = scales::percent, limits = c(0,1),
                     sec.axis = sec_axis(~ (. * 150) + 400, name = "")) +
  geom_point(aes(x = ap34c, y = (mpuntaje_mean - 400) / 150), color = "black", size = 4) +
  geom_text(aes(label = round(mpuntaje_mean, 2), y = (mpuntaje_mean - 400) / 150),
            color = "black", size = 4, vjust = -1.5) +
  labs(x = "", y = "Cantidad",
       title = "Cantidad de estudiantes según qué tan exlcuidos en la escuela se siente",
       subtitle = "MIGRANTES") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5)) +
  scale_fill_manual(values = "darkviolet") +
  guides(fill = FALSE)

a

# NATIVES

# porcentaje de NA's muy alto
print(paste0("El porcentaje aprox de NA's es: ", scales::percent((mean(is.na(natives$ap34c))))))


siente_exclusion <- natives %>% select(ap34c, mpuntaje) %>% 
  group_by(ap34c) %>% summarise(count = n(),
                                mpuntaje_mean = mean(mpuntaje, na.rm=TRUE),
                                mpuntaje_sd = sd(mpuntaje,na.rm=TRUE))
siente_exclusion$prop <- (siente_exclusion$count/ sum(siente_exclusion$count))
siente_exclusion

# Tiene un alto porcentaje de NA's
# De los validos, el 20% esta de acuerdo que se le excluye
# pareciera haber una relacion lineal positiva 

b <- siente_exclusion %>% na.omit() %>% 
  ggplot(aes(x = ap34c, y = prop, fill = "MyColor")) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = paste0(scales::percent(prop), " (", count, ")")),
            vjust = -0.5, size = 4) +
  # Add secondary y-axis for mpuntaje_mean variable
  scale_y_continuous(labels = scales::percent, limits = c(0,1),
                     sec.axis = sec_axis(~ (. * 150) + 400, name = "Puntaje promedio de matemática")) +
  geom_point(aes(x = ap34c, y = (mpuntaje_mean - 400) / 150), color = "black", size = 4) +
  geom_text(aes(label = round(mpuntaje_mean, 2), y = (mpuntaje_mean - 400) / 150),
            color = "black", size = 4, vjust = -1.5) +
  labs(x = "", y = "",
       title = "Cantidad de estudiantes según qué tan exlcuidos en la escuela se siente",
       subtitle = "NATIVES") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5)) +
  scale_fill_manual(values = "royalblue") +
  guides(fill = FALSE)

b


# la distribucion porcentual parece bajar algunos puntos para los que estan de acuerdo
# aun asi, el comportamiento en ese ambito y en el puntaje promedio son parecidos
grid.arrange(a,b,ncol=2)


# AP34d: SE SIENTE INCOMODO Y FUERA DE LUGAR
# MIGRANTES
# porcentaje de NA's muy alto
print(paste0("El porcentaje aprox de NA's es: ", scales::percent((mean(is.na(migrantes$ap34d))))))


fuera_lugar <- migrantes %>% select(ap34d, mpuntaje) %>% 
  group_by(ap34d) %>% summarise(count = n(),
                                mpuntaje_mean = mean(mpuntaje, na.rm=TRUE),
                                mpuntaje_sd = sd(mpuntaje,na.rm=TRUE))
fuera_lugar$prop <- (fuera_lugar$count/ sum(fuera_lugar$count))
fuera_lugar

# Tiene un alto porcentaje de NA's
# De los validos, el 14% se siento incomodo o fuera de lugar
# pareciera haber una relacion lineal positiva 

a <- fuera_lugar %>% na.omit() %>% 
  ggplot(aes(x = ap34d, y = prop, fill = "MyColor")) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = paste0(scales::percent(prop), " (", count, ")")),
            vjust = -0.5, size = 4) +
  # Add secondary y-axis for mpuntaje_mean variable
  scale_y_continuous(labels = scales::percent, limits = c(0,1),
                     sec.axis = sec_axis(~ (. * 150) + 400, name = "")) +
  geom_point(aes(x = ap34d, y = (mpuntaje_mean - 400) / 150), color = "black", size = 4) +
  geom_text(aes(label = round(mpuntaje_mean, 2), y = (mpuntaje_mean - 400) / 150),
            color = "black", size = 4, vjust = -1.5) +
  labs(x = "", y = "Cantidad",
       title = "Cantidad de estudiantes según qué tan 'incómodo/fuera de lugar' se siente en la escuela",
       subtitle = "MIGRANTES") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5)) +
  scale_fill_manual(values = "darkviolet") +
  guides(fill = FALSE)

a

# NATIVES
# porcentaje de NA's muy alto
print(paste0("El porcentaje aprox de NA's es: ", scales::percent((mean(is.na(natives$ap34d))))))


fuera_lugar <- natives %>% select(ap34d, mpuntaje) %>% 
  group_by(ap34d) %>% summarise(count = n(),
                                mpuntaje_mean = mean(mpuntaje, na.rm=TRUE),
                                mpuntaje_sd = sd(mpuntaje,na.rm=TRUE))
fuera_lugar$prop <- (fuera_lugar$count/ sum(fuera_lugar$count))
fuera_lugar

# Tiene un alto porcentaje de NA's
# De los validos, el 14% se siento incomodo o fuera de lugar
# pareciera haber una relacion lineal positiva 

b <- fuera_lugar %>% na.omit() %>% 
  ggplot(aes(x = ap34d, y = prop, fill = "MyColor")) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = paste0(scales::percent(prop), " (", count, ")")),
            vjust = -0.5, size = 4) +
  # Add secondary y-axis for mpuntaje_mean variable
  scale_y_continuous(labels = scales::percent, limits = c(0,1),
                     sec.axis = sec_axis(~ (. * 150) + 400, name = "Puntaje promedio de matemática")) +
  geom_point(aes(x = ap34d, y = (mpuntaje_mean - 400) / 150), color = "black", size = 4) +
  geom_text(aes(label = round(mpuntaje_mean, 2), y = (mpuntaje_mean - 400) / 150),
            color = "black", size = 4, vjust = -1.5) +
  labs(x = "", y = "",
       title = "Cantidad de estudiantes según qué tan 'incómodo/fuera de lugar' se siente en la escuela",
       subtitle = "NATIVES") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5)) +
  scale_fill_manual(values = "royalblue") +
  guides(fill = FALSE)

b

# El comportamiento entre grupos es parecido a la variable anterior 

grid.arrange(a,b,ncol=2)


# AP34E: SE SIENTE SOLO
# MIGRANTES
# porcentaje de NA's muy alto
print(paste0("El porcentaje aprox de NA's es: ", scales::percent((mean(is.na(migrantes$ap34e))))))


siente_solo <- migrantes %>% select(ap34e, mpuntaje) %>% 
  group_by(ap34e) %>% summarise(count = n(),
                                mpuntaje_mean = mean(mpuntaje, na.rm=TRUE),
                                mpuntaje_sd = sd(mpuntaje,na.rm=TRUE))
siente_solo$prop <- (siente_solo$count/ sum(siente_solo$count))
siente_solo

# El 33% son valores perdidos
# Mas del 50% esta en desacuerdo dejando 14% aprox de estudiantes con sensacion de soledad
# pareciera haber una relacion lineal negativa (cuanto mas de acuerdo mas bajo los puntajes promedios) 

a <- siente_solo %>% na.omit() %>% 
  ggplot(aes(x = ap34e, y = prop, fill = "MyColor")) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = paste0(scales::percent(prop), " (", count, ")")),
            vjust = -0.5, size = 4) +
  # Add secondary y-axis for mpuntaje_mean variable
  scale_y_continuous(labels = scales::percent, limits = c(0,1),
                     sec.axis = sec_axis(~ (. * 150) + 400, name = "")) +
  geom_point(aes(x = ap34e, y = (mpuntaje_mean - 400) / 150), color = "black", size = 4) +
  geom_text(aes(label = round(mpuntaje_mean, 2), y = (mpuntaje_mean - 400) / 150),
            color = "black", size = 4, vjust = -1.5) +
  labs(x = "", y = "Cantidad",
       title = "Cantidad de estudiantes según su sensación de soledad en la escuela",
       subtitle = "MIGRANTES") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5)) +
  scale_fill_manual(values = "darkviolet") +
  guides(fill = FALSE)

a

# NATIVES
# porcentaje de NA's muy alto
print(paste0("El porcentaje aprox de NA's es: ", scales::percent((mean(is.na(natives$ap34e))))))


siente_solo <- natives %>% select(ap34e, mpuntaje) %>% 
  group_by(ap34e) %>% summarise(count = n(),
                                mpuntaje_mean = mean(mpuntaje, na.rm=TRUE),
                                mpuntaje_sd = sd(mpuntaje,na.rm=TRUE))
siente_solo$prop <- (siente_solo$count/ sum(siente_solo$count))
siente_solo

# El 33% son valores perdidos
# Mas del 55% esta en desacuerdo dejando 10% aprox de estudiantes con sensacion de soledad
# pareciera haber una relacion lineal negativa (cuanto mas de acuerdo mas bajo los puntajes promedios) 

b <- siente_solo %>% na.omit() %>% 
  ggplot(aes(x = ap34e, y = prop, fill = "MyColor")) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = paste0(scales::percent(prop), " (", count, ")")),
            vjust = -0.5, size = 4) +
  # Add secondary y-axis for mpuntaje_mean variable
  scale_y_continuous(labels = scales::percent, limits = c(0,1),
                     sec.axis = sec_axis(~ (. * 150) + 400, name = "Puntaje promedio de matemática")) +
  geom_point(aes(x = ap34e, y = (mpuntaje_mean - 400) / 150), color = "black", size = 4) +
  geom_text(aes(label = round(mpuntaje_mean, 2), y = (mpuntaje_mean - 400) / 150),
            color = "black", size = 4, vjust = -1.5) +
  labs(x = "", y = "",
       title = "Cantidad de estudiantes según su sensación de soledad en la escuela",
       subtitle = "NATIVES") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5)) +
  scale_fill_manual(values = "royalblue") +
  guides(fill = FALSE)

b

## compraracion
grid.arrange(a,b,ncol=2)










