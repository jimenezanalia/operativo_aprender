rm(list = ls())
options(scipen=999)


library(readxl)
library(readr)
library(dplyr)
library(haven)
library(data.table)
library(ggfortify)
library (summarytools)
library(factoextra)
library(FactoMineR)
library(compareGroups)
library(moments)
library(viridis)
library(ggridges)
library(ggthemes)


# cargamos el set de datos

# OA2021 <- fread("DatosOA2021/Base estudiantes 6 grado primaria 2021.csv", sep = ";", dec = ",",
#                 header = TRUE)

OA2021 <- read.csv("DatosOA2021/Base estudiantes 6 grado primaria 2021.csv",
                   header=TRUE, dec = ",",
                   row.names=NULL, sep=";", stringsAsFactors=F,
                   check.names = F, encoding = "ASCII")



# Agregamos las etiquetas de las variables
OA2021$ID1 <- factor(OA2021$ID1)
OA2021$idalumno <- factor(OA2021$idalumno)
OA2021$region <- factor(OA2021$region)

OA2021$jurisdiccion <- 
  factor(OA2021$jurisdiccion,
         levels = c(2,6,10,14,18,22,26,30,34,38,42,46,50,54,58,62,66,70,74,78,82,86,90,94),
         labels = c('CABA','Buenos aires','Catamarca','Córdoba',
                    'Corrientes','Chaco','Chubut','Entre Ríos','Formosa','Jujuy','La Pampa',
                    'La Rioja','Mendoza','Misiones','Neuquén','Río Negro','Salta','San Juan',
                    'San Luis','Santa Cruz','Santa Fe','Santiago del Estero','Tucumán',
                    'Tierra del Fuego'))


OA2021$sector <- ifelse(OA2021$sector == 1, 0, 1)
OA2021$sector <- factor(OA2021$sector, levels = c(0,1),
                        labels = c("Estatal", "Privado"))

OA2021$ambito <- factor(OA2021$ambito, levels = c(1,2),
                        labels = c("Rural", "Urbano"))

OA2021$ap01 <- factor(OA2021$ap01, levels = c(1,2,3,4,5),
                      labels = c("<= 10 años", "11 años", "12 años",
                                 "13 años", ">= 14 años"))

OA2021$ap02 <- factor(OA2021$ap02, levels = seq(1,12,1), labels = month.name)

OA2021$ap03 <- factor(OA2021$ap03, levels = c(1,2,3), labels = c("M","F","X"))

OA2021$ap04a[OA2021$ap04a == 12] <- NA
OA2021$ap04a <- factor(OA2021$ap04a, levels = seq(1,11,1),
                       labels = c('Argentina','Bolivia','Brasil','Chile','Colombia', 'Ecuador',
                                  'Paraguay','Perú','Uruguay','Venezuela', 'Otros'))

OA2021$ap04b[OA2021$ap04b == 12] <- NA
OA2021$ap04b <- factor(OA2021$ap04b, levels = seq(1,11,1),
                       labels = c('Argentina','Bolivia','Brasil','Chile','Colombia', 'Ecuador',
                                  'Paraguay','Perú','Uruguay','Venezuela', 'Otros'))

OA2021$ap04c[OA2021$ap04c == 12] <- NA
OA2021$ap04c <- factor(OA2021$ap04c, levels = seq(1,11,1),
                       labels = c('Argentina','Bolivia','Brasil','Chile','Colombia', 'Ecuador',
                                  'Paraguay','Perú','Uruguay','Venezuela', 'Otros'))

OA2021$ap04d[OA2021$ap04d == 12] <- NA
OA2021$ap04d <- factor(OA2021$ap04d, levels = seq(1,11,1),
                       labels = c('Argentina','Bolivia','Brasil','Chile','Colombia', 'Ecuador',
                                  'Paraguay','Perú','Uruguay','Venezuela', 'Otros'))

OA2021$ap04e[OA2021$ap04e == 12] <- NA
OA2021$ap04e <- factor(OA2021$ap04e, levels = seq(1,11,1),
                       labels = c('Argentina','Bolivia','Brasil','Chile','Colombia', 'Ecuador',
                                  'Paraguay','Perú','Uruguay','Venezuela', 'Otros'))

OA2021$ap05[OA2021$ap05 == 16] <- NA
OA2021$ap05 <- factor(OA2021$ap05, levels = seq(1,15),
                      labels = c("< 1 año", "1 año", "2 años", "3 años", "4 años", "5 años", "6 años",
                                 "7 años", "8 años", "9 años", "10 años", "11 años", "12 años",
                                 "13 años", ">= 14 años"))

OA2021$ap06_clasif <- ifelse(OA2021$ap06 == 1, OA2021$ap06,
                             ifelse(OA2021$ap06 == 2, OA2021$ap06, 5))

OA2021$ap06_clasif <- factor(OA2021$ap06_clasif, levels = c(1,2,5),
                             labels = c("Español", "Lenguas indígenas","Otros"))


OA2021$ap06 <- factor(OA2021$ap06, levels = seq(1,5,1),
                      labels = c("Español","Lenguas indígenas","Portugués","Inglés","Otros"))


OA2021$ap08 <- ifelse((OA2021$ap08a == 1 & OA2021$ap08b ==1), "Vive con padre y madre",
                      ifelse(OA2021$ap08a ==1, "Vive solo con la madre",
                             ifelse(OA2021$ap08b == 1, "Vive solo con el padre",
                                    ifelse(OA2021$ap08i == 1, "No se sabe con quién vive",
                                           "Vive con otro pariente/encargado"))))
OA2021$ap08 <- replace(OA2021$ap08, is.na(OA2021$ap08), "No se sabe con quién vive")

OA2021$ap08 <- factor(OA2021$ap08)




OA2021$ap17 <- factor(OA2021$ap17, levels = c(1,2,3),
                      labels = c("Sí", "No", "No sé"))


OA2021$ap18 <- factor(OA2021$ap18, levels = c(1,2,3),
                      labels = c("Sí", "No", "No sé"))

OA2021$minority <- ifelse((as.numeric(OA2021$ap17) == 1 | as.numeric(OA2021$ap18) == 1), "Sí",
                          ifelse((as.numeric(OA2021$ap17) == 2 | as.numeric(OA2021$ap18) == 2), "No", NA))
OA2021$minority <- factor(OA2021$minority)

OA2021$ap19a <- factor(OA2021$ap19a, levels = c(1,2,3,4),
                       labels = c("Todos los días", "Lunes a Viernes", "Algunos días","Nunca"))

OA2021$ap19b <- factor(OA2021$ap19b, levels = c(1,2,3,4),
                       labels = c("Todos los días", "Lunes a Viernes", "Algunos días","Nunca"))

OA2021$ap19c <- factor(OA2021$ap19c, levels = c(1,2,3,4),
                       labels = c("Todos los días", "Lunes a Viernes", "Algunos días","Nunca"))

OA2021$ap22 <- factor(OA2021$ap22, levels = seq(1,7,1),
                      labels = c("Camino","Auto/Moto","Transporte escolar","Colectivo","Bicicleta",
                                 "Tren/Subte","Otro"))

OA2021$ap23 <- factor(OA2021$ap23, levels = c(1,2,3,4),
                      labels = c("<0,5 hs", "0,5 - 1 hs", "1 - 2 hs", ">2 hs"))

OA2021$ap25 <- factor(OA2021$ap25, levels = c(1,2,3,4),
                      labels = c("No", "1 vez","2 veces", "3 veces o más"))

OA2021$ap26a <- factor(OA2021$ap26a, levels = c(1,2,3,4),
                       labels = c("Ninguna","Algunas","Bastantes","Todas"))

OA2021$ap26b <- factor(OA2021$ap26b, levels = c(1,2,3,4),
                       labels = c("Ninguna","Algunas","Bastantes","Todas"))

OA2021$ap32 <- factor(OA2021$ap32, levels = c(1,2),
                      labels = c("Si","No"))

OA2021$ap33 <- factor(OA2021$ap33, levels = seq(1,5,1),
                      labels = c("Todos", "Mayoría","Algunos","Pocos","Ninguno"))

OA2021$ap34a <- factor(OA2021$ap34a, levels = c(1,2,3),
                       labels = c("Muy de acuerdo", "De acuerdo", "En desacuerdo"))

OA2021$ap34b <- factor(OA2021$ap34b, levels = c(1,2,3),
                       labels = c("Muy de acuerdo", "De acuerdo", "En desacuerdo"))

OA2021$ap34c <- factor(OA2021$ap34c, levels = c(1,2,3),
                       labels = c("Muy de acuerdo", "De acuerdo", "En desacuerdo"))

OA2021$ap34d <- factor(OA2021$ap34d, levels = c(1,2,3),
                       labels = c("Muy de acuerdo", "De acuerdo", "En desacuerdo"))

OA2021$ap34e <- factor(OA2021$ap34e, levels = c(1,2,3),
                       labels = c("Muy de acuerdo", "De acuerdo", "En desacuerdo"))

OA2021$ap36 <- ifelse(OA2021$ap36c == 3 | OA2021$ap36f == 3 | OA2021$ap36d == 3, "Muchas veces",
                      ifelse(OA2021$ap36c == 2 | OA2021$ap36f == 2 | OA2021$ap36d == 2, "Algunas veces",
                             ifelse(OA2021$ap36c == 1 | OA2021$ap36d == 1 | OA2021$ap36f == 1, "Nunca", NA)))

OA2021$ap36 <- factor(OA2021$ap36)


OA2021$NSE_nivel <- factor(OA2021$NSE_nivel, levels =c(1,2,3),
                           labels = c("Bajo","Medio","Alto"))


OA2021$sobreedad <- ifelse(OA2021$sobreedad <2, "Menos de edad teórica",
                           ifelse(OA2021$sobreedad == 2, "Edad teórica", "Sobreedad"))
OA2021$sobreedad <- factor(OA2021$sobreedad)


OA2021$migracion <- factor(OA2021$migracion, levels = c(1,2),
                           labels = c("Native", "Migrante"))

OA2021$Nivel_Ed_Madre[OA2021$Nivel_Ed_Madre == 9] <- NA
OA2021$Nivel_Ed_Madre <- factor(OA2021$Nivel_Ed_Madre, levels = seq(1,8,1),
                                labels = c("No fue a la escuela","Primario incompleto","Primario completo",
                                           "Secundario incompleto", "Secundario completo","Universitario incompleto",
                                           "Universitario completo","Posgrado"))

OA2021$Nivel_Ed_Padre[OA2021$Nivel_Ed_Padre == 9] <- NA
OA2021$Nivel_Ed_Padre <- factor(OA2021$Nivel_Ed_Padre, levels = seq(1,8,1),
                                labels = c("No fue a la escuela","Primario incompleto","Primario completo",
                                           "Secundario incompleto", "Secundario completo","Universitario incompleto",
                                           "Universitario completo","Posgrado"))

# creamos una variable Nivel_Ed_MyP (el maximo nivel que tenga alguno de los padres)

OA2021$Nivel_Ed_MyP <-
  ifelse(is.na(OA2021$Nivel_Ed_Padre) == TRUE, OA2021$Nivel_Ed_Madre,
         ifelse(is.na(OA2021$Nivel_Ed_Madre)==TRUE, OA2021$Nivel_Ed_Padre,
                ifelse(as.numeric(OA2021$Nivel_Ed_Madre) >= as.numeric(OA2021$Nivel_Ed_Padre),
                       OA2021$Nivel_Ed_Madre,OA2021$Nivel_Ed_Padre)))

OA2021$Nivel_Ed_MyP <- factor(OA2021$Nivel_Ed_MyP, levels = seq(1,8,1),
                                 labels = c("No fue a la escuela","Primario incompleto","Primario completo",
                                            "Secundario incompleto", "Secundario completo","Universitario incompleto",
                                            "Universitario completo","Posgrado"))



OA2021$origen <- ifelse(as.numeric(OA2021$migracion)==1, "native",
                        ifelse(as.numeric(OA2021$ap04a)!=1, "migrante",
                               ifelse(as.numeric(OA2021$ap04b)!=1 | as.numeric(OA2021$ap04c)!= 1, "hije_migrante","native")))
OA2021$origen <- factor(OA2021$origen)

# desglosado por pais de origen (ya sea hijo o nacido en el pais extranjero)
OA2021$origen2<- ifelse(as.numeric(OA2021$migracion)==1,"Native", ifelse(as.numeric(OA2021$ap04a)==2, "Bolivia",
                                                                         ifelse(as.numeric(OA2021$ap04a)==4,
                                                                                "Chile",ifelse(as.numeric(OA2021$ap04a)==7,
                                                                                               "Paraguay",
                                                                                               ifelse(as.numeric(OA2021$ap04a)==8, "Peru",
                                                                                                      ifelse(as.numeric(OA2021$ap04a)==10,
                                                                                                             "Venezuela",
                                                                                                             ifelse(as.numeric(OA2021$ap04a)==3|
                                                                                                                      as.numeric(OA2021$ap04a)==5|
                                                                                                                      as.numeric(OA2021$ap04a)==6|
                                                                                                                      as.numeric(OA2021$ap04a)==9|
                                                                                                                      as.numeric(OA2021$ap04a)==11, "Otro",
                                                                                                                    ifelse((as.numeric(OA2021$ap04b)==2 |
                                                                                                                              as.numeric(OA2021$ap04c)==2) & as.numeric(OA2021$ap04a)==1, "Bolivia",
                                                                                                                           ifelse((as.numeric(OA2021$ap04b)==4 |
                                                                                                                                     as.numeric(OA2021$ap04c)==4) & as.numeric(OA2021$ap04a)==1, "Chile",
                                                                                                                                  ifelse((as.numeric(OA2021$ap04b)==7 |
                                                                                                                                            as.numeric(OA2021$ap04c)==7) & as.numeric(OA2021$ap04a)==1, "Paraguay",
                                                                                                                                         ifelse((as.numeric(OA2021$ap04b)==8 |
                                                                                                                                                   as.numeric(OA2021$ap04c)==8) & as.numeric(OA2021$ap04a)==1, "Peru",
                                                                                                                                                ifelse((as.numeric(OA2021$ap04b)==10 |
                                                                                                                                                          as.numeric(OA2021$ap04c)==10) & as.numeric(OA2021$ap04a)==1, "Venezuela",
                                                                                                                                                       ifelse((as.numeric(OA2021$ap04b)==3 |
                                                                                                                                                                 as.numeric(OA2021$ap04c)==3 |
                                                                                                                                                                 as.numeric(OA2021$ap04b)==5 |
                                                                                                                                                                 as.numeric(OA2021$ap04c)==5 |
                                                                                                                                                                 as.numeric(OA2021$ap04b)==6 |
                                                                                                                                                                 as.numeric(OA2021$ap04c)==6 |
                                                                                                                                                                 as.numeric(OA2021$ap04b)==9 |
                                                                                                                                                                 as.numeric(OA2021$ap04c)==9 |
                                                                                                                                                                 as.numeric(OA2021$ap04b)==11 |
                                                                                                                                                                 as.numeric(OA2021$ap04c)==11) &
                                                                                                                                                                as.numeric(OA2021$ap04a)==1, "Otro", NA)))))))))))))

OA2021$origen2 <- factor(OA2021$origen2)


# desglosado por hijo de migrantes o nacido en otro pais
OA2021$origen1<- ifelse(as.numeric(OA2021$migracion)==1,"Nativo", ifelse(as.numeric(OA2021$ap04a)==2, "Boliviano",
                                                                         ifelse(as.numeric(OA2021$ap04a)==4,
                                                                                "Chileno",ifelse(as.numeric(OA2021$ap04a)==7,
                                                                                                   "Paraguayo",
                                                                                                   ifelse(as.numeric(OA2021$ap04a)==8, "Peruano",
                                                                                                          ifelse(as.numeric(OA2021$ap04a)==10,
                                                                                                                 "Venezolano",
                                                                                                                 ifelse(as.numeric(OA2021$ap04a)==3|
                                                                                                                          as.numeric(OA2021$ap04a)==5|
                                                                                                                          as.numeric(OA2021$ap04a)==6|
                                                                                                                          as.numeric(OA2021$ap04a)==9|
                                                                                                                          as.numeric(OA2021$ap04a)==11, "nac_otro",
                                                                                                                        ifelse((as.numeric(OA2021$ap04b)==2 |
                                                                                                                                  as.numeric(OA2021$ap04c)==2) & as.numeric(OA2021$ap04a)==1, "Hijo de bolivianos",
                                                                                                                               ifelse((as.numeric(OA2021$ap04b)==4 |
                                                                                                                                         as.numeric(OA2021$ap04c)==4) & as.numeric(OA2021$ap04a)==1, "Hijo de chilenos",
                                                                                                                                      ifelse((as.numeric(OA2021$ap04b)==7 |
                                                                                                                                                as.numeric(OA2021$ap04c)==7) & as.numeric(OA2021$ap04a)==1, "Hijo de paraguayos",
                                                                                                                                             ifelse((as.numeric(OA2021$ap04b)==8 |
                                                                                                                                                       as.numeric(OA2021$ap04c)==8) & as.numeric(OA2021$ap04a)==1, "Hijo de peruanos",
                                                                                                                                                    ifelse((as.numeric(OA2021$ap04b)==10 |
                                                                                                                                                              as.numeric(OA2021$ap04c)==10) & as.numeric(OA2021$ap04a)==1, "Hijo de venezolanos",
                                                                                                                                                           ifelse((as.numeric(OA2021$ap04b)==3 |
                                                                                                                                                                     as.numeric(OA2021$ap04c)==3 |
                                                                                                                                                                     as.numeric(OA2021$ap04b)==5 |
                                                                                                                                                                     as.numeric(OA2021$ap04c)==5 |
                                                                                                                                                                     as.numeric(OA2021$ap04b)==6 |
                                                                                                                                                                     as.numeric(OA2021$ap04c)==6 |
                                                                                                                                                                     as.numeric(OA2021$ap04b)==9 |
                                                                                                                                                                     as.numeric(OA2021$ap04c)==9 |
                                                                                                                                                                     as.numeric(OA2021$ap04b)==11 |
                                                                                                                                                                     as.numeric(OA2021$ap04c)==11) &
                                                                                                                                                                    as.numeric(OA2021$ap04a)==1, "hije_otro", NA)))))))))))))
OA2021$origen1 <- as.factor(OA2021$origen1)

OA2021$ldesemp <- factor(OA2021$ldesemp, levels = seq(1,4,1),
                              labels = c("Por debajo del nivel básico", "Básico",
                                         "Satisfactorio", "Avanzado"))

OA2021$mdesemp <- factor(OA2021$mdesemp, levels = seq(1,4,1),
                         labels = c("Por debajo del nivel básico", "Básico",
                                    "Satisfactorio", "Avanzado"))


# Estandarizamos los valores NA
OA2021[OA2021 == -9] <- NA
OA2021[OA2021 == -6] <- NA

# guardamos el set de datos en formato spss
# write_sav(OA2021, "OA2021.sav")


# # separamos los migrantes
# migrantes <- OA2021 %>% filter(as.numeric(migracion)!=1)
# # ahora separaremos a los natives. Hay que tener en cuenta que el 23%aprox es NA para saber si es native/migrante
# natives <- OA2021 %>% filter(as.numeric(migracion)==1)
# 

# ///  A PARTIR DE AQUI CARGAMOS LA BASE DE RELEVAMIENTO ANUAL /////


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

# creamos la variable tiene internet

ch21$has_internet <- ifelse(ch21$`Internet - Tipo de servicio - Gratuito` == 1 |
                              ch21$`Internet - Tipo de servicio - Pago` == 1, 1, 0)

# CONSTRUCCION DE INDICE DE EQUIPAMIENTO

for_index <- ch21[, c(seq(12,15),seq(24,28),31,65, seq(43,47))]

ch_index <-  FactoMineR::PCA(for_index,scale.unit = T, graph = F, ncp = 2)

index01<- ch_index$ind$coord[,1]

# Print the percentage explained by each component
eigenvalues <- ch_index$eig
# eigenvalues[, 1:2]

eigenvalues <- data.frame(eigenvalues[, 1:2])

# agregamos el indice al set de datos
ch21$ch_index <-index01

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


# /////////// UNION DE LOS DATASETS //////////////

union21 <-   ch21_2 %>% 
  left_join(matri21, by = "id") %>% 
  left_join(pob21, by = join_by(id == ID1))

# calculamos la proporcion de migrantes de primaria en las escuelas 

union21$migrante_prop <- 
  ifelse(union21$total_primaria == 0 , 0, union21$total_migrantes / union21$total_primaria)

union21$ch_standard <- (union21$ch_index - mean(union21$ch_index)) / sd(union21$ch_index)
# summary(union21$ch_standard)

# chequeamos los NA's del indice de equipamiento

# mean(is.na(union21$ch_standard))

rm(list = setdiff(ls(), c("union21", "OA2021")))


# //// UNION CON OPERATIVO APRENDER ////////////

union21 <- union21 %>% select(id,ch_index, migrante_prop, total_primaria, total_migrantes)

union21 <- setDT(union21)

OA2021 <- merge(OA2021, union21, by.x = "ID1", by.y = "id", all.x = TRUE)

# OA2021 <- OA2021 %>% left_join(union21, by = join_by(ID1 == id), keep = TRUE)

# colnames(OA2021)

OA2021 <- OA2021[, c(seq(1,26),seq(112,120),seq(169,195))]

OA2021$ch_standard <- (OA2021$ch_index - mean(OA2021$ch_index, na.rm = TRUE))/sd(OA2021$ch_index, na.rm = TRUE)


# FILTRAMOS LOS ORIGENES A TRABAJAR

OA2021_final <- OA2021 %>% 
  filter(origen1 == "Boliviano" | origen1 == "Hijo de bolivianos" | origen1 == "Paraguayo"
         | origen1 == "Hijo de paraguayos" | origen1 == "Peruano"  | origen1 == "Hijo de peruanos"
         | origen1 == "Venezolano" | origen1 == "Hijo de venezolanos" 
         | origen1 == "Nativo")

# Remove empty levels
OA2021_final$origen1 <- droplevels(OA2021_final$origen1)

# Check the levels
levels(OA2021_final$origen1)

rm(list = setdiff(ls(), c("OA2021_final", "union21", "OA2021")))

# Define the desired order for the x-axis categories

desired_order <- c("Nativo", "Hijo de peruanos", "Peruano", "Hijo de paraguayos", "Paraguayo",
                   "Hijo de bolivianos", "Boliviano", "Hijo de venezolanos", "Venezolano")

# Reorder the levels of the ch_cat variable based on the desired order
OA2021_final$origen1 <- factor(OA2021_final$origen1, levels = desired_order)

# /// Categorizamos las variables cuantitativas para el ACM

OA2021_final$ch_cat <- ifelse(OA2021_final$ch_standard <=
                                quantile(OA2021_final$ch_standard, 1/3, na.rm = TRUE), "Bajo",
                              ifelse(OA2021_final$ch_standard <=
                                       quantile(OA2021_final$ch_standard, 2/3, na.rm = TRUE), "Medio", "Alto"))

OA2021_final$ch_cat <- factor(OA2021_final$ch_cat)


OA2021_final <- OA2021_final %>%
  rename(
    Indice_equipamiento = ch_cat,
    Puntaje_lengua = ldesemp,
    Puntaje_matematica = mdesemp )

library(arrow)

write_parquet(OA2021_final, "RA_OA_2021.parquet")
