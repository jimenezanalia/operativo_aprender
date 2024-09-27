rm(list = ls())
options(scipen=999)

library(readxl)
library(readr)
library(dplyr)
library(compareGroups)
library(ggplot2)
library(FactoMineR)
library(factoextra)
library(ggrepel)
require(missMDA)
library(ggridges)
library(ggthemes)
library(PupillometryR)
library(scales)
library(arrow)

OA2021_final <- read_parquet("RA_OA_2021.parquet")

# origen: todos menos 'otros' y 'venezuela' 

tabla <- compareGroups(origen1 ~ mpuntaje + lpuntaje + sector + ambito + NSE_puntaje + 
                                 + sobreedad   + ap06_clasif + migrante_prop + ch_standard  ,
                               data= OA2021_final, max.ylev=13, simplify = TRUE, method = 1)


# RENOMBRAMOS las variables de las filas
tabla[["Puntaje de matemática"]] = tabla[["mpuntaje"]]
tabla[["mpuntaje"]] <- NULL

tabla[["Puntaje de lengua"]] = tabla[["lpuntaje"]]
tabla[["lpuntaje"]] <- NULL

tabla[["Sector"]] = tabla[["sector"]]
tabla[["sector"]] <- NULL

tabla[["Ámbito"]] = tabla[["ambito"]]
tabla[["ambito"]] <- NULL

tabla[["Nivel Socioeconómico"]] = tabla[["NSE_puntaje"]]
tabla[["NSE_puntaje"]] <- NULL

tabla[["Sobreedad"]] = tabla[["sobreedad"]]
tabla[["sobreedad"]] <- NULL

tabla[["Idioma hablado en la casa"]] = tabla[["ap06_clasif"]]
tabla[["ap06_clasif"]] <- NULL

tabla[["Discriminación"]] = tabla[["ap36"]]
tabla[["ap36"]] <- NULL

tabla[["Proporción de migrantes"]] = tabla[["migrante_prop"]]
tabla[["migrante_prop"]] <- NULL

tabla[["Indice de equipamiento"]] = tabla[["ch_standard"]]
tabla[["ch_standard"]] <- NULL

tabla1 <- createTable(tabla)
tabla1

# export2html(tabla1, file = "aber.html")
# export2xls(tabla1, file='bivariate_analysis.xlsx')



# ANALISIS CORRESPONDENCIA MULTIPLE

# SOLO MIGRANTES y origen1

for_mca <- OA2021_final %>% 
  filter( origen != "native") %>%  
          # & !is.na(mpuntaje_clasif) & !is.na(lpuntaje_clasif) &
          #   !is.na(NSE_nivel) & !is.na(ch_standard)) %>%
  select(Puntaje_matematica, Puntaje_lengua, sector, ambito, NSE_nivel, Indice_equipamiento,
         origen1, ponder)

imputes <- imputeFAMD(for_mca[,1:7])

res1 <- MCA(for_mca[,1:7],ncp = 12, graph = FALSE, method = "CA", 
                  row.w=for_mca$ponder, tab.disj = imputes$tab.disj)


var1 <- get_mca_var(res1)
var1

# fviz_screeplot(res1)

# data frame with variable coordinates
cats = apply(for_mca[1:7], 2, function(x) nlevels(as.factor(x)))
cats

res1_vars_df = data.frame(var1$coord, Variable = rep(names(cats), cats))
# Recode the Variable column with desired names
res1_vars_df$Variable <- recode(res1_vars_df$Variable,
                                "Puntaje_matematica" = "Matemática",
                                "Puntaje_lengua" = "Lengua",
                                "sector" = "Sector",
                                "ambito" = "Ambito",
                                "NSE_nivel" = "Nivel NSE",
                                "Indice_equipamiento" = "Indice de equipamiento",
                                "origen1" = "Origen"
)


  ggplot(data = res1_vars_df, aes(x = Dim.1, y = Dim.2, label = rownames(res1_vars_df))) +
    geom_hline(yintercept = 0, colour = "gray70") +
    geom_vline(xintercept = 0, colour = "gray70") +
    geom_text_repel(aes(colour = Variable)) +  # Use geom_text_repel instead of geom_text
    ggtitle("") +
    theme(plot.title = element_text(hjust = 0.5)) + 
    paper_theme()

  
  
  
  
  # fviz_mca_ind(res1, 
#              label = "none", # hide individual labels
#              habillage = "NSE_nivel", # color by groups ,
#              addEllipses = TRUE, ellipse.type = "t",
#              ggtheme = theme_minimal())
######

# SOLO NATIVOS y origen1

for_mca <- OA2021_final %>% 
  # filter(!is.na(mpuntaje) & !is.na(lpuntaje) & !is.na(NSE_nivel) & !is.na(ch_standard)) %>% 
  select(Puntaje_matematica, Puntaje_lengua, sector, ambito, NSE_nivel, Indice_equipamiento,
         origen1, ponder)

imputes <- imputeFAMD(for_mca[,1:7])

res2 <- MCA(for_mca[,1:7],ncp = 12, method = "CA", 
                  graph = FALSE, row.w=for_mca$ponder, tab.disj = imputes$tab.disj)


var2 <- get_mca_var(res2)
var2

# data frame with variable coordinates
cats = apply(for_mca[,1:7], 2, function(x) nlevels(as.factor(x)))
cats

res2_vars_df = data.frame(var2$coord, Variable = rep(names(cats), cats))
# Recode the Variable column with desired names
res2_vars_df$Variable <- recode(res2_vars_df$Variable,
                                "Puntaje_matematica" = "Matematica",
                                "Puntaje_lengua" = "Lengua",
                                "sector" = "Sector",
                                "ambito" = "Ambito",
                                "NSE_nivel" = "Nivel NSE",
                                "Indice_equipamiento" = "Indice de equipamiento",
                                "origen1" = "Origen")


ggplot(data = res2_vars_df, aes(x = Dim.1, y = Dim.2, label = rownames(res2_vars_df))) +
  geom_hline(yintercept = 0, colour = "gray70") +
  geom_vline(xintercept = 0, colour = "gray70") +
  geom_text_repel(aes(colour = Variable)) +  # Use geom_text_repel instead of geom_text
  ggtitle("") +
  paper_theme() +
  theme(plot.title = element_text(hjust = 0.5))


 # plot(res2, invisible=c("ind"),autoLab="y",cex=0.7, title="MCA para migrantes, hijos y nativos")


  # /////// FACTOR ANALYSIS FOR MIXED DATA ////////


for_mca <- OA2021_final %>% 
  # filter(!is.na(mpuntaje) & !is.na(lpuntaje) & !is.na(sector) & !is.na(NSE_puntaje) &
  #          !is.na(ch_standard) & !is.na(origen1)) %>%
  select(Puntaje_matematica, Puntaje_lengua, sector, ambito, NSE_nivel, Indice_equipamiento,
         origen1, ponder)


imputes <- imputeFAMD(for_mca[,1:6])

res3 <- FAMD(for_mca[,1:6], ncp = 8, graph = FALSE, row.w = for_mca$pondera, 
             tab.disj = imputes$tab.disj)
# print(res3)

eig.val <- get_eigenvalue(res3)
head(eig.val)

fviz_screeplot(res3)

# Plot of variables
fviz_famd_var(res3, repel = TRUE)
# Contribution to the first dimension
fviz_contrib(res3, "var", axes = 1)
# Contribution to the second dimension
fviz_contrib(res3, "var", axes = 2)

# graficamos las variables CUANTITATIVAS

fviz_famd_var(res3, "quanti.var", col.var = "contrib", 
              gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
              repel = TRUE)

# graficamos las variables CUALITATIVAS

fviz_famd_var(res3, "quali.var", col.var = "contrib", 
              gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07")
)

# GRAFICAMOS LOS INDIVIDUOS

fviz_famd_ind(res3, col.ind = "cos2", 
              gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
              repel = FALSE)

# AGREGAMOS ELIPSES

fviz_mfa_ind(res3, 
             habillage = "origen1", # color by groups 
             palette = rainbow(7),
             addEllipses = TRUE, ellipse.type = "confidence", 
             repel = TRUE # Avoid text overlapping
) 

# ////// GRAFICO DE VARIABLES CUANTITATIVAS ////////////

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
                              reverse = T)) + theme_few()

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
       title = "") + theme_few() +
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
       title = "") + theme_few() +
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
                              reverse = T)) + theme_few()

