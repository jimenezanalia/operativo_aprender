library(lme4)
library(lmerTest)
library(performance)
library(r2mlm)
library (nlme)

OA2021_final <- setDT(OA2021_final)

# ///// ELECCION DE MODELO MULTINIVEL ///////////

## creamos el MODELO NULO ~ Puntaje de matematica
# CLUSTER: ESCUELAS

math_null_model <- lme4::lmer(mpuntaje ~ 1 + (1 | ID1), data = OA2021_final, weights = mpondera)

# todos los intervalos de confianza calculados muestran que el intercepto es significativo al modelo nulo
# el puntaje promedio de matematica a lo largo de todas las escuelas es 495. Con una sd promedio
# de 54.53
summary(math_null_model)

confint(math_null_model)
confint(math_null_model, method=c("boot"), boot.type=c("perc"))
confint(math_null_model, method=c("Wald"))

# El 24% de la varianza está explicada por el agrupamiento de ESCUELAS, es decir, el nivel ESCUELA tiene efecto
# sobre los puntajes de matemática de los alumnos

icc(math_null_model)

# Paso 2: agregamos todas las variables de nivel 1 FIJOS

model1 <- lme4::lmer(mpuntaje ~  NSE_puntaje  + sector + (1 | ID1), 
                     data = OA2021_final, weights = mpondera)

# control: hijos de bolivianos
# los coeficientes se interpretan como si fuera una regresion lineal al ser efectos fijos
# el intercepto varía de acuerdo a la escuela a la que pertenece
summary(model1)

# ###NO SON SIGNIFICATIVOS:
# Hijos de venezolanos
# nacidos en peru
# lenguas indigenas
confint(model1, method=c("Wald"))

# el intercepto es signficativo?? tarda mucho en correr
confint(model1)

# Paso3: agregamos las variables de nivel 2 FIJOS

model2 <- lme4::lmer(mpuntaje ~ NSE_puntaje  + sector + ambito + ch_standard + (1 | ID1), 
                     data = OA2021_final, weights = mpondera)
summary(model2)


confint(model2, method=c("Wald"))

confint(model2)

# paso 4: verificar (de a uno) las variables de nivel 1 que puedan variar por escuelas

model3 <- lme4::lmer(mpuntaje ~ NSE_puntaje  + sector + ambito + ch_standard +
                       origen1 + (1 | ID1), data = OA2021_final, weights = mpondera)
summary(model3)

# NO ES SIGNIFICATIVO:
# Nacidos en peru
confint(model3, method=c("Wald"))

# elegimos el modelo 2 que es mas simple que el 3.
anova( model2, model3)

model4 <- lme4::lmer(mpuntaje ~ NSE_puntaje  + sector + ambito + ch_standard +
                       origen1 + (NSE_puntaje | ID1), data = OA2021_final, weights = mpondera)
summary(model4)
confint(model4, method = "Wald")
confint(model4)
r2(model4)

anova(model3, model4)

model5 <- lme4::lmer(mpuntaje ~ NSE_puntaje + NSE_puntaje*sector  + sector + ambito + 
                       ch_standard + origen1 + (NSE_puntaje | ID1),
                     data = OA2021_final, weights = mpondera)
summary(model5)
anova(model3, model5)

model6 <- lmer(mpuntaje ~ NSE_puntaje + NSE_puntaje * ambito + sector + ambito + ch_standard +
                 origen1 + (NSE_puntaje | ID1), data = OA2021_final, weights = mpondera,
               control = lmerControl(optimizer = "bobyqa"))
summary(model6)
anova(model3, model6)
confint(model6)

# ///////// GRAFICO DE LOS MODELO ///////

# ASSUMPTION 2: FUNCTIONAL FORM IS CORRECT: CHECK LINEARITY BETWEEN X's AND Y's
model6_data <- OA2021_final %>% 
  select(ID1,mpuntaje, NSE_puntaje, sector, ambito, ch_standard, origen1) %>% na.omit() 

# /// NIVEL 1 - INDIVIDUOS /////

# por la cantidad de datos es muy dificil controlar (imposible)
model6_data %>% 
  ggplot(mapping = aes(x = NSE_puntaje, y = mpuntaje)) +
  geom_point(alpha = .2)

# obtenemos los residuales
model6_data$l1resid <- residuals(model6)
head(l1resid)

# VERIFICAR QUE LOS RESIDUALES NO ESTEN CORRELACIONADOS (HOMOCEDASTICIDAD)

model6_data %>%   ggplot(mapping = aes(x = NSE_puntaje, y = l1resid)) +
  geom_point(alpha = 0.5) +
  labs(x = "NSE", y = "residuals")
# CHEQUEAMOS NUMERICAMENTE POR LA CANTIDAD DE DATOS
cor.test(model6_data$l1resid, model6_data$NSE_puntaje)


# NORMALMENTE DISTRIBUIDO?
model6_data %>% 
  ggplot(mapping = aes(x = l1resid)) +
  geom_histogram()

# SE COMPRUEBA CON ESTA
model6_data %>% 
  ggplot(mapping = aes(sample = l1resid)) +
  stat_qq()

# //// NIVEL 2 - ESCUELAS //////

l2_model6 <- model6_data %>% 
  group_by(ID1) %>%  # group data by clustering variable, school
  mutate(
    mpuntaje_mean = mean(mpuntaje),
    nse_mean = mean(NSE_puntaje)# create mean math achievement per school
  ) %>% 
  select(ID1, nse_mean, mpuntaje_mean) %>% 
  unique() # select unique rows (rather than having school, ses_mean, and mathach_mean repeating over and over again)

l2_model6$intercept_resid = ranef(model6)$ID1[, 1]
l2_model6$slope_resid = ranef(model6)$ID1[, 2]

# INDEPENDIENTE
   # intercepto
l2_model6 %>% 
  ggplot(mapping = aes(x = intercept_resid, y = nse_mean)) +
  geom_point()

cor.test(l2_model6$nse_mean, l2_model6$intercept_resid)

  # coeficiente aleatorio
l2_model6 %>% 
  ggplot(mapping = aes(x = slope_resid, y =nse_mean)) +
  geom_point()

cor.test(l2_model6$nse_mean, l2_model6$slope_resid)


# NORMAL MULTIVARIATE DISTRIBUTION

#intercepto: residuales
l2_model6 %>% 
  ggplot(mapping = aes(x = intercept_resid)) +
  geom_histogram(binwidth = .75)

l2_model6 %>% 
  ggplot(mapping = aes(sample = intercept_resid)) +
  stat_qq()

# coeficientes: residuales

l2_model6 %>% 
  ggplot(mapping = aes(x = slope_resid)) +
  geom_histogram(binwidth = .50)

l2_model6 %>% 
  ggplot(mapping = aes(sample = intercept_resid)) +
  stat_qq()


# //// residueles de nivel1 y 2 son independientes

n_per_school <- model6_data %>% 
  group_by(ID1) %>% # group by school
  select(ID1) %>% # we just want to count schools
  count() %>% 
  ungroup() %>% 
  select(n) %>% 
  unlist()

model6_data$intercept_resid <- rep(l2_model6$intercept_resid, times = n_per_school)
model6_data$slope_resid <- rep(l2_model6$slope_resid, times = n_per_school)

model6_data %>% 
  ggplot(mapping = aes(x = l1resid, y = intercept_resid)) +
  geom_point()

cor.test(model6_data$l1resid, model6_data$intercept_resid)

## residuales: coeficiente

model6_data %>% 
  ggplot(mapping = aes(x = l1resid, y = slope_resid)) +
  geom_point()

cor.test(model6_data$l1resid, model6_data$slope_resid)



###Para explorar por Provincias

ggplot(OA2021_final, aes(x = jurisdiccion, y = mpuntaje, fill = jurisdiccion)) +
  geom_boxplot() +
  labs(title = "Boxplot de Puntajes en Matemática por Provincias",
       x = "jurisdiccion", y = "Puntaje") +
  theme_minimal()

anovap <- aov( mpuntaje ~ jurisdiccion, 
               data = OA2021_final )

summary (anovap)

TukeyHSD(anovap)

##Vamos a probar si la regresión da los mismo que la ANOVA.


lineal <- gls(data=OA2021_final, 
          model=mpuntaje ~ 1 + relevel(factor(jurisdiccion),"CABA"), 
          method="REML", na.action = na.omit)

summary(lineal)



modeloprovincias <- lme4::lmer(mpuntaje ~ 1 + (1 | jurisdiccion), data = OA2021_final, weights = mpondera) 

icc_provincias <- performance::icc(modeloprovincias)

print(icc_provincias)

###El ICC es de 0.026 es bajo pero contextualmente es relevante. Hay mucha variabilidad al interior de las provincias, pero aún así el anova es significativo, por ejemplo
## CABA tiene medias mayores que todos. Esto lo vemos con la prueba de tukey


###PAra chequear por colegios anova. Este no carga por la memoria

anova2 <- aov( mpuntaje ~ ID1, 
               data = OA2021_final )

summary (anova2)


###Para explorar NSE y puntaje

ggplot(OA2021_final, aes(x = NSE_nivel, y = mpuntaje, fill = NSE_nivel)) +
  geom_boxplot() +
  labs(title = "Boxplot de Puntajes en Matemática por NSE",
       x = "NSE", y = "Puntaje") +
  theme_minimal()

anovaNSE <- aov( mpuntaje ~ NSE_nivel, 
               data = OA2021_final )

summary (anovaNSE)

TukeyHSD(anovaNSE)

modeloNSE<- lme4::lmer(mpuntaje ~ 1 + (1 | NSE_nivel), data = OA2021_final,weights = mpondera)



### cambia mucho cuando sacamos el ponderador##  

## modeloNSE<- lme4::lmer(mpuntaje ~ 1 + (1 | NSE_nivel), data = OA2021_final)



icc_NSE <- performance::icc(modeloNSE)

print(icc_NSE)


###vamos a explorar el NSE por individuos y los colegios que es nivel 2


modeloNSECOLE<- lme4::lmer(NSE_puntaje ~ 1 + (1 | ID1), data = OA2021_final,weights = ponder)

icc_NSECOLE <- performance::icc(modeloNSECOLE)

print(icc_NSECOLE)


                           
                           
