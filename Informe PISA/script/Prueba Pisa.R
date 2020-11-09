#Importamos las librerias
library(readr)
library(glmnet)
library(tidyverse)
library(fBasics)
library(car)
library(dplyr)
library(ggplot2)
library(knitr)
library(MASS)
library(corrplot)
library(PerformanceAnalytics)
library(gvlma)
library(tinytex)
library(devtools)
library(rsample)
library(tidyr)
library(broom)
library(flextable)
library(mgcv)
library(reshape2)
library(patchwork)
library(corrplot)
library(visreg) #VISUALIZACION DE MODELO GAM
library(splines) #SPLINES
library(Metrics) #SPLIN DE REGRESION
library(MLmetrics) #SPLIN DE REGRESION
library(graphics)
library(tidyr)


#Importamos el dataset
pisa <- read_csv("pisasci2006.csv", 
                                col_types = cols(Overall = col_number(), 
                                                 Issues = col_number(), Explain = col_number(), 
                                                 Evidence = col_number(), Interest = col_number(), 
                                                 Support = col_number(), Income = col_number(), 
                                                 Health = col_number(), Edu = col_number(), 
                                                 HDI = col_number()))

pisa <- drop_na(pisa)
dim(pisa)
View(pisa)


#EDA
pisa <- dplyr::select(pisa, -Country, -Issues, -Explain, -Evidence)
par(mfrow=c(1,1))

col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
corrplot(cor(pisa), method = "color", col = col(200),
         type = "upper", order = "hclust", number.cex = .7,
         addCoef.col = "black", # Add coefficient of correlation
         tl.col = "black", tl.srt = 90, # Text label color and rotation
         # Combine with significance
         sig.level = 0.01, insig = "blank", 
         # hide correlation coefficient on the principal diagonal
         diag = TRUE)

par(mfrow=c(2,4))

pisa <- as.data.frame(pisa)

for (p in 1:7) {
  hist(pisa[,p],main=colnames(pisa)[p],xlab="",col="lightblue")
  lines(density(pisa[,p]))
}

pisa <- read_csv("pisasci2006.csv", 
                 col_types = cols(Overall = col_number(), 
                                  Issues = col_number(), Explain = col_number(), 
                                  Evidence = col_number(), Interest = col_number(), 
                                  Support = col_number(), Income = col_number(), 
                                  Health = col_number(), Edu = col_number(), 
                                  HDI = col_number()))


#Dividimos la muestra en training y en test
set.seed(1998)

pisa_split <- initial_split(pisa, prop = .7)
dim(pisa)
#Practica (70%)
pisa_training_data <- training(pisa_split) 
dim(pisa_training_data)
#Test (30%)
pisa_testing_data <- testing(pisa_split) 
dim(pisa_testing_data)



#Plot the data - Lineal Regression
par(mfrow=c(2,3))

g_1 <- plot(Overall ~ Interest, data=pisa) +
  abline(lm(Overall ~ Interest, data = pisa))
g_2 <- plot(Overall ~ Support, data=pisa) +
  abline(lm(Overall ~ Support, data = pisa))
g_3 <- plot(Overall ~ Income, data=pisa) +
  abline(lm(Overall ~ Income, data = pisa))
g_4 <- plot(Overall ~ Health, data=pisa) +
  abline(lm(Overall ~ Health, data = pisa))
g_5 <- plot(Overall ~ Edu, data=pisa) +
  abline(lm(Overall ~ Edu, data = pisa))
g_6 <- plot(Overall ~ HDI, data=pisa) +
  abline(lm(Overall ~ HDI, data = pisa))


#Plot the data -smooth condition https://ggplot2.tidyverse.org/reference/geom_smooth.html

g1 <- ggplot(pisa_training_data, aes(Overall, Interest) ) +
  geom_point() +
  stat_smooth()
g1_smooth <- smooth.spline(Overall, cv = TRUE)


g2 <- ggplot(pisa_training_data, aes(Overall, Support) ) +
  geom_point() +
  stat_smooth()
g3 <- ggplot(pisa_training_data, aes(Overall, Income) ) +
  geom_point() +
  stat_smooth()
g4 <- ggplot(pisa_training_data, aes(Overall, Health) ) +
  geom_point() +
  stat_smooth()
g5 <- ggplot(pisa_training_data, aes(Overall, Edu) ) +
  geom_point() +
  stat_smooth()
g6 <- ggplot(pisa_training_data, aes(Overall, HDI) ) +
  geom_point() +
  stat_smooth()

(g1 + g2 + g3)/
  (g4 + g5 + g6)

#
# Splin de Regresión
#
library(Metrics) #SPLIN DE REGRESION
library(MLmetrics) #SPLIN DE REGRESION

#grados de libertad = 3
knots <- quantile(pisa_training_data$Overall, p = c(0.25, 0.5, 0.75))

#MODELIZO y genero el B-Spline para un Spline polinomial
model_Interest <- lm (Overall ~ bs(Interest, knots = knots), data = pisa_training_data)
model_Support <- lm (Overall ~ bs(Support, knots = knots), data = pisa_training_data)
model_Income <- lm (Overall ~ bs(Income, knots = knots), data = pisa_training_data)
model_Health <- lm (Overall ~ bs(Health, knots = knots), data = pisa_training_data)
model_Edu <- lm (Overall ~ bs(Edu, knots = knots), data = pisa_training_data)
model_HDI <- lm (Overall ~ bs(HDI, knots = knots), data = pisa_training_data)


#realizo las predicciones
options(warn=-1)

predictions_Interest <- model_Interest %>% predict(pisa_testing_data)
predictions_Support <- model_Support %>% predict(pisa_testing_data)
predictions_Income <- model_Income %>% predict(pisa_testing_data)
predictions_Health <- model_Health %>% predict(pisa_testing_data)
predictions_Edu <- model_Edu %>% predict(pisa_testing_data)
predictions_HDI <- model_HDI %>% predict(pisa_testing_data)

options(warn=1)

#Calculo la rmse y el r2

data.frame(
  RMSE = rmse(predictions_Interest, pisa_testing_data$Overall),
  R2 = R2_Score(predictions_Interest, pisa_testing_data$Overall)
)
data.frame(
  RMSE = rmse(predictions_Support, pisa_testing_data$Overall),
  R2 = R2_Score(predictions_Support, pisa_testing_data$Overall)
)
data.frame(
  RMSE = rmse(predictions_Income, pisa_testing_data$Overall),
  R2 = R2_Score(predictions_Income, pisa_testing_data$Overall)
)
data.frame(
  RMSE = rmse(predictions_Health, pisa_testing_data$Overall),
  R2 = R2_Score(predictions_Health, pisa_testing_data$Overall)
)
data.frame(
  RMSE = rmse(predictions_Edu, pisa_testing_data$Overall),
  R2 = R2_Score(predictions_Edu, pisa_testing_data$Overall)
)
data.frame(
  RMSE = rmse(predictions_HDI, pisa_testing_data$Overall),
  R2 = R2_Score(predictions_HDI, pisa_testing_data$Overall)
)

#Visualizo

gg_1 <- ggplot(pisa_training_data, aes(Overall, Interest) ) +
  geom_point() +
  stat_smooth(method = lm, formula = y ~ splines::bs(x, df = 4))
gg_2 <- ggplot(pisa_training_data, aes(Overall, Support) ) +
  geom_point() +
  stat_smooth(method = lm, formula = y ~ splines::bs(x, df = 4))
gg_3 <- ggplot(pisa_training_data, aes(Overall, Income) ) +
  geom_point() +
  stat_smooth(method = lm, formula = y ~ splines::bs(x, df = 4))
gg_4 <- ggplot(pisa_training_data, aes(Overall, Health) ) +
  geom_point() +
  stat_smooth(method = lm, formula = y ~ splines::bs(x, df = 4))
gg_5 <- ggplot(pisa_training_data, aes(Overall, Edu) ) +
  geom_point() +
  stat_smooth(method = lm, formula = y ~ splines::bs(x, df = 4))
gg_6 <- ggplot(pisa_training_data, aes(Overall, HDI) ) +
  geom_point() +
  stat_smooth(method = lm, formula = y ~ splines::bs(x, df = 4))

(gg_1 + gg_2 + gg_3)/
  (gg_4 + gg_5 + gg_6)



#
#Modelo GAM
#

#Multiple Predictors

linear_model <- lm(Overall ~ Interest + Support + Income + Health + Edu + HDI, data=pisa_training_data)
summary(linear_model)

gam_model_1 <- gam(Overall ~ s(Income) + s(Health)  + s(Edu) + s(HDI), data=pisa_training_data)
summary(gam_model_1)

gam_model_2 <- gam(Overall ~ s(Income)  + s(Edu) + s(HDI), data=pisa_training_data)
summary(gam_model_2)

gam_model_3 <- gam(Overall ~ s(Income) + s(Edu) , data=pisa_training_data)
summary(gam_model_3)

library(visreg) #VISUALIZACION DE MODELO GAM

par(mfrow=c(2,2))
plot(linear_model)
par(mfrow=c(2,3))
visreg(linear_model)

par(mfrow=c(2,2))
plot(gam_model_1)
par(mfrow=c(1,3))
plot(gam_model_2)
par(mfrow=c(1,2))
plot(gam_model_3)

par(mfrow=c(1,1))
vis.gam(gam_model_3, type='response', plot.type='contour')

visreg2d(gam_model_3, xvar='Income', yvar='Edu', scale='response')
visreg2d(gam_model_2, xvar='Income', yvar='HDI', scale='response')



par(mfrow=c(2,2))
gam.check(gam_model_1)
gam.check(gam_model_2)
gam.check(gam_model_3)

#ANOVA
anova(linear_model,gam_model_1,gam_model_2,gam_model_3, test = 'F')


#CV
set.seed(1998)

#MODELOS
linear_model <- lm(Overall ~ Interest + Support + Income + Health + Edu + HDI, data=pisa_training_data)
predictions_linear_model <- linear_model %>% predict(pisa_testing_data)
data.frame(
  RMSE = rmse(predictions_linear_model, pisa_testing_data$Overall),
  R2 = R2_Score(predictions_linear_model, pisa_testing_data$Overall)
)


gam_model_1 <- gam(Overall ~ s(Income) + s(Health)  + s(Edu) + s(HDI), data=pisa_training_data)
predictions_gam_model_1 <- gam_model_1 %>% predict(pisa_testing_data)
data.frame(
  RMSE = rmse(predictions_gam_model_1, pisa_testing_data$Overall),
  R2 = R2_Score(predictions_gam_model_1, pisa_testing_data$Overall)
)

gam_model_2 <- gam(Overall ~ s(Income)  + s(Edu) + s(HDI), data=pisa_training_data)
predictions_gam_model_2 <- gam_model_2 %>% predict(pisa_testing_data)
data.frame(
  RMSE = rmse(predictions_gam_model_2, pisa_testing_data$Overall),
  R2 = R2_Score(predictions_gam_model_2, pisa_testing_data$Overall)
)

gam_model_3 <- gam(Overall ~ s(Income) + s(Edu) , data=pisa_training_data)
predictions_gam_model_3 <- gam_model_3 %>% predict(pisa_testing_data)
data.frame(
  RMSE = rmse(predictions_gam_model_3, pisa_testing_data$Overall),
  R2 = R2_Score(predictions_gam_model_3, pisa_testing_data$Overall)
)














