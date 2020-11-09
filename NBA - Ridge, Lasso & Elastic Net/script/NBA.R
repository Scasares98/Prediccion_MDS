
#Importacion de librerías
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


nba <- read_delim("nba_2.csv", ";", escape_double = FALSE, 
                    trim_ws = TRUE)

attach(nba)
head(nba)
summary(nba)
names(nba)


#Limpieza de datos
nba <- na.omit(nba)

#Modificamos Variables para trabajarlas mejor

#nba <- rename_with(nba, ~ tolower(gsub('%', '', .x, fixed = T)))
#nba <- rename_with(nba, ~ tolower(gsub('3', 'three', .x, fixed = T)))
#nba <- rename_with(nba, ~ tolower(gsub('/', '_', .x, fixed = T)))



#Convertir la variable Tm (Teams) de cualitativa a Cuantitativa
unique(nba$Tm)
levels <- c('HOU', 'GSW', 'SAC', 'CHI', 'POR',
            'DAL', 'BOS', 'MEM', 'DEN',
            'TOT', 'LAC', 'ORL', 'MIA',
            'IND', 'LAL', 'MIN', 'PHO',
            'ATL', 'CLE', 'NYK', 'CHO',
            'MIL', 'SAS', 'UTA', 'NOP',
            'WAS', 'PHI', 'BRK', 'OKC',
            'DET', 'TOR')

nba$Team <- match(nba$Tm, levels)

#Convertir la variable NBA_Country (Países) de cualitativa a Cuantitativa
unique(nba$NBA_Country)
niveles <- c('China', 'Georgia', 'USA', 'Canada', 'Spain',
            'France', 'Czech Republic', 'Russia', 'South Sudan',
            'Switzerland', 'New Zealand', 'Haiti', 'Democratic Re_',
            'Tunisia', 'Brazil', 'Germany', 'Australia',
            'Cameroon', 'Israel', 'Turkey', 'United Kingdo...',
            'Montenegro', 'Serbia', 'Argentina', 'Bosnia',
            'Lithuania', 'Croatia', 'Italy', 'Poland',
            'Dominican Rep...', 'Finland', 'Latvia', 'Bosnia & Herz...',
            'Sweden', 'Ukraine', 'Austria', 'Puerto Rico',
            'Senegal', 'Slovenia', 'Greece', 'Democratic Re...',
            'Mali', 'Bahamas', 'Egypt')
nba$Pais <- match(nba$NBA_Country, niveles)


#Realizamos el modelo con todas las variables numericas y las cualitativas convertidas a cuantitativas

model <- lm(log(Salary, base = 10) ~ NBA_DraftNumber + Age + G + MP + PER + PAr + FTr + ORB + DRB + TRB +
              AST + STL + BLK + TOV + USG + OWS + DWS + WS + WS_1 + OBPM + DBPM + BPM + VORP + Team + Pais, data = nba)
model

#plot(model)
#summary(model)

#------------------------------------------------------------
#DIVIDIR EL MODELO EN LA PARTE DE TRAINING (70%) Y TEST (30%) - Aleatoriamente
#------------------------------------------------------------

#Dividir el Dataset

set.seed(2020)


nba_split <- initial_split(nba, prop = .7, strata = 'Salary')

#Practica (70%)
nba_training_data <- training(nba_split) 
dim(nba_training_data)

#Test (30%)
nba_testing_data <- testing(nba_split) 
dim(nba_testing_data)

# Create training and testing feature model matrices and response vectors.
# we use model.matrix(...)[, -1] to discard the intercept
nba_training_data_a <- model.matrix(Player ~ ., nba_training_data)[, -1]
nba_training_data_b <- log(nba_training_data$Salary)

nba_testing_data_a <- model.matrix(Player ~ ., nba_testing_data)[, -2]
nba_testing_data_b <- log(nba_testing_data$Salary)

View(nba_training_data_a)
View(nba_training_data_b)

View(nba_testing_data_a)
View(nba_testing_data_b)


#-------------------------
#Ridge Regression
#-------------------------

nba_ridge <- glmnet(
  x = nba_training_data_a,
  y = nba_training_data_b,
  alpha = 0
)

plot(nba_ridge, xvar = 'lambda', main = 'Ridge')

#Encabezado
nba_ridge$lambda %>% head()
names(nba_ridge)
head(nba_ridge)


#------------
#Tuning A - Ver modelo con menor error al cuadrado en funcion de su lambda
#------------

nba_ridge_cv <- cv.glmnet(
  x = nba_training_data_a,
  y = nba_training_data_b,
  alpha = 0
)

# plot results
plot(nba_ridge_cv)


#Minimo
min(nba_ridge_cv$cvm) #0.6679
#Valor de lambda para el minimo
nba_ridge_cv$lambda.min  #0.112
log(nba_ridge_cv$lambda.min) #-2.185

#Minimo LAMBDA + 1 desv estandar
nba_ridge_cv$cvm[nba_ridge_cv$lambda == nba_ridge_cv$lambda.1se]
nba_ridge_cv$lambda.1se
#Valor Lmabda
log(nba_ridge_cv$lambda.1se)

#Grafica del valor de los coecifientes para el valor de lambda
plot(nba_ridge, xvar = "lambda")
abline(v = log(nba_ridge_cv$lambda.1se), col = "red", lty = "dashed")

#Tabla top 25 coecifientes con mas influencia
coef(nba_ridge_cv, s = "lambda.1se") %>%
  broom::tidy() %>%
  top_n(25, wt = abs(value)) %>%
  ggplot(aes(value, reorder(row, value))) +
  geom_point() +
  ggtitle("Top 25 influential variables") +
  xlab("Coefficient") +
  ylab(NULL)



#-------------------------
#Lasso Regression
#-------------------------

#Calculo coecifientes en funcion de lambda
nba_lasso <- glmnet(
  x = nba_training_data_a,
  y = nba_training_data_b,
  alpha = 1
)

plot(nba_lasso, xvar = "lambda")


#Calculo medio de residuos en funcion del logaritmo de lambda

nba_lasso_cv <- cv.glmnet(
  x = nba_training_data_a,
  y = nba_training_data_b,
  alpha = 1
)

plot(nba_lasso_cv)

#Valor de lambda para el minimo MSE
min(nba_lasso_cv$cvm) 
#Valor minimo de lambda
nba_lasso_cv$lambda.min

#Valor minimo de lambda + 1 desviacion estandar
nba_lasso_cv$cvm[nba_lasso_cv$lambda == nba_lasso_cv$lambda.1se]
log(nba_lasso_cv$lambda.1se)


#Grafica del valor de los coecifientes en funcion de lambda

plot(nba_lasso, xvar = 'lambda')
abline(v = log(nba_lasso_cv$lambda.min), col = "red", lty = "dashed")
abline(v = log(nba_lasso_cv$lambda.1se), col = "red", lty = "dashed")


#Tabla coecifientes con mas influencia


coef(nba_lasso_cv, s = "lambda.1se") %>%
  tidy() %>%
  ggplot(aes(value, reorder(row, value), color = value > 0)) +
  geom_point(show.legend = FALSE) +
  ggtitle("Influential variables") +
  xlab("Coefficient") +
  ylab(NULL)



#-------------------------
#Elastic Net Regression
#-------------------------


lasso    <- glmnet(nba_training_data_a, nba_training_data_b, alpha = 1.0) 
elastic1 <- glmnet(nba_training_data_a, nba_training_data_b, alpha = 0.25) 
elastic2 <- glmnet(nba_training_data_a, nba_training_data_b, alpha = 0.75) 
ridge    <- glmnet(nba_training_data_a, nba_training_data_b, alpha = 0.0)

par(mfrow = c(2, 2), mar = c(6, 4, 6, 2) + 0.1)
plot(lasso, xvar = "lambda", main = "Lasso (Alpha = 1)\n\n\n")
plot(elastic1, xvar = "lambda", main = "Elastic Net (Alpha = .25)\n\n\n")
plot(elastic2, xvar = "lambda", main = "Elastic Net (Alpha = .75)\n\n\n")
plot(ridge, xvar = "lambda", main = "Ridge (Alpha = 0)\n\n\n")


#Ajuste del modelo (tuning)

# maintain the same folds across all models
fold_id <- sample(1:10, size = length(nba_training_data_b), replace=TRUE)

# search across a range of alphas
tuning_grid <- tibble::tibble(
  alpha      = seq(0, 1, by = .1),
  mse_min    = NA,
  mse_1se    = NA,
  lambda_min = NA,
  lambda_1se = NA
)
tuning_grid


for(i in seq_along(tuning_grid$alpha)) {
  
  # fit CV model for each alpha value
  fit <- cv.glmnet(nba_training_data_a, nba_training_data_b, alpha = tuning_grid$alpha[i])
  
  # extract MSE and lambda values
  tuning_grid$mse_min[i]    <- fit$cvm[fit$lambda == fit$lambda.min]
  tuning_grid$mse_1se[i]    <- fit$cvm[fit$lambda == fit$lambda.1se]
  tuning_grid$lambda_min[i] <- fit$lambda.min
  tuning_grid$lambda_1se[i] <- fit$lambda.1se
}

tuning_grid


tuning_grid %>%
  mutate(se = mse_1se - mse_min) %>%
  ggplot(aes(alpha, mse_min)) +
  geom_line(size = 2) +
  geom_ribbon(aes(ymax = mse_min + se, ymin = mse_min - se), alpha = .25) +
  ggtitle("MSE ± one standard error")


#-----------
#Predicon
#-----------

#Lasso
cv_lasso <- cv.glmnet(nba_training_data_a, nba_training_data_b, alpha = 1.0)
min(cv_lasso$cvm)


pred <- predict(cv_lasso, s = cv_lasso$lambda.min, nba_testing_data_a)
mean((nba_testing_data_b - pred)^2)





