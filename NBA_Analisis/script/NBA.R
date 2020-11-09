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
tinytex::install_tinytex()
library(tinytex)


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
nba$Pais <- match(nba$NBA_Country, levels)


#Realizamos el modelo con todas las variables numericas y las cualitativas convertidas a cuantitativas

model <- lm(log(Salary, base = 10) ~ NBA_DraftNumber + Age + G + MP + PER + PAr + FTr + ORB + DRB + TRB +
              AST + STL + BLK + TOV + USG + OWS + DWS + WS + WS_1 + OBPM + DBPM + BPM + VORP + Team + Pais, data = nba)
model

plot(model)

summary(model)

#Correlacion

nba_num <- select(nba, -Salary)

CorModel <- round(cor(nba_num),2)
CorModel
corrplot(CorModel, title="Matriz de Correlaciones", tl.col="grey", lowCI.mat = 1, uppCI.mat = 1)

#Modelo de AKAIKE

stepAIC(model, direction = 'both')

#Seleccion de variables en función de lo que el modelo de AKaike ha determinado

primer_filtro <- lm(formula = Salary ~ NBA_DraftNumber + Age + G + MP + PER + 
               PAr + ORB + TRB + USG + WS + OBPM + Team, data = nba)
summary(primer_filtro)

#Modelo VIF como segundo filtro para eliminar la multicolinealidad

vif(primer_filtro)
sqrt(vif(primer_filtro)) > 2

#Modelo tras aplicar VIF

modelo_filtro_vif <- lm(formula = Salary ~ NBA_DraftNumber + Age + G + 
                          PAr + ORB + TRB + USG + WS + OBPM + Team, data = nba)
summary(modelo_filtro_vif)


#Comprobar que la multicolinealidad ha desaparecido

BIC(primer_filtro, modelo_filtro_vif)


#Validación del modelo por medio de la GVLMA

val_modelo <- gvlma(modelo_filtro_vif)
summary(val_modelo)

#Muestr aleatorio simple

set.seed(2020)
n = 25
muestreo <- sample(1:nrow(nba), size = n, replace = FALSE)
nba_muestreo <- nba[muestreo, ]
nba_muestreo

nba_salario <- predict(model4, newdata = nba_muestreo)
nba_salario