#Import Libraries

library(readxl)
library(readr)
library(forecast)
library(xts)
library(ggplot2)
library(ggfortify)
library(corrplot)
library(zoo)
library(TSA)

#Rpositorio Github:

# https://github.com/luoyuweidu/Time-series--Intervention-Analysis/blob/master/code.R

#Import Dataset

data <- read_excel("data.xlsx")
data['Fecha'] <- seq(as.Date("1958/01/07"), as.Date("1963/04/22"), by = "week")
data <- data[,3:5]

data$Fecha <- as.Date(data$Fecha, "%d/%m/%Y") 
head(data)

X <- data[,1:2]
Y <- data[,3]

#Convertir valores

xts_Crest <- xts(data$Crest, order.by = data$Fecha)
zCrest <- to.weekly((xts_Crest))
zCrest <- as.zoo(xts_Crest)
names(zCrest) <- "Cuota Crest"

df_crest <- data.frame(value = as.vector(zCrest),
                      time = time(zCrest))


xts_Colgate <- xts(data$Colgate, order.by = data$Fecha)
zColgate <- to.weekly((xts_Colgate))
zColgate <- as.zoo(xts_Colgate)
names(zColgate) <- "Cuota Colgate"

df_colgate <- data.frame(value = as.vector(zColgate),
                       time = time(zColgate))


data_union <- cbind(xts_Crest,xts_Colgate)
names(data_union) <- c("Colgate", "Crest")
View(data_union)

#EDA

#Moving average
autoplot(data_union)
autoplot(log(data_union)) #no tiene sentido tomar logaritmo

autoplot(data_union, facets = FALSE) +
  ggtitle("Cuota de Mercado Crest y Colgate") +
  xlab("Tiempo") +
  ylab("Cuota de Mercado") +
  geom_vline(xintercept = as.Date("1960-08-01"), col = "black", alpha = 1)


hist(data_union$Colgate, col = 'royalblue1', border = 0)
hist(data_union$Crest, col = 'coral1', border = 0)

corrplot(cor(data_union, method = 'pearson'), method = 'pie', type = 'lower')
cor(data$Crest, data$Colgate)
summary(data_union)

is.null(data_union) #no hay NA´s


#TRAINING Y TEST

semanas_pred <- 16
semanas_crest <- length(zCrest) #276
semanas_colgate <- length(zColgate) #276



training_crest <- window(zCrest, start = index(zCrest[1]),
                       end = index(zCrest[semanas_crest - semanas_pred])) #de 1 a 260
training_crest_log <- window(log(zCrest), start = index(zCrest[1]),
                         end = index(zCrest[semanas_crest - semanas_pred])) #de 1 a 260

test_crest <- window(zCrest, start = index(zCrest[semanas_crest - semanas_pred + 1]),
                     end = index(zCrest[semanas_crest])) #de 260 a 276
test_crest_log <- window(log(zCrest), start = index(zCrest[semanas_crest - semanas_pred + 1]),
                     end = index(zCrest[semanas_crest])) #de 260 a 276


training_colgate <- window(zColgate, start = index(zCrest[1]),
                       end = index(zColgate[semanas_colgate - semanas_pred])) #de 1 a 260
training_colgate_log <- window(log(zColgate), start = index(zCrest[1]),
                           end = index(zColgate[semanas_colgate - semanas_pred])) #de 1 a 260

test_colgate <- window(zColgate, start = index(zColgate[semanas_colgate - semanas_pred + 1]),
                     end = index(zColgate[semanas_colgate])) #de 260 a 276
test_colgate_log <- window(log(zColgate), start = index(zColgate[semanas_colgate - semanas_pred + 1]),
                       end = index(zColgate[semanas_colgate])) #de 260 a 276

#Autocorrelación y Autocorrelación parcial

#Se transforman la varianza y la media para convertir a la serie en estacionaria para garantizar la buena estimación de la predicción. La varianza se realiza mediante la toma de logaritmos y la media mediante la diferenciación (establecer tantos períodos de retardos como sean necesarios). Se ve una correlación parcial a 3-4 semanas en ambos casos (quizá correlación a un mes)

ggtsdisplay(diff(log(zCrest)), main = 'Autocorrelación y Autocorrelación parcial de Crest')

ggtsdisplay(diff(log(zColgate)), main = 'Autocorrelación y Autocorrelación parcial de Colgate')


#####Auto Arima#####

#Crest
arima_crest <- auto.arima(training_crest, lambda = 0)

prediccion_modelo_arima_crest <- forecast(arima_crest, h = semanas_pred )

autoplot(prediccion_modelo_arima_crest, main = "Predicción ARIMA Crest") +
  xlab("Fecha") +
  ylab("Cuota Crest")

ggtsdisplay(arima_crest$residuals)
Box.test(arima_crest$residuals, lag = 3, fitdf = 1, type = 'Lj' ) #0.77


#Colgate
arima_colgate = auto.arima(training_colgate, lambda = 0)
summary(arima_colgate)

prediccion_modelo_arima_colgate <- forecast(arima_colgate, h = semanas_pred)

autoplot(prediccion_modelo_arima_crest, main = "Predicción ARIMA Colgate") +
  xlab("Fecha") +
  ylab("Cuota Colgate")


#otros
ggtsdisplay(arima_colgate$residuals)
Box.test(arima_colgate$residuals, lag = 3, fitdf = 1, type = 'Lj') #0.69


##Valores atípicos

detectAO(arima.modelo.crest) 
detectAO(arima.modelo.colgate) 
detectIO(arima.modelo.crest) 
detectIO(arima.modelo.colgate)


## Modelo intervención ##

mod.intervencion.crest <- arimax(x = training_crest_log, order = c(0,1,1), 
                                 xtransf = data.frame(Aug60_step = 1*(seq(training_crest_log) > 135),
                                                      Aug60_pulse = 1*(seq(training_crest_log) == 135)), 
                                 xreg = data.frame(Aug60_sem2 = 1*(seq(training_crest_log) == 136)), 
                                 io = c(99), 
                                 transfer = list(c(0,0), c(0,0)), 
                                 method = "ML") 

mod.intervencion.colgate <- arimax(x = training_colgate_log, order = c(0,1,1),
                                   xtransf = data.frame(Aug60_step = 1*(seq(training_colgate_log) > 135),
                                                        Aug60_pulse = 1*(seq(training_colgate_log) == 135)),
                                   io = c(102),
                                   transfer = list(c(0,0), c(0,0)),
                                   method = "ML")
mod.intervencion.crest
mod.intervencion.colgate


## Función de transferencia

Se estudia el impacto de la decisión de la ADA en Colgate, el orden es el ARIMA de Colgate y el transfer es 0,0 
pues el impacto es de impulso y se mantiene en el tiempo. Posteriormente, se comprueba la irrelevancia de los residuos 


mod.transf <- arimax(x = colgate.log,
                     order = c(0,1,1), 
                     include.mean = TRUE,
                     xtransf = crest.log, 
                     transfer = list(c(0,0)), 
                     method = "ML")
mod.transf
tsdisplay(mod.transf$residuals)





