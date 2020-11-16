#Importacion de librerias

library(readr)
library(forecast)
library(xts)
library(ggplot2)
library(ggfortify)


#Importamos los datos

ventas <- read.csv("IngresosApple.csv", sep = ";")

#Transformamos los datos

ventas <- data.frame(ventas[,-1], row.names =  ventas[,1])

colnames(ventas)[1] <- 'Ingresos'

head(ventas)

#Visualizamos los datos de manera general
zventas <- as.zoo(ventas$Ingresos)

autoplot(zventas)+ggtitle('Ingresos Trimestrales Apple')+
  xlab('Trimestres')+ylab('Ingresos')


#Dividimos los datos por trimestres

tsventas <- ts(coredata(ventas), start = c(2008,2), frequency = 4)

ggfreqplot(tsventas, freq = 4, nrow = 1, facet.labeller = c('T1','T2','T3','T4')) +
  ggtitle('Ingresos Trimestrales')



#Seasonal, trend & remainder

stl(tsventas[, 1], s.window = "periodic")

plot(stl(tsventas[, 1], s.window = "periodic"))


#Tipo de serie temporal

Damped Mult & Multiplica


#Estimacion

#Fit Simple Exponential Smoothing
fit1 <- ses(zventas)

#Fit Holt
fit2 <- holt(zventas)

#Fit Holt- exponential
fit3 <- holt(zventas,exponential=TRUE,initial="simple")

#Fit Holt - damped
fit4 <- holt(zventas,damped=TRUE)

#Fit Holt - (exponential+damped)
fit5 <- holt(zventas,exponential=TRUE,damped=TRUE)

fit1$model
fit2$model
fit3$model
fit4$model
fit5$model



#Plot models fitted
plot(fit3, type="o", ylab="Primas",  flwd=1, plot.conf=FALSE)
lines(window(zventas),type="o")
lines(fit1$mean,col=2)
lines(fit2$mean,col=3)
lines(fit4$mean,col=5)
lines(fit5$mean,col=6)
legend("topleft", lty=1, pch=1, col=1:6,
       c("Data","SES","Holt's","Exponential",
         "Additive Damped","Multiplicative Damped"))



#seasonal model Holt-winters
fit6 <- hw(tsventas,seasonal="additive")
fit7 <- hw(tsventas,seasonal="multiplicative")



#Plot models
plot(fit7,ylab="Primas",
     plot.conf=FALSE, type="o", fcol="white", xlab="Year")
lines(window(zventas),type="o",col="blue")
lines(fitted(fit6), col="red", lty=2)
lines(fitted(fit7), col="green", lty=2)
lines(fit6$mean, type="o", col="red")
lines(fit7$mean, type="o", col="green")
legend("topleft",lty=1, pch=1, col=1:3, 
       c("data","Holt Winters' Additive","Holt Winters' Multiplicative"))

#Calculate Components
states <- cbind(fit6$model$states[,1:3],fit7$model$states[,1:3])
colnames(states) <- c("level","slope","seasonal","level","slope","seasonal")
plot(states, xlab="Year")

#Datos
fit6$model$state[,1:3]
fitted(fit6)
fit6$mean





#select automatic ETS

## Select automatic ETS
etsfit<-ets(tsventas)
#forecast model
fventas.ets=forecast(etsfit)
#Results
summary(fventas.ets)

#Plot
plot(fventas.ets)
lines(window(zventas),type="o")

#-----------------------------------------


zlVentas=log(zventas)

df_newl <- data.frame(value = as.vector(zlVentas),
                      time = time(zlVentas))

ggplot(df_newl)+geom_point(aes(x=time,y=value))+geom_line(aes(x=time,y=value))+ylab("Ventas")+ggtitle("Ventas Trimestrales LOG Apple")+xlab("Trimestres")

ggtsdisplay(zlVentas)

ggtsdisplay(diff(zlVentas))


ggtsdisplay(diff(zlVentas,4))


ggtsdisplay(diff(diff(zlVentas,4),1))








