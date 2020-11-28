library(quantmod) 
library(forecast)
library(fGarch)

library(forecast)
library(xts)
library(ggplot2)
library(ggfortify)
library(corrplot)
library(zoo)
library(TSA)

#Importar datos

getSymbols('BMW.DE', from='2014-01-02', to='2020-11-24')
getSymbols('VOW3.DE', from='2014-01-02', to='2020-11-24')

BMW <- BMW.DE[,4]
VOW <- VOW3.DE[,4]

BMW <- na.omit(BMW)
VOW <- na.omit(VOW)

dim(BMW)
dim(VOW)

View(BMW.DE)
View(VOW)

data_union <- cbind(BMW, VOW)

#RENDIMINETOS



autoplot.zoo(BMW)
autoplot.zoo(VOW)

autoplot(data_union)
autoplot(log(data_union)) #no tiene sentido tomar logaritmo

autoplot(data_union, facets = FALSE) +
  ggtitle("Cuota de Mercado Crest y Colgate") +
  xlab("Tiempo") +
  ylab("Cuota de Mercado") +
  geom_vline(xintercept = as.Date("1960-08-01"), col = "black", alpha = 1)

#### GARCH ####



#funciones
archTest <- function(rtn,m=10){
  # Perform Lagrange Multiplier Test for ARCH effect of a time series
  # rtn: time series
  # m: selected AR order
  # TSAY(2013)
  y=(rtn-mean(rtn))^2
  T=length(rtn)
  atsq=y[(m+1):T]
  x=matrix(0,(T-m),m)
  for (i in 1:m){
    x[,i]=y[(m+1-i):(T-i)]
  }
  md=lm(atsq~x)
  summary(md)
}


###FIN FUNCIONES



#Calculate Daily Arithmetic Return
dRentCont=dailyReturn(BMW,type='log',leading=FALSE)
#Exclude NA (First data)
dRentCont=na.exclude(dRentCont)

plot.zoo(cbind(BMW,dRentCont),main=paste('BMW y  Rentabilidad'),xlab="años",ylab=c("Precio","rentabilidad"))
grid(lwd=2)


#Volatilidad GARCH
#Plot return squared
plot.zoo(cbind(BMW,dRentCont,dRentCont^2),main=paste("BMW y  Rentabilidad"),xlab="años",ylab=c("Precio","rentabilidad","Volatilidad"))

#testing mean
t.test(dRentCont)

#ACF & PACF 
# VolProxy=abs(dRentCont) # absolute value
VolProxy=dRentCont^2 #squared


#ACF y PACF
tsdisplay(VolProxy) 

#Ljung-Box Test 
Box.test(VolProxy,lag=10,  type="Lj")
Box.test(VolProxy,lag=20,  type="Lj")
Box.test(VolProxy,lag=40,  type="Lj")

#LM test
archTest(dRentCont,20)

#ARCH(1)
m1=garchFit(~1+garch(1,0),data=dRentCont,trace=F) # Fit an ARCH(1) model
summary(m1)
resi=residuals(m1,standardize=T) #residuals
resi=xts(resi,order.by=index(dRentCont)) #residuals as xts
tsdisplay(resi^2) #acf pacf residuals

#GARCH(1,1)
m2=garchFit(~1+garch(1,1),data=dRentCont,trace=F) # Fit an GARCH(1,1) model
summary(m2)

resi=residuals(m2,standardize=T) #residuals
resi=xts(resi,order.by=index(dRentCont)) #residuals as xts
tsdisplay(resi^2) #acf pacf residuals
plot(m2)


#t-student
m3=garchFit(~1+garch(1,1),data=dRentCont,trace=F,cond.dist="std")
summary(m3)
plot(m3)


v1=volatility(m3)  # Obtain volatility
v1=xts(v1,order.by=index(dRentCont)) #  volatility as XTS
plot(sqrt(252)*v1)

resi=residuals(m3,standardize=T) # Standardized residuals
resi=xts(resi,order.by=index(dRentCont)) # Standardized residuals as XTS
tsdisplay(resi^2) #acf pacf residuals
plot(resi)

predict(m3) #forecast volatility
predict(m3, n.ahead = 10, plot=TRUE, crit_val=2) #plot with 2*standard error
predict(m3,n.ahead=20,plot=TRUE,conf=.9,nx=100) # plot 100 data with 90% confidence




#### VAR ####

library(vars)


rbmw=monthlyReturn(BMW)
rvow=monthlyReturn(VOW)

#generar vector
vY=cbind(rbmw,rvow)
colnames(vY)=c("BMW","VOW")
vY=na.omit(vY)

#Seleccionar modelo
VARselect(vY)
#estimar
model.var=VAR(vY)
summary(model.var)
model.var1=VAR(vY,type="none")
summary(model.var1)
#causalidad de granger
causality(model.var1)
#respuesta al impulso
model.ri=irf(model.var1)
model.ri
plot(model.ri)
##prediccion
predict(model.var1, n.ahead = 8, ci = 0.95) 


## Ejemplo 2 ##

library(quantmod)
library(vars)



var1 <- VAR(vY, lag.max=4, ic="AIC")

summary(var1)
var1
VARselect(vY,lag.max=4)

plot(var1) #Diagram of fit and residuals for each variables
coef(var1) #concise summary of the estimated variables
residuals(var1) #list of residuals (of the corresponding ~lm)
fitted(var1) #list of fitted values

var.pred <- predict(var1, n.ahead=10, ci=0.95)
Phi(var1) #coefficient matrices of VMA representation
var.irf <- irf(var1)
plot(var.irf)










