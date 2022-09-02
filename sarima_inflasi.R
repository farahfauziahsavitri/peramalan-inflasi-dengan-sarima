library(tseries)
library(forecast)
library(lmtest)

data.sarima=read.csv("C:/Users/farah/Dropbox/PC/Downloads/C-Download/github/inflasi.csv",header=T,sep=",")
data.sarima

pass=ts(data.sarima[,2], frequency=12,start=c(2003))
pass
plot.ts(pass)

acf(pass, lag.max=71)
pacf(pass, lag.max=71)


##Transformasi Box Cox
lamda=BoxCox.lambda(pass)
lamda
# nilainya -0.2. baru dikatakan stabil jika lambda 1 (hyndman)
tf1=pass^lamda
p=BoxCox.lambda(tf1)
p
tf2=tf1^p
q=BoxCox.lambda(tf2)
q
tf3=tf2^q
BoxCox.lambda(tf3)
#karna nilai boxcox sudah mendekati 1 maka bisa dikatakan sudah stabil variansnya


#cek stasioner data dalam rata2(unit root test
adf.test(tf3)
adf.test(tf3,k=12)

diff1=diff(tf3)
acf(diff1,lag.max=100)

diff.s1=diff(diff1,lag=12)
acf(diff.s1,lag.max=100)

adf.test(diff1)
adf.test(diff1,k=12)

#penentuan orde arima
acf(diff.s1,lag.max=71) 
pacf(diff.s1,lag.max=71) 

#kemungkinan model Sarima
fit1=arima(tf3,order=c(2,1,1),seasonal=list(order=c(2,1,1),period=12))
fit2=arima(tf3,order=c(2,1,0),seasonal=list(order=c(2,1,1),period=12))
fit3=arima(tf3,order=c(1,1,1),seasonal=list(order=c(2,1,1),period=12))
fit4=arima(tf3,order=c(1,1,0),seasonal=list(order=c(2,1,1),period=12))
fit5=arima(tf3,order=c(0,1,1),seasonal=list(order=c(2,1,1),period=12))

fit6=arima(tf3,order=c(2,1,1),seasonal=list(order=c(2,1,0),period=12))
fit7=arima(tf3,order=c(2,1,0),seasonal=list(order=c(2,1,0),period=12))
fit8=arima(tf3,order=c(1,1,1),seasonal=list(order=c(2,1,0),period=12))
fit9=arima(tf3,order=c(1,1,0),seasonal=list(order=c(2,1,0),period=12))
fit10=arima(tf3,order=c(0,1,1),seasonal=list(order=c(2,1,0),period=12))

fit11=arima(tf3,order=c(2,1,1),seasonal=list(order=c(1,1,1),period=12))
fit12=arima(tf3,order=c(2,1,0),seasonal=list(order=c(1,1,1),period=12))
fit13=arima(tf3,order=c(1,1,1),seasonal=list(order=c(1,1,1),period=12))
fit14=arima(tf3,order=c(1,1,0),seasonal=list(order=c(1,1,1),period=12))
fit15=arima(tf3,order=c(0,1,1),seasonal=list(order=c(1,1,1),period=12))

fit16=arima(tf3,order=c(2,1,1),seasonal=list(order=c(1,1,0),period=12))
fit17=arima(tf3,order=c(2,1,0),seasonal=list(order=c(1,1,0),period=12))
fit18=arima(tf3,order=c(1,1,1),seasonal=list(order=c(1,1,0),period=12))
fit19=arima(tf3,order=c(1,1,0),seasonal=list(order=c(1,1,0),period=12))
fit20=arima(tf3,order=c(0,1,1),seasonal=list(order=c(1,1,0),period=12))

fit21=arima(tf3,order=c(2,1,1),seasonal=list(order=c(0,1,1),period=12))
fit22=arima(tf3,order=c(2,1,0),seasonal=list(order=c(0,1,1),period=12))
fit23=arima(tf3,order=c(1,1,1),seasonal=list(order=c(0,1,1),period=12))
fit24=arima(tf3,order=c(1,1,0),seasonal=list(order=c(0,1,1),period=12))
fit25=arima(tf3,order=c(0,1,1),seasonal=list(order=c(0,1,1),period=12))
#signifikansi koefisien
coeftest(fit1)
coeftest(fit2)
coeftest(fit3)
coeftest(fit4)
coeftest(fit5)
coeftest(fit6)
coeftest(fit7)
coeftest(fit8)
coeftest(fit9)
coeftest(fit10)
coeftest(fit11)
coeftest(fit12)
coeftest(fit13)
coeftest(fit14)
coeftest(fit15)
coeftest(fit16)
coeftest(fit17)
coeftest(fit18)
coeftest(fit19)
coeftest(fit20)
coeftest(fit21)
coeftest(fit22)
coeftest(fit23)
coeftest(fit24)
coeftest(fit25)


#model yang signifikan semua fit2,5,7,9,10,12,14,15,17,19,20,21,22,24,25

#residual
res2=residuals(fit2)
res5=residuals(fit5)
res7=residuals(fit7)
res9=residuals(fit9)
res10=residuals(fit10)
res12=residuals(fit12)
res14=residuals(fit14)
res15=residuals(fit15)
res17=residuals(fit17)
res19=residuals(fit19)
res20=residuals(fit20)
res21=residuals(fit21)
res22=residuals(fit22)
res24=residuals(fit24)
res25=residuals(fit25)

#diagnostik residuals fit2
#non autokorelasi
Box.test(res2, type=c("Ljung-Box"))
#non heteroskedastisitas
Box.test(res2^2, type=c("Ljung-Box"))
#normalitas
n2=length(res2)
mean2=mean(res2)
sd2=sd(res2)
resn2=rnorm(n2,mean2,sd2)
ks.test(res2,resn2)
#grafik residual
tsdiag(fit2)

#diagnostik residuals fit5
#non autokorelasi
Box.test(res5, type=c("Ljung-Box"))
#non heteroskedastisitas
Box.test(res5^2, type=c("Ljung-Box"))
#normalitas
n5=length(res5)
mean5=mean(res5)
sd5=sd(res5)
resn5=rnorm(n5,mean5,sd5)
ks.test(res5,resn5)
#grafik residual
tsdiag(fit5)

#diagnostik residual fit7
#non autokorelasi
Box.test(res7, type=c("Ljung-Box"))
#non heteroskedastisitas
Box.test(res7^2, type=c("Ljung-Box"))
#normalitas
n7=length(res7)
mean7=mean(res7)
sd7=sd(res7)
resn7=rnorm(n7,mean7,sd7)
ks.test(res7,resn7)
#grafik residual
tsdiag(fit7)


#diagnostik residual fit9
#non autokorelasi
Box.test(res9, type=c("Ljung-Box"))
#non heteroskedastisitas
Box.test(res9^2, type=c("Ljung-Box"))
#normalitas
n9=length(res9)
mean9=mean(res9)
sd9=sd(res9)
resn9=rnorm(n9,mean9,sd9)
ks.test(res9,resn9)
#grafik residual
tsdiag(fit9)

#diagnostik residual fit10
#non autokorelasi
Box.test(res10, type=c("Ljung-Box"))
#non heteroskedastisitas
Box.test(res10^2, type=c("Ljung-Box"))
#normalitas
n10=length(res10)
mean10=mean(res10)
sd10=sd(res10)
resn10=rnorm(n10,mean10,sd10)
ks.test(res10,resn10)
#grafik residual
tsdiag(fit10)

#diagnostik residual fit12
#non autokorelasi
Box.test(res12, type=c("Ljung-Box"))
#non heteroskedastisitas
Box.test(res12^2, type=c("Ljung-Box"))
#normalitas
n12=length(res12)
mean12=mean(res12)
sd12=sd(res12)
resn12=rnorm(n12,mean12,sd12)
ks.test(res12,resn12)
#grafik residual
tsdiag(fit12)

#diagnostik residual fit14
#non autokorelasi
Box.test(res14, type=c("Ljung-Box"))
#non heteroskedastisitas
Box.test(res14^2, type=c("Ljung-Box"))
#normalitas
n14=length(res14)
mean14=mean(res14)
sd14=sd(res14)
resn14=rnorm(n14,mean14,sd14)
ks.test(res14,resn14)
#grafik residual
tsdiag(fit14)

#diagnostik residual fit15
#non autokorelasi
Box.test(res15, type=c("Ljung-Box"))
#non heteroskedastisitas
Box.test(res15^2, type=c("Ljung-Box"))
#normalitas
n15=length(res15)
mean15=mean(res15)
sd15=sd(res15)
resn15=rnorm(n15,mean15,sd15)
ks.test(res15,resn15)
#grafik residual
tsdiag(fit15)

#diagnostik residual fit17
#non autokorelasi
Box.test(res17, type=c("Ljung-Box"))
#non heteroskedastisitas
Box.test(res17^2, type=c("Ljung-Box"))
#normalitas
n17=length(res17)
mean17=mean(res17)
sd17=sd(res17)
resn17=rnorm(n17,mean17,sd17)
ks.test(res17,resn17)
#grafik residual
tsdiag(fit17)

#diagnostik residual fit19
#non autokorelasi
Box.test(res19, type=c("Ljung-Box"))
#non heteroskedastisitas
Box.test(res19^2, type=c("Ljung-Box"))
#normalitas
n19=length(res19)
mean19=mean(res19)
sd19=sd(res19)
resn19=rnorm(n19,mean19,sd19)
ks.test(res19,resn19)
#grafik residual
tsdiag(fit19)

#diagnostik residual fit20
#non autokorelasi
Box.test(res20, type=c("Ljung-Box"))
#non heteroskedastisitas
Box.test(res20^2, type=c("Ljung-Box"))
#normalitas
n20=length(res20)
mean20=mean(res20)
sd20=sd(res20)
resn20=rnorm(n20,mean20,sd20)
ks.test(res20,resn20)
#grafik residual
tsdiag(fit20)

#diagnostik residual fit21
#non autokorelasi
Box.test(res21, type=c("Ljung-Box"))
#non heteroskedastisitas
Box.test(res21^2, type=c("Ljung-Box"))
#normalitas
n21=length(res21)
mean21=mean(res21)
sd21=sd(res21)
resn21=rnorm(n21,mean21,sd21)
ks.test(res21,resn21)
#grafik residual
tsdiag(fit21)

#diagnostik residual fit22
#non autokorelasi
Box.test(res22, type=c("Ljung-Box"))
#non heteroskedastisitas
Box.test(res22^2, type=c("Ljung-Box"))
#normalitas
n22=length(res22)
mean22=mean(res22)
sd22=sd(res22)
resn22=rnorm(n22,mean22,sd22)
ks.test(res22,resn22)
#grafik residual
tsdiag(fit22)

#diagnostik residual fit24
#non autokorelasi
Box.test(res24, type=c("Ljung-Box"))
#non heteroskedastisitas
Box.test(res24^2, type=c("Ljung-Box"))
#normalitas
n24=length(res24)
mean24=mean(res24)
sd24=sd(res24)
resn24=rnorm(n24,mean24,sd24)
ks.test(res24,resn24)
#grafik residual
tsdiag(fit24)


#diagnostik residual fit25
#non autokorelasi
Box.test(res25, type=c("Ljung-Box"))
#non heteroskedastisitas
Box.test(res25^2, type=c("Ljung-Box"))
#normalitas
n25=length(res25)
mean25=mean(res25)
sd25=sd(res25)
resn25=rnorm(n25,mean25,sd25)
ks.test(res25,resn25)
#grafik residual
tsdiag(fit25)



#model yang signifikan semua dan white noise adalah fit5,7,9,10,14,17,19,20,24

#plot aktual dan fitted values
fitted5=fitted(fit5)
plot(tf3)+lines(fitted5, col="red")
#peramalan
forecast5=forecast(fit5, h=12)
summary(forecast5)
plot(forecast5)


#plot aktual dan fitted values
fitted7=fitted(fit7)
plot(tf3)+lines(fitted7, col="red")
#peramalan
forecast7=forecast(fit7, h=12)
summary(forecast7)
plot(forecast7)


#plot aktual dan fitted values
fitted9=fitted(fit9)
plot(tf3)+lines(fitted9, col="red")
#peramalan
forecast9=forecast(fit9, h=12)
summary(forecast9)
plot(forecast9)


#plot aktual dan fitted values
fitted10=fitted(fit10)
plot(tf3)+lines(fitted10, col="red")
#peramalan
forecast10=forecast(fit10, h=12)
summary(forecast10)
plot(forecast10)


#plot aktual dan fitted values
fitted14=fitted(fit14)
plot(tf3)+lines(fitted14, col="red")
#peramalan
forecast14=forecast(fit14, h=12)
summary(forecast14)
plot(forecast14)


#plot aktual dan fitted values
fitted17=fitted(fit17)
plot(tf3)+lines(fitted17, col="red")
#peramalan
forecast17=forecast(fit17, h=12)
summary(forecast17)
plot(forecast17)


#plot aktual dan fitted values
fitted19=fitted(fit19)
plot(tf3)+lines(fitted19, col="red")
#peramalan
forecast19=forecast(fit19, h=12)
summary(forecast19)
plot(forecast19)


#plot aktual dan fitted values
fitted20=fitted(fit20)
plot(tf3)+lines(fitted20, col="red")
#peramalan
forecast20=forecast(fit20, h=12)
summary(forecast20)
plot(forecast20)


#plot aktual dan fitted values
fitted24=fitted(fit24)
plot(tf3)+lines(fitted24, col="red")
#peramalan
forecast24=forecast(fit24, h=12)
summary(forecast24)
plot(forecast24)
