library(tseries)
library(forecast)
library(urca)
###Data Selection
ATT=ATT <- read.csv("~/Desktop/ts project/ATT.csv", sep=";")
Ver=Verizon <- read.csv("~/Desktop/ts project/Verizon.csv", sep="")
par(mfrow=c(1,1))
x=ts(log(ATT[85:821,]),start=c(2008,5,1),freq=255)
plot(x,main='Log Price of ATT')
y=ts(log(Ver[85:821,]),start=c(2008,5,1),freq=255)
plot(y,main='Log Price of Verizon')
cor(log(ATT[85:821,]),log(Ver[85:821,]))

###Simple Linear Regression with AR(3) Model
m1=lm(log(ATT[85:821,]) ~ log(Ver[85:821,]))
summary(m1)
wt=m1$residuals
plot(ts(wt),start=c(2008,5,1),freq=255)
acf(wt)
pacf(wt)
m3=arima(wt,order=c(3,0,0),include.mean = F)
m3
summary(m3)
adf.test(m3$residuals)
par(mfrow=c(2,2))
acf(m3$residuals)
acf(m3$residuals^2)
pacf(m3$residuals)
pacf(m3$residuals^2)
par(mfrow=c(1,1))


###Cointegration
ATTad=log(ATT[684:714,])
Verad=log(Ver[684:714,])
xx=ts(ATTad)
plot(xx,main='Selected Log Price of ATT')
yy=ts(Verad)
plot(yy,main='Selected Log Price of Verizon')
xxy=cbind(ATTad,Verad)
mx=ar(xxy)
mx$order
cot=ca.jo(xxy, ecdet='none', type='trace', K=3, spec='longrun')  
summary(cot)
cot@V[2,1] #gamma

###Strategy Application
zz=cbind(xx,yy)
weight=c(1,cot@V[2,1])
z=ts(zz%*%weight)
plot(z)
acf(z)
pacf(z)
sd(z)
mean(z)
abs((z-mean(z))/sd(z))
plot((z-mean(z))/sd(z))

####Fixed Gamma
spread = (z-mean(z))/sd(z)
longpos = rep(0,31)
i = 1
while(i != 31)
{
  if(spread[i]<-1.5)
  {
    longpos[i] = 1
    for(j in (i+1):31)
    {
      if((spread[j]>-0.3)|| j == 31)
        break
    }
    longpos[j] = -1
    i = j + 1
  }
  else
  {
    i = i + 1
  }
}
shortpos = rep(0,31)
i = 1
while(i != 31)
{
  if(spread[i] > 1.5)
  {
    shortpos[i] = 1
    for(j in (i+1):31)
    {
      if((spread[j]<0.3)|| j == 31)
        break
    }
    shortpos[j] = -1
    i = j + 1
  }
  else
  {
    i = i + 1
  }
}

gain1 = 0
buy=c(0,0)
sell=c(0,0)
for(i in 1:31)
{
  if(longpos[i]==1)
  {
    buy = c(ATT[714+i,],Ver[714+i,])
  }
  if(longpos[i] == -1)
  {
    sell = c(ATT[714+i,],Ver[714+i,])
    gain1=gain1+c(1,-Ver[714,]^(cot@V[2,1]-1))%*%(sell-buy)
  }
}

gain2 = 0
buy=c(0,0)
sell=c(0,0)
for(i in 1:31)
{
  if(shortpos[i]==1)
  {
    sell = c(ATT[714+i,],Ver[714+i,])
  }
  if(shortpos[i] == -1)
  {
    buy = c(ATT[714+i,],Ver[714+i,])
    gain2=gain2+c(1,-Ver[714,]^(cot@V[2,1]-1))%*%(sell-buy)
  }
}

gain = gain1 + gain2
gain

####Floating Gamma(Changing Every 5 Days)
####For simplification, I only post the calculation of the first two gains.
####The method of using floating gamma changing every 10 days to calculate the gain is the same.   
####First Gamma
ATTad=log(ATT[684:714,])
Verad=log(Ver[684:714,])
xx=ts(ATTad)
plot(xx,main='Selected Log Price of ATT')
yy=ts(Verad)
plot(yy,main='Selected Log Price of Verizon')
xxy=cbind(ATTad,Verad)
mx=ar(xxy)
mx$order
cot=ca.jo(xxy, ecdet='none', type='trace', K=3, spec='longrun')  
summary(cot)
cot@V[2,1] #gamma
zz=cbind(xx,yy)
weight=c(1,cot@V[2,1])
z=ts(zz%*%weight)
plot(z)
acf(z)
pacf(z)
sd(z)
mean(z)
abs((z-mean(z))/sd(z))
plot((z-mean(z))/sd(z))

spread = (z-mean(z))/sd(z)
longpos = rep(0,6)
i = 1
while(i != 6)
{
  if(spread[i]<-1.5)
  {
    longpos[i] = 1
    for(j in (i+1):6)
    {
      if((spread[j]>-0.3)|| j == 6)
        break
    }
    longpos[j] = -1
    i = j + 1
  }
  else
  {
    i = i + 1
  }
}
shortpos = rep(0,6)
i = 1
while(i != 6)
{
  if(spread[i] > 1.5)
  {
    shortpos[i] = 1
    for(j in (i+1):6)
    {
      if((spread[j]<0.3)|| j == 6)
        break
    }
    shortpos[j] = -1
    i = j + 1
  }
  else
  {
    i = i + 1
  }
}

gain1 = 0
buy=c(0,0)
sell=c(0,0)
for(i in 1:6)
{
  if(longpos[i]==1)
  {
    buy = c(ATT[714+i,],Ver[714+i,])
  }
  if(longpos[i] == -1)
  {
    sell = c(ATT[714+i,],Ver[714+i,])
    gain1=gain1+c(1,-Ver[714,]^(cot@V[2,1]-1))%*%(sell-buy)
  }
}

gain2 = 0
buy=c(0,0)
sell=c(0,0)
for(i in 1:6)
{
  if(shortpos[i]==1)
  {
    sell = c(ATT[714+i,],Ver[714+i,])
  }
  if(shortpos[i] == -1)
  {
    buy = c(ATT[714+i,],Ver[714+i,])
    gain2=gain2+c(1,-Ver[714,]^(cot@V[2,1]-1))%*%(sell-buy)
  }
}

gain = gain1 + gain2
gain

####Second Gamma
ATTad=log(ATT[689:719,])
Verad=log(Ver[689:719,])
xx=ts(ATTad)
plot(xx,main='Selected Log Price of ATT')
yy=ts(Verad)
plot(yy,main='Selected Log Price of Verizon')
xxy=cbind(ATTad,Verad)
mx=ar(xxy)
mx$order
cot=ca.jo(xxy, ecdet='none', type='trace', K=3, spec='longrun')  
summary(cot)
cot@V[2,1] #gamma

spread = (z-mean(z))/sd(z)
longpos = rep(0,6)
i = 1
while(i != 6)
{
  if(spread[i]<-1.5)
  {
    longpos[i] = 1
    for(j in (i+1):6)
    {
      if((spread[j]>-0.3)|| j == 6)
        break
    }
    longpos[j] = -1
    i = j + 1
  }
  else
  {
    i = i + 1
  }
}
shortpos = rep(0,6)
i = 1
while(i != 6)
{
  if(spread[i] > 1.5)
  {
    shortpos[i] = 1
    for(j in (i+1):6)
    {
      if((spread[j]<0.3)|| j == 6)
        break
    }
    shortpos[j] = -1
    i = j + 1
  }
  else
  {
    i = i + 1
  }
}

gain1 = 0
buy=c(0,0)
sell=c(0,0)
for(i in 1:6)
{
  if(longpos[i]==1)
  {
    buy = c(ATT[719+i,],Ver[719+i,])
  }
  if(longpos[i] == -1)
  {
    sell = c(ATT[719+i,],Ver[719+i,])
    gain1=gain1+c(1,-Ver[719,]^(cot@V[2,1]-1))%*%(sell-buy)
  }
}

gain2 = 0
buy=c(0,0)
sell=c(0,0)
for(i in 1:6)
{
  if(shortpos[i]==1)
  {
    sell = c(ATT[719+i,],Ver[719+i,])
  }
  if(shortpos[i] == -1)
  {
    buy = c(ATT[719+i,],Ver[719+i,])
    gain2=gain2+c(1,-Ver[719,]^(cot@V[2,1]-1))%*%(sell-buy)
  }
}

gain = gain1 + gain2
gain