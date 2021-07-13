# This is a package wriiten for Thaihealth
# Written by Dr.Woraphon Yamaka
# Center of Excellence in econometrics, Chiang Mai University
# Email. woraphon.econ@gmai.com
# License : CEE.09


forecastecon=function(Y,Hstep){
  ## ADD LAG
  Lag=function(x, lag){
    c(rep(0,lag), head(x, length(x)-lag))
  }


  ###  Select month=12, hstep.forecast=2
  Hstep=Hstep
  fore1=matrix(0,12,Hstep)
  fore2=matrix(0,12,Hstep)
  fore3=matrix(0,12,Hstep)
  fore4=matrix(0,12,Hstep)
  fore5=matrix(0,12,Hstep)
  ## 12 months

  for ( H in 1:12)  {

    Q=log(Y[,H])

    ### ==============Change here

    Forecast=Q
    ##Recursive forcast
    F1=c()
    F2=c()
    F3=c()
    F4=c()
    F5=c()

    d=c()
    datt=Forecast
    n=length(Forecast)


    for ( i in 1:Hstep){

      dat1=c(datt,F1)
      x1=(ts((dat1)))
      lagx1 <- (Lag(x1, 1))    # add lag 1


      dat2=c(datt,F2)
      x2=(ts((dat2)))
      lagx2 <- (Lag(x2, 1))    # add lag 1


      dat3=c(datt,F3)
      x3=(ts((dat3)))
      lagx3 <- (Lag(x3, 1))    # add lag 1


      dat4=c(datt,F4)
      x4=(ts((dat4)))
      lagx4 <- (Lag(x4, 1))    # add lag 1

      dat5=c(datt,F5)
      x5=(ts((dat5)))
      lagx5 <- (Lag(x5, 1))    # add lag 1



      mod1 <- lm(x1~lagx1)
      mod2 <- nnetTs(x2, m=1, size=1)
      mod3 <- gam(x3~s(lagx3,df=1))
      mod4<-  Arima(x4,order=c(1,1,0),seasonal=list(order=c(0,0,0),period=1))
      #act_hid = c("tanh", "sigmoid", "linear", "exp")
      mod5 <- ann(x=data.frame(lagx5), y=data.frame(x5), size = c(10), act_hid = "sigmoid", act_out = "linear", rang = 0.1)


      # Forecat linear regression
      newdata1=cbind(1,dat1[length(dat1)])
      f1=(coef(mod1)[1]+coef(mod1)[2]*newdata1[,2])
      # Forecast Neural Network
      f2=(predict(mod2,n.ahead=1))
      # Forecast GAM
      newdata3=cbind(1,s(dat3)[length(dat3)])
      f3=(coef(mod3)[1]+coef(mod3)[2]*s(newdata3[,2], df=1))
      # Foracast SARIMA
      f4=(forecast(mod4,h=1)$mean[1])
      # Foracast ANN
      newdata5=data.frame((dat5)[length(dat5)])
      f5=(predict(mod5,newdata5))

      F1[i]=c(f1)    # AR
      F2[i]=c(f2)    # NEURAL NETWORK
      F3[i]=c(f3)    # GAM
      F4[i]=c(f4)    # SARIMA
      F5[i]=c(f5)    # ANN

    }
    # Chose only the best model
    fore1[H,]=F1
    fore2[H,]=F2
    fore3[H,]=F3
    fore4[H,]=F4
    fore5[H,]=F5
  }
}
