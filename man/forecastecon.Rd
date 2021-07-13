\name{forecastecon}
\alias{forecastecon}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Forecasting models
}
\description{
In this function, several forecasting models are introduced namely autoregressive, GAM, neural networks and sarima models.
}
\usage{
forecastecon(Y,Hstep)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
 \item{Y}{ vector of variable}
\item{Hstep}{ h-step forecasts}

}
\details{
The function is designed for predict the data of thaihealth.

}
\value{
\item{result }{The result contain the prediction results of five models: AR, Neural networks, GAM, SARIMA, ANN}

}
\references{

}
\author{
Woraphon Yamaka and Paravee Maneejuk
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
## Required packages
library("tempdisagg")
library("tsDyn")
library("validann")
library("Hmisc")
library("gam")
library(forecast)
library("TSA")

data=read.table(file.choose(),header=TRUE)

#=======Prepare data====================
Y1=ts(data[,1], start=c(2551,1,1), freq=12)
Y2=ts(data[,2], start=c(2551,1,1), freq=12)
Y3=ts(data[,3], start=c(2551,1,1), freq=12)
# COnvert data
# "quarterly"=4
# "semi"=2
# "annual"=1
#" "month"=12

NewY1 <- ta(Y1, conversion = "sum", to = 12)
NewY2 <- ta(Y2, conversion = "sum", to = 12)
NewY3 <- ta(Y3, conversion = "sum", to = 12)


### TEST SARIMA with YEARLY DATA
Year1 <- ta(Y1, conversion = "sum", to = 1)
Year2 <- ta(Y2, conversion = "sum", to = 1)
Year3 <- ta(Y3, conversion = "sum", to = 1)
Tol=log((Year1 +Year2 +Year3 )*0.02)
fit <- auto.arima(Tol, max.p=5, max.q=5, max.P=5, max.Q=5)

#====SARIMA==== Model 1
modcig <- Arima(Tol,order=c(1,0,0),seasonal=list(order=c(1,1,1),period=1))
summary(modcig)
exp(forecast(modcig ,h=5)[]$mean) # 2% is income of SSS
plot(forecast(modcig ,h=5), main= "Total revenue")


#====== Rearrange data month to month (OCT-SEP)
n=nrow(data)/12  ## Number of YEAR
Qcig=matrix(0,n,12)
Qalcho=matrix(0,n,12)
Qbeer=matrix(0,n,12)

for ( i in 1:12){
I=i-1
for (j in 1:n){
Qcig[j,i]=NewY1[(I+(1+((j-1)*12)))]
Qalcho[j,i]=NewY2[(I+(1+((j-1)*12)))]
Qbeer[j,i]=NewY3[(I+(1+((j-1)*12)))]
}
}

Y=Qcig+Qalcho+Qbeer
# Main function
model=forecastecon(Y=Y,Hstep=4)
Eforecast=(model$SARIMA+model$ANN)/2
windows()
actual=c(336.8,364.93,417.61,416.07,335.34,440.75,257.69,168.02,183.61,473.54,369.53,331.23,348.82, 356.39,381.80,379.91)
plot(ts(Eforecast,start=c(2563,1),freq=12), ylim=c(100,600), type="b", ylab="million baht")
lines(ts(actual,start=c(2563,1),freq=12),col = "red", lwd = 2,lty = 4, type="l")


}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ Forecasts }% use one of  RShowDoc("KEYWORDS")
\keyword{ Thai health }% __ONLY ONE__ keyword per line
