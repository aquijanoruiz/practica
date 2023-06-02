# R commands used in Lecture 1:
#   "##" denotes explanation of the command.

## Install R on your computer as demonstrated in class ##
## double click on R-icon to start R ##
setwd("C:\\JianGuoYun\\Nutstore\\WORKDISK\\Xiamen University\\courses\\financial time series analysis\\mine\\Lecture notes\\Lecture1")  ## set my working directory.
par(mar=c(1,1,1,1)) # solve the issue plot.new() : figure margins too large
### You should use your working directory ###
library(quantmod)  ## or require(quantmod)  load the package "quantmod"
getSymbols("AAPL") ## get daily Apple stock data from Yahoo Finance
dim(AAPL)          ## find the size of the data downloaded
head(AAPL)         ## show the first 6 rows of data
tail(AAPL)         ## show the last 6 rows of data
chartSeries(AAPL)  ## plot Apple daily closing stock prices with trading volume
chartSeries(AAPL,theme="white")  ## Same as the previous command, but use "white" background for the plot.
chartSeries(AAPL[,6]) ## plot the daily adjusted prices of Apple stock.
getSymbols("AAPL",from="2005-01-03",to="2013-12-31")  ## specify the data span

getSymbols("UNRATE",src="FRED") ## Load U.S. monthly unemplyment rate from Federal Reserve Bank of St Louis.
chartSeries(UNRATE) ## plot the U.S. monthly unemployment rate

getSymbols("DEXUSEU",src="FRED") ## Load Dollar verus Euro daily exchange rates from FRED.
chartSeries(DEXUSEU) ## plot the daily dollar-euro exchange rates.

getSymbols("^VIX") ## load daily VIX index
chartSeries(VIX) ## plot daily VIX index

getSymbols("^TNX") ## load interest rate
chartSeries(TNX) ## plot interest rate

#################### 3M Stock ####################
library(fBasics) # Load package
da=read.table("d-mmm-0111.txt",header=T) # Load data
# header=T means the first row of data file contains names.
# deafult is no names.
head(da) #Show the first 6 rows of data
mmm=da[,2]  # Obtain 3m simple returns
basicStats(mmm) #Compute summary statistics
mean(mmm)
var(mmm)
stdev(mmm) # standard deviation
t.test(mmm)  # Testing mean return = 0
s3=skewness(mmm)
T=length(mmm) # Sample size
t3=s3/sqrt(6/T) # Skewness test
pp=2*(1-pnorm(t3)) # Compute p-value
s4=kurtosis(mmm)
t4=s4/sqrt(24/T) # Kurtosis test
normalTest(mmm,method='jb') # JB-test

#################### IBM Stock and SP500 ####################
da=read.table("m-ibmsp-2611.txt",header=T) #Load  data
dim(da)
ibm=log(da$ibm+1) # Compute log returns
sp=log(da$sp+1)
rt=cbind(ibm,sp) # Obtain bivariate returns
m1=apply(rt,2,mean) # Obtain sample means
v1=cov(rt) # Obtain sample covariance matrix
m1
v1
library(mnormt) # Load package
x=rmnorm(1029,mean=m1,varcov=v1) # Simulation of multivariate normal distribution
dim(x)
plot(x[,2],x[,1],xlab='sim-sp',ylab='sim-ibm',cex=0.8)


#################### correlations ####################
da=read.table("m-ibmsp6709.txt",header=T)
head(da)
ibm=da$ibm
sp5=da$sp
cor(sp5,ibm)
cor(sp5,ibm,method='spearman')
cor(sp5,ibm,method='kendall')

