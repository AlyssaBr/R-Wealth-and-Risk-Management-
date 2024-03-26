###################################
# Created by Alyssa Brunen
##################################
### Hult Inter. Business School
##################################
# Modeling and Analytics MFIN
##################################
# Wealth and Investment Management 
##################################
# Individual Assignment A1
##################################


### Loading Libraries
library(rpart)
library(ggplot2)
library(lattice)
library(rpart.plot)
library(caret)
library(tidyverse)
library(tseries)
library(rugarch)
library(dplyr)
library(corrplot)
library(dplyr)
library(quantmod)
library(readxl)

### Downloading Tickers

VNQ_ticker <- getSymbols("VNQ", auto.assign=FALSE)
QQQ_ticker <- getSymbols("QQQ", auto.assign=FALSE)
IXN_ticker <- getSymbols("IXN", auto.assign=FALSE)
IEF_ticker <- getSymbols("IEF", auto.assign=FALSE)
GLD_ticker <- getSymbols("GLD", auto.assign=FALSE)

### Combine all tickers 

joined_prices <- merge.xts(GLD_ticker, IEF_ticker, IXN_ticker, QQQ_ticker, VNQ_ticker)

### Dataframe with only Adjusted Prices 

Portfolio_Adj.Prices <- joined_prices[, c(6, 12, 18, 24, 30)]

### Data Frame for Returns only 
Portfolio_Returns_no.alloc <- as.data.frame(Portfolio_Adj.Prices) %>%     
  mutate(GLD_ROR=(GLD.Adjusted-lag(GLD.Adjusted))/lag(GLD.Adjusted)) %>%  
  mutate(IEF_ROR=(IEF.Adjusted-lag(IEF.Adjusted))/lag(IEF.Adjusted)) %>%
  mutate(IXN_ROR=(IXN.Adjusted-lag(IXN.Adjusted))/lag(IXN.Adjusted)) %>%
  mutate(QQQ_ROR=(QQQ.Adjusted-lag(QQQ.Adjusted))/lag(QQQ.Adjusted)) %>%
  mutate(VNQ_ROR=(VNQ.Adjusted-lag(VNQ.Adjusted))/lag(VNQ.Adjusted))

### Create Returns 

GLD_returns <- monthlyReturn(getSymbols("GLD", auto.assign=FALSE))
IEF_returns <- monthlyReturn(getSymbols("IEF", auto.assign=FALSE))
IXN_returns <- monthlyReturn(getSymbols("IXN", auto.assign=FALSE))
QQQ_returns <- monthlyReturn(getSymbols("QQQ", auto.assign=FALSE))
VNQ_returns <- monthlyReturn(getSymbols("VNQ", auto.assign=FALSE))

Portfolio_MonthlyROR_no.alloc <- merge.xts(GLD_returns, IEF_returns, IXN_returns,QQQ_returns,VNQ_returns)


# Return Visualization from 2007 
chart_Series(GLD_returns)
chart_Series(IEF_returns)
chart_Series(IXN_returns)
chart_Series(QQQ_returns)
chart_Series(VNQ_returns)


###Renaming the Column Names

colnames(Portfolio_MonthlyROR_no.alloc)[1] <- "GLD"
colnames(Portfolio_MonthlyROR_no.alloc)[2] <- "IEF"
colnames(Portfolio_MonthlyROR_no.alloc)[3] <- "IXN"
colnames(Portfolio_MonthlyROR_no.alloc)[4] <- "QQQ"
colnames(Portfolio_MonthlyROR_no.alloc)[5] <- "VNQ"


### Creating Portfolio Data frame with correct allocation for each Asset 

IXN_alloc <- 0.175
QQQ_alloc <- 0.221
IEF_alloc <- 0.285
VNQ_alloc <- 0.089
GLD_alloc <- 0.23


Portfolio.ret <- as.data.frame(Portfolio_MonthlyROR_no.alloc)%>%
  mutate(GLD_alloc*GLD+ IXN_alloc*IXN+ QQQ_alloc* QQQ + IEF_alloc*IEF + VNQ_alloc*VNQ )

colnames(Portfolio.ret)[6] <- "Total"

######################################################################
## Question 1: What is the most recent 12M*, 18M, 24M (months) return for each of the securities? (And Total)
######################################################################


time_index <- nrow(Portfolio.ret) 

### GLD 12M
Portfolio.ret$GLD[time_index:(time_index-11)]

## GLD 18M 
Portfolio.ret$GLD[time_index:(time_index-17)]

## GLD 24M
Portfolio.ret$GLD[time_index:(time_index-23)]

### IXN 12M
Portfolio.ret$IXN[time_index:(time_index-11)]

### IXN 18M
Portfolio.ret$IXN[time_index:(time_index-17)]

### IXN 24M
Portfolio.ret$IXN[time_index:(time_index-23)]

###IEF 12M
Portfolio.ret$IEF[time_index:(time_index-11)]


### IEF 18M
Portfolio.ret$IEF[time_index:(time_index-17)]

### IEF 24M
Portfolio.ret$IEF[time_index:(time_index-23)]



### QQQ 12M
Portfolio.ret$QQQ[time_index:(time_index-11)]


#### Total Portfolio 
Portfolio.ret$Total[time_index:(time_index-11)]


Portfolio.ret$Total[time_index:(time_index-23)]


########
## Return Visualization Code 
#Returns.all <- as.data.frame(Portfolio_Adj.Prices) %>%     #this means we take the p1-p0 so one up 
#  mutate(GLD_returns=(GLD.Adjusted-lag(GLD.Adjusted))/lag(GLD.Adjusted)) %>%  #lag is the p0 and the first variable is the p1
#  mutate(IEF_returns=(IEF.Adjusted-lag(IEF.Adjusted))/lag(IEF.Adjusted)) %>%
#  mutate(IXN_returns=(IXN.Adjusted-lag(IXN.Adjusted))/lag(IXN.Adjusted)) %>%
#  mutate(QQQ_returns=(QQQ.Adjusted-lag(QQQ.Adjusted))/lag(QQQ.Adjusted)) %>%
#  mutate(VNQ_returns=(VNQ.Adjusted-lag(VNQ.Adjusted))/lag(IEF.Adjusted)) %>%
  
  ### Visualization of Anual return for each Asset 
tickers <- c("GLD", "QQQ", "VNQ", "IXN", "IEF")

get_stock_data <- function(ticker) {
  stock_data <- getSymbols(ticker, src = "yahoo", from = "2022-07-25", to = "2023-07-25", auto.assign = FALSE)
  return(stock_data)
}

all_data <- lapply(tickers, get_stock_data)
names(all_data) <- tickers

# Step 3: Analyze the data and calculate performance metrics
get_returns <- function(stock_data) {
  daily_returns <- na.omit(ROC(Ad(stock_data)))
  annualized_returns <- ((1 + mean(daily_returns))^252) - 1
  return(annualized_returns)
}

returns <- sapply(all_data, get_returns)

# Step 4: Create visual representation (bar plot) of the annualized returns
returns_df <- data.frame(Ticker = names(returns), Annualized_Return = returns)
plot <- returns_df %>%
  ggplot(aes(x = reorder(Ticker, Annualized_Return), y = Annualized_Return)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(title = "Annualized Returns for Stock Tickers",
       x = "Ticker", y = "Annualized Return") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Display the plot
print(plot)

# Step 5: Suggest the ticker with the highest annualized return
best_ticker <- returns_df[which.max(returns_df$Annualized_Return), "Ticker"]
cat("Based on historical data up to July 25th, the suggested ticker for an investor is:", best_ticker, "\n")

######################################################################

######################################################################
## Question 2: What are the correlations between your assets? Are there any interesting correlations?
######################################################################

# For all time 


cor(Portfolio.ret,)

# Output: 
#             GLD         IEF         IXN         QQQ        VNQ     
#GLD   1.00000000  0.36358849  0.05601218  0.07115042 0.12841838 
#IEF   0.36358849  1.00000000 -0.09669245 -0.09775812 0.05071319 
#IXN   0.05601218 -0.09669245  1.00000000  0.97345862 0.64457320 
#QQQ   0.07115042 -0.09775812  0.97345862  1.00000000 0.64747924 
#VNQ   0.12841838  0.05071319  0.64457320  0.64747924 1.00000000 

# For last 12M 

cor(Portfolio.ret[time_index:(time_index-11),], )
# Output: 
#            GLD          IEF         IXN         QQQ        VNQ     
#GLD   1.0000000    0.9071300   0.4802408   0.4554878  0.4945006 
#IEF   0.9071300    1.0000000   0.7363601   0.7370020  0.7284763 
#IXN   0.4802408    0.7363601   1.0000000   0.9810553  0.7579519 
#QQQ   0.4554878    0.7370020   0.9810553   1.0000000  0.7615618 
#VNQ   0.4945006    0.7284763   0.7579519   0.7615618  1.0000000 


## Correlation Visualtization 

cor_matrix<- cor(Portfolio.ret[,c( "GLD", "IEF", "IXN", "QQQ", "VNQ", "Total")])

corrplot(cor_matrix, method = "circle", tl.col = "black", tl.srt = 50)

cor_matrix2<- cor(Portfolio.ret[,c( "GLD", "IEF", "IXN", "QQQ", "VNQ")])

corrplot(cor_matrix2, method = "circle", tl.col = "black", tl.srt = 50)

cor_matrix4<- cor(Portfolio_MonthlyROR_no.alloc[,c( "GLD", "IEF", "IXN", "QQQ", "VNQ")])

corrplot(cor_matrix4, method = "circle", tl.col = "black", tl.srt = 50)


######################################################################
## Question 3: What is the most recent 12M sigma (risk) for each of the securities (and for the entire portfolio)? 
######################################################################

spread <- function(x){
  my_min <- min(x, na.rm = TRUE)
  my_max<- max(x, na.rm = TRUE)
  my_spread <- my_max - my_min
  my_sd <- sd(x, na.rm = TRUE)
  return (c(my_spread, my_sd))
}

# Spread for each asset: 
spread(x=Portfolio.ret$GLD) 
# [1] 0.28926156 0.04936472
spread(x=Portfolio.ret$IEF) 
# [1] 0.12265384 0.01911386
spread(x=Portfolio.ret$IXN) 
# [1] 0.31110491 0.05641641
spread(x=Portfolio.ret$QQQ) 
# [1] 0.30606874 0.05483589
spread(x=Portfolio.ret$VNQ) 
# [1] 0.62410660 0.06625582

# Spread for Total Portfolio 
spread(x=Portfolio.ret$Total) 
# [1] 0.21457414 0.03020739




hist(Portfolio.ret$Total)
hist(Portfolio.ret$GLD)
hist(Portfolio.ret$VNQ)
hist(Portfolio.ret$IXN)
hist(Portfolio.ret$IEF)
hist(Portfolio.ret$QQQ)


##### RISK Calculation (AKA Sigma)

GLD_sigma <- sd(Portfolio.ret$GLD [time_index:(time_index-11)])
# [1] 0.04565563
IEF_sigma <- sd(Portfolio.ret$IEF [time_index:(time_index-11)])
# [1] 0.02958028
IXN_sigma <- sd(Portfolio.ret$IXN [time_index:(time_index-11)])
# [1] 0.07907487
QQQ_sigma <- sd(Portfolio.ret$QQQ [time_index:(time_index-11)])
# [1] 0.07061041
VNQ_sigma <- sd(Portfolio.ret$VNQ [time_index:(time_index-11)])
# [1] 0.06765254 

# Total Portfolio 
Total_sigma <- sd(Portfolio.ret$Total [time_index:(time_index-11)])
# [1] 0.04793094 

# Use Total with a benchmark portfolio 

Bench <- monthlyReturn(getSymbols("DJIA", auto.assign = FALSE))
Bench_Portfolio <- merge.xts(Portfolio_MonthlyROR_no.alloc, Bench)
colnames(Bench_Portfolio)[6] <- "DJIA"
time_index <- nrow(Bench_Portfolio)

DJIASigma <- sd(Bench_Portfolio$DJIA[time_index:(time_index-11)])

cor(Portfolio.ret[,c( "GLD", "IEF", "IXN", "QQQ", "VNQ")])


cor(Bench_Portfolio[,c( "GLD", "IEF", "IXN", "QQQ", "VNQ", "DJIA")])
cor_matrix3<- cor(Bench_Portfolio[time_index:(time_index-11),], )
corrplot(cor_matrix3, method = "circle", tl.col = "black",tl.srt=50)


##########
#########
#Tracking Error# 



GLD_TE <- sd(Portfolio.ret$GLD[time_index:(time_index-11)]-
               Bench_Portfolio$DJIA[time_index:(time_index-11)])

IEF_TE <- sd(Portfolio.ret$IEF[time_index:(time_index-11)]-
               Bench_Portfolio$DJIA[time_index:(time_index-11)])

IXN_TE <- sd(Portfolio.ret$IXN[time_index:(time_index-11)]-
               Bench_Portfolio$DJIA[time_index:(time_index-11)])

QQQ_TE <- sd(Portfolio.ret$QQQ[time_index:(time_index-11)]-
               Bench_Portfolio$DJIA[time_index:(time_index-11)])

VNQ_TE <- sd(Portfolio.ret$VNQ[time_index:(time_index-11)]-
               Bench_Portfolio$DJIA[time_index:(time_index-11)])




# For all time 

######################################################################
## Question 4: Based on the previous 3 questions, which holdings would you sell, which holdings would you buy? 
######################################################################

# sharpe rate: its negative it moves slower thank market, sell

riskfree <- 0.0015

# we will calculate the expected return (of the last 12 months) with the mean() function 

GLD_SHARPE <- (mean(Portfolio.ret$GLD[time_index:(time_index-11)])-riskfree)/GLD_sigma
# 0.1769205
IEF_SHARPE <- (mean(Portfolio.ret$IEF[time_index:(time_index-11)])-riskfree)/IEF_sigma
# -0.3067268
IXN_SHARPE <- (mean(Portfolio.ret$IXN[time_index:(time_index-11)])-riskfree)/IXN_sigma
# 0.2381766
VNQ_SHARPE <- (mean(Portfolio.ret$VNQ[time_index:(time_index-11)])-riskfree)/VNQ_sigma
# -0.1833891
QQQ_SHARPE <- (mean(Portfolio.ret$QQQ[time_index:(time_index-11)])-riskfree)/QQQ_sigma
# 0.2442542

############ efficient Frontier for 10 stocks
library(quantmod)
library(tseries)
library(stats)
library(quadprog)

enddate <- "2022-2-28"
t<-1389 #The first time you run this, you'll see the error so adjust the t with the requested number

myvector <- c()
nstocks <- 5
pricinglist <- as.data.frame(matrix(ncol=nstocks, nrow=t))
colnames(pricinglist) <- c("VNQ","QQQ", "GLD", "IEF", "IXN")
#the pricinglist data starts form 2016-8-22 - this is the first row

for (i in 1:(ncol(pricinglist))){
  current_ticker <- colnames(pricinglist)[i]
  newtable <- getSymbols(current_ticker, src = "yahoo", from="2016-8-22", to=enddate, auto.assign=FALSE)
  pricinglist[,i] <- newtable[,6]
}

#forecasting the next price using a backpropagation training algorithm in a neural network. 
# a Autoregressive Model of fourth order AR4 was used.


newpricingdataset <- pricinglist

#creating a dataset with monthly ROR for each day using continuous compounding
dailyROR <- as.data.frame(matrix(ncol=ncol(newpricingdataset), nrow=nrow(newpricingdataset)-25))
colnames(dailyROR) <- colnames(pricinglist)
for (c in 1:(ncol(newpricingdataset))){
  for (r in 1:(nrow(newpricingdataset)-25)){
    dailyROR[r,c] <- log(as.numeric(newpricingdataset[(r+25),c])/as.numeric(newpricingdataset[r,c]))
  }
}
#The most current expected return for n+25 (n is today) is in the last row of the above dataset

#calculating Expected(R) for all securities 
averet <- as.matrix(dailyROR[nrow(dailyROR),], nrow=1)
#calculating covariance matrix
rcov <- cov(dailyROR[(nrow(dailyROR)-125):(nrow(dailyROR)),]) #125 stands for 6 trading months
target.r <- 1/1000
#using solver to get to optimal weights


effFrontier = function(averet, rcov, nports, shorts, wmax, wmin)
{
  mxret <- max(averet)
  mnret <- -mxret
  n.assets <- ncol(averet)
  reshigh <- rep(wmax, n.assets)
  reslow <- rep(wmin, n.assets)
  min.rets <- seq(mnret, mxret, length.out=nports)
  vol <- rep(NA, nports)
  ret <- rep(NA, nports)
  pw <- data.frame(matrix(ncol=nports, nrow=n.assets))
  for (i in 1:nports)
  {
    port.sol <- NULL
    try(port.sol <- portfolio.optim(x=averet, pm=min.rets[i], covmat=rcov,   reshigh = reshigh, reslow= reslow, shorts=F)
        , silent=T)
    if(!is.null(port.sol))
    {
      vol[i] <- sqrt(as.vector(port.sol$pw %*% rcov %*% port.sol$pw))
      ret[i] <- averet %*% port.sol$pw
      pw[,i] <- port.sol$pw
    }
  }
  return(list(vol=vol, ret = ret, weights = pw))
  
}

maxSharpe <- function(averet, rcov, shorts=F, wmax=0.2, min.weight=0.01)
{
  optim.callback=function(param, averet, rcov, reshigh, reslow, shorts)
  { 
    port.sol = NULL
    try(port.sol <- portfolio.optim(x=averet, pm=param, covmat=rcov,
                                    reshigh=reshigh, reslow=reslow, shorts=shorts),silent=T)
    if(is.null(port.sol)) { ratio= 10^9} else 
    {
      m.return <- averet %*% port.sol$pw
      m.risk <- sqrt(as.vector(port.sol$pw %*% rcov %*% port.sol$pw))
      ratio <- m.return/m.risk
      assign("w", port.sol$pw, inherits=T)
    }
    return(ratio)
  }
  
  ef <- effFrontier(averet=averet, rcov=rcov, shorts=shorts, wmax=wmax, nports = 100, wmin=min.weight)
  n <- ncol(averet)
  reshigh <- rep(wmax, n)
  reslow <- rep(min.weight, n)
  
  max.sh <- which.max(ef$ret/ef$vol)
  
  
  if(is.na(ef$ret[max.sh-1])){lowerinterval<-ef$ret[max.sh]}else{lowerinterval <- ef$ret[max.sh-1]}
  if(is.na(ef$ret[max.sh+1])){upperinterval<-ef$ret[max.sh]}else{upperinterval <- ef$ret[max.sh+1]}
  
  w <- rep(0, ncol(averet))
  xmin <- optimize(f=optim.callback, interval = c(lowerinterval, upper=upperinterval), 
                   averet=averet, rcov=rcov, reshigh=reshigh, reslow=reslow, shorts=shorts)
  return(w)
  return(xmin)
}

z <- maxSharpe(averet, rcov, shorts=F, wmax=0.4)
print(z)

## Output: > print(z)
# "VNQ",        "QQQ",      "GLD",    "IEF",    "IXN"
# 0.15103069 0.03896931 0.40000000 0.40000000 0.01000000
######################################################################
## Question 5: How will your portfolio risk and expected returns change after rebalancing (selling and buying)?
######################################################################

### Reallocation 
IXN_alloc2 <- 0.01000000
QQQ_alloc2 <- 0.03896931
IEF_alloc2 <- 0.40000000
VNQ_alloc2 <- 0.15103069
GLD_alloc2 <- 0.40000000


Portfolio.ret.newalloc <- as.data.frame(Portfolio_MonthlyROR_no.alloc)%>%
  mutate(GLD_alloc2*GLD+ IXN_alloc2*IXN+ QQQ_alloc2* QQQ + IEF_alloc2*IEF + VNQ_alloc2*VNQ )

colnames(Portfolio.ret.newalloc)[0] <- "Date"
colnames(Portfolio.ret.newalloc)[6] <- "Total"


######################################################################
## Question 6: Can you build an efficient frontier for this portfolio (select 3 assets with similar high sharpe)? What can you say based on the efficient frontier?
######################################################################

### Balance/ efficient frontier  for all 10

############ efficient Frontier for 10 stocks
library(quantmod)
library(tseries)
library(stats)
library(quadprog)

enddate <- "2023-7-25"
t<-504 #The first time you run this, you'll see the error so adjust the t with the requested number

myvector <- c()
nstocks <- 5
pricinglist <- as.data.frame(matrix(ncol=nstocks, nrow=t))
colnames(pricinglist) <- c("QQQ", "GLD","IXN","SRET", "FXNAX")
#the pricinglist data starts form 2016-8-22 - this is the first row

for (i in 1:(ncol(pricinglist))){
  current_ticker <- colnames(pricinglist)[i]
  newtable <- getSymbols(current_ticker, src = "yahoo", from="2021-7-22", to=enddate, auto.assign=FALSE)
  pricinglist[,i] <- newtable[,6]
}

#forecasting the next price using a backpropagation training algorithm in a neural network. 
# a Autoregressive Model of fourth order AR4 was used.


newpricingdataset <- pricinglist

#creating a dataset with monthly ROR for each day using continuous compounding
dailyROR <- as.data.frame(matrix(ncol=ncol(newpricingdataset), nrow=nrow(newpricingdataset)-25))
colnames(dailyROR) <- colnames(pricinglist)
for (c in 1:(ncol(newpricingdataset))){
  for (r in 1:(nrow(newpricingdataset)-25)){
    dailyROR[r,c] <- log(as.numeric(newpricingdataset[(r+25),c])/as.numeric(newpricingdataset[r,c]))
  }
}
#The most current expected return for n+25 (n is today) is in the last row of the above dataset

#calculating Expected(R) for all securities 
averet <- as.matrix(dailyROR[nrow(dailyROR),], nrow=1)
#calculating covariance matrix
rcov <- cov(dailyROR[(nrow(dailyROR)-125):(nrow(dailyROR)),]) #125 stands for 6 trading months
target.r <- 1/1000
#using solver to get to optimal weights


effFrontier = function(averet, rcov, nports, shorts, wmax, wmin)
{
  mxret <- max(averet)
  mnret <- -mxret
  n.assets <- ncol(averet)
  reshigh <- rep(wmax, n.assets)
  reslow <- rep(wmin, n.assets)
  min.rets <- seq(mnret, mxret, length.out=nports)
  vol <- rep(NA, nports)
  ret <- rep(NA, nports)
  pw <- data.frame(matrix(ncol=nports, nrow=n.assets))
  for (i in 1:nports)
  {
    port.sol <- NULL
    try(port.sol <- portfolio.optim(x=averet, pm=min.rets[i], covmat=rcov,   reshigh = reshigh, reslow= reslow, shorts=F)
        , silent=T)
    if(!is.null(port.sol))
    {
      vol[i] <- sqrt(as.vector(port.sol$pw %*% rcov %*% port.sol$pw))
      ret[i] <- averet %*% port.sol$pw
      pw[,i] <- port.sol$pw
    }
  }
  return(list(vol=vol, ret = ret, weights = pw))
  
}

maxSharpe <- function(averet, rcov, shorts=F, wmax=0.2, min.weight=0.01)
{
  optim.callback=function(param, averet, rcov, reshigh, reslow, shorts)
  { 
    port.sol = NULL
    try(port.sol <- portfolio.optim(x=averet, pm=param, covmat=rcov,
                                    reshigh=reshigh, reslow=reslow, shorts=shorts),silent=T)
    if(is.null(port.sol)) { ratio= 10^9} else 
    {
      m.return <- averet %*% port.sol$pw
      m.risk <- sqrt(as.vector(port.sol$pw %*% rcov %*% port.sol$pw))
      ratio <- m.return/m.risk
      assign("w", port.sol$pw, inherits=T)
    }
    return(ratio)
  }
  
  ef <- effFrontier(averet=averet, rcov=rcov, shorts=shorts, wmax=wmax, nports = 100, wmin=min.weight)
  n <- ncol(averet)
  reshigh <- rep(wmax, n)
  reslow <- rep(min.weight, n)
  
  max.sh <- which.max(ef$ret/ef$vol)
  
  
  if(is.na(ef$ret[max.sh-1])){lowerinterval<-ef$ret[max.sh]}else{lowerinterval <- ef$ret[max.sh-1]}
  if(is.na(ef$ret[max.sh+1])){upperinterval<-ef$ret[max.sh]}else{upperinterval <- ef$ret[max.sh+1]}
  
  w <- rep(0, ncol(averet))
  xmin <- optimize(f=optim.callback, interval = c(lowerinterval, upper=upperinterval), 
                   averet=averet, rcov=rcov, reshigh=reshigh, reslow=reslow, shorts=shorts)
  return(w)
  return(xmin)
}

z <- maxSharpe(averet, rcov, shorts=F, wmax=0.3)
print(z)


### check 6 and 7 on the code 

#################################
### Extra: CAPM
time_index2 <- nrow(Bench_Portfolio) #this is how many monthly observations we have in our data frame
last_12_months <- Bench_Portfolio[(time_index2-11) : time_index2, ]
##we will use the RUS1000 as the benchmark to regress against
GLD_reg <- lm(GLD ~ DJIA ,data=last_12_months)  #we run a linear regression with the bencharmk returns (RUS1000) as X and asset returns as Y
summary(GLD_reg)

IXN_reg <- lm(IXN ~ DJIA ,data=last_12_months)  #we run a linear regression with the bencharmk returns as X and asset returns as Y
summary(IXN_reg)

QQQ_reg <- lm(QQQ ~ DJIA ,data=last_12_months)  #we run a linear regression with the bencharmk returns as X and asset returns as Y
summary(QQQ_reg)

VNQ_reg <- lm(VNQ ~ DJIA ,data=last_12_months)  #we run a linear regression with the bencharmk returns as X and asset returns as Y
summary(VNQ_reg)

IEF_reg <- lm(IEF ~ DJIA ,data=last_12_months)  #we run a linear regression with the bencharmk returns as X and asset returns as Y
summary(IXN_reg)



#How do our residuals look like in this model? are these models good?
#we want to see residuals(standardized) that are linear 
plot(GLD_reg, which=2, col=c("red"))
plot(IEF_reg, which=2, col=c("blue"))
plot(QQQ_reg, which=2, col=c("green4"))
plot(IXN_reg, which=2, col=c("brown"))
plot(VNQ_reg, which=2, col=c("purple"))

#####################################################################
## ARMA/ARIMA 
library(tidyverse)
library(tseries)
library(rugarch)

adf.test(Portfolio.ret$GLD)
#p value below 0.05 = Data stationary 
adf.test(Portfolio.ret$IEF)
#p value below 0.05 = Data stationary 
adf.test(Portfolio.ret$IXN)
#p value below 0.05 = Data stationary
adf.test(Portfolio.ret$VNQ)
#p value below 0.05 = Data stationary
adf.test(Portfolio.ret$QQQ)
#p value below 0.05 = Data stationary

# ACF and PACF for GLD  
acf(Portfolio.ret$GLD)
pacf(Portfolio.ret$GLD)
GLD_TS <- ts(Portfolio.ret$GLD, frequency = 5)
GLD_Dec <- decompose(GLD_TS)
plot(GLD_Dec)
GLD_arima <- arima(Portfolio.ret$GLD, 
                       order=c(11,0,11)) 
predict(GLD_arima, n.ahead =18) 

# for IXN
acf(Portfolio.ret$IXN)
pacf(Portfolio.ret$IXN)
IXN_TS <- ts(Portfolio.ret$IXN, frequency = 5)
IXN_dec <- decompose(IXN_TS)
plot(IXN_dec)
IXN_arima <- arima(Portfolio.ret$IXN, 
                   order=c(5,0,6)) 
predict(IXN_arima, n.ahead =18) 

# For IEF
acf(Portfolio.ret$IEF)
pacf(Portfolio.ret$IEF)
IEF_TS <- ts(Portfolio.ret$IEF, frequency = 5)
IEF_dec <- decompose(IXN_TS)
plot(IEF_dec)
IEF_arima <- arima(Portfolio.ret$IEF, 
                   order=c(5,0,6)) 
predict(IEF_arima, n.ahead =18) 

# for QQQ
acf(Portfolio.ret$QQQ)
pacf(Portfolio.ret$QQQ)
QQQ_TS <- ts(Portfolio.ret$QQQ, frequency = 5)
QQQ_dec <- decompose(QQQ_TS)
plot(QQQ_dec)
QQQ_arima <- arima(Portfolio.ret$QQQ, 
                   order=c(6,0,6)) 
predict(QQQ_arima, n.ahead =18) 

# For VNQ
acf(Portfolio.ret$VNQ)
pacf(Portfolio.ret$VNQ)
VNQ_TS <- ts(Portfolio.ret$VNQ, frequency = 5)
VNQ_dec <- decompose(VNQ_TS)
plot(VNQ_dec)
VNQ_arima <- arima(Portfolio.ret$VNQ, 
                   order=c(2,0,2)) 
predict(VNQ_arima, n.ahead =18) 


######

# NEW PORTFOLIO

GLD_alloc2 <- 0.1769
QQQ_alloc2 <- 0.2443
IXN_alloc2 <- 0.2382
SRET_alloc2 <- 0.13441
FXNAX_alloc2 <- 0.20589


GLD_returns2 <- monthlyReturn(getSymbols("GLD", auto.assign=FALSE))
SRET_returns2 <- monthlyReturn(getSymbols("SRET", auto.assign=FALSE))
IXN_returns2 <- monthlyReturn(getSymbols("IXN", auto.assign=FALSE))
QQQ_returns2<- monthlyReturn(getSymbols("QQQ", auto.assign=FALSE))
FXNAX_returns2 <- monthlyReturn(getSymbols("FXNAX", auto.assign=FALSE))

Portfolio2 <- merge.xts(GLD_returns2, SRET_returns2, IXN_returns2,QQQ_returns2,FXNAX_returns2)


colnames(Portfolio2)[1] <- "GLD"
colnames(Portfolio2)[2] <- "SRET"
colnames(Portfolio2)[3] <- "IXN"
colnames(Portfolio2)[4] <- "QQQ"
colnames(Portfolio2)[5] <- "FXNAX"


Portfolio2allo <- as.data.frame(Portfolio2)%>%
  mutate(GLD_alloc2*GLD+ IXN_alloc2*IXN+ QQQ_alloc2* QQQ + SRET_alloc2*SRET + FXNAX_alloc2*FXNAX )

colnames(Portfolio2allo)[6] <- "Total"


SRET_sigma2 <- sd(Portfolio2allo$SRET[time_index:(time_index-11)])
# [1] 
FXNAX_sigma2 <- sd(Portfolio2allo$FXNAX[time_index:(time_index-11)])


# Total Portfolio 
Total_sigma2 <- sd(Portfolio2allo$Total [time_index:(time_index-11)])
# [1] 0.04793094 

GLD_SHARPE2 <- (mean(Portfolio2allo$GLD[time_index:(time_index-11)])-riskfree)/GLD_sigma
# 0.1769205
SRET_SHARPE2 <- (mean(Portfolio2$SRET[time_index:(time_index-11)])-riskfree)/SRET_sigma2

FXNAX_SHARPE2 <- (mean(Portfolio2allo$FXNAX[time_index:(time_index-11)])-riskfree)/FXNAX_sigma2



### expected return 
## SRET
acf(Portfolio2allo2$SRET)
pacf(Portfolio2allo2$SRET)
SRET_TS <- ts(Portfolio2allo2$SRET, frequency = 5)
SRET_dec <- decompose(SRET_TS)
plot(SRET_dec)
SRET_arima <- arima(Portfolio2allo2$SRET, 
                   order=c(9,0,8)) 
predict(SRET_arima, n.ahead =18) 

acf(Portfolio2allo$GLD)
pacf(Portfolio2allo$GLD)
GLD_TS2 <- ts(Portfolio2allo$GLD, frequency = 5)
GLD_Dec2 <- decompose(GLD_TS2)
plot(GLD_Dec2)
GLD_arima2 <- arima(Portfolio2allo$GLD, 
                   order=c(11,0,11)) 
predict(GLD_arima2, n.ahead =18) 

acf(Portfolio2allo2$FXNAX)
pacf(Portfolio2allo2$FXNAX)
FXNAX_TS2 <- ts(Portfolio2allo$FXNAX, frequency = 5)
FXNAX_Dec2 <- decompose(FXNAX_TS2)
plot(FXNAX_Dec2)
FXNAX_arima2 <- arima(Portfolio2allo$FXNAX, 
                    order=c(5,0,5)) 
predict(FXNAX_arima2, n.ahead =18) 


acf(Portfolio2allo$IXN)
pacf(Portfolio2allo$IXN)
IXN_TS2 <- ts(Portfolio2allo$IXN, frequency = 5)
IXN_dec2 <- decompose(IXN_TS2)
plot(IXN_dec2)
IXN_arima2 <- arima(Portfolio2allo$IXN, 
                   order=c(6,0,6)) 
predict(IXN_arima2, n.ahead =18) 

acf(Portfolio2allo$QQQ)
pacf(Portfolio2allo$QQQ)
QQQ_TS2 <- ts(Portfolio2allo$QQQ, frequency = 5)
QQQ_dec2 <- decompose(QQQ_TS2)
plot(QQQ_dec2)
QQQ_arima <- arima(Portfolio.ret$QQQ, 
                   order=c(6,0,6)) 
predict(QQQ_arima, n.ahead =18) 



### effficient frontier 

library(data.table)
library(scales)
library(ggplot2)
library(quantmod)
library(reshape2)
ticker1 <- "IXN"
ticker2<- "QQQ"
ticker3<- "GLD"
mydf1 <- as.data.frame(getSymbols(ticker1, auto.assign=FALSE))
mydf2 <- as.data.frame(getSymbols(ticker2, auto.assign=FALSE))
mydf3 <- as.data.frame(getSymbols(ticker3, auto.assign=FALSE))

combined_df <- cbind(mydf1[,4], mydf2[,4], mydf3[,4])

dt <- as.data.frame(combined_df)
colnames(dt) <- c(ticker1, ticker2, ticker3)
dt$date = as.Date(rownames(mydf1))
dt <- melt(dt, id="date")
colnames(dt) <- c("date", "ticker","price")
dt <- data.table(dt)
# create indexed values
dt[, idx_price := price/price[1], by = ticker]
# plot the indexed values
ggplot(dt, aes(x = date, y = idx_price, color = ticker)) +
  geom_line() +
  # Miscellaneous Formatting
  theme_bw() + ggtitle("Price Developments") +
  xlab("Date") + ylab("Pricen(Indexed 2000 = 1)") +
  scale_color_discrete(name = "Company")

# calculate the arithmetic returns
dt[, ret := price / shift(price, 1) - 1, by = ticker]

# summary table
# take only non-na values
tab <- dt[!is.na(ret), .(ticker, ret)]

# calculate the expected returns (historical mean of returns) and volatility (standard deviation of returns)
tab <- tab[, .(er = round(mean(ret), 4),
               sd = round(sd(ret), 4)),
           by = "ticker"]

ggplot(tab, aes(x = sd, y = er, color = ticker)) +
  geom_point(size = 5) +
  # Miscellaneous Formatting
  theme_bw() + ggtitle("Risk-Return Tradeoff") +
  xlab("Volatility") + ylab("Expected Returns") +
  scale_y_continuous(label = percent, limits = c(0, 0.03)) +
  scale_x_continuous(label = percent, limits = c(0, 0.1))

