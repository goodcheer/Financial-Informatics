setwd("C:/Users/kyuchul/Documents/Carrer/학교_2/2017-2/금융인포메틱스/Final Project")
packages <- c("quantmod", "PerformanceAnalytics", "TTR", "corrplot", "rpart", "rpart.plot")

for(i in packages){
  library(i,character.only = T)
}

tickers <- read.csv("SP500Constituent.csv")$Ticker.symbol
tickers <- as.character(tickers)



all.dat <- sapply(tickers, function(x) {
  tryCatch(getSymbols(x,auto.assign = FALSE,from="2013-01-01", to="2016-12-31"), 
           error=function(e) conditionMessage(e))} )

#delete failed ones 
flag <- unlist(lapply(all.dat, function(x) class(x)[1])) == "character"
all.dat[flag] <- NULL

#save(all.dat, file = "sp500all.RData")
# load(file = "sp500all.RData")


all.vol <- lapply(all.dat, function(x) mean(x[,5], na.rm = T))

all.vol.mean <- t(as.data.frame(all.vol))
ascend.vol <- order(all.vol.mean)

# Sample 50 each from different Range of Total Volumes
target.all <- c(all.dat[ascend.vol[40:90]],
                    all.dat[ascend.vol[140:190]],
                    all.dat[ascend.vol[240:290]],
                    all.dat[ascend.vol[340:390]],
                    all.dat[ascend.vol[440:490]])



# sharpe ratio
mycolnames <- c("Open", "High", "Low", "Close", "Volume", "Adjusted")
target.all <- lapply(target.all, setNames, mycolnames)

all.dailyReturn <- lapply(target.all, function(x) round(dailyReturn(x$Adjusted, type="arithmetic"),2))

all.dailyReturn <- do.call("merge",all.dailyReturn)
all.dailyReturn <- na.omit(all.dailyReturn)

# recent 1 year Sharpe Ratios
all.sharpes <- apply(all.dailyReturn, 2, 
                     function(x) SharpeRatio.annualized(x[length(x)-251:length(x)],Rf = 0))

# Top 50  absolute sharpe securities
toUse <- rank(abs(all.sharpes)) > 205
targets <- target.all[toUse]

# save(targets, file="targets_50.RData")


## ---------------------- ##
## Generate Indicators.   ##
## ---------------------- ##

makeIndics <- function(target){
  
  closeprice <- Cl(target)
  
  #Daily Return
  daily <- round(dailyReturn(target$Close, type='arithmetic'),3)
  daily <- as.vector(daily)
  
  #SMA & LMA
  ShMA <- 20; LMA <- 50;
  
  sma <- round(SMA(closeprice, ShMA),1)
  sma <- c(NA,head(sma,-1))
  
  lma <- round(SMA(closeprice, LMA),1)
  lma <- c(NA,head(lma,-1))
  
  #EMA : Exponential moving average.
  ema <- round(EMA(closeprice, 20), 1)
  ema <- c(NA,head(ema,-1))
  
  #DEMA : Double-exponential moving average
  dema <- round(DEMA(closeprice, 20),1)
  dema <- c(NA,head(dema,-1))
  
  #EVWMA : Elastic, volume-weighted moving average.
  evwma <- round(EVWMA(closeprice, target$Volume, 20),1)
  evwma <- c(NA,head(evwma,-1))
  
  #ZLEMA : Zero lag exponential moving average.
  zlema <- round(ZLEMA(closeprice, 20),1)
  zlema <- c(NA,head(zlema,-1))
  
  #ALMA : Arnaud Legoux moving average.
  alma <- ALMA(closeprice)
  alma <- c(NA,head(alma,-1))
  
  #HMA : Hull moving average.
  hma <- HMA(closeprice)
  hma <- c(NA,head(hma,-1))
  
  
  #RSI = AU / (AU + AD)
  rsi <- round(RSI(closeprice,n=14,maType="WMA"),1)
  rsi <- c(NA, head(rsi,-1))
  
  #CMO : The Chande Momentum Oscillator (CMO) is a modified RSI
  cmo <- round(CMO(closeprice, n=14),1)
  cmo <- c(NA, head(cmo,-1))
  
  #ADX
  temp <- ADX(target[,c("High","Low","Close")])
  temp <- as.data.frame(temp)
  adx <- round(temp$ADX,1)
  adx <- c(NA,head(adx,-1))
  
  #CCI : The Commodity Channel Index (CCI) attempts to identify starting and ending trends.
  temp <- CCI(target[,c("High","Low","Close")])
  temp <- as.data.frame(temp)
  cci <- round(temp$cci,1)
  cci <- c(NA,head(cci,-1))
  
  volume <- as.vector(target$Volume)
  
  # OBV : On Balance Volume
  obv <- OBV(closeprice, volume)
  obv <- c(NA, head(obv,-1))
  
  #chaikinAD : The Chaikin Accumulation / Distribution (AD)
  temp <- chaikinAD(target[,c("High","Low","Close")], volume)
  temp <- as.data.frame(temp)
  cAD <- round(temp$temp,1)
  cAD <- c(NA, head(cAD,-1))
  
  indics <- data.frame(rsi, sma, lma, adx, alma, cci, cmo, dema,
                       ema, evwma, hma,zlema, obv, cAD)
  
  rownames(indics) <- index(target)
  
  # check Nas in "indics"
  na.indx <- unname(apply(is.na(indics),1, sum) > 0) # rows with more than one NA
  
  # if NAs are consecutive
  naIdif <- diff(which(na.indx)) 
  start <- which(na.indx)[length(which(na.indx))] + 1
  
  # since only first 50 observations have NAs, remove them
  # to exclude periods without certain indicators
  try(if(sum(naIdif != 1) != 0 ){
    stop("NAs are not consecutive")}
    else{indics <- na.omit(indics)})
  
  dat <- cbind(indics, daily[start:length(daily)])
  names(dat)[15] <- "daily"
  
  return(dat)
}


## --------------------------- ##
## Stratified random Partition ##
## --------------------------- ##

STRPart <- function(df, factorCol, p){
  
  # get levels of factor column
  df.lvl <- levels( df[,factorCol])
  
  # seperate by levels
  df.classes <- list()
  
  for(i in 1:length(df.lvl)){
    df.classes[[i]] <- df[df[,factorCol]==df.lvl[i],]
  }
  
  # shuffling
  df.classes <- lapply(df.classes, function(x) x[sample(nrow(x)),])
  
  # Stratified Partition
  df.train <- lapply(df.classes,FUN = function(x) x[1:round(nrow(x)*p),])
  df.train <- do.call("rbind",df.train)
  df.test <- lapply(df.classes, FUN = function(x) x[(round(nrow(x)*p)+1):nrow(x),])
  df.test <- do.call("rbind",df.test)
  
  return(list(train = df.train, test = df.test))
}


doModel <- function(target, pRat){
  
  dat <- makeIndics(target)
  indics <- dat[,-15]
  daily <- dat[,15]
  
  # Remove no return change (zero)
  dat <- subset(dat, daily != 0)
  
  dat$daily <- ifelse(dat$daily > 0, "Up", "Down")
  dat$daily <- as.factor(x = dat$daily)
  names(dat)[15] <- "Class"
  
  
  #write.csv(dat, file="decision tree charting.csv")
  
  ## ------------------------- ##
  ## Choose 3 indicators Set   ##
  ## ------------------------- ##
  
  corMat <- cor(indics)
  corrplot.mixed(corMat)
  
  # True if abs(correlation) over 0.2
  corred <- abs(corMat) > 0.2
  
  # All combinations of 3 variables
  indicNames <- rownames(corMat)
  varSet <- t(combn(indicNames,m = 3))
  colnames(varSet) <- c("v1","v2","v3")
  
  # Correlated item list
  related <- function(corrMatrix){
    r <- list()
    vars <- rownames(corrMatrix)
    for(i in 1:length(vars)){
      r[[paste0(vars[i])]] <- vars[corrMatrix[i,]]
    }
    return(r)
  }
  
  mylist <- related(corred)
  
  
  # Find out sets with no correlated variables 
  f1 <- apply(varSet,1, function(x) !(x[2] %in% mylist[[x[1]]]))
  varSet2 <- varSet[f1,]
  
  f2 <- apply(varSet2,1, function(x) !(x[3] %in% mylist[[x[1]]]))
  varSet3 <- varSet2[f2,]
  
  f3 <- apply(varSet3,1, function(x) !(x[2] %in% mylist[[x[3]]]))
  varSet4 <- varSet3[f3,]
  varSet <- varSet4
  
  
  cat("Generate ", nrow(varSet)," Models\n")
  dt.set <- STRPart(dat, ncol(dat), pRat)
  
  ## --------------------------- ##
  ## Model Generation            ##
  ## --------------------------- ##
  
  dt.models <- list()
  
  # models 
  for(i in 1:nrow(varSet)){
    dt.models[[i]] <- rpart(Class~., data = dt.set$train[,c(varSet[i,],"Class")], method="class")
  }
  
  # predicts
  pred.test <- lapply(dt.models, function(x) predict(x, newdata = dt.set$test[,-15], type= "class"))

  # confusion matrix
  true.test <- dt.set$test[,15]
  cm.test <- lapply(pred.test, function(x) table(x, true.test))
  browser()
  # accuracy calcuation
  acc.test <- unlist(lapply(cm.test, function(x) sum(diag(x))/sum(x)))
  
  return(list(model = dt.models[[which.max(acc.test)]],
              data = dt.set,
              acc.test = max(acc.test)))
}


targets.model <- lapply(targets, function(x) {
  tryCatch(doModel(x, 0.7), error=function(e) conditionMessage(e) )
})


names(targets.model)

#delete failed ones 
flag <- unlist(lapply(targets.model, function(x) class(x)[1])) == "character"
targets.model[flag] <- NULL

# Use models with over (lambda=0.57) accuracy
modelUse <- unlist(lapply(targets.model, function(x) x$acc.test > 0.57 ))

finalModel <- targets.model[modelUse]
# save(finalModel, file="finalModels.RData")

# Draw Trees
lapply(finalModel, function(x) prp(x$model, type = 0, extra=6))

# ---------------------------#
# BackTesting                #
# ---------------------------#

assess <- names(finalModel)

# sec is ticker of securtiy (character)
backtest <- function(sec){
  t <- target.all[[sec]]
  t.model <- finalModel[[sec]]$model
  t.data <- makeIndics(t)
  names(t.model$variable.importance)
  
  t.pred <- predict(t.model, newdata = t.data[,-15], type="class")
  t.signal <- ifelse(t.pred == "Up", 1, -1)
  
  t.nullreturn <- data.frame(return = t.data$daily)
  t.nullreturn <- xts(x = t.nullreturn, order.by = as.POSIXct(rownames(t.data)))
  t.myreturn <- data.frame(return = t.data$daily * t.signal)
  t.myreturn <- xts(x = t.myreturn, order.by = as.POSIXct(rownames(t.data)))
  
  AR.port <- table.AnnualizedReturns(t.myreturn)
  AR.null <- table.AnnualizedReturns(t.nullreturn)
  Drdwn <- table.Drawdowns(t.myreturn)
  
  vR <- VaR(t.myreturn) ## worst scenario loss
  Rcum <- Return.cumulative(t.myreturn)
  
  chart.Summary.port <- function(){charts.PerformanceSummary(t.myreturn)}
  chart.Summary.null <- function(){charts.PerformanceSummary(t.nullreturn)}
  
  hitratio <- sum(t.myreturn > 0) / dim(t.myreturn)[1]
  
  return(list(AR.port = AR.port, AR.null = AR.null,
              table.Drawdown = Drdwn, VaR = vR, Return.cum = Rcum, hitRatio= hitratio,
              chart.Summary.port = chart.Summary.port, chart.Summary.null = chart.Summary.null))
}

reports <- lapply(assess, backtest)
names(reports) <- assess

#save(reports, file="reports.RData")

ARS.port <- do.call(cbind, lapply(reports, function(x) x$AR.port))
colnames(ARS.port) <- assess

ARS.null <- do.call(cbind, lapply(reports, function(x) x$AR.null))
colnames(ARS.null) <- assess

ARS.compr <- as.matrix(ARS.port) - as.matrix(ARS.null)

