#customer churn analysis
library(dplyr)
library(tidyr)
library(ggplot2)
rawdata <- read.csv("/Users/rendongmin/Desktop/IMC/tel_churn/customerchurn.csv")
#Contract Month-to-month, One year, Two year
rawdata <- rawdata %>% filter(tenure <=70) %>% filter(tenure > 0)
datam <- rawdata %>% filter(Contract == 'Month-to-month')
data1y <- rawdata %>% filter(Contract == 'One year')
data2y <- rawdata %>% filter(Contract == 'Two year')

kpstats <- function(data){
  mdata2 <- data %>% select(c('tenure','MonthlyCharges','Churn'))
  mdata2$ChurnB <- 0
  mdata2$ChurnB[mdata2$Churn=='Yes'] <- 1
  mdata2$CensorB <- 0
  mdata2$CensorB[mdata2$Churn=='No'] <- 1
  surv <- mdata2 %>% group_by(tenure) %>% 
    summarize(nchurn = sum(ChurnB),ncensor = sum(CensorB))
  surv$nrisk <- 0
  surv$nrisk[1] <- dim(data)[1]
  for (i in 2:dim(surv)[1]) {
    surv$nrisk[i] <- surv$nrisk[i-1] - surv$nchurn[i-1] - surv$ncensor[i-1]
    }
  surv$retension <- 1-surv$nchurn/surv$nrisk
  surv$survival <- 1
  for (i in 2:dim(surv)[1]) {
    surv$survival[i] <- surv$survival[i-1]*surv$retension[i]
    }
  surv$hazard <- surv$nchurn/surv$nrisk
  surv$cprob <- surv$nchurn/surv$nrisk[1]
  return(surv)
}

#Survival
statsm <- kpstats(datam)
stats1y <- kpstats(data1y)
stats2y <- kpstats(data2y)
survplot <- data.frame(statsm$survival,stats1y$survival,stats2y$survival, statsm$tenure)
colnames(survplot) <- c('Month','1year','2year','tenure')
survplot <- survplot %>% gather(key='Contract',value='Survival',-tenure)
ggplot(survplot,
       aes(x=tenure,y=Survival,colour=Contract))+
       geom_line(lwd=0.8) + labs(x='Tenure',y='Survival Rate') +
       ggtitle('Survival Curve across Different Contracts')
# Hazard
survplot <- data.frame(statsm$hazard,stats1y$hazard,stats2y$hazard, statsm$tenure)
colnames(survplot) <- c('Month','1year','2year','tenure')
survplot <- survplot %>% gather(key='Contract',value='Hazard',-tenure)
ggplot(survplot,
       aes(x=tenure,y=Hazard,colour=Contract))+
  geom_line(lwd=0.8) + labs(x='Tenure',y='Hazard Rate') +
  ggtitle('Hazard Rate across Different Contracts')


#CLV Calculation
#Customer Value Distribution
ggplot(rawdata,aes(x=TotalCharges,fill=Contract))+geom_density(alpha=0.5)
ggplot(rawdata,aes(x=TotalCharges,fill=Contract))+geom_density(alpha=0.5)
rawdata %>% group_by(Contract) %>% summarize(mean(MonthlyCharges))
rawdata %>% group_by(Contract) %>% summarize(mean(TotalCharges))
#Customer Lifetime Value

#write a function to calculate CLV
CLV <- function(payment,tenure,statsdata) {
  paylist <- c()
  dpay2 = 0
  for (i in 1:70){
    dpay = payment/(1+0.01)^(i-1)
    dpay2 = dpay2 + dpay
    paylist <- c(paylist,dpay2)
  }
  for (i in 71:150){
    dpay = payment/(1+0.01)^(i-1)
    dpay2 = dpay2 + dpay
    paylist <- c(paylist,dpay2)
  }
  retend <- c(statsdata$retension,rep(0.8,80))
  cumret <- c(1)
  probs <- c(1-retend[1])
  for (i in 2:150){
    cum <- cumret[i-1]*retend[i-1]
    cumret <- c(cumret,cum)
    prob <- (1-retend[i])*cumret[i]
    probs <- c(probs,prob)
  }
  probs <- probs/cumret[1]
  return(crossprod(probs[1:150],paylist)[1])
}

datam$clv=0
head(datam)
for (i in 1:dim(datam)[1]){
  datam[i,]$clv <- CLV(datam[i,]$MonthlyCharges,datam[i,]$tenure,statsm)
}
data1y$clv=0
for (i in 1:dim(data1y)[1]){
  data1y[i,]$clv <- CLV(data1y[i,]$MonthlyCharges,data1y[i,]$tenure,stats1y)
}
data2y$clv=0
for (i in 1:dim(data2y)[1]){
  data2y[i,]$clv <- CLV(data2y[i,]$MonthlyCharges,data2y[i,]$tenure,stats2y)
}

#Ditribution
data2 <- rbind(datam,data1y,data2y)
data2$total_value <- data2$TotalCharges + data2$clv
clvplot <- data2 %>% select(Contract,clv,total_value,TotalCharges,MonthlyCharges)
ggplot(clvplot,aes(x=total_value,fill=Contract))+geom_histogram(alpha=1)
ggplot(clvplot,aes(x=clv,fill=Contract))+
  geom_density(alpha=0.8)+facet_wrap(~Contract)+
  ggtitle("CLV Density Distribution by Contract")+
  labs(x='Customer Lifetime Value')
ggplot(data2, aes(x=Contract,y=MonthlyCharges,colours=Contract))+geom_violin()

summary <- clvplot %>% group_by(Contract) %>% summarise(mean(total_value),mean(TotalCharges),mean(clv), mean(MonthlyCharges))
summary <- as.data.frame(summary)
colnames(summary) <- c('Contract','Total Value','Total Chrages','CLV','MonthCharges')
summary$intCLV <- as.integer(summary$CLV)

ggplot(summary,aes(x=Contract, y= CLV, label=intCLV)) + geom_col()+
  ggtitle('Average CLV of Different Contract') +
  geom_text(size = 3, position = position_stack(vjust = 0.5),col='white')
