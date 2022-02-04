setwd("D:/R . Data/hw2/Data")
library(dplyr)
library(data.table)
library(AER)
library(ggplot2)
library(mfx)
data1<-fread("datind2009.csv")

# Exercise1 (1) 
data2<-data1[complete.cases(data1[,10])] #Drop NA in wage
Y<-data2$wage
X<-data2$age
cor(Y,X)  #-0.1788512
# or cor = sum((X-mean(X))*(Y-mean(Y))) /(sqrt(sum((X-mean(X))^2))*sqrt(sum((Y-mean(Y))^2))) 
var(X)  
var(Y)

#(2)
X<- cbind(matrix(1,20232,1),X)  # include intercept
¦Â<- solve(t(X)%*%X)%*%t(X)%*%Y
¦Â
#¦Â=-180.1765 ,intercept=22075.1

#(3) OLS
resid<- Y-X%*%¦Â
sigma2<-as.numeric(t(resid) %*% resid) / (nrow(X) - ncol(X))
sqrt(sigma2)  #18622.31
se_¦Â <-diag(sqrt(sigma2 * solve(t(X) %*% X)))
se_¦Â   #357.8275  ; 6.9687
#bootstrap 49
reg = lm(wage ~ age,data = data2)
R    = 49;                      # number of bootstraps
nind = nrow(X);            # number of individuals
nvar = length(reg$coefficients)  # number of variables
outs = mat.or.vec(R,nvar)
set.seed(123)
for (i in 1:R)
{
  samp     = sample(1:nind,nind,rep=TRUE)
  dat_samp = data2[samp,]
  reg1     = lm(wage ~ age,data = dat_samp)
  outs[i,] = reg1$coefficients
}
mean_est = apply(outs,2,mean)
sd_est   = apply(outs,2,sd)
est = cbind(summary(reg)$coefficients[,1],
            summary(reg)$coefficients[,2],
            mean_est,
            sd_est)
colnames(est) = c("CF: est","CF: sd","BT: est","BT: sd")
est  #295.9£¬ 5.68

#bootstrap 499
R    = 499;                      # number of bootstraps
nind = nrow(X);            # number of individuals
nvar = length(reg$coefficients)  # number of variables
outs = mat.or.vec(R,nvar)
set.seed(123)
for (i in 1:R)
{
  samp     = sample(1:nind,nind,rep=TRUE)
  dat_samp = data2[samp,]
  reg1     = lm(wage ~ age,data = dat_samp)
  outs[i,] = reg1$coefficients
}
mean_est = apply(outs,2,mean)
sd_est   = apply(outs,2,sd)
est = cbind(summary(reg)$coefficients[,1],
            summary(reg)$coefficients[,2],
            mean_est,
            sd_est)
colnames(est) = c("CF: est","CF: sd","BT: est","BT: sd")
est  
#305.614962,  5.364861
#As replications increased, bootstrap results were closer to OLS.


# exercise2(1)
dati1<-fread("datind2005.csv",colClasses=c(idind="character",idmen="character"))
dati2<-fread("datind2006.csv",colClasses=c(idind="character",idmen="character"))
dati3<-fread("datind2007.csv",colClasses=c(idind="character",idmen="character"))
dati4<-fread("datind2008.csv", colClasses=c(idind="character",idmen="character"))
dati5<-fread("datind2009.csv", colClasses=c(idind="character",idmen="character"))
dati6<-fread("datind2010.csv", colClasses=c(idind="character",idmen="character"))
dati7<-fread("datind2011.csv", colClasses=c(idind="character",idmen="character"))
dati8<-fread("datind2012.csv", colClasses=c(idind="character",idmen="character"))
dati9<-fread("datind2013.csv", colClasses=c(idind="character",idmen="character"))
dati10<-fread("datind2014.csv", colClasses=c(idind="character",idmen="character"))
dati11<-fread("datind2015.csv", colClasses=c(idind="character",idmen="character"))
dati12<-fread("datind2016.csv", colClasses=c(idind="character",idmen="character"))
dati13<-fread("datind2017.csv", colClasses=c(idind="character",idmen="character"))
dati14<-fread("datind2018.csv", colClasses=c(idind="character",idmen="character"))
dat_total<-rbind(dati1,dati2,dati3,dati4,dati5,
                 dati6,dati7,dati8,dati9,dati10,dati11,dati12,
                 dati13,dati14)
dat_total<-dat_total[complete.cases(dat_total[,10])] #drop NA

dat_t=dat_total[,c(4,9,10)]

breaks <- c(0,18,25,30,35,40,45,50,55,60,110)
labels <- c("18-","18-25", "26-30", "31-35", "36-40", "41-45","46-50","51-55", "56-60", "60+");
dat_t[,"ag"] <- cut(dat_t$age, breaks = breaks, labels = labels)
#"ag" in dat_t

#(2)
plot <- ggplot(data=dat_t, aes(x=year, y=wage, color=ag))+geom_point(size=3)
plot

#The income of people aged between 18 and 40 tends to rise year by year.
# Other age groups seems to have no change

#(3)  first divide into (k-1)dummy variables.(k=14years)
dat_total$"2005" = as.numeric(dat_total$year==2005)
dat_total$"2006" = as.numeric(dat_total$year==2006)
dat_total$"2007" = as.numeric(dat_total$year==2007)
dat_total$"2008" = as.numeric(dat_total$year==2008)
dat_total$"2009" = as.numeric(dat_total$year==2009)
dat_total$"2010" = as.numeric(dat_total$year==2010)
dat_total$"2011" = as.numeric(dat_total$year==2011)
dat_total$"2012"= as.numeric(dat_total$year==2012)
dat_total$"2013" = as.numeric(dat_total$year==2013)
dat_total$"2014" = as.numeric(dat_total$year==2014)
dat_total$"2015" = as.numeric(dat_total$year==2015)
dat_total$"2016" = as.numeric(dat_total$year==2016)
dat_total$"2017" = as.numeric(dat_total$year==2017)
#Put these dummy variables into A
A = cbind(rep(1,length(dat_total$age)),dat_total$age) %>% 
  cbind(dat_total$"2005",dat_total$"2006",dat_total$"2007",dat_total$"2008",dat_total$"2009",dat_total$"2010",
        dat_total$"2011",dat_total$"2012",dat_total$"2013",dat_total$"2014",dat_total$"2015",
        dat_total$"2016",dat_total$"2017")
B = dat_total$wage
¦Â_a<- solve(t(A)%*%A)%*%t(A)%*%B  
¦Â_a


#exercise3(1)
data3<-fread("datind2007.csv")
data3<-data3[-which(data3$empstat=="Inactive")]
data3<-data3[-which(data3$empstat=="Retired")]
data3<-data3[complete.cases(data3[,10])]
data3

#(2)   
flike = function(par,x1,yvar)
{xbeta = par[1] + par[2]*x1 
  pr  = pnorm(xbeta)
 pr[pr>0.999999] = 0.999999
  pr[pr<0.000001] = 0.000001
  like           = yvar*log(pr) + (1-yvar)*log(1-pr)
  return(-sum(like))}

#(3)
set.seed(123)
x1 = data3$age
yvar = as.numeric(data3$empstat == "Employed")
ntry = 500       #ntry can be larger
out = mat.or.vec(ntry,3)
for (i in 1:ntry){
  start = runif(2,-5,5)
  res = optim(start,fn = flike,method = "BFGS",
      control = list(trace=6,maxit=1000),
      x1 = x1,
      yvar = yvar)
  out[i,c(1,2)] = res$par
  out[i,3] = res$value}
out = data.frame(out)
colnames(out) = c("theta", "bar_age", "likelihood")
out[which(out$likelihood == min(out$likelihood)),]
#Age coefficient is positive. It only means age has positive effects on employment. 

#(4) We have 2 variables here.
flike1 = function(par,x1,x2,yvar)
{xbeta = par[1] + par[2]*x1 +par[3]*x2
pr  = pnorm(xbeta)
pr[pr>0.999999] = 0.999999
pr[pr<0.000001] = 0.000001
like           = yvar*log(pr) + (1-yvar)*log(1-pr)
return(-sum(like))}
set.seed(123)
x1 = data3$age
x2=data3$wage
yvar = as.numeric(data3$empstat == "Employed")
ntry = 100    #ntry can be larger
out1 = mat.or.vec(ntry,4)
for (i in 1:ntry){
  start = c(runif(1,-0.1,0.1),runif(1,-0.01,0.01),runif(1,-0.0001,0.0001))
  res = optim(start,fn = flike1,method = "BFGS",
              control = list(trace=6,maxit=1000),
              x1 = x1,x2=x2,
              yvar = yvar)
  out1[i,c(1,2,3)] = res$par
  out1[i,4] = res$value}
out1 = data.frame(out1)
colnames(out1) = c("theta", "bar_age","bar_wage", "likelihood")
out1[which(out1$likelihood == min(out1$likelihood)),]
#Yes. Both coefficients are positive. 
#It shows that both wage and age have a positive effect on employment

#Exercise4(1) use data from Exercise2(1)
dat_total1<-rbind(dati1,dati2,dati3,dati4,dati5,
                 dati6,dati7,dati8,dati9,dati10,dati11)

dat_total1<-dat_total1[-which(dat_total1$empstat=="Inactive")]
dat_total1<-dat_total1[-which(dat_total1$empstat=="Retired")]
dat_total1<-dat_total1[complete.cases(dat_total1[,10])]

dat_total1$"2005" = as.numeric(dat_total1$year==2005)
dat_total1$"2006" = as.numeric(dat_total1$year==2006)
dat_total1$"2007" = as.numeric(dat_total1$year==2007)
dat_total1$"2008" = as.numeric(dat_total1$year==2008)
dat_total1$"2009" = as.numeric(dat_total1$year==2009)
dat_total1$"2010" = as.numeric(dat_total1$year==2010)
dat_total1$"2011" = as.numeric(dat_total1$year==2011)
dat_total1$"2012"= as.numeric(dat_total1$year==2012)
dat_total1$"2013" = as.numeric(dat_total1$year==2013)
dat_total1$"2014" = as.numeric(dat_total1$year==2014)
dat_total1
#create year dummy variables

#(2)# probit
set.seed(123)
x1 = dat_total1$age
x2 = dat_total1$"2005"
x3 = dat_total1$"2006"
x4 = dat_total1$"2007"
x5 = dat_total1$"2008"
x6 = dat_total1$"2009"
x7 = dat_total1$"2010"
x8 = dat_total1$"2011"
x9 = dat_total1$"2012"
x10 = dat_total1$"2013"
x11 = dat_total1$"2014"
yvar = as.numeric(dat_total1$empstat == "Employed")

flike2 = function(par,x1,x2,x3,x4,x5,x6,x7,x8,x9,x10,x11,yvar)
{xbeta = par[1] + par[2]*x1 +par[3]*x2+par[4]*x3+par[5]*x4+par[6]*x5+
  par[7]*x6+par[8]*x7+par[9]*x8+par[10]*x9+par[11]*x10+par[12]*x11
pr  = pnorm(xbeta)
pr[pr>0.999999] = 0.999999
pr[pr<0.000001] = 0.000001
like           = yvar*log(pr) + (1-yvar)*log(1-pr)
return(-sum(like))}


ntry = 10     #I set a small ntry here because my computer runs slowly.
out2 = mat.or.vec(ntry,13)
for (i in 1:ntry){
  start = c(runif(1,-1,1),runif(11,-0.1,0.1))
  res = optim(start,fn = flike2,method = "BFGS",
              control = list(trace=6,maxit=1000),
              x1 = x1,x2=x2,x3=x3,x4=x4,x5=x5,x6=x6,x7=x7,x8=x8,
              x9=x9,x10=x10,x11=x11,
              yvar = yvar)
out2[i,1:12] = res$par
out2[i,13] = res$value}
out2 = data.frame(out2)
colnames(out2) = c("theta", "bar_age","x2","x3","x4","x5","x6","x7","x8","x9",
                   "x10","x11","likelihood")
out2<-out2[which(out2$likelihood == min(out2$likelihood)),]
out2

# logit
flike3 = function(par,x1,x2,x3,x4,x5,x6,x7,x8,x9,x10,x11,yvar)
{xbeta = par[1] + par[2]*x1 +par[3]*x2+par[4]*x3+par[5]*x4+par[6]*x5+
  par[7]*x6+par[8]*x7+par[9]*x8+par[10]*x9+par[11]*x10+par[12]*x11
pr  = 1/(1+exp(-x_beta))
pr[pr>0.999999] = 0.999999
pr[pr<0.000001] = 0.000001
like           = yvar*log(pr) + (1-yvar)*log(1-pr)
return(-sum(like))}

yvar = as.numeric(dat_total1$empstat == "Employed")
ntry = 10    #I set a small ntry here because my computer runs slowly.
out3 = mat.or.vec(ntry,13)
for (i in 1:ntry){
  start = c(runif(1,-1,1),runif(11,-0.1,0.1))
  res = optim(start,fn = flike2,method = "BFGS",
              control = list(trace=6,maxit=1000),
              x1 = x1,x2=x2,x3=x3,x4=x4,x5=x5,x6=x6,x7=x7,x8=x8,
              x9=x9,x10=x10,x11=x11,
              yvar = yvar)
  out3[i,1:12] = res$par
  out3[i,13] = res$value}
out3 = data.frame(out2)
colnames(out3) = c("theta", "bar_age","x2","x3","x4","x5","x6","x7","x8","x9",
                   "x10","x11","likelihood")
out3<-out3[which(out3$likelihood == min(out3$likelihood)),]
out3

#linear
A = cbind(rep(1,length(dat_total1$age)),x1) %>% 
  cbind(x2,x3,x4,x5,x6,x7,x8,x9,x10,x11)
B = yvar
¦Â_a1<- solve(t(A)%*%A)%*%t(A)%*%B # 
colnames(¦Â_a1) = c("theta", "bar_age","x2","x3","x4","x5","x6","x7","x8","x9",
                   "x10","x11")
¦Â_a1

(3)#probit
#yvar is dummy variable showing empstat
dat_total2<-as.data.frame(cbind(x1,x2,x3,x4,x5,x6,x7,x8,x9,x10,x11,yvar))

glm1<-glm(yvar~x1+x2+x3+x4+x5+x6+x7+x8+x9+x10+x11,family=binomial(link=probit),data=dat_total2)
summary(glm1)  #




#logit
glm2<-glm(yvar~x1+x2+x3+x4+x5+x6+x7+x8+x9+x10+x11,family=binomial(link=logit),data=dat_total2)
summary(glm2)



#linear
lm1<-lm(yvar~x1+x2+x3+x4+x5+x6+x7+x8+x9+x10+x11,data=dat_total2)
summary(lm1)

#The parameters calculated by the three models are different.
##For probit and logit model, positive coefficient only means variables have positive effects 
#on employment. On the contrary, Ols coefficient shows the change of dependent variable when 
#the independent variables change 1 unit.
#¦Âwage is most significant. In different models, some years are significant and some are not.

#Exercise5  (1)
x1_ave = mean(x1)
x2_ave = mean(x2)
x3_ave = mean(x3)
x4_ave = mean(x4)
x5_ave = mean(x5)
x6_ave = mean(x6)
x7_ave = mean(x7)
x8_ave = mean(x8)
x9_ave = mean(x9)
x10_ave = mean(x10)
x11_ave = mean(x11)
x_ave = c(1,x1_ave,x2_ave,x3_ave,x4_ave,x5_ave,x6_ave,
  x7_ave,x8_ave,x9_ave,x10_ave,x11_ave)

#marginal effect of probit
¦Âx_ave = sum(out2[,-13] *x_ave)   #out2 is from exercise4,probit
margi_pro = dnorm(¦Âx_ave) * out2[,-13]
margi_pro

#or
#probitscalar<-mean(dnorm(predict(glm1,type="link")))  
#probitcalar*coef(glm1)

#marginal effect of logit
¦Âx_ave1 = sum(out3[,-13] *x_ave)
margi_log = exp(¦Âx_ave1)/(1+exp(¦Âx_ave1))^2 *out3[,-13]
margi_log

#or
#logitscalar<-mean(dlogis(predict(glm2,type="link")))
#logitscalar*coef(glm2)

#(2)standard error

#use packages "mfx"
#probit standard errors
probitmfx(formula = yvar ~ x1+x2+x3+x4+x5+x6+x7+x8+x9+x10+x11, data=dat_total2,atmean = TRUE)

#logit standard errors
logitmfx(formula = yvar ~ x1+x2+x3+x4+x5+x6+x7+x8+x9+x10+x11, data=dat_total2,atmean = TRUE)


