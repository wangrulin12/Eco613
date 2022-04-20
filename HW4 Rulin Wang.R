setwd("D:/R . Data/hw4/Data/Data")
library(plyr)
library(dplyr)
library(data.table)
library(ggplot2)
library(mlogit)
library(nnet)
library(AER)
library(panelr)
library(plm)

# Exercise1 (1) 
data1 <-fread("dat_A4.csv")

data1<-  data1 %>% mutate(age = 2019- KEY_BDATE_Y_1997 ) 
## "age" is the age of each individual in 2019

data1<-  data1 %>% mutate(work_exp= rowSums(data1[,18:28],na.rm = "TRUE")/52)

## "work_exp" is the working year of each individual. 
 # I think 52 weeks represents one year of work experience.

#(2) First change 95(ungraded) to 0 in all variables related to education
a <- which(data1$CV_HGC_BIO_DAD_1997==95)
data1$CV_HGC_BIO_DAD_1997[a] <- 0
b <- which(data1$CV_HGC_BIO_MOM_1997==95)
data1$CV_HGC_BIO_MOM_1997[b] <- 0
c <- which(data1$CV_HGC_RES_DAD_1997==95)
data1$CV_HGC_RES_DAD_1997[c] <- 0
d <- which(data1$CV_HGC_RES_MOM_1997==95)
data1$CV_HGC_RES_MOM_1997[d] <- 0

###Then I assume highest educational degree= None is 0 years of schooling
# I assume GET nad high school take 12 years; Associate degree and junior college take 14 years;
#  Normal college degree takes 16 years ; Master's degree takes 18 years; 
#  PhD and Professional degree take 20 years.

data1$edu_year<- NA
data1$edu_year[which(data1$YSCH.3113_2019=="1")] <- 0
data1$edu_year[which(data1$YSCH.3113_2019=="2")] <- 12
data1$edu_year[which(data1$YSCH.3113_2019=="3")] <- 12
data1$edu_year[which(data1$YSCH.3113_2019=="4")] <- 14
data1$edu_year[which(data1$YSCH.3113_2019=="5")] <- 16
data1$edu_year[which(data1$YSCH.3113_2019=="6")] <- 18
data1$edu_year[which(data1$YSCH.3113_2019=="7")] <- 20
data1$edu_year[which(data1$YSCH.3113_2019=="8")] <- 20
data1$edu_year[which(data1$YSCH.3113_2019=="-1")] <- 0
data1$edu_year[which(data1$YSCH.3113_2019=="-2")] <- 0
## "edu_year" in "data1" is years of schooling of each individual

#(3) visualization
data1[,30][is.na(data1[,30])] <- 0   # change NA to 0
data2 <- subset(data1,data1$YINC_1700_2019 > 0)  ## Choose positive income

## i) income by age 
data2 %>% ggplot(aes(x = YINC_1700_2019, color = as.factor(age))) +  
           labs(x="income",
             y = "density",title = "Income for different Ages"
           ) + geom_density() 

data2 %>% ggplot(aes(x = as.factor(age), y = YINC_1700_2019, ))+  
  labs(x="age",
       y = "Income distribution", title = "Income for different ages"
  )  + geom_boxplot()

## ii) income by gender
data2 %>% ggplot(aes(x = YINC_1700_2019, color = as.factor(KEY_SEX_1997))) +  
  labs(x="income", y = "density",color = "1 = Men,2=Women",
       title = "Income for gender group" ) + geom_density() 

data2 %>% ggplot(aes(x = as.factor(KEY_SEX_1997), y = YINC_1700_2019, ))+  
  labs(x="1 = Men,2=Women",
       y = "Income distribution", title = "Income for gender gruop "
  )  + geom_boxplot()

## iii) income by number of children
data2 %>% filter(CV_BIO_CHILD_HH_U18_2019 >= 0,CV_BIO_CHILD_HH_U18_2019<=5) %>% 
  ## rm na, because na doesn't mean they have no children.
# I also drop individual with 6-9 children because samples are too small.
ggplot(aes(x = YINC_1700_2019, color = as.factor(CV_BIO_CHILD_HH_U18_2019))) +  
  labs(x="income", y = "density",color = "number of children",
       title = "Income for individual with different number of children " ) + geom_density() 

data2 %>% filter(CV_BIO_CHILD_HH_U18_2019 >= 0,CV_BIO_CHILD_HH_U18_2019<=5) %>% 
  ggplot(aes(x = as.factor(CV_BIO_CHILD_HH_U18_2019), y = YINC_1700_2019, ))+  
  labs(x="number of children",
       y = "Income distribution", title = "Income for individual with different number of children "
  )  + geom_boxplot()  

## share of 0 income
##i)Income by age
age <- data1 %>% group_by(age) %>%
  summarize("share" =length(which((YINC_1700_2019==0)=="TRUE"))/length(YINC_1700_2019)) 

ggplot(age,aes(x = age, fill = age,weight = share)) +  
  labs( y = "Share of 0", title = "sahre of 0 Income for different age group "
  )  +geom_bar() 

##ii)Income by gender
gender<-data1 %>% group_by(KEY_SEX_1997) %>%
  summarize("share" =length(which((YINC_1700_2019==0)=="TRUE"))/length(YINC_1700_2019)) 
colnames(gender)[1]<- "gender"
gender$gender[which(gender$gender=="1")] <- "men"
gender$gender[which(gender$gender=="2")] <- "women"

ggplot(gender,aes(x = gender, fill = gender,weight = share)) +  
  labs( y = "Share of 0", title = "sahre of 0 Income for different age group "
  )  +geom_bar() 




##iii)Income by number of children
children<-data1 %>% filter(CV_BIO_CHILD_HH_U18_2019 >= 0,CV_BIO_CHILD_HH_U18_2019<=5) %>% 
  ##drop na and 6-9children
  group_by(CV_BIO_CHILD_HH_U18_2019) %>%
  summarize("share" =length(which((YINC_1700_2019==0)=="TRUE"))/length(YINC_1700_2019)) 
colnames(children)[1]<- "children"

ggplot(children,aes(x =children, fill = children ,weight = share)) +  
  labs( x="number of children",y = "share of 0", 
        title = "Share of 0 Income for different number of children "
  )  +geom_bar() 

## IV)Income by marital status
marital <- data1 %>% group_by(CV_MARSTAT_COLLAPSED_2019) %>% 
  filter(CV_MARSTAT_COLLAPSED_2019 >= 0)%>% 
  summarize("share" =length(which((YINC_1700_2019==0)=="TRUE"))/length(YINC_1700_2019)) 
colnames(marital)[1]<- "marital"

marital$marital[which(marital$marital=="0")] <- "Never-married"
marital$marital[which(marital$marital=="1")] <- "Married"
marital$marital[which(marital$marital=="2")] <- " Separated"
marital$marital[which(marital$marital=="3")] <- "Divorced"
marital$marital[which(marital$marital=="4")] <- "Widowed"   ##change names

ggplot(marital,aes(x =marital, fill = marital ,weight = share)) +  
  labs( x="marital status",y = "share of 0", 
        title = "Share of 0 Income for different marital status "
  )  +geom_bar() 

## combined children and marital status
child_marital <- data1 %>%  filter(CV_BIO_CHILD_HH_U18_2019 >= 0, CV_MARSTAT_COLLAPSED_2019>=0) %>%
  group_by(CV_BIO_CHILD_HH_U18_2019,CV_MARSTAT_COLLAPSED_2019) %>%
  summarize("share" =length(which((YINC_1700_2019==0)=="TRUE"))/length(YINC_1700_2019)) 

colnames(child_marital)[1]<- "children"
colnames(child_marital)[2]<- "marital"
child_marital$marital[which(child_marital$marital=="0")] <- "Never-married"
child_marital$marital[which(child_marital$marital=="1")] <- "Married"
child_marital$marital[which(child_marital$marital=="2")] <- " Separated"
child_marital$marital[which(child_marital$marital=="3")] <- "Divorced"
child_marital$marital[which(child_marital$marital=="4")] <- "Widowed"   ##change names


ggplot(child_marital,aes(x =children, fill = marital ,y = share)) +  
  labs( x="number of children",y = "share of 0",title = "Share of 0 Income for different marital status "
)  +
 geom_bar(stat="identity") 
 
##   Interpretation
# (1)For respondents aged 35 to 39, there was little difference in income distribution.
# 38-year-old respondents earned slightly more than the other groups;

#(2) For respondents, men generally earn more than women;

#(3) Respondents with 1-3 children had higher incomes than those with more than 3 children 
 # and no children;

#(4) For respondents aged 35 to 39, their 0 income ratio was similar, at about 0.4;

#(5)For respondents, men have the similar 0 income ratio as women, at about 0.4;

#(6) Respondents with no children and 4-5 children have a higher 0 income ratio than those with 1-3 children
# Seperated and never-married respondents have a higher 0 income ratio than others.
# Never-married respondents with 6-7 children have a very high 0 income ratio.

# Exercise2 (1)  Continue to use "data2" (positive income)
#OLS model: YINC_1700_2019 = ¦Â1*work_exp + ¦Â2 * edu_year+ ¦Â0
 
OLS1<- data2 %>% filter(work_exp >= 0,edu_year>=0) %>%  ## drop NA
  lm(YINC_1700_2019 ~ work_exp + edu_year, data =.) %>%
  summary()
OLS1
## Working experience and year of education both have postive effect on income.
# For each additional year of work experience, earnings rise by about 1068$
# For each additional year of education, earnings rise by about 2341$

## But there might be a selection problem in this model because people who report 0 income or don't report may not be random.
## For example, people with high income may not be willing to report their income.
#  Thus income data may have error.

##(2) Heckman model can deal with this selection problem because in the first step, Heckman used a probit model
# to calculate the probability of reporting income. In the second step,  Heckman combined these predicted individual probabilities into an additional explanatory variable, 
# along with other control variables, to solve the selection problem

##(3) create a dummy to show 0 or no reported income
data3<-data1
data3$miss <- ifelse(data1$YINC_1700_2019 > 0, 1,0)  
data3<-data3 %>% filter(work_exp >= 0,edu_year>=0)  ##drop NA in work_exp & edu_year

##step1
likely_fuc1 <- function(par,work_exp, edu_year){
  Y = par[1] + par[2]* work_exp + par[3]* edu_year
  prob = pnorm(Y)
  Like = d1*log(prob) + (1-d1)*log(1-prob)
  return(-sum(like))}

step1 <- glm(formula = miss ~ work_exp + edu_year, family = binomial(link = "probit"), data = data3)
summary(step1)

step1_value<- predict(step1)
imr <- dnorm(step1_value)/  pnorm(step1_value)  #pdf/cdf
summary(imr)


##Step2: take imr to oringinal model
imr<-as.data.frame(imr)
data3<-cbind(data3,imr)

likely_fuc2 <- function (par,work_exp, edu_year, imr)  {
  Y   = par[1] + par[2]* work_exp + par[3]* edu_year + par[4] * imr
  Prob = dnorm(Y)
  Prob[Prob>0.99999] = 0.99999
  Prob[Prob<0.00001] = 0.00001
  Like = log(Prob)
  return( - sum(Like) )}

OLS2 <- data3 %>%lm(YINC_1700_2019 ~work_exp + edu_year+imr, data =.)
summary(OLS2)
##  imr is significant. The results of Heckman are quite different from those of OLS. 
## Many low-income people may report 0 income or not report income, which leads to overly optimistic OLS results.

# Exercise3(1)
data1%>% ggplot( aes(x = YINC_1700_2019)) +
  labs(x = "income",y = "number of respondents",
    title = "Incomedistribution" ) +geom_histogram(bins = 20L)
length(which(data1$YINC_1700_2019==100000))  
## 637 people have 100,000 income . I see 0 and 100,000 repeated many times. Repeating 0 is 
# reasonable because many people don't have income. But 100,000 is unreasonable. 
## I check there are notes showing that reports used truncated values. So it might be 100,000.

#(2) Tobit model can be used to solve this problem. Independent variables: income
# dependent variables: work year; education year 

##(3)
data4<-data1
data4$censor <- 0
data4$censor[which(data4$YINC_1700_2019<100000)] <- 1
data4<-data4 %>% filter(work_exp >= 0,edu_year>=0) 



TOB<-  
  tobit(YINC_1700_2019 ~ work_exp + edu_year ,left=-Inf,right = 100000,data=data3)
summary(TOB)


par <- as.vector(c(TOB$coefficients,10.35))
censor<-data4$censor
work_exp<-data4$work_exp
edu_year<- data4$edu_year
income<-data4$YINC_1700_2019

tobit_flike = function(par,x1,x2,x3,y){
  yhat = par[1]*x1 + par[2]*x2 
  residual = y - yhat
  standardization = (100000-yhat)/exp(par[4])
  like = x3*log(dnorm(residual/exp(par[4]))/exp(par[4])) + (1-x3)*log(1 - pnorm(standardization))
  return(-sum(like))
}

start_2 <- par + runif(7,-10,10)
res_2 <- optim(start_2,fn=tobit_flike,method="BFGS",control=list
        (trace=6,REPORT=1,maxit=1000),x1=work_exp,x2=edu_year,x3=censor,y=income,hessian=TRUE)
res_2$par

#(4) Interpretation: Income has a positive correlation with work experience and education year.
# Compared to the original model, coefficients are larger. This is because when using censor data, 
# we ignore some large data, then we underestimate the coefficients.

#Exercise4
data5<-fread("dat_A4_panel.csv")

#(1) Education, work experience and marital status are determinants of wages. But ability may create a selection bias.
# Because people with higher ability can get higher grade then study longer. 
# Also,  people with higher ability may perform better in marriage market, and they can work more years due to their ability.
 # And people with higher ability usually have a higher salary. 
#Thus ability is related to both dependent and independent variables, resulting in endogenous problems.

#(2)
data5_panel <- data5
data5_panel <- data5_panel %>% 
  rename(highest_degree_1998=CV_HIGHEST_DEGREE_9899_1998)
data5_panel <- data5_panel %>% 
  rename(highest_degree_1999=CV_HIGHEST_DEGREE_9900_1999)
data5_panel <- data5_panel %>% 
  rename(highest_degree_2000=CV_HIGHEST_DEGREE_0001_2000)
data5_panel <- data5_panel %>% 
  rename(highest_degree_2001=CV_HIGHEST_DEGREE_0102_2001)
data5_panel <- data5_panel %>% 
  rename(highest_degree_2002=CV_HIGHEST_DEGREE_0203_2002)
data5_panel <- data5_panel %>% 
  rename(highest_degree_2003=CV_HIGHEST_DEGREE_0304_2003)
data5_panel <- data5_panel %>% 
  rename(highest_degree_2004=CV_HIGHEST_DEGREE_0405_2004)
data5_panel <- data5_panel %>% 
  rename(highest_degree_2005=CV_HIGHEST_DEGREE_0506_2005)
data5_panel <- data5_panel %>% 
  rename(highest_degree_2006=CV_HIGHEST_DEGREE_0607_2006)
data5_panel <- data5_panel %>% 
  rename(highest_degree_2007=CV_HIGHEST_DEGREE_0708_2007)
data5_panel <- data5_panel %>% 
  rename(highest_degree_2008=CV_HIGHEST_DEGREE_0809_2008)
data5_panel <- data5_panel %>% 
  rename(highest_degree_2009=CV_HIGHEST_DEGREE_0910_2009)
data5_panel <- data5_panel %>% 
  rename(highest_degree_2010=CV_HIGHEST_DEGREE_1011_2010)
##change name to show year
data5_panel_long <- long_panel(data5_panel, prefix="_", begin  = 1997, end = 2019, 
                               label_location = "end")
data5_panel_long <- subset(data5_panel_long,wave!="2012" & wave!="2014"& wave!="2016"& wave!="2018")
#drop 2012,2014,2016,2018

data5_panel_long <- select(data5_panel_long,c(1:18,22:30))
work<-as.data.frame(data5_panel_long[,c(10:16,20:27)])
work[is.na(work)]<-0
for (i in 3:17) {work[,i]<-as.numeric(work[,i])}
work$work_exp <- rowSums(work[,3:17])/52   # Calculate work year
data5_panel_long$work_exp <- work$work_exp

data5_panel_long$age <- data5_panel_long$wave - data5_panel_long$KEY_BDATE_Y
# Calculate age

#(I)Between estimator

data6 <- as.matrix(data5_panel_long)
data6 <- as.data.frame(data6)
data6$id <- as.numeric(data6$id)
data6$"YINC-1700" <- as.numeric(data6$"YINC-1700")
data6$highest_degree <- as.numeric(data6$highest_degree)
data6$work_exp <- as.numeric(data6$work_exp)
data6$CV_MARSTAT_COLLAPSED <- as.numeric(data6$CV_MARSTAT_COLLAPSED)
#Changing the data format

data6 <- select(data6,id,wave,"YINC-1700",highest_degree,work_exp,CV_MARSTAT_COLLAPSED)
data6 <- data6 %>% mutate(highest_degree_received = case_when
                          (data6$highest_degree == 0 ~ "12",
            data6$highest_degree == 1 ~ "12",
                data6$highest_degree == 2 ~ "12",
        data6$highest_degree == 3 ~ "14",
    data6$highest_degree == 4 ~ "16",
      data6$highest_degree == 5 ~ "18",
        data6$highest_degree == 6 ~ "20",
        data6$highest_degree == 7 ~ "20"))   ##Create education year
data6$highest_degree_received <- as.numeric(data6$highest_degree_received)
data6 <- na.omit(data6)
data7 <- data6 %>% group_by(id) %>% summarize(income=mean(`YINC-1700`,na.rm = TRUE),
        exper=mean(work_exp,na.rm = TRUE),
    edu=mean(highest_degree_received,na.rm = TRUE),
      ms=mean(CV_MARSTAT_COLLAPSED,na.rm=TRUE))
panel_estimator1 <- lm(income~edu+exper+ms, data = data7)
summary(panel_estimator1)


#(ii)Within estimator

data6$aveincome <- ave(data6$"YINC-1700", data6$id, FUN=function(x)ave(x, na.rm=T))
data6$aveedu <- ave(data6$highest_degree_received, data6$id, FUN=function(x)ave(x, na.rm=T)) 
data6$aveexp <- ave(data6$work_exp, data6$id, FUN=function(x)ave(x, na.rm=T)) 
data6$avems<- ave(data6$CV_MARSTAT_COLLAPSED, data6$id, FUN=function(x)ave(x, na.rm=T))
data6$income1 <- data6$"YINC-1700" - data6$aveincome
data6$edu1 <- data6$highest_degree_received - data6$aveedu
data6$exp1 <- data6$work_exp - data6$aveexp
data6$ms1 <- data6$CV_MARSTAT_COLLAPSED - data6$avems
panel_estimator2 <- lm(data6$income1 ~ data6$edu1 + data6$exp1 + data6$ms1)
summary(panel_estimator2)

#(iii)Difference estimator
#continue to use data6
data8 <- select(data6,id,wave,"YINC-1700",highest_degree_received,work_exp,CV_MARSTAT_COLLAPSED)
data8$income1 <- ave(data6$"YINC-1700", data6$id, FUN=function(x) dplyr::lag(x))
data8$edu1 <- ave(data6$highest_degree_received, data6$id, FUN=function(x) dplyr::lag(x))
data8$exp1 <- ave(data6$work_exp, data6$id, FUN=function(x) dplyr::lag(x))
data8$ms1 <- ave(data6$CV_MARSTAT_COLLAPSED, data6$id, FUN=function(x) dplyr::lag(x))
data8$income2 <- data8$"YINC-1700"- data8$income1
data8$edu2 <- data8$highest_degree_received- data8$edu1
data8$exp2 <- data8$work_exp- data8$exp1
data8$ms2 <- data8$CV_MARSTAT_COLLAPSED- data8$ms1
panel_estimator3 <- lm(data8$income2 ~ data8$edu2 + data8$exp2 + data8$ms2)
summary(panel_estimator3 )

#(3) The coefficients obtained by three models are different. But all coefficients are significant 
# and positive. Thus we can make sure education, work experience and marital status have positive effects on wages.
# In addition, I believe that the three models produce different coefficients because of 
# the different ways in which they deal with the data.
# Different approaches to the problem of ability result in different results.

