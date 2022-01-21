#exercise1 (1)#
setwd("D:/R . Data/hw1/Data/Data")
getwd()
library(dplyr)
library(data.table)
data1<-fread("dathh2007.csv")
length(unique(data1$idmen))# 10498

#(2)
data2<-fread("dathh2005.csv")
sum(data2$mstatus=="Couple, with Kids") 
#3374  #or data2 %>% group_by(mstatus) %>% count

#(3)
data3<-fread("datind2008.csv")
nrow(data3) #25510

#(4)
data4<-fread("datind2016.csv")
table(cut(data4$age,c(25,35),include.lowest=TRUE))#2765

#(5)
data5<-fread("datind2009.csv")
table(data5[,c("profession","gender")])

#(6)
data6<-fread("datind2005.csv")
data7<-fread("datind2019.csv")
data6<-data6[complete.cases(data6$wage),]     #rm.na
data7<-data7[complete.cases(data7$wage),]
summary(data6$wage)  #mean=11992
summary(data7$wage)  #mean=15350
sd(data6$wage,na.rm=T)  #sd=17318.56
sd(data7$wage,na.rm=T) #sd=23207.18
quantile(data6$wage,c(0.1,0.9),na.rm=TRUE) #D1=0,D9=32340.4,D9/D1=infinity
quantile(data7$wage,c(0.1,0.9),na.rm=TRUE) #D1=0,D9=40267,D9/D1=infinity
#gini
gini_df <- data6 %>% 
  arrange(wage) %>%
  mutate(Rect = rank(wage)/n()) %>%
  mutate(Rect_Income = cumsum(wage)/sum(wage)) %>%
  mutate(gini = sum(2*(Rect- Rect_Income)/n()))
gini_num <- gini_df %>% select(gini) %>%distinct()
gini_num     #gini_num=0.6671654

gini_df1 <- data7 %>% 
  arrange(wage) %>%
  mutate(Rect = rank(wage)/n()) %>%
  mutate(Rect_Income = cumsum(wage)/sum(wage)) %>%
  mutate(gini = sum(2*(Rect- Rect_Income)/n()))
gini_num1 <- gini_df1 %>% select(gini) %>%distinct()
gini_num1  #gini_num1=0.6655301

 #(7)
data8<-fread("datind2010.csv")
summary(data8$age)
age1<-cut(data8$age,breaks = c(0,20,40,60,80,110),include.lowest=TRUE,
          na.rm=TRUE,labels = c(0~20,20~40,40~60,60~80,">80")) 
age2<-summary(age1)
hist(data8$age,xlab="age",ylab="number") #histogram of age
hist(data8[data8$gender == "Male",]$age, breaks = 10,xlab="age",ylab="number")  
#histogram of female
hist(data8[data8$gender == "Female",]$age, breaks = 10,xlab="age",ylab="number")
#histogram of male. There is little age difference between men and women

#(8)
data9<-fread("dathh2011.csv")
data10<-fread("datind2011.csv")
data9<-data9[which(data9$location == "Paris"),]
c <- inner_join(data10,data9,c("idmen"))
list(c)
nrow(c) #c:3514


#exercise2 (1)#
dati1<-fread("datind2004.csv",colClasses=c(idind="character",idmen="character"))
dati2<-fread("datind2005.csv",colClasses=c(idind="character",idmen="character"))
dati3<-fread("datind2006.csv",colClasses=c(idind="character",idmen="character"))
dati4<-fread("datind2007.csv",colClasses=c(idind="character",idmen="character"))
dati5<-fread("datind2008.csv", colClasses=c(idind="character",idmen="character"))
dati6<-fread("datind2009.csv", colClasses=c(idind="character",idmen="character"))
dati7<-fread("datind2010.csv", colClasses=c(idind="character",idmen="character"))
dati8<-fread("datind2011.csv", colClasses=c(idind="character",idmen="character"))
dati9<-fread("datind2012.csv", colClasses=c(idind="character",idmen="character"))
dati10<-fread("datind2013.csv", colClasses=c(idind="character",idmen="character"))
dati11<-fread("datind2014.csv", colClasses=c(idind="character",idmen="character"))
dati12<-fread("datind2015.csv", colClasses=c(idind="character",idmen="character"))
dati13<-fread("datind2016.csv", colClasses=c(idind="character",idmen="character"))
dati14<-fread("datind2017.csv", colClasses=c(idind="character",idmen="character"))
dati15<-fread("datind2018.csv", colClasses=c(idind="character",idmen="character"))
dati16<-fread("datind2019.csv", colClasses=c(idind="character",idmen="character"))
append1<-rbind(dati1,dati2,dati3,dati4,dati5,
               dati6,dati7,dati8,dati9,dati10,dati11,dati12,
               dati13,dati14,dati15,dati16) #append individual

#(2)
dath1<-fread("dathh2004.csv",colClasses=c(idmen="character"))
dath2<-fread("dathh2005.csv",colClasses=c(idmen="character"))
dath3<-fread("dathh2006.csv",colClasses=c(idmen="character"))
dath4<-fread("dathh2007.csv",colClasses=c(idmen="character"))
dath5<-fread("dathh2008.csv",colClasses=c(idmen="character"))
dath6<-fread("dathh2009.csv",colClasses=c(idmen="character"))
dath7<-fread("dathh2010.csv",colClasses=c(idmen="character"))
dath8<-fread("dathh2011.csv",colClasses=c(idmen="character"))
dath9<-fread("dathh2012.csv",colClasses=c(idmen="character"))
dath10<-fread("dathh2013.csv",colClasses=c(idmen="character"))
dath11<-fread("dathh2014.csv",colClasses=c(idmen="character"))
dath12<-fread("dathh2015.csv",colClasses=c(idmen="character"))
dath13<-fread("dathh2016.csv",colClasses=c(idmen="character"))
dath14<-fread("dathh2017.csv",colClasses=c(idmen="character"))
dath15<-fread("dathh2018.csv",colClasses=c(idmen="character"))
dath16<-fread("dathh2019.csv",colClasses=c(idmen="character"))
append2<-rbind(dath1,dath2,dath3,dath4,dath5,
  dath6,dath7,dath8,dath9,dath10,dath11,dath12,
  dath13,dath14,dath15,dath16) #append households

#(3)
var1 <- colnames(append1)
var2 <- colnames(append2)
intersect(var1,var2) 
#The common variables are "idmen" and "year". V1 is not a variable.

#(4)
merge1<- left_join(append1, append2, by = c("idmen",'year'))

#(5)
a<- append1 %>% group_by(idmen, year) %>% summarize(count = n()) %>% filter(count>4)
list(a)
nrow(a)    #12436
length(unique(a$idmen)) #3622
#(6)
append1 %>% group_by(idmen,empstat) %>% summarize(count = n()) %>% 
   filter(empstat == "Unemployed", count>0) %>% nrow()
#8162
# or append4<-append1[which(append1$empstat== "Unemployed"),]
  #length(unique(append4$idmen)) 


#(7)
w<-append1[which(append1$profession>0),] %>% 
  #First remove the data with profession as blank
  group_by(idmen,year,profession) %>% summarize(count = n()) %>% filter(count>1)
nrow(w)  #7615

#(8)
append3<-append2[which(append2$mstatus=="Couple, with Kids"),] #couple with kids
d<-inner_join(append1,append3,by=c("idmen","year"))  #d:individual from households with kids
  #or merge1[which(merge1$mstatus=="Couple, with Kids"),]
length(unique(d$idind))  #55049

#(9)
append5<-append2[which(append2$location=="Paris"),] # Households in Paris
e<-inner_join(append1,append5,by=c("idmen","year")) #Individual in paris
#or merge1[which(merge1$location=="Paris"),]
length(unique(e$idind))  #14563

#(10)
f<-append1 %>% group_by(idmen,year) %>% summarize(count = n()) #
max(f$count)      #14 people
f %>% filter(count=="14")   
#2207811124040100 in 2007 and 2510263102990100 in 2010; 

#(11)
nrow(dath7)  #household present in 2010
nrow(dath8)  #household present in 2011
hboth<-semi_join(dath7,dath8,by=c("idmen")) #household both present in 2010 and 2011
nrow(hboth)      #8984 household present in 2010 and 2011

#exercise3 (1)
x<-arrange(append2,idmen,year)   # x
g<-append2 %>% group_by(idmen) %>% summarize(count=n()) 
# distribution of the time spent in the survey

#(2)
j<- append2 %>% group_by(idmen,year,datent) %>% filter(year==datent) 
# J:household moved into its current dwelling at the year of survey#
head(j,10)
j4<-semi_join(merge1,j,by=c("idmen")) #individual moved
j1<-  j4 %>% group_by(year) %>% summarize(count = n())
j2<-append1 %>% group_by(year) %>% summarize(count = n())
j3<- j2 %>% summarize(year,ratio=j1$count/j2$count)   
#j3:move in ratio each year
plot(j3$year,j3$ratio, xlab = "year", ylab = "move in ratio")

#(3)
k<- append2 %>% group_by(idmen,year,myear) %>% filter(year==myear,year<2015) 
l<- append2 %>% group_by(idmen,year,myear) %>% filter(move=="2",year>=2015)
#I think household migrate that year if move==2
m<- rbind(k,l) # m: household migrated at the year of survey#
head(m,10)

k4<-semi_join(merge1,k,by=c("idmen")) %>% filter(year<2015) #individual moved
k1<-k4 %>% group_by(year) %>% summarize(count = n())
k2<-append1 %>% group_by(year) %>% filter(year<2015) %>% summarize(count = n())
k3<- k2 %>% summarize(year,ratio=k1$count/k2$count) #migration ratio before 2015

l4<-semi_join(merge1,l,by=c("idmen")) %>% filter(year>=2015)
l1<-l4 %>% group_by(year) %>% summarize(count = n())
l2<-append1 %>% group_by(year) %>% filter(year>=2015) %>% summarize(count = n())
l3<- l2 %>% summarize(year,ratio=l1$count/l2$count) #migration ratio after 2015
m1<-rbind(k3,l3)
plot(m1$year,m1$ratio, xlab = "year", ylab = "migration in ratio")

#(4)
plot(m1$year,m1$ratio, pch = 19,type = "b",col = "black", xlab = "year", ylab = "ratio")
axis(side=1)
lines(j3$year,j3$ratio,  col = "yellow") 
#I like the first way because it is more direct and convenient.
# And the logic of statistics is easy to understand. 
#The second method needs to be divided into two variables and is not very rigorous after 2015.

#(5)

p1<-append1 %>%  select(idind,empstat,idmen) %>% group_by(idind,empstat)  %>% 
  summarize(count = n()) 
p2<-p1[duplicated(p1$idind),] %>% select(idind)  #individual changed employment status.
p3<-inner_join(append1,p2,by=c("idind")) #p3 individual changed employment  with idmen

p4<-append1 %>% select(idind,profession,idmen) %>%  group_by(idind,profession)  %>% 
  summarize(count = n()) 
  p5<-p4[duplicated(p4$idind),] %>% select(idind)
  #p5individual changed profession
  p6<-inner_join(append1,p5,by=c("idind"))   #p5individual changed profession with idmen
  p<-rbind(p3,p6)

  o<- append2 %>% group_by(idmen,year) %>% filter(myear>0,year<2015) 
  p<- append2 %>% group_by(idmen,year) %>% filter(move=="2",year>=2015)
  p7<-rbind(o,p)   #p7household migration
 p8<-inner_join(p7,p,by=c("idmen"))  #p8 households changed profession or employment status.
  length(unique(p8$idmen))  #2192
  
  #exercise4
  #(1) Only consider exit and define the year the person exits as the attrition of that year 
  z<-arrange(append1,idind,year) # z:entry and exit for each individual.
   z_<-z[!duplicated(z$idind, fromLast=TRUE), ] 
   #Discard duplicate values and keep the maximum year. 
   #So z_ only records the year in which each person left the panel
  prop.table(table(z_$year))  #The percentage of people who exit each year
   
  
  
  #(2) Consider entering and leaving at the same time. 
  #Calculate the annual change in individual
  z1<-nrow(dati1)-nrow(dati2)
   z2<-nrow(dati2)-nrow(dati3)
   z3<-nrow(dati3)-nrow(dati4)
   z4<-nrow(dati4)-nrow(dati5)
   z5<-nrow(dati5)-nrow(dati6)
   z6<-nrow(dati6)-nrow(dati7)
   z7<-nrow(dati7)-nrow(dati8)
   z8<-nrow(dati8)-nrow(dati9)
   z9<-nrow(dati9)-nrow(dati10)
   z10<-nrow(dati10)-nrow(dati11)
   z11<-nrow(dati11)-nrow(dati12)
   z12<-nrow(dati12)-nrow(dati13)
   z13<-nrow(dati13)-nrow(dati14)
   z14<-nrow(dati14)-nrow(dati15)
   z15<-nrow(dati15)-nrow(dati16)    #z1-z15:reduction in the number of individuals each year
     z16<-as.numeric(mget(paste0("z",1:15)))
   prop.table(z16) #The percentage of employees reduced each year
   # Negative values mean increase