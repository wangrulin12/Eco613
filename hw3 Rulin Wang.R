setwd("D:/R . Data/hw3/data/Data")
library(plyr)
library(dplyr)
library(data.table)
library(AER)
library(mlogit)
library(nnet)
library(Epi)
# Exercise1 (1) 
data1<-fread("datstu_v2.csv")
 
length(unique(data1$V1))  #340823 students

dat_1<- unique(data1$schoolcode1) 
dat_2<- unique(data1$schoolcode2)
dat_3<- unique(data1$schoolcode3)
dat_4<- unique(data1$schoolcode4)
dat_5<- unique(data1$schoolcode5)
dat_6<- unique(data1$schoolcode6)
#I get unique first to avoid too much data
dat_1<- data.frame(dat_1)
colnames(dat_1) = c("school")  ##change to the same name
dat_2<- data.frame(dat_2)
colnames(dat_2) = c("school")
dat_3<- data.frame(dat_3)
colnames(dat_3) = c("school")
dat_4<- data.frame(dat_4)
colnames(dat_4) = c("school")
dat_5<- data.frame(dat_5)
colnames(dat_5) = c("school")
dat_6<- data.frame(dat_6)   
colnames(dat_6) = c("school")
dat_7<-rbind(dat_1,dat_2,dat_3,dat_4,dat_5,dat_6)

dat_7<- dat_7[complete.cases(dat_7$school),]
dat_7<-data.frame(dat_7)  ##drop NA
length(unique(dat_7$dat_7))  ##640 schools

pgm1<-unique(data1$choicepgm1)
pgm2<-unique(data1$choicepgm2)
pgm3<-unique(data1$choicepgm3)
pgm4<-unique(data1$choicepgm4)
pgm5<-unique(data1$choicepgm5)
pgm6<-unique(data1$choicepgm6)

pgm1<- data.frame(pgm1)
colnames(pgm1) = c("pgm")
pgm2<- data.frame(pgm2)
colnames(pgm2) = c("pgm")
pgm3<- data.frame(pgm3)
colnames(pgm3) = c("pgm")
pgm4<- data.frame(pgm4)
colnames(pgm4) = c("pgm")
pgm5<- data.frame(pgm5)
colnames(pgm5) = c("pgm")
pgm6<- data.frame(pgm6)
colnames(pgm6) = c("pgm")

pgm7<-rbind(pgm1,pgm2,pgm3,pgm4,pgm5,pgm6)
unique(pgm7)
length(unique(pgm7$pgm)) ##32programs and 1 empty 

##(2)
choice1<-data1 %>% select(schoolcode1,choicepgm1)
choice2<-data1 %>% select(schoolcode2,choicepgm2)
choice3<-data1 %>% select(schoolcode3,choicepgm3)
choice4<-data1 %>% select(schoolcode4,choicepgm4)
choice5<-data1 %>% select(schoolcode5,choicepgm5)
choice6<-data1 %>% select(schoolcode6,choicepgm6)

choices <- rbind(choice1, choice2, 
  choice3, choice4, choice5, choice6,use.names=FALSE)
choices<-unique(choices)

choices<-choices[complete.cases(choices[,1:2])]  ##drop na
choices <- choices[-which(choices$choicepgm1 == ""), ]  ## drop empty data
colnames(choices)[1]<- "schoolcode"
nrow(choices)
## 2773 choices. 

#(3)
data2<-fread("datsss.csv")
school <- data2 %>% select(schoolcode, sssdistrict)
school <- unique(school)  ## schoolcode with their district
student<-data1 %>% select(,1,5:10,17)    
##students with their information

colnames(student)[8] <- "sssdistrict"
student_1<- left_join(student,school, by = c("sssdistrict"))
##student_1 lists all schools have the same district as students' home
student_2<-student_1 %>% filter(schoolcode1==schoolcode|schoolcode2==schoolcode
                                |schoolcode3==schoolcode|schoolcode4==schoolcode
                                |schoolcode5==schoolcode|schoolcode6==schoolcode)
## student_2 includes students applying to at least one senior high schools in
##  the same district to home, but students may appear more than once.
length(unique(student_2$V1))   ##265464

#(4)
data3<-data1[complete.cases(data1[,18])] #Drop NA in rank
 data3<-data3%>% select(,5:10,18)
   adm1<- data3  %>% filter(rankplace=="1") %>% select(,1)
   adm2<- data3  %>% filter(rankplace=="2") %>% select(,2)
   adm3<- data3  %>% filter(rankplace=="3") %>% select(,3)
   adm4<- data3  %>% filter(rankplace=="4") %>% select(,4)
   adm5<- data3  %>% filter(rankplace=="5") %>% select(,5)
   adm6<- data3  %>% filter(rankplace=="6") %>% select(,6)
   # Find admission school of each students 
adm<- rbind(adm1,adm2,adm3,adm4,adm5,adm6,use.names=F)  
table<-data.frame(table(adm$schoolcode1))
colnames(table)[1] <- "school"     
### table: admitted students of each school

#(5)
data4<-data1[complete.cases(data1[,2])] 
data4<-data4[complete.cases(data4[,18])]  #Drop NA in scores,rank   
  data4<-data4%>% select(,2,5:10,18)
  adm_1<- data4  %>% filter(rankplace=="1") %>% select(,1,2)
  colnames(adm_1)[2]="schoolcode"
  adm_2<- data4  %>% filter(rankplace=="2") %>% select(,1,3)
  colnames(adm_2)[2]="schoolcode"
  adm_3<- data4  %>% filter(rankplace=="3") %>% select(,1,4)
  colnames(adm_3)[2]="schoolcode"
  adm_4<- data4  %>% filter(rankplace=="4") %>% select(,1,5)
  colnames(adm_4)[2]="schoolcode"
  adm_5<- data4  %>% filter(rankplace=="5") %>% select(,1,6)
  colnames(adm_5)[2]="schoolcode"
  adm_6<- data4  %>% filter(rankplace=="6") %>% select(,1,7) 
  colnames(adm_6)[2]="schoolcode"
  adm_7<- rbind(adm_1,adm_2,adm_3,adm_4,adm_5,adm_6)  

  adm_7 <- adm_7[,c(2,1)]    
  adm_7<-adm_7[order(adm_7$schoolcode,adm_7$score)]  #order
  table1<- aggregate(adm_7$score,by=list(schoolcode=adm_7$schoolcode),min)
  colnames(table1)[2] <- "cutoff"  
## table1 includes lowest scores of each school
  
  #(6)
  table2<- aggregate(adm_7$score,by=list(schoolcode=adm_7$schoolcode),mean)
  colnames(table2)[2] <- "quality"
  ## table2 includes average scores of each school
 
  # Exercise2 
 ## I will reuse dataset "choices","school" in Exercise1 (2),(3)
  ## "table1", "table2" in (5),(6)
  school<-school[order(school$schoolcode)]
  
##I find some schools have three locations, like "Accra Metropolitan"
#  "Accra Metro", blank. These should be one place.
  school1<-school[!duplicated(school$schoolcode),] ##remain the first one

choices<-choices[order(choices$schoolcode)]
  data5<-left_join(choices,school1,by="schoolcode") #add strict
  
  ll<-data2 %>% select(,3,5,6) %>% unique()
  ll<-ll[!duplicated(ll$schoolcode),]
data5<-left_join(data5,ll,by="schoolcode") #add latitude and longitude
## Then do the same thing as Exercise1(4,5,6), but include program now

data6<-data1[complete.cases(data1[,18])] #Drop NA in rank
data6<-data6%>% select(,2,5:16,18)
adms1<- data6  %>% filter(rankplace=="1") %>% select(,1,2,8)
adms2<- data6  %>% filter(rankplace=="2") %>% select(,1,3,9)
adms3<- data6  %>% filter(rankplace=="3") %>% select(,1,4,10)
adms4<- data6  %>% filter(rankplace=="4") %>% select(,1,5,11)
adms5<- data6  %>% filter(rankplace=="5") %>% select(,1,6,12)
adms6<- data6  %>% filter(rankplace=="6") %>% select(,1,7,13)

adms<- rbind(adms1,adms2,adms3,adms4,adms5,adms6,use.names=F) 

table_ <- aggregate(x=adms$score, by = list(adms$schoolcode1,adms$choicepgm1), FUN=length)
colnames(table_) <- c("schoolcode","choicepgm1","size")
### table_: admitted students of each program of school 



adms <- adms[,c(2,3,1)]  
adms<-adms[order(adms$schoolcode1,adms$choicepgm1,adms$score)]  #order

table11 <- aggregate(x=adms$score, by = list(adms$schoolcode1,adms$choicepgm1), FUN=min)
colnames(table11) <- c("schoolcode","choicepgm1","cutoff")
## table11 includes lowest scores of each program of school

table12 <-aggregate(x=adms$score, by = list(adms$schoolcode1,adms$choicepgm1), FUN=mean)
colnames(table12) <- c("schoolcode","choicepgm1","quality")
## table12 includes lowest scores of each program of school

data5<-left_join(data5,table11,by=c("schoolcode","choicepgm1")) #add lowest score
data5<-left_join(data5,table12,by=c("schoolcode","choicepgm1")) #add average score
data5<-left_join(data5,table_,by=c("schoolcode","choicepgm1")) #add size
## data5 is school level dataset

#Exercise3
# I will use "ll" in Exercise2

data7<-fread("datjss.csv")  
data7<- data7 %>% select(,2:4)
colnames(data7) <- c("jssdistrict","jsslong","jsslat")
data8<-left_join(data1,data7,by=c("jssdistrict"))
### data8 have home location

colnames(ll) <- c("schoolcode1","ssslong1","ssslat1")
### change name, then merge.
data8<-left_join(data8,ll,by=c("schoolcode1"))

##Then repeat otehr 5 times
colnames(ll) <- c("schoolcode2","ssslong2","ssslat2")
   data8<-left_join(data8,ll,by=c("schoolcode2"))
colnames(ll) <- c("schoolcode3","ssslong3","ssslat3")
   data8<-left_join(data8,ll,by=c("schoolcode3"))
colnames(ll) <- c("schoolcode4","ssslong4","ssslat4")
   data8<-left_join(data8,ll,by=c("schoolcode4"))
colnames(ll) <- c("schoolcode5","ssslong5","ssslat5")
   data8<-left_join(data8,ll,by=c("schoolcode5"))
colnames(ll) <- c("schoolcode6","ssslong6","ssslat6")
   data8<-left_join(data8,ll,by=c("schoolcode6"))
   
  ##Then create new variables dist1-6 

data8<-  data8 %>% mutate(dist1 = sqrt( (69.172*(ssslong1 - jsslong) * 
  cos(jsslat/57.3)) ^2   +  (69.172 * (ssslat1 - jsslat))^2 ) ) 

data8<-  data8 %>% mutate(dist2 = sqrt( (69.172*(ssslong2 - jsslong) 
  * cos(jsslat/57.3)) ^2   +  (69.172 * (ssslat2 - jsslat))^2 ) ) 

data8<-  data8 %>% mutate(dist3 = sqrt( (69.172*(ssslong3 - jsslong) 
  * cos(jsslat/57.3)) ^2   +  (69.172 * (ssslat3 - jsslat))^2 ) ) 
data8<-  data8 %>% mutate(dist4 = sqrt( (69.172*(ssslong4 - jsslong) 
    * cos(jsslat/57.3)) ^2   +  (69.172 * (ssslat4 - jsslat))^2 ) ) 
data8<-  data8 %>% mutate(dist5 = sqrt( (69.172*(ssslong5 - jsslong) 
  * cos(jsslat/57.3)) ^2   +  (69.172 * (ssslat5 - jsslat))^2 ) ) 
data8<-  data8 %>% mutate(dist6 = sqrt( (69.172*(ssslong6 - jsslong) 
  * cos(jsslat/57.3)) ^2   +  (69.172 * (ssslat6 - jsslat))^2 ) ) 

## In "data8", dis1-dis6 are distance of 6 choices.

#Exercise4  
# Continue working on data8
#(1) 
data9<- data8 %>% mutate(scode_rev1= substr(schoolcode1,1,3))
data9<- data9 %>% mutate(scode_rev2= substr(schoolcode2,1,3))
data9<- data9 %>% mutate(scode_rev3= substr(schoolcode3,1,3))
data9<- data9 %>% mutate(scode_rev4= substr(schoolcode4,1,3))
data9<- data9 %>% mutate(scode_rev5= substr(schoolcode5,1,3))
data9<- data9 %>% mutate(scode_rev6= substr(schoolcode6,1,3))  
##create new variables scode_rev1-6
data9<-data9 %>% select(,1:4,11:18,33:44)   
## delete useless information. variables scode_rev1-6                  
                 
#(2)  create pgm_rev1-6 as requirement
data9 <- data9 %>%
mutate(pgm_rev1 = ifelse(choicepgm1=="General Arts"|choicepgm1=="Visual Arts","arts", 
  ifelse(choicepgm1=="Business"|choicepgm1=="Home Economics","economics",
    ifelse (choicepgm1=="General Science","science","others")   )))

data9 <- data9 %>%
  mutate(pgm_rev2 = ifelse(choicepgm2=="General Arts"|choicepgm2=="Visual Arts","arts", 
            ifelse(choicepgm2=="Business"|choicepgm2=="Home Economics","economics",
                  ifelse (choicepgm2=="General Science","science","others")  )))

data9 <- data9 %>%
  mutate(pgm_rev3 = ifelse(choicepgm3=="General Arts"|choicepgm3=="Visual Arts","arts", 
          ifelse(choicepgm3=="Business"|choicepgm3=="Home Economics","economics",
              ifelse (choicepgm3=="General Science","science","others") )))

data9 <- data9 %>%
  mutate(pgm_rev4 = ifelse(choicepgm4=="General Arts"|choicepgm4=="Visual Arts","arts", 
             ifelse(choicepgm4=="Business"|choicepgm4=="Home Economics","economics",
                  ifelse (choicepgm4=="General Science","science","others") )))

data9 <- data9 %>%
  mutate(pgm_rev5 = ifelse(choicepgm5=="General Arts"|choicepgm5=="Visual Arts","arts", 
              ifelse(choicepgm5=="Business"|choicepgm5=="Home Economics","economics",
                    ifelse (choicepgm5=="General Science","science","others") )))

data9 <- data9 %>%
  mutate(pgm_rev6 = ifelse(choicepgm6=="General Arts"|choicepgm6=="Visual Arts","arts", 
          ifelse(choicepgm6=="Business"|choicepgm6=="Home Economics","economics",
              ifelse (choicepgm6=="General Science","science","others") )))

data9<-data9 %>% select(,1:4,11:30)   
## delete useless information. variables pgm_rev1-6 in data9  

#(3)
data9 <- data9 %>% mutate(choice_rev1=paste(scode_rev1,pgm_rev1))
data9 <- data9 %>% mutate(choice_rev2=paste(scode_rev2,pgm_rev2))
data9 <- data9 %>% mutate(choice_rev3=paste(scode_rev3,pgm_rev3))
data9 <- data9 %>% mutate(choice_rev4=paste(scode_rev4,pgm_rev4))
data9 <- data9 %>% mutate(choice_rev5=paste(scode_rev5,pgm_rev5))
data9 <- data9 %>% mutate(choice_rev6=paste(scode_rev6,pgm_rev6))
##  variables choice_rev1-6 in data9 

#(4) Do the same thing as exercise1
data10<-data9[complete.cases(data9[,2])] 
data10<-data10[complete.cases(data10[,6])]  #Drop NA in scores,rank   

data11<-data10%>% select(,2,6,25:30)

adm_11<- data11  %>% filter(rankplace=="1") %>% select(,1,3)
colnames(adm_11)[2]="choice_rev"
adm_21<- data11  %>% filter(rankplace=="2") %>% select(,1,4)
colnames(adm_21)[2]="choice_rev"
adm_31<- data11  %>% filter(rankplace=="3") %>% select(,1,5)
colnames(adm_31)[2]="choice_rev"
adm_41<- data11  %>% filter(rankplace=="4") %>% select(,1,6)
colnames(adm_41)[2]="choice_rev"
adm_51<- data11  %>% filter(rankplace=="5") %>% select(,1,7)
colnames(adm_51)[2]="choice_rev"
adm_61<- data11  %>% filter(rankplace=="6") %>% select(,1,8) 
colnames(adm_61)[2]="choice_rev"
adm_71<- rbind(adm_11,adm_21,adm_31,adm_41,adm_51,adm_61)  

adm_71 <- adm_71[,c(2,1)]    
adm_71<-adm_71[order(adm_71$choice_rev,adm_71$score)]  #order
table31<- aggregate(adm_71$score,by=list(choice_rev=adm_71$choice_rev),min)
colnames(table31)[2] <- "cutoff"  
## table31 includes lowest scores of each choice

table32<- aggregate(adm_71$score,by=list(choice_rev=adm_71$choice_rev),mean)
colnames(table32)[2] <- "quality"
## table32 includes average scores of each choice


##(4)(5) order scores in data10, and get first 20000 students
## There are many students getting 355,tied for 20,000. I just get exactly 20,000
data12<-data10[order(-data10$score)]
data12<-data12[c(1:20000),]
## data12 is the 20,000 highest score students.
##Create data13 only includes the first choice
data13<-data12%>% select(,1:7,25)
colnames(data13)[8] <-"choice_rev"  ##change to the same name
data13<-left_join(data13,table31,by=c("choice_rev"))  
data13<-left_join(data13,table32,by=c("choice_rev"))  #add cutoff&quality

##Exercise5(1)
length(unique(data13$choice_rev))  ##246choices
#Then we can build a model with dependent Variable: choice_rev (246 choices)
#  and independent variables: test scores 
#Because test scores are different for each student (choosers' character),
#  we can use multinomial logit to analyze  the effect of the student test score on
#  his first choice. 
data14<- data13
data14$choice_rev <- as.numeric(as.factor(data14$choice_rev))
choice_rev <- data13 %>% select(choice_rev) %>% cbind(data14$choice_rev)  
##  "choice_rev" Number corresponding choice
likelyfun1 = function(beta, data, choice_number, 
                    multinomial_v_num, multinomial_v_start) {
  N = nrow(data14)
  pij = mat.or.vec(N,choice_number)
  ch = data14$choice_rev
   pij[,1] = 1
    for(j in seq(1,choice_number-1))  ### omit a choice 
      {pij[,j+1] = exp(beta[j] + 
                        apply(data[,seq(multinomial_v_start, 
                                       multinomial_v_start + multinomial_v_num - 1),
                                  with=FALSE]* 
                                beta[choice_number + j - 1 + 
                                       choice_number * seq(0,multinomial_v_num-1)],1,sum)) }
  prob   = sweep(pij,MARGIN=1,FUN="/",STATS=rowSums(pij))
  probc = NULL
  for (i in 1:N)
  {probc[i] = prob[i,ch[i]]}
  probc[probc>0.999999] = 0.999999
  probc[probc<0.000001] = 0.000001
  like = sum(log(probc))
  return(-like) }   ##likelyhood function

##(2)
start = runif(490,-0.1,0.1)
res <- optim(start,
             fn=likelyfun1,
             method="BFGS",
             control=list(trace=6,maxit=3000),
             data=data14, 
             choice_number=245, 
             multinomial_v_num=1, 
             multinomial_v_start=2,)   ##start from line10, 1 variable
#Because My computer can't calculate the result according to the above function, 
# instead I choose to use multinom to try

multi1 <- multinom(choice_rev ~ score, data= data14)
multi12 <- as.data.frame( coef(multi1) )
colnames(multi12)[1] <- "Intercept"
multi13<- as.data.frame(c(multi12$Intercept,multi12$score))
colnames(multi13)[1] <- "beta"
## "multi12" then calculate probility


score <- data14$score
choice_rev <- data14$choice_rev

prob_fun1 = function(param,score,choice_rev){
  ni = length(score)
nj = length(unique(choice_rev))
ut = mat.or.vec(ni,nj)
  pn1 = param[1:nj-1]
  pn2 = param[(nj):(2*nj-2)]
  ut[,1] = 0
  for (j in seq(1,nj-1)) {
    ut[,j+1] = pn1[j] + pn2[j]*score
  }
  prob = exp(ut)
  prob = sweep(prob,MARGIN=1,STATS=rowSums(prob),FUN='/')
  return(prob)
}

pij = prob_fun1(multi13$beta,score,choice_rev)[,1:245]
beta_j = multi13$beta[246:490]
beta_i_bar = apply(pij,1,function(x) return(sum(x * beta_j)))
ME_1 = data.frame(pij * beta_j - pij * beta_i_bar)
ME_1 = data.frame(apply(ME_1,MARGIN=2,mean))
colnames(ME_1)[1] <- "marginal effect"


#Exercise6 (1) continue to use data14
# Then we need to bulid a model with dependent Variable: choice_rev (246 choices)
#   and independent variables: quality 
#Because quality is the character for each choice (same choices have same characters),
#  we can use conditional logit to analyze  the effect of the student test score on
#  his first choice. 
likelyfun2 = function(beta, data, choice_number, 
                      conditional_v_num, conditional_v_start) {
  N = nrow(data14)
  pij = mat.or.vec(N,choice_number)
  ch = data$choice_rev
  pij[,1] = exp(0 + apply(data[,conditional_v_start +
                                choice_number * seq(0,conditional_v_num-1),
                              with=FALSE]*beta[seq(choice_number,
                                                   choice_number+conditional_v_num-1)],
                          1,sum))
  for(j in seq(1,choice_number-1)){
    pij[,j+1] = exp(beta[j] +
                      apply(data[,conditional_v_start +
                                  choice_number * seq(0,conditional_v_num-1) + j,
                                with=FALSE] * beta[seq(choice_number,
                                                       choice_number+conditional_v_num-1)],
                            1,sum)) }
prob = sweep(pij,MARGIN=1,FUN="/",STATS=rowSums(pij))
probc = NULL
for (i in 1:N)
{probc[i] = prob[i,ch[i]]}
probc[probc>0.999999] = 0.999999
probc[probc<0.000001] = 0.000001
like = sum(log(probc))
return(-like)}

#(2) Like exercise5, because My computer can't calculate, 
# I try clogit£¬but failed.

data15<-mlogit.data(data14, varying = 11:255, shape = "wide", sep = '_',
                          choice = "choice_rev")
start = runif(180,-1,1)
res1 <- optim(start,
                fn=likelyfun2,
                  method="BFGS",
                    control=list(trace=6,maxit=3000),
                            data=data14,
                            choice_number=245,
                            conditional_v_num=1,
                            conditional_v_start=10)
# My computer cannot solve this problem, so I give up marginal effect.
#Formula: ???pij/???xik= pij (¦Äijk ??? pik)¦Â where ¦Äijk = 1 if j = k; ¦Äijk = 0 if j ??= k






#Exercise7 (1) 	I think the second model (conditional logit) is better, 
#  because ¡°others¡± is a school characteristics rather than individual characteristics. 
# Thus the effect to quality is less than scores when removing ¡°choices¡±. 
#(quality only reduces 1 character)


#(2)
data16 = data12[data12$pgm_rev1 != 'others',]##drop others, then same as exercise4
data16<-data16%>% select(,1:7,25)
colnames(data16)[8] <-"choice_rev" 
data16<-left_join(data16,table31,by=c("choice_rev"))  
data16<-left_join(data16,table32,by=c("choice_rev"))  
data16$choice_rev <- as.numeric(as.factor(data16$choice_rev))

#Then the same as exercise6(2), my computer couldn't figure it out.
#Codes below are what I think I should do.

data17<-mlogit.data(data16, varying = 11:255, shape = "wide", sep = '_',
                    choice = "choice_rev")

start = runif(245,-0.1,0.1)
res2 <- optim(start,
              fn=likelyfun2,
              method="BFGS",
              control=list(trace=6,maxit=3000),
              data=data17,
              choice_number=244,
              conditional_v_num=1,
              conditional_v_start=10)

# (3) Compare reg1 and reg2 can get the result, but I didn't figure it out. 
# I guess choice probabilities do not change much, because there aren't many "others".




