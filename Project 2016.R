install.packages('ggplot2')
install.packages('stargazer')
install.packages('xtable')
install.packages('tidyr')
library(ggplot2)
project <- read.csv("C:/Users/Iris/Downloads/project2016.csv")
female <- subset(project, female==1)
male <- subset(project, female==0)
##Table 1
table <- matrix(c(mean(project$educ), mean(project$age), mean(project$y), mean(project$va),
                  mean(female$educ), mean(female$age), mean(female$y), mean(female$va),
                  mean(male$educ), mean(male$age), mean(male$y), mean(male$va), 
                  t.test(female$educ, male$educ)$statistic, t.test(female$age, male$age)$statistic, t.test(female$y, male$y)$statistic, t.test(female$va, male$va)$statistic), 
                nrow=4, ncol=4)
colnames(table) <- c("All", "Female", "Male", "T-stat")
rownames(table) <- c("Education", "Age", "Hourly Wage", "Productivity")
#####xtable(table)
##Figure 1A
library(ggplot2)
ggplot(data=female, aes(x=y), alpha=0.2)+
  geom_histogram(colour = "pink", fill = "pink", binwidth = .05) +
  geom_histogram(data = male, aes(x=y), fill = "blue", alpha = 0.2, binwidth = .05) +
  xlab("Log Hourly Wages") + ylab("Frequency") +
  ggtitle("Log Hourly Wages for Males and Females")+
  scale_colour_manual(name="Legend",values=c(Female="Pink", Male="Blue"))

##Figure 1B
ggplot(data=female, aes(x=va), alpha=0.2)+
  geom_histogram(colour = "pink", fill = "pink", binwidth = .05) +
  geom_histogram(data = male, aes(x=va), fill = "blue", alpha = 0.2, binwidth = .05) +
  xlab("Productivity of Current Employer") + ylab("Frequency") +
  ggtitle("Productivity for Males and Females")+
  theme(legend.position = "right")+
  scale_fill_discrete(name= "Sex", labels = c("Female", "Male"))


##Table 2
s1 <- lm(y ~ female, project)
project$age2 <- (project$age)^2
project$age3 <- (project$age)^3
s2 <- lm(y ~ educ + age + age2 + age3 + female,project)
male$age2 <- (male$age)^2
male$age3 <- (male$age)^3
female$age2 <- (female$age)^2
female$age3 <- (female$age)^3
s3 <- lm(y ~ educ + age + age2 + age3, male)
s4 <- lm(y ~ educ + age + age2 + age3, female)
library(stargazer)
stargazer(s1,s2,s3,s4, dep.var.labels = c("Log Hourly Wages"), type='text', covariate.labels= c("Education", "Age", "Age2", "Age3", "Female"), omit.stat=c("LL","ser","f"))
male$predict <- predict(s3)
female$predict <- predict(s4)
##Oaxaca Lecture 8
tab2 <- matrix(c(mean(female$educ), mean(female$age), mean(female$age2), mean(female$age3),
                 mean(male$educ), mean(male$age), mean(male$age2), mean(male$age3)), nrow=4, ncol=2)
tab2 <- as.data.frame(tab2)
colnames(tab2) <- c("Female", "Male")
tab2$diffA <- tab2$Female-tab2$Male
tab2$fcoeff <- c(s4$coefficients[2], s4$coefficients[3], s4$coefficients[4], s4$coefficients[5])
tab2$dotA <- tab2$diffA*tab2$fcoeff
tab2$mcoeff <- c(s3$coefficients[2], s3$coefficients[3], s3$coefficients[4], s3$coefficients[5])
tab2$diffB <- (tab2$fcoeff-tab2$mcoeff)
tab2$dotB <- tab2$diffB*tab2$Male
A <- sum(tab2$dotA)
B <- sum(tab2$dotB)
A+B+(s4$coefficients[1]-s3$coefficients[1])
##Figure 2
fgraph <- ggplot(female, aes(x=age, y=y)) +
  geom_point(alpha = .3, position = "identity") +
  geom_line(aes(x=age, y=predict, color= "Cubic")) +
  scale_colour_manual(name="Legend",values=c(Cubic="Red")) +
  facet_grid( ~ educ) +
  ggtitle("Log Hourly Wage by Education Group for Female") +
  labs(x="Age", y="Log Hourly Wage")

mgraph <- ggplot(male, aes(x=age, y=y)) +
  geom_point(alpha = .3, position = "identity") +
  geom_line(aes(x=age, y=predict, color= "Cubic")) +
  scale_colour_manual(name="Legend",values=c(Cubic="Red")) +
  facet_grid( ~ educ) +
  ggtitle("Log Hourly Wage by Education Group for Male") +
  labs(x="Age", y="Log Hourly Wage")
mgraph
##Figure 3
library(caTools)
male12 <- subset(male, educ== 12)
female12 <- subset(female, educ== 12)
set.seed(1)
s=sample(1:nrow(male12),nrow(male12)/2, replace=FALSE)
mtrain <- male12[s, ]
mtest = male12[-s,]
s=sample(1:nrow(female12),nrow(female12)/2, replace=FALSE)
ftrain <- female12[s, ]
ftest = female12[-s,]
###Cubic Splines
library(splines)
k <- seq(3,10,1)
r1<-data.frame()
deg <- k+3
a <- 0
for (i in deg){
  a <- a+1
  r1[a,1] = i-3
  fit = lm(y~bs(age, df=i), data=mtrain)
  pred = predict(fit, newdata = mtest)
  MSE <- mean((mtest$y-pred)^2)
  r1[a,2] = MSE
}
colnames(r1) <- c("K", "MSE")

r2<-data.frame()
a <- 0
for (i in deg){
  a <- a+1
  r2[a,1] = i-3
  fit = lm(y~bs(age, df=i), data=ftrain)
  pred = predict(fit, newdata = ftest)
  MSE <- mean((ftest$y-pred)^2)
  r2[a,2] = MSE
}
colnames(r2) <- c("K", "MSE")

##Figure 3 
mplot <- plot(x=r1$K, y=r1$MSE)
fplot <- plot(x=r2$K, y=r2$MSE)
###Cubic Fit
mcubic <- lm(y~age + age2 + age3, data = male12)
male12$cubic <- predict(mcubic)
cmsem <- mean((male12$cubic - male12$y)^2)

mbestk <- subset(r1, MSE==min(r1$MSE))$K
mdeg <- mbestk + 3
mspline <- lm(y~bs(age, df=mdeg), data=male12)
male12$spline <- predict(mspline)
smsem <- mean((male12$spline-male12$y)^2)
smsem < cmsem

fcubic <- lm(y~age + age2 + age3, data = female12)
female12$cubic <- predict(fcubic)
cmsef <- mean((female12$cubic - female12$y)^2)

fbestk <- subset(r2, MSE==min(r2$MSE))$K
fdeg <- fbestk + 3
fspline <- lm(y~bs(age, df=fdeg), data=female12)
female12$spline <- predict(fspline)
smsef <- mean((female12$spline-female12$y)^2)
smsef < cmsef

##Figure 4a
fig4a <- ggplot(male12, aes(x=age,y=y))+
  geom_jitter(alpha = .3, position = "identity")+
  geom_line(aes(x=age,y=cubic,color = "Cubic"))+
  geom_line(aes(x=age, y=spline, color= "Cubic with Splines"))+
  scale_colour_manual(name="Legend",values=c("Cubic with Splines"="orange", Cubic="red")) +
  ggtitle("Males")+
  labs(x= "Age", y= "Wage")
## Figure 4b
fig4b <- ggplot(female12, aes(x=age,y=y))+
  geom_jitter(alpha = .3, position = "identity")+
  geom_line(aes(x=age,y=cubic,color = "Cubic"))+
  geom_line(aes(x=age, y=spline, color= "Cubic with Splines"))+
  scale_colour_manual(name="Legend",values=c("Cubic with Splines"="orange", Cubic="red")) +
  ggtitle("Females")+
  labs(x= "Age", y= "Wage")

## Table 3
M1<- lm(y~female + educ + age + age2 + age3 , data = project)
M2<- lm(y~female + educ + age + age2 + age3 + va, data = project)
M3<- lm(y~educ + age + age2 + age3 + va, data = male)
M4<- lm(y~educ + age + age2 + age3 + va, data = female)
stargazer(M1, M2, M3, M4, title = "Standard Wage Models w/ productivity", type = "text", omit.stat=c("LL","ser","f"))

#Construct Oaxaca Decompositions with va 
oax <- matrix(c(mean(female$educ), mean(female$age), mean(female$age2), mean(female$age3), mean(female$va),
                mean(male$educ), mean(male$age), mean(male$age2), mean(male$age3), mean(male$va)), 
              nrow=5, ncol=2)
oax <- as.data.frame(oax)
colnames(oax) <- c("Female", "Male")
oax$diffA <- oax$Female-oax$Male
oax$fcoeff <- c(M4$coefficients[2], M4$coefficients[3], M4$coefficients[4], M4$coefficients[5], M4$coefficients[6])
oax$dotA <- oax$diffA*oax$fcoeff
oax$fcoeff <- c(M4$coefficients[2], M4$coefficients[3], M4$coefficients[4], M4$coefficients[5], M4$coefficients[6])
oax$mcoeff <- c(M3$coefficients[2], M3$coefficients[3], M3$coefficients[4], M3$coefficients[5], M3$coefficients[6])
oax$diffB <- (oax$fcoeff-oax$mcoeff)
oax$dotB <- oax$diffB*oax$Male
sum(oax$dotA)+sum(oax$dotB)+(M4$coefficients[1]-M3$coefficients[1])
### renormalize (lec 8 slide 22-24)
minva <- min(project$va)
oax$Male[5] <- oax$Male[5]-minva
oax$Female[5] <- oax$Female[5]-minva
male$norm <- male$va-minva
female$norm <- female$va-minva
M3<- lm(y~educ + age + age2 + age3 + norm, data = male)
M4<- lm(y~educ + age + age2 + age3 + norm, data = female)
oax$fcoeff[5] <- M4$coefficients[6]
oax$mcoeff[5] <- M3$coefficients[6]
oax$diffA <- oax$Female-oax$Male
oax$dotA <- oax$diffA*oax$fcoeff
oax$diffB <- (oax$fcoeff-oax$mcoeff)
oax$dotB <- oax$diffB*oax$Male
sum(oax$dotA)+sum(oax$dotB)+(M4$coefficients[1]-M3$coefficients[1])
## Table 4
C1<- lm(dy~ age + age2 + dva, data = project)
C2<- lm(dy~ age + age2 + female + dva, data = project)
C3<- lm(dy~ age + age2 + dva, data = female)
C4<- lm(dy~ age + age2 + dva, data = male)
stargazer(C1, C2, C3, C4, title = "Change in Wage Due to Job Changes",type = "text", omit.stat=c("LL","ser","f"))
## Table 5
#A - quartile
quant <- quantile(project$va_previous)
library(dplyr)
library(tidyr)
project <-project %>%
  mutate(quartile_vap = ifelse(project$va_previous<=quant[2], "1",
                               ifelse(project$va_previous>quant[2] & project$va_previous<=quant[3], "2",
                                      ifelse(project$va_previous>quant[3] & project$va_previous<=quant[4], "3","4"))))
tab5 <- as.data.frame(prop.table(table(project$quartile_vap, project$female),2))
tab5 <- spread(tab5, Var2, Freq)
tab5 <- tab5[,-1]
colnames(tab5) <- c("Male (va_p)", "Female (va_p)")
#B
quant <- quantile(project$va)
project <-project %>%
  mutate(quartile_va = ifelse(project$va<=quant[2], "1",
                               ifelse(project$va>quant[2] & project$va<=quant[3], "2",
                                      ifelse(project$va>quant[3] & project$va<=quant[4], "3","4"))))
tab5a <- as.data.frame(prop.table(table(project$quartile_va, project$female),2))
tab5a <- spread(tab5a, Var2, Freq)
tab5a <- tab5a[,-1]
colnames(tab5a) <- c("Male (va)", "Female (va)")
tab5 <- cbind(tab5, tab5a)

#C
fem<- subset(project, female == 1)
male<- subset(project, female == 0)

t6<-matrix(c(mean(fem$yl2),mean(fem$yl1),mean(fem$y),mean(fem$yp1)))
rownames(t6)<-c("yl2","yl1","y","yp1")
t6<-as.data.frame(t6)
colnames(t6)<-c("Female Means")

t6$"Male Means"<-c(mean(male$yl2),mean(male$yl1),mean(male$y),mean(male$yp1))

f.moveup<- subset(fem, quartile_vap == 1 & quartile_va ==4)
f.movedown<- subset(fem, quartile_vap == 4 & quartile_va == 1)
m.moveup<- subset(male, quartile_vap == 1 & quartile_va ==4)
m.movedown<- subset(male, quartile_vap == 4 & quartile_va == 1)

t6$"Female MoveUp"<-c(mean(f.moveup$yl2), mean(f.moveup$yl1),mean(f.moveup$y),mean(f.moveup$yp1))
t6$"Female MoveDown"<-c(mean(f.movedown$yl2), mean(f.movedown$yl1),mean(f.movedown$y),mean(f.movedown$yp1))
t6$"Male MoveUp"<-c(mean(m.moveup$yl2), mean(m.moveup$yl1),mean(m.moveup$y),mean(m.moveup$yp1))
t6$"Male MoveDown"<-c(mean(m.movedown$yl2), mean(m.movedown$yl1),mean(m.movedown$y),mean(m.movedown$yp1))

#Panel A-Males
panela <- subset(male, quartile_va==1)
panela<- as.data.frame(panela %>%
                         group_by(quartile_vap) %>%
                         summarize(yl2 = mean(yl2), yl1 = mean(yl1), y = mean(y), yp1 = mean(yp1)))
m.panela_graph <- ggplot(panela, aes(x=c(-2:1)))+
  geom_point(aes(x=-2, y=yl2, color = quartile_vap))+
  geom_point(aes(x=-1,y=yl1, color = quartile_vap))+
  geom_point(aes(x=-0,y=y, color = quartile_vap))+
  geom_point(aes(x=1,y=yp1, color = quartile_vap))+
  ggtitle("Panel A - Males")+
  labs(x="Event Time", y="Wage")
#Panel B-Males
panelb <- subset(male, quartile_va==2)
panelb<- as.data.frame(panelb %>%
                         group_by(quartile_vap) %>%
                         summarize(yl2 = mean(yl2), yl1 = mean(yl1), y = mean(y), yp1 = mean(yp1)))

m.panelb_graph <- ggplot(panelb, aes(x=c(-2:1)))+
  geom_point(aes(x=-2, y=yl2, color = quartile_vap))+
  geom_point(aes(x=-1,y=yl1, color = quartile_vap))+
  geom_point(aes(x=-0,y=y, color = quartile_vap))+
  geom_point(aes(x=1,y=yp1, color = quartile_vap))+
  ggtitle("Panel B - Males")+
  labs(x="Event Time", y="Wage")

#Panel C-Males
panelc <- subset(male, quartile_va==3)
panelc<- as.data.frame(panelc %>%
                         group_by(quartile_vap) %>%
                         summarize(yl2 = mean(yl2), yl1 = mean(yl1), y = mean(y), yp1 = mean(yp1)))
m.panelc_graph <- ggplot(panelc, aes(x=c(-2:1)))+
  geom_point(aes(x=-2, y=yl2, color = quartile_vap))+
  geom_point(aes(x=-1,y=yl1, color = quartile_vap))+
  geom_point(aes(x=-0,y=y, color = quartile_vap))+
  geom_point(aes(x=1,y=yp1, color = quartile_vap))+
  ggtitle("Panel C - Males")+
  labs(x="Event Time", y="Wage")
#Panel D-Males
paneld <- subset(male, quartile_va==4)
paneld<- as.data.frame(paneld %>%
                         group_by(quartile_vap) %>%
                         summarize(yl2 = mean(yl2), yl1 = mean(yl1), y = mean(y), yp1 = mean(yp1)))
m.paneld_graph <- ggplot(paneld, aes(x=c(-2:1)))+
  geom_point(aes(x=-2, y=yl2, color = quartile_vap))+
  geom_point(aes(x=-1,y=yl1, color = quartile_vap))+
  geom_point(aes(x=-0,y=y, color = quartile_vap))+
  geom_point(aes(x=1,y=yp1, color = quartile_vap))+
  ggtitle("Panel  D - Males")+
  labs(x="Event Time", y="Wage")
#Panel A-Females
f.panela <- subset(fem, quartile_va==1)
f.panela<- as.data.frame(f.panela %>%
                           group_by(quartile_vap) %>%
                           summarize(yl2 = mean(yl2), yl1 = mean(yl1), y = mean(y), yp1 = mean(yp1)))
f.panela_graph <- ggplot(f.panela, aes(x=c(-2:1)))+
  geom_point(aes(x=-2, y=yl2, color = quartile_vap))+
  geom_point(aes(x=-1,y=yl1, color = quartile_vap))+
  geom_point(aes(x=-0,y=y, color = quartile_vap))+
  geom_point(aes(x=1,y=yp1, color = quartile_vap))+
  ggtitle("Panel A - Females")+
  labs(x="Event Time", y="Wage")
#Panel B-Females
f.panelb <- subset(fem, quartile_va==2)
f.panelb<- as.data.frame(f.panelb %>%
                           group_by(quartile_vap) %>%
                           summarize(yl2 = mean(yl2), yl1 = mean(yl1), y = mean(y), yp1 = mean(yp1)))
f.panelb_graph <- ggplot(f.panelb, aes(x=c(-2:1)))+
  geom_point(aes(x=-2, y=yl2, color = quartile_vap))+
  geom_point(aes(x=-1,y=yl1, color = quartile_vap))+
  geom_point(aes(x=-0,y=y, color = quartile_vap))+
  geom_point(aes(x=1,y=yp1, color = quartile_vap))+
  ggtitle("Panel B - Females")+
  labs(x="Event Time", y="Wage")
#Panel C-Females
f.panelc <- subset(fem, quartile_va==3)
f.panelc<- as.data.frame(f.panelc %>%
                           group_by(quartile_vap) %>%
                           summarize(yl2 = mean(yl2), yl1 = mean(yl1), y = mean(y), yp1 = mean(yp1)))
f.panelc_graph <- ggplot(f.panelc, aes(x=c(-2:1)))+
  geom_point(aes(x=-2, y=yl2, color = quartile_vap))+
  geom_point(aes(x=-1,y=yl1, color = quartile_vap))+
  geom_point(aes(x=-0,y=y, color = quartile_vap))+
  geom_point(aes(x=1,y=yp1, color = quartile_vap))+
  ggtitle("Panel C - Females")+
  labs(x="Event Time", y="Wage")
#Panel D-Females
f.paneld <- subset(fem, quartile_va==4)
f.paneld<- as.data.frame(f.paneld %>%
                           group_by(quartile_vap) %>%
                           summarize(yl2 = mean(yl2), yl1 = mean(yl1), y = mean(y), yp1 = mean(yp1)))
f.paneld_graph <- ggplot(f.paneld, aes(x=c(-2:1)))+
  geom_point(aes(x=-2, y=yl2, color = quartile_vap))+
  geom_point(aes(x=-1,y=yl1, color = quartile_vap))+
  geom_point(aes(x=-0,y=y, color = quartile_vap))+
  geom_point(aes(x=1,y=yp1, color = quartile_vap))+
  ggtitle("Panel D - Females")+
  labs(x="Event Time", y="Wage")

#Figure 5 - Panel A to D
library(cowplot)
plot_grid(m.panela_graph,m.panelb_graph,m.panelc_graph,m.paneld_graph, ncol=2, nrow=2)
#Figure 6 - Panel A to D
plot_grid(f.panela_graph,f.panelb_graph,f.panelc_graph,f.paneld_graph, ncol=2, nrow=2)

## Table 6
trueBva <- C1$coefficients[4]
project$newy <- project$y - trueBva*project$va
nmale <- subset(project, female==0)
nfemale <- subset(project,female==1)
nM3 <- lm(newy~educ+age+age2+age3, data=nmale)
nM4 <- lm(newy~educ+age+age2+age3, data=nfemale)
oaxN <- matrix(c(mean(nfemale$educ), mean(nfemale$age), mean(nfemale$age2), mean(nfemale$age3), mean(nfemale$va),
                mean(nmale$educ), mean(nmale$age), mean(nmale$age2), mean(nmale$age3), mean(nmale$va)), 
              nrow = 5 , ncol=2)
oaxN <- as.data.frame(oaxN)
colnames(oaxN) <- c("Female", "Male")
oaxN$diffA <- oaxN$Female-oaxN$Male
oaxN$fcoeff <- c(nM4$coefficients[2], nM4$coefficients[3], nM4$coefficients[4], nM4$coefficients[5], trueBva)
oaxN$dotA <- oaxN$diffA*oaxN$fcoeff
oaxN$mcoeff <- c(nM3$coefficients[2], nM3$coefficients[3], nM3$coefficients[4], nM3$coefficients[5], trueBva)
oaxN$diffB <- (oaxN$fcoeff-oaxN$mcoeff)
oaxN$dotB <- oaxN$diffB*oaxN$Male
sum(oaxN$dotA)+sum(oaxN$dotB)+(nM4$coefficients[1]-nM3$coefficients[1])
