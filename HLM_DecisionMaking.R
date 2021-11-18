#loading my data
#dat = read.csv('full_data.csv')
#View(dat)

setwd("C:/Users/tacar/Dropbox/HLM/Project")


library(lattice)
library(knitr)
library(survival)
library(nlme)
library(ggplot2)
require(ggplot2)
#examination of the data
names(dat)
dim(dat)
length(unique(dat$id)) #130 unique IDs


length(unique(dat$balloon_count))


#dat.col = dat[dat$response==2,] #collected baloons

#View(dat.col)

#dat.pop = dat[dat$response==3,] #popped balloons

#View(dat.pop)

#dat.test = dat[dat$response!=1,] #all but the pumps

#View(dat.sm)

#recode id numbers
#dat$id_new <- dat$id.-999

#Correct one!!!
#dat.sm = dat[dat$trial_code!=1,] #selecting through trial code

#View(dat.sm)

write.csv(dat.sm, file = 'small_data.csv')


#reading in new data
dat.sm = read.csv('small_data2.csv')
View(dat.sm)

#grouped data visuals
gd1 = groupedData(pump_count~trial_code|Group, data=dat.sm)
plot(gd1)

#=========================================================#
#======================ICCS===============================#
#=========================================================#


#checking ICC for individuals
lm0 = lme(pump_count~1, random=~1|id, dat.sm)
summary(lm0)

VarCorr(lm0)


ICC = function(mod){
  vc1 = VarCorr(mod) #get varcovar from lme model
  vest= as.matrix(vc1)[,1] #get first clumn from varcovar matrix
  vest= as.numeric(vest) #treat values as numbers
  myicc= vest[1]/(vest[1] + vest[2]) #compute icc
  return(myicc) #present the ICC to the caller!!!
} 

ICC(lm0) #35.9%


#Checking ICC by group


lmgroup = lme(pump_count~1, random=~1|Group, dat.sm)
summary(lmgroup)

VarCorr(lmgroup)


ICC(lmgroup) #3% for group


#earnings ICC

lmearnid = lme(earnings~1, random=~1|id, dat.sm)
summary(lmearnid)

VarCorr(lmearnid)

ICC(lmearnid) #27.45%


#Checking ICC by group


lmearngroup = lme(pump_count~1, random=~1|Group, dat.sm)
summary(lmearngroup)

VarCorr(lmearngroup)

ICC(lmearngroup) #3% for earn by group

#number of decisions an individual makes
dat.sm$decision_num = dat.sm$trail_num-3
View(dat.sm)


mean(dat.sm$pump_count)
lines(mean(dat.sm$pump_count), col='red')

mean(dat.sport$pump_count)

mean(dat.esport$pump_count)

mean(dat.cont$pump_count)

mean(dat.sport$mean_timebtwpumps)

sd(dat.sport$pump_count)

sd(dat.esport$pump_count)

sd(dat.cont$pump_count)

mean(dat.esport$mean_timebtwpumps)

mean(dat.cont$mean_timebtwpumps)

sd(dat.sport$mean_timebtwpumps)

sd(dat.esport$mean_timebtwpumps)

sd(dat.cont$mean_timebtwpumps)

length(unique(dat.sport$id))
length(unique(dat.esport$id))
length(unique(dat.cont$id))

mean(dat.sm$)

?Anova

anova(dat.sm$mean_timebtwpumps, dat.sm$pump_count)

range(dat.sport$pump_count)

range(dat.esport$pump_count)

range(dat.cont$pump_count)

mean(dat.sm$trail_num)

mean(dat.sport$trail_num)

mean(dat.esport$trail_num)

mean(dat.cont$trail_num)

dat.sm = dat.sm[,-1]
View(dat.sm)

dat.sport = dat.sm[dat.sm$Group==2,]
View(dat.sport)

op = par(mfrow=c(3,1))
plot(pump_count~balloon_count, dat.sport, type='l', main='Sport')

plot(pump_count~balloon_count, dat.esport, type='l', ylim= c(1:120), main='eSport')

plot(pump_count~balloon_count, dat.cont, type='l', main='Control')
par(op)


op = par(mfrow=c(3,1))
plot(mean_timebtwpumps~balloon_count, dat.sport, type='l',main='Sport')

plot(mean_timebtwpumps~balloon_count, dat.esport, type='l', main='Sport')

plot(mean_timebtwpumps~balloon_count, dat.cont, type='l', main='Control')

par(op)

?lines

dat.esport = dat.sm[dat.sm$Group==1,]


dat.cont = dat.sm[dat.sm$Group==0,]

cor(dat.sport$pump_count, dat.sport$mean_timebtwpumps)
cor(dat.esport$pump_count, dat.esport$mean_timebtwpumps)
cor(dat.cont$pump_count, dat.cont$mean_timebtwpumps)

?ggplot

ggplot(dat.sm, aes(x = balloon_count, y = pump_count)) + geom_point(shape = 1) + geom_smooth(method = lm, 
                                                                               se = FALSE)



op = par(mfrow=c(3,1))
ggplot(dat.sport, aes(x = balloon_count, y = pump_count)) + geom_point(shape = 1) + geom_smooth(method = lm, se = FALSE)
ggplot(dat.esport, aes(x = balloon_count, y = pump_count)) + geom_point(shape = 1) + geom_smooth(method = lm, se = FALSE)
ggplot(dat.cont, aes(x = balloon_count, y = pump_count)) + geom_point(shape = 1) + geom_smooth(method = lm, se = FALSE)
par(op)
#=========================================================#
#==================Visualizations=========================#
#=========================================================#

#Grouped Lines
grouplines = function(df, col,grp){
  #subset the data
  tmpdat = df[df[,col]==grp,]
  #set up my plot canvas
  plot(NA, 
       xlim=range(df$balloon_count),
       ylim=range(df$pump_count),
       xlab = 'Balloon Count',
       ylab = 'Pump Count',
       main= paste(col, grp, sep=':')
  )
  #plot lines form each member of the group
  for(i in unique(tmpdat$id)){
    abline(lm(balloon_count, 
              tmpdat[tmpdat$id==i,]))
  }
  #plot group means as a fat read line
  abline(lm(pump_count~balloon_count, tmpdat), lwd=4, col='red')
}


layout(matrix(c(1,3),nrow=1))
grouplines(dat.sm, 'Group',1)
grouplines(dat.sm, 'Group',2)
grouplines(dat.sm, 'Group',3)


xyplot(pump_count~balloon_count|factor(Group),
       panel=function(x,y){
         panel.xyplot(x,y);
         panel.lmline(x,y,lty=2)
       },
       data=dat.sm)

xyplot(pump_count~balloon_count|Group==2,
       panel=function(x,y){
         panel.xyplot(x,y);
         panel.lmline(x,y,lty=2)
       },
       data=dat.sm)
xyplot(pump_count~mean_timebtwpumps|balloon_count,
       panel=function(x,y){
         panel.xyplot(x,y);
         panel.lmline(x,y,lty=2)
       },
       data=dat.sm)

xyplot(mean_timebtwpumps~pump_count|Group,
       panel=function(x,y){
         panel.xyplot(x,y);
         panel.lmline(x,y,lty=2)
       },
       data=dat.sm)

#Collect vs pop with mean time between pumps and pump count
xyplot(mean_timebtwpumps~pump_count|trial_code,
       panel=function(x,y){
         panel.xyplot(x,y);
         panel.lmline(x,y,lty=2)
       },
       data=dat.sm)

#Collect vs pop with mean time between pumps and balloon count
xyplot(mean_timebtwpumps~balloon_count|trial_code,
       panel=function(x,y){
         panel.xyplot(x,y);
         panel.lmline(x,y,lty=2)
       },
       data=dat.sm)


xyplot(trail_num~balloon_count|Group,
       panel=function(x,y){
         panel.xyplot(x,y);
         panel.lmline(x,y,lty=2)
       },
       data=dat.sm)


#run the models

lm0 = lme(pump_count~1, random=~1|id, dat.sm)
summary(lm0)

VarCorr(lm0)

lm1 = update(lm0, .~balloon_count, method='ML')

summary(lm1)

#when you log transform stuff.. you interpret it as percent change.. so this shows .045 that is .045% change
#trying to compare models
anova(lm0,lm1)
#could look at residuals.. they are reducing.. so is the intercepts
#we want to explain away the variances


#Random coefficients model
lm2 = update(lm1, random=~balloon_count|id, method='ML') #the 1 in the slope is a given.. if you dont want the intercept you need to take it out with -1


summary(lm2)

#degrees of freedom stay the same for the two models because there was no parameters changed in the fixed effects structure
summary(lm1)$tTable
summary(lm2)$tTable

anova(lm1,lm2)


#now everyone can have their own change.. now we see the slope from experience and differences in the residuals as well
VarCorr(lm1)
VarCorr(lm2)
#We dropped far in residual fit.. it became better becasue everyone has their own change in trajectory.



lm3 = lme(pump_count~balloon_count + factor(Group), random=~ balloon_count|id, method='ML', data=dat.sm)
summary(lm3)
summary(lm3)$tTable #this shows for people who have 0 experience (experience is centered at 0) the difference between black and white is not significant

anova(lm2,lm3)

#interaction between Group and balloon count
lm4 = lme(pump_count ~ balloon_count*factor(Group), random =~balloon_count|id, method='ML',data=dat.sm)

anova(lm3, lm4)

summary(lm4)

anova(lm3,lm4)
#the intercept is now for non blacks with 0 experience.. 
#the impact of experience is less for blacks than the impact of experience for non-blacks
#add the fixed effects exper and the fixed effect for exper"blackyes to determine the outcome


lm5 = lme(pump_count~balloon_count + factor(Group) + mean_timebtwpumps, random=~balloon_count|id, method='ML', data=dat.sm)
summary(lm5)
#where you center the data makes the binary comparisons change
anova(lm4, lm5)
anova(lm3,lm5)


lm7 = lme(pump_count~balloon_count*factor(Group) + mean_timebtwpumps, random=~balloon_count|id, method='ML', data=dat.sm)
summary(lm7)

anova(lm4, lm5)
lm6 = lme(pump_count~balloon_count*(factor(Group)*mean_timebtwpumps), random=~balloon_count|id, method='ML', data=dat.sm)
summary(lm6)


anova(lm5, lm6)
#Interacting experience with HGC and black
lm6 = lme(lnw~exper*(black+hgc.9), random=~exper|id, method='ML', data=smdat)
summary(lm6)$tTable
#understanding the intercept.. at 9th grade for non blacks who have 0 experience.. is in theintercept
#The things that are not interactive become indicated as.. blackyes is for people who are white with 0 experience and highest grade completed 9.. for people who have 0 experience with 9th grade completed for whites; for people who have highest grade completed for whites with no experience

#no interaction of highest grade completed by experience
#but there is an interaction of experience and black

#now will remove the intercept term but keep the slope term

lm7 = lme(lnw~exper + hgc.9 +exper:black, random=~ exper|id, method='ML', data=smdat)
summary(lm7)$tTable
summary(lm7)


#By earnings

#### SO this means nothing!!!!!!!!!!!!

lm0 = lme(earnings~1, random=~1|id, dat.sm)
summary(lm0)

VarCorr(lm0)

lm1 = update(lm0, .~balloon_count, method='ML')

summary(lm1)

#when you log transform stuff.. you interpret it as percent change.. so this shows .045 that is .045% change
#trying to compare models
anova(lm0,lm1)
#could look at residuals.. they are reducing.. so is the intercepts
#we want to explain away the variances


#Random coefficients model
lm2 = update(lm1, random=~balloon_count|id, method='ML') #the 1 in the slope is a given.. if you dont want the intercept you need to take it out with -1


summary(lm2)

#degrees of freedom stay the same for the two models because there was no parameters changed in the fixed effects structure
summary(lm1)$tTable
summary(lm2)$tTable

anova(lm1,lm2)


#now everyone can have their own change.. now we see the slope from experience and differences in the residuals as well
VarCorr(lm1)
VarCorr(lm2)
#We dropped far in residual fit.. it became better becasue everyone has their own change in trajectory.



lm3 = lme(earnings~balloon_count + Group, random=~ balloon_count|id, method='ML', data=dat.sm)
summary(lm3)
summary(lm3)$tTable #this shows for people who have 0 experience (experience is centered at 0) the difference between black and white is not significant

anova(lm2,lm3)

#interaction between Group and balloon count
lm4 = lme(earnings ~ balloon_count*Group, random =~balloon_count|id, method='ML',data=dat.sm)


summary(lm4)

anova(lm3,lm4)
#the intercept is now for non blacks with 0 experience.. 
#the impact of experience is less for blacks than the impact of experience for non-blacks
#add the fixed effects exper and the fixed effect for exper"blackyes to determine the outcome

lm5 = lme(earnings~balloon_count+Group+pump_count, random=~balloon_count|id, method='ML', data=dat.sm)
summary(lm5)$tTable
#where you center the data makes the binary comparisons chang


#Interacting experience with pump count and group
lm6 = lme(earnings~balloon_count*(Group+pump_count), random=~balloon_count|id, method='ML', data=dat.sm)
summary(lm6)$tTable

anova(lm5, lm6)

#now will remove the intercept term but keep the slope term

lm7 = lme(earnings~balloon_count + pump_count +balloon_count:Group, random=~ balloon_count|id, method='ML', data=dat.sm)
summary(lm7)$tTable
summary(lm7)

anova(lm6,lm7)


#Looking at trial code by pump count or earnings?
lm0 = lme(trial_code~1, random=~1|id, dat.sm)
summary(lm0)

VarCorr(lm0)

lm1 = update(lm0, .~balloon_count, method='ML')
summary(lm1)

lm2 = update(lm1, random=~balloon_count|Group, method='ML')
summary(lm2)

anova(lm1,lm2)

lm3 = lme(trial_code~balloon_count + pump_count, random=~ balloon_count|Group, method='ML', data=dat.sm)
summary(lm3)

anova(lm2,lm3)

lm4 = lme(trial_code ~ balloon_count*pump_count, random =~balloon_count|Group, method='ML',data=dat.sm)
summary(lm4)

anova(lm3, lm4)

#Pump count by latency (timebtwpumps) and balloon count per individual


#Intercepts ONly model
lm0 = lme(pump_count~1, random=~1|id, dat.sm)
summary(lm0)

VarCorr(lm0)

ICC(lm0)

#Unconditional Growth Model
lm1 = update(lm0, .~mean_timebtwpumps, method='ML')
summary(lm1)


#Random Coefficients Model
lm2 = update(lm1, random=~mean_timebtwpumps|id, method='ML')
summary(lm2)

anova(lm1,lm2)

lm3 = lme(pump_count~mean_timebtwpumps + balloon_count, random=~mean_timebtwpumps|id, method='ML', data=dat.sm)
summary(lm3)

anova(lm2,lm3)

lm4 = lme(pump_count ~mean_timebtwpumps*balloon_count, random =~mean_timebtwpumps|id, method='ML',data=dat.sm)
summary(lm4)

anova(lm3, lm4)

lm5 = lme(pump_count~mean_timebtwpumps + balloon_count+Group, random=~mean_timebtwpumps|id, method='ML', data=dat.sm)
summary(lm5)

anova(lm4,lm5)

lm6 = lme(pump_count~mean_timebtwpumps*(balloon_count*Group), random=~mean_timebtwpumps|id, method='ML', data=dat.sm)
summary(lm6)

anova(lm5, lm6)
anova(lm4, lm6)

lm7 = lme(pump_count~mean_timebtwpumps + balloon_count + mean_timebtwpumps:Group, random=~ mean_timebtwpumps|id, method='ML', data=dat.sm)
summary(lm7)

anova(lm6,lm7)
summary(lm0)
