library(adaptMCMC)
library(numDeriv)
library(ggplot2)
library(corrplot)
library(GGally)
library(MASS)
library(hms)
library(lubridate)
library(gridExtra)
######################################
### Sleep Duration Regression
######################################
### Load Data
merged <- read.csv("data/mergeddf.csv")

### Mullticolinearity
# Is there mullticolinearity?
corrplot(cor(merged[,-c(9, 10)], use="pairwise.complete.obs"), type='upper', method="circle")

# VIF to delete multicolienar predictors
dropcols = c(9,10,12,13,14,15,16,17)
freq_mod1 <- lm(Minutes.Asleep ~ .,data=merged[,-dropcols])
summary(freq_mod1)
car::vif(freq_mod1) #drop Activity.Calories

dropcols = c(8,9,10,12,13,14,15,16,17)
freq_mod2 <- lm(Minutes.Asleep ~ .,data=merged[,-dropcols])
summary(freq_mod2)
car::vif(freq_mod2) #drop Distance

dropcols = c(3,8,9,10,12,13,14,15,16,17)
freq_mod3 <- lm(Minutes.Asleep ~ .,data=merged[,-dropcols])
summary(freq_mod3)
car::vif(freq_mod3) #drop Calories burned

dropcols = c(1,3,8,9,10,12,13,14,15,16,17)
freq_mod4 <- lm(Minutes.Asleep ~ .,data=merged[,-dropcols])
summary(freq_mod4)
car::vif(freq_mod4) #drop Steps

dropcols = c(1,2,3,8,9,10,12,13,14,15,16,17)
freq_mod5 <- lm(Minutes.Asleep ~ .,data=merged[,-dropcols])
summary(freq_mod5)
car::vif(freq_mod5) #looks good!

# save small df
sm_merge <- merged[,-dropcols]
rm(merged)

### Heteroscedasticity
# Analyze residual plots
plot(freq_mod5$residuals, freq_mod5$fitted.values) # variance looks somewhat constant?
# Breusch-Pagan test
lmtest::bptest(freq_mod5) # At 0.05 significance level, don't reject null

### Check for non-linearity
catcol = c(22,23)
ggpairs(sm_merge[,-catcol])

# Examine response vs. minutes sedentary
ggplot(sm_merge[,-catcol], aes(x=Minutes.Sedentary, y=Number.of.Awakenings)) +
  geom_point()  # potentially quadratic? 

sm_merge$Min.Sed.Sq <- sm_merge$Minutes.Sedentary^2 #add quadratic term

# Examine response vs. minutes lightly active
ggplot(sm_merge[,-catcol], aes(x=Minutes.Lightly.Active, y=Minutes.Asleep)) +
  geom_point() # looks random...

# Examine response vs. minutes very active
ggplot(sm_merge[,-catcol], aes(x=Minutes.Very.Active, y=Minutes.Asleep)) +
  geom_point() # looks random...

### Bayesian regression
# Define design matrix X and y
sm_merge$Intercept <- 1
X=as.matrix(sm_merge[,-5])
y=sm_merge$Minutes.Asleep
n = dim(X)[1]
p = dim(X)[2]-1
m = 10000

# Initializations
freq_mod6 <- lm(Minutes.Asleep~., data=sm_merge[,-13])
summary(freq_mod6)

sigma2=numeric(m)
sigma2[1]=summary(freq_mod6)$sigma^2
beta=matrix(0,nrow=m,ncol=p+1)

# Compute betahat
Sinv=solve(t(X)%*%X)
betahat=Sinv%*%t(X)%*%y

# MC sampling
for(i in 1:m)
{
  e=y-X%*%betahat
  sigma2[i]=1/rgamma(1,(n-p-1)/2,t(e)%*%e/2)
  beta[i,]=mvrnorm(1,betahat,sigma2[i]*Sinv)
}
# Plot posteriors of betas
beta = as.data.frame(beta)
colnames(beta) <- colnames(X)
plot1 <- ggplot(beta,aes(Minutes.Sedentary)) + 
          geom_density(fill = "orange", alpha = 0.5) +
          labs(x=expression(beta[Minutes.Sedentary])) +
          geom_vline(xintercept = 0,
                     color="red", size=0.5)

plot2 <- ggplot(beta,aes(Minutes.Lightly.Active)) + 
  geom_density(fill = "orange", alpha = 0.5) +
  labs(x=expression(beta[Minutes.Lightly.Active])) +
  geom_vline(xintercept = 0,
             color="red", size=0.5)

plot3 <- ggplot(beta,aes(Minutes.Fairly.Active)) + 
  geom_density(fill = "orange", alpha = 0.5) +
  labs(x=expression(beta[Minutes.Fairly.Active])) +
  geom_vline(xintercept = 0,
             color="red", size=0.5)

plot4 <- ggplot(beta,aes(Minutes.Very.Active)) + 
  geom_density(fill = "orange", alpha = 0.5) +
  labs(x=expression(beta[Minutes.Very.Active])) +
  geom_vline(xintercept = 0,
             color="red", size=0.5)

plot5 <- ggplot(beta,aes(homework)) + 
  geom_density(fill = "orange", alpha = 0.5) +
  labs(x=expression(beta[homework])) +
  geom_vline(xintercept = 0,
             color="red", size=0.5)

plot6 <- ggplot(beta,aes(project)) + 
  geom_density(fill = "orange", alpha = 0.5) +
  labs(x=expression(beta[project])) +
  geom_vline(xintercept = 0,
             color="red", size=0.5)

plot7 <- ggplot(beta,aes(exam)) + 
  geom_density(fill = "orange", alpha = 0.5) +
  labs(x=expression(beta[exam])) +
  geom_vline(xintercept = 0,
             color="red", size=0.5)

plot8 <- ggplot(beta,aes(Weekly.Assign.Num)) + 
  geom_density(fill = "orange", alpha = 0.5) +
  labs(x=expression(beta[Weekly.Assign.Num])) +
  geom_vline(xintercept = 0,
             color="red", size=0.5)

plot9 <- ggplot(beta,aes(Is.Weekend)) + 
  geom_density(fill = "orange", alpha = 0.5) +
  labs(x=expression(beta[Is.Weekend])) +
  geom_vline(xintercept = 0,
             color="red", size=0.5)

plot10 <- ggplot(beta,aes(Is.Break)) + 
  geom_density(fill = "orange", alpha = 0.5) +
  labs(x=expression(beta[Is.Break])) +
  geom_vline(xintercept = 0,
             color="red", size=0.5)

plot11 <- ggplot(beta,aes(Min.Sed.Sq)) + 
  geom_density(fill = "orange", alpha = 0.5) +
  labs(x=expression(beta[Min.Sed.Sq])) +
  theme(axis.text=element_text(size=5)) +
  geom_vline(xintercept = 0,
             color="red", size=0.5)

plot12 <- ggplot(beta,aes(Intercept)) + 
  geom_density(fill = "orange", alpha = 0.5) +
  labs(x=expression(beta[Intercept])) +
  geom_vline(xintercept = 0,
             color="red", size=0.5)

grid.arrange(plot1,plot2,plot3,plot4,plot5,plot6,plot7,plot8,plot9,plot10,plot11,plot12, ncol=4)

######################################
### Time to Bed Regression
######################################
### Load Data
merged <- read.csv("data/mergeddf.csv")
sleep_time <- hms::hms(second(merged$Start.Time), minute(merged$Start.Time), hour(merged$Start.Time))
oneAM <- hms::hms(second('2020-02-02 00:00:00'), minute('2020-02-02 00:00:00'), hour('2020-02-02 00:00:00'))
merged$Time.Offset <- as.integer((sleep_time-oneAM)/60) # Add time offset response

### Mullticolinearity
# Is there mullticolinearity?
corrplot(cor(merged[,-c(9, 10)], use="pairwise.complete.obs"), type='upper', method="circle")

# VIF to delete multicolienar predictors
dropcols = c(9,10,11,12,13,14,15,16,17)
freq_mod1 <- lm(Time.Offset ~ .,data=merged[,-dropcols])
summary(freq_mod1)
car::vif(freq_mod1) #drop Activity.Calories

dropcols = c(8,9,10,11,12,13,14,15,16,17)
freq_mod2 <-lm(Time.Offset ~ .,data=merged[,-dropcols])
summary(freq_mod2)
car::vif(freq_mod2) #drop Distance

dropcols = c(3,8,9,10,11,12,13,14,15,16,17)
freq_mod3 <- lm(Time.Offset ~ .,data=merged[,-dropcols])
summary(freq_mod3)
car::vif(freq_mod3) #drop Calories burned

dropcols = c(1,3,8,9,10,11,12,13,14,15,16,17)
freq_mod4 <- lm(Time.Offset ~ .,data=merged[,-dropcols])
summary(freq_mod4)
car::vif(freq_mod4) #drop Steps

dropcols = c(1,2,3,8,9,10,11,12,13,14,15,16,17)
freq_mod5 <- lm(Time.Offset ~ .,data=merged[,-dropcols])
summary(freq_mod5)
car::vif(freq_mod5) #looks good!

# save small df
sm_merge <- merged[,-dropcols]
rm(merged)
###
ggpairs(sm_merge)

### Bayesian regression
# Define design matrix X and y
sm_merge$Intercept <- 1
X=as.matrix(sm_merge[,-11])
y=sm_merge$Time.Offset
n = dim(X)[1]
p = dim(X)[2]-1
m = 10000

# Initializations
freq_mod6 <- lm(Time.Offset~., data=sm_merge[,-12])
summary(freq_mod6)

sigma2=numeric(m)
sigma2[1]=summary(freq_mod6)$sigma^2
beta=matrix(0,nrow=m,ncol=p+1)

# Compute betahat
Sinv=solve(t(X)%*%X)
betahat=Sinv%*%t(X)%*%y

# MC sampling
for(i in 1:m)
{
  e=y-X%*%betahat
  sigma2[i]=1/rgamma(1,(n-p-1)/2,t(e)%*%e/2)
  beta[i,]=mvrnorm(1,betahat,sigma2[i]*Sinv)
}
colnames(X)
par(mfrow=c(2,2))
plot(density(beta[,1]), main=colnames(X)[1])
abline(v=0,col=2)
plot(density(beta[,2]), main=colnames(X)[2])
abline(v=0,col=2)
plot(density(beta[,3]), main=colnames(X)[3])
abline(v=0,col=2)
plot(density(beta[,4]), main=colnames(X)[4])
abline(v=0,col=2)
par(mfrow=c(1,1))

par(mfrow=c(2,2))
plot(density(beta[,5]), main=colnames(X)[5])
abline(v=0,col=2)
plot(density(beta[,6]), main=colnames(X)[6])
abline(v=0,col=2)
plot(density(beta[,7]), main=colnames(X)[7])
abline(v=0,col=2)
plot(density(beta[,8]), main=colnames(X)[8])
abline(v=0,col=2)
par(mfrow=c(1,1))

par(mfrow=c(2,2))
plot(density(beta[,9]), main=colnames(X)[9])
abline(v=0,col=2)
plot(density(beta[,10]), main=colnames(X)[10])
abline(v=0,col=2)
plot(density(beta[,11]), main=colnames(X)[11])
abline(v=0,col=2)
plot(density(beta[,12]), main=colnames(X)[12])
abline(v=0,col=2)
par(mfrow=c(1,1))

# nothing is significant...

###############################################
### Sleep Quality Regression (# of Awakenings)
##############################################
### Load Data
merged <- read.csv("data/mergeddf.csv")

### Mullticolinearity
# VIF to delete multicolienar predictors
dropcols = c(9,10,11,12,14,15,16,17)
freq_mod1 <- lm(Number.of.Awakenings ~ .,data=merged[,-dropcols])
summary(freq_mod1)
car::vif(freq_mod1) #drop Activity.Calories

dropcols = c(8,9,10,11,12,14,15,16,17)
freq_mod2 <-lm(Number.of.Awakenings ~ .,data=merged[,-dropcols])
summary(freq_mod2)
car::vif(freq_mod2) #drop Distance

dropcols = c(3,8,9,10,11,12,14,15,16,17)
freq_mod3 <- lm(Number.of.Awakenings ~ .,data=merged[,-dropcols])
summary(freq_mod3)
car::vif(freq_mod3) #drop Calories burned

dropcols = c(1,3,8,9,10,11,12,14,15,16,17)
freq_mod4 <- lm(Number.of.Awakenings ~ .,data=merged[,-dropcols])
summary(freq_mod4)
car::vif(freq_mod4) #drop Steps

dropcols = c(1,2,3,8,9,10,11,12,14,15,16,17)
freq_mod5 <- lm(Number.of.Awakenings ~ .,data=merged[,-dropcols])
summary(freq_mod5)
car::vif(freq_mod5) #looks good!

# save small df
sm_merge <- merged[,-dropcols]
rm(merged)

### Linearity
ggpairs(sm_merge)

sm_merge$Min.Sed.Sq <- sm_merge$Minutes.Sedentary^2 #add quadratic term

### Bayesian regression
# Define design matrix X and y
sm_merge$Intercept <- 1
X=as.matrix(sm_merge[,-5])
y=sm_merge$Number.of.Awakenings
n = dim(X)[1]
p = dim(X)[2]-1
m = 10000

# Initializations
freq_mod6 <-  lm(Number.of.Awakenings ~., data=sm_merge[,-13])
summary(freq_mod6)

sigma2=numeric(m)
sigma2[1]=summary(freq_mod6)$sigma^2
beta=matrix(0,nrow=m,ncol=p+1)

# Compute betahat
Sinv=solve(t(X)%*%X)
betahat=Sinv%*%t(X)%*%y

# MC sampling
for(i in 1:m)
{
  e=y-X%*%betahat
  sigma2[i]=1/rgamma(1,(n-p-1)/2,t(e)%*%e/2)
  beta[i,]=mvrnorm(1,betahat,sigma2[i]*Sinv)
}

beta = as.data.frame(beta)
colnames(beta) <- colnames(X)
plot1 <- ggplot(beta,aes(Minutes.Sedentary)) + 
  geom_density(fill = "seagreen4", alpha = 0.5) +
  labs(x=expression(beta[Minutes.Sedentary])) +
  theme(axis.text=element_text(size=5)) +
  geom_vline(xintercept = 0,
             color="red", size=0.5)

plot2 <- ggplot(beta,aes(Minutes.Lightly.Active)) + 
  geom_density(fill = "seagreen4", alpha = 0.5) +
  labs(x=expression(beta[Minutes.Lightly.Active])) +
  theme(axis.text=element_text(size=5)) +
  geom_vline(xintercept = 0,
             color="red", size=0.5)

plot3 <- ggplot(beta,aes(Minutes.Fairly.Active)) + 
  geom_density(fill = "seagreen4", alpha = 0.5) +
  labs(x=expression(beta[Minutes.Fairly.Active])) +
  geom_vline(xintercept = 0,
             color="red", size=0.5)

plot4 <- ggplot(beta,aes(Minutes.Very.Active)) + 
  geom_density(fill = "seagreen4", alpha = 0.5) +
  labs(x=expression(beta[Minutes.Very.Active])) +
  theme(axis.text=element_text(size=5)) +
  geom_vline(xintercept = 0,
             color="red", size=0.5)

plot5 <- ggplot(beta,aes(homework)) + 
  geom_density(fill = "seagreen4", alpha = 0.5) +
  labs(x=expression(beta[homework])) +
  geom_vline(xintercept = 0,
             color="red", size=0.5)

plot6 <- ggplot(beta,aes(project)) + 
  geom_density(fill = "seagreen4", alpha = 0.5) +
  labs(x=expression(beta[project])) +
  theme(axis.text=element_text(size=5)) +
  geom_vline(xintercept = 0,
             color="red", size=0.5)

plot7 <- ggplot(beta,aes(exam)) + 
  geom_density(fill = "seagreen4", alpha = 0.5) +
  labs(x=expression(beta[exam])) +
  geom_vline(xintercept = 0,
             color="red", size=0.5)

plot8 <- ggplot(beta,aes(Weekly.Assign.Num)) + 
  geom_density(fill = "seagreen4", alpha = 0.5) +
  labs(x=expression(beta[Weekly.Assign.Num])) +
  geom_vline(xintercept = 0,
             color="red", size=0.5)

plot9 <- ggplot(beta,aes(Is.Weekend)) + 
  geom_density(fill = "seagreen4", alpha = 0.5) +
  labs(x=expression(beta[Is.Weekend])) +
  theme(axis.text=element_text(size=5)) +
  geom_vline(xintercept = 0,
             color="red", size=0.5)

plot10 <- ggplot(beta,aes(Is.Break)) + 
  geom_density(fill = "seagreen4", alpha = 0.5) +
  labs(x=expression(beta[Is.Break])) +
  geom_vline(xintercept = 0,
             color="red", size=0.5)

plot11 <- ggplot(beta,aes(Min.Sed.Sq)) + 
  geom_density(fill = "seagreen4", alpha = 0.5) +
  labs(x=expression(beta[Min.Sed.Sq])) +
  theme(axis.text=element_text(size=5)) +
  geom_vline(xintercept = 0,
             color="red", size=0.5)

plot12 <- ggplot(beta,aes(Intercept)) + 
  geom_density(fill = "seagreen4", alpha = 0.5) +
  labs(x=expression(beta[Intercept])) +
  geom_vline(xintercept = 0,
             color="red", size=0.5)

grid.arrange(plot1,plot2,plot3,plot4,plot5,plot6,plot7,plot8,plot9,plot10,plot11,plot12, ncol=4)

###############################################
### Sleep Quality Regression (Minutes Asleep/Time In Bed)
##############################################
### Load Data
merged <- read.csv("data/mergeddf.csv")
merged$Quality <- merged$Minutes.Asleep/merged$Time.in.Bed

### Mullticolinearity
# VIF to delete multicolienar predictors
dropcols = c(9,10,11,12,13,14,15,16,17)
freq_mod1 <- lm(Quality ~ .,data=merged[,-dropcols])
summary(freq_mod1)
car::vif(freq_mod1) #drop Activity.Calories

dropcols = c(8,9,10,11,12,13,14,15,16,17)
freq_mod2 <-lm(Quality ~ .,data=merged[,-dropcols])
summary(freq_mod2)
car::vif(freq_mod2) #drop Distance

dropcols = c(3,8,9,10,11,12,13,14,15,16,17)
freq_mod3 <- lm(Quality ~ .,data=merged[,-dropcols])
summary(freq_mod3)
car::vif(freq_mod3) #drop Calories burned

dropcols = c(1,3,8,9,10,11,12,13,14,15,16,17)
freq_mod4 <- lm(Quality ~ .,data=merged[,-dropcols])
summary(freq_mod4)
car::vif(freq_mod4) #drop Steps

dropcols = c(1,2,3,8,9,10,11,12,13,14,15,16,17)
freq_mod5 <-lm(Quality ~ .,data=merged[,-dropcols])
summary(freq_mod5)
car::vif(freq_mod5) #looks good!

# save small df
sm_merge <- merged[,-dropcols]
rm(merged)
sm_merge$Min.Sed.Sq <- sm_merge$Minutes.Sedentary^2 #add quadratic term
### Linearity
ggpairs(sm_merge)

### Bayesian regression
# Define design matrix X and y
sm_merge$Intercept <- 1
X=as.matrix(sm_merge[,-11])
y=sm_merge$Quality
n = dim(X)[1]
p = dim(X)[2]-1
m = 10000

# Initializations
freq_mod6 <-  lm(Quality ~., data=sm_merge[,-13])
summary(freq_mod6)

sigma2=numeric(m)
sigma2[1]=summary(freq_mod6)$sigma^2
beta=matrix(0,nrow=m,ncol=p+1)

# Compute betahat
Sinv=solve(t(X)%*%X)
betahat=Sinv%*%t(X)%*%y

# MC sampling
for(i in 1:m)
{
  e=y-X%*%betahat
  sigma2[i]=1/rgamma(1,(n-p-1)/2,t(e)%*%e/2)
  beta[i,]=mvrnorm(1,betahat,sigma2[i]*Sinv)
}
colnames(X)
par(mfrow=c(2,2))
plot(density(beta[,1]), main=colnames(X)[1])
abline(v=0,col=2)
plot(density(beta[,2]), main=colnames(X)[2])
abline(v=0,col=2)
plot(density(beta[,3]), main=colnames(X)[3])
abline(v=0,col=2)
plot(density(beta[,4]), main=colnames(X)[4])
abline(v=0,col=2)
par(mfrow=c(1,1))

par(mfrow=c(2,2))
plot(density(beta[,5]), main=colnames(X)[5])
abline(v=0,col=2)
plot(density(beta[,6]), main=colnames(X)[6])
abline(v=0,col=2)
plot(density(beta[,7]), main=colnames(X)[7])
abline(v=0,col=2)
plot(density(beta[,8]), main=colnames(X)[8])
abline(v=0,col=2)
par(mfrow=c(1,1))

par(mfrow=c(2,2))
plot(density(beta[,9]), main=colnames(X)[9])
abline(v=0,col=2)
plot(density(beta[,10]), main=colnames(X)[10])
abline(v=0,col=2)
plot(density(beta[,11]), main=colnames(X)[11])
abline(v=0,col=2)
par(mfrow=c(1,1))

# Results looks very similar to # of awakenings approach
