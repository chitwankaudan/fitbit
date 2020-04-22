library(adaptMCMC)
library(numDeriv)
library(ggplot2)

#################################
### Sleep Times Model
################################
### Load indicator data
sleep <- read.csv("data/sleepdf.csv")
head(sleep)
tail(sleep)
y <- sleep$Asleep
t <- sleep$Time.Offset
#rm(sleep)

# Our logistic model
# y_i | a, b ~ Bern(p(t_i))
# logit(p(t_i)) = a + b*t_i
# p(a, b) propto 1

# Define nlogh, logh, h
nlogh = function(theta){
  alpha = theta[1]
  beta = theta[2]
  loglik = ( sum( y*(alpha+beta*t - log(1+exp(alpha+beta*t))) ) +
               -sum( (1-y)*(log(1+exp(alpha+beta*t))) ) )
  return (-1*loglik)
}
logh = function(theta){
  return(-nlogh(theta))
}
h = function(theta){
  return(exp(logh(theta)))
}

# Find good initializations (laplacian)
thetahat=optim(c(-10, 2), nlogh)$par
Sigmahat=solve(hessian(nlogh,thetahat))

# MCMC sampling using adaptMCMC
m=10000
s=(2.4/sqrt(2))^2*Sigmahat #specify covariance matrix of the jumping distribution
out=MCMC(logh,n=m,init=thetahat,scale=s,adapt=T,acc.rate=.35)
theta = out$samples

# MCMC diagnostics
1-rejectionRate(mcmc(theta))
effectiveSize(theta)
cumuplot(mcmc(theta,start=100,end=m,thin=10)) # check for convergence
plot(mcmc(theta)) #density plots

# Plot logistic curve for quick sanity check
frequentist_logistic = function(time){
  exp(thetahat[1] + thetahat[2]*time) / (1+exp(thetahat[1] + thetahat[2]*time))
}

ggplot(sleep, aes(x=Time.Offset, y=Asleep)) + 
  geom_point(color='slateblue2', alpha=0.01) +
  scale_x_continuous(breaks=c(0,60,120,180,240),
                   labels=c('12:00AM',"1:00AM", "2:00AM", "3:00AM", "4:00AM")) +
  geom_line(aes(x=Time.Offset, y=frequentist_logistic(Time.Offset)), color='black', linetype='dashed') +
  ggtitle("Sleep Time Model")+
  theme(plot.title = element_text(hjust = 0.5)) 
  

# Plot density
theta <- data.frame(theta)
colnames(theta) <- c('alpha', 'beta')
# Plot alpha
a = ggplot(theta, aes(alpha)) + 
  geom_density(fill = "slateblue", alpha = 0.5) +
  geom_vline(xintercept = thetahat[1],
             color="black", linetype="dashed", size=0.5) +
  labs(y='Frequency', x=expression(alpha)) + 
  ggtitle('Alpha Posterior For Sleep Times') +
  theme(plot.title = element_text(hjust = 0.5)) 

# Plot beta
b = ggplot(theta, aes(beta)) + 
  geom_density(fill = "slateblue", alpha = 0.5) +
  geom_vline(xintercept = thetahat[2],
             color="black", linetype="dashed", size=0.5) +
  labs(y='Frequency', x=expression(beta)) + 
  ggtitle('Beta Posterior For Sleep Times') +
  theme(plot.title = element_text(hjust = 0.5)) 

grid.arrange(a,b,ncol=2)
# Distribution of sleeping at 12:00AM
prob_12AM <- data.frame(exp(theta$alpha + theta$beta*0) / (1+exp(theta$alpha + theta$beta*0)))
colnames(prob_12AM) <- c("Prob")
twelve <-ggplot(prob_12AM, aes(Prob)) + 
  geom_density(fill = "slateblue", alpha = 0.5) +
  geom_vline(aes(xintercept = mean(Prob)),
             color="black", linetype="dashed", size=0.5) +
  labs(y='Frequency', x="Prob. of Sleeping by 1:00AM") + 
  ggtitle('Sleeping by 1:00AM Density Plot') +
  theme(plot.title = element_text(hjust = 0.5))
# Distribution of sleeping at 1:00AM
prob_1AM <- data.frame(exp(theta$alpha + theta$beta*60) / (1+exp(theta$alpha + theta$beta*60)))
colnames(prob_1AM) <- c("Prob")

one <-ggplot(prob_1AM, aes(Prob)) + 
  geom_density(fill = "slateblue", alpha = 0.5) +
  geom_vline(aes(xintercept = mean(Prob)),
             color="black", linetype="dashed", size=0.5) +
  labs(y='Frequency', x="Prob. of Sleeping by 1:00AM") + 
  ggtitle('Sleeping by 1:00AM Density Plot') +
  theme(plot.title = element_text(hjust = 0.5))

prob_2AM <- data.frame(exp(theta$alpha + theta$beta*120) / (1+exp(theta$alpha + theta$beta*120)))
colnames(prob_2AM) <- c("Prob")

two <- ggplot(prob_2AM, aes(Prob)) + 
  geom_density(fill = "tomato2", alpha = 0.5) +
  geom_vline(aes(xintercept = mean(Prob)),
             color="black", linetype="dashed", size=0.5) +
  labs(y='Frequency', x="Prob. of Sleeping by 2:00AM") + 
  ggtitle('Sleeping by 2:00AM Density Plot') +
  theme(plot.title = element_text(hjust = 0.5)) 

prob_3AM <- data.frame(exp(theta$alpha + theta$beta*180) / (1+exp(theta$alpha + theta$beta*180)))
colnames(prob_3AM) <- c("Prob")

three <- ggplot(prob_3AM, aes(Prob)) + 
  geom_density(fill = "tomato2", alpha = 0.5) +
  geom_vline(aes(xintercept = mean(Prob)),
             color="black", linetype="dashed", size=0.5) +
  labs(y='Frequency', x="Prob. of Sleeping by 3:00AM") + 
  ggtitle('Sleeping by 3:00AM Density Plot') +
  theme(plot.title = element_text(hjust = 0.5))
grid.arrange(one,two,three,ncol=1)

#################################
### Wake Times Model
################################
### Load indicator data
wake <- read.csv("data/wakedf.csv")
head(wake)
tail(wake)
y <- wake$Asleep
t <- wake$Time.Offset
#rm(sleep)

# Find good initializations (laplacian)
thetahat=optim(c(1, -2), nlogh)$par
Sigmahat=solve(hessian(nlogh,thetahat))

# MCMC sampling using adaptMCMC
m=10000
s=(2.4/sqrt(2))^2*Sigmahat #specify covariance matrix of the jumping distribution
out=MCMC(logh,n=m,init=thetahat,scale=s,adapt=T,acc.rate=.35)
theta = out$samples

# MCMC diagnostics
1-rejectionRate(mcmc(theta))
effectiveSize(theta)
cumuplot(mcmc(theta,start=100,end=m,thin=10)) # check for convergence
plot(mcmc(theta)) #density plots

# Plot logistic curve for quick sanity check
ggplot(wake, aes(x=Time.Offset, y=Asleep)) + 
  geom_point(color='slateblue2', alpha=0.01) +
  scale_x_continuous(breaks=c(0,60,120,180,240,300,360),
                     labels=c('6:00AM',"7:00AM","8:00AM","9:00AM","10:00AM","11:00AM", "12:00AM")) +
  geom_line(aes(x=Time.Offset, y=frequentist_logistic(Time.Offset)), color='black', linetype='dashed')+
  ggtitle("Wake Time Model")+
  theme(plot.title = element_text(hjust = 0.5)) 

# Plot density
theta <- data.frame(theta)
colnames(theta) <- c('alpha', 'beta')
# Plot alpha
a=ggplot(theta, aes(alpha)) + 
  geom_density(fill = "deepskyblue4", alpha = 0.5) +
  geom_vline(xintercept = thetahat[1],
             color="black", linetype="dashed", size=0.5) +
  labs(y='Frequency', x=expression(alpha)) + 
  ggtitle('Alpha Posterior For Wake Times') +
  theme(plot.title = element_text(hjust = 0.5)) 

# Plot beta
b=ggplot(theta, aes(beta)) + 
  geom_density(fill = "deepskyblue4", alpha = 0.5) +
  geom_vline(xintercept = thetahat[2],
             color="black", linetype="dashed", size=0.5) +
  labs(y='Frequency', x=expression(beta)) + 
  ggtitle('Beta Posterior For Wake Times') +
  theme(plot.title = element_text(hjust = 0.5)) 
grid.arrange(a,b,ncol=2)
# Prob of waking by certain times
prob_8AM <- data.frame(1 - (exp(theta$alpha + theta$beta*120) / (1+exp(theta$alpha + theta$beta*120))))
colnames(prob_8AM) <- c("Prob")

eight<-ggplot(prob_8AM, aes(Prob)) + 
  geom_density(fill = "deepskyblue1", alpha = 0.5) +
  geom_vline(aes(xintercept = mean(Prob)),
             color="black", linetype="dashed", size=0.5) +
  labs(y='Frequency', x="Prob. of Waking by 8:00AM") + 
  ggtitle('Waking by 8:00AM Density Plot') +
  theme(plot.title = element_text(hjust = 0.5))

prob_9AM <- data.frame(1 - (exp(theta$alpha + theta$beta*180) / (1+exp(theta$alpha + theta$beta*180))))
colnames(prob_9AM) <- c("Prob")

nine = ggplot(prob_9AM, aes(Prob)) + 
  geom_density(fill = "deepskyblue4", alpha = 0.5) +
  geom_vline(aes(xintercept = mean(Prob)),
             color="black", linetype="dashed", size=0.5) +
  labs(y='Frequency', x="Prob. of Waking by 9:00AM") + 
  ggtitle('Waking by 9:00AM Density Plot') +
  theme(plot.title = element_text(hjust = 0.5))

prob_915AM <- data.frame(1 - (exp(theta$alpha + theta$beta*195) / (1+exp(theta$alpha + theta$beta*195))))
colnames(prob_915AM) <- c("Prob")

ggplot(prob_915AM, aes(Prob)) + 
  geom_density(fill = "deepskyblue4", alpha = 0.5) +
  geom_vline(aes(xintercept = mean(Prob)),
             color="black", linetype="dashed", size=0.5) +
  labs(y='Frequency', x="Prob. of Waking by 9:15AM") + 
  ggtitle('Waking by 9:15AM Density Plot') +
  theme(plot.title = element_text(hjust = 0.5))

prob_930AM <- data.frame(1 - (exp(theta$alpha + theta$beta*210) / (1+exp(theta$alpha + theta$beta*210))))
colnames(prob_930AM) <- c("Prob")

ggplot(prob_930AM, aes(Prob)) + 
  geom_density(fill = "deepskyblue1", alpha = 0.5) +
  geom_vline(aes(xintercept = mean(Prob)),
             color="black", linetype="dashed", size=0.5) +
  labs(y='Frequency', x="Prob. of Waking by 9:30AM") + 
  ggtitle('Waking by 9:30AM Density Plot') +
  theme(plot.title = element_text(hjust = 0.5))

prob_10AM <- data.frame(1 - (exp(theta$alpha + theta$beta*240) / (1+exp(theta$alpha + theta$beta*240))))
colnames(prob_10AM) <- c("Prob")

ten=ggplot(prob_10AM, aes(Prob)) + 
  geom_density(fill = "deepskyblue1", alpha = 0.5) +
  geom_vline(aes(xintercept = mean(Prob)),
             color="black", linetype="dashed", size=0.5) +
  labs(y='Frequency', x="Prob. of Waking by 10:00AM") + 
  ggtitle('Waking by 10:00AM Density Plot') +
  theme(plot.title = element_text(hjust = 0.5))

prob_11AM <- data.frame(1 - (exp(theta$alpha + theta$beta*300) / (1+exp(theta$alpha + theta$beta*300))))
colnames(prob_11AM) <- c("Prob")

ggplot(prob_11AM, aes(Prob)) + 
  geom_density(fill = "deepskyblue1", alpha = 0.5) +
  geom_vline(aes(xintercept = mean(Prob)),
             color="black", linetype="dashed", size=0.5) +
  labs(y='Frequency', x="Prob. of Waking by 11:00AM") + 
  ggtitle('Waking by 11:00AM Density Plot') +
  theme(plot.title = element_text(hjust = 0.5))

grid.arrange(eight,nine,ten,ncol=1)
