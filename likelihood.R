

 

#-----------------------------------
#  Example 1
#  known standard deviation
#  find mu by MLE
#-----------------------------------

x = rnorm(77, mean=100, sd=10)

hist(x)

logL.fn1 = function(z, u)
{ln.fz = dnorm(z, mean = u, sd = 10, log = TRUE)}

 #ln.fz = -log(sqrt(2*pi)) - log(sigma) - (0.5/(sigma^2))*(z - u)^2

mu = seq(50, 150, by=2)

lnL.mu = sapply(x, logL.fn1, mu)
lnL.sum = apply(lnL.mu, 1, sum)

plot(mu, lnL.sum, type='l', 
main="Log Likelihood Estimate of Mean", 
xlab="Estimate of Mean", ylab="Log Likelihood")
segments(x0=100, y0=min(lnL.sum), x1=100, y1=max(lnL.sum), col='red')

mu.max = mu[lnL.sum==max(lnL.sum)]
#-----------------------------------


#-----------------------------------
#  Example 2
#  known mu
#  find standard deviation by MLE
#-----------------------------------

x = rnorm(1000, mean=100, sd=10)

logL.fn2 = function(z, sigma){ln.fz = dnorm(z, mean = 100, sd = sigma, log = TRUE)}

sig = 50:200/10

lnL.sigma = sapply(x, logL.fn2, sig)
lnL.sum = apply(lnL.sigma, 1, sum)

plot(sig, lnL.sum, type='l', 
main="Log Likelihood Estimate of Variance", 
xlab="Estimate of StdDev", ylab="Log Likelihood")
segments(x0=10, y0=min(lnL.sum), x1=10, y1=max(lnL.sum), col='red')

sd.max = sig[lnL.sum==max(lnL.sum)]
segments(x0=sd.max, y0=min(lnL.sum), x1=sd.max, y1=max(lnL.sum), col='blue')

#-----------------------------------


#-----------------------------------
#  Example 3
#  known sigma
#  find mu by MLE
#  Likelihood allowing for right-censoring
#-----------------------------------

x = sort(rnorm(200, mean=100, sd=10))
c = c(rep(1, 175), rep(0, 25)) #censoring indicator "1"=complete "0"=censored

mu = seq(50, 150, by=2)

logL.mu.complete = function(z, u)
{ln.fz = dnorm(z, mean = u, sd = 10, log = TRUE)}

logL.mu.rcensored = function(z, u)
{ln.fz = pnorm(z, mean = u, sd = 10, lower.tail = FALSE, log.p = TRUE)}

lnL.mu.comp <- t(sapply(x[c==1], logL.mu.complete, mu))
lnL.mu.rcens <- t(sapply(x[c==0], logL.mu.rcensored, mu))

lnL.mu.all = rbind(lnL.mu.comp, lnL.mu.rcens)

lnL.mu.sum = apply(lnL.mu.all, 2, sum)

plot(mu, lnL.mu.sum, type='l', 
main="Log Likelihood Estimate of Mean with Right-censoring", 
xlab="Estimate of Mean", ylab="Log Likelihood")
segments(x0=100, y0=min(lnL.mu.sum), x1=100, y1=max(lnL.mu.sum), col='red')

#calculate mean for only 'complete' observations
xm1 = mean(x[c==1])

#calculate mean for all observations, but treating 'right censored' 
#as if they failed at the same time (termination time of test)
x2 = c( x[c==1], (rep(x[176], length(x[c==0]))) )
xm2 = mean(x2)

segments(x0=xm1, y0=min(lnL.mu.sum), x1=xm1, y1=max(lnL.mu.sum), col='blue')

mu.max = mu[lnL.mu.sum==max(lnL.mu.sum)]
#-----------------------------------


#-----------------------------------
#  Example 4
#  known mean
#  find sigma by MLE
#  Likelihood allowing for right-censoring
#-----------------------------------

x = sort(rnorm(200, mean=100, sd=10))
c = c(rep(1, 175), rep(0, 25)) #censoring indicator "1"=complete "0"=censored

sig = 1:200/10

logL.sigma.complete = function(z, sigma)
{ln.fz = dnorm(z, mean = 100, sd = sigma, log = TRUE)}

logL.sigma.rcensored = function(z, sigma)
{ln.fz = pnorm(z, mean = 100, sd = sigma, lower.tail = FALSE, log.p = TRUE)}

lnL.sigma.comp <- t(sapply(x[c==1], logL.sigma.complete, sig))
lnL.sigma.rcens <- t(sapply(x[c==0], logL.sigma.rcensored, sig))

lnL.sigma.all = rbind(lnL.sigma.comp, lnL.sigma.rcens)

lnL.sigma.sum = apply(lnL.sigma.all, 2, sum)

plot(sig[50:150], lnL.sigma.sum[50:150], type='l', 
main="Log Likelihood Estimate of StdDev with Right-censoring", 
xlab="Estimate of StdDev", ylab="Log Likelihood")
segments(x0=10, y0=min(lnL.sigma.sum), x1=10, y1=max(lnL.sigma.sum), col='red')
x9=sig[lnL.sigma.sum == max(lnL.sigma.sum)]
segments(x0=x9, y0=min(lnL.sigma.sum), x1=x9, y1=max(lnL.sigma.sum), col='blue')


#calculate stddev for only 'complete' observations
xsd1 = sd(x[c==1])

#calculate stddev for all observations, but treating 'right censored' 
#as if they failed at the same time (termination time of test)
xs2 = c( x[c==1], (rep(x[176], length(x[c==0]))) )
xsd2 = sd(xs2)

segments(x0=xm1, y0=min(lnL.sum), x1=xm1, y1=max(lnL.sum), col='blue')

sd.max = sig[lnL.sigma.sum==max(lnL.sigma.sum)]
#-----------------------------------


#-----------------------------------
#  Example 5 
#  Data from lognormally distributed population
#  known scale
#  find mu by MLE - but mistakenly assume normal dist
#-----------------------------------

x = rlnorm(n=200, meanlog = 0, sdlog = 1)
hist(x)

logL.fn1 = function(z, u){ln.fz = dnorm(z, mean = u, sd = 1, log = TRUE)}

mu = seq(-50, 50, by=2)

lnL.mu = sapply(x, logL.fn1, mu)
lnL.sum = apply(lnL.mu, 1, sum)

mle.mu = cbind(mu, lnL.sum)
plot(mu, lnL.sum, type='l', 
main="Log Likelihood Estimate of Mean", 
xlab="Estimate of Mean", ylab="Log Likelihood")
segments(x0=0, y0=min(lnL.sum), x1=0, y1=max(lnL.sum), col='red')
x9 = mu[lnL.sum == max(lnL.sum)]
segments(x0=x9, y0=min(lnL.sum), x1=x9, y1=max(lnL.sum), col='blue')

mu.max = mu[lnL.sum==max(lnL.sum)]
#-----------------------------------


#-----------------------------------
#  Example 6 
#  Data from lognormally distributed population
#  known scale
#  find mu by MLE - but correctly assume lognormally dist
#-----------------------------------

x = rlnorm(n=200, meanlog = 0, sdlog = 1)
hist(x)

logL.fn1 = function(z, u){ln.fz = dlnorm(z, mean = u, sd = 1, log = TRUE)}

mu = seq(-50, 50, by=2)

lnL.mu = sapply(x, logL.fn1, mu)
lnL.sum = apply(lnL.mu, 1, sum)

plot(mu, lnL.sum, type='l', 
main="Log Likelihood Estimate of Mean", 
xlab="Estimate of Mean", ylab="Log Likelihood")
segments(x0=0, y0=min(lnL.sum), x1=0, y1=max(lnL.sum), col='red')
x9 = mu[lnL.sum == max(lnL.sum)]
segments(x0=x9, y0=min(lnL.sum), x1=x9, y1=max(lnL.sum), col='blue')

mu.max = mu[lnL.sum==max(lnL.sum)]
#-----------------------------------


 

#-----------------------------------
#  Example 7 
#  Data from normally distributed population
#  unknown mean and standard deviation
#  find mu and stddev by MLE using matrix of results
#-----------------------------------

x = rnorm(n=375, mean = 100, sd = 10)
#hist(x)

logL.fn1 = function(z, u, s){ln.fz = dnorm(z, mean = u, sd = s, log = TRUE)}

mu.vec = seq(70, 130, by=1)
sig.vec = seq(5, 15, by=1)

lnL.df = as.data.frame(array(data = NA, dim = c(length(mu.vec)*length(sig.vec),6)))
colnames(lnL.df) = c("index","j","i","mu","sig","lnL")

for (j in 1:length(mu.vec))
{
 for (i in 1:length(sig.vec))
 {
 index = i + (j-1)*length(sig.vec)
 lnL.sum = sum(sapply(x, logL.fn1, mu.vec[j], sig.vec[i]))
 lnL.df[index,1] = index
 lnL.df[index,2] = j
 lnL.df[index,3] = i
 lnL.df[index,4] = mu.vec[j]
 lnL.df[index,5] = sig.vec[i]
 lnL.df[index,6] = lnL.sum
 }
}

lnL.min.ind = lnL.df[lnL.df$lnL==min(lnL.df$lnL), 1]
lnL.min.mu = lnL.df[lnL.df$lnL==min(lnL.df$lnL), 4]
lnL.min.sig = lnL.df[lnL.df$lnL==min(lnL.df$lnL), 5]
lnL.min.val = lnL.df[lnL.df$lnL==min(lnL.df$lnL), 6]

lnL.max.ind = lnL.df[lnL.df$lnL==max(lnL.df$lnL), 1]
lnL.max.mu = lnL.df[lnL.df$lnL==max(lnL.df$lnL), 4]
lnL.max.sig = lnL.df[lnL.df$lnL==max(lnL.df$lnL), 5]
lnL.max.val = lnL.df[lnL.df$lnL==max(lnL.df$lnL), 6]

library(scatterplot3d)
help(scatterplot3d)


par(mfrow=c(2,2))

s3d = scatterplot3d(x=lnL.df$mu, y=lnL.df$sig, z=lnL.df$lnL, 
 main="Loglikelihood of Mu and Sigma", xlab="mu", ylab="sigma", zlab="Log Likelihood")
s3d$points3d(lnL.max.mu, lnL.max.sig, lnL.max.value, col="red", type="h", pch=16)

plot(lnL.df$mu, lnL.df$lnL, type='p', main="Log Likelihood Estimate of Mean", 
 xlab="mu", ylab="Log Likelihood")
segments(x0=lnL.max.mu, y0=min(lnL.df$lnL), x1=lnL.max.mu, y1=max(lnL.df$lnL), col='red')

plot(lnL.df$sig, lnL.df$lnL, type='p', main="Log Likelihood Estimate of Sigma", 
 xlab="sigma", ylab="Log Likelihood")
segments(x0=lnL.max.sig, y0=min(lnL.df$lnL), x1=lnL.max.sig, y1=max(lnL.df$lnL), col='red')

#-----------------------------------




#-----------------------------------
#  Example 8 
#  Data from exponentially distributed population
#  known lambda (aka rate)
#  find lambda by MLE 
#-----------------------------------

x = rexp(n=1000, rate=15)

#par(mfrow=c(2,1))
#hist(x.pop)
#hist(x.sample)

logL.fn1 = function(z, scale){ln.fz = dexp(z, scale, log = TRUE)}

lambda = seq(1, 30, by=0.1)

lnL.lambda = sapply(x.sample, logL.fn1, lambda)
lnL.sum = apply(lnL.lambda, 1, sum)

lambda.max = lambda[lnL.sum==max(lnL.sum)]


plot(lambda, lnL.sum, type='l', 
main="Log Likelihood Estimate of lambda (rate)", 
xlab="Estimate of lambda", ylab="Log Likelihood")
segments(x0=15, y0=min(lnL.sum), x1=15, y1=max(lnL.sum), col='red')
x9 = lambda[lnL.sum == max(lnL.sum)]
segments(x0=x9, y0=min(lnL.sum), x1=x9, y1=max(lnL.sum), col='blue')

#-----------------------------------


#-----------------------------------
#  Example 9 
#  Data from exponentially distributed population
#  known lambda (aka rate=15)
#  find scale of Weibull by MLE (should equal 1)
#  with Weibull scale parameter equal to 1/lambda
#-----------------------------------
par(mfrow=c(1,1))
x = rexp(n=1000, rate=15)
hist(x)

logL.fn1 = function(z, shape){ln.fz = dweibull(z, shape, scale = 1/15, log = TRUE)}

wshape = seq(0.1, 2, by=0.1)

lnL.shape = sapply(x, logL.fn1, wshape)
lnL.sum = apply(lnL.shape, 1, sum)


plot(wshape, lnL.sum, type='l', 
main="Log Likelihood Estimate of Shape", 
xlab="Estimate of Shape", ylab="Log Likelihood")
segments(x0=1, y0=min(lnL.sum), x1=1, y1=max(lnL.sum), col='red')
x9 = wshape[lnL.sum == max(lnL.sum)]
segments(x0=x9, y0=min(lnL.sum), x1=x9, y1=max(lnL.sum), col='blue')

wshape.max = wshape[lnL.sum==max(lnL.sum)]
#-----------------------------------


#-----------------------------------
#  Example 10 
#  Data from exponentially distributed population
#  known lambda (aka rate)
#  set Shape of Weibull to equal 1
#  find Weibull Scale parameter (should be equal to exp lambda)
#-----------------------------------

#y = rweibull(n=1000, shape=1, scale=1/15)
x = rexp(n=1000, rate=15)
#par(mfrow=c(2,1))
hist(x)
#hist(y)

logL.fn1 = function(z, scale){ln.fz = dweibull(z, shape=1, scale, log = TRUE)}

wscale = seq(0.01, 1, by=0.001)

lnL.scale = sapply(x, logL.fn1, wscale)
lnL.sum = apply(lnL.scale, 1, sum)

plot(wscale, lnL.sum, type='l', 
main="Log Likelihood Estimate of Scale", 
xlab="Estimate of Scale", ylab="Log Likelihood")
segments(x0=1/15, y0=min(lnL.sum), x1=1/15, y1=max(lnL.sum), col='red')
x9 = wscale[lnL.sum == max(lnL.sum)]
segments(x0=x9, y0=min(lnL.sum), x1=x9, y1=max(lnL.sum), col='blue')

wscale.max = wscale[lnL.sum==max(lnL.sum)]
#-----------------------------------

 

#-----------------------------------
#  Example 11 
#  Data from exponentially distributed population
#  known lambda (aka rate)
#  Take random sample and estimate lambda by MLE 
#  Demonstrate that the MLE is asymptotically normal
#-----------------------------------

#Define a "population" from an Exponential distribution
x.pop = rexp(n=10000, rate=15)

l.max.vec = array(data = NA, dim = 1000, dimnames = NULL)

for (i in 1:1000)
{
x.sample = sample(x.pop, size=100, replace=TRUE)

logL.fn1 = function(z, scale){ln.fz = dexp(z, scale, log = TRUE)}

lambda = seq(1, 30, by=0.1)

lnL.lambda = sapply(x.sample, logL.fn1, lambda)
lnL.sum = apply(lnL.lambda, 1, sum)

lambda.max = lambda[lnL.sum==max(lnL.sum)]
l.max.vec[i] = lambda.max
}

l.max.ordered = sort(l.max.vec, decreasing=FALSE)
lcl.lambda = l.max.ordered[25]
ucl.lambda = l.max.ordered[975]
median.lambda = l.max.ordered[500]
min.lambda = min(l.max.ordered)
max.lambda = max(l.max.ordered)

hist(l.max.vec)
segments(x0=median.lambda, y0=0, x1=median.lambda, y1=300, col="blue")
segments(x0=lcl.lambda, y0=0, x1=lcl.lambda, y1=300, col="red")
segments(x0=ucl.lambda, y0=0, x1=ucl.lambda, y1=300, col="red")

#-----------------------------------


 


