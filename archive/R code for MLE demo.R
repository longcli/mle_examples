# MLE


###############################################################################


# real distribution -----------------------------------------------------------

mu.actual = 100
sd.actual = 10

set.seed(1)

x.actual = rnorm(n = 30, mean = mu.actual, sd = sd.actual)


x.axis.min = mu.actual - 5*sd.actual
x.axis.max = mu.actual + 5*sd.actual


# # for visual effect only - mostly don't use 
# x.range = max.x.actual - min.x.actual
# ssize = 10
# x.actual = seq(min.x.actual, max.x.actual, by = x.range/ssize)


x.pdf.actual = dnorm(x.actual, mean = mu.actual, sd = sd.actual)

x.pdf.lines = cbind(x.actual, 0)
y.pdf.lines = cbind(x.actual, x.pdf.actual)


# calculate MLE for actual distribution
mle.actual = sum(dnorm(x = x.actual, mean = mu.actual, sd = sd.actual, log = TRUE))


# create data for plot of actual distribution
plot(density(rnorm(n = 100000, mean = mu.actual, sd = sd.actual)), 
     main = paste("MLE = ", round(mle.actual,2)))
# add some features
rug(x.actual)
segments(x0 = x.pdf.lines[,1], y0 = x.pdf.lines[,2], 
         x1 = y.pdf.lines[,1], y1 = y.pdf.lines[,2], 
         lty = 3, col = 'blue')
points(y.pdf.lines, col = 'red')




# hypothesized distribution (NORMAL) ------------------------------------------

# guess at parameters
mu.guess = 120
sd.guess = 5


min.x.guess = mu.guess - 5*sd.guess
max.x.guess = mu.guess + 5*sd.guess


xmin.plot = min(x.axis.min, min.x.guess)
xmax.plot = max(x.axis.max, max.x.guess)


x.pdf.guess = dnorm(x.actual, mean = mu.guess, sd = sd.guess)

x.pdf.lines = cbind(x.actual, 0)
y.pdf.lines = cbind(x.actual, x.pdf.guess)


mle.guess = sum(dnorm(x = x.actual, mean = mu.guess, sd = sd.guess, log = TRUE))


# plot of hypothesized distribution
plot(density(rnorm(n = 100000, mean = mu.guess, sd = sd.guess)), 
     xlim = c(xmin.plot, xmax.plot), 
     main = paste("MLE = ", round(mle.guess,2)))
# add some features
rug(x.actual)
segments(x0 = x.pdf.lines[,1], y0 = x.pdf.lines[,2], 
         x1 = y.pdf.lines[,1], y1 = y.pdf.lines[,2], 
         lty = 3, col = 'blue')
points(y.pdf.lines, col = 'red')


# contour plot for MLE --------------------------------------------------------

# create min and max values of mu and sd for mle contour plot
mu.surface.min = 0.8*mu.actual
mu.surface.max = 1.22*mu.actual
mu.surface.range = mu.surface.max - mu.surface.min
mu.surface.vec = seq(mu.surface.min, mu.surface.max, by = mu.surface.range/100)


sd.surface.min = 0.8*sd.actual
sd.surface.max = 2.2*sd.actual
sd.surface.range = sd.surface.max - sd.surface.min
sd.surface.vec = seq(sd.surface.min, sd.surface.max, by = sd.surface.range/100)



# function to calculate MLE over a vector of parameters -----------------------

mle.fn = function(n.param){
  sum(dnorm(x = x.actual, mean = n.param[1], sd = n.param[2], log = TRUE))
}  # end fn


# plot of MLE for mu only -----------------------------------------------------
# n.param is a two-element vector of mu and sd

nparams = cbind(mu.surface.vec, sd.actual)  # here, only mu varies
# head(nparams)


mle.vec = apply(nparams, MARGIN = 1, FUN = mle.fn)
max.mle = max(mle.vec)
max.mle.mu = mu.surface.vec[mle.vec == max.mle]


plot(mu.surface.vec, mle.vec, type = 'l', xlab = 'Hypothesized mu', ylab = 'Maximum Likelihood')
title('Maximum Likelihood as Hypothesized mu varies')
points(max.mle.mu, max.mle, col = 'red')
abline(v = max.mle.mu, col = 'red', lty = 2)



# plot of MLE for sd only -----------------------------------------------------
# n.param is a two-element vector of mu and sd

nparams = cbind(mu.actual, sd.surface.vec)  # here, only sd varies
# head(nparams)


mle.vec = apply(nparams, MARGIN = 1, FUN = mle.fn)
max.mle = max(mle.vec)
max.mle.sd = sd.surface.vec[mle.vec == max.mle]


plot(sd.surface.vec, mle.vec, type = 'l', xlab = 'Hypothesized mu', ylab = 'Maximum Likelihood')
title('Maximum Likelihood as Hypothesized mu varies')
points(max.mle.sd, max.mle, col = 'red')
abline(v = max.mle.sd, col = 'red', lty = 2)



# plot of MLE for both mu and sd ----------------------------------------------
# n.param is a two-element vector of mu and sd

nparams = data.frame(expand.grid(mu.surface.vec, sd.surface.vec))
names(nparams) = c('mu.vec', 'sd.vec')
# head(nparams); tail(nparams)


mle.vec = apply(nparams, MARGIN = 1, FUN = mle.fn)

mle.comb = cbind(nparams, mle.vec)
# head(mle.comb); tail(mle.comb)


max.mle = max(mle.vec)
max.mle.comb = mle.comb[mle.vec == max.mle,]


library(lattice)

wireframe(mle.vec ~ mu.vec*sd.vec, data = mle.comb,
          drape = TRUE,
          colorkey = TRUE,
          screen = list(z = -60, x = -60))



library(rgl)

max.mle.seg = t(cbind(t(max.mle.comb), c(max.mle.comb[,1:2],min(mle.vec))))

with(mle.comb, plot3d(mu.vec, sd.vec, mle.vec, col="blue", size=3, xlab = 'mu', ylab = 'stdev', zlab = 'MLE'))
points3d(max.mle.comb, col = 'red', size = 10)
lines3d(max.mle.seg, col = 'red')



# convert mle df from long to wide
# http://www.cookbook-r.com/Manipulating_data/Converting_data_between_wide_and_long_format/#from-long-to-wide-1


# change from 'long' to 'wide' format
# make mu = rows, sd = columns, matrix cells = mle value
library(reshape2)

mle.comb.wide = dcast(mle.comb, mu.vec ~ sd.vec, value.var = 'mle.vec')

m.mle.comb.wide = as.matrix(mle.comb.wide[,-1])  # drops the first column with mu.vec


# from rgl package
persp3d(x = mu.surface.vec, y = sd.surface.vec, z = m.mle.comb.wide)



# from plot3D package
library(plot3D)
# http://cran.r-project.org/web/packages/plot3D/vignettes/volcano.pdf

# note that 'persp3D' is similar to 'persp3d' but a different package

persp3D(x = mu.surface.vec, y = sd.surface.vec, z = m.mle.comb.wide, 
        #contour = list(side = c("zmax", "z")), 
        contour = list(side = c("zmin", "z")), 
        colkey = list(length = 0.5, shift = -0.1),
        phi = 30, theta = 20, d = 10, box = TRUE,
        xlab = 'mu', ylab = 'sd', zlab = 'mle')






# END CODE ####################################################################