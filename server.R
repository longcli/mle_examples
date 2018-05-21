

library(tidyverse)

 

#-----------------------------------
#  Example 1
#  known standard deviation
#  find mu by MLE
#-----------------------------------

mu_sample <- 100
sd_sample <- 10

xs = rnorm(77, mean = mu_sample, sd = sd_sample)

hist(x)

logL_fn1 = function(z, u){
  ln_fz = dnorm(z, mean = u, sd = 10, log = TRUE)
  return(ln_fz)
}


# ALTERNATE FUNCTION
#ln.fz = -log(sqrt(2*pi)) - log(sigma) - (0.5/(sigma^2))*(z - u)^2


# # BASE R APPROACH -----------------------------------------------------------
# mu_L <- seq(50, 150, by = 2)
# lnL_mu_L = sapply(xs, logL_fn1, mu_L)
# lnL_sum = apply(lnL_mu_L, 1, sum)
# 
# plot(mu_L, lnL_sum, type='l', 
#      main="Log Likelihood Estimate of Mean", 
#      xlab="Estimate of Mean", ylab="Log Likelihood")
# segments(x0=100, y0=min(lnL.sum), x1=100, y1=max(lnL.sum), col='red')



# TIDY R APPROACH -------------------------------------------------------------

mu_L <- seq(50, 150, by = 2)
sd_L <- sd_sample

df_L <- as.data.frame(expand.grid(mu_sample, sd_sample, mu_L, sd_L, xs)) %>% 
  rename(mu_sample = Var1, sd_sample = Var2, mu_L = Var3, sd_L = Var4, xs = Var5) %>% 
  arrange(xs, mu_L, mu_sample, sd_sample, xs) %>% 
  # calculate loglikelihood for each observation xs in each mu_L
  mutate(lnL_mu_L = logL_fn1(xs, mu_L))


with(df_L, table(mu_L))


# loglikelihood for each mu_L
df_L_sum <- df_L %>% 
  group_by(mu_sample, sd_sample, mu_L, sd_L) %>% 
  # puts sample data and logL data into nested df
  nest() %>% 
  group_by(mu_sample, sd_sample, mu_L, sd_L) %>% 
  # sum of all loglikelihood values within each mu_L
  mutate(lnL_sum = map(data, ~ sum(.x$lnL_mu_L)))



plot_lnL_fn <- function(df_fn){
  
  # df_fn <- df_L_sum
  # glimpse(df_fn)
  # df_fn$lnL_sum
  
  muL_max <- df_fn[lnL_sum == max(lnL_sum), 'mu_L']
  lnL_mu_max <- df_fn[lnL_sum == max(lnL_sum), 'lnL_sum']
  lnL_mu_min <- df_fn[lnL_sum == min(lnL_sum), 'lnL_sum']
  
  
  plot_mu <- ggplot(df_fn, aes(x = mu_L, y = unlist(lnL_sum))) +
    geom_line() + 
    geom_segment(x = muL_max[[1]], y = unlist(lnL_mu_min[[1]]), 
                 xend = muL_max[[1]], yend = unlist(lnL_mu_max[[1]]), 
                 color = 'red', linetype = 2) + 
    ggtitle("Plot of loglikelihood for each value of mu_L", 
            subtitle = paste("Max LogL occurs at mu = ", muL_max)) + 
    ylab('Value of Loglikelihood') + 
    xlab('Value of mu_L')
  
  return(plot_mu)
  
}


plot_obj1 <- plot_lnL_fn(df_L_sum)
plot(plot_obj1)





# plot for specific mu and sd ------

xmin1 <- mu_sample - 4*sd_sample
# xmin2 <- 100 - 4*sd_sample
# xmin <- min(xmin1, xmin2)
xmin <- xmin1

xmax1 = mu_sample + 4*sd_sample
# xmax2 = 100 + 4*sd_sample
# xmax <- max(xmax1, xmax2)
xmax <- xmax1


xq <- seq(xmin, xmax, by = 0.1)
xp <- dnorm(x = xq, mean = mu_sample, sd = sd_sample)

d_sample_dist <- data.frame(cbind(xq, xp))


# select value of mu and sd from loglikelihood fittings
# fmuL <- 80
# fsdL <- 10

mle_params <- c(80, 10)  # mean, stdev

fml_xs <- dnorm(x = xs, mean = mle_params[1], sd = mle_params[2])
fml_L <- dnorm(x = xs, mean = mle_params[1], sd = mle_params[2], log = TRUE)

d_plot_wlnL <- data.frame(cbind(xs, fml_xs, fml_L))


# plot using Base R -----------------------------------------------------------

# sum_lnL <- sum(dnorm(x = xs, mean = fmuL, sd = fsdL, log = TRUE))
# 
# plot(xq, xp, type = 'l')
# points(x = xs, y = rep(0, length(xs)), col='blue')
# points(x = xs, y = fml_xs, col='red')
# segments(x0 = xs, y0 = rep(0, length(xs)), x1 = xs, y1 = fml_xs, lty = "dotted")



# plot using ggplot2 function -------------------------------------------------

plot_dist_lnL_fn <- function(df_sample_fn, df_fn2, ml_params_fn){

  # TEST ONLY
  # df_sample_fn <- d_sample_dist
  # df_fn2 <- d_plot_wlnL
  # ml_params_fn <- mle_params
  
  # df_sample with xq and xp is the sample distribution
  # df_fn2 with xs and fml_xs is the likelihood distribution
  
  L_sum <- round(sum(df_fn2$fml_L), 2)
  
  
  # use this to create distribution based on likelihood parameters
  df_xq_w_Lparms <- df_sample_fn %>% 
    select(xq) %>% 
    mutate(y_distL = dnorm(xq, mean = ml_params_fn[1], sd = ml_params_fn[2]))
  
  
  plot_dist <- ggplot() +
    # plot sample distribution
    geom_line(data = df_sample_fn, aes(x = xq, y = xp, color = 'actual parameters')) +  # 'gray'
    # plot fit based on likelihood parameters 
    geom_line(data = df_xq_w_Lparms, aes(x = xq, y = y_distL, color = 'likelihood parameters'), size = 1) +  # 'steelblue'
    # add rug plot for sample data
    geom_rug(data = df_fn2, aes(x = xs)) + 
    # add vertical line segment and point where xs intersects likelihood distribution
    geom_point(data = df_fn2, aes(x = xs, y = fml_xs), color = 'red') + 
    geom_segment(data = df_fn2, aes(x = xs, y = 0, xend = xs, yend = fml_xs, color = 'actual parameters'), linetype = 2) + 
    # titles and stuff
    ggtitle("Plot of Sample Distribution with Likelihood Assumptions", 
            subtitle = paste("Sum of Likelihood values = ", L_sum)) +
    ylab('Distribution') + 
    xlab('Sample x') + 
    theme(legend.position = "top") + 
    scale_colour_manual(name="Distribution",
                        values=c('actual parameters' = "gray", 
                                 'likelihood parameters' = "steelblue")) 
    
  ?theme
  return(plot_dist)
  
}

?geom_segment

plot_obj2 <- plot_dist_lnL_fn(d_sample_dist, d_plot_wlnL, mle_params)
plot(plot_obj2)


# library(plotly)
plotly_obj2 <- ggplotly(plot_obj2 + 
                          ggtitle(paste("Plot of Sample Distribution with 
                                        Likelihood Assumptions <br> Sum of 
                                        Likelihood values = ", L_sum)), 
                        tooltip = "y")


library(gridExtra)

gridExtra::grid.arrange(
  plot_obj1, 
  plot_obj2, 
  nrow = 1
)


library(cowplot)

plot_grid(plot_obj1, plot_obj2)



#  Example 2 ------------------------------------------------------------------
#  known mu
#  find standard deviation by MLE
# 
# 
# x = rnorm(1000, mean=100, sd=10)
# 
# logL.fn2 = function(z, sigma){ln.fz = dnorm(z, mean = 100, sd = sigma, log = TRUE)}
# 
# sig = 50:200/10
# 
# lnL.sigma = sapply(x, logL.fn2, sig)
# lnL.sum = apply(lnL.sigma, 1, sum)
# 
# plot(sig, lnL.sum, type='l', 
# main="Log Likelihood Estimate of Variance", 
# xlab="Estimate of StdDev", ylab="Log Likelihood")
# segments(x0=10, y0=min(lnL.sum), x1=10, y1=max(lnL.sum), col='red')
# 
# sd.max = sig[lnL.sum==max(lnL.sum)]
# segments(x0=sd.max, y0=min(lnL.sum), x1=sd.max, y1=max(lnL.sum), col='blue')




 

#  Example 7 ------------------------------------------------------------------
#  Data from normally distributed population
#  unknown mean and standard deviation
#  find mu and stddev by MLE using matrix of results

# x = rnorm(n=375, mean = 100, sd = 10)
# #hist(x)
# 
# logL.fn1 = function(z, u, s){ln.fz = dnorm(z, mean = u, sd = s, log = TRUE)}
# 
# mu.vec = seq(70, 130, by=1)
# sig.vec = seq(5, 15, by=1)
# 
# lnL.df = as.data.frame(array(data = NA, dim = c(length(mu.vec)*length(sig.vec),6)))
# colnames(lnL.df) = c("index","j","i","mu","sig","lnL")
# 
# for (j in 1:length(mu.vec))
# {
#  for (i in 1:length(sig.vec))
#  {
#  index = i + (j-1)*length(sig.vec)
#  lnL.sum = sum(sapply(x, logL.fn1, mu.vec[j], sig.vec[i]))
#  lnL.df[index,1] = index
#  lnL.df[index,2] = j
#  lnL.df[index,3] = i
#  lnL.df[index,4] = mu.vec[j]
#  lnL.df[index,5] = sig.vec[i]
#  lnL.df[index,6] = lnL.sum
#  }
# }
# 
# lnL.min.ind = lnL.df[lnL.df$lnL==min(lnL.df$lnL), 1]
# lnL.min.mu = lnL.df[lnL.df$lnL==min(lnL.df$lnL), 4]
# lnL.min.sig = lnL.df[lnL.df$lnL==min(lnL.df$lnL), 5]
# lnL.min.val = lnL.df[lnL.df$lnL==min(lnL.df$lnL), 6]
# 
# lnL.max.ind = lnL.df[lnL.df$lnL==max(lnL.df$lnL), 1]
# lnL.max.mu = lnL.df[lnL.df$lnL==max(lnL.df$lnL), 4]
# lnL.max.sig = lnL.df[lnL.df$lnL==max(lnL.df$lnL), 5]
# lnL.max.val = lnL.df[lnL.df$lnL==max(lnL.df$lnL), 6]
# 
# library(scatterplot3d)
# help(scatterplot3d)
# 
# 
# par(mfrow=c(2,2))
# 
# s3d = scatterplot3d(x=lnL.df$mu, y=lnL.df$sig, z=lnL.df$lnL, 
#  main="Loglikelihood of Mu and Sigma", xlab="mu", ylab="sigma", zlab="Log Likelihood")
# s3d$points3d(lnL.max.mu, lnL.max.sig, lnL.max.value, col="red", type="h", pch=16)
# 
# plot(lnL.df$mu, lnL.df$lnL, type='p', main="Log Likelihood Estimate of Mean", 
#  xlab="mu", ylab="Log Likelihood")
# segments(x0=lnL.max.mu, y0=min(lnL.df$lnL), x1=lnL.max.mu, y1=max(lnL.df$lnL), col='red')
# 
# plot(lnL.df$sig, lnL.df$lnL, type='p', main="Log Likelihood Estimate of Sigma", 
#  xlab="sigma", ylab="Log Likelihood")
# segments(x0=lnL.max.sig, y0=min(lnL.df$lnL), x1=lnL.max.sig, y1=max(lnL.df$lnL), col='red')






 


