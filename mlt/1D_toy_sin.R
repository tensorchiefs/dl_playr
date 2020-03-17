## Code for creating figure 1 

library(mlt)
set.seed(1)

NB = 10 
num_data <- 100
int_steps = 500 #Steps for the integration
y_start = -0.2
y_end = 1.0
get_data_sin = function(){
  x = seq(0,5,length.out = num_data)
  #y = rnorm(1500, 0.1*x+0.2+0.15*sin(x*2), 0.005 + 0.01 * x) #For a normal distribution
 
  y = 0.1*x+0.2+0.15*sin(x*2) + (0.01 + 0.015 * x)*rexp(num_data, 1) #For a exponential distribution
  return (list(x=x, y=y, dat=data.frame(x=x, y=y), scale=1))
}

get_data_sin_dist = function(x, y_in){
  #return(dnorm(y_in,0.1*x+0.2+0.15*sin(x*2), 0.005 + 0.01 * x)) #For a normal distribution
  offset = 0.1*x+0.2+0.15*sin(x*2)
  scale =  (0.01 + 0.015 * x)
  return(dexp((y_in - offset)/scale,1)/scale)
}

xy_dat = get_data_sin()
x = xy_dat$x
y = xy_dat$y
dat=xy_dat$dat

delta=abs(diff(range(y)))/4
my_min = min(y)-delta
my_max = max(y)+delta

# MLT
library(mlt)
#library(basefun)
fm = y ~ x
var_y <- numeric_var("y", support = c(y_start, y_end))
bb <- Bernstein_basis(var_y, order=NB, ui="increasing")
y_grid <- as.data.frame(mkgrid(bb, n = int_steps))
# set up model for mlt
ctm = ctm(bb, shift=fm[-2L], data=dat, todistr="Normal") 
mlt_fit <- mlt(ctm, data = dat, verbose=TRUE)
(logLik_mlt = logLik(mlt_fit)/ nrow(dat))
mlt_fit$coef

preds = predict(mlt_fit, newdata=dat,  q=y_grid$y, type='density')
plot(x,y, ylim=c(my_min, my_max), col='grey')
strech=0.25
n=length(y)  # number of simulated points
for (idx in c(1, n*0.1,n*0.25,n*0.5,n*0.75,n*0.9)){
  f = preds[,idx]
  lines(strech*f+x[idx],y_grid$y)
  abline(v=x[idx], lty=2)
}


library(keras)
library(tensorflow)
library(tfprobability)
source('bern_utils.R')
source("model_utils.R")
source('data.R')
# MLT Network Model 
xx = tf$Variable(as.matrix(x, rcols=1), dtype='float32')
yy = tf$Variable(as.matrix(y, rcols=1), dtype='float32')

len_theta = as.integer(NB + 1L)
T_OUT = 100
run = 1
history = make_hist()

source('model_7.R')
model_7 = new_model_7(len_theta = len_theta, x_dim = 1, y_range=1, eta_term = TRUE, a_term = TRUE)
model_7$name = 'model_7_with3rd'
history = model_train(model_7, history, xx, yy,xx, yy, T_STEPS = 15000) 

########
# Creating the plot


NLLS = 0
#--------- Creating the plot
#pdf("1D_toy_2l.pdf", width=1.41*7, height=7) 
#plot(x,y, ylim=c(my_min, my_max),col='grey', xlim=c(0,5.5))
plot(x,y,col='grey', xlim=c(0,5.5), ylim=c(-0.2,1.2))
#streches = c(0.1, 0.1, 0.1)
#for (i in c(n*0.010,n*0.25,n*0.5,n*0.75,n*0.9)){
cc = 0
#for (i in c(1000,1001,1002,1003,1004,1005)){#c(n*0.01,n*0.35,n*0.9)){
ret_dl = NULL
for (i in c(n*0.01,n*0.5,n*0.95)){
  cc = cc + 1
  strech = 0.1#streches[cc]
  NLL = model_test(model_7,xx[i,,drop=FALSE],yy[i,,drop=FALSE])
  print(NLL)
  NLLS = NLLS + NLL
  
  
 
  ret = model_get_p_y(model_7, xx[i,,drop=FALSE], y_start, y_end, int_steps)
  f = ret$p_y
  ret$dl_mlt = f
  #lines(strech*f+x[i],ret$y,col='red')
  lines(strech*f+x[i],ret$y,col='black')
  abline(v=x[i], lty=2)
  print(paste0(i, '  ',round(sum(ret$p_y)/(int_steps/(y_end - y_start)),3)))
  f = preds[,i]
  ret$mlt = f
  lines(strech*f+x[i],y_grid$y, lty=2, col='black')
  
  if (TRUE){
    f = get_data_sin_dist(x[i], ret$y)
    lines(strech*f+x[i],ret$y, lty=1, col='green')
  }
  ret$gen = f
  ret$x = x[i]
  ret_dl = rbind(ret_dl, ret)
}
#dev.off()



head(ret_dl)
library(tidyr)
ret_models_sin = gather(ret_dl, 'method', 'p_y', c(mlt, dl_mlt, gen))
head(ret_models_sin)
dat_sin = dat
save(dat_sin, ret_models_sin, file='1D_Toy_sin.rdata')



