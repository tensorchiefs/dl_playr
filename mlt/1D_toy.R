library(mlt)


NB = 15

get_data_lin = function(sigma = 0.05){
  x = seq(0,5,length.out = 100)
  y = rnorm(100, x*0.1 + 0.2, sigma)
  return (list(x=x, y=y, dat=data.frame(x=x, y=y), scale=1))
}

# No slope and noise gets larger
get_data_nx = function(){
  x = seq(0,5,length.out = 500)
  y = rnorm(500, mean=c(x*0.1 + 0.2), 0.005 + 0.04*x)
  return (list(x=x, y=y, dat=data.frame(x=x, y=y), scale=1))
}


# No slope the noise justs gets larger
get_data_0x = function(){
  x = seq(0,5,length.out = 500)
  y = rnorm(500,  mean=0.5, sd=(0.01 + 0.05*x))
  return (list(x=x, y=y, dat=data.frame(x=x, y=y), scale=1))
}

get_data_sin = function(){
  x = seq(0,5,length.out = 100)
  y = rnorm(100, 0.1*x+0.2+0.15*sin(x*2), 0.05)
  return (list(x=x, y=y, dat=data.frame(x=x, y=y), scale=1))
}

get_data_2l = function(){ #daten auf 2 geraden 
  n=500
  x1 = seq(0,5,length.out = n/2)
  x2 = seq(0,5,length.out = n/2)
  y1 = rnorm(n/2, mean=(x1*0.1 + 0.2), 0.05 + 0.05*x)
  y2 = rnorm(n/2, mean=(x2*(-0.5) + 0.2), 0.25  - 0.04*x)
  x = c(x1,x2)
  y = c(y1, y2)
  y = y / 5 + 0.5
  return (list(x=x, y=y, dat=data.frame(x=x, y=y), scale=1))
}


# daten auf 2 geraden 
# -> get_data_2l(), NB=50 und NB=10 sieht recht verschieden aus.

# horizontal, aber varianz wird gr?sser
# -> get_data_0x()
# -> je flacher die Steigung, desto schwerer wird variierende Varianz gelernt

# coeff interpretieren -> # falls todist=Normal: 
# falls todist=Normal: beta gibt change in h(y) wenn x um 1 unit steigt 

# in mlt probieren dass bernstein-coef auch von x abh?ngen (interaction?)
# -> mlt vignette page 37.
# Bernstein-Koeffizienten k?nnen nur von kategorischen Variablen abh?ngen (?)
# die als strata Variablen gesetzt werden, siehe argument interacting in ctm





xy_dat = get_data_sin()

xy_dat = get_data_lin(sigma = 0.01)
xy_dat = get_data_0x()
xy_dat = get_data_nx() 
xy_dat = get_data_2l()

max(xy_dat$y)
min(xy_dat$y)
x = xy_dat$x
y = xy_dat$y
dat=xy_dat$dat

delta=abs(diff(range(y)))/4
my_min = min(y)-delta
my_max = max(y)+delta

plot(x,y, ylim=c(my_min, my_max))
# LM
fm = y ~ x
fit_lm = lm(fm, data=dat)
abline(fit_lm)
fit_lm$coef
(NLL_LM = logLik(fit_lm) / nrow(dat))

# MLT
library(mlt)
#library(basefun)
var_y <- numeric_var("y", support = c(my_min, my_max))
bb <- Bernstein_basis(var_y, order=NB, ui="increasing")
y_grid <- as.data.frame(mkgrid(bb, n = 500))
# set up model for mlt
ctm = ctm(bb, shift=fm[-2L], data=dat, todistr="Normal") 
mlt_fit <- mlt(ctm, data = dat, verbose=TRUE)
(logLik_mlt = logLik(mlt_fit)/ nrow(dat))
mlt_fit$coef

preds = predict(mlt_fit, newdata=dat,  q=y_grid$y, type='density')
plot(x,y, ylim=c(my_min, my_max), col='grey')
strech=0.25
n=length(y)  # number of simulated points
for (idx in c(n*0.1,n*0.25,n*0.5,n*0.75,n*0.9)){
  f = preds[,idx]
  lines(strech*f+x[idx],y_grid$y)
  abline(v=x[idx], lty=2)
}


# MLT Network Model 
xx = tf$Variable(as.matrix(x, rcols=1), dtype='float32')
yy = tf$Variable(as.matrix(y, rcols=1), dtype='float32')

library(tensorflow)
library(tfprobability)
library(keras)
source('bern_utils.R')
source("model_utils.R")
source('data.R')
source('model_7.R')
len_theta = as.integer(NB + 1L)
T_OUT = 100
run = 1
history = make_hist()

model_7 = new_model_7(len_theta = len_theta, x_dim = 1, y_range=1, eta_term = TRUE)
history = model_train(model_7, history, xx, yy,xx, yy, T_STEPS = 15000) 
model_test(model_7,xx,yy)

NLLS = 0
plot(x,y, ylim=c(my_min, my_max),col='grey')
strech = 0.1
for (i in c(n*0.1,n*0.25,n*0.5,n*0.75,n*0.9)){
  NLL = model_test(model_7,xx[i,,drop=FALSE],yy[i,,drop=FALSE])
  NLLS = NLLS + NLL
  int_steps = 100
  ret = model_get_p_y(model_7, xx[i,,drop=FALSE], 0, 1, int_steps)
  print(paste0(i, '  ',round(sum(ret$p_y)/int_steps,3)))
  
  #Plot Single Predictions
  #plot(ret$y, -log(ret$p_y), main=paste0(i,' train Int=', round(sum(ret$p_y)/int_steps,3), ' NLL=', round(NLL,3)))
  #plot(ret$y, ret$p_y, main=paste0(i,' train Int=', round(sum(ret$p_y)/int_steps,3), ' NLL=', round(NLL,3)))
  #abline(v = as.numeric(yy[i,]))
  
  f = ret$p_y
  lines(strech*f+x[i],ret$y)
  abline(v=x[i], lty=2)
  #abline(v=x[idx], lty=2)
  
}
NLLS / 100


