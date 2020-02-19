library(mlt)


NB = 10

get_data_lin = function(){
  x = seq(0,5,length.out = 100)
  y = rnorm(100, x*0.1 + 0.2, 0.05)
  return (list(x=x, y=y, dat=data.frame(x=x, y=y), scale=1))
}

get_data_nx = function(){
  x = seq(0,5,length.out = 100)
  y = rnorm(100, mean=c(x*0.5 + 0.2), 0.05 + 0.2*x)
  return (list(x=x, y=y, dat=data.frame(x=x, y=y), scale=1))
}

get_data_0x = function(){
  x = seq(0,5,length.out = 100)
  y = rnorm(100,  mean=0.2, sd=(0.05 + 0.2*x))
  return (list(x=x, y=y, dat=data.frame(x=x, y=y), scale=1))
}

get_data_sin = function(){
  x = seq(0,5,length.out = 100)
  y = rnorm(100, 0.1*x+0.2+0.15*sin(x*2), 0.05)
  return (list(x=x, y=y, dat=data.frame(x=x, y=y), scale=1))
}

get_data_2l = function(){
  n=500
  x1 = seq(0,5,length.out = n/2)
  x2 = seq(0,5,length.out = n/2)
  y1 = rnorm(n/2, mean=(x1*0.1 + 0.2), 0.05 + 0.1*x)
  y2 = rnorm(n/2, mean=(x2*(-0.5) + 0.2), 0.5  -0.1*x)
  x = c(x1,x2)
  y = c(y1, y2)
  return (list(x=x, y=y, dat=data.frame(x=x, y=y), scale=1))
}


# daten auf 2 geraden 
# -> get_data_2l(), NB=50 und NB=10 sieht recht verschieden aus.

# horizontal, aber varianz wird grösser
# -> get_data_0x()
# -> je flacher die Steigung, desto schwerer wird variierende Varianz gelernt

# coeff interpretieren -> # falls todist=Normal: 
# falls todist=Normal: beta gibt change in h(y) wenn x um 1 unit steigt 

# in mlt probieren dass bernstein-coef auch von x abhängen (interaction?)
# -> mlt vignette page 37.
# Bernstein-Koeffizienten können nur von kategorischen Variablen abhängen (?)
# die als strata Variablen gesetzt werden, siehe argument interacting in ctm

xy_dat = get_data_lin()
xy_dat = get_data_nx()
xy_dat = get_data_sin()
xy_dat = get_data_0x()
xy_dat = get_data_2l()

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
plot(x,y, ylim=c(my_min, my_max))
strech=0.5
n=100  # number of simulated points
for (idx in c(n*0.1,n*0.25,n*0.5,n*0.75,n*0.9)){
  f = preds[,idx]
  lines(strech*f+x[idx],y_grid$y)
  abline(v=x[idx], lty=2)
}


