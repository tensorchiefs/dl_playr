NB = 10

get_data_lin = function(){
  x = seq(0,5,length.out = 100)
  y = rnorm(100, x*0.1 + 0.2, 0.05)
  return (list(x=x, y=y, dat=data.frame(x=x, y=y), scale=1))
}

get_data_nx = function(){
  x = seq(0,5,length.out = 100)
  y = rnorm(100, x*0.1 + 0.2, 0.05 + 0.02*x)
  return (list(x=x, y=y, dat=data.frame(x=x, y=y), scale=1))
}

get_data_sin = function(){
  x = seq(0,5,length.out = 100)
  y = rnorm(100, 0.1*x+0.2+0.15*sin(x*2), 0.05)
  return (list(x=x, y=y, dat=data.frame(x=x, y=y), scale=1))
}



xy_dat = get_data_lin()
xy_dat = get_data_nx()
xy_dat = get_data_sin()

x = xy_dat$x
y = xy_dat$y
dat=xy_dat$dat
plot(x,y)

# LM
fm = y ~ x
fit_lm = lm(fm, data=dat)
fit_lm$coef
(NLL_LM = logLik(fit_lm) / nrow(dat))

# MLT
library(mlt)
#library(basefun)
var_y <- numeric_var("y", support = c(0, 1))
bb <- Bernstein_basis(var_y, order=NB, ui="increasing")
y_grid <- as.data.frame(mkgrid(bb, n = 500))
# set up model for mlt
ctm = ctm(bb, shift=fm[-2L], data=dat, todistr="Normal") 
mlt_fit <- mlt(ctm, data = dat, verbose=TRUE)
(logLik_mlt = logLik(mlt_fit)/ nrow(dat))
mlt_fit$coef

preds = predict(mlt_fit, newdata=dat,  q=y_grid$y, type='density')
plot(x,y)
for (idx in c(1,25,50,75,90)){
  f = preds[,idx]
  lines(f/10+x[idx],y_grid$y)
}
