## Data set
get_data_boston = function () {
  data("BostonHousing2", package = "mlbench")
  dat=BostonHousing2
  dat$town = NULL
  dat$cmedv = NULL #Remove second version of 
  dat$chas = as.numeric(as.character(dat$chas))
  #str(dat)  #506 obs. of  19 variables
  names(dat)
  scale = max(dat$medv) - min(dat$medv)
  #dat$y_obs = dat$medv
  dat$y = utils_scale(dat$medv)
  dat$medv = NULL
  names(dat)
  #MLT has problems with int
  dat$tract = as.numeric(dat$tract)
  dat$rad = as.numeric(dat$rad)
  dat$tax = as.numeric(dat$tax)
  dat$b = as.numeric(dat$b)
  
  
  y = as.matrix(dat$y)
  datx = dat[,4:(ncol(dat)-1)]
  print(paste0('Names in X : ',names(datx)))
  x = as.matrix(datx) #<------ Here ist y dabei gewesen!!!!
  x = scale(x, center = TRUE, scale = TRUE)
  #rm(dat)
  #x is now data-matrix
  #y is repsone matrix
  return (list(x=x,y=y,dat=dat,scale=scale))
}

# d = get_data_boston()
# str(d$dat)
