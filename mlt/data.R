## Data set
get_data_boston = function (scale_x=FALSE, idx_train = NULL) {
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
  if (scale_x) {
    if (is.null(idx_train)) {
      x = scale(x, center = TRUE, scale = TRUE)  
    } else{
      m = apply(x[idx_train,], 2, mean)
      s = apply(x[idx_train,], 2, sd)
      x = (x - m) / (s)
      print("-----")
      print(apply(x, 2, mean))
      print(apply(x, 2, sd))
    }
  }
  #x = scale(x, center = TRUE, scale = TRUE)
  #rm(dat)
  #x is now data-matrix
  #y is repsone matrix
  return (list(x=x,y=y,dat=dat,scale=scale))
}


get_data_proteins = function(scale_x = TRUE, idx_train=NULL){
  data <- read.table("~/Documents/workspace/dl_playr/mlt/data/data.txt", quote="\"", comment.char="")
  y = data$V10
  y = as.matrix(y)
  x = as.matrix(data[,1:9])
  scale = max(y) - min(y)
  y = utils_scale(y)
  if (scale_x){
    x = scale(x, center = TRUE, scale = TRUE)
    if (is.null(idx_train)) {
      x = scale(x, center = TRUE, scale = TRUE)  
    } else{
      m = apply(x[idx_train,], 2, mean)
      s = apply(x[idx_train,], 2, sd)
      x = (x - m) / (s)
    }
  }
  dat = data.frame(x)
  dat$y = y
  return (list(x=x,y=y,dat=dat,scale=scale))
}


# d = get_data_boston()
# str(d$dat)
