rm(list=ls())
library(MASS)
library(ggplot2)
library(mlt)
library(basefun)
library(keras)
library(tensorflow)
library(tfprobability)

# source('~/Documents/workspace/dl_playr/mlt/bern_utils.R')
#source('bern_utils.R')
source('https://raw.githubusercontent.com/tensorchiefs/dl_playr/master/mlt/bern_utils.R')

windows = TRUE
T_STEPS = 500
runs = 2
flds = NULL
T_OUT = 100
nb = 8
len_theta = nb + 1

if(windows){
  start_index=1
} else {
  start_index=0
}

## Data set
get_data_boston = function () {
  data("BostonHousing2", package = "mlbench")
  dat=BostonHousing2
  #str(dat)  #506 obs. of  19 variables
  names(dat)
  scale = max(dat$medv) - min(dat$medv)
  dat$cmedv = NULL #Remove second version of 
  #dat$y_obs = dat$medv
  dat$y = utils_scale(dat$medv)
  dat$medv = NULL
  names(dat)
  y = as.matrix(dat$y)
  x = as.matrix(dat[,5:ncol(dat)]) #<------ Here ist y dabei!!!!!!!
  datx = dat[,5:(ncol(dat)-1)]
  print(paste0('Names in X : ',names(datx)))
  x = as.matrix(datx) #<------ Here ist y dabei gewesen!!!!
  #rm(dat)
  #x is now data-matrix
  #y is repsone matrix
  return (list(x=x,y=y,dat=dat,scale=scale))
}


# Creation of folds ###
#
if (is.null(flds)) {
  require(caret)
  set.seed(42)
  d = get_data_boston()
  # create list with k entries, each holding the indices of the train data in the kth fold
  flds <- createFolds(d$y, k = runs, list = TRUE, returnTrain = TRUE)  
  # save the list with indices
  save(x=flds, file=paste0('./runs/boston_',runs,'_folds.rdata'))
}

#Main Loop############ 
#
#run = 4 s

# prepare data.frrame where we collect results in each step
history = data.frame(matrix(NA, nrow = 1, ncol=4))
names(history) = c('step', 'fold', 'nll_train', 'nll_test')
history$method = 'NA'
history.row = 1

for (run in 1:runs){ #<----------------
  # run =1
  print(run)
  d = get_data_boston()
  x=d$x
  y=d$y
  s=d$scale
  datt = d$dat
  datt$y = y[,1]
  
  idx_train = flds[[run]]
  idx_test = setdiff(1:nrow(x), idx_train)
  
  x_train = tf$Variable(x[idx_train,], dtype='float32')
  x_test = tf$Variable(x[idx_test,], dtype='float32')
  y_train = tf$Variable(y[idx_train,,drop=FALSE], dtype='float32')
  y_test = tf$Variable(y[idx_test,,drop=FALSE], dtype='float32')
  rm(x,y,d) #For savety
  
  source('model_5.R')
  history = model_train(history)
  print(model_test(x_test, y_test))
}

history = history[-1,]
write.table(x = history, file = paste0('./runs/boston_cv_history',T_STEPS,'.csv'), row.names = FALSE, sep=';')

