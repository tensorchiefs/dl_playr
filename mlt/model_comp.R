rm(list=ls())
library(MASS)
library(ggplot2)
library(mlt)
library(basefun)
library(keras)
library(tensorflow)
library(tfprobability)

# source('~/Documents/workspace/dl_playr/mlt/bern_utils.R')
#source('https://raw.githubusercontent.com/tensorchiefs/dl_playr/master/mlt/bern_utils.R')
source('bern_utils.R')
source("model_utils.R")
source('data.R')


T_STEPS = 300
runs = 5
flds = NULL
T_OUT = 100
nb = 20L
len_theta = nb + 1L


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
history = make_hist()
for (run in 1:runs){ #<----------------
  # run =1
  print(run)
  d = get_data_boston()
  x=d$x
  x_dim = as.integer(dim(x)[2])
  y=d$y
  s=d$scale
  
 
  idx_train = flds[[run]]
  idx_test = setdiff(1:nrow(x), idx_train)
  
  x_train1 = tf$Variable(x[idx_train,], dtype='float32')
  x_test = tf$Variable(x[idx_test,], dtype='float32')
  y_train1 = tf$Variable(y[idx_train,,drop=FALSE], dtype='float32')
  y_test = tf$Variable(y[idx_test,,drop=FALSE], dtype='float32')
  
  source('model_1.R')
  #--- For the mlt (still a bit unnice)
  # uses the global variables idx_train and idx_test 
  datt = d$dat
  datt$y = y[,1]
  history = model_train(history, NULL, NULL) #Call model_train from last sourced model

  rm(x,y,d) #For savety
  source('model_2.R')
  model_2 = new_model_2(len_theta = len_theta, x_dim = x_dim, y_range=s)
  history = model_train(model_2, history, x_train1, y_train1, x_test, y_test, T_STEPS = T_STEPS) #Call model_train from last sourced model
  print(model_test(model_2, x_test, y_test))

  source('model_3.R')
  model_3 = new_model_3(len_theta = len_theta, x_dim = x_dim, y_range=s)
  history = model_train(model_3, history, x_train1, y_train1,x_test, y_test, T_STEPS = T_STEPS) #Call model_train from last sourced model
  print(model_test(model_3, x_test, y_test))

  #Model 4 is model 3 with real network instead of linear regression
  source('model_4.R')
  model_4 = new_model_4(len_theta = len_theta, x_dim = x_dim, y_range=s)
  history = model_train(model_4, history, x_train1, y_train1,x_test, y_test, T_STEPS = T_STEPS) #Call model_train from last sourced model
  print(model_test(model_4, x_test, y_test))
  
  source('model_5.R')
  model_5 = new_model_5(len_theta = len_theta, x_dim = x_dim, y_range=s)
  history = model_train(model_5, history, x_train1, y_train1,x_test, y_test, T_STEPS = T_STEPS)
  print(model_test(model_5, x_test, y_test))
  
  source('model_5.R')
  reg_factor = 0.05
  model_5_reg = new_model_5(len_theta = len_theta, x_dim = x_dim, y_range=s, reg_factor = reg_factor)
  model_5_reg$name = paste0('model_5_reg', reg_factor)
  history = model_train(model_5_reg, history, x_train1, y_train1,x_test, y_test, T_STEPS = T_STEPS) 
  print(model_test(model_5_reg,x_test, y_test))
  
  source('model_5.R')
  model_6 = new_model_5(len_theta = len_theta, x_dim = x_dim, y_range=s, reg_factor = reg_factor, is_theta_x = TRUE)
  model_6$name = paste0('model_6_reg', reg_factor)
  history = model_train(model_6, history, x_train1, y_train1,x_test, y_test, T_STEPS = T_STEPS) 
  print(model_test(model_6,x_test, y_test))

}

history = history[-1,]
save(history, file = paste0('./runs/boston_cv_history_',T_STEPS,'.Rdata'))

# print(model_test(x_test, y_test))
# df1 = data.frame(ind=NA,int=NA)
# model_hy$summary()
# for (i in 10:50){
#   out_row = model_hy(x_test[i,, drop=FALSE]) #Pick row and compute CPD
#   df = bernp.p_y(my_bernp, out_row, from = 0, to = 1, length.out = 1000)# pred_dens(model, i, dat, beta_dist_h, beta_dist_h_dash, p=p)
#   print(i)
#   print(sum(df$p_y)/1000)
#   df1 = rbind(df1, data.frame(ind = i, int = sum(df$p_y)/1000))
#   plot(df[,1:2], type='l', ylim=c(min(df$h),max(df$h)))
#   lines(df$y, df$h)
# }
# df1 = df1[2:nrow(df1),]
# hist(df1$int)
