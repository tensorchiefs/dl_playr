#rm(list=ls())
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
source('get_data_UCI.R')

get_data = get_data_boston
path = '/Users/oli/Dropbox/__ZHAW/__Projekte_Post_ZHAH/shared_Oliver_Beate/mlt/UCI_Datasets/bostonHousing/'

get_data = get_data_protein
path = '/Users/oli/Dropbox/__ZHAW/__Projekte_Post_ZHAH/shared_Oliver_Beate/mlt/UCI_Datasets/protein-tertiary-structure/'

#get_data = get_data_proteins
#ret = get_data(path = '/Users/oli/Dropbox/__ZHAW/__Projekte_Post_ZHAH/shared_Oliver_Beate/mlt/UCI_Datasets/bostonHousing/')
#ret = get_data(path = '/Users/oli/Dropbox/__ZHAW/__Projekte_Post_ZHAH/shared_Oliver_Beate/mlt/UCI_Datasets/protein-tertiary-structure/')

SCALE = TRUE
reg_factor = 0.05 #Boston 0.05 Protein 0
T_STEPS = 500 #w.r.t Batchsize Protein 75000, Boston 50000
bs = 128L
flds = NULL
runs = 5
T_OUT = 100
nb = 8L
len_theta = nb + 1L

#Main Loop############ 
#
#run = 4 s
history = make_hist()
ret = get_data(path)
runs = ret$runs
for (run in 1:runs){ #<----------------
  # run =1
  ret = get_data(path, split_num=run, spatz = 0.05)
    
  print(paste0("Run ", run, ' from ', runs))
  x_train1 = tf$Variable(ret$X_train, dtype='float32')
  x_test = tf$Variable(ret$X_test, dtype='float32')
  y_train1 = tf$reshape(tf$Variable(ret$y_train, dtype='float32'), c(-1L,1L))
  y_test = tf$reshape(tf$Variable(ret$y_test, dtype='float32'), c(-1L,1L))
  print(paste0('training ', x_train1$shape, ' testing ', x_test$shape))
  
  
  source('model_7.R')
  x_dim = ncol(x_test)
  model_7 = new_model_7(len_theta = len_theta, x_dim = x_dim, y_range=ret$scale, eta_term = TRUE, a_term = TRUE,reg_factor = -1, bs = bs)
  model_7$name = paste0('model_7_reg_',reg_factor, '_', ret$name)
  history = model_train(model_7, history, x_train1, y_train1,x_test, y_test, T_STEPS = T_STEPS)
  
  for (i in 1:20){
    ret = model_get_p_y(model_7, x_train1[i,,drop=FALSE], 0, 1, 100)
    print(paste0(i, '  ',round(sum(ret$p_y)/100,3)))
    plot(ret$y, ret$p_y, main=paste0(i,' train ', round(sum(ret$p_y)/300,3)))
  }

}


out = paste0(path,Sys.Date(),round(rnorm(1,mean=30,sd=5),3),'/')
dir.create(out)
save(history, file = paste0(out,'history_',T_STEPS,'.Rdata'))
