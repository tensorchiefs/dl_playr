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

### data loaded from db 
offset_beate  = "c:/Users/sick/dl Dropbox/beate sick/IDP_Projekte/DL_Projekte/shared_Oliver_Beate/mlt/UCI_Datasets/"
offset = offset_beate

# get_data = get_data_protein
# path = paste(offset, 'protein-tertiary-structure/', sep="")

# get_data = get_data_boston
# path = paste(offset, 'bostonHousing/', sep="")

get_data = get_data_concrete
path = paste(offset, 'concrete/', sep="")

ret = get_data(path)
str(ret)
(runs = ret$runs)

SCALE = TRUE
reg_factor = 0.0 #Boston 0.05 Protein 0
T_STEPS = 9000 #12000 #w.r.t Batchsize Protein 75000, Boston 50000
bs = -1 # boston -1, protein 128L, energy -1, concrete -1
flds = NULL
#runs = 5
T_OUT = 100 # war auf 500 (zu hoch?)
nb = 10L
len_theta = nb + 1L
spatz = 0.0
x_scale = FALSE

# grid_reg_factor = c(0.0, 0.025, 0.05)
# grid_spatz= c(0.0, 0.01, 0.05)
# grid_x_scale = c(FALSE, TRUE)

grid_reg_factor = c(0.0, 0.03)
grid_spatz= c(0.0, 0.05)
grid_x_scale = c(FALSE, TRUE)

hist_grid= make_hist_grid()
history = make_hist()

runs = runs
runs = min(6,runs)  #  !! nur zum testen klein setzen
## !!!! warning: must have inverted order then for-loop below !!
param_matrix = expand.grid(no_step=1:floor(T_STEPS/T_OUT),
                           no_fold=1:runs,
                           regularization=grid_reg_factor, 
                           spatz=grid_spatz,
                           x_scale=grid_x_scale)
( k_max = nrow(param_matrix) )

k=1
for (x_scale in grid_x_scale){        # grid loop 
for (spatz in grid_spatz){            # grid loop 
for (reg_factor in grid_reg_factor){  # grid loop 
  print(paste0("durchgang hyperparameter-kombi k :" , k, "von :" , k_max ))
  k=k+1
  # reg_factor = reg_factor[1]
   for (run in 1:runs){ # loop over folds (=runs = splits)
    # run =1
    ret = get_data(path, split_num=run, spatz = spatz, x_scale=x_scale)
    train_x = ret$X_train
    x_train = train_x[1:round(0.8*nrow(train_x)),]
    x_val = train_x[(round(0.8*nrow(train_x))+1):nrow(train_x),]
    train_y= ret$y_train
    y_train = train_y[1:round(0.8*nrow(train_x))]
    y_val = train_y[(round(0.8*nrow(train_x))+1):nrow(train_x)]
    print(paste0("fold" , run, ' from ', runs))
    x_train1 = tf$Variable(x_train, dtype='float32')
    x_test = tf$Variable(x_val, dtype='float32')
    y_train1 = tf$reshape(tf$Variable(y_train, dtype='float32'), c(-1L,1L))
    y_test = tf$reshape(tf$Variable(y_val, dtype='float32'), c(-1L,1L))

    print(paste0('training ', x_train1$shape, ' testing ', x_test$shape))
    
    source('model_7.R')
    x_dim = ncol(x_test)
    model_7 = new_model_7(len_theta = len_theta, x_dim = x_dim, y_range=ret$scale, 
                          eta_term = TRUE, a_term = TRUE,reg_factor = reg_factor, bs = bs)
    model_7$name = paste0('model_7_', ret$name)
    history = model_train(model_7, history, x_train1, y_train1,x_test, y_test, T_STEPS = T_STEPS)
    print('dim(history) :')
    print(dim(history))
    for (i in 1:20){
      ret = model_get_p_y(model_7, x_train1[i,,drop=FALSE], 0, 1, 100)
      #print(paste0(i, '  ',round(sum(ret$p_y)/100,3)))
      plot(ret$y, ret$p_y, main=paste0(i,' train ', round(sum(ret$p_y)/300,3)))
    }
  }
}}} # close grid loops


history = cbind(history[-1,], param_matrix)  
hist_grid = history
str(hist_grid)
 
 ind <- apply(hist_grid, 1, function(x) any(is.na(x)))
 hist_grid <- hist_grid[ !ind, ]

 hist_grid$step = as.numeric(hist_grid$step)
 hist_grid$fold = as.factor(hist_grid$fold)
 hist_grid$nll_train = as.numeric(hist_grid$nll_train)
 hist_grid$nll_test = as.numeric(hist_grid$nll_test)
 hist_grid$regularization = as.factor(hist_grid$regularization)
 hist_grid$spatz = as.factor(hist_grid$spatz)
 hist_grid$x_scale = as.factor(hist_grid$x_scale)
 
 # hist_grid_cp2 = hist_grid
 # hist_grid =hist_grid_cp2

 summary(hist_grid)
 
 xtabs(~spatz+x_scale, data=hist_grid)
 xtabs(~spatz+regularization, data=hist_grid)
 
( out_path = paste0(path,Sys.Date(),round(rnorm(1,mean=30,sd=5),3),'_','grid_search/') )
 (out_name = paste0(out_path,'history_',T_STEPS,'.Rdata'))
 dir.create(out_path)
 save(hist_grid, file = out_name)
