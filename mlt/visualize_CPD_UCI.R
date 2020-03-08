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
offset_oliver = '/Users/oli/Dropbox/__ZHAW/__Projekte_Post_ZHAH/shared_Oliver_Beate/mlt/UCI_Datasets/'
offset_beate  = "c:/Users/sick/dl Dropbox/beate sick/IDP_Projekte/DL_Projekte/shared_Oliver_Beate/mlt/UCI_Datasets/"
offset = offset_beate

# get_data = get_data_boston
# path = paste(offset, 'bostonHousing/', sep="")
# name_tmp = "boston"

# get_data = get_data_protein
# path = paste(offset, 'protein-tertiary-structure/', sep="")

# get_data = get_data_wine
# path = paste(offset, 'wine-quality-red/', sep="")
# name_tmp = "wine"


# get_data = get_data_energy
# path = paste(offset, 'energy/', sep="")
# name_tmp = "energy"

get_data = get_data_yacht
path = paste(offset, 'yacht/', sep="")
name_tmp = "yacht"

# get test data for which we predict CPD
dat = get_data(path, split_num=19, spatz = 0, x_scale=TRUE)
str(dat)

x_test = tf$Variable(dat$X_test, dtype='float32')
y_test = tf$reshape(tf$Variable(dat$y_test, dtype='float32'), c(-1L,1L))

# set arguments for model_7 for which we then load weights
x_dim = ncol(x_test)
(runs = dat$runs)

SCALE = TRUE
reg_factor = 0.01
T_STEPS =  12000 #12000 #w.r.t Batchsize Protein 75000, Boston 50000
bs = -1 # boston -1, protein 128L, energy -1
flds = NULL
T_OUT = 100 # war auf 500 (zu hoch?)
nb = 10L
len_theta = nb + 1L
spatz = 0.0
x_scale = TRUE


source('model_7.R')

model_7 = new_model_7(len_theta = len_theta, x_dim = x_dim, y_range=dat$scale, 
                      eta_term = TRUE, a_term = TRUE,reg_factor = reg_factor, bs = bs)

# Loading of a saved model

(out_path = path = paste0(offset, 'out_models/'))

model_7$model = load_model_hdf5(paste0(out_path, name_tmp, '_model_7_.hfd5'))
model_7$model_g = load_model_hdf5(paste0(out_path,name_tmp, '_model_7_g.hfd5'))
model_7$model_s = load_model_hdf5(paste0(out_path,name_tmp, '_model_7_s.hfd5'))
model_7$model_beta = load_model_hdf5(paste0(out_path,name_tmp, '_model_7_beta.hfd5'))
model_7$model_a = load_model_hdf5(paste0(out_path,name_tmp, '_model_7_a.hfd5'))

model_test(model_7, x_test, y_test) 

(out_cpd = paste0(out_path,'plot_cpd/',name_tmp,'/'))

for (i in 1:20){
  # i=4
  ret = model_get_p_y(model_7, x_test[i,,drop=FALSE], 0, 1, 100)
  jpeg(paste0(out_cpd,name_tmp,'_fold_19_obs_',i,'_CPD.jpg'))
  plot(ret$y, ret$p_y, type="l",
       main=paste0(i,' test ', round(sum(ret$p_y)/100,3)))
  dev.off()
}

str(dat)




