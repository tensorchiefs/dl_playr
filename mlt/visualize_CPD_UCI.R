# !! pfad und arguments of fit anpassen !!!

#rm(list=ls())
library(MASS)
library(ggplot2)
library(cowplot)
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
# ( path = paste0(offset, 'out_models/save_with_run_ID/boston/2020-03-0723.914_test_run/', sep="") )
# ( path_test_data = paste0(offset,'bostonHousing/') )
# name_tmp = "boston"
# dir(path)  # should contain hdf5 files
# dir(path_test_data) # should contain subdir "data"
# 


# get_data = get_data_concrete
# #( path = paste0(offset, 'out_models/save_with_run_ID/concrete/2020-03-0823.553_test_run/', sep="") )
# ( path = paste0(offset, 'out_models/save_with_run_ID/concrete/2020-03-0830.066_test_run/', sep="") )
# 
# ( path_test_data = paste0(offset,'concrete/') )
# name_tmp = "concrete"
# dir(path)  # should contain hdf5 files
# dir(path_test_data) # should contain subdir "data"


# get_data = get_data_energy
# (path = paste0(offset, 'out_models/save_with_run_ID/energy/2020-03-0833.266_test_run/', sep="") )
# #(path = paste0(offset, 'out_models/save_with_run_ID/energy/2020-03-0729.071_test_run/', sep="") )
# 
# ( path_test_data = paste0(offset,'energy/') )
# name_tmp = "energy"
# dir(path)  # should contain hdf5 files
# dir(path_test_data) # should contain subdir "data"


get_data = get_data_naval
( path = paste0(offset, 'out_models/save_with_run_ID/naval/2020-03-0732.245_test_run/', sep="") )
( path_test_data = paste0(offset,'naval-propulsion-plant/') )
name_tmp = "naval"
dir(path)  # should contain hdf5 files
dir(path_test_data) # should contain subdir "data"



# get_data = get_data_wine
# #( path = paste0(offset, 'out_models/save_with_run_ID/wine/2020-03-0723.67_test_run/', sep="") )
# # ( path = paste0(offset, 'out_models/save_with_run_ID/wine/2020-03-0827.921_test_run/', sep="") )
# (path = paste0(offset, 'out_models/save_with_run_ID/wine/2020-03-0932.525_test_run/', sep="") )
# # ( path = paste0(offset, 'out_models/save_with_run_ID/wine/2020-03-0926.808_test_run/', sep="") )
# 
# ( path_test_data = paste0(offset,'wine-quality-red/') )
# name_tmp = "wine"
# dir(path)  # should contain hdf5 files
# dir(path_test_data) # should contain subdir "data"

# get_data = get_data_protein
# (path_test_data = paste(offset, 'protein-tertiary-structure', sep=""))
# ( path = paste0(offset, 'out_models/save_with_run_ID/protein/2020-03-0727.913_test_run', sep="") )

# name_tmp = "protein"
# dir(path)  # should contain hdf5 files
# dir(path_test_data) # should contain subdir "data"


# get_data = get_data_yacht
# #( path = paste(offset, 'out_models/save_with_run_ID/yacht/2020-03-0723.67_test_run/', sep="") )
# ( path = paste(offset, 'out_models/save_with_run_ID/yacht/2020-03-0834.954_test_run/', sep="") )
# ( path_test_data = paste0(offset,'yacht/') )
# name_tmp = "yacht"
# dir(path)  # should contain hdf5 files
# dir(path_test_data) # should contain subdir "data"

# get test data from run 19 for which we predict CPD
####################################################
my_num_splits = 19   # folds-1
# my_num_splits = 4
dat = get_data(path_test_data, split_num=my_num_splits, spatz = 0, x_scale=TRUE)
str(dat)

x_test = tf$Variable(dat$X_test, dtype='float32')
y_test = tf$reshape(tf$Variable(dat$y_test, dtype='float32'), c(-1L,1L))

# ! set arguments for model_7 for which we then load weights
(x_dim = ncol(x_test))
(runs = dat$runs)

## cp this block form R-script
SCALE = TRUE
reg_factor = 0.0
T_STEPS = 120000 #12000 #
bs = 256L # boston -1, protein 128L, energy -1
flds = NULL
T_OUT = 100 # war auf 500 (zu hoch?)
nb = 10L
len_theta = nb + 1L
spatz = 0.0
x_scale = TRUE

###########


source('model_7.R')

model_7 = new_model_7(len_theta = len_theta, x_dim = x_dim, y_range=dat$scale, 
                      eta_term = TRUE, a_term = TRUE,reg_factor = reg_factor, bs = bs)

# ignore: Error in on_load() :... 

# Loading of last a saved model

# (out_path = path = paste0(offset, 'out_models/'))

model_7$model = load_model_hdf5(paste0(path, name_tmp, '_model_7_.hfd5'))
model_7$model_g = load_model_hdf5(paste0(path,name_tmp, '_model_7_g.hfd5'))
model_7$model_s = load_model_hdf5(paste0(path,name_tmp, '_model_7_s.hfd5'))
model_7$model_beta = load_model_hdf5(paste0(path,name_tmp, '_model_7_beta.hfd5'))
model_7$model_a = load_model_hdf5(paste0(path,name_tmp, '_model_7_a.hfd5'))

# model_7$model = load_model_hdf5(paste0(path,  'model_7_.hfd5'))
# model_7$model_g = load_model_hdf5(paste0(path, 'model_7_g.hfd5'))
# model_7$model_s = load_model_hdf5(paste0(path, 'model_7_s.hfd5'))
# model_7$model_beta = load_model_hdf5(paste0(path, 'model_7_beta.hfd5'))
# model_7$model_a = load_model_hdf5(paste0(path, 'model_7_a.hfd5'))


model_test(model_7, x_test, y_test) 

#(out_cpd = paste0(out_path,'plot_cpd/',name_tmp,'/'))

str(dat)

( path_cpd = paste0(path,'plots_cpd_trafo/') )
dir.create(path_cpd)

# collect integral of CPD of all test
##############################################

area_cpd = data.frame(area=rep(-1, times=length(y_test)))

for (i in 1:length(y_test)){
  # i=4
  no_points=300
  my_from = -0.5
  my_to = 1.5
  ret = model_get_p_y(model_7, x_test[i,,drop=FALSE], 
                      from=my_from, to=my_to, length.out=no_points)
  # str(ret)  # we have 100 points
  #no_points = dim(ret)[1]
  area_cpd[i,"area"] = round((my_to-my_from)*sum(ret$p_y)/no_points,3)
}


cut= 0.98
( no_cut = sum(area_cpd$area<cut) )
( p_cut = sum(area_cpd$area<cut)/length(y_test) )

cut2= 0.95
( no_cut2 = sum(area_cpd$area<cut2) )
( p_cut2 = sum(area_cpd$area<cut2)/length(y_test) )

hist_area=ggplot(data=area_cpd, aes(area)) +
  geom_histogram(col="black", 
                 fill="skyblue", 
                 alpha = .2) + 
  coord_cartesian(xlim=c(0,1)) +
  labs(title=paste0('in ',length(y_test),' predicted test CPDs are ', 
                 no_cut,' below ', cut, ' (and ',no_cut2,' below ', cut2,')'),
       subtitle=paste0('this is a fraction of ', round(p_cut,2), 
                       ' (and ', round(p_cut2,2),')'))
hist_area
ggsave(paste0(path_cpd,'hist_area_CPDs_',name_tmp,'_fold_19_test.jpg'), 
       plot = last_plot(), width = 8, height = 3.5)

# draw plots with cpd, trafo, hist for the first no_bsp test data
###############################################################

(no_bsp = min(length(y_test), 40))

for (i in 1:no_bsp){
  # i=4
  ret = model_get_p_y(model_7, x_test[i,,drop=FALSE], my_from, my_to, no_points)
  integral = round((my_to-my_from)*sum(ret$p_y)/no_points,3)
  title_tmp = paste0(dat$name,'_run_19_obs_', i, '_integral_', integral)
  p_dens = ggplot(data=ret, aes(x = y, y = p_y)) +
    geom_line(lwd=1.2) + 
    ggtitle(title_tmp)

  p_trafo = ggplot(data=ret, aes(x = y, y = h)) +
    geom_line(lwd=1) + 
    ggtitle("trafo")

  hist_y1=ggplot(data=ret, aes(y_tilde)) +
    geom_histogram(col="black", 
                   fill="skyblue", 
                   alpha = .2) + 
    ggtitle('input to sigmoid (=g_x*y-s_x)')
  
  hist_y2=ggplot(data=ret, aes(y_tilde_2)) +
    geom_histogram(col="black", 
                   fill="skyblue", 
                   alpha = .2) + 
    coord_cartesian(xlim=c(0,1)) +
    ggtitle('input to Bern-fct (=sigmoid(g_x*y-s_x))')
  
  plot_grid(p_dens, p_trafo, hist_y1, hist_y2, nrow=2)
  
  ggsave(paste0(path_cpd,name_tmp,'_fold_19_obs_',i,'_CPD_trafo.jpg'), 
         plot = last_plot(), width = 14, height = 5)
}




