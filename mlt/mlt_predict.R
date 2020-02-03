
library(sloop)
library(keras)
library(tensorflow)
library(tfprobability)
source('mlt_utils.R')

#Private and complete constructor
new_mltnet = function(len_theta = integer()){
  stopifnot(is.integer(len_theta))
  stopifnot(len_theta > 0)
  structure( 
    class = "mltnet",
    len_theta,
    beta_dist_h = init_beta_dist_for_h(len_theta),
    beta_dist_h_dash = init_beta_dist_for_h_dash(len_theta)
    )  
}

# Constructor 
mltnet = function(len_theta=integer()){
  new_mltnet(as.integer(len_theta))
}

# Implementation of generic methods
print.mltnet = function(x) {
  print('Hallo')
}

# Registration of generics


#d = new_mltnet(3) #not OK
d = mltnet(3)
sloop::s3_dispatch(print(d))
print(d)
