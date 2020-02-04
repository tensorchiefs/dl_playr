library(keras)
library(tensorflow)
tf_version() #needs to be > 2.0
tf$executing_eagerly() #Need to be true

#### Tensorflow core
(x = matrix(rnorm(rep(0,3*4)),nrow = 3, ncol=4))
(kc  = k_constant(x)) # turns x to tensor (not trainable)
(kt = k_variable(x))  # turns x to tf.variable (traninable)
(ka = keras_array(x) )  # turns x to numpy-style array
class(ka) #"numpy.ndarray"

#### Keras mode
model <- keras_model_sequential() 
model %>% 
  layer_dense(units=(10), input_shape = c(4), activation = 'tanh') %>% 
  layer_dense(units=(2)) %>% 
  layer_activation('linear') 
model$summary()
model$predict(x) #Should return a 3x2 matrix
model$predict(kc) 

library(tfprobability)
tfp_version() #needs to be at least >= 0.7 better =0.8
tfd_normal(loc=0,scale=1)$prob(0) #Should be tensor with tf.Tensor(0.3989423, shape=(), dtype=float32)
