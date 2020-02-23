#Just to check the output of an untrained NN
x_dim = 10
X = matrix(rnorm(x_dim*100), ncol = x_dim)

model <- keras_model_sequential() 
model %>% 
  layer_dense(units=(10), input_shape = c(x_dim), activation = 'tanh') %>% 
  layer_dense(units=(100), activation = 'tanh') %>% 
  layer_dense(units=(100), activation = 'tanh') %>% 
  layer_dense(units=(100), activation = 'tanh') %>% 
  layer_dense(units=len_theta) %>% 
  layer_activation('linear') 

hist(as.numeric(model(X)), 100)
