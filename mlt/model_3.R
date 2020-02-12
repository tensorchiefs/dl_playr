# need to implemnt
#   model_train(history)
#   model_test
optimizer = tf$keras$optimizers$Adam(learning_rate=0.0001)
model_hy <- keras_model_sequential() 
model_hy %>% 
  layer_dense(units=(10), input_shape = c(1), activation = 'relu') %>% 
  #layer_dense(units=(100), activation = 'relu') %>% 
  layer_dense(units=(nb+1)) %>% 
  layer_activation('linear') 
summary(model_hy)

#summary(model_hy)
model_beta <- keras_model_sequential() 
model_beta %>% 
  layer_dense(1, activation='linear', input_shape = c(ncol(x_test)), use_bias=FALSE)
summary(model_beta)

ones = k_ones(c(y_train$shape[start_index],1))#tf$Variable(as.matrix(dat$rm)[,drop=FALSE], dtype='float32')

ones$shape
my_bernp = bernp(len_theta = len_theta)
tilde_theta_im = model_hy(ones)
beta_x = model_beta(x_train)
bernp.nll(my_bernp, out_bern = tilde_theta_im, y = y_train, out_eta = beta_x,y_range=s)

train_step = function(x, y, model_hy, model_beta){
  with(tf$GradientTape() %as% tape, {
    tilde_theta_im = model_hy(ones)
    beta_x = model_beta(x)
    NLL = bernp.nll(my_bernp, out_bern = tilde_theta_im, y = y, out_eta = beta_x, y_range=s)
  })
  tvars = list(model_hy$trainable_variables, model_beta$trainable_variables)
  grads = tape$gradient(NLL, tvars)
  optimizer$apply_gradients(
    purrr::transpose(list(grads[[1]], tvars[[1]]))
  )
  optimizer$apply_gradients(
    purrr::transpose(list(grads[[2]], tvars[[2]]))
  )
  return(NLL)
}

train_step_au = tf_function(train_step)
model_train = function(history, save_model = FALSE){
  start_time = Sys.time()
  for (r in 1:T_STEPS){
    l  = train_step_au(x=x_train, y=y_train, model_hy=model_hy,model_beta=model_beta )  
    if (r %% T_OUT == 0){
      tilde_theta_im = model_hy(k_ones(c(y_test$shape[start_index],1)))
      beta_x = model_beta(x_test)
      nll = bernp.nll(my_bernp, out_bern = tilde_theta_im, y = y_test, out_eta = beta_x, y_range=s) 
      
      print(paste(r, 'model_3: likelihood (in optimize) ' ,l$numpy() , 'likelihood (in test) ',nll$numpy()))
      history = rbind(history, c(r, run, l$numpy() , nll$numpy(), 'model_3'))
    } 
  }
  end_time = Sys.time() 
  
  if (save_model) {
    save_model_hdf5(model_hy, paste0('boston_cv_model3_hy_F',run,'_Steps_',T_STEPS))
    save_model_hdf5(model_beta, paste0('boston_cv_model3_beta_F',run,'_Steps_',T_STEPS))
  }
  return(history)
}

model_test = function(x_test, y_test){
  tilde_theta_im = model_hy(k_ones(c(y_test$shape[start_index],1)))
  beta_x = model_beta(x_test)
  nll = bernp.nll(my_bernp, out_bern = tilde_theta_im, y = y_test, out_eta = beta_x, y_range=s) 
  return(nll$numpy())
}
