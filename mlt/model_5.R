optimizer = tf$keras$optimizers$Adam(learning_rate=0.0001)

get_string = function(){
  return ("Model_5 with ")
}

## Model for the coefficients
model_hy <- keras_model_sequential() 
model_hy %>% 
  #layer_dense(units=(10), input_shape = c(1), activation = 'tanh') %>% 
  layer_dense(units=(nb+1),input_shape = c(1)) %>% 
  layer_activation('linear') 
summary(model_hy)

model_beta <- keras_model_sequential() 
model_beta %>% 
  layer_dense(10, activation='tanh', input_shape = c(ncol(x_train))) %>% 
  # layer_dense(50, activation='tanh') %>% 
  # layer_dense(10, activation='tanh') %>% 
  layer_dense(5, activation='tanh',kernel_regularizer = regularizer_l2(2.05)) %>% 
  layer_dense(1, activation='linear', kernel_regularizer = regularizer_l2(2.05)) 
summary(model_beta)

#summary(model_hy)
model_gamma <- keras_model_sequential() 
model_gamma %>% 
  layer_dense(10, activation='tanh', input_shape = c(ncol(x_train)), kernel_regularizer = regularizer_l2(2.05)) %>% 
  # layer_dense(50, activation='tanh') %>%
  # layer_dense(10, activation='tanh') %>% 
  layer_dense(5, activation='tanh', kernel_regularizer = regularizer_l2(2.05)) %>% 
  layer_dense(1, activation='linear') 
summary(model_gamma)

ones = k_ones(c(y_train$shape[start_index],1))#tf$Variable(as.matrix(dat$rm)[,drop=FALSE], dtype='float32')
my_bernp = bernp(len_theta = len_theta)
tilde_theta_im = model_hy(ones)
beta_x = model_beta(x_train)
gamma_x = model_gamma(x_train)

nll_model5 = function(bernp, out_bern, y, out_eta , out_gamma) {
  theta_im = to_theta(out_bern)
  hy = eval_h(theta_im, y_i = y, beta_dist_h = bernp$beta_dist_h)
  sp = tf$math$softplus(out_gamma[,1])
  z =  sp * hy -  out_eta[,1]
  h_y_dash = sp * eval_h_dash(theta_im, y, beta_dist_h_dash = bernp$beta_dist_h_dash)
  return(-tf$math$reduce_mean(bernp$stdnorm$log_prob(z) + tf$math$log(h_y_dash)))
}

add_squarred_weights_penalty = function(ls, NLL, f=0.05){
  for (l in ls){
    #NLL = NLL + f*tf$reduce_sum(tf$math$square(l))
    NLL = NLL + f*tf$reduce_sum(tf$math$abs(l))
  }
  return(NLL)
}

train_step = function(x, y, model_hy, model_beta, model_gamma){
  with(tf$GradientTape() %as% tape, {
    tilde_theta_im = model_hy(ones)
    beta_x = model_beta(x)
    gamma_x = model_gamma(x)
    NLL = nll_model5(my_bernp, out_bern = tilde_theta_im, y = y_train, out_eta = beta_x, out_gamma=gamma_x)
    NLL = add_squarred_weights_penalty(model_beta$trainable_variables, NLL) 
    NLL = add_squarred_weights_penalty(model_gamma$trainable_variables, NLL)
  })
  tvars = list(model_hy$trainable_variables, model_beta$trainable_variables, model_gamma$trainable_variables)
  grads = tape$gradient(NLL, tvars)
  optimizer$apply_gradients(
    purrr::transpose(list(grads[[1]], tvars[[1]]))
  )
  optimizer$apply_gradients(
    purrr::transpose(list(grads[[2]], tvars[[2]]))
  )
  optimizer$apply_gradients(
    purrr::transpose(list(grads[[3]], tvars[[3]]))
  )
  return(NLL)
}

train_step_au = tf_function(train_step) 

model_train = function(history, save_model = FALSE){
  start_time = Sys.time()
  for (r in 1:T_STEPS){
    l  = train_step_au(x=x_train, y=y_train, model_hy=model_hy,
                       model_beta=model_beta, model_gamma=model_gamma )  
    if (r %% T_OUT == 0){
      
      tilde_theta_im = model_hy(k_ones(c(y_test$shape[start_index],1)))
      beta_x = model_beta(x_test)
      gamma_x = model_gamma(x_test)
      nll = nll_model5(my_bernp, out_bern = tilde_theta_im, y = y_test, out_eta = beta_x, out_gamma = gamma_x) +  log(s)
      
      print(paste(r, 'likelihood (in optimize) ' ,l$numpy() , 'likelihood (in test) ',nll$numpy()))
      history = rbind(history, c(r, run, l$numpy() , nll$numpy(), 'model_5'))
    } 
  }
  end_time = Sys.time() 
  
  if (save_model){
    save_model_hdf5(model_hy, paste0('boston_cv_model5_hy_F',run,'_Steps_',T_STEPS))
    save_model_hdf5(model_beta, paste0('boston_cv_model5_beta_F',run,'_Steps_',T_STEPS))
    save_model_hdf5(model_gamma, paste0('boston_cv_model5_gamma_F',run,'_Steps_',T_STEPS))
  }
  return(history)
}

model_test = function(x_test, y_test){
  tilde_theta_im = model_hy(k_ones(c(y_test$shape[start_index],1)))
  beta_x = model_beta(x_test)
  gamma_x = model_gamma(x_test)
  nll = nll_model5(my_bernp, out_bern = tilde_theta_im, y = y_test, out_eta = beta_x, out_gamma = gamma_x) + log(s)
  return (nll$numpy())
}

