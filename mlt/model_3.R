############################################################################
# This model is a NN implementation of the 'MLT linear transformation model'
# need to implemnt
#   model_train(history)
#   model_test


###############################
# Baseline transformation gives (after piping through to_theta) the coefficients for the bernstein polynoms
.make_model_hy = function(len_theta) {
  model_hy <- keras_model_sequential() 
  model_hy %>% 
    layer_dense(units=(10), input_shape = c(1), activation = 'relu') %>% 
    #layer_dense(units=(100), activation = 'relu') %>% 
    layer_dense(units=len_theta) %>% 
    layer_activation('linear') 
  return(model_hy)
}

###############################
# Linear shift term gives coefficients for the linear predictor (beta)
.make_model_beta = function(x_dim){
  model_beta <- keras_model_sequential() 
  model_beta %>% 
    layer_dense(1, activation='linear', input_shape = x_dim, use_bias=FALSE)
  return(model_beta)
}

#"Private" and complete constructor. This constructor verifies the input.
new_model_3 = function(len_theta = integer(), x_dim, y_range){
  stopifnot(is.integer(len_theta))
  stopifnot(len_theta > 0)
  structure( #strcutur is a bunch of data with 
    list( #bunch of data
      len_theta=len_theta,  
      x_dim = x_dim,
      optimizer = tf$keras$optimizers$Adam(learning_rate=0.0001),
      model_beta = .make_model_beta(x_dim),
      model_hy = .make_model_hy(len_theta),
      bernp = make_bernp(len_theta),
      y_range = y_range,
      name = 'model_3',
      ones = -42 #Will be set in training
    ),
    class = "model_3" #class attribute set so it's a class
  )  
}

train_step_3 = function(x, y, model){
  model_hy = model$model_hy
  model_beta = model$model_beta
  with(tf$GradientTape() %as% tape, {
    tilde_theta_im = model_hy(model$ones)
    beta_x = model_beta(x)
    NLL = bernp.nll(model$bernp, out_bern = tilde_theta_im, y = y, out_eta = beta_x, y_range=model$y_range)
  })
  tvars = list(model_hy$trainable_variables, model_beta$trainable_variables)
  grads = tape$gradient(NLL, tvars)
  model$optimizer$apply_gradients(
    purrr::transpose(list(grads[[1]], tvars[[1]]))
  )
  model$optimizer$apply_gradients(
    purrr::transpose(list(grads[[2]], tvars[[2]]))
  )
  return(NLL)
}

train_step_au = tf_function(train_step_3)

#######################
# The training function
model_train = function(model, history, x_train, y_train, x_test, y_test,T_STEPS, save_model = FALSE){
  start_time = Sys.time()
  model$ones = k_ones(c(y_train$shape[start_index],1))
  ones_test = k_ones(c(y_test$shape[start_index],1))
  for (r in 1:T_STEPS){
    l  = train_step_au(x=x_train, y=y_train, model=model)  
    if (r %% T_OUT == 0){
      tilde_theta_im = model$model_hy(ones_test)
      beta_x = model$model_beta(x_test)
      nll = bernp.nll(model$bernp, out_bern = tilde_theta_im, y = y_test, out_eta = beta_x, y_range=model$y_range) 
      
      print(paste(r, 'model_3: likelihood (in optimize) ' ,l$numpy() , 'likelihood (in test) ',nll$numpy()))
      history = rbind(history, c(r, run, l$numpy() , nll$numpy(), model$name))
    } 
  }
  end_time = Sys.time() 
  
  if (save_model) {
    save_model_hdf5(model_hy, paste0('boston_cv_model3_hy_F',run,'_Steps_',T_STEPS))
    save_model_hdf5(model_beta, paste0('boston_cv_model3_beta_F',run,'_Steps_',T_STEPS))
  }
  return(history)
}

#######################
# The training function
model_test = function(x_test, y_test){
  tilde_theta_im = model_hy(k_ones(c(y_test$shape[start_index],1)))
  beta_x = model_beta(x_test)
  nll = bernp.nll(my_bernp, out_bern = tilde_theta_im, y = y_test, out_eta = beta_x, y_range=s) 
  return(nll$numpy())
}
