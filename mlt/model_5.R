optimizer = tf$keras$optimizers$Adam(learning_rate=0.0001)
get_string = function(){
  return ("Model_5 with ")
}

###############################
# Baseline transformation gives (after piping through to_theta) 
# the coefficients for the bernstein polynoms
.make_model_hy = function(len_theta) {
  model_hy <- keras_model_sequential() 
  model_hy %>% 
    #layer_dense(units=(10), input_shape = c(1), activation = 'tanh') %>% 
    layer_dense(units=len_theta,input_shape = c(1)) %>% 
    layer_activation('linear') 
  return(model_hy)
}


.make_model_hy_xdep = function(len_theta, x_dim) {
  model_hy <- keras_model_sequential() 
  model_hy %>% 
    layer_dense(units=(10), input_shape = x_dim, activation = 'tanh') %>% 
    #layer_dense(units=(100), activation = 'tanh') %>% 
    layer_dense(units=(len_theta)) %>% 
    layer_activation('linear') 
  return(model_hy)
}


###############################
# Linear shift term gives coefficients for the predictor (beta)
.make_model_beta = function(x_dim){
  model_beta <- keras_model_sequential() 
  model_beta %>% 
    layer_dense(10, activation='tanh', input_shape = x_dim) %>% 
    # layer_dense(50, activation='tanh') %>% 
    # layer_dense(10, activation='tanh') %>% 
    layer_dense(5, activation='tanh',kernel_regularizer = regularizer_l2(2.05)) %>% 
    layer_dense(1, activation='linear', kernel_regularizer = regularizer_l2(2.05)) 
  return(model_beta)
}

###############################
# The Scale Model
.make_model_gamma = function(x_dim){
  model_gamma <- keras_model_sequential() 
  model_gamma %>% 
    layer_dense(10, activation='tanh', input_shape = x_dim) %>% 
    # layer_dense(50, activation='tanh') %>%
    # layer_dense(10, activation='tanh') %>% 
    layer_dense(5, activation='tanh') %>% 
    layer_dense(1, activation='linear') 
  return(model_gamma)
}

#"Private" and complete constructor. This constructor verifies the input.
new_model_5 = function(len_theta = integer(), x_dim, y_range, reg_factor=-1.0, is_theta_x = FALSE){
  stopifnot(is.integer(len_theta))
  stopifnot(len_theta > 0)
  if (is_theta_x){
    model_hy = .make_model_hy_xdep(len_theta, x_dim)
    name = 'model_5_theta_x'
  } else{
    model_hy = .make_model_hy(len_theta)
    name = 'model_5'
  }
  
  structure( #strcutur is a bunch of data with 
    list( #bunch of data
      len_theta=len_theta,  
      x_dim = x_dim,
      optimizer = tf$keras$optimizers$Adam(learning_rate=0.0001),
      model_beta = .make_model_beta(x_dim),
      model_hy = model_hy,
      model_gamma = .make_model_gamma(x_dim),
      bernp = make_bernp(len_theta),
      y_range = y_range,
      name = name,
      reg_factor = reg_factor,
      is_theta_x = is_theta_x,
      ones = -42 #Will be set in training
    ),
    class = "model_5" #class attribute set so it's a class
  ) 
}

# ones = k_ones(c(y_train$shape[start_index],1))#tf$Variable(as.matrix(dat$rm)[,drop=FALSE], dtype='float32')
# my_bernp = bernp(len_theta = len_theta)
# tilde_theta_im = model_hy(ones)
# beta_x = model_beta(x_train)
# gamma_x = model_gamma(x_train)

nll_model5 = function(bernp, out_bern, y, out_eta , out_gamma,y_range) {
  theta_im = to_theta(out_bern)
  hy = eval_h(theta_im, y_i = y, beta_dist_h = bernp$beta_dist_h)
  sp = tf$math$softplus(out_gamma[,1])
  z =  sp * hy -  out_eta[,1]
  h_y_dash = sp * eval_h_dash(theta_im, y, beta_dist_h_dash = bernp$beta_dist_h_dash)
  return(-tf$math$reduce_mean(bernp$stdnorm$log_prob(z) + tf$math$log(h_y_dash)) + log(y_range))
}

add_squarred_weights_penalty = function(weights, NLL, lambda=0.05){
  for (w in weights){
    NLL = NLL + lambda*tf$reduce_sum(tf$math$square(w)) #L2
    #NLL = NLL + lambda*tf$reduce_sum(tf$math$abs(w)) #L1
  }
  return(NLL)
}

train_step = function(x, y, model){
  model_hy = model$model_hy
  model_beta = model$model_beta
  model_gamma = model$model_gamma
  #x$assign_add(tf$random$normal(shape = tf$shape(x),stddev = 0.01))
  #y$assign_add(tf$random$normal(shape = tf$shape(y),stddev = 0.01))
  with(tf$GradientTape() %as% tape, {
    if (model$is_theta_x) {
      tilde_theta_im = model$model_hy(x) 
    } else{
      tilde_theta_im = model$model_hy(model$ones) 
    }
    beta_x = model_beta(x)
    gamma_x = model_gamma(x)
    NLL = nll_model5(model$bernp, out_bern = tilde_theta_im, y = y, out_eta = beta_x, out_gamma=gamma_x, y_range = model$y_range)
    if (model$reg_factor > 0){
      NLL = add_squarred_weights_penalty(weights=model_hy$trainable_variables, NLL=NLL, lambda=reg_factor)
      NLL = add_squarred_weights_penalty(weights=model_beta$trainable_variables, NLL=NLL, lambda=reg_factor)
      NLL = add_squarred_weights_penalty(weights=model_gamma$trainable_variables, NLL=NLL, lambda=reg_factor)  
    }
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

model_train = function(model,history, x_train, y_train, x_test, y_test, T_STEPS ,save_model = FALSE){
  start_time = Sys.time()
  model$ones = k_ones(c(y_train$shape[start_index],1))
  ones_test = k_ones(c(y_test$shape[start_index],1))
  for (r in 1:T_STEPS){
    l  = train_step_au(x=x_train, y=y_train, model=model)  
    if (r %% T_OUT == 0){
      if (model$is_theta_x) {
        tilde_theta_im = model$model_hy(x_test) 
      } else{
        tilde_theta_im = model$model_hy(ones_test) 
      }
      beta_x = model$model_beta(x_test)
      gamma_x = model$model_gamma(x_test)
      nll = nll_model5(model$bernp, out_bern = tilde_theta_im, y = y_test, out_eta = beta_x, out_gamma = gamma_x, y_range=model$y_range)
      print(paste(r, model$name, ' likelihood (in optimize) ' ,l$numpy() , 'likelihood (in test) ',nll$numpy()))
      history = rbind(history, c(r, run, l$numpy() , nll$numpy(), model$name))
    } 
  }
  end_time = Sys.time() 
  
  if (save_model){
    save_model_hdf5(model$model_hy, paste0('boston_cv_model5_hy_F',run,'_Steps_',T_STEPS))
    save_model_hdf5(model$model_beta, paste0('boston_cv_model5_beta_F',run,'_Steps_',T_STEPS))
    save_model_hdf5(model$model_gamma, paste0('boston_cv_model5_gamma_F',run,'_Steps_',T_STEPS))
  }
  return(history)
}


#######################
# The training function
model_test = function(model, x_test, y_test) {
  if (model$is_theta_x) {
    tilde_theta_im = model$model_hy(x_test) 
  } else{
    tilde_theta_im = model$model_hy(k_ones(c(y_test$shape[start_index],1))) 
  }
  beta_x = model$model_beta(x_test)
  gamma_x = model$model_gamma(x_test)
  nll = nll_model5(model$bernp, out_bern = tilde_theta_im, y = y_test, out_eta = beta_x, out_gamma = gamma_x, model$y_range)
  return (nll$numpy())
}


