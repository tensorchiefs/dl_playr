# need to implemnt
#   model_train(history)
#   model_test

# Private definition of the network model
.make_model = function(len_theta, x_dim){ 
  model <- keras_model_sequential() 
  model %>% 
    layer_dense(units=(10), input_shape = c(x_dim), activation = 'tanh') %>% 
    layer_dense(units=(100), activation = 'tanh') %>% 
    layer_dense(units=len_theta) %>% 
    layer_activation('linear') 
  return (model)
}

# Private definition of the network model
.make_model_g = function(x_dim){ 
  model <- keras_model_sequential() 
  model %>% 
    layer_dense(units=(10), input_shape = c(x_dim), activation = 'tanh') %>% 
    #layer_dense(units=(100), activation = 'tanh') %>% 
    layer_dense(units=1) %>% 
    layer_activation('softplus') 
  return (model)
}

# Private definition of the network model
.make_model_s = function(x_dim){ 
  modeld <- keras_model_sequential() 
  modeld %>% 
    layer_dense(units=(10), input_shape = c(x_dim), activation = 'tanh') %>% 
    #layer_dense(units=(100), activation = 'tanh') %>% 
    layer_dense(units=1) %>% 
    layer_activation('linear') 
  return (modeld)
}


#"Private" and complete constructor. This constructor verifies the input.
new_model_7 = function(len_theta = integer(), x_dim, y_range){
  stopifnot(is.integer(len_theta))
  stopifnot(len_theta > 0)
  structure( #strcutur is a bunch of data with 
    list( #bunch of data
      len_theta=len_theta,  
      x_dim = x_dim,
      optimizer = tf$keras$optimizers$Adam(learning_rate=0.0001),
      model = .make_model(len_theta, x_dim),
      model_g = .make_model_g(x_dim),
      model_s = .make_model_s(x_dim),
      bernp = make_bernp(len_theta),
      y_range = y_range,
      name = 'model_7'
    ),
    class = "model_7" #class attribute set so it's a class
  )  
}

calc_NLL = function(out_hy, g, s, y_train, y_range, bernp) {
  y_tilde = g*y_train - s - 0.5 #TODO make nicer
  y_tilde_2 = tf$math$sigmoid(y_tilde)
  # print(summary(as.numeric(y_tilde)))
  # print('g')
  #print(summary(as.numeric(y_tilde_2)))
  theta_im = to_theta(out_hy)
  z = eval_h(theta_im, y_i = y_tilde_2, beta_dist_h = bernp$beta_dist_h)
  h_y_dash_part1 = eval_h_dash(theta_im, y_tilde_2, beta_dist_h_dash = bernp$beta_dist_h_dash) 
  h_y_dash_part2 = g 
  l_h_y_dash_part3 = tf$math$log(tf$math$sigmoid(y_tilde)) +  tf$math$log((1 - tf$math$sigmoid(y_tilde)))
  return(-tf$math$reduce_mean(bernp$stdnorm$log_prob(z) + tf$math$log(h_y_dash_part1) + tf$math$log(h_y_dash_part2) +l_h_y_dash_part3) + log(y_range))
}

train_step = function(x_train, y_train, model){
  with(tf$GradientTape() %as% tape, {
    out_hy = model$model(x_train)
    g = model$model_g(x_train)
    sss = model$model_s(x_train)
    NLL = calc_NLL(out_hy, g, sss, y_train, model$y_range, model$bernp)
  })
  grads = tape$gradient(NLL, model$model$trainable_variables)
  model$optimizer$apply_gradients(
    purrr::transpose(list(grads, model$model$trainable_variables))
  )
  return(NLL)
}


train_step_au = tf_function(train_step) 
model_train = function(model, history, x_train, y_train, x_test, y_test,save_model = FALSE, T_STEPS){
  start_time = Sys.time()
  for (r in 1:T_STEPS){
    l  = train_step_au(x_train=x_train, y_train=y_train, model=model)  
    if (r %% T_OUT == 0){
      out_hy = model$model(x_test)
      g = model$model_g(x_test)
      s = model$model_s(x_test)
      NLL = calc_NLL(out_hy, g, s, y_test, model$y_range, model$bernp)
      print(paste(r, 'likelihood (in optimize) ' ,l$numpy(), 'likelihood (in test) ',NLL$numpy()))
      history = rbind(history, c(r, run, l$numpy() , NLL$numpy(), model$name))
    } 
  }
  end_time = Sys.time() 
  if (save_model){
    save_model_hdf5(model, paste0('boston_cv_model7_F',run,'_Steps_',T_STEPS))
  }
  return(history)
}

model_test = function(model, x_test, y_test){
  out_hy = model$model(x_test)
  g = model$model_g(x_test)
  s = model$model_s(x_test)
  NLL = calc_NLL(out_hy, g, s, y_test, model$y_range, model$bernp)
  return(NLL$numpy())
}