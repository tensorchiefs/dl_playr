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
    layer_dense(units=(100), activation = 'tanh') %>% 
    layer_dense(units=1) %>% 
    layer_activation('softplus') #TODO why not linear
  return (model)
}

# Private definition of the network model
.make_model_s = function(x_dim){ 
  modeld <- keras_model_sequential() 
  modeld %>% 
    layer_dense(units=(10), input_shape = c(x_dim), activation = 'tanh') %>% 
    layer_dense(units=(100), activation = 'tanh') %>% 
    layer_dense(units=1) %>% 
    layer_activation('linear') 
  return (modeld)
}

###############################
# Linear shift term gives coefficients for the predictor (beta)
.make_model_beta = function(x_dim){
  model_beta <- keras_model_sequential() 
  model_beta %>% 
    layer_dense(10, activation='tanh', input_shape = x_dim) %>% 
    layer_dense(50, activation='tanh') %>% 
    layer_dense(10, activation='tanh') %>% 
    layer_dense(5, activation='tanh') %>% 
    layer_dense(1, activation='linear')
  return(model_beta)
}


#"Private" and complete constructor. This constructor verifies the input.
new_model_7 = function(len_theta = integer(), x_dim, y_range, eta_term=FALSE, reg_factor=-1){
  stopifnot(is.integer(len_theta))
  stopifnot(len_theta > 0)
  model_beta = NULL
  if (eta_term){
    model_beta = .make_model_beta(x_dim)
  }
  
  structure( #strcutur is a bunch of data with 
    list( #bunch of data
      len_theta=len_theta,  
      x_dim = x_dim,
      optimizer = tf$keras$optimizers$Adam(learning_rate=0.0001),
      model = .make_model(len_theta, x_dim),
      model_g = .make_model_g(x_dim),
      model_s = .make_model_s(x_dim),
      model_beta = model_beta,
      bernp = make_bernp(len_theta),
      reg_factor = reg_factor,
      y_range = y_range,
      name = 'model_7'
    ),
    class = "model_7" #class attribute set so it's a class
  )  
}

add_squarred_weights_penalty = function(weights, NLL, lambda=0.05){
  for (w in weights){
    NLL = NLL + lambda*tf$reduce_sum(tf$math$square(w)) #L2
    #NLL = NLL + lambda*tf$reduce_sum(tf$math$abs(w)) #L1
  }
  return(NLL)
}

calc_NLL = function(out_hy, g, s, y_train, y_range, out_eta, bernp) {
  y_tilde = g*y_train - s #TODO make nicer
  y_tilde_2 = tf$math$sigmoid(y_tilde)
  # print(summary(as.numeric(y_tilde)))
  # print('g')
  # print(summary(as.numeric(y_tilde_2)))
  theta_im = to_theta(out_hy)
  if (!is.null(out_eta)){
    z = eval_h(theta_im, y_i = y_tilde_2, beta_dist_h = bernp$beta_dist_h) - out_eta[,1]
  } else {
    z = eval_h(theta_im, y_i = y_tilde_2, beta_dist_h = bernp$beta_dist_h) 
  }
  h_y_dash_part1 = eval_h_dash(theta_im, y_tilde_2, beta_dist_h_dash = bernp$beta_dist_h_dash) 
  h_y_dash_part2 = g 
  #sig' = sig(1-sig)
  l_h_y_dash_part3 = tf$math$log(tf$math$sigmoid(y_tilde)) +  tf$math$log((1 - tf$math$sigmoid(y_tilde)))
  return(-tf$math$reduce_mean(
    bernp$stdnorm$log_prob(z) + 
    tf$math$log(h_y_dash_part1) + 
    tf$math$log(h_y_dash_part2) +
    l_h_y_dash_part3) + 
    log(y_range)
    )
}

train_step = function(x_train, y_train, model){
  with(tf$GradientTape() %as% tape, {
    out_hy = model$model(x_train)
    g = model$model_g(x_train)
    sss = model$model_s(x_train)
    NLL = calc_NLL(out_hy, g, sss, y_train, model$y_range, out_eta = model$beta_x, bernp=model$bernp)
    if (model$reg_factor > 0){
      NLL = add_squarred_weights_penalty(weights=model$model$trainable_variables, NLL=NLL, lambda=model$reg_factor)
      NLL = add_squarred_weights_penalty(weights=model$model_g$trainable_variables, NLL=NLL, lambda=model$reg_factor)
      NLL = add_squarred_weights_penalty(weights=model$model_s$trainable_variables, NLL=NLL, lambda=model$reg_factor)
    }
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
      model_beta = model$model_beta
      if (!is.null(model_beta)){
        beta_x = model$model_beta(x_test)  
      } else {
        beta_x = NULL
      }
      NLL = calc_NLL(out_hy, g, s, y_test, model$y_range, out_eta = beta_x, bernp=model$bernp)
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
  NLL = calc_NLL(out_hy, g, s, y_test, model$y_range,  out_eta = model$beta_x, bernp=model$bernp)
  return(NLL$numpy())
}

model_get_p_y = function(model, x, from, to, length.out){
  bernp = model$bernp
  #stopifnot(x$shape[start_index] == 1) #We need a single row
  y_cont = keras_array(matrix(seq(from,to,length.out = length.out), nrow=length.out,ncol=1))
  
  out_hy = model$model(x)
  theta_im = to_theta(out_hy)
  
  g = model$model_g(x)
  s = model$model_s(x)
  
  y_tilde = g*y_cont - s
  y_tilde_2 = tf$math$sigmoid(y_tilde)
  theta_rep = k_tile(theta_im, c(length.out, 1))
  #print(theta_rep)
  if(is.null( model$model_beta)){
    z = eval_h(theta_rep, y_tilde_2, beta_dist_h = bernp$beta_dist_h)
  } else{
    beta_x = model$model_beta(x)
    z = eval_h(theta_rep, y_tilde_2, beta_dist_h = bernp$beta_dist_h) - beta_x[,1]
  }
 
  p_y = bernp$stdnorm$prob(z) * as.array(eval_h_dash(theta_rep, y_tilde_2, beta_dist_h_dash = bernp$beta_dist_h_dash))
  p_y = tf$transpose(p_y * g) * tf$math$sigmoid(y_tilde) * (1.0 - tf$math$sigmoid(y_tilde))
  
  df = data.frame(
    y = seq(from,to,length.out = length.out),
    p_y = as.numeric(p_y),
    h = z$numpy()
    )
  df$y_tilde = as.numeric(y_tilde)
  df$y_tilde_2 = as.numeric(y_tilde_2)
  return (df)
}








