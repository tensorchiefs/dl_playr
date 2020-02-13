# need to implemnt
#   model_train(history)
#   model_test

# Private definition of the network model
.make_model = function(len_theta, x_dim){ 
  model <- keras_model_sequential() 
  model %>% 
    layer_dense(units=(10), input_shape = c(x_dim), activation = 'tanh') %>% 
    #layer_dense(units=(100), activation = 'tanh') %>% 
    layer_dense(units=len_theta) %>% 
    layer_activation('linear') 
  return (model)
}

#"Private" and complete constructor. This constructor verifies the input.
new_model_2 = function(len_theta = integer(), x_dim, y_range){
  stopifnot(is.integer(len_theta))
  stopifnot(len_theta > 0)
  structure( #strcutur is a bunch of data with 
    list( #bunch of data
      len_theta=len_theta,  
      x_dim = x_dim,
      optimizer = tf$keras$optimizers$Adam(learning_rate=0.0001),
      model = .make_model(len_theta, x_dim),
      bernp = make_bernp(len_theta),
      y_range = y_range,
      name = 'model_2'
    ),
    class = "model_2" #class attribute set so it's a class
  )  
}

train_step = function(x_train, y_train, model){
  with(tf$GradientTape() %as% tape, {
    out = model$model(x_train)
    NLL = bernp.nll(model$bernp, out, y_train, model$y_range)
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
      out = model$model(x_test)
      nll = bernp.nll(bernp=model$bernp, out_bern=out, y=y_test, y_range=model$y_range, out_eta = NULL) 
      print(paste(r, 'likelihood (in optimize) ' ,l$numpy(), 'likelihood (in test) ',nll$numpy()))
      history = rbind(history, c(r, run, l$numpy() , nll$numpy(), model$name))
    } 
  }
  end_time = Sys.time() 
  if (save_model){
    save_model_hdf5(model, paste0('boston_cv_model2_F',run,'_Steps_',T_STEPS))
  }
  return(history)
}

model_test = function(model, x_test, y_test){
  out = model$model(x_test)
  nll = bernp.nll(model$bernp, out, y_test, y_range=model$y_range) 
  return(nll$numpy())
}