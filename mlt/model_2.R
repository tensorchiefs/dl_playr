# need to implemnt
#   model_train(history)
#   model_test
optimizer = tf$keras$optimizers$Adam(learning_rate=0.0001)
model <- keras_model_sequential() 
model %>% 
  layer_dense(units=(10), input_shape = c(ncol(x_train)), activation = 'tanh') %>% 
  #layer_dense(units=(100), activation = 'tanh') %>% 
  layer_dense(units=(nb+1)) %>% 
  layer_activation('linear') 

model$summary()
try( #Hack Attack
  {
    bernp(len_theta = len_theta)
    bernp(len_theta = len_theta)
  }
)
my_bernp = bernp(len_theta = len_theta)

train_step = function(x_train, y_train, model){
  with(tf$GradientTape() %as% tape, {
    out = model(x_train)
    NLL = bernp.nll(my_bernp, out, y_train, y_range=s)
  })
  grads = tape$gradient(NLL, model$trainable_variables)
  optimizer$apply_gradients(
    purrr::transpose(list(grads, model$trainable_variables))
  )
  return(NLL)
}


train_step_au = tf_function(train_step) 
model_train = function(history, save_model = FALSE){
  start_time = Sys.time()
  for (r in 1:T_STEPS){
    l  = train_step_au(x_train=x_train, y_train=y_train, model=model)  
    if (r %% T_OUT == 0){
      out = model(x_test)
      nll = bernp.nll(bernp=my_bernp, out_bern=out, y=y_test, y_range=s, out_eta = NULL) 
      
      print(paste(r, 'likelihood (in optimize) ' ,l$numpy(), 
                  'likelihood (in test) ',nll$numpy()))
      history = rbind(history, c(r, run, l$numpy() , nll$numpy(), 'model_2'))
    } 
  }
  end_time = Sys.time() 
  if (save_model){
    save_model_hdf5(model, paste0('boston_cv_model2_F',run,'_Steps_',T_STEPS))
  }
  return(history)
}

model_test = function(x_test, y_test){
  out = model(x_test)
  nll = bernp.nll(my_bernp, out, y_test, y_range=s) 
  return(nll$numpy())
}