u_scale = function (col, min_col, max_col){
  return ( (col-min_col)/(max_col-min_col) )
}

col_scale <- function(col_train, spatz=0.05, col_test) {
  max_col = max(col_train) * (1 + spatz)
  min_col = min(col_train) * (1 - spatz)
  col_train = u_scale(col_train, min_col=min_col, max_col=max_col)
  col_test = u_scale(col_test, min_col=min_col, max_col=max_col)
  return (list(col_train=col_train, col_test=col_test))
}

# utils_back_scale = function(y_scale, y, spatz = 0){
#   min_y = min(y) * (1 - spatz/2)
#   max_y = max(y) * (1 + spatz/2)
#   return (y_scale * (max_y - min_y) + min_y)
# }

load_data = function(path, split_num=0, spatz = 0.05, x_scale =TRUE) {
  idx_train = read.table(paste0(path, '/data/index_train_', split_num, '.txt')  ) + 1 #We are R-Based
  idx_test = read.table(paste0(path, '/data/index_test_', split_num, '.txt')  ) + 1 #We are R-Based
  y_col = read.table(paste0(path, '/data/index_target.txt')  )  + 1
  x_cols = read.table(paste0(path, '/data/index_features.txt')  )  + 1
  runs = as.numeric(read.table(paste0(path, '/data/n_splits.txt')))
  dat = as.matrix(read.table(paste0(path, '/data/data.txt')))
  X = dat[,x_cols$V1]
  y = dat[,y_col$V1]
  X_train = X[idx_train$V1,]
  y_train = y[idx_train$V1]
  X_test = X[idx_test$V1,]
  y_test = y[idx_test$V1]

  y_s2 = col_scale(col_train=y_train, spatz=0.05, col_test=y_test)
  y_train = y_s2$col_train
  y_test = y_s2$col_test
  scale = max(y_train) - min(y_train)
  
  if(x_scale==TRUE){
    X_train=as.matrix(X_train) # also in case of 1 x
    for( i in 1:ncol(X_train)){
      X_s2 = col_scale(col_train=X_train[,i], spatz=0.05, col_test=X_test[,i])
      X_train[,i] = X_s2$col_train
      X_test[,i] = X_s2$col_test
      }
    
  }
  return (list(X_train=X_train, y_train=y_train, X_test=X_test, y_test=y_test, runs = runs, scale = scale))
}

get_data_boston = function(path, split_num=0, spatz = 0.05, x_scale =TRUE) {
  name = 'boston'
  ret = load_data(path, split_num, spatz, x_scale)
  ret$name = name
  return (ret)
}

get_data_protein = function(path, split_num=0, spatz = 0.05) {
  name = 'protein'
  ret = load_data(path, split_num, spatz)
  ret$name = name
  return (ret)
}


get_data_concrete = function(path, split_num=0, spatz = 0.05) {
  name = 'concrete'
  ret = load_data(path, split_num, spatz)
  ret$name = name
  return (ret)
}

get_data_energy = function(path, split_num=0, spatz = 0.05) {
  name = 'energy'
  ret = load_data(path, split_num, spatz)
  ret$name = name
  return (ret)
}




  
  