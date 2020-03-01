u_scale = function (y, min_y, max_y){
  return ( (y-min_y)/(max_y-min_y) )
}

# utils_back_scale = function(y_scale, y, spatz = 0){
#   min_y = min(y) * (1 - spatz/2)
#   max_y = max(y) * (1 + spatz/2)
#   return (y_scale * (max_y - min_y) + min_y)
# }

load_data = function(path, split_num=0, spatz = 0.05) {
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
  
  max_y = max(y_train) * (1 + spatz)
  min_y = min(y_train) * (1 - spatz)
  scale = max_y - min_y
  y_train = u_scale(y_train, min_y=min_y, max_y=max_y)
  y_test = u_scale(y_test, min_y=min_y, max_y=max_y)
  return (list(X_train=X_train, y_train=y_train, X_test=X_test, y_test=y_test, runs = runs, scale = scale))
}

get_data_boston = function(path, split_num=0, spatz = 0.05) {
  name = 'boston'
  ret = load_data(path, split_num, spatz)
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





  
  