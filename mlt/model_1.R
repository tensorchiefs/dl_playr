# #### Model 1
fm = (y ~ crim + zn + indus + chas + nox + rm + age + dis + rad + tax + ptratio + b + lstat)
#fm = (y ~ rm + lstat + indus + chas + nox + rm + age + dis )
#fm = (y ~ rm  )

var_y <- numeric_var("y", support = c(0, 1), bounds = c(-Inf, Inf), add = c(-2,2))
bb <- Bernstein_basis(var_y, order=nb, ui="increasing")
ctm = ctm(bb, shift=fm[-2L], data=datt[idx_train,], todistr="Normal")
# fm[-2L] defnes the basis function for the shift term h_y(y) in h(y|x)=h_y(y)+h_x(x)
mlt_fit <- mlt(ctm, data = datt[idx_train,], verbose=TRUE)

calc_test_nll_mlt <- function(mlt_fit, datt, idx_test, s ) {
  ll = 0
  ok = 1
  for (i in idx_test){
    try({
      d = predict(mlt_fit, newdata = datt[i,],q=datt$y[i],type='density')
      ll = ll + log(d) - log(s)
      ok  = ok + 1
    })
  }
  print(sprintf ("Proportion of working instances %.2f",ok/length(idx_test)))
  return(-ll/ok)
}

nll = calc_test_nll_mlt(mlt_fit, datt, idx_test, s )
model_train = function(history, x_train, y_train, save_model = FALSE){
  # history[history.row,] = c(1, run, -logLik(mlt_fit)/length(idx_train) + log(s), 
  #                           nll, 'model_1')
  # history.row = history.row + 1
  history = rbind(history, c(1, run, -logLik(mlt_fit)/length(idx_train) + log(s), nll, 'model_1'))
  history = rbind(history, c(T_STEPS, run, -logLik(mlt_fit)/length(idx_train) + log(s), nll, 'model_1'))
  pred_median = predict(mlt_fit, newdata = datt[idx_test,], type='quantile', prob=0.5)
  sprintf("MLT MAD on Test %.4f", mean(abs(as.numeric(pred_median) - datt$y[idx_test])))
  return (history)
}


#nll_mlt[run] = nll
