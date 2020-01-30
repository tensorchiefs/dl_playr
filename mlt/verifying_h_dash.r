library(basefun)

# specify a numeric variable on interval 0,1
var_d= numeric_var("x", support = c(0, 1), add=c(0,0))
# set up monoton increasing Bernstein polynomial in interval supp+add
bb <- Bernstein_basis(var_d, order = 3, ui = "increasing")
# set up grid in interval supp+add
x <- as.data.frame(mkgrid(bb, n = 5))


# MLT learns the trafo as linear combi of Berstein functions
# more specifically the coef of this linear combi are learned

# make up some coef:
theta <- c(0.1,1.1,4,12)

# Use mlt functions to determine trafo h
#########################################
# now use this made-up coeff to determine the trafo h
# use the mlt-Berstein polynom to determine the trafo ouputs at grid positions:
h_t_x = predict(bb, newdata = x, coef = theta)


# draw trafo derived with mlt functionality
plot(x$x, h_t_x, type = "l", main="h with basefun")

# draw the derivation of h with basefun-functional
d=model.matrix(bb,data=data.frame(x=x$x))
deriv=model.matrix(bb,data=data.frame(x=x$x), deriv=c("x"=1))
dd=deriv%*%theta
plot(x$x, dd, main="h' with basefun")

# Determine trafo h and h' with own functions
#########################################

# scaling is ignored - we assume data in [0,1]

# trafo h is needed to get cum_dist F_y(y)=F_z(h(y))
## Bernstein-pol-trafo is given by (MLT Paper p.12):
h <- function(theta, y) {
  M = length(theta)-1 #Order 
  dd = rep(0, length(y)) 
  for (m in 0:M){
    dd = dd + theta[m+1] * dbeta(y,m+1,M-m+1)   
  }
  dd = dd / (M + 1.)
  return (dd)
}

# draw trafo h derived with basefun functionality vs own function
plot(x$x, h_t_x, type = "l", col="blue", 
     main="h with basefun (blue) and own fct (red)")
lines(x$x, h(theta, x$x), col="red", lty=2, lwd=2)

# determine h' ((MLT Paper p.12))
# h' is needed to get densities: f_y(y)=f_z(h(y))*h'(y)
h_dash <- function(theta, y, r=1) {
  M = length(theta)-1 #Order 
  dd = rep(0, length(y)) 
  for (m in 0:(M-1)){
    dd = dd + (theta[m+2] - theta[m+1]) * dbeta(y,m+1,M-m)   
  }
  #dd = dd * M / ((M + 1.)*r)  # so ists im paper, wird aber falsch
  dd = dd  / r  # so wirds richtig
  return (dd)
}


# plot h' computed in three ways
# first way: with our implementation of h'
h_dash(theta, x$x)

plot(x$x, h_dash(theta, x$x), type = "l",lty=2,col='red', lwd=3,
     main="h' with basefun (blue), our fct (red), numeric derivation (black) ")
# second way: numerical derivation of h
# check if h' is correct by comparing it with manual devied derivation of h:
diff(h(theta,seq(0,1,0.01)))/0.01
lines(seq(0,2,0.01)[1:100],diff(h(theta,seq(0,2.01,0.01)))[1:100]/0.01)
# third way: with basefun-functionality
lines(x$x, dd, lty=3, col="blue", lwd=3)


