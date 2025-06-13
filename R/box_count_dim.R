box_count_dim <- function(X){
  
  # X is a lpp object
  verifyclass(X,"lpp") 
  
  # Calculating $\Gamma(i)$
  
  n <- npoints(X)
  spn <- spread_net(X)
  N_end <- segmentcount(spn, n-1)$N
  
  # D Theoretical
  
  x_1=0
  x_2=log(n-1)
  y_1=log(1-exp(-n))
  y_2=log(n-1)+log(1-exp(-(n/(n-1))))
  D_theoretical=(y_2-y_1)/(x_2-x_1)+(log(1-exp(-n)))/(log(n-1))
  
  # D Estimated
  
  y_2_estimated=log(N_end)
  D_estimated=(y_2_estimated-y_1)/(x_2-x_1)+(log(1-exp(-n)))/(log(n-1))
  
  invisible(list(D_theo=D_theoretical, D_est=D_estimated, N_est=N_end))
}
