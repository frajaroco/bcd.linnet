box_count_dim <- function(X){

  # X is a lpp object
  verifyclass(X,"lpp")

  # Calculating Gamma(i)
  n <- npoints(X)
  spn <- spread_net(X)
  N_end <- segmentcount(spn,n-1)$N
  
  # D Theoretical
  D_theoretical <- (log(n-1)+log(1-exp(-(n/(n-1)))))/(log(n-1))
  
  # D Estimated
  D_estimated <- log(N_end)/(log(n-1))
  
  invisible(list(D_theo=D_theoretical,D_est=D_estimated,N_est=N_end))
}
