box_count_dim_pois <- function(X, ns){
  
  # X is a lpp object
  verifyclass(X,"lpp") 
  
  # Calculating $\Gamma(i)$
  
  n <- npoints(X)
  k <- volume(domain(X))
  line <- linnet(ppp(x=c(1,k+1), y=c(1,1), c(0,k+2), c(0,2)), 
                 edges=matrix(c(1,2), ncol=2))
  linepoi <- rpoislpp(lambda=n/k, L=line, nsim = ns)
  n_end <- unlist(lapply(1:ns, function(y){npoints(linepoi[[y]])}))

  k0 <- function(i){lapply(1:(i+1), function(x){(x-1)*k/i})}
  count <- lapply(1:ns, function(y){table(cut(linepoi[[y]]$data$x, breaks = c(as.numeric(k0(n_end[[y]]))), 
                                include.lowest = TRUE, right = FALSE))})
  N_end <- unlist(lapply(1:ns, function(y){sum(unlist(count[[y]]) != 0)}))
  
  # D Theoretical
  
  D_theoretical <- unlist(lapply(n_end, function(i){
  x_1=0
  x_2=log(i-1)
  y_1=log(1-exp(-i))
  y_2=log(i-1)+log(1-exp(-(i/(i-1))))
  return((y_2-y_1)/(x_2-x_1)+(log(1-exp(-i)))/(log(i-1)))
  }))
  
  # D Estimated
  
  D_estimated <- unlist(lapply(1:ns, function(i){
    x_1=0
    x_2=log(n_end[[i]]-1)
    y_1=log(1-exp(-n_end[[i]]))
    y_2=log(n_end[[i]]-1)+log(1-exp(-(n_end[[i]]/(n_end[[i]]-1))))
    y_2_estimated=log(N_end[[i]])
  return((y_2_estimated-y_1)/(x_2-x_1)+(log(1-exp(-n_end[[i]])))/(log(n_end[[i]]-1)))
  }))
  
  D = D_theoretical-D_estimated
  
  invisible(list(D_theo=D_theoretical, D_est=D_estimated, N_est=N_end, D=D))
}
