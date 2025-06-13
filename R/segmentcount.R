segmentcount <- function(X,i){
  
  # k: es la longitud total de la red o de la línea
  
  k <- X$new_vertices[length(X$new_vertices)]
  k0 <- lapply(1:(i+1), function(x){(x-1)*k/i})
  count <- table(cut(X$new_points, breaks = c(as.numeric(k0)), include.lowest = TRUE, 
                     right = FALSE))
  N <- sum(unlist(count) != 0)
  invisible(list(count=count,N=N))
}
