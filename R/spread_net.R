spread_net <- function(X){
  
  # X is a lpp object
  verifyclass(X,"lpp")
  
  # nLines: número de segmentos que componen la red
  # X$domain$lines$ends: matriz de vertices inicial y final de cada segmento
  nLines <- nrow(X$domain$lines$ends)
  
  # distanze: vector de longitudes de los segmentos que componen la red.
  distanze <- vector(l = nLines)
  
  distanze <- lapply(1:nLines, function(x){
    dist(rbind(as.numeric(X$domain$lines$ends[x,c(1,2)]),
               as.numeric(X$domain$lines$ends[x,c(3,4)])))})
  
  # dist_final: vector vertices de los segmentos ubicados en una linea
  #             comenzando desde cero
  # cumsum: vector de sumas acumuladas de las componentes de distanze
  dist_final <- c(0, cumsum(distanze))
  
  # nX: número de puntos del patrón puntual X
  # new_points: puntos del patrón puntual X ubicados sobre la linea
  # id: segmento en el que está ubicado un punto del patrón
  # X$data$seg: columna de segmentos a los cuales pertenece cada punto
  # X$data$tp: columna de distancias a las que se encuentra cada punto dentro 
  #            del segmento
  # tp: cada tp está normalizada entre 0 y 1.
  # unnorm: Función que ubica cada punto dentro del respectivo segmento
  #         guardanto la proporcionalidad tp
  nX <- npoints(X)
  new_points <- vector(l = nX)
  new_points <- as.numeric(lapply(1:nX, function(x){
    id <- X$data$seg[x]
    unnorm <- function(z, min, max){(max - min) * z + min}
    new_points <- unnorm(z = X$data$tp[x], min = dist_final[id], 
                         max = dist_final[id + 1])}))
  invisible(list(new_vertices = dist_final, new_points = new_points,
                 nLines = nLines, nX = nX))
}