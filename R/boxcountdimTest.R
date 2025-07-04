# Function log-log test
boxcountdimTest <- function(X ,nsimev=NULL){

# X is a lpp object
verifyclass(X,"lpp") 
BCD <- box_count_dim(X)

# Parameters
k <- volume(domain(X))
n <- npoints(X)

if(is.null(nsimev)) {
  tsim=100
} else {
  tsim=nsimev}

D_sim <- box_count_dim_pois(X, ns = tsim)$D
B_calc <- BCD$D_theo-BCD$D_est
q1=quantile(D_sim, 0.025)
q3=quantile(D_sim, 0.975)

ifelse(B_calc < median(D_sim),
       ValorP <- 2*sum(unlist(lapply(1:tsim, function(x){
         ifelse(D_sim[x] < B_calc, 1, 0)})))/tsim,
       ValorP <- 2*sum(unlist(lapply(1:tsim, function(x){
         ifelse(D_sim[x] > B_calc, 1, 0)})))/tsim)

invisible(return(list(Q1=q1, Q3=q3, B_calc = B_calc, PV=ValorP, B_sim=D_sim)))
}
