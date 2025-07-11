# Function log-log test
boxcountdimTest <- function(X, nsimev=100, alpha=0.05){

# X is a lpp object
verifyclass(X,"lpp") 
BCD <- box_count_dim(X)

D_sim <- box_count_dim_pois(X, ns = nsimev)$D
B_calc <- BCD$D_theo-BCD$D_est
q1=quantile(D_sim, alpha/2)
q3=quantile(D_sim, 1-alpha/2)

ifelse(B_calc < median(D_sim),
       ValorP <- 2*sum(unlist(lapply(1:nsimev, function(x){
         ifelse(D_sim[x] < B_calc, 1, 0)})))/nsimev,
       ValorP <- 2*sum(unlist(lapply(1:nsimev, function(x){
         ifelse(D_sim[x] > B_calc, 1, 0)})))/nsimev)

invisible(return(list(Q1=q1, Q3=q3, B_calc = B_calc, PV=ValorP, B_sim=D_sim)))
}
