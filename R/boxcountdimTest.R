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

sim <- rpoislpp(n/k,domain(X), nsim=tsim)
D <- lapply(1:tsim, function(x){box_count_dim(sim[[x]])})
D_m <- t(matrix(unlist(D),nrow=3,ncol=tsim))
D_sim <- D_m[,1]-D_m[,2]
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
