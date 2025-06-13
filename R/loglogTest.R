loglogTest <- function(X, nsimev = NULL, two.p = FALSE){

require(spatstat)
require(GET)

verifyclass(X,"lpp")

k <- volume(domain(X))
n <- npoints(X)

# Calculating $N(\Gamma(i))$

spn <- spread_net(X)
N <- c(unlist(lapply(1:(n-1), function(x){
  segmentcount(spn, x)$N})))

# Domain log-log function

i=1:(n-1)
r=log(i)

# Estimated log

y_est=log(N)

# Theoretical log

y_theo <- log(i*(1-exp(-(n/i))))

# return

gobs <- fv(data.frame(r = r, est = y_est, theo = y_theo),
                    "r", quote(Log(N(delta[i]))),
                    "est", cbind(est, theo) ~ r,
                    labl=c("%s(i)", "%s[est]", "%s[theo]"),
                    desc=c("values on the abscissa of the log-log plot",
                           "estimated values with %s",
                           "theoretical values with Log(i*(1-exp(-n/i)))"),
                    fname="Log")

if(is.null(nsimev)) {
  tsim=100
} else {
  tsim=nsimev
}

  random <- rpoislpp(n/k, domain(X), nsim = tsim)
  n0 <- as.numeric(lapply(1:tsim, function(x){npoints(random[[x]])}))

  # Calculating $N(\Gamma(i))$

  spns <- lapply(1:tsim, function(x){spread_net(random[[x]])})
  Ns <- lapply(1:tsim,function(y){c(unlist(lapply(1:(n-1), function(x){
    segmentcount(spns[[y]], x)$N})))})

  # Estimated log

  y_ests = sapply(1:tsim, function(x){log(Ns[[x]])})

  Cu <- curve_set(list(r = gobs$r, obs = gobs$est,
                              sim_m = y_ests, theo = gobs$theo))

  if(two.p == FALSE) {
    Test <- global_envelope_test(Cu, alpha = 0.05, type = "erl")
    p <- attr(Test, "p")
  } else {

    Test1 <- global_envelope_test(Cu, alpha = 0.01, type = "erl")
    p1 <- attr(Test1, "p")

    Test5 <- global_envelope_test(Cu, alpha = 0.05, type = "erl")
    p5 <- attr(Test5, "p")

    Test <- list(Test01=Test1, Test05=Test5)
    p <- list(p01=p1, p05=p5)

  }

invisible(return(list(Test=Test,p=p)))
}
