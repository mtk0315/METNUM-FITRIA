  KODE 1
x <-c(1, 2, 4,5)
y <-c(2, 5, 8,10)
lagrange <- function(x,y,xp) {
  n <-length(x)
  yp <-0
  for(i in 1:n) {
    L <-1
    for(j in 1:n) {
      if (i !=j) {
        L <-L * (xp-x[j])/ (x[i]-x[j])
      }
    }
    yp <- yp+ y[i] * L
  }
  return(yp)
}
lagrange(x,y, 3)

KODE 2
f <- function(x) {
  x^2-2
}
df <- function(x) {
  2 * x
}
newton <- function (x0, tol= 1e-6, max_iter=100) {
  x <-x0
  for (i in 1:max_iter) {
    x_new<-x-f(x) / df(x)
    if(abs(x_new-x)< tol) {
      return(x_new)
    }
    x <-x_new
  }
  return(x)
}

KODE 3
x <- 0.5

sin_taylor <- function(x) {
  hasil <- x - (x^3 / factorial(3)) +
    (x^5 / factorial(5)) -
    (x^7 / factorial(7))
  
  return(hasil)
}

hasil <- sin_taylor(x)

cat("Nilai pendekatan Taylor:", hasil, "\n")
cat("Nilai sebenarnya:", sin(x), "\n")
cat("Selisih:", abs(hasil - sin(x)), "\n")