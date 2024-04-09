dispersie = function(x,p)
{
  media = sum(p*x);
  dispersie = sum(p*(x-media)^2);
  return(dispersie)
}

y = c(23,32,31,27,27,33,25,21)
q = c(1/8, 1/16, 1/8, 1/16, 1/8, 1/16, 1/8, 5/16)
disperse(y,q)

Poisson <- function(n, lambda)
{
  x=0:(n-1)
  y=dpois(x, lambda)
  barplot(y, space=0, main='barplot', sub="ex 10", xlab="axa x", ylab="axa y")
}

Poisson(15, 3)