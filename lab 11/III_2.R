p1 = 1/4
p2 = 3/4

lambda1 = 4
lambda2 = 12

N = 10000
suma_x <- 0
for (i in 1:N) {
  x = p1*rexp(1, lambda1) + p2*rexp(1, lambda2)
  suma_x = suma_x + x
}
asteptare = suma_x/N

cat("Asteptare estimata:", asteptare, "\n")