functie = function(u) {
  exp(-2*u^2)
}

lambda_valoare = 3
N = 50000
x_valori = rexp(N, lambda_valoare)

estimare = sum(functie(x_valori))/(lambda_valoare*N)

valoare_reala = sqrt(pi)/8
eroare = abs(estimare - valoare_reala)

cat("Estimare:", estimare, "\n")
cat("Valoare reală:", valoare_reala, "\n")
cat("Eroare:", eroare, "\n")