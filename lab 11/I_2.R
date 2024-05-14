regiune_parabola = function() {
  f = function(x) -2*x^2 + 5*x - 2
  
  a_val = 0
  b_val = 2
  regiune_exacta = integrate(f, a_val, b_val)$value
  
  set.seed(123)
  n = 10000
  x_valori = runif(n, a_val, b_val)
  y_valori = runif(n, 0, max(f(x_valori)))
  
  in_interior = y_valori <= f(x_valori)
  proportie_in_interior = mean(in_interior)
  
  aria_estimata = (b_val - a_val) * max(f(x_valori)) * proportie_in_interior
  eroare_relativa = abs(regiune_exacta - aria_estimata) / regiune_exacta
  
  informatii_rezultat = list(aria = aria_estimata, aria_exacta = regiune_exacta, eroare_rel = eroare_relativa)
  return(informatii_rezultat)
}

rezultat_info = regiune_parabola()

cat("Aria estimată:", format(rezultat_info$aria, digits = 8), "\n")
cat("Aria exactă:", format(rezultat_info$aria_exacta, digits = 8), "\n")
cat("Eroare relativă:", format(rezultat_info$eroare_rel, digits = 2), "%\n")
