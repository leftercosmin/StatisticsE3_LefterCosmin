simuleaza_variabila <- function(valori, probabilitati, n = 1) {
  if(length(valori) != length(probabilitati)) {
    stop("vectorii trebuie sa aiba aceeasi lungime")
  if (sum(probabilitati) != 1) {
    stop("suma probabilitatior trebuie sa fie 1")
  }
  sample(valori, size = n, replace = TRUE, prob = probabilitati)
}
