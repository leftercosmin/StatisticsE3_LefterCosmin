#B1.

volum_tor <- function(R, r, esantion) 
{
  x1<-runif(esantion, -R - r, R + r)
  x2<-runif(esantion, -R - r, R + r)
  x3<-runif(esantion, -r, r)
  
  puncte_interioare <- x3^2 + (sqrt(x1^2 + x2^2) - R)^2 < r^2
  proportie_interior <- sum(puncte_interioare) / esantion
  volum_cub <- (2 * (R + r))^2 * (2 * r)
  volum_estimat <- proportie_interior * volum_cub
  
  return(volum_estimat)
}

volum_exact <- 2 * pi^2 * 10 * 3^2
esantioane <- c(10000, 20000, 50000)

for (esantion in esantioane) 
{
  volum_estimat <- volum_tor(10, 3, esantion)
  eroare_relativa <- abs(volum_estimat - volum_exact) / volum_exact
  cat("Esantion de dimensiune: ", esantion, " volum estimat: ", volum_estimat, " Eroare relativ: ", eroare_relativa, "\n")
}

#B2.

arie_triunghi<-function(esantion)
{
  interior <- 0
  for (i in 1:esantion)
  {
  #generare coordonate x si y aleatoare
    x<-runif(1, a, b) 
    y<-runif(1, c, d)  
    if (y >= 0 & y <= 2 * x & y <= 6 - 3 * x)
    {
      interior<-interior + 1
    }
  }
  arie_rectangular<-(b - a)*(d - c)
  arie_triunghi<-arie_rectangular*(interior / esantion)
  return(arie_triunghi)
}
a <- 0
b <- 2
c <- 0
d <- 6
arie <- arie_triunghi(20000)
print(arie)

#B3.

valoare_integrala1<-log(3)-log(2)
valoare_integrala2<-61.2
valoare_integrala3<-1/2

functie1<-function(x)
{
  (2*x-1)/(x^2-x-6)
}
integrala1<-integrate(functie1,-1,1)$value

functie2<-function(x)
{
  (x+4)/((x-3)^(1/3))
}
integrala2<-integrate(functie2,3,11)$value

functie3<-function(x)
{
  x*exp(-x^2)
}
integrala3<-integrate(functie3,0,Inf)$value

cat("Integrala I:\nValoare estimata=",integrala1, "\nValoare exacta=", valoare_integrala1,"\nDiferenta=", abs(integrala1-valoare_integrala1), "\n\n")
cat("Integrala II:\nValoare estimata=",integrala2, "\nValoare exacta=", valoare_integrala2,"\nDiferenta=", abs(integrala2-valoare_integrala2), "\n\n")
cat("Integrala III:\nValoare estimata=",integrala3, "\nValoare exacta=", valoare_integrala3,"\nDiferenta=", abs(integrala3-valoare_integrala3), "\n\n")

#B4.

numar_useri_initial<-10000
tinta_useri<-15000
n<-1000
p<-0.25
q<-0.01 #prob retragere user

#a)
simulare_a<-function()
{
  numar_useri<-numar_useri_initial
  ani<-0
  
  while(numar_useri<tinta_useri)
  {
    useri_noi<-rbinom(1,n,p)
    useri_retrasi<-rbinom(1,numar_useri,q)
    numar_useri<-numar_useri+useri_noi-useri_retrasi
    ani<-ani+1
  }
  
  return(ani)
}

nr_simulari<-1000
rezultate_a<-replicate(nr_simulari,simulare_a())
nr_mediu_ani<-mean(rezultate)
cat("Numarul mediu de ani pana cand iSocialize va avea cel putin 15000 de useri: ",nr_mediu_ani)

#b)
numar_luni<-40*12+10 #40 ani + 10 luni

simulare_b<-function()
{
  numar_useri<-numar_useri_initial
  
  for(i in 1:numar_luni)
  {
    useri_noi<-rbinom(1,n,p)
    useri_retrasi<-rbinom(1,numar_useri,q)
    numar_useri<-numar_useri+useri_noi-useri_retrasi
  }
  
  return(numar_useri>=tinta_useri)
}
rezultate_b<-replicate(nr_simulari,simulare_b())
probabilitate <- mean(rezultate_b)
cat("Probabilitatea ca dupa 40 de ani si 10 luni sa existe cel putin 15000 de utilizatori in retea este: ", probabilitate)

#c)