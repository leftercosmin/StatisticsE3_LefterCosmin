#A1.
probabilitati<-function(lambda, p,n,k,m)
{
  
  #a)
  valori<-k:m
  poisson<- dpois(valori,lambda)
  geometric<-dgeom(valori-1, p)
  binomial<-dbinom(valori,n,p)
  
  #b)
  #par(mfrow=c(3,1))
  
  barplot(main="Poisson",poisson, names.arg=valori, col = "cornflowerblue", xlab = "valori", ylab = "probabilitate")
  barplot(main="Geometric",geometric, names.arg = valori, col = "steelblue3",xlab = "valori", ylab = "probabilitate")
  barplot(main="Binomial",binomial, names.arg = valori, col = "skyblue3",xlab = "valori", ylab = "probabilittate")
}

lambda <- 2
p <- 0.5
n <- 10
k <- 0
m <- 4
rez <- probabilitati(lambda, p, n, k, m)
print(rez)

#c)
cel_mai_mic_k<-function(lambda) 
{
  k<-0
  limita<-1 - 10^(-6)
  
  while (ppois(k, lambda) <= limita)
  {
    k<-k + 1
  }
}

k0<-cel_mai_mic_k(lambda)
print(k0)

#A2.
#a)
nume_fisier<-"C:/Users/lefco/Downloads/note_PS.csv"

note_statistica<-function(nume_fisier)
{
  data<-read.csv(nume_fisier)
  
  S<-data$S
  P<-data$P
  
  mean_S<-mean(S)
  mean_P<-mean(P)
  
  frec_abs_S<-table(S)
  frec_abs_P<-table(P)
  
  frec_rel_S<-as.vector(frec_abs_S)/length(S)
  frec_rel_P<-as.vector(frec_abs_P)/length(P)
  
  cat("frecvente absolute, frecvente relative si media pentru P:\n")
  print(frec_abs_P)
  cat(", ")
  print(frec_rel_P)
  cat(", ")
  print(mean_P)
  cat("\n")
  
  cat("frecvente absolute, frecvente relative si media pentru S:\n")
  print(frec_abs_S)
  cat(", ")
  print(frec_rel_S)
  cat(", ")
  print(mean_S)
  cat("\n")
}
#b)

note_statistica2<-function(nume_fisier,esantion)
{
  esantion<-data[[esantion]] #acelasi data ca in functia de mai sus
  
  Q1<-quantile(esantion, 0.25)
  Q3<-quantile(esantion, 0.75)
  IQR_val<-Q3 - Q1
  
  lim_inf<-Q1 - 1.5 * IQR_val
  lim_sup<-Q3 + 1.5 * IQR_val
  
  esantion2<-esantion[esantion>lim_inf & esantion<lim_sup]
  intervale<-seq(1, 10, by = 1)
  frec<-hist(esantion2, breaks = intervale, plot = FALSE)$counts
  
  barplot(frec, names.arg = paste0("(", intervale[-length(intervale)], ",", intervale[-1], "]"),xlab = "intervale", ylab = "fercvente", col = "khaki1")
  return(esantion2)
}

esantion<-"P"
esantionP<-note_statistica2(nume_fisier,esantion)