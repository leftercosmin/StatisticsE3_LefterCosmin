#C1.
#a)
gen_permutare<-function(n)
{
  valori<-runif(n)
  permutare<-order(valori)
  return(permutare)
}

gen_siruribiti<-function(n,k)
{
  siruribiti<-matrix(sample(c(0,1),n*k, replace=TRUE), ncol=k)
  return(siruribiti)
}

n<-10
k<-5

permutare<-gen_permutare(n)
cat("Permutarea aleatoare:", permutare, "\n")

siruribiti<-gen_siruribiti(n,k)
cat("Sirurile de biti generatte:")
siruribiti

#b)
comparatie_lexicografica<-function(Wi,Wj)
{
  Lij <- min(length(Wi), length(Wj))
  
  for (l in 1:Lij)
  {
    if (Wi[l] < Wj[l])
      return(TRUE)
    else if (Wi[l] > Wj[l])
      return(FALSE)
  }

  while(length(Wi)<=length(Wj))
  {
    bit<-sample(c(0,1),1) #bit aleator
    Wi <- c(Wi,bit) #se adauga bitul ^ la sfarsitul lui Wi
    if(Wi<Wj)
      return(TRUE)
    else if(Wi > Wj)
      return(FALSE)
  }
  
  return(FALSE)
}

Wi <- c(1, 0, 1, 0)
Wj <- c(1, 0, 1, 1)
rezultat <- comparatie_lexicografica(Wi, Wj)
if (rezultat)
  print("Wi este lexicografic mai mic decat Wj")
else
  print("Wi nu este lexicografic mai mic decat Wj")

#c)
random_quickSort<-function(lista)
{
  partajare<-function(lista,stanga,dreapta)
  {
  index_pivot<-sample(stanga:dreapta,1)
  pivot<-lista[index_pivot]
  lista[index_pivot]>lista[dreapta]
  lista[dreapta]<-pivot
  
  i<-stanga-1
  for(j in stanga:(dreapta-1))
  {
    if(comparatie_lexicografica(lista[j],pivot))
    {
      i<-i+1
      temp<-lista[i]
      lista[i]<-lista[j]
      lista[j]<-temp
    }
  }
  temp<-lista[i+1]
  lista[i+1]<-lista[dreapta]
  lista[dreapta]<-temp
  return(i+1)
  }

quicksort_rec<-function(lista,stanga,dreapta) #pt sortare recursiva
{
  if(stanga<dreapta)
  {
    index_pivot<-partajare(lista,stanga,dreapta)
    quicksort_rec(lista,stanga,pivot_index-1)
    quicksort_rec(lista,pivot_index+1,dreapta)
  }
}

quicksort_rec(lista,1,length(lista))
}

#d)
permutare_aleatoare<-function(n)
{
#permutarea initiala
  permutare<-sample(1:n)
  return(permutare)
}

gen_cuvinte<-function(n, k)
{
  cuvinte<-matrix(sample(c(0, 1), n * k, replace = TRUE), ncol = k)
  return(cuvinte)
}

quicksort_rec(cuvinte, permutare, 1, length(permutare))

sortare_si_permutare <- function(n, k) {
  cuvinte<-gen_cuvinte(n, k)
  
  permutare<-permutare_aleatoare(n)
  
  random_quicksort(cuvinte, permutare)
  
  return(permutare)
}

#C2.
#a)
graf_gen<-function(nr_noduri,nr_muchii)
{
  muchii<-matrix(NA,ncol=2,nrow=nr_muchii)
  
  for(i in 1:nr_muchii)
  {
    muchii[i, ]<-sample(1:nr_noduri,2,replace=FALSE)
  }
  return(muchii)
}

taietura_max<-function(nr_noduri,nr_muchii,graf)
{
  n<-floor(nr_noduri/2)
  A<-sample(1:nr_noduri,n,replace=FALSE)
  B<-setdiff(1:nr_noduri,A)
  
  cardinal_taietura<-0
  
  for(muchie in 1:nr_muchii)
  {
    u<-graf[muchie,1]
    v<-graf[muchie,2]
  }
  
  if ((u %in% A && v %in% B) || (u %in% B && v %in% A))
    cardinal_taietura <- cardinal_taietura + 1
  
  return(cardinal_taietura)
}

nr_noduri<-4
nr_muchii<-7
graf<-graf_gen(nr_noduri,nr_muchii)
print(graf)
#b)