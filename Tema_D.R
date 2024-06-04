#D1.

date<-read.csv("C:/Users/lefco/Downloads/probabilitati.csv")
interval99<-t.test(date$probabilitati, conf.leve=0.99)$conf.int
interval95<-t.test(date$probabilitati, conf.leve=0.95)$conf.int

cat("Intervalul de incredere de 99%: pentru medie este", interval99)
cat("Intervalul de incredere de 95%: pentru medie este", interval95)

#D2.
date<-read.csv("C:/Users/lefco/Downloads/statistica.csv")
media<-mean(data$statistica)
devstand<-sd(data$statistica)
n<-length(data$statistica)

t_critic_95<-qt(1 - 0.05/2, df = n - 1)
t_critic_99<-qt(1 - 0.01/2, df = n - 1)
margin_eroare_95<-t_critic_95 * dev_stand / sqrt(n)
margin_eroare_99<-t_critic_99 * dev_stand / sqrt(n)
interval_95<-c(media - margin_eroare_95, media + margin_eroare_95)
interval_99<-c(media - margin_eroare_99, media + margin_eroare_99)
cat("interval de incredere de 95% pentru media punctajelor:", interval_95, "\n")
cat("interval de incredere de 99% pentru media punctajelor:", interval_99, "\n")

#D3