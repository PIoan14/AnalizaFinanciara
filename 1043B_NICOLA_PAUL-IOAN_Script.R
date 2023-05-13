date_lucru <- read.csv("DataStarbucksMC.csv", header = TRUE, sep = ",")
date_txt <- read.table("DataStarbucksMC.txt", header = TRUE, sep = "\t")


date_actiuni <- data.frame(date_lucru $Pret_StarBucks, date_lucru $Pret_MC)

attach(date_actiuni)

plot.ts(date_lucru.Pret_StarBucks, main = "Evolutia pretului de inchidere al actiunilor StarBucks"
        ,type = "b", col = "blue")

min(date_lucru.Pret_StarBucks)

max(date_lucru.Pret_StarBucks)

a <- max(date_lucru.Pret_StarBucks) -min(date_lucru.Pret_StarBucks)
a



which.max(date_lucru.Pret_StarBucks)


plot.ts(date_lucru.Pret_MC, main = "Evolutia pretului de inchidere al actiunilor MC",
        type = "b", col = "magenta")
max(date_lucru.Pret_MC)

mean(date_lucru.Pret_MC)

min(date_lucru.Pret_MC)

k <-max(date_lucru.Pret_MC)-min(date_lucru.Pret_MC)
k

date_rentabilitati<-read.csv("rentabilitati.csv", header= TRUE, sep = ",")

date_rentabilitati_txt <- read.table("rentabilitati.txt", header = TRUE, sep ="\t")

date_rent_r <- data.frame(date_rentabilitati $Rentabilitati, date_rentabilitati $ModificariRentabilitati,
                          date_rentabilitati $SP, date_rentabilitati $RentabilitatiSP , 
                       date_rentabilitati $RentabilitatiMC )
attach(date_rent_r)

sirRentab <- c(date_rentabilitati.Rentabilitati)[-252]
mean(sirRentab)


amplitRent = max(sirRentab)- min(sirRentab)
amplitRent

modificari <- c(date_rentabilitati.ModificariRentabilitati)[-252]
min(modificari)
mean(modificari)
mean(sirRentab)

w <-max(modificari)-min(modificari)
w

summary(date_rentabilitati)

library(moments)

sk_rentabilitati <- skewness(sirRentab)
sk_rentabilitati

ks_rentabilitati <- kurtosis(sirRentab)
ks_rentabilitati

ab_std<-sd(sirRentab)
ab_std

indiciSP <- c(date_rentabilitati.SP)
indiciSP[2]

min(indiciSP)
mean(indiciSP)

max(indiciSP)

amplitSP <- max(indiciSP) -min(indiciSP)
amplitSP

sd(indiciSP)


A <- matrix(ncol = 3 ,nrow = 2, dimnames = list(c("distributie", "aplatizare"),
                                                c("Starbucks", "Mc Donald's", "S&P500")))


sk_starbucks = skewness(date_lucru.Pret_StarBucks)
sk_starbucks

sk_mc = skewness(date_lucru.Pret_MC)
sk_mc

sk_SP = skewness(date_rentabilitati.SP)
sk_SP

A[1,] <- c(sk_starbucks, sk_mc, sk_SP)


k_starbucks = kurtosis(date_lucru.Pret_StarBucks)
k_starbucks

k_mc = kurtosis(date_lucru.Pret_MC)
k_mc

k_SP = kurtosis(date_rentabilitati.SP)
k_SP

A[2,] <-c(k_starbucks, k_mc, k_SP)

View(A)


library(corrplott)

preturi <- read.csv("preturi.csv", header = TRUE, sep = ",")

preturi_t <- read.table("preturi.txt", header = TRUE, sep = "\t")

preturi_f <- data.frame(preturi $Pret_StarBucks, preturi $Pret_MC,
                          preturi $SP)
attach(preturi_f)


B<- cor(preturi)
View(B)

corrplot(B)

plot(preturi.SP, type = "l", main = "actiuni starbucks si S&P",ylim = c(0,4999))
lines(preturi.Pret_StarBucks, type = "b", col = "blue")


plot(date_rentabilitati.Rentabilitati, main ="rentabilitati starbucks si S&P500", type = "b", col = "magenta")
lines(date_rentabilitati.RentabilitatiSP, type = "l")

x <- c(date_rentabilitati.RentabilitatiSP)




plot(date_rentabilitati.RentabilitatiMC, main= "Rntabilitati Mc Donald's si s&p500", type = "b",
     col = "green")
lines(date_rentabilitati.RentabilitatiSP, type = "l")

# DE AICI IN JOS SE AFLA GRAFICE SECUNDARE CARE, DESI NU AU FOST INTRODUSE IN PREZENTARE, AU AJUTAT LA REALIZAREA ACESTEIA. PENTRU A FI MAI USOR DE SEPARAT DE CELE PRINCIPALE, AM DECIS SA LE PUN PE TOATE LA FINAL.


hist(sirRentab, main = "Rentabilitati 2021", col = "red")

plot(indiciSP, col= 'yellow')

plot(indiciSP)

plot.ts(date_rentabilitati.Rentabilitati, main = "Rentabilitati SBX",  col = "blue",
        type = "b")


plot(indiciSP, col= 'yellow')

plot(indiciSP, type  = "l")



