#Serpa et al BEHAVIORAL REPERTOIRE OF LAHILLE’S BOTTLENOSE DOLPHINS DURING THE “COOPERATIVE FISHING” WITH ARTISANAL FISHERS

library("mgcv")#gam
library("tidyverse")#normality test
library("ggplot2")
library("rstatix")#Shapiro_test
library("Hmisc")#spearman test
library("writexl")

setwd("C:/Users/User/Desktop/boto")
data<-read.csv("boto_29012021.csv", header=T)

data2<- data [, -c(1)]

#Normality

normalidade <- data2 %>% shapiro_test(Active_surfacing, Porpoise, Leap, Humping_surface, Regular_dive, Peduncle_dive, Tail_out_dive,
                                      Fast_swim, Sharking, Head_slap, Side_slap, Underwater_tail.slap, Horizontal_circle, Pinwheeling, Side_roll, Bubble.blow, Cast_net)
view(normalidade)

data16 <- data2[,-c(10,13,15,18,19,21,22,23,24,25,26,28)]


#Fligner-Killen test (variances homogeneity)
fligner <- data2 %>% fligner.test(Active_surfacing, Porpoise, Leap, Humping_surface, Regular_dive, Peduncle_dive, Tail_out_dive,
                                  Fast_swim, Sharking, Head_slap, Side_slap, Underwater_tail.slap, Horizontal_circle, Pinwheeling, Side_roll, Bubble.blow)
view(fligner)
o<- fligner.test(Cast_net ~ interaction(Active_surfacing, Porpoise, Leap, Humping_surface), data=data2)
view(o)

#Spearman
corr_var <- rcorr(as.matrix(data16), type = c("spearman"))
corr_var$P
view(corr_var$P)
view(corr_var)


write.table(corr_var$P,'clipboard', sep = '\t')

write.csv(corr)
#selecting data
naosignificativos <- corr_var$P[which(corr_var$P<0.05)]
#,(corr_var$P<0.01)#inside []

view(naosignificativos)

data16wcastnet <- data2[,-c(10,13,15,18,19,21,22,23,24,25,26)]


dat <- data16wcastnet[data16wcastnet$Cast_net!=0,]#excluindo os zeros da análise
dat <- dat[dat$Cast_net<0.5,]
dat <- dat[gam_Active_surfacing$fitted.values<0.25,]#excluindo os outliers da primeira gam realizada
str(dat)

#1 Round


#gam_Active_surfacingI <- gam(Cast_net ~ s(Active_surfacing, k=3), data = data16wcastnet, method = "GCV.Cp")
#gam_Active_surfacing <- gam(Cast_net ~ s(Active_surfacing, k=5), data = dat, family = poisson())

gam_Active_surfacing <- gam(Cast_net ~ s(Active_surfacing, k=3), data = dat, method = "GCV.Cp")
plot(gam_Active_surfacing,pages=1,residuals=TRUE,all.terms=TRUE,shade=TRUE,shade.col="gray80")
gam_Active_surfacing$aic#-103.2511
summary(gam_Active_surfacing)
par(mfrow=c(2,2))
gam.check(gam_Active_surfacing)

gam_Porpoise <- gam(Cast_net ~ s(Porpoise, k=5), data = dat, method = "GCV.Cp")
gam_Porpoise$aic#-103.6379

gam_Leap <- gam(Cast_net ~ s(Leap, k=3), data = dat, method = "GCV.Cp")
gam_Leap$aic#-103.0974

gam_Humping_surface <- gam(Cast_net ~ s(Humping_surface, k=5), data = dat, method = "GCV.Cp")
gam_Humping_surface$aic#-102.7452

gam_Regular_dive <- gam(Cast_net ~ s(Regular_dive, k=10), data = dat, method = "GCV.Cp")
gam_Regular_dive$aic#-103.7374
plot(gam_Regular_dive,pages=1,residuals=TRUE,all.terms=TRUE,shade=TRUE,shade.col="gray80")


gam_Peduncle_dive <- gam(Cast_net ~ s(Peduncle_dive, k=6), data = dat, method = "GCV.Cp")
gam_Peduncle_dive$aic#-103.8586

gam_Tail_out_dive <- gam(Cast_net ~ as.factor(Tail_out_dive), data = dat, method = "GCV.Cp")
gam_Tail_out_dive$aic#-93.10441

gam_Fast_swim <- gam(Cast_net ~ s(Fast_swim, k=5), data = dat, method = "GCV.Cp")
gam_Fast_swim$aic#-106.7913

gam_Sharking <- gam(Cast_net ~ s(Sharking, k=6), data = dat, method = "GCV.Cp")
plot(gam_Sharking,pages=1,residuals=TRUE,all.terms=TRUE,shade=TRUE,shade.col="gray80")
gam_Sharking$aic#-103.7801

gam_Head_slap <- gam(Cast_net ~ s(Head_slap, k=7), data = dat, method = "GCV.Cp")

par(mfrow=c(2,2))
gam.check(gam_Head_slap)

plot(gam_Head_slap,pages=1,residuals=TRUE,all.terms=TRUE,shade=TRUE,shade.col="gray80")
summary(gam_Head_slap)
gam_Head_slap$aic#-107.1627

gam_Side_slap <- gam(Cast_net ~ s(Side_slap, k=7), data = dat, method = "GCV.Cp")
gam_Side_slap$aic#-103.0643

gam_Underwater_tail.slap <- gam(Cast_net ~ s(Underwater_tail.slap, k=5), data = dat, method = "GCV.Cp")
gam_Underwater_tail.slap$aic#-103.7646

gam_Horizontal_circle <- gam(Cast_net ~ s(Horizontal_circle, k=4), data = dat, method = "GCV.Cp")
gam_Horizontal_circle$aic#-102.8893

gam_Pinwheeling <- gam(Cast_net ~ s(Pinwheeling, k=3), data = dat, method = "GCV.Cp")
gam_Pinwheeling$aic#-102.565

gam_Side_roll <- gam(Cast_net ~ s(Side_roll, k=4), data = dat, method = "GCV.Cp")
gam_Side_roll$aic#-103.1135

gam_Bubble.blow <- gam(Cast_net ~ s(Bubble.blow, k=3), data = dat, method = "GCV.Cp")
gam_Bubble.blow$aic#-104.8728

#2 Round

gam_Head_slap_Active <- gam(Cast_net ~ s(Head_slap, k=7) + s(Active_surfacing, k=5), data = dat, method = "GCV.Cp")
gam_Head_slap_Active$aic#-105.4382

gam_Head_slap_Porpoise <- gam(Cast_net ~ s(Head_slap, k=7) + s(Porpoise, k=5), data = dat, method = "GCV.Cp")
gam_Head_slap_Porpoise$aic#-106.6278

gam_Head_slap_Leap <- gam(Cast_net ~ s(Head_slap, k=7) + s(Leap, k=3), data = dat, method = "GCV.Cp")
gam_Head_slap_Leap$aic#-105.4754

gam_Head_slap_Humping_surface <- gam(Cast_net ~ s(Head_slap, k=7) + s(Humping_surface, k=5), data = dat, method = "GCV.Cp")
gam_Head_slap_Humping_surface$aic#-105.2202

gam_Head_slap_Regular_dive <- gam(Cast_net ~ s(Head_slap, k=7) + s(Regular_dive, k=10), data = dat, method = "GCV.Cp")
gam_Head_slap_Regular_dive$aic#-107.1578

gam_Head_slap_Peduncle_dive <- gam(Cast_net ~ s(Head_slap, k=7) + s(Peduncle_dive, k=6), data = dat, method = "GCV.Cp")
gam_Head_slap_Peduncle_dive$aic#-108.6158

gam_Head_slap_Tailoutdive <- gam(Cast_net ~ s(Head_slap, k=7) + as.factor(Tail_out_dive), data = dat, method = "GCV.Cp")
gam_Head_slap_Tailoutdive$aic#-96.29713

gam_Head_slap_Fast_swim <- gam(Cast_net ~ s(Head_slap, k=7) + s(Fast_swim, k=5), data = dat, method = "GCV.Cp")
gam_Head_slap_Fast_swim$aic#-108.9169*********

gam_Head_slap_Sharking <- gam(Cast_net ~ s(Head_slap, k=7) + s(Sharking, k=6), data = dat, method = "GCV.Cp")
gam_Head_slap_Sharking$aic#-106.6295

gam_Head_slap_Underwater_tail.slap <- gam(Cast_net ~ s(Head_slap, k=7) + s(Underwater_tail.slap, k=5), data = dat, method = "GCV.Cp")
gam_Head_slap_Underwater_tail.slap$aic#-106.15

gam_Head_slap_Horizontal_circle <- gam(Cast_net ~ s(Head_slap, k=7) + s(Horizontal_circle, k=4), data = dat, method = "GCV.Cp")
gam_Head_slap_Horizontal_circle$aic#-105.2454

gam_Head_slap_Pinwheeling <- gam(Cast_net ~ s(Head_slap, k=7) + s(Pinwheeling, k=3), data = dat, method = "GCV.Cp")
gam_Head_slap_Pinwheeling$aic#-105.1699

gam_Head_slap_Sideroll <- gam(Cast_net ~ s(Head_slap, k=7) + s(Side_roll, k=4), data = dat, method = "GCV.Cp")
gam_Head_slap_Sideroll$aic#-105.393

gam_Head_slap_Bubble.blow <- gam(Cast_net ~ s(Head_slap, k=7) + s(Bubble.blow, k=3), data = dat, method = "GCV.Cp")
gam_Head_slap_Bubble.blow$aic#-107.6786

#3 Round

gam_Head_slap_Fast_swim_Active_surfacing <- gam(Cast_net ~ s(Head_slap, k=7) + s(Fast_swim, k=5) + s(Active_surfacing, k=5), data = dat, method = "GCV.Cp")
gam_Head_slap_Fast_swim_Active_surfacing$aic#-106.9531

gam_Head_slap_Fast_swim_Porpoise <- gam(Cast_net ~ s(Head_slap, k=7) + s(Fast_swim, k=5) + s(Porpoise, k=5), data = dat, method = "GCV.Cp")
gam_Head_slap_Fast_swim_Porpoise$aic#-107.9242

gam_Head_slap_Fast_swim_Leap <- gam(Cast_net ~ s(Head_slap, k=7) + s(Fast_swim, k=5) + s(Leap, k=3), data = dat, method = "GCV.Cp")
gam_Head_slap_Fast_swim_Leap$aic#-107.1319

gam_Head_slap_Fast_swim_Humping_surface <- gam(Cast_net ~ s(Head_slap, k=7) + s(Fast_swim, k=5) + s(Humping_surface, k=5), data = dat, method = "GCV.Cp")
gam_Head_slap_Fast_swim_Humping_surface$aic#-107.0071

gam_Head_slap_Fast_swim_Regular_dive <- gam(Cast_net ~ s(Head_slap, k=7) + s(Fast_swim, k=5) + s(Regular_dive, k=10), data = dat, method = "GCV.Cp")
gam_Head_slap_Fast_swim_Regular_dive$aic#-109.6137

gam_Head_slap_Fast_swim_Peduncle_dive <- gam(Cast_net ~ s(Head_slap, k=7) + s(Fast_swim, k=5) + s(Peduncle_dive, k=6), data = dat, method = "GCV.Cp")
gam_Head_slap_Fast_swim_Peduncle_dive$aic#-110.5171**********

gam_Head_slap_Fast_swim_Tailoutdive <- gam(Cast_net ~ s(Head_slap, k=7) + s(Fast_swim, k=5) + as.factor(Tail_out_dive), data = dat, method = "GCV.Cp")
gam_Head_slap_Fast_swim_Tailoutdive$aic#-98.01365

gam_Head_slap_Fast_swim_Sharking <- gam(Cast_net ~ s(Head_slap, k=7) + s(Fast_swim, k=5) + s(Sharking, k=6), data = dat, method = "GCV.Cp")
gam_Head_slap_Fast_swim_Sharking$aic#-108.9177

gam_Head_slap_Fast_swim_Underwater_tail.slap <- gam(Cast_net ~ s(Head_slap, k=7) + s(Fast_swim, k=5) + s(Underwater_tail.slap, k=5), data = dat, method = "GCV.Cp")
gam_Head_slap_Fast_swim_Underwater_tail.slap$aic#-108.0044

gam_Head_slap_Fast_swim_Horizontal_circle <- gam(Cast_net ~ s(Head_slap, k=7) + s(Fast_swim, k=5) + s(Horizontal_circle, k=4), data = dat, method = "GCV.Cp")
gam_Head_slap_Fast_swim_Horizontal_circle$aic#-107.1001

gam_Head_slap_Fast_swim_Pinwheeling <- gam(Cast_net ~ s(Head_slap, k=7) + s(Fast_swim, k=5) + s(Pinwheeling, k=3), data = dat, method = "GCV.Cp")
gam_Head_slap_Fast_swim_Pinwheeling$aic#-107.0751

gam_Head_slap_Fast_swim_Bubble.blow <- gam(Cast_net ~ s(Head_slap, k=7) + s(Fast_swim, k=5) + s(Bubble.blow, k=3), data = dat, method = "GCV.Cp")
gam_Head_slap_Fast_swim_Bubble.blow$aic#-108.9019

gam_Head_slap_Fast_swim_Side_roll <- gam(Cast_net ~ s(Head_slap, k=7) + s(Fast_swim, k=5) + s(Side_roll, k=4), data = dat, method = "GCV.Cp")
gam_Head_slap_Fast_swim_Side_roll$aic#-107.3972

#4 Round

gam_Head_slap_Fast_swim_Peduncle_dive_Active_surfacing <- gam(Cast_net ~ s(Head_slap, k=7) + s(Fast_swim, k=5) + s(Peduncle_dive, k=6) + s(Active_surfacing, k=5), data = dat, method = "GCV.Cp")
gam_Head_slap_Fast_swim_Peduncle_dive_Active_surfacing$aic#-108.6035

gam_Head_slap_Fast_swim_Peduncle_dive_Porpoise <- gam(Cast_net ~ s(Head_slap, k=7) + s(Fast_swim, k=5) + s(Peduncle_dive, k=6) + s(Porpoise, k=5), data = dat, method = "GCV.Cp")
gam_Head_slap_Fast_swim_Peduncle_dive_Porpoise$aic#-109.2154

gam_Head_slap_Fast_swim_Peduncle_dive_Leap <- gam(Cast_net ~ s(Head_slap, k=7) + s(Fast_swim, k=5) + s(Peduncle_dive, k=6) + s(Leap, k=3), data = dat, method = "GCV.Cp")
gam_Head_slap_Fast_swim_Peduncle_dive_Leap$aic#-109.2157

gam_Head_slap_Fast_swim_Peduncle_dive_Humping_surface <- gam(Cast_net ~ s(Head_slap, k=7) + s(Fast_swim, k=5) + s(Peduncle_dive, k=6) + s(Humping_surface, k=5), data = dat, method = "GCV.Cp")
gam_Head_slap_Fast_swim_Peduncle_dive_Humping_surface$aic#-108.5632

gam_Head_slap_Fast_swim_Peduncle_dive_Regular_dive <- gam(Cast_net ~ s(Head_slap, k=7) + s(Fast_swim, k=5) + s(Peduncle_dive, k=6) + s(Regular_dive, k=10), data = dat, method = "GCV.Cp")
gam_Head_slap_Fast_swim_Peduncle_dive_Regular_dive$aic#-117.2637***********

gam_Head_slap_Fast_swim_Peduncle_dive_Tailoutdive <- gam(Cast_net ~ s(Head_slap, k=7) + s(Fast_swim, k=5) + s(Peduncle_dive, k=6) + as.factor(Tail_out_dive), data = dat, method = "GCV.Cp")
gam_Head_slap_Fast_swim_Peduncle_dive_Tailoutdive$aic#-99.6509

gam_Head_slap_Fast_swim_Peduncle_dive_Sharking <- gam(Cast_net ~ s(Head_slap, k=7) + s(Fast_swim, k=5) + s(Peduncle_dive, k=6) + s(Sharking, k=6), data = dat, method = "GCV.Cp")
gam_Head_slap_Fast_swim_Peduncle_dive_Sharking$aic#-110.3575

gam_Head_slap_Fast_swim_Peduncle_dive_Underwater_tail.slap <- gam(Cast_net ~ s(Head_slap, k=7) + s(Fast_swim, k=5) + s(Peduncle_dive, k=6) + s(Underwater_tail.slap, k=5), data = dat, method = "GCV.Cp")
gam_Head_slap_Fast_swim_Peduncle_dive_Underwater_tail.slap$aic#-109.5085

gam_Head_slap_Fast_swim_Peduncle_dive_Horizontal_circle <- gam(Cast_net ~ s(Head_slap, k=7) + s(Fast_swim, k=5) + s(Peduncle_dive, k=6) + s(Horizontal_circle, k=4), data = dat, method = "GCV.Cp")
gam_Head_slap_Fast_swim_Peduncle_dive_Horizontal_circle$aic#-108.7838

gam_Head_slap_Fast_swim_Peduncle_dive_Pinwheeling <- gam(Cast_net ~ s(Head_slap, k=7) + s(Fast_swim, k=5) + s(Peduncle_dive, k=6) + s(Pinwheeling, k=3), data = dat, method = "GCV.Cp")
gam_Head_slap_Fast_swim_Peduncle_dive_Pinwheeling$aic#-108.596

gam_Head_slap_Fast_swim_Peduncle_dive_Bubble.blow <- gam(Cast_net ~ s(Head_slap, k=7) + s(Fast_swim, k=5) + s(Peduncle_dive, k=6) + s(Bubble.blow, k=3), data = dat, method = "GCV.Cp")
gam_Head_slap_Fast_swim_Peduncle_dive_Bubble.blow$aic#-110.4049

gam_Head_slap_Fast_swim_Peduncle_dive_Side_roll <- gam(Cast_net ~ s(Head_slap, k=7) + s(Fast_swim, k=5) + s(Peduncle_dive, k=6) + s(Side_roll, k=4), data = dat, method = "GCV.Cp")
gam_Head_slap_Fast_swim_Peduncle_dive_Side_roll$aic#-108.9519

#5 Round

gam_Head_slap_Fast_swim_Peduncle_dive_Regular_dive_Active_surfacing <- gam(Cast_net ~ s(Head_slap, k=7) + s(Fast_swim, k=5) + s(Peduncle_dive, k=6) + s(Regular_dive, k=10) + s(Active_surfacing, k=5), data = dat, method = "GCV.Cp")
gam_Head_slap_Fast_swim_Peduncle_dive_Regular_dive_Active_surfacing$aic#-115.7096

gam_Head_slap_Fast_swim_Peduncle_dive_Regular_dive_Porpoise <- gam(Cast_net ~ s(Head_slap, k=7) + s(Fast_swim, k=5) + s(Peduncle_dive, k=6)+ s(Regular_dive, k=10) + s(Porpoise, k=5), data = dat, method = "GCV.Cp")
gam_Head_slap_Fast_swim_Peduncle_dive_Regular_dive_Porpoise$aic#-116.0117

gam_Head_slap_Fast_swim_Peduncle_dive_Regular_dive_Leap <- gam(Cast_net ~ s(Head_slap, k=7) + s(Fast_swim, k=5) + s(Peduncle_dive, k=6)+ s(Regular_dive, k=10) + s(Leap, k=3), data = dat, method = "GCV.Cp")
gam_Head_slap_Fast_swim_Peduncle_dive_Regular_dive_Leap$aic#-115.5191

gam_Head_slap_Fast_swim_Peduncle_dive_Regular_dive_Humping_surface <- gam(Cast_net ~ s(Head_slap, k=7) + s(Fast_swim, k=5) + s(Peduncle_dive, k=6)+ s(Regular_dive, k=10) + s(Humping_surface, k=5), data = dat, method = "GCV.Cp")
gam_Head_slap_Fast_swim_Peduncle_dive_Regular_dive_Humping_surface$aic#-115.2681

gam_Head_slap_Fast_swim_Peduncle_dive_Regular_dive_Tailoutdive <- gam(Cast_net ~ s(Head_slap, k=7) + s(Fast_swim, k=5) + s(Peduncle_dive, k=6)+ s(Regular_dive, k=10) + as.factor(Tail_out_dive), data = dat, method = "GCV.Cp")
gam_Head_slap_Fast_swim_Peduncle_dive_Regular_dive_Tailoutdive$aic#-110.514

gam_Head_slap_Fast_swim_Peduncle_dive_Regular_dive_Sharking <- gam(Cast_net ~ s(Head_slap, k=7) + s(Fast_swim, k=5) + s(Peduncle_dive, k=6)+ s(Regular_dive, k=10) + s(Sharking, k=6), data = dat, method = "GCV.Cp")
gam_Head_slap_Fast_swim_Peduncle_dive_Regular_dive_Sharking$aic#-117.494************

gam_Head_slap_Fast_swim_Peduncle_dive_Regular_dive_Underwater_tail.slap <- gam(Cast_net ~ s(Head_slap, k=7) + s(Fast_swim, k=5) + s(Peduncle_dive, k=6)+ s(Regular_dive, k=10) + s(Underwater_tail.slap, k=5), data = dat, method = "GCV.Cp")
gam_Head_slap_Fast_swim_Peduncle_dive_Regular_dive_Underwater_tail.slap$aic#-115.6819

gam_Head_slap_Fast_swim_Peduncle_dive_Regular_dive_Horizontal_circle <- gam(Cast_net ~ s(Head_slap, k=7) + s(Fast_swim, k=5) + s(Peduncle_dive, k=6)+ s(Regular_dive, k=10) + s(Horizontal_circle, k=4), data = dat, method = "GCV.Cp")
gam_Head_slap_Fast_swim_Peduncle_dive_Regular_dive_Horizontal_circle$aic#-115.6588

gam_Head_slap_Fast_swim_Peduncle_dive_Regular_dive_Pinwheeling <- gam(Cast_net ~ s(Head_slap, k=7) + s(Fast_swim, k=5) + s(Peduncle_dive, k=6)+ s(Regular_dive, k=10) + s(Pinwheeling, k=3), data = dat, method = "GCV.Cp")
gam_Head_slap_Fast_swim_Peduncle_dive_Regular_dive_Pinwheeling$aic#-117.1573

gam_Head_slap_Fast_swim_Peduncle_dive_Regular_dive_Bubble.blow <- gam(Cast_net ~ s(Head_slap, k=7) + s(Fast_swim, k=5) + s(Peduncle_dive, k=6)+ s(Regular_dive, k=10) + s(Bubble.blow, k=3), data = dat, method = "GCV.Cp")
gam_Head_slap_Fast_swim_Peduncle_dive_Regular_dive_Bubble.blow$aic#-116.4953

gam_Head_slap_Fast_swim_Peduncle_dive_Regular_dive_Side_roll <- gam(Cast_net ~ s(Head_slap, k=7) + s(Fast_swim, k=5) + s(Peduncle_dive, k=6)+ s(Regular_dive, k=10) + s(Side_roll, k=4), data = dat, method = "GCV.Cp")
gam_Head_slap_Fast_swim_Peduncle_dive_Regular_dive_Side_roll$aic#-115.3376

#6 Round

gam_Head_slap_Fast_swim_Peduncle_dive_Regular_dive_Sharking_Active_surfacing <- gam(Cast_net ~ s(Head_slap, k=7) + s(Fast_swim, k=5) + s(Peduncle_dive, k=6) + s(Regular_dive, k=10) + s(Sharking, k=6) + s(Active_surfacing, k=5), data = dat, method = "GCV.Cp")
gam_Head_slap_Fast_swim_Peduncle_dive_Regular_dive_Sharking_Active_surfacing$aic#-115.8455

gam_Head_slap_Fast_swim_Peduncle_dive_Regular_dive_Sharking_Porpoise <- gam(Cast_net ~ s(Head_slap, k=7) + s(Fast_swim, k=5) + s(Peduncle_dive, k=6)+ s(Regular_dive, k=10) + s(Sharking, k=6) + s(Porpoise, k=5), data = dat, method = "GCV.Cp")
gam_Head_slap_Fast_swim_Peduncle_dive_Regular_dive_Sharking_Porpoise$aic#-115.8113

gam_Head_slap_Fast_swim_Peduncle_dive_Regular_dive_Sharking_Leap <- gam(Cast_net ~ s(Head_slap, k=7) + s(Fast_swim, k=5) + s(Peduncle_dive, k=6)+ s(Regular_dive, k=10) + s(Sharking, k=6) + s(Leap, k=3), data = dat, method = "GCV.Cp")
gam_Head_slap_Fast_swim_Peduncle_dive_Regular_dive_Sharking_Leap$aic#-115.7472

gam_Head_slap_Fast_swim_Peduncle_dive_Regular_dive_Sharking_Humping_surface <- gam(Cast_net ~ s(Head_slap, k=7) + s(Fast_swim, k=5) + s(Peduncle_dive, k=6)+ s(Regular_dive, k=10) + s(Sharking, k=6) + s(Humping_surface, k=5), data = dat, method = "GCV.Cp")
gam_Head_slap_Fast_swim_Peduncle_dive_Regular_dive_Sharking_Humping_surface$aic#-115.6879

gam_Head_slap_Fast_swim_Peduncle_dive_Regular_dive_Sharking_Tailoutdive <- gam(Cast_net ~ s(Head_slap, k=7) + s(Fast_swim, k=5) + s(Peduncle_dive, k=6)+ s(Regular_dive, k=10) + s(Sharking, k=6) + as.factor(Tail_out_dive), data = dat, method = "GCV.Cp")
gam_Head_slap_Fast_swim_Peduncle_dive_Regular_dive_Sharking_Tailoutdive$aic#-111.2242

gam_Head_slap_Fast_swim_Peduncle_dive_Regular_dive_Sharking_Underwater_tail.slap <- gam(Cast_net ~ s(Head_slap, k=7) + s(Fast_swim, k=5) + s(Peduncle_dive, k=6)+ s(Regular_dive, k=10) + s(Sharking, k=6) + s(Underwater_tail.slap, k=5), data = dat, method = "GCV.Cp")
gam_Head_slap_Fast_swim_Peduncle_dive_Regular_dive_Sharking_Underwater_tail.slap$aic#-115.9105

gam_Head_slap_Fast_swim_Peduncle_dive_Regular_dive_Sharking_Horizontal_circle <- gam(Cast_net ~ s(Head_slap, k=7) + s(Fast_swim, k=5) + s(Peduncle_dive, k=6)+ s(Regular_dive, k=10) + s(Sharking, k=6) + s(Horizontal_circle, k=4), data = dat, method = "GCV.Cp")
gam_Head_slap_Fast_swim_Peduncle_dive_Regular_dive_Sharking_Horizontal_circle$aic#-115.528

gam_Head_slap_Fast_swim_Peduncle_dive_Regular_dive_Sharking_Pinwheeling <- gam(Cast_net ~ s(Head_slap, k=7) + s(Fast_swim, k=5) + s(Peduncle_dive, k=6)+ s(Regular_dive, k=10) + s(Sharking, k=6) + s(Pinwheeling, k=3), data = dat, method = "GCV.Cp")
gam_Head_slap_Fast_swim_Peduncle_dive_Regular_dive_Sharking_Pinwheeling$aic#-117.1978

gam_Head_slap_Fast_swim_Peduncle_dive_Regular_dive_Sharking_Bubble.blow <- gam(Cast_net ~ s(Head_slap, k=7) + s(Fast_swim, k=5) + s(Peduncle_dive, k=6)+ s(Regular_dive, k=10) + s(Sharking, k=6) + s(Bubble.blow, k=3), data = dat, method = "GCV.Cp")
gam_Head_slap_Fast_swim_Peduncle_dive_Regular_dive_Sharking_Bubble.blow$aic#-117.2292

gam_Head_slap_Fast_swim_Peduncle_dive_Regular_dive_Sharking_Side_roll <- gam(Cast_net ~ s(Head_slap, k=7) + s(Fast_swim, k=5) + s(Peduncle_dive, k=6)+ s(Regular_dive, k=10) + s(Sharking, k=6) + s(Side_roll, k=4), data = dat, method = "GCV.Cp")
gam_Head_slap_Fast_swim_Peduncle_dive_Regular_dive_Sharking_Side_roll$aic#-115.5383

#Winner
summary(gam_Head_slap_Fast_swim_Peduncle_dive_Regular_dive_Sharking)
par(mfrow=c(2,2))
gam.check(gam_Head_slap_Fast_swim_Peduncle_dive_Regular_dive_Sharking)

#GAM model
par(mfrow = c(3,3))
plot(gam_Fast_swim_Leap_Horiz_Sider_RegD_Shark_sideslap_hump_peddive,select = 1, pages=0,residuals=TRUE,all.terms=TRUE,shade=TRUE,shade.col="gray80", 
     cex.lab = 2, cex.axis = 1.5, cex.main = 1.5, cex.sub = 1.5, ylab = " ", xlab="Fast swim", rug = TRUE, seWithMean=TRUE)
plot(gam_Fast_swim_Leap_Horiz_Sider_RegD_Shark_sideslap_hump_peddive,select = 2, pages=0,residuals=TRUE,all.terms=TRUE,shade=TRUE,shade.col="gray80", 
     cex.lab = 2, cex.axis = 1.5, cex.main = 1.5, cex.sub = 1.5, ylab = " ", xlab="Leap", rug = TRUE, seWithMean=TRUE)
plot(gam_Fast_swim_Leap_Horiz_Sider_RegD_Shark_sideslap_hump_peddive,select = 3, pages=0,residuals=TRUE,all.terms=TRUE,shade=TRUE,shade.col="gray80", 
     cex.lab = 2, cex.axis = 1.5, cex.main = 1.5, cex.sub = 1.5, ylab = " ", xlab="Horizontal circle", rug = TRUE, seWithMean=TRUE)
plot(gam_Fast_swim_Leap_Horiz_Sider_RegD_Shark_sideslap_hump_peddive,select = 4, pages=0,residuals=TRUE,all.terms=TRUE,shade=TRUE,shade.col="gray80", 
     cex.lab = 2, cex.axis = 1.5, cex.main = 1.5, cex.sub = 1.5, ylab = " ", xlab="Side roll", rug = TRUE, seWithMean=TRUE)
plot(gam_Fast_swim_Leap_Horiz_Sider_RegD_Shark_sideslap_hump_peddive,select = 5, pages=0,residuals=TRUE,all.terms=TRUE,shade=TRUE,shade.col="gray80", 
     cex.lab = 2, cex.axis = 1.5, cex.main = 1.5, cex.sub = 1.5, ylab = " ", xlab="Regular dive", rug = TRUE, seWithMean=TRUE)
plot(gam_Fast_swim_Leap_Horiz_Sider_RegD_Shark_sideslap_hump_peddive,select = 6, pages=0,residuals=TRUE,all.terms=TRUE,shade=TRUE,shade.col="gray80", 
     cex.lab = 2, cex.axis = 1.5, cex.main = 1.5, cex.sub = 1.5, ylab = " ", xlab="Sharking", rug = TRUE, seWithMean=TRUE)
plot(gam_Fast_swim_Leap_Horiz_Sider_RegD_Shark_sideslap_hump_peddive,select = 7, pages=0,residuals=TRUE,all.terms=TRUE,shade=TRUE,shade.col="gray80", 
     cex.lab = 2, cex.axis = 1.5, cex.main = 1.5, cex.sub = 1.5, ylab = " ", xlab="Side slap", rug = TRUE, seWithMean=TRUE)
plot(gam_Fast_swim_Leap_Horiz_Sider_RegD_Shark_sideslap_hump_peddive,select = 8, pages=0,residuals=TRUE,all.terms=TRUE,shade=TRUE,shade.col="gray80", 
     cex.lab = 2, cex.axis = 1.5, cex.main = 1.5, cex.sub = 1.5, ylab = " ", xlab="Humping surface", rug = TRUE, seWithMean=TRUE)
plot(gam_Fast_swim_Leap_Horiz_Sider_RegD_Shark_sideslap_hump_peddive,select = 9, pages=0,residuals=TRUE,all.terms=TRUE,shade=TRUE,shade.col="gray80", 
     cex.lab = 2, cex.axis = 1.5, cex.main = 1.5, cex.sub = 1.5, ylab = " ", xlab="Peduncle dive", rug = TRUE, seWithMean=TRUE)


#1 Round

gam_Active_surfacing <- gam(Cast_net ~ s(Active_surfacing, k=3), data = dat, method = "GCV.Cp")
plot(gam_Active_surfacing,pages=1,residuals=TRUE,all.terms=TRUE,shade=TRUE,shade.col="gray80")
gam_Active_surfacing$aic#-118.7074
summary(gam_Active_surfacing)
par(mfrow=c(2,2))
gam.check(gam_Active_surfacing)

gam_Porpoise <- gam(Cast_net ~ s(Porpoise, k=5), data = dat, method = "GCV.Cp")
gam_Porpoise$aic#-118.2915

gam_Leap <- gam(Cast_net ~ s(Leap, k=3), data = dat, method = "GCV.Cp")
gam_Leap$aic#-117.0162

gam_Humping_surface <- gam(Cast_net ~ s(Humping_surface, k=5), data = dat, method = "GCV.Cp")
gam_Humping_surface$aic#-116.5459

gam_Regular_dive <- gam(Cast_net ~ s(Regular_dive, k=7), data = dat, method = "GCV.Cp")
gam_Regular_dive$aic#-118.6331
plot(gam_Regular_dive,pages=1,residuals=TRUE,all.terms=TRUE,shade=TRUE,shade.col="gray80")

gam_Peduncle_dive <- gam(Cast_net ~ s(Peduncle_dive, k=6), data = dat, method = "GCV.Cp")
gam_Peduncle_dive$aic#-118.1383

gam_Tail_out_dive <- gam(Cast_net ~ as.factor(Tail_out_dive), data = dat, method = "GCV.Cp")
gam_Tail_out_dive$aic#-107.8509

gam_Fast_swim <- gam(Cast_net ~ s(Fast_swim, k=5), data = dat, method = "GCV.Cp")
gam_Fast_swim$aic#-119.9736

gam_Sharking <- gam(Cast_net ~ s(Sharking, k=6), data = dat, method = "GCV.Cp")
plot(gam_Sharking,pages=1,residuals=TRUE,all.terms=TRUE,shade=TRUE,shade.col="gray80")
gam_Sharking$aic#-118.3265

gam_Head_slap <- gam(Cast_net ~ s(Head_slap, k=5), data = dat, method = "GCV.Cp")
#par(mfrow=c(2,2))
#gam.check(gam_Head_slap)
#plot(gam_Head_slap,pages=1,residuals=TRUE,all.terms=TRUE,shade=TRUE,shade.col="gray80")
#summary(gam_Head_slap)
gam_Head_slap$aic#-124.3889

gam_Side_slap <- gam(Cast_net ~ s(Side_slap, k=5), data = dat, method = "GCV.Cp")
gam_Side_slap$aic#-118.3865

gam_Underwater_tail.slap <- gam(Cast_net ~ s(Underwater_tail.slap, k=5), data = dat, method = "GCV.Cp")
gam_Underwater_tail.slap$aic#-118.2037

gam_Horizontal_circle <- gam(Cast_net ~ s(Horizontal_circle, k=4), data = dat, method = "GCV.Cp")
gam_Horizontal_circle$aic#-117.1335

gam_Pinwheeling <- gam(Cast_net ~ s(Pinwheeling, k=3), data = dat, method = "GCV.Cp")
gam_Pinwheeling$aic#-116.5093

gam_Side_roll <- gam(Cast_net ~ s(Side_roll, k=4), data = dat, method = "GCV.Cp")
gam_Side_roll$aic#-117.3285

gam_Bubble.blow <- gam(Cast_net ~ s(Bubble.blow, k=3), data = dat, method = "GCV.Cp")
gam_Bubble.blow$aic#-117.302

#2 Round

gam_Head_slap_Active <- gam(Cast_net ~ s(Head_slap, k=5) + s(Active_surfacing, k=3), data = dat, method = "GCV.Cp")
gam_Head_slap_Active$aic#-124.0001

gam_Head_slap_Porpoise <- gam(Cast_net ~ s(Head_slap, k=5) + s(Porpoise, k=5), data = dat, method = "GCV.Cp")
gam_Head_slap_Porpoise$aic#-124.7866

gam_Head_slap_Leap <- gam(Cast_net ~ s(Head_slap, k=5) + s(Leap, k=3), data = dat, method = "GCV.Cp")
gam_Head_slap_Leap$aic#-122.5525

gam_Head_slap_Humping_surface <- gam(Cast_net ~ s(Head_slap, k=5) + s(Humping_surface, k=5), data = dat, method = "GCV.Cp")
gam_Head_slap_Humping_surface$aic#-122.4005

gam_Head_slap_Regular_dive <- gam(Cast_net ~ s(Head_slap, k=5) + s(Regular_dive, k=7), data = dat, method = "GCV.Cp")
gam_Head_slap_Regular_dive$aic#-122.9876

gam_Head_slap_Peduncle_dive <- gam(Cast_net ~ s(Head_slap, k=5) + s(Peduncle_dive, k=6), data = dat, method = "GCV.Cp")
gam_Head_slap_Peduncle_dive$aic#-126.6307******

gam_Head_slap_Tailoutdive <- gam(Cast_net ~ s(Head_slap, k=5) + as.factor(Tail_out_dive), data = dat, method = "GCV.Cp")
gam_Head_slap_Tailoutdive$aic#-114.9889

gam_Head_slap_Fast_swim <- gam(Cast_net ~ s(Head_slap, k=5) + s(Fast_swim, k=5), data = dat, method = "GCV.Cp")
gam_Head_slap_Fast_swim$aic#-125.877

gam_Head_slap_Sharking <- gam(Cast_net ~ s(Head_slap, k=5) + s(Sharking, k=6), data = dat, method = "GCV.Cp")
gam_Head_slap_Sharking$aic#-124.7469

gam_Head_slap_Underwater_tail.slap <- gam(Cast_net ~ s(Head_slap, k=5) + s(Underwater_tail.slap, k=5), data = dat, method = "GCV.Cp")
gam_Head_slap_Underwater_tail.slap$aic#-123.1264

gam_Head_slap_Horizontal_circle <- gam(Cast_net ~ s(Head_slap, k=5) + s(Horizontal_circle, k=4), data = dat, method = "GCV.Cp")
gam_Head_slap_Horizontal_circle$aic#-122.6713

gam_Head_slap_Pinwheeling <- gam(Cast_net ~ s(Head_slap, k=5) + s(Pinwheeling, k=3), data = dat, method = "GCV.Cp")
gam_Head_slap_Pinwheeling$aic#-122.4664

gam_Head_slap_Sideroll <- gam(Cast_net ~ s(Head_slap, k=5) + s(Side_roll, k=4), data = dat, method = "GCV.Cp")
gam_Head_slap_Sideroll$aic#-122.7736

gam_Head_slap_Bubble.blow <- gam(Cast_net ~ s(Head_slap, k=5) + s(Bubble.blow, k=3), data = dat, method = "GCV.Cp")
gam_Head_slap_Bubble.blow$aic#-123.0317

#3 Round

gam_Head_slap__Peduncle_diveActive <- gam(Cast_net ~ s(Head_slap, k=5) + s(Peduncle_dive, k=6) + s(Active_surfacing, k=3), data = dat, method = "GCV.Cp")
gam_Head_slap__Peduncle_diveActive$aic#-126.207

gam_Head_slap__Peduncle_divePorpoise <- gam(Cast_net ~ s(Head_slap, k=5) + s(Peduncle_dive, k=6) + s(Porpoise, k=5), data = dat, method = "GCV.Cp")
gam_Head_slap__Peduncle_divePorpoise$aic#-125.7515

gam_Head_slap__Peduncle_diveLeap <- gam(Cast_net ~ s(Head_slap, k=5) + s(Peduncle_dive, k=6) + s(Leap, k=3), data = dat, method = "GCV.Cp")
gam_Head_slap__Peduncle_diveLeap$aic#-124.8759

gam_Head_slap__Peduncle_diveHumping_surface <- gam(Cast_net ~ s(Head_slap, k=5) + s(Peduncle_dive, k=6) + s(Humping_surface, k=5), data = dat, method = "GCV.Cp")
gam_Head_slap__Peduncle_diveHumping_surface$aic#-124.6713

gam_Head_slap__Peduncle_diveRegular_dive <- gam(Cast_net ~ s(Head_slap, k=5) + s(Peduncle_dive, k=6) + s(Regular_dive, k=7), data = dat, method = "GCV.Cp")
gam_Head_slap__Peduncle_diveRegular_dive$aic#-128.6878******

gam_Head_slap__Peduncle_diveTailoutdive <- gam(Cast_net ~ s(Head_slap, k=5) + s(Peduncle_dive, k=6) + as.factor(Tail_out_dive), data = dat, method = "GCV.Cp")
gam_Head_slap__Peduncle_diveTailoutdive$aic#-118.1293

gam_Head_slap__Peduncle_diveFast_swim <- gam(Cast_net ~ s(Head_slap, k=5) + s(Peduncle_dive, k=6) + s(Fast_swim, k=5), data = dat, method = "GCV.Cp")
gam_Head_slap__Peduncle_diveFast_swim$aic#-128.135

gam_Head_slap__Peduncle_diveSharking <- gam(Cast_net ~ s(Head_slap, k=5) + s(Peduncle_dive, k=6) + s(Sharking, k=6), data = dat, method = "GCV.Cp")
gam_Head_slap__Peduncle_diveSharking$aic#-126.7896

gam_Head_slap__Peduncle_diveUnderwater_tail.slap <- gam(Cast_net ~ s(Head_slap, k=5) + s(Peduncle_dive, k=6) + s(Underwater_tail.slap, k=5), data = dat, method = "GCV.Cp")
gam_Head_slap__Peduncle_diveUnderwater_tail.slap$aic#-125.1327

gam_Head_slap__Peduncle_diveHorizontal_circle <- gam(Cast_net ~ s(Head_slap, k=5) + s(Peduncle_dive, k=6) + s(Horizontal_circle, k=4), data = dat, method = "GCV.Cp")
gam_Head_slap__Peduncle_diveHorizontal_circle$aic#-125.0567

gam_Head_slap__Peduncle_divePinwheeling <- gam(Cast_net ~ s(Head_slap, k=5) + s(Peduncle_dive, k=6) + s(Pinwheeling, k=3), data = dat, method = "GCV.Cp")
gam_Head_slap__Peduncle_divePinwheeling$aic#-124.7042

gam_Head_slap__Peduncle_diveSideroll <- gam(Cast_net ~ s(Head_slap, k=5) + s(Peduncle_dive, k=6) + s(Side_roll, k=4), data = dat, method = "GCV.Cp")
gam_Head_slap__Peduncle_diveSideroll$aic#-125.071

gam_Head_slap__Peduncle_diveBubble.blow <- gam(Cast_net ~ s(Head_slap, k=5) + s(Peduncle_dive, k=6) + s(Bubble.blow, k=3), data = dat, method = "GCV.Cp")
gam_Head_slap__Peduncle_diveBubble.blow$aic#-125.2826

#4  Round

gam_Head_slap__Peduncle_dive_Regular_dive_Active <- gam(Cast_net ~ s(Head_slap, k=5) + s(Peduncle_dive, k=6) + s(Regular_dive, k=7) + s(Active_surfacing, k=3), data = dat, method = "GCV.Cp")
gam_Head_slap__Peduncle_dive_Regular_dive_Active$aic#-128.4076

gam_Head_slap__Peduncle_dive_Regular_dive_Porpoise <- gam(Cast_net ~ s(Head_slap, k=5) + s(Peduncle_dive, k=6) + s(Regular_dive, k=7) + s(Porpoise, k=5), data = dat, method = "GCV.Cp")
gam_Head_slap__Peduncle_dive_Regular_dive_Porpoise$aic#-128.2402

gam_Head_slap__Peduncle_dive_Regular_dive_Leap <- gam(Cast_net ~ s(Head_slap, k=5) + s(Peduncle_dive, k=6) + s(Regular_dive, k=7) + s(Leap, k=3), data = dat, method = "GCV.Cp")
gam_Head_slap__Peduncle_dive_Regular_dive_Leap$aic#-123.3589

gam_Head_slap__Peduncle_dive_Regular_dive_Humping_surface <- gam(Cast_net ~ s(Head_slap, k=5) + s(Peduncle_dive, k=6) + s(Regular_dive, k=7) + s(Humping_surface, k=5), data = dat, method = "GCV.Cp")
gam_Head_slap__Peduncle_dive_Regular_dive_Humping_surface$aic#-123.2714

gam_Head_slap__Peduncle_dive_Regular_dive_Fast_swim <- gam(Cast_net ~ s(Head_slap, k=5) + s(Peduncle_dive, k=6) + s(Regular_dive, k=7) + s(Fast_swim, k=5), data = dat, method = "GCV.Cp")
gam_Head_slap__Peduncle_dive_Regular_dive_Fast_swim$aic#-130.6233

gam_Head_slap__Peduncle_dive_Regular_dive_Tailoutdive <- gam(Cast_net ~ s(Head_slap, k=5) + s(Peduncle_dive, k=6) + s(Regular_dive, k=7) + as.factor(Tail_out_dive), data = dat, method = "GCV.Cp")
gam_Head_slap__Peduncle_dive_Regular_dive_Tailoutdive$aic#-127.094

gam_Head_slap__Peduncle_dive_Regular_dive_Sharking <- gam(Cast_net ~ s(Head_slap, k=5) + s(Peduncle_dive, k=6) + s(Regular_dive, k=7) + s(Sharking, k=6), data = dat, method = "GCV.Cp")
gam_Head_slap__Peduncle_dive_Regular_dive_Sharking$aic#-132.9811*********

gam_Head_slap__Peduncle_dive_Regular_dive_Underwater_tail.slap <- gam(Cast_net ~ s(Head_slap, k=5) + s(Peduncle_dive, k=6) + s(Regular_dive, k=7) + s(Underwater_tail.slap, k=5), data = dat, method = "GCV.Cp")
gam_Head_slap__Peduncle_dive_Regular_dive_Underwater_tail.slap$aic#-124.2233

gam_Head_slap__Peduncle_dive_Regular_dive_Horizontal_circle <- gam(Cast_net ~ s(Head_slap, k=5) + s(Peduncle_dive, k=6) + s(Regular_dive, k=7) + s(Horizontal_circle, k=4), data = dat, method = "GCV.Cp")
gam_Head_slap__Peduncle_dive_Regular_dive_Horizontal_circle$aic#-124.0435

gam_Head_slap__Peduncle_dive_Regular_dive_Pinwheeling <- gam(Cast_net ~ s(Head_slap, k=5) + s(Peduncle_dive, k=6) + s(Regular_dive, k=7) + s(Pinwheeling, k=3), data = dat, method = "GCV.Cp")
gam_Head_slap__Peduncle_dive_Regular_dive_Pinwheeling$aic#-128.7311

gam_Head_slap__Peduncle_dive_Regular_dive_Sideroll <- gam(Cast_net ~ s(Head_slap, k=5) + s(Peduncle_dive, k=6) + s(Regular_dive, k=7) + s(Side_roll, k=4), data = dat, method = "GCV.Cp")
gam_Head_slap__Peduncle_dive_Regular_dive_Sideroll$aic#-123.6624

gam_Head_slap__Peduncle_dive_Regular_dive_Bubble.blow <- gam(Cast_net ~ s(Head_slap, k=5) + s(Peduncle_dive, k=6) + s(Regular_dive, k=7) + s(Bubble.blow, k=3), data = dat, method = "GCV.Cp")
gam_Head_slap__Peduncle_dive_Regular_dive_Bubble.blow$aic#-127.4557

#5  Round

gam_Head_slap__Peduncle_dive_Regular_dive_Sharking_Active <- gam(Cast_net ~ s(Head_slap, k=5) + s(Peduncle_dive, k=6) + s(Regular_dive, k=7) + s(Sharking, k=6) + s(Active_surfacing, k=3), data = dat, method = "GCV.Cp")
gam_Head_slap__Peduncle_dive_Regular_dive_Sharking_Active$aic#-136.1516**********

gam_Head_slap__Peduncle_dive_Regular_dive_Sharking_Porpoise <- gam(Cast_net ~ s(Head_slap, k=5) + s(Peduncle_dive, k=6) + s(Regular_dive, k=7) + s(Sharking, k=6) + s(Porpoise, k=5), data = dat, method = "GCV.Cp")
gam_Head_slap__Peduncle_dive_Regular_dive_Sharking_Porpoise$aic#-132.5609

gam_Head_slap__Peduncle_dive_Regular_dive_Sharking_Leap <- gam(Cast_net ~ s(Head_slap, k=5) + s(Peduncle_dive, k=6) + s(Regular_dive, k=7) + s(Sharking, k=6) + s(Leap, k=3), data = dat, method = "GCV.Cp")
gam_Head_slap__Peduncle_dive_Regular_dive_Sharking_Leap$aic#-130.8514

gam_Head_slap__Peduncle_dive_Regular_dive_Sharking_Humping_surface <- gam(Cast_net ~ s(Head_slap, k=5) + s(Peduncle_dive, k=6) + s(Regular_dive, k=7) + s(Sharking, k=6) + s(Humping_surface, k=5), data = dat, method = "GCV.Cp")
gam_Head_slap__Peduncle_dive_Regular_dive_Sharking_Humping_surface$aic#-135.5689

gam_Head_slap__Peduncle_dive_Regular_dive_Sharking_Fast_swim <- gam(Cast_net ~ s(Head_slap, k=5) + s(Peduncle_dive, k=6) + s(Regular_dive, k=7) + s(Sharking, k=6) + s(Fast_swim, k=5), data = dat, method = "GCV.Cp")
gam_Head_slap__Peduncle_dive_Regular_dive_Sharking_Fast_swim$aic#-132.7207

gam_Head_slap__Peduncle_dive_Regular_dive_Sharking_Tailoutdive <- gam(Cast_net ~ s(Head_slap, k=5) + s(Peduncle_dive, k=6) + s(Regular_dive, k=7) + s(Sharking, k=6) + as.factor(Tail_out_dive), data = dat, method = "GCV.Cp")
gam_Head_slap__Peduncle_dive_Regular_dive_Sharking_Tailoutdive$aic#-133.0604

gam_Head_slap__Peduncle_dive_Regular_dive_Sharking_Underwater_tail.slap <- gam(Cast_net ~ s(Head_slap, k=5) + s(Peduncle_dive, k=6) + s(Regular_dive, k=7) + s(Sharking, k=6) + s(Underwater_tail.slap, k=5), data = dat, method = "GCV.Cp")
gam_Head_slap__Peduncle_dive_Regular_dive_Sharking_Underwater_tail.slap$aic#-124.7932

gam_Head_slap__Peduncle_dive_Regular_dive_Sharking_Horizontal_circle <- gam(Cast_net ~ s(Head_slap, k=5) + s(Peduncle_dive, k=6) + s(Regular_dive, k=7) + s(Sharking, k=6) + s(Horizontal_circle, k=4), data = dat, method = "GCV.Cp")
gam_Head_slap__Peduncle_dive_Regular_dive_Sharking_Horizontal_circle$aic#-133.385

gam_Head_slap__Peduncle_dive_Regular_dive_Sharking_Pinwheeling <- gam(Cast_net ~ s(Head_slap, k=5) + s(Peduncle_dive, k=6) + s(Regular_dive, k=7) + s(Sharking, k=6) + s(Pinwheeling, k=3), data = dat, method = "GCV.Cp")
gam_Head_slap__Peduncle_dive_Regular_dive_Sharking_Pinwheeling$aic#-133.6413

gam_Head_slap__Peduncle_dive_Regular_dive_Sharking_Sideroll <- gam(Cast_net ~ s(Head_slap, k=5) + s(Peduncle_dive, k=6) + s(Regular_dive, k=7) + s(Sharking, k=6) + s(Side_roll, k=4), data = dat, method = "GCV.Cp")
gam_Head_slap__Peduncle_dive_Regular_dive_Sharking_Sideroll$aic#-131.8212

gam_Head_slap__Peduncle_dive_Regular_dive_Sharking_Bubble.blow <- gam(Cast_net ~ s(Head_slap, k=5) + s(Peduncle_dive, k=6) + s(Regular_dive, k=7) + s(Sharking, k=6) + s(Bubble.blow, k=3), data = dat, method = "GCV.Cp")
gam_Head_slap__Peduncle_dive_Regular_dive_Sharking_Bubble.blow$aic#-132.5871

#6  Round

gam_Head_slap__Peduncle_dive_Regular_dive_Sharking_Active_Porpoise <- gam(Cast_net ~ s(Head_slap, k=5) + s(Peduncle_dive, k=6) + s(Regular_dive, k=7) + s(Sharking, k=6) + s(Active_surfacing, k=3) + s(Porpoise, k=5), data = dat, method = "GCV.Cp")
gam_Head_slap__Peduncle_dive_Regular_dive_Sharking_Active_Porpoise$aic#-136.171

gam_Head_slap__Peduncle_dive_Regular_dive_Sharking_Active_Leap <- gam(Cast_net ~ s(Head_slap, k=5) + s(Peduncle_dive, k=6) + s(Regular_dive, k=7) + s(Sharking, k=6) + s(Active_surfacing, k=3) + s(Leap, k=3), data = dat, method = "GCV.Cp")
gam_Head_slap__Peduncle_dive_Regular_dive_Sharking_Active_Leap$aic#-133.8043

gam_Head_slap__Peduncle_dive_Regular_dive_Sharking_Active_Humping_surface <- gam(Cast_net ~ s(Head_slap, k=5) + s(Peduncle_dive, k=6) + s(Regular_dive, k=7) + s(Sharking, k=6) + s(Active_surfacing, k=3) + s(Humping_surface, k=5), data = dat, method = "GCV.Cp")
gam_Head_slap__Peduncle_dive_Regular_dive_Sharking_Active_Humping_surface$aic#-135.9766

gam_Head_slap__Peduncle_dive_Regular_dive_Sharking_Active_Fast_swim <- gam(Cast_net ~ s(Head_slap, k=5) + s(Peduncle_dive, k=6) + s(Regular_dive, k=7) + s(Sharking, k=6) + s(Active_surfacing, k=3) + s(Fast_swim, k=5), data = dat, method = "GCV.Cp")
gam_Head_slap__Peduncle_dive_Regular_dive_Sharking_Active_Fast_swim$aic#-137.5683*********

gam_Head_slap__Peduncle_dive_Regular_dive_Sharking_Active_Tailoutdive <- gam(Cast_net ~ s(Head_slap, k=5) + s(Peduncle_dive, k=6) + s(Regular_dive, k=7) + s(Sharking, k=6) + s(Active_surfacing, k=3) + as.factor(Tail_out_dive), data = dat, method = "GCV.Cp")
gam_Head_slap__Peduncle_dive_Regular_dive_Sharking_Active_Tailoutdive$aic#-133.3805

gam_Head_slap__Peduncle_dive_Regular_dive_Sharking_Active_Underwater_tail.slap <- gam(Cast_net ~ s(Head_slap, k=5) + s(Peduncle_dive, k=6) + s(Regular_dive, k=7) + s(Sharking, k=6) + s(Active_surfacing, k=3) + s(Underwater_tail.slap, k=5), data = dat, method = "GCV.Cp")
gam_Head_slap__Peduncle_dive_Regular_dive_Sharking_Active_Underwater_tail.slap$aic#-134.4644

gam_Head_slap__Peduncle_dive_Regular_dive_Sharking_Active_Horizontal_circle <- gam(Cast_net ~ s(Head_slap, k=5) + s(Peduncle_dive, k=6) + s(Regular_dive, k=7) + s(Sharking, k=6) + s(Active_surfacing, k=3) + s(Horizontal_circle, k=4), data = dat, method = "GCV.Cp")
gam_Head_slap__Peduncle_dive_Regular_dive_Sharking_Active_Horizontal_circle$aic#-135.356

gam_Head_slap__Peduncle_dive_Regular_dive_Sharking_Active_Pinwheeling <- gam(Cast_net ~ s(Head_slap, k=5) + s(Peduncle_dive, k=6) + s(Regular_dive, k=7) + s(Sharking, k=6) + s(Active_surfacing, k=3) + s(Pinwheeling, k=3), data = dat, method = "GCV.Cp")
gam_Head_slap__Peduncle_dive_Regular_dive_Sharking_Active_Pinwheeling$aic#-136.3621

gam_Head_slap__Peduncle_dive_Regular_dive_Sharking_Active_Sideroll <- gam(Cast_net ~ s(Head_slap, k=5) + s(Peduncle_dive, k=6) + s(Regular_dive, k=7) + s(Sharking, k=6) + s(Active_surfacing, k=3) + s(Side_roll, k=4), data = dat, method = "GCV.Cp")
gam_Head_slap__Peduncle_dive_Regular_dive_Sharking_Active_Sideroll$aic#-134.0734

gam_Head_slap__Peduncle_dive_Regular_dive_Sharking_Active_Bubble.blow <- gam(Cast_net ~ s(Head_slap, k=5) + s(Peduncle_dive, k=6) + s(Regular_dive, k=7) + s(Sharking, k=6) + s(Active_surfacing, k=3) + s(Bubble.blow, k=3), data = dat, method = "GCV.Cp")
gam_Head_slap__Peduncle_dive_Regular_dive_Sharking_Active_Bubble.blow$aic#-136.943

#7  Round

gam_Head_slap__Peduncle_dive_Regular_dive_Sharking_Active_Fast_swim_Porpoise <- gam(Cast_net ~ s(Head_slap, k=5) + s(Peduncle_dive, k=6) + s(Regular_dive, k=7) + s(Sharking, k=6) + s(Active_surfacing, k=3) + s(Fast_swim, k=5) + s(Porpoise, k=5), data = dat, method = "GCV.Cp")
gam_Head_slap__Peduncle_dive_Regular_dive_Sharking_Active_Fast_swim_Porpoise$aic#-135.675

gam_Head_slap__Peduncle_dive_Regular_dive_Sharking_Active_Fast_swim_Leap <- gam(Cast_net ~ s(Head_slap, k=5) + s(Peduncle_dive, k=6) + s(Regular_dive, k=7) + s(Sharking, k=6) + s(Active_surfacing, k=3) + s(Fast_swim, k=5) + s(Leap, k=3), data = dat, method = "GCV.Cp")
gam_Head_slap__Peduncle_dive_Regular_dive_Sharking_Active_Fast_swim_Leap$aic#-135.6428

gam_Head_slap__Peduncle_dive_Regular_dive_Sharking_Active_Fast_swim_Humping_surface <- gam(Cast_net ~ s(Head_slap, k=5) + s(Peduncle_dive, k=6) + s(Regular_dive, k=7) + s(Sharking, k=6) + s(Active_surfacing, k=3) + s(Fast_swim, k=5) + s(Humping_surface, k=5), data = dat, method = "GCV.Cp")
gam_Head_slap__Peduncle_dive_Regular_dive_Sharking_Active_Fast_swim_Humping_surface$aic#-136.6221

gam_Head_slap__Peduncle_dive_Regular_dive_Sharking_Active_Fast_swim_Tailoutdive <- gam(Cast_net ~ s(Head_slap, k=5) + s(Peduncle_dive, k=6) + s(Regular_dive, k=7) + s(Sharking, k=6) + s(Active_surfacing, k=3) + s(Fast_swim, k=5) + as.factor(Tail_out_dive), data = dat, method = "GCV.Cp")
gam_Head_slap__Peduncle_dive_Regular_dive_Sharking_Active_Fast_swim_Tailoutdive$aic#-130.5935

gam_Head_slap__Peduncle_dive_Regular_dive_Sharking_Active_Fast_swim_Underwater_tail.slap <- gam(Cast_net ~ s(Head_slap, k=5) + s(Peduncle_dive, k=6) + s(Regular_dive, k=7) + s(Sharking, k=6) + s(Active_surfacing, k=3) + s(Fast_swim, k=5) + s(Underwater_tail.slap, k=5), data = dat, method = "GCV.Cp")
gam_Head_slap__Peduncle_dive_Regular_dive_Sharking_Active_Fast_swim_Underwater_tail.slap$aic#-130.5935

gam_Head_slap__Peduncle_dive_Regular_dive_Sharking_Active_Fast_swim_Horizontal_circle <- gam(Cast_net ~ s(Head_slap, k=5) + s(Peduncle_dive, k=6) + s(Regular_dive, k=7) + s(Sharking, k=6) + s(Active_surfacing, k=3) + s(Fast_swim, k=5) + s(Horizontal_circle, k=4), data = dat, method = "GCV.Cp")
gam_Head_slap__Peduncle_dive_Regular_dive_Sharking_Active_Fast_swim_Horizontal_circle$aic#-137.5671

gam_Head_slap__Peduncle_dive_Regular_dive_Sharking_Active_Fast_swim_Pinwheeling <- gam(Cast_net ~ s(Head_slap, k=5) + s(Peduncle_dive, k=6) + s(Regular_dive, k=7) + s(Sharking, k=6) + s(Active_surfacing, k=3) + s(Fast_swim, k=5) + s(Pinwheeling, k=3), data = dat, method = "GCV.Cp")
gam_Head_slap__Peduncle_dive_Regular_dive_Sharking_Active_Fast_swim_Pinwheeling$aic#-137.8249*********

gam_Head_slap__Peduncle_dive_Regular_dive_Sharking_Active_Fast_swim_Sideroll <- gam(Cast_net ~ s(Head_slap, k=5) + s(Peduncle_dive, k=6) + s(Regular_dive, k=7) + s(Sharking, k=6) + s(Active_surfacing, k=3) + s(Fast_swim, k=5) + s(Side_roll, k=4), data = dat, method = "GCV.Cp")
gam_Head_slap__Peduncle_dive_Regular_dive_Sharking_Active_Fast_swim_Sideroll$aic#-130.5612

gam_Head_slap__Peduncle_dive_Regular_dive_Sharking_Active_Fast_swim_Bubble.blow <- gam(Cast_net ~ s(Head_slap, k=5) + s(Peduncle_dive, k=6) + s(Regular_dive, k=7) + s(Sharking, k=6) + s(Active_surfacing, k=3) + s(Fast_swim, k=5) + s(Bubble.blow, k=3), data = dat, method = "GCV.Cp")
gam_Head_slap__Peduncle_dive_Regular_dive_Sharking_Active_Fast_swim_Bubble.blow$aic#-137.6008

#8  Round

gam_Head_slap__Peduncle_dive_Regular_dive_Sharking_Active_Fast_swim_Pinwheeling_Porpoise <- gam(Cast_net ~ s(Head_slap, k=5) + s(Peduncle_dive, k=6) + s(Regular_dive, k=7) + s(Sharking, k=6) + s(Active_surfacing, k=3) + s(Fast_swim, k=5) + s(Pinwheeling, k=3) + s(Porpoise, k=5), data = dat, method = "GCV.Cp")
gam_Head_slap__Peduncle_dive_Regular_dive_Sharking_Active_Fast_swim_Pinwheeling_Porpoise$aic#-135.8592

gam_Head_slap__Peduncle_dive_Regular_dive_Sharking_Active_Fast_swim_Pinwheeling_Leap <- gam(Cast_net ~ s(Head_slap, k=5) + s(Peduncle_dive, k=6) + s(Regular_dive, k=7) + s(Sharking, k=6) + s(Active_surfacing, k=3) + s(Fast_swim, k=5) + s(Pinwheeling, k=3) + s(Leap, k=3), data = dat, method = "GCV.Cp")
gam_Head_slap__Peduncle_dive_Regular_dive_Sharking_Active_Fast_swim_Pinwheeling_Leap$aic#-135.8646

gam_Head_slap__Peduncle_dive_Regular_dive_Sharking_Active_Fast_swim_Pinwheeling_Humping_surface <- gam(Cast_net ~ s(Head_slap, k=5) + s(Peduncle_dive, k=6) + s(Regular_dive, k=7) + s(Sharking, k=6) + s(Active_surfacing, k=3) + s(Fast_swim, k=5) + s(Pinwheeling, k=3) + s(Humping_surface, k=5), data = dat, method = "GCV.Cp")
gam_Head_slap__Peduncle_dive_Regular_dive_Sharking_Active_Fast_swim_Pinwheeling_Humping_surface$aic#-137.719

gam_Head_slap__Peduncle_dive_Regular_dive_Sharking_Active_Fast_swim_Pinwheeling_Tailoutdive <- gam(Cast_net ~ s(Head_slap, k=5) + s(Peduncle_dive, k=6) + s(Regular_dive, k=7) + s(Sharking, k=6) + s(Active_surfacing, k=3) + s(Fast_swim, k=5) + s(Pinwheeling, k=3) + as.factor(Tail_out_dive), data = dat, method = "GCV.Cp")
gam_Head_slap__Peduncle_dive_Regular_dive_Sharking_Active_Fast_swim_Pinwheeling_Tailoutdive$aic#-128.4305

gam_Head_slap__Peduncle_dive_Regular_dive_Sharking_Active_Fast_swim_Pinwheeling_Underwater_tail.slap <- gam(Cast_net ~ s(Head_slap, k=5) + s(Peduncle_dive, k=6) + s(Regular_dive, k=7) + s(Sharking, k=6) + s(Active_surfacing, k=3) + s(Fast_swim, k=5) + s(Pinwheeling, k=3) + s(Underwater_tail.slap, k=5), data = dat, method = "GCV.Cp")
gam_Head_slap__Peduncle_dive_Regular_dive_Sharking_Active_Fast_swim_Pinwheeling_Underwater_tail.slap$aic#-130.7829

gam_Head_slap__Peduncle_dive_Regular_dive_Sharking_Active_Fast_swim_Pinwheeling_Horizontal_circle <- gam(Cast_net ~ s(Head_slap, k=5) + s(Peduncle_dive, k=6) + s(Regular_dive, k=7) + s(Sharking, k=6) + s(Active_surfacing, k=3) + s(Fast_swim, k=5) + s(Pinwheeling, k=3) + s(Horizontal_circle, k=4), data = dat, method = "GCV.Cp")
gam_Head_slap__Peduncle_dive_Regular_dive_Sharking_Active_Fast_swim_Pinwheeling_Horizontal_circle$aic#-136.166

gam_Head_slap__Peduncle_dive_Regular_dive_Sharking_Active_Fast_swim_Pinwheeling_Sideroll <- gam(Cast_net ~ s(Head_slap, k=5) + s(Peduncle_dive, k=6) + s(Regular_dive, k=7) + s(Sharking, k=6) + s(Active_surfacing, k=3) + s(Fast_swim, k=5) + s(Pinwheeling, k=3) + s(Side_roll, k=4), data = dat, method = "GCV.Cp")
gam_Head_slap__Peduncle_dive_Regular_dive_Sharking_Active_Fast_swim_Pinwheeling_Sideroll$aic#-130.8649

gam_Head_slap__Peduncle_dive_Regular_dive_Sharking_Active_Fast_swim_Pinwheeling_Bubble.blow <- gam(Cast_net ~ s(Head_slap, k=5) + s(Peduncle_dive, k=6) + s(Regular_dive, k=7) + s(Sharking, k=6) + s(Active_surfacing, k=3) + s(Fast_swim, k=5) + s(Pinwheeling, k=3) + s(Bubble.blow, k=3), data = dat, method = "GCV.Cp")
gam_Head_slap__Peduncle_dive_Regular_dive_Sharking_Active_Fast_swim_Pinwheeling_Bubble.blow$aic#-138.5366*********

#9  Round

gam_Head_slap__Peduncle_dive_Regular_dive_Sharking_Active_Fast_swim_Pinwheeling_Bubble.blow_Porpoise <- gam(Cast_net ~ s(Head_slap, k=5) + s(Peduncle_dive, k=6) + s(Regular_dive, k=7) + s(Sharking, k=6) + s(Active_surfacing, k=3) + s(Fast_swim, k=5) + s(Pinwheeling, k=3) + s(Bubble.blow, k=3) + s(Porpoise, k=5), data = dat, method = "GCV.Cp")
gam_Head_slap__Peduncle_dive_Regular_dive_Sharking_Active_Fast_swim_Pinwheeling_Bubble.blow_Porpoise$aic#-136.7407

gam_Head_slap__Peduncle_dive_Regular_dive_Sharking_Active_Fast_swim_Pinwheeling_Bubble.blow_Leap <- gam(Cast_net ~ s(Head_slap, k=5) + s(Peduncle_dive, k=6) + s(Regular_dive, k=7) + s(Sharking, k=6) + s(Active_surfacing, k=3) + s(Fast_swim, k=5) + s(Pinwheeling, k=3) + s(Bubble.blow, k=3) + s(Leap, k=3), data = dat, method = "GCV.Cp")
gam_Head_slap__Peduncle_dive_Regular_dive_Sharking_Active_Fast_swim_Pinwheeling_Bubble.blow_Leap$aic#-136.5066

gam_Head_slap__Peduncle_dive_Regular_dive_Sharking_Active_Fast_swim_Pinwheeling_Bubble.blow_Humping_surface <- gam(Cast_net ~ s(Head_slap, k=5) + s(Peduncle_dive, k=6) + s(Regular_dive, k=7) + s(Sharking, k=6) + s(Active_surfacing, k=3) + s(Fast_swim, k=5) + s(Pinwheeling, k=3) + s(Bubble.blow, k=3) + s(Humping_surface, k=5), data = dat, method = "GCV.Cp")
gam_Head_slap__Peduncle_dive_Regular_dive_Sharking_Active_Fast_swim_Pinwheeling_Bubble.blow_Humping_surface$aic#-139.4444********

gam_Head_slap__Peduncle_dive_Regular_dive_Sharking_Active_Fast_swim_Pinwheeling_Bubble.blow_Tailoutdive <- gam(Cast_net ~ s(Head_slap, k=5) + s(Peduncle_dive, k=6) + s(Regular_dive, k=7) + s(Sharking, k=6) + s(Active_surfacing, k=3) + s(Fast_swim, k=5) + s(Pinwheeling, k=3) + s(Bubble.blow, k=3) + as.factor(Tail_out_dive), data = dat, method = "GCV.Cp")
gam_Head_slap__Peduncle_dive_Regular_dive_Sharking_Active_Fast_swim_Pinwheeling_Bubble.blow_Tailoutdive$aic#-133.583

gam_Head_slap__Peduncle_dive_Regular_dive_Sharking_Active_Fast_swim_Pinwheeling_Bubble.blow_Underwater_tail.slap <- gam(Cast_net ~ s(Head_slap, k=5) + s(Peduncle_dive, k=6) + s(Regular_dive, k=7) + s(Sharking, k=6) + s(Active_surfacing, k=3) + s(Fast_swim, k=5) + s(Pinwheeling, k=3) + s(Bubble.blow, k=3) + s(Underwater_tail.slap, k=5), data = dat, method = "GCV.Cp")
gam_Head_slap__Peduncle_dive_Regular_dive_Sharking_Active_Fast_swim_Pinwheeling_Bubble.blow_Underwater_tail.slap$aic#-129.1465

gam_Head_slap__Peduncle_dive_Regular_dive_Sharking_Active_Fast_swim_Pinwheeling_Bubble.blow_Horizontal_circle <- gam(Cast_net ~ s(Head_slap, k=5) + s(Peduncle_dive, k=6) + s(Regular_dive, k=7) + s(Sharking, k=6) + s(Active_surfacing, k=3) + s(Fast_swim, k=5) + s(Pinwheeling, k=3) + s(Bubble.blow, k=3) + s(Horizontal_circle, k=4), data = dat, method = "GCV.Cp")
gam_Head_slap__Peduncle_dive_Regular_dive_Sharking_Active_Fast_swim_Pinwheeling_Bubble.blow_Horizontal_circle$aic#-136.8277

gam_Head_slap__Peduncle_dive_Regular_dive_Sharking_Active_Fast_swim_Pinwheeling_Bubble.blow_Sideroll <- gam(Cast_net ~ s(Head_slap, k=5) + s(Peduncle_dive, k=6) + s(Regular_dive, k=7) + s(Sharking, k=6) + s(Active_surfacing, k=3) + s(Fast_swim, k=5) + s(Pinwheeling, k=3) + s(Bubble.blow, k=3) + s(Side_roll, k=4), data = dat, method = "GCV.Cp")
gam_Head_slap__Peduncle_dive_Regular_dive_Sharking_Active_Fast_swim_Pinwheeling_Bubble.blow_Sideroll$aic#-136.8167

#10  Round

gam_Head_slap__Peduncle_dive_Regular_dive_Sharking_Active_Fast_swim_Pinwheeling_Bubble.blow_Humping_surface_Porpoise <- gam(Cast_net ~ s(Head_slap, k=5) + s(Peduncle_dive, k=6) + s(Regular_dive, k=7) + s(Sharking, k=6) + s(Active_surfacing, k=3) + s(Fast_swim, k=5) + s(Pinwheeling, k=3) + s(Bubble.blow, k=3) + s(Humping_surface, k=5) + s(Porpoise, k=5), data = dat, method = "GCV.Cp")
gam_Head_slap__Peduncle_dive_Regular_dive_Sharking_Active_Fast_swim_Pinwheeling_Bubble.blow_Humping_surface_Porpoise$aic#-137.5349

gam_Head_slap__Peduncle_dive_Regular_dive_Sharking_Active_Fast_swim_Pinwheeling_Bubble.blow_Humping_surface_Leap <- gam(Cast_net ~ s(Head_slap, k=5) + s(Peduncle_dive, k=6) + s(Regular_dive, k=7) + s(Sharking, k=6) + s(Active_surfacing, k=3) + s(Fast_swim, k=5) + s(Pinwheeling, k=3) + s(Bubble.blow, k=3) + s(Humping_surface, k=5) + s(Leap, k=3), data = dat, method = "GCV.Cp")
gam_Head_slap__Peduncle_dive_Regular_dive_Sharking_Active_Fast_swim_Pinwheeling_Bubble.blow_Humping_surface_Leap$aic#-137.3993

gam_Head_slap__Peduncle_dive_Regular_dive_Sharking_Active_Fast_swim_Pinwheeling_Bubble.blow_Humping_surface_Tailoutdive <- gam(Cast_net ~ s(Head_slap, k=5) + s(Peduncle_dive, k=6) + s(Regular_dive, k=7) + s(Sharking, k=6) + s(Active_surfacing, k=3) + s(Fast_swim, k=5) + s(Pinwheeling, k=3) + s(Bubble.blow, k=3) + s(Humping_surface, k=5) + as.factor(Tail_out_dive), data = dat, method = "GCV.Cp")
gam_Head_slap__Peduncle_dive_Regular_dive_Sharking_Active_Fast_swim_Pinwheeling_Bubble.blow_Humping_surface_Tailoutdive$aic#-126.796

gam_Head_slap__Peduncle_dive_Regular_dive_Sharking_Active_Fast_swim_Pinwheeling_Bubble.blow_Humping_surface_Underwater_tail.slap <- gam(Cast_net ~ s(Head_slap, k=5) + s(Peduncle_dive, k=6) + s(Regular_dive, k=7) + s(Sharking, k=6) + s(Active_surfacing, k=3) + s(Fast_swim, k=5) + s(Pinwheeling, k=3) + s(Bubble.blow, k=3) + s(Humping_surface, k=5) + s(Underwater_tail.slap, k=5), data = dat, method = "GCV.Cp")
gam_Head_slap__Peduncle_dive_Regular_dive_Sharking_Active_Fast_swim_Pinwheeling_Bubble.blow_Humping_surface_Underwater_tail.slap$aic#-128.2484

gam_Head_slap__Peduncle_dive_Regular_dive_Sharking_Active_Fast_swim_Pinwheeling_Bubble.blow_Humping_surface_Horizontal_circle <- gam(Cast_net ~ s(Head_slap, k=5) + s(Peduncle_dive, k=6) + s(Regular_dive, k=7) + s(Sharking, k=6) + s(Active_surfacing, k=3) + s(Fast_swim, k=5) + s(Pinwheeling, k=3) + s(Bubble.blow, k=3) + s(Humping_surface, k=5) + s(Horizontal_circle, k=4), data = dat, method = "GCV.Cp")
gam_Head_slap__Peduncle_dive_Regular_dive_Sharking_Active_Fast_swim_Pinwheeling_Bubble.blow_Humping_surface_Horizontal_circle$aic#-136.9517

gam_Head_slap__Peduncle_dive_Regular_dive_Sharking_Active_Fast_swim_Pinwheeling_Bubble.blow_Humping_surface_Sideroll <- gam(Cast_net ~ s(Head_slap, k=5) + s(Peduncle_dive, k=6) + s(Regular_dive, k=7) + s(Sharking, k=6) + s(Active_surfacing, k=3) + s(Fast_swim, k=5) + s(Pinwheeling, k=3) + s(Bubble.blow, k=3) + s(Humping_surface, k=5) + s(Side_roll, k=4), data = dat, method = "GCV.Cp")
gam_Head_slap__Peduncle_dive_Regular_dive_Sharking_Active_Fast_swim_Pinwheeling_Bubble.blow_Humping_surface_Sideroll$aic#-129.9676

#Winner:
summary(gam_Head_slap__Peduncle_dive_Regular_dive_Sharking_Active_Fast_swim_Pinwheeling_Bubble.blow_Humping_surface)
par(mfrow=c(3,2))
gam.check(gam_Head_slap__Peduncle_dive_Regular_dive_Sharking_Active_Fast_swim_Pinwheeling_Bubble.blow_Humping_surface)
acf(resid(gam_Head_slap__Peduncle_dive_Regular_dive_Sharking_Active_Fast_swim_Pinwheeling_Bubble.blow_Humping_surface), main="Series resid (GAM model)")

#GAM check
layout(matrix(c(1,2,3,4,5,5), 3, 2, byrow = TRUE), widths=c(2,2,1), heights=c(1,1,1))
gam.check(gam_Head_slap__Peduncle_dive_Regular_dive_Sharking_Active_Fast_swim_Pinwheeling_Bubble.blow_Humping_surface, old.style = FALSE)
acf(resid(gam_Head_slap__Peduncle_dive_Regular_dive_Sharking_Active_Fast_swim_Pinwheeling_Bubble.blow_Humping_surface), main="Series resid (GAM model)")

#GAM model
par(mfrow = c(3,3))
plot(gam_Head_slap__Peduncle_dive_Regular_dive_Sharking_Active_Fast_swim_Pinwheeling_Bubble.blow_Humping_surface,select = 1, pages=0,residuals=TRUE,all.terms=TRUE,shade=TRUE,shade.col="gray80", 
     cex.lab = 2,ylim = c(-0.3,0.6),cex.axis = 1.5, cex.main = 1.5, cex.sub = 1.5, ylab = " ", xlab="Head slap", rug = TRUE, seWithMean=TRUE)
plot(gam_Head_slap__Peduncle_dive_Regular_dive_Sharking_Active_Fast_swim_Pinwheeling_Bubble.blow_Humping_surface,select = 2, pages=0,residuals=TRUE,all.terms=TRUE,shade=TRUE,shade.col="gray80", 
     cex.lab = 2,ylim = c(-0.3,0.3),cex.axis = 1.5, cex.main = 1.5, cex.sub = 1.5, ylab = " ", xlab="Peduncle dive", rug = TRUE, seWithMean=TRUE)
plot(gam_Head_slap__Peduncle_dive_Regular_dive_Sharking_Active_Fast_swim_Pinwheeling_Bubble.blow_Humping_surface,select = 3, pages=0,residuals=TRUE,all.terms=TRUE,shade=TRUE,shade.col="gray80", 
     cex.lab = 2,ylim = c(-0.5,0.3), cex.axis = 1.5, cex.main = 1.5, cex.sub = 1.5, ylab = " ", xlab="Regular dive", rug = TRUE, seWithMean=TRUE)
plot(gam_Head_slap__Peduncle_dive_Regular_dive_Sharking_Active_Fast_swim_Pinwheeling_Bubble.blow_Humping_surface,select = 4, pages=0,residuals=TRUE,all.terms=TRUE,shade=TRUE,shade.col="gray80", 
     cex.lab = 2, ylim = c(-0.4,0.3),cex.axis = 1.5, cex.main = 1.5, cex.sub = 1.5, ylab = " ", xlab="Sharking", rug = TRUE, seWithMean=TRUE)
plot(gam_Head_slap__Peduncle_dive_Regular_dive_Sharking_Active_Fast_swim_Pinwheeling_Bubble.blow_Humping_surface,select = 5, pages=0,residuals=TRUE,all.terms=TRUE,shade=TRUE,shade.col="gray80", 
     cex.lab = 2,ylim = c(-0.15,0.15), cex.axis = 1.5, cex.main = 1.5, cex.sub = 1.5, ylab = " ", xlab="Active surfacing", rug = TRUE, seWithMean=TRUE)
plot(gam_Head_slap__Peduncle_dive_Regular_dive_Sharking_Active_Fast_swim_Pinwheeling_Bubble.blow_Humping_surface,select = 6, pages=0,residuals=TRUE,all.terms=TRUE,shade=TRUE,shade.col="gray80", 
     cex.lab = 2,ylim = c(-0.15,0.2),cex.axis = 1.5, cex.main = 1.5, cex.sub = 1.5, ylab = " ", xlab="Fast swim", rug = TRUE, seWithMean=TRUE)
plot(gam_Head_slap__Peduncle_dive_Regular_dive_Sharking_Active_Fast_swim_Pinwheeling_Bubble.blow_Humping_surface,select = 7, pages=0,residuals=TRUE,all.terms=TRUE,shade=TRUE,shade.col="gray80", 
     cex.lab = 2,ylim = c(-0.1,0.2), cex.axis = 1.5, cex.main = 1.5, cex.sub = 1.5, ylab = " ", xlab="Pinwheeling", rug = TRUE, seWithMean=TRUE)
plot(gam_Head_slap__Peduncle_dive_Regular_dive_Sharking_Active_Fast_swim_Pinwheeling_Bubble.blow_Humping_surface,select = 8, pages=0,residuals=TRUE,all.terms=TRUE,shade=TRUE,shade.col="gray80", 
     cex.lab = 2,ylim = c(-0.15,0.2), cex.axis = 1.5, cex.main = 1.5, cex.sub = 1.5, ylab = " ", xlab="Bubble blow", rug = TRUE, seWithMean=TRUE)
plot(gam_Head_slap__Peduncle_dive_Regular_dive_Sharking_Active_Fast_swim_Pinwheeling_Bubble.blow_Humping_surface,select = 9, pages=0,residuals=TRUE,all.terms=TRUE,shade=TRUE,shade.col="gray80", 
     cex.lab = 2,ylim = c(-0.15,0.2), cex.axis = 1.5, cex.main = 1.5, cex.sub = 1.5, ylab = " ", xlab="Humping surface", rug = TRUE, seWithMean=TRUE)

