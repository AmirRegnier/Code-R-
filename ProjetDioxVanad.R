df<-Data_Time_Temperature_Rho_100_200_300_400_500C

df1<-df[,1:3]
df2<-df[,4:6]
df3<-df[,7:9]
df4<-df[,10:12]
df5<-df[,13:15]

for (i in c(df1,df2,df3,df4,df5)){
  #1colnames(i)=c("Times (s)","Temperature (°C)","Resistivity (Ohm m)")
  print(dim(i))
}
dim(df1)
colnames(df5)=c("Times (s)","Temperature (°C)","Resistivity (Ohm m)")
df1<-df1[-c(1),]
df2<-df2[-c(1),]
df3<-df3[-c(1),]
df4<-df4[-c(1),]
df5<-df5[-c(1),]

df1$`Times (s)`<-as.numeric(df1$`Times (s)`)
df1$`Temperature (°C)`<-as.numeric(df1$`Temperature (°C)`)
df1$`Resistivity (Ohm m)`<-as.numeric(df1$`Resistivity (Ohm m)`)

df2$`Times (s)`<-as.numeric(df2$`Times (s)`)
df2$`Temperature (°C)`<-as.numeric(df2$`Temperature (°C)`)
df2$`Resistivity (Ohm m)`<-as.numeric(df2$`Resistivity (Ohm m)`)

df3$`Times (s)`<-as.numeric(df3$`Times (s)`)
df3$`Temperature (°C)`<-as.numeric(df3$`Temperature (°C)`)
df3$`Resistivity (Ohm m)`<-as.numeric(df3$`Resistivity (Ohm m)`)

df4$`Times (s)`<-as.numeric(df4$`Times (s)`)
df4$`Temperature (°C)`<-as.numeric(df4$`Temperature (°C)`)
df4$`Resistivity (Ohm m)`<-as.numeric(df4$`Resistivity (Ohm m)`)

df5$`Times (s)`<-as.numeric(df5$`Times (s)`)
df5$`Temperature (°C)`<-as.numeric(df5$`Temperature (°C)`)
df5$`Resistivity (Ohm m)`<-as.numeric(df5$`Resistivity (Ohm m)`)



df1.acp <- PCA(df1,graph=FALSE)

fviz_pca_ind(df1.acp, col.ind="cos2")
fviz_pca_ind(df1.acp, col.ind="contrib")
fviz_pca_var(df1.acp, col.var="cos2")

corrplot(cor(df1))
#Courbe de de le tempéraure en fonction du temps 
par(mfrow=c(2,3))
plot(df1$`Times (s)`,df1$`Temperature (°C)`,type="l",main = "Température de 100°C ")
abline(v=537,col="red")

plot(xlab="Temps (en s)",ylab="Température (en °C)",main="Première phase du profil de température [0,t1]",df1$`Times (s)`[df1$`Times (s)`<=537],df1$`Temperature (°C)`[df1$`Times (s)`<=537],type="l",col="red")

plot(xlab="Temps (en s)",ylab="Température (en °C)",main="Seconde phase du profil de température [t1,t2]",df1$`Times (s)`[df1$`Times (s)`>=538& df1$`Times (s)`<=48558],df1$`Temperature (°C)`[df1$`Times (s)`>=538& df1$`Times (s)`<=48558],type="l",col="red")

plot(xlab="Temps (en s)",ylab="Température (en °C)",main="Dernière phase du profil de température [t2,fin]",df1$`Times (s)`[df1$`Times (s)`>=48558],df1$`Temperature (°C)`[df1$`Times (s)`>=48558],type="l",col="red")



par(mfrow=c(2,3))
#100°C
plot(xlab="Temps (en s)",ylab="Température (en °C)",df1$`Times (s)`,df1$`Temperature (°C)`,type="l",main = "Température de 100°C ")
abline(v=537,col="red")
abline(v=48558,col="green")

plot(xlab="Temps (en s)",ylab="Résistivité en Ω.m",df1$`Times (s)`,df1$`Resistivity (Ohm m)`,type="l",main = "Température de 100°C ")
abline(v=537,col="red")
abline(v=48558,col="green")


#200°C
plot(xlab="Temps (en s)",ylab="Température (en °C)",df2$`Times (s)`,df2$`Temperature (°C)`,type="l",main="Température de 200°C ")
abline(v=1219,col="red")
abline(v=49124,col="green")

plot(xlab="Temps (en s)",ylab="Résistivité en Ω.m",df2$`Times (s)`,df2$`Resistivity (Ohm m)`,type="l",main="Température de 200°C ")
abline(v=1219,col="red")
abline(v=49124,col="green")

#300°C
plot(xlab="Temps (en s)",ylab="Température (en °C)",df3$`Times (s)`,df3$`Temperature (°C)`,type="l",main="Température de 300°C ")
abline(v=1795,col="red")
abline(v=49750,col="green")

plot(xlab="Temps (en s)",ylab="Résistivité en Ω.m",df3$`Times (s)`,df3$`Resistivity (Ohm m)`,type="l",main="Température de 300°C ")
abline(v=1795,col="red")
abline(v=49750,col="green")


#400°C
plot(xlab="Temps (en s)",ylab="Température (en °C)",df4$`Times (s)`,df4$`Temperature (°C)`,type="l",main="Température de 400°C ")
abline(v=2400,col="red")
abline(v=50358,col="green")

plot(xlab="Temps (en s)",ylab="Résistivité en Ω.m",df4$`Times (s)`,df4$`Resistivity (Ohm m)`,type="l",main="Température de 400°C ")
abline(v=2400,col="red")
abline(v=50358,col="green")

#500°C
plot(xlab="Temps (en s)",ylab="Température (en °C)",df5$`Times (s)`,df5$`Temperature (°C)`,type="l",main="Température de 500°C ")
abline(v=3025,col="red")
abline(v=50985,col="green")

plot(xlab="Temps (en s)",ylab="Résistivité en Ω.m",df5$`Times (s)`,df5$`Resistivity (Ohm m)`,type="l",main="Température de 500°C ")
abline(v=3025,col="red")
abline(v=50985,col="green")



#Courbe entre [0,t1]
par(mfrow=c(2,3))
plot(xlab="Temps",ylab="Résisitivité",df1$`Times (s)`[df1$`Times (s)`<=537],df1$`Resistivity (Ohm m)`[df1$`Times (s)`<=537],type="l",main = "Température de 100°C ")
plot(xlab="Temps",ylab="Résisitivité",df2$`Times (s)`[df2$`Times (s)`<=1219],df2$`Resistivity (Ohm m)`[df2$`Times (s)`<=1219],type="l",main = "Température de 200°C ")
plot(xlab="Temps",ylab="Résisitivité",df3$`Times (s)`[df3$`Times (s)`<=1795],df3$`Resistivity (Ohm m)`[df3$`Times (s)`<=1795],type="l",main = "Température de 300°C ")
plot(xlab="Temps",ylab="Résisitivité",df4$`Times (s)`[df4$`Times (s)`<=2400],df4$`Resistivity (Ohm m)`[df4$`Times (s)`<=2400],type="l",main = "Température de 400°C ")
plot(xlab="Temps",ylab="Résisitivité",df5$`Times (s)`[df5$`Times (s)`<=3025],df5$`Resistivity (Ohm m)`[df5$`Times (s)`<=3025],type="l",main = "Température de 500°C ")
#Courbe enttre [t2,0]
par(mfrow=c(2,3))
plot(xlab="Temps",ylab="Résisitivité",df1$`Times (s)`[df1$`Times (s)`>=48558],df1$`Resistivity (Ohm m)`[df1$`Times (s)`>=48558],type="l",main = "Température de 100°C ")
plot(xlab="Temps",ylab="Résisitivité",df2$`Times (s)`[df2$`Times (s)`>=49124],df2$`Resistivity (Ohm m)`[df2$`Times (s)`>=49124],type="l",main = "Température de 200°C ")
plot(xlab="Temps",ylab="Résisitivité",df3$`Times (s)`[df3$`Times (s)`>=49750],df3$`Resistivity (Ohm m)`[df3$`Times (s)`>=49750],type="l",main = "Température de 300°C ")
plot(xlab="Temps",ylab="Résisitivité",df4$`Times (s)`[df4$`Times (s)`>=50358],df4$`Resistivity (Ohm m)`[df4$`Times (s)`>=50358],type="l",main = "Température de 400°C ")
plot(xlab="Temps",ylab="Résisitivité",df5$`Times (s)`[df5$`Times (s)`>=50985],df5$`Resistivity (Ohm m)`[df5$`Times (s)`>=50985],type="l",main = "Température de 500°C ")



par(mfrow=c(2,3))
plot(df1$`Temperature (°C)`,df1$`Resistivity (Ohm m)`)
plot(df2$`Temperature (°C)`,df2$`Resistivity (Ohm m)`)
plot(df3$`Times (s)`,df3$`Resistivity (Ohm m)`)
df3$`Resistivity (Ohm m)`
plot(df4$`Times (s)`,df4$`Resistivity (Ohm m)`)
plot(df5$`Temperature (°C)`,df5$`Resistivity (Ohm m)`)




par(mfrow=c(2,3))
plot(df1$`Times (s)`,df1$`Resistivity (Ohm m)`,type="l",main="Température à 100")
plot(df2$`Times (s)`,df2$`Resistivity (Ohm m)`,type="l",main="Température à 200")
plot(df3$`Times (s)`,df3$`Resistivity (Ohm m)`,type="l",main="Température à 300")
plot(df4$`Times (s)`,df4$`Resistivity (Ohm m)`,type="l",main="Température à 400")
plot(df5$`Times (s)`,df5$`Resistivity (Ohm m)`,type="l",main="Température à 500")

par(mfrow=c(2,3))
plot(main="Température seuil à 100°C",xlab="Température en °C",ylab="Résistivité en Ω.m",df1$`Temperature (°C)`,df1$`Resistivity (Ohm m)`)
plot(main="Température seuil à 200°C",xlab="Température en °C",ylab="Résistivité en Ω.m",df2$`Temperature (°C)`,df2$`Resistivity (Ohm m)`)
plot(main="Température seuil à 300°C",xlab="Température en °C",ylab="Résistivité en Ω.m",df3$`Temperature (°C)`,df3$`Resistivity (Ohm m)`)
plot(main="Température seuil à 400°C",xlab="Température en °C",ylab="Résistivité en Ω.m",df4$`Temperature (°C)`,df4$`Resistivity (Ohm m)`)
plot(main="Température seuil à 500°C",xlab="Température en °C",ylab="Résistivité en Ω.m",df5$`Temperature (°C)`,df5$`Resistivity (Ohm m)`)

plot(main="Température des hystérésis imposé",hyst100$Température,ylab = "Température en °C",xlab="Temps en minutes")

df1<-df1[1:823,]
df2<-df2[1:845,]
df3<-df3[1:873,]
df4<-df4[1:892,]
df5<-df5[1:913,]

Moyennedf1<-mean(df1$`Resistivity (Ohm m)`)
Moyennedf2<-mean(df2$`Resistivity (Ohm m)`)
Moyennedf3<-mean(df3$`Resistivity (Ohm m)`)
Moyennedf4<-mean(df4$`Resistivity (Ohm m)`)
Moyennedf5<-mean(df5$`Resistivity (Ohm m)`)


Sddf1<-sd(df1$`Resistivity (Ohm m)`)
Sddf2<-sd(df2$`Resistivity (Ohm m)`)
Sddf3<-sd(df3$`Resistivity (Ohm m)`)
Sddf4<-sd(df4$`Resistivity (Ohm m)`)
Sddf5<-sd(df5$`Resistivity (Ohm m)`)


Vectmoyenne<-c(Moyennedf1,Moyennedf2,Moyennedf3,Moyennedf4,Moyennedf5)
TempMoy<-c(100,200,300,400,500)
TempMoy2<-c(100,200,300,400)
Vectmoyenne2<-c(Moyennedf1,Moyennedf2,Moyennedf3,Moyennedf4)

Vectsd<-c(Sddf1,Sddf2,Sddf3,Sddf4,Sddf5)
Vectsd2<-c(Sddf1,Sddf2,Sddf3,Sddf4)
plot(TempMoy,Vectmoyenne,main="Valeur moyenne de le résistivité en fonction de la température ")
plot(TempMoy2,Vectmoyenne2,main="Valeur moyenne de le résistivité en fonction de la température ")
Vectsd
plot(TempMoy,Vectsd,main="Ecart-type de le résistivité en fonction de la température ")
plot(TempMoy2,Vectsd2,main="Ecart-type de le résistivité en fonction de la température ")

var(c(1,2,3))
help(sd)
df1<-df1[1:823,]
df2<-df2[1:845,]
df3<-df3[1:873,]
df4<-df4[1:892,]
df5<-df5[1:913,]

modelm1<-lm(Vectmoyenne~poly(TempMoy,2))
modelm2<-lm(Vectmoyenne~poly(TempMoy,3))
summary(modelm1)
summary(modelm2)

plot(TempMoy,Vectmoyenne,main="Courbe Résistivité moyenne en fonction de la température")

plot(modelm2$fitted, modelm2$residuals, xlab="Valeurs ajustées", ylab="Résidus")
plot(modelm2$fitted.values,type="l")

plot(TempMoy,Vectmoyenne,main="Approximation de le résistivité moyenne",ylim=c(-0.001,0.005))
lines(TempMoy,predict(modelm2),type="l",lty=2, lwd=2) 

plot(TempMoy,Vectmoyenne,main="Courbe Résistivité moyenne en fonction de la température",ylim=c(-0.003,0.005))
lines(TempMoy,predict(modelm1),type="l",lty=2, lwd=2) 


#On s"intéresse à la phase 2 uniquement ici
df100<-df1[df1$`Temperature (°C)`==100,]
df200<-df2[df2$`Temperature (°C)`==200,]
df300<-df3[df3$`Temperature (°C)`==300,]
df400<-df4[df4$`Temperature (°C)`==400,]
df500<-df5[df5$`Temperature (°C)`==500,]


par(mfrow=c(2,3))
plot(df1$`Times (s)`,df1$`Resistivity (Ohm m)`,type="l",main="Température à 100")
plot(df2$`Times (s)`,df2$`Resistivity (Ohm m)`,type="l",main="Température à 200")
plot(df3$`Times (s)`,df3$`Resistivity (Ohm m)`,type="l",main="Température à 300")
plot(df4$`Times (s)`,df4$`Resistivity (Ohm m)`,type="l",main="Température à 400")
plot(df5$`Times (s)`,df5$`Resistivity (Ohm m)`,type="l",main="Température à 500")


par(mfrow=c(2,3))
plot(df100$`Times (s)`,df100$`Resistivity (Ohm m)`,type="l",main="Température à 100")
plot(df200$`Times (s)`,df200$`Resistivity (Ohm m)`,type="l",main="Température à 200")
plot(df300$`Times (s)`,df300$`Resistivity (Ohm m)`,type="l",main="Température à 300")
plot(df400$`Times (s)`,df400$`Resistivity (Ohm m)`,type="l",main="Température à 400")
plot(df500$`Times (s)`,df500$`Resistivity (Ohm m)`,type="l",main="Température à 500")


time401<-df400[1,1]
time402<-df400[799,1]

time301<-df300[1,1]
time302<-df300[799,1]


Moyennedf100<-mean(df100$`Resistivity (Ohm m)`)
Moyennedf200<-mean(df200$`Resistivity (Ohm m)`)
Moyennedf300<-mean(df300$`Resistivity (Ohm m)`)
Moyennedf400<-mean(df400$`Resistivity (Ohm m)`)
Moyennedf500<-mean(df500$`Resistivity (Ohm m)`)

Vectmoyenne100<-c(Moyennedf100,Moyennedf200,Moyennedf300,Moyennedf400,Moyennedf500)
TempMoy<-c(100,200,300,400,500)
plot(TempMoy,Vectmoyenne100,type="l",main="Valeur moyenne de le résistivité en fonction de la température ")

modelm3<-lm(Vectmoyenne100~poly(TempMoy,2))
modelm4<-lm(Vectmoyenne100~poly(TempMoy,3))



summary(modelm3)
summary(modelm4)

plot(TempMoy,Vectmoyenne100,main="Courbe Résistivité moyenne en fonction de la température")

plot(modelm3$fitted, modelm3$residuals, xlab="Valeurs ajustées", ylab="Résidus")
plot(modelm3$fitted.values,type="l")

plot(modelm4$fitted, modelm4$residuals, xlab="Valeurs ajustées", ylab="Résidus")
plot(modelm4$fitted.values,type="l")

plot(TempMoy,Vectmoyenne100,main="Courbe Résistivité moyenne en fonction de la température")
lines(TempMoy,predict(modelm3),type="l",lty=2, lwd=2) 

plot(TempMoy,Vectmoyenne100,main="Approximation de valeurs moyennes de le résistivités sans les bords",ylim=c(-0.1*10**-3,1*10**-3))
lines(TempMoy,predict(modelm4),type="l",lty=2, lwd=2) 

plot(TempMoy,Vectmoyenne100,main="Approximation de valeurs moyennes de le résistivités sans les bords",ylim=c(-0.1*10**-3,1*10**-3))
lines(TempMoy,predict(modelm5),type="l",lty=2, lwd=2) 

#Taux d'accroissement
ds1<-rep(0,997)
ds2<-rep(0,997)
ds3<-rep(0,997)
ds4<-rep(0,997)
ds5<-rep(0,997)
ds1

tauxaccroiss<-function(df){
  ds<-rep(0,dim(df)[1]-1)
for (i in 1:dim(df)[1]-1){
  
  ds[i]<-(df$`Resistivity (Ohm m)`[i+1]-df$`Resistivity (Ohm m)`[i])/(df$`Times (s)`[i+1]-df$`Times (s)`[i])
  
}
  return(ds)
}
ds1<-tauxaccroiss(df1)
ds2<-tauxaccroiss(df2)
ds3<-tauxaccroiss(df3)
ds4<-tauxaccroiss(df4)
ds5<-tauxaccroiss(df5)



par(mfrow=c(2,3))
plot(ds1,type="l",main="Température à 100")
plot(ds2,type="l",main="Température à 200")
plot(ds3,type="l",main="Température à 300")
plot(ds4,type="l",main="Température à 400")
plot(ds5,type="l",main="Température à 500")


Moyenneds1<-mean(ds1)
Moyenneds2<-mean(ds2)
Moyenneds3<-mean(ds3)
Moyenneds4<-mean(ds4)
Moyenneds5<-mean(ds5)

Vectmoyenneds<-c(Moyenneds1,Moyenneds2,Moyenneds3,Moyenneds4,Moyenneds5)

plot(TempMoy,Vectmoyenneds)


modelm6<-lm(Vectmoyenneds~poly(TempMoy,2))
modelm7<-lm(Vectmoyenneds~poly(TempMoy,3))

summary(modelm6)
summary(modelm7)

plot(TempMoy,Vectmoyenneds,ylim=c(-1*10**-7,2*10**-6),main="Approximation des valeurs moyennes du taux d'accroissement")
lines(TempMoy,predict(modelm7),type="l",lty=2, lwd=2) 





ds100<-tauxaccroiss(df100)
ds200<-tauxaccroiss(df200)
ds300<-tauxaccroiss(df300)
ds400<-tauxaccroiss(df400)
ds500<-tauxaccroiss(df500)



par(mfrow=c(2,3))
plot(ds100,type="l",main="Température à 100")
plot(ds200,type="l",main="Température à 200")
plot(ds300,type="l",main="Température à 300")
plot(ds400,type="l",main="Température à 400")
plot(ds500,type="l",main="Température à 500")


Moyenneds100<-mean(ds100)
Moyenneds200<-mean(ds200)
Moyenneds300<-mean(ds300)
Moyenneds400<-mean(ds400)
Moyenneds500<-mean(ds500)

Vectmoyenneds00<-c(Moyenneds100,Moyenneds200,Moyenneds300,Moyenneds400,Moyenneds500)

plot(TempMoy,Vectmoyenneds00)


modelm10<-lm(Vectmoyenneds00~poly(TempMoy,2))
modelm11<-lm(Vectmoyenneds00~poly(TempMoy,3))

summary(modelm10)
summary(modelm11)
Vectmoyenneds00
plot(TempMoy,Vectmoyenneds00,main="Approximation des taux d'accroissements moyens ",ylim=c(-5*10**-9,1.2*10**-7))
lines(TempMoy,predict(modelm11),type="l",lty=2, lwd=2) 



#On fait une régression aprés T2
df101<-df1[df1$`Temperature (°C)`!=100,]
df102<-df101[0:9,]
df103<-df101[10:24,]
df201<-df2[df2$`Temperature (°C)`!=200,]
df202<-df2[0:19,]
df203<-df201[20:47,]
df301<-df3[df3$`Temperature (°C)`!=300,]
df302<-df301[0:29,]
df303<-df301[30:74,]
df401<-df4[df4$`Temperature (°C)`!=400,]
df402<-df401[0:39,]
df403<-df401[40:93,]
df501<-df5[df5$`Temperature (°C)`!=500,]
df502<-df501[0:49,]
df503<-df501[50:114,]

moyennedf103<-mean(df103$`Resistivity (Ohm m)`)
moyennedf203<-mean(df203$`Resistivity (Ohm m)`)
moyennedf303<-mean(df303$`Resistivity (Ohm m)`)
moyennedf403<-mean(df403$`Resistivity (Ohm m)`)
moyennedf503<-mean(df503$`Resistivity (Ohm m)`)

Vectmoyenne3<-c(moyennedf103,moyennedf203,moyennedf303,moyennedf403,moyennedf503)
Vectmoyenne3
modelm8<-lm(Vectmoyenne3~poly(TempMoy,2))
modelm9<-lm(Vectmoyenne3~poly(TempMoy,3))

plot(TempMoy,Vectmoyenne3,main="Approximation  sur l'intervalle [t2,+inf] ",ylim=c(-2*10**-3,0.06))
lines(TempMoy,predict(modelm9),type="l",lty=2, lwd=2) 


#Analyse de [t1,t2]


t1100=597
t2100=48558


t2100=1219
t2200=49124

t3100=1795
t3200=49750


t4100=2400
t4200=50358


t5100=3025
t5200=50985


plot(df1$`Temperature (°C)`[df1$`Times (s)`<=597],df1$`Resistivity (Ohm m)`[df1$`Times (s)`<=597],col="red")
plot(df2$`Temperature (°C)`[df2$`Times (s)`<=1219],df2$`Resistivity (Ohm m)`[df2$`Times (s)`<=1219],col="red")


boxplot(names=c("100°C","200°C","300°C","400°C","500°C"),ylab="Résistivité en Ω.m",xlab="Température seuil",df1$`Resistivity (Ohm m)`,df2$`Resistivity (Ohm m)`,df3$`Resistivity (Ohm m)`,df4$`Resistivity (Ohm m)`,df5$`Resistivity (Ohm m)`)




plot(df1$`Temperature (°C)`[df1$`Times (s)`>=48558],df1$`Resistivity (Ohm m)`[df1$`Times (s)`>=48558],col="green")

plot(df2$`Temperature (°C)`[df2$`Times (s)`<=1219],df2$`Resistivity (Ohm m)`[df2$`Times (s)`<=1219],col="red")
plot(df2$`Temperature (°C)`[df2$`Times (s)`>=49124],df2$`Resistivity (Ohm m)`[df2$`Times (s)`>=49124],col="green")

plot(df3$`Temperature (°C)`[df3$`Times (s)`<=1795],df3$`Resistivity (Ohm m)`[df3$`Times (s)`<=1795],col="red")
plot(df3$`Temperature (°C)`[df3$`Times (s)`>=49750],df3$`Resistivity (Ohm m)`[df3$`Times (s)`>=49750],col="green")

plot(df4$`Temperature (°C)`[df4$`Times (s)`<=2400],df4$`Resistivity (Ohm m)`[df4$`Times (s)`<=2400],col="red")
plot(df4$`Temperature (°C)`[df4$`Times (s)`>=50358],df4$`Resistivity (Ohm m)`[df4$`Times (s)`>=50358],col="green")

plot(df5$`Temperature (°C)`[df5$`Times (s)`<=3025],df5$`Resistivity (Ohm m)`[df5$`Times (s)`<=3025],col="red")
plot(df5$`Temperature (°C)`[df5$`Times (s)`>=50985],df4$`Resistivity (Ohm m)`[df5$`Times (s)`>=50985],col="green")


df102<-df1[df1$`Times (s)`<=597,]
df103<-df1[df1$`Times (s)`>=48558,]

df202<-df2[df2$`Times (s)`<=1219,]
df203<-df2[df2$`Times (s)`>=49124,]

df302<-df3[df3$`Times (s)`<=1795,]
df303<-df3[df3$`Times (s)`>=49750,]

df402<-df4[df4$`Times (s)`<=2400,]
df403<-df4[df4$`Times (s)`>=50358,]

df502<-df5[df5$`Times (s)`<=3025,]
df503<-df5[df5$`Times (s)`>=50985,]


plot(df102$`Temperature (°C)`,df102$`Resistivity (Ohm m)`)

ds102<-tauxaccroiss(df102)
ds103<-tauxaccroiss(df103)

ds202<-tauxaccroiss(df202)
ds203<-tauxaccroiss(df203)

ds302<-tauxaccroiss(df302)
ds303<-tauxaccroiss(df303)

ds402<-tauxaccroiss(df402)
ds403<-tauxaccroiss(df403)

ds502<-tauxaccroiss(df502)
ds503<-tauxaccroiss(df503)

par(mfrow=c(2,3))
plot(ds102,type="l",main="Température à 100")
plot(ds202,type="l",main="Température à 200")
plot(ds302,type="l",main="Température à 300")
plot(ds402,type="l",main="Température à 400")
plot(ds502,type="l",main="Température à 500")


par(mfrow=c(2,3))
plot(ds103,type="l",main="Température à 100")
plot(ds203,type="l",main="Température à 200")
plot(ds303,type="l",main="Température à 300")
plot(ds403,type="l",main="Température à 400")
plot(ds503,type="l",main="Température à 500")

hyst<-Data_Hysteresis_5_T

hyst100<-hyst[,1:2]
hyst200<-hyst[,3:4]
hyst300<-hyst[,5:6]
hyst400<-hyst[,7:8]
hyst500<-hyst[,9:10]
h

colnames(hyst500)<-c("Température","Résistivité")
colnames(hyst100)<-c("Température","Résistivité")
colnames(hyst200)<-c("Température","Résistivité")
colnames(hyst300)<-c("Température","Résistivité")
colnames(hyst400)<-c("Température","Résistivité")





plot(hyst100$Température,hyst100$Résistivité)
plot(hyst200$Température,hyst200$Résistivité)
plot(hyst300$Température,hyst300$Résistivité)
plot(hyst400$Température,hyst400$Résistivité,type="l")
plot(hyst500$Température,hyst500$Résistivité)

hyst100$Température<-as.numeric(hyst100$Température)
hyst200$Température<-as.numeric(hyst200$Température)
hyst300$Température<-as.numeric(hyst300$Température)
hyst400$Température<-as.numeric(hyst400$Température)
hyst500$Température<-as.numeric(hyst500$Température)

hyst100$Résistivité<-as.numeric(hyst100$Résistivité)
hyst200$Résistivité<-as.numeric(hyst200$Résistivité)
hyst300$Résistivité<-as.numeric(hyst300$Résistivité)
hyst400$Résistivité<-as.numeric(hyst400$Résistivité)
hyst500$Résistivité<-as.numeric(hyst500$Résistivité)

#On décompose en 2 les hystérésis
hyst101<-hyst100[1:42,]
hyst102<-hyst100[42:82,]

hyst201<-hyst200[1:42,]
hyst202<-hyst200[42:78,]

hyst301<-hyst300[1:40,]
hyst302<-hyst300[40:77,]

hyst401<-hyst400[1:39,]
hyst402<-hyst400[39:75,]

hyst501<-hyst500[1:41,]
hyst502<-hyst500[41:82,]


mean(hyst101$Résistivité)
mean(hyst201$Résistivité)
mean(hyst301$Résistivité)
mean(hyst401$Résistivité)
mean(hyst501$Résistivité)


VectMoyHyst<-c(mean(hyst101$Résistivité),mean(hyst201$Résistivité),mean(hyst301$Résistivité),mean(hyst401$Résistivité),mean(hyst501$Résistivité))
VectMoyHyst2<-c(mean(hyst101$Résistivité),mean(hyst201$Résistivité),mean(hyst301$Résistivité),mean(hyst401$Résistivité)

plot(TempMoy,VectMoyHyst)
plot(TempMoy2,VectMoyHyst2)

sd(hyst101$Résistivité)
sd(hyst201$Résistivité)
sd(hyst301$Résistivité)
sd(hyst401$Résistivité)
sd(hyst501$Résistivité)

mean(hyst101$Résistivité)
sd(hyst201$Résistivité)
sd(hyst301$Résistivité)
sd(hyst401$Résistivité)
sd(hyst501$Résistivité)
VectMoyHyst<-c(mean(hyst101$Résistivité),mean(hyst201$Résistivité),mean(hyst301$Résistivité),mean(hyst401$Résistivité),mean(hyst501$Résistivité))
VectMoyHyst2<-c(mean(hyst101$Résistivité),mean(hyst201$Résistivité),mean(hyst301$Résistivité),mean(hyst401$Résistivité))

plot(TempMoy,VectMoyHyst,main="Courbe de la résistivité moyenne de la partie supérieur de l'hystérésis")
plot(TempMoy2,VectMoyHyst2,main="Courbe de la résistivité moyenne de la partie supérieur de l'hystérésis")

VectMoyHyst1<-c(mean(hyst102$Résistivité),mean(hyst202$Résistivité),mean(hyst302$Résistivité),mean(hyst402$Résistivité),mean(hyst502$Résistivité))
VectMoyHyst12<-c(mean(hyst102$Résistivité),mean(hyst202$Résistivité),mean(hyst302$Résistivité),mean(hyst402$Résistivité))

plot(TempMoy,VectMoyHyst1,main="Courbe de la résistivité moyenne de la partie supérieur de l'hystérésis")
plot(TempMoy2,VectMoyHyst12,main="Courbe de la résistivité moyenne de la partie supérieur de l'hystérésis")





VectSdHyst<-c(sd(hyst101$Résistivité),sd(hyst201$Résistivité),sd(hyst301$Résistivité),sd(hyst401$Résistivité),sd(hyst501$Résistivité))
VectSdHyst2<-c(sd(hyst101$Résistivité),sd(hyst201$Résistivité),sd(hyst301$Résistivité),sd(hyst401$Résistivité))
plot(TempMoy,VectSdHyst,main="Courbe de l'écart-type de l'hystérésis supérieur",xlab="Température")
plot(TempMoy2,VectSdHyst2,main="Courbe de l'écart-type de l'hystérésis supérieur",xlab="Température")

VectSdHyst11<-c(sd(hyst202$Résistivité),sd(hyst202$Résistivité),sd(hyst302$Résistivité),sd(hyst402$Résistivité),sd(hyst502$Résistivité))
VectSdHyst112<-c(sd(hyst202$Résistivité),sd(hyst202$Résistivité),sd(hyst302$Résistivité),sd(hyst402$Résistivité))
plot(TempMoy,VectSdHyst11,main="Courbe de l'écart-type de l'hystérésis inférieur",xlab="Température")
plot(TempMoy2,VectSdHyst112,main="Courbe de l'écart-type de l'hystérésis inférieur",xlab="Température")





#Calcul erreur quadratique moyenne 
par(mfrow=c(2,3))
#Hystérsis 100°C
plot(hyst101$Température,hyst101$Résistivité,col="red",main="Hystérsis pour le film à 100°C")
points(hyst102$Température,hyst102$Résistivité,col="blue")

#Hystérésis 200°C
plot(hyst201$Température,hyst201$Résistivité,col="red",main="Hystérsis pour le film à 200°C")
points(hyst102$Température,hyst102$Résistivité,col="blue")
#,ylim=c(7.10**-6,9.6*10**-6))
plot(df202$`Temperature (°C)`[df202$`Temperature (°C)`<=100],df202$`Resistivity (Ohm m)`[df202$`Temperature (°C)`<=100],col="blue")
#Hystérsis 300°C
plot(hyst301$Température,hyst301$Résistivité,col="red",main="Hystérsis pour le film à 200°C")#,ylim=c(7.10**-6,9.6*10**-6))
points(hyst302$Température,hyst302$Résistivité,col="blue")
plot(df302$`Temperature (°C)`[df302$`Temperature (°C)`<=100],df302$`Resistivity (Ohm m)`[df302$`Temperature (°C)`<=100],col="blue")
#Hystérésis 400°C
plot(hyst401$Température,hyst401$Résistivité,col="red",main="Hystérsis pour le film à 200°C")#,ylim=c(7.10**-6,9.6*10**-6))
points(hyst402$Température,hyst402$Résistivité,col="blue")
plot(df401$`Temperature (°C)`[df401$`Temperature (°C)`<=100],df402$`Resistivity (Ohm m)`[df402$`Temperature (°C)`<=100],col="blue")
#Hystérsis 500°C
plot(hyst501$Température,hyst501$Résistivité,col="red",main="Hystérsis pour le film à 200°C")#,ylim=c(7.10**-6,9.6*10**-6))
points(hyst502$Température,hyst502$Résistivité,col="blue")
plot(df502$`Temperature (°C)`[df502$`Temperature (°C)`<=100],df502$`Resistivity (Ohm m)`[df502$`Temperature (°C)`<=100],col="blue")



#Calcul différence entre les 2 oparties de l'hystérésis
plot(hyst101[3:41,]$Température)
points(rev(hyst102[2:41,]$Température),col="blue")

plot(hyst201[6:42,]$Température)
points(rev(hyst202$Température),col="blue")


plot(hyst301[4:40,]$Température)
points(rev(hyst302[2:40,]$Température),col="blue")


plot(hyst401[3:39,]$Température)
points(rev(hyst402$Température),col="blue")


plot(hyst501[5:41,]$Température)
points(rev(hyst502[1:36,]$Température),col="blue")


plot(hyst400$Température,hyst400$Résistivité)




plot(hyst102$Température,hyst102$Résistivité,col="red",ylim=c(7.10**-6,9.6*10**-6))
plot(df103$`Temperature (°C)`,df103$`Resistivity (Ohm m)`,col="blue")

par(mfrow=c(2,3))
#1e courbe
df103bis<-c(25,25.9,27.2,28.9,30.9,32.9,36.9,40.9,45.2,52.9,60.9,70.9,80.9,91.7,100)

hyst102bis<-hyst102[c(1,5,11,16,21,25,29,31,33,35,37,38,39,41),]
df103bis<-df103[-c(2,6),]
df103bis$`Temperature (°C)`<-hyst102bis$Température
plot(hyst102bis$Température,hyst102bis$Résistivité,col="red",main="Comparaison Courbe et Hystérésis à 100°C")
points(df103bis$`Temperature (°C)`,df103bis$`Resistivity (Ohm m)`,col="blue")

rmsevect1<-(df103bis$`Resistivity (Ohm m)`-hyst102bis$Résistivité)^2
rmse1<-mean(rmsevect1)
cor1<-cor(df103bis$`Resistivity (Ohm m)`,hyst102bis$Résistivité)

#2e courbe

plot(hyst202$Température,hyst202$Résistivité,col="red")#,ylim=c(7.10**-6,9.6*10**-6))
points(df203$`Temperature (°C)`[df203$`Temperature (°C)`<=100],df203$`Resistivity (Ohm m)`[df203$`Temperature (°C)`<=100],col="blue")

hyst202bis<-hyst202[c(2,7,12,17,22,26,29,31,33,35,36,37),]
df203bis<-df203[df203$`Temperature (°C)`<=100,]
df203bis<-df203bis[-c(13:18),]
df203bis$`Temperature (°C)`<-hyst202bis$Température

plot(hyst202bis$Température,hyst202bis$Résistivité,col="red",main="Comparaison Courbe et Hystérésis à 200°C")
points(df203bis$`Temperature (°C)`,df203bis$`Resistivity (Ohm m)`,col="blue")

rmsevect2<-(df203bis$`Resistivity (Ohm m)`-hyst202bis$Résistivité)^2
rmse2<-mean(rmsevect2)
cor2<-cor(df203bis$`Resistivity (Ohm m)`,hyst202bis$Résistivité)




#3e courbe
plot(hyst302$Température,hyst302$Résistivité,col="red")#,ylim=c(7.10**-6,9.6*10**-6))
plot(df303$`Temperature (°C)`[df302$`Temperature (°C)`<=100],df303$`Resistivity (Ohm m)`[df302$`Temperature (°C)`<=100],col="blue")

hyst302bis<-hyst302[c(5,10,15,20,25,28,31,33,34,35,36,37),]
df303bis<-df303[df303$`Temperature (°C)`<=100,]
df303bis<-df303bis[-c(13:25),]
df303bis$`Temperature (°C)`<-hyst302bis$Température

plot(hyst302bis$Température,hyst302bis$Résistivité,col="red",main="Comparaison Courbe et Hystérésis à 300°C")
points(df303bis$`Temperature (°C)`,df303bis$`Resistivity (Ohm m)`,col="blue")

rmsevect3<-(df303bis$`Resistivity (Ohm m)`-hyst302bis$Résistivité)^2
rmse3<-mean(rmsevect3)
cor3<-cor(df303bis$`Resistivity (Ohm m)`,hyst302bis$Résistivité)

#4e courbe
plot(hyst402$Température,hyst402$Résistivité,col="red")#,ylim=c(7.10**-6,9.6*10**-6))
plot(df403$`Temperature (°C)`[df403$`Temperature (°C)`<=100],df403$`Resistivity (Ohm m)`[df403$`Temperature (°C)`<=100],col="blue")

hyst402bis<-hyst402[c(5,10,15,20,24,28,30,32,34,35,36,37),]
df403bis<-df403[df403$`Temperature (°C)`<=100,]
df403bis<-df403bis[-c(13:24),]
df403bis$`Temperature (°C)`<-hyst402bis$Température

plot(hyst402bis$Température,hyst402bis$Résistivité,col="red",main="Comparaison Courbe et Hystérésis à 400°C")
points(df403bis$`Temperature (°C)`,df403bis$`Resistivity (Ohm m)`,col="blue")

rmsevect4<-(df403bis$`Resistivity (Ohm m)`-hyst402bis$Résistivité)^2
rmse4<-mean(rmsevect4)
cor4<-cor(df403bis$`Resistivity (Ohm m)`,hyst402bis$Résistivité)


#5e courbe
hyst502bis<-hyst502[c(1,7,12,17,22,26,28,31,32,34,36,37),]
df503bis<-df503[df503$`Temperature (°C)`<=100,]
df503bis<-df503bis[-c(10,12,15:26),]
df503bis$`Temperature (°C)`<-hyst502bis$Température

plot(hyst502bis$Température,hyst502bis$Résistivité,col="red",main="Comparaison Courbe et Hystérésis à 500°C")
points(df503bis$`Temperature (°C)`,df503bis$`Resistivity (Ohm m)`,col="blue")

rmsevect5<-(df503bis$`Resistivity (Ohm m)`-hyst502bis$Résistivité)^2
rmse5<-mean(rmsevect5)
cor5<-cor(df503bis$`Resistivity (Ohm m)`,hyst502bis$Résistivité)


vectrmse<-c(rmse1,rmse2,rmse3,rmse4,rmse5)
vectcor<-c(cor1,cor2,cor3,cor4,cor5)


plot(TempMoy,vectrmse,main="Erreur qudratique moyenne entre courbe et hystérésis")
plot(TempMoy,vectcor,main="Corrélation entre courbe et hystérésis")



#Entre [0,t1]
#1e courbe
hyst502bis<-hyst502[c(),]
df102bis<-df102[df102$`Temperature (°C)`<=100,]
df503bis<-df503bis[-c(),]
df503bis$`Temperature (°C)`<-hyst502bis$Température

plot(hyst101$Température,hyst101$Résistivité,col="red")
points(df102bis$`Temperature (°C)`,df102bis$`Resistivity (Ohm m)`,col="blue")



#2e courbe
hyst201bis<-hyst201[c(),]
df202bis<-df202[df202$`Temperature (°C)`<=100,]
df503bis<-df503bis[-c(),]
df503bis$`Temperature (°C)`<-hyst502bis$Température

plot(hyst201$Température,hyst201$Résistivité,col="red")
plot(df202bis$`Temperature (°C)`,df202bis$`Resistivity (Ohm m)`,col="blue")


#3e courbe
hyst301bis<-hyst301[c(),]
df302bis<-df302[df302$`Temperature (°C)`<=100,]
df503bis<-df503bis[-c(),]
df503bis$`Temperature (°C)`<-hyst502bis$Température

plot(hyst301$Température,hyst301$Résistivité,col="red")
plot(df302bis$`Temperature (°C)`,df302bis$`Resistivity (Ohm m)`,col="blue")


#4e courbe
hyst301bis<-hyst301[c(),]
df402bis<-df402[df402$`Temperature (°C)`<=100,]
df503bis<-df503bis[-c(),]
df503bis$`Temperature (°C)`<-hyst502bis$Température

plot(hyst401$Température,hyst401$Résistivité,col="red")
plot(df402bis$`Temperature (°C)`,df402bis$`Resistivity (Ohm m)`,col="blue")


#5e courbe
hyst301bis<-hyst301[c(),]
df502bis<-df502[df502$`Temperature (°C)`<=100,]
df503bis<-df503bis[-c(),]
df503bis$`Temperature (°C)`<-hyst502bis$Température

plot(hyst501$Température,hyst501$Résistivité,col="red")
plot(df502bis$`Temperature (°C)`,df502bis$`Resistivity (Ohm m)`,col="blue")



#2e jeux de donnée avec profil température
DF<-Data_Figure5
cycle15<-DF[,1:2]
cycle610<-DF[,3:4]
cycle11<-DF[,5:6]


#Cycle 1 à 5
cycle15<-cycle15[-c(1),]
cycle15<-cycle15[1:240,]
colnames(cycle15)<-c("T(°C)","Rho (Ohm.m)")
cycle15$`T(°C)`<-as.numeric(cycle15$`T(°C)`)
cycle15$`Rho (Ohm.m)`<-as.numeric(cycle15$`Rho (Ohm.m)`)

#Cycle 6 à 10

cycle610<-cycle610[-c(712),]
colnames(cycle610)<-c("T(°C)","Rho (Ohm.m)")
cycle610$`T(°C)`<-as.numeric(cycle610$`T(°C)`)
cycle610$`Rho (Ohm.m)`<-as.numeric(cycle610$`Rho (Ohm.m)`)


#Cycle 11
cycle11<-cycle11[-c(1),]
cycle11<-cycle11[1:210,]
colnames(cycle11)<-c("T(°C)","Rho (Ohm.m)")
cycle11$`T(°C)`<-as.numeric(cycle11$`T(°C)`)
cycle11$`Rho (Ohm.m)`<-as.numeric(cycle11$`Rho (Ohm.m)`)

par(mfrow=c(2,2))
plot(cycle15$`T(°C)`,main="Profil Température 1 à 5")#50 puis 100 jusqu(à 250)

plot(cycle610$`T(°C)`,main = "Profil Température 6 à 10")#300 jusqu'à 500
plot(cycle11$`T(°C)`,main = "Profil Température 11")#550

plot(cycle$`T(°C)`,main="Profil de Température du cycle")

plot(cycle15$`Rho (Ohm.m)`)

#Fusion des 3 data frame 
cycle15bis<-cycle15
cycle15bis<-rbind(cycle15bis,cycle610)
cycle<-rbind(cycle15bis,cycle11)


plot(cycle$`Rho (Ohm.m)`,ylab="Résistivité",xlab="Temps",main="Résistivité durant l'intégralité du cycle")
points(cycle$`T(°C)`,col="red")



vectPicTemp<-c(8,30,70,126,206,292,409,546,687,861,1057)
vectCreuxTemps<-c(1,14,45,92,161,239,343,471,602,766,951,1164)
MoyenneCourbe11<-rep(0,11)
SdCourbe11<-rep(0,11)
par(mfrow=c(3,4))
for (i in 1:11){
  
  plot(cycle$`Rho (Ohm.m)`[vectCreuxTemps[i]:vectCreuxTemps[i+1]],xlab="Résistivité",ylab="Temps",main="Courbe pour chaque pic")
  SdCourbe11[i]<-sd(na.rm=TRUE,cycle[vectCreuxTemps[i]:vectCreuxTemps[i+1],]$`Rho (Ohm.m)`)
  MoyenneCourbe11[i]<-mean(na.rm=TRUE,cycle[vectCreuxTemps[i]:vectCreuxTemps[i+1],]$`Rho (Ohm.m)`)
}
cycle$`Rho (Ohm.m)`<-as.numeric(cycle$`Rho (Ohm.m)`)
MoyenneCourbe12<-MoyenneCourbe11[-c(11)]
SdCourbe12<-SdCourbe11[-c(11)]
VectTemp<-c(50,100,150,200,250,300,350,400,450,500,550)
VectTemp2<-c(50,100,150,200,250,300,350,400,450,500)
plot(VectTemp,MoyenneCourbe11,main="Courbe des valeurs moyennes de la résistivité pour chaque pic")
plot(VectTemp2,MoyenneCourbe12,main="Courbe des valurs moyenne de la résistivité pour chaque pic")

plot(VectTemp,SdCourbe11,main="Courbe d el'écart-type de la résistivité pour chaque pic")
plot(VectTemp2,SdCourbe12,main="Courbe de l'écart-type de la résistivité pour chaque pic")


tauxaccroiss<-function(df){
  ds<-rep(0,dim(df)[1]-1)
  for (i in 1:dim(df)[1]-1){
    
    ds[i]<-(df$`Rho (Ohm.m)`[i+1]-df$`Rho (Ohm.m)`[i])
    
  }
  return(ds)
}

#Taux Accroissement par apport à la température 
ds1<-tauxaccroiss(cycle[vectCreuxTemps[1]:vectCreuxTemps[2],])
ds2<-tauxaccroiss(cycle[vectCreuxTemps[2]:vectCreuxTemps[3],])
ds3<-tauxaccroiss(cycle[vectCreuxTemps[3]:vectCreuxTemps[4],])
ds4<-tauxaccroiss(cycle[vectCreuxTemps[4]:vectCreuxTemps[5],])
ds5<-tauxaccroiss(cycle[vectCreuxTemps[5]:vectCreuxTemps[6],])
ds6<-tauxaccroiss(cycle[vectCreuxTemps[6]:vectCreuxTemps[7],])
ds7<-tauxaccroiss(cycle[vectCreuxTemps[7]:vectCreuxTemps[8],])
ds8<-tauxaccroiss(cycle[vectCreuxTemps[8]:vectCreuxTemps[9],])
ds9<-tauxaccroiss(cycle[vectCreuxTemps[9]:vectCreuxTemps[10],])
ds10<-tauxaccroiss(cycle[vectCreuxTemps[10]:vectCreuxTemps[11],])
ds11<-tauxaccroiss(cycle[vectCreuxTemps[11]:vectCreuxTemps[12],])
#Hystérésis 

par(mfrow=c(3,4))
for (i in 1:11){
  plot(cycle$`T(°C)`[vectCreuxTemps[i]:vectCreuxTemps[i+1]],cycle$`Rho (Ohm.m)`[vectCreuxTemps[i]:vectCreuxTemps[i+1]],xlab="Résistivité",ylab="Température",main="Courbe de la résistivité en fonction de la température pour chaque pic")
  #MoyenneCourbe11[i]<-mean(cycle[vectCreuxTemps[i]:vectCreuxTemps[i+1],]$`Rho (Ohm.m)`)
}
plot(cycle$`T(°C)`[vectCreuxTemps[5]:vectCreuxTemps[5+1]],cycle$`Rho (Ohm.m)`[vectCreuxTemps[5]:vectCreuxTemps[5+1]],xlab="Résistivité",ylab="Température",main="Courbe de la résistivité en fonction de la température pour chaque pic")

cycle$`Rho (Ohm.m)`[vectCreuxTemps[5]:vectCreuxTemps[5+1]]



par(mfrow=c(3,4))
for (i in 1:11){

  plot(cycle$`T(°C)`[vectCreuxTemps[i]:vectCreuxTemps[i+1]],xlab="Résistivité",ylab="Température",main="Hystérésis pour chaque pic")
  #MoyenneCourbe11[i]<-mean(cycle[vectCreuxTemps[i]:vectCreuxTemps[i+1],]$`Rho (Ohm.m)`)
}

#100
par(mfrow=c(2,2))
hist(hyst100$Résistivité)
hist(cycle$`Rho (Ohm.m)`[vectCreuxTemps[2]:vectCreuxTemps[2+1]])
plot(density(main="Densité pour l'hystérésis à 100°",hyst100$Résistivité),kernel="epanechnikov")
plot(main="Densité à 100 degré",density(cycle$`Rho (Ohm.m)`[vectCreuxTemps[2]:vectCreuxTemps[2+1]],kernel="epanechnikov"))

boxplot(main="Boite à moustache des 2 profils à 100°C",names=c("hystérésis","hystérésis avec fatigue"),ylab="Résisitivité",hyst100$Résistivité,cycle$`Rho (Ohm.m)`[vectCreuxTemps[2]:vectCreuxTemps[2+1]])


#200
par(mfrow=c(2,2))
hist(hyst200$Résistivité)
hist(cycle$`Rho (Ohm.m)`[vectCreuxTemps[4]:vectCreuxTemps[4+1]])
plot(density(main="Densité pour l'hystérésis à 200°",hyst200$Résistivité),kernel="epanechnikov")
plot(main="Densité à 200 °",density(cycle$`Rho (Ohm.m)`[vectCreuxTemps[4]:vectCreuxTemps[4+1]],kernel="epanechnikov"))


boxplot(main="Boite à moustache des 2 profils à 200°C",names=c("hystérésis","hystérésis avec fatigue"),ylab="Résisitivité",hyst200$Résistivité,cycle$`Rho (Ohm.m)`[vectCreuxTemps[4]:vectCreuxTemps[4+1]])

#300
par(mfrow=c(2,2))
hist(hyst300$Résistivité)
hist(cycle$`Rho (Ohm.m)`[vectCreuxTemps[6]:vectCreuxTemps[6+1]])
plot(density(main="Densité pour l'hystérésis à 300°",hyst300$Résistivité),kernel="epanechnikov")
plot(main="Densité à 300°",density(cycle$`Rho (Ohm.m)`[(vectCreuxTemps[6]+5):vectCreuxTemps[6+1]],kernel="epanechnikov"))

boxplot(main="Boite à moustache des 2 profils à 300°C",names=c("hystérésis","hystérésis avec fatigue"),ylab="Résisitivité",hyst300$Résistivité,cycle$`Rho (Ohm.m)`[vectCreuxTemps[6]:vectCreuxTemps[6+1]])

#400
par(mfrow=c(2,2))
hist(hyst400$Résistivité)
hist(cycle$`Rho (Ohm.m)`[vectCreuxTemps[8]:vectCreuxTemps[8+1]])
plot(density(main="Densité pour l'hystérésis à 400°",hyst400$Résistivité),kernel="epanechnikov")
plot(main="Densité à 400°",density(cycle$`Rho (Ohm.m)`[vectCreuxTemps[8]:vectCreuxTemps[8+1]],kernel="epanechnikov"))

boxplot(main="Boite à moustache des 2 profils à 400°C",names=c("hystérésis","hystérésis avec fatigue"),ylab="Résisitivité",hyst400$Résistivité,cycle$`Rho (Ohm.m)`[vectCreuxTemps[8]:vectCreuxTemps[8+1]])


par(mfrow=c(2,2))
hist(hyst500$Résistivité)
hist(cycle$`Rho (Ohm.m)`[vectCreuxTemps[10]:vectCreuxTemps[10+1]])
plot(density(main="Densité pour l'hystérésis à 6500°",hyst500$Résistivité),kernel="epanechnikov")
plot(main="Densité à 500°",density(cycle$`Rho (Ohm.m)`[vectCreuxTemps[10]:vectCreuxTemps[10+1]],kernel="epanechnikov"))

shapiro.test(hyst500$Résistivité)#ecla ne suit pasune loi normale


boxplot(main="Boite à moustache des 2 profils à 500°C",names=c("hystérésis","hystérésis avec fatigue"),ylab="Résisitivité",hyst500$Résistivité,cycle$`Rho (Ohm.m)`[vectCreuxTemps[10]:vectCreuxTemps[10+1]])
boxplot(ylab="Résisitivité",names=c("100°C","200°C","300°C","400°C"),df1$`Resistivity (Ohm m)`,df2$`Resistivity (Ohm m)`,df3$`Resistivity (Ohm m)`,df4$`Resistivity (Ohm m)`)

boxplot(main="Boite à moustache",ylab="Résisitivité",names=c("100°C","200°C","300°C","400°C","500°C"),df1$`Resistivity (Ohm m)`,df2$`Resistivity (Ohm m)`,df3$`Resistivity (Ohm m)`,df4$`Resistivity (Ohm m)`,df5$`Resistivity (Ohm m)`)

#Boite à moustache figure à 5 tempéraure
boxplot(hyst100$Résistivité,hyst200$Résistivité,hyst300$Résistivité,hyst400$Résistivité)

#Boite à moustache hystérésis 2e figure

boxplot(main="Boite à moustache de 10 premiers pics",ylab="Résistivité",xlab="Température",cycle$`Rho (Ohm.m)`[vectCreuxTemps[1]:vectCreuxTemps[2]],cycle$`Rho (Ohm.m)`[vectCreuxTemps[2]:vectCreuxTemps[3]],cycle$`Rho (Ohm.m)`[vectCreuxTemps[3]:vectCreuxTemps[4]],cycle$`Rho (Ohm.m)`[vectCreuxTemps[4]:vectCreuxTemps[5]],cycle$`Rho (Ohm.m)`[vectCreuxTemps[5]:vectCreuxTemps[6]],cycle$`Rho (Ohm.m)`[vectCreuxTemps[6]:vectCreuxTemps[7]],cycle$`Rho (Ohm.m)`[vectCreuxTemps[7]:vectCreuxTemps[8]],cycle$`Rho (Ohm.m)`[vectCreuxTemps[8]:vectCreuxTemps[9]],cycle$`Rho (Ohm.m)`[vectCreuxTemps[9]:vectCreuxTemps[10]],cycle$`Rho (Ohm.m)`[vectCreuxTemps[10]:vectCreuxTemps[10+1]])



#ACP et Matrice de corrélation
library(factoextra)
library(FactoMineR)
library(corrplot)
cycle.acp <- PCA(cycle,graph=FALSE)

fviz_pca_ind(cycle.acp, col.ind="cos2")
fviz_pca_ind(cycle.acp, col.ind="contrib")
fviz_pca_var(cycle.acp, col.var="cos2")

fviz_pca_biplot(cycle.acp)
corrplot(cor(cycle))


#Déterminons la loi qui suit la distrib des hystérésis
library(fitdistrplus)
library(mc2d)
plotdist(hyst100$Résistivité)
hist(hyst100$Résistivité)
plot(density(hyst100$Résistivité),kernel="epanechnikov")
plot(density(cycle$`Rho (Ohm.m)`[vectCreuxTemps[2]:vectCreuxTemps[2+1]]),kernel="epanechnikov")
hist(cycle$`Rho (Ohm.m)`[vectCreuxTemps[2]:vectCreuxTemps[2+1]])
#Graphe de Cullen Frey

par(mfrow=c(1,2))
descdist(hyst100$Résistivité,boot=100)
descdist(cycle$`Rho (Ohm.m)`[vectCreuxTemps[2]:vectCreuxTemps[2+1]],boot=100)

par(mfrow=c(1,2))
descdist(hyst200$Résistivité,boot=100)
descdist(cycle$`Rho (Ohm.m)`[vectCreuxTemps[4]:vectCreuxTemps[4+1]],boot=100)

par(mfrow=c(1,2))
descdist(hyst300$Résistivité,boot=100)
descdist(cycle$`Rho (Ohm.m)`[(vectCreuxTemps[6]+5):vectCreuxTemps[6+1]],boot=100)

par(mfrow=c(1,2))
descdist(hyst400$Résistivité,boot=100)
descdist(cycle$`Rho (Ohm.m)`[vectCreuxTemps[8]:vectCreuxTemps[8+1]],boot=100)

par(mfrow=c(1,2))
descdist(hyst500$Résistivité,boot=100)
descdist(cycle$`Rho (Ohm.m)`[vectCreuxTemps[10]:vectCreuxTemps[10+1]],boot=100)

#On n'a des valeurs manquantes
hyst300$Température
hyst200<-hyst200[-c(79:82),]#On n'a pas les valeurs à 20 degré
hyst300<-hyst300[-c(78:82),]
hyst400<-hyst400[-c(76:82),]
hyst500

descdist(cycle$`Rho (Ohm.m)`[vectCreuxTemps[2]:vectCreuxTemps[2+1]],boot=1000)
descdist(cycle$`Rho (Ohm.m)`[vectCreuxTemps[4]:vectCreuxTemps[4+1]],boot=1000)
descdist(cycle$`Rho (Ohm.m)`[(vectCreuxTemps[6]+5):vectCreuxTemps[6+1]],boot=1000)
descdist(cycle$`Rho (Ohm.m)`[vectCreuxTemps[8]:vectCreuxTemps[8+1]],boot=1000)
descdist(cycle$`Rho (Ohm.m)`[vectCreuxTemps[10]:vectCreuxTemps[10+1]],boot=1000)

hyst100$Résistivité
fitWeibull11 <- fitdist(hyst100$Résistivité,"uniform",method="mse")#J'ai une erreur avec la méthode du max de vraisemblance
fitWeibull12 <- fitdist(cycle$`Rho (Ohm.m)`[vectCreuxTemps[2]:vectCreuxTemps[2+1]],"weibull",method="mse")
a<-cycle$`Rho (Ohm.m)`[vectCreuxTemps[2]:vectCreuxTemps[2+1]]
summary(fitWeibull11)
summary(fitWeibull12)
gofstat(fitWeibull11)
plot(fitWeibull11)
plot(fitWeibull51)#Marche bien

fitWeibull21 <- fitdist(hyst200$Résistivité,"weibull",method="mse")#J'ai une erreur avec la méthode du max de vraisemblance
fitWeibull22 <- fitdist(cycle$`Rho (Ohm.m)`[vectCreuxTemps[4]:vectCreuxTemps[4+1]],"weibull",method="mse")
summary(fitWeibull21)
summary(fitWeibull22)


fitWeibull31 <- fitdist(hyst300$Résistivité,"weibull",method="mse")#J'ai une erreur avec la méthode du max de vraisemblance
fitWeibull32 <- fitdist(cycle$`Rho (Ohm.m)`[vectCreuxTemps[6]:vectCreuxTemps[6+1]],"weibull",method="mse")
summary(fitWeibull31)
summary(fitWeibull32)


fitWeibull41 <- fitdist(hyst400$Résistivité,"weibull",method="mse")#J'ai une erreur avec la méthode du max de vraisemblance
fitWeibull42 <- fitdist(cycle$`Rho (Ohm.m)`[vectCreuxTemps[8]:vectCreuxTemps[8+1]],"weibull",method="mse")
summary(fitWeibull41)
summary(fitWeibull42)


fitWeibull51 <- fitdist(hyst500$Résistivité,"weibull",method="mse")#J'ai une erreur avec la méthode du max de vraisemblance
#Seul exemple qui a marché
fitWeibull52 <- fitdist(cycle$`Rho (Ohm.m)`[vectCreuxTemps[10]:vectCreuxTemps[10+1]],"weibull",method="mse")
summary(fitWeibull51)
summary(fitWeibull52)




#Hystérésis





