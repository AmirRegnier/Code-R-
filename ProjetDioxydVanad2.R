df<-Rho_vs_time_400C_600_to_3200min
df600<-df[,1:2]
df800<-df[,3:4]
df1000<-df[,5:6]
df1200<-df[,7:8]
df1400<-df[,9:10]
df2000<-df[,11:12]
df2600<-df[,13:14]
df3200<-df[,15:16]

df600<-df600[-c(1),]
df800<-df800[-c(1),]
df1000<-df1000[-c(1),]
df1200<-df1200[-c(1),]
df1400<-df1400[-c(1),]
df2000<-df2000[-c(1),]
df2600<-df2600[-c(1),]
df3200<-df3200[-c(1),]
#On renomme les noms des colonnes 
colnames(df600)<-c("Time (s)","Rho (Ohm m)")
colnames(df800)<-c("Time (s)","Rho (Ohm m)")
colnames(df1000)<-c("Time (s)","Rho (Ohm m)")
colnames(df1200)<-c("Time (s)","Rho (Ohm m)")
colnames(df1400)<-c("Time (s)","Rho (Ohm m)")
colnames(df2000)<-c("Time (s)","Rho (Ohm m)")
colnames(df2600)<-c("Time (s)","Rho (Ohm m)")
colnames(df3200)<-c("Time (s)","Rho (Ohm m)")

df600$`Time (s)`<-as.numeric(df600$`Time (s)`)
df600$`Rho (Ohm m)`<-as.numeric(df600$`Rho (Ohm m)`)

df600$`Time (s)`<-as.numeric(df600$`Time (s)`)
df600$`Rho (Ohm m)`<-as.numeric(df600$`Rho (Ohm m)`)

df800$`Time (s)`<-as.numeric(df800$`Time (s)`)
df800$`Rho (Ohm m)`<-as.numeric(df800$`Rho (Ohm m)`)

df1000$`Time (s)`<-as.numeric(df1000$`Time (s)`)
df1000$`Rho (Ohm m)`<-as.numeric(df1000$`Rho (Ohm m)`)

df1200$`Time (s)`<-as.numeric(df1200$`Time (s)`)
df1200$`Rho (Ohm m)`<-as.numeric(df1200$`Rho (Ohm m)`)

df1400$`Time (s)`<-as.numeric(df1400$`Time (s)`)
df1400$`Rho (Ohm m)`<-as.numeric(df1400$`Rho (Ohm m)`)

df2000$`Time (s)`<-as.numeric(df2000$`Time (s)`)
df2000$`Rho (Ohm m)`<-as.numeric(df2000$`Rho (Ohm m)`)

df2600$`Time (s)`<-as.numeric(df2600$`Time (s)`)
df2600$`Rho (Ohm m)`<-as.numeric(df2600$`Rho (Ohm m)`)

df3200$`Time (s)`<-as.numeric(df3200$`Time (s)`)
df3200$`Rho (Ohm m)`<-as.numeric(df3200$`Rho (Ohm m)`)

par(mfrow=c(2,4))
plot(xlab="Temps (en s)",ylab="Résistivité (en Ω.m)",df600$`Time (s)`,df600$`Rho (Ohm m)`,main="Courbe pour T=600")
plot(xlab="Temps (en s)",ylab="Résistivité (en Ω.m)",df800$`Time (s)`,df800$`Rho (Ohm m)`,main="Courbe pour T=800")
plot(xlab="Temps (en s)",ylab="Résistivité (en Ω.m)",df1000$`Time (s)`,df1000$`Rho (Ohm m)`,main="Courbe pour T=1000")
plot(xlab="Temps (en s)",ylab="Résistivité (en Ω.m)",df1200$`Time (s)`,df1200$`Rho (Ohm m)`,main="Courbe pour T=1200")
plot(xlab="Temps (en s)",ylab="Résistivité (en Ω.m)",df1400$`Time (s)`,df1400$`Rho (Ohm m)`,main="Courbe pour T=1400")
plot(xlab="Temps (en s)",ylab="Résistivité (en Ω.m)",df2000$`Time (s)`,df2000$`Rho (Ohm m)`,main="Courbe pour T=2000")
plot(xlab="Temps (en s)",ylab="Résistivité (en Ω.m)",df2600$`Time (s)`,df2600$`Rho (Ohm m)`,main="Courbe pour T=2600")
plot(xlab="Temps (en s)",ylab="Résistivité (en Ω.m)",df3200$`Time (s)`,df3200$`Rho (Ohm m)`,main="Courbe pour T=3200")

boxplot(main="Boite à moustache résistivité selon le temps d'oxydation",names=c("600 min","800min","1000 min","1200 min","1400 min","2000 min","2600 min","3200 min"),df600$`Rho (Ohm m)`,df800$`Rho (Ohm m)`,df1000$`Rho (Ohm m)`,df1200$`Rho (Ohm m)`,df1400$`Rho (Ohm m)`,df2000$`Rho (Ohm m)`,df2600$`Rho (Ohm m)`,df3200$`Rho (Ohm m)`)


df600<-df600[1:696,]
df800<-df800[1:892,]
df1000<-df1000[1:1095,]
df1200<-df1200[1:1292,]
df1400<-df1400[1:1491,]
df2000<-df2000[1:2086,]
df2600<-df2600[1:2677,]
df3200<-df3200[1:3266,]


Moyennedf600<-mean(df600$`Rho (Ohm m)`)
Moyennedf800<-mean(df800$`Rho (Ohm m)`)
Moyennedf1000<-mean(df1000$`Rho (Ohm m)`)
Moyennedf1200<-mean(df1200$`Rho (Ohm m)`)
Moyennedf1400<-mean(df1400$`Rho (Ohm m)`)
Moyennedf2000<-mean(df2000$`Rho (Ohm m)`)
Moyennedf2600<-mean(df2600$`Rho (Ohm m)`)
Moyennedf3200<-mean(df3200$`Rho (Ohm m)`)


Sddf600<-sd(df600$`Rho (Ohm m)`)
Sddf800<-sd(df800$`Rho (Ohm m)`)
Sddf1000<-sd(df1000$`Rho (Ohm m)`)
Sddf1200<-sd(df1200$`Rho (Ohm m)`)
Sddf1400<-sd(df1400$`Rho (Ohm m)`)
Sddf2000<-sd(df2000$`Rho (Ohm m)`)
Sddf2600<-sd(df2600$`Rho (Ohm m)`)
Sddf3200<-sd(df3200$`Rho (Ohm m)`)

summary(df2600)



VectMoyenne<-c(Moyennedf600,Moyennedf800,Moyennedf1000,Moyennedf1200,Moyennedf1400,Moyennedf2000,Moyennedf2600,Moyennedf3200)
TempsMoy<-c(600,800,1000,1200,1400,2000,2600,3200)
VectMoyenne
VectSd<-c(Sddf600,Sddf800,Sddf1000,Sddf1200,Sddf1400,Sddf2000,Sddf2600,Sddf3200)
TempsMoy<-c(600,800,1000,1200,1400,2000,2600,3200)



modelm1<-lm(VectMoyenne~poly(TempsMoy,2))
modelm2<-lm(VectMoyenne~poly(TempsMoy,3))
modelm3<-lm(VectMoyenne~poly(TempsMoy,4))
summary(modelm1)
summary(modelm2)
summary(modelm3)#Trés bon choix

plot(TempsMoy,VectMoyenne,main="Courbe Résistivité moyenne en fonction du Temps")
plot(TempsMoy,VectSd,main="Courbe de l'écaet-type de la résistivité en fonction du temps d'oxydation")
VectSd

plot(modelm3$fitted, modelm3$residuals, xlab="Valeurs ajustées", ylab="Résidus")
plot(modelm3$fitted.values,type="l")

plot(TempsMoy,VectMoyenne,main="Courbe Résistivité moyenne en fonction du temps")
lines(TempsMoy,predict(modelm2),type="l",lty=2, lwd=2)

plot(TempsMoy,VectMoyenne,main="Approximation de le résistivité moyenne en fonction du temps")
lines(TempsMoy,predict(modelm3),type="l",lty=2, lwd=2)#Meilleur modèle



#Taux Accroissement 
tauxaccroiss<-function(df){
  ds<-rep(0,dim(df)[1]-1)
  for (i in 1:dim(df)[1]-1){
    
    ds[i]<-(df$`Rho (Ohm m)`[i+1]-df$`Rho (Ohm m)`[i])/(df$`Time (s)`[i+1]-df$`Time (s)`[i])
    
  }
  return(ds)
}

ds600<-tauxaccroiss(df600)
ds800<-tauxaccroiss(df800)
ds1000<-tauxaccroiss(df1000)
ds1200<-tauxaccroiss(df1200)
ds1400<-tauxaccroiss(df1400)
ds2000<-tauxaccroiss(df2000)
ds2600<-tauxaccroiss(df2600)
ds3200<-tauxaccroiss(df3200)

par(mfrow=c(2,4))
plot(ds600,type="l",main="Temps à 600")
plot(ds800,type="l",main="Temps à 800")
plot(ds1000,type="l",main="Temps à 1000")
plot(ds1200,type="l",main="Temps à 1200")
plot(ds1400,type="l",main="Temps à 1400")
plot(ds2000,type="l",main="Temps à 2000")
plot(ds2600,type="l",main="Temps à 2600")
plot(ds3200,type="l",main="Temps à 3200")

Moyenneds600<-mean(ds600)
Moyenneds800<-mean(ds800)
Moyenneds1000<-mean(ds1000)
Moyenneds1200<-mean(ds1200)
Moyenneds1400<-mean(ds1400)
Moyenneds2000<-mean(ds2000)
Moyenneds2600<-mean(ds2600)
Moyenneds3200<-mean(ds3200)

VectMoyenneds<-c(Moyenneds600,Moyenneds800,Moyennedf1000,Moyenneds1200,Moyenneds1400,Moyenneds2000,Moyenneds2600,Moyenneds3200)

plot(TempsMoy,VectMoyenneds,type="l")

ds600#8#665
par(mfrow=c(2,4))
plot(df600[160:639,]$`Time (s)`,df600[160:639,]$`Rho (Ohm m)`,main="Phase d'oxydation à 600 min")#ok
plot(df800[130:830,]$`Time (s)`,df800[130:830,]$`Rho (Ohm m)`,main="Phase d'oxydation à 800 min")#ok
plot(df1000[95:1010,]$`Time (s)`,df1000[95:1010,]$`Rho (Ohm m)`,main="Phase d'oxydation à 1000 min")
plot(df1200[85:1220,]$`Time (s)`,df1200[85:1220,]$`Rho (Ohm m)`,main="Phase d'oxydation à 1200 min")
plot(df1400[105:1420,]$`Time (s)`,df1400[105:1420,]$`Rho (Ohm m)`,main="Phase d'oxydation à 1400 min")
plot(df2000[65:2000,]$`Time (s)`,df2000[65:2000,]$`Rho (Ohm m)`,main="Phase d'oxydation à 2000 min")
plot(df2600[125:2610,]$`Time (s)`,df2600[125:2610,]$`Rho (Ohm m)`,main="Phase d'oxydation à 2600 min")
plot(df3200[225:3200,]$`Time (s)`,df3200[225:3200,]$`Rho (Ohm m)`,main="Phase d'oxydation à 3200 min")


par(mfrow=c(2,4))
plot(df600[1:159,]$`Time (s)`,df600[1:159,]$`Rho (Ohm m)`,main="Phase d'oxydation à 600 min")#ok
plot(df800[1:130,]$`Time (s)`,df800[1:130,]$`Rho (Ohm m)`,main="Phase d'oxydation à 800 min")#ok
plot(df1000[1:95,]$`Time (s)`,df1000[1:95,]$`Rho (Ohm m)`,main="Phase d'oxydation à 1000 min")
plot(df1200[1:85,]$`Time (s)`,df1200[1:85,]$`Rho (Ohm m)`,main="Phase d'oxydation à 1200 min")
plot(df1400[1:105,]$`Time (s)`,df1400[1:105,]$`Rho (Ohm m)`,main="Phase d'oxydation à 1400 min")
plot(df2000[1:65,]$`Time (s)`,df2000[1:65,]$`Rho (Ohm m)`,main="Phase d'oxydation à 2000 min")
plot(df2600[1:125,]$`Time (s)`,df2600[1:125,]$`Rho (Ohm m)`,main="Phase d'oxydation à 2600 min")
plot(df3200[1:225,]$`Time (s)`,df3200[1:225,]$`Rho (Ohm m)`,main="Phase d'oxydation à 3200 min")


df601<-df600[160:639,]
df801<-df800[130:830,]
df1001<-df1000[95:1010,]
df1201<-df1200[85:1220,]
df1401<-df1400[105:1420,]
df2001<-df2000[65:2000,]
df2601<-df2600[125:2610,]
df3201<-df3200[225:3200,]

vect2<-c(df601[480,2],df801[601,2],df1001[916,2],df1201[1136,2],df1401[1316,2],df2001[1936,2],df2601[2486,2],df3201[2976,2])





modelm601<-lm(df601$`Rho (Ohm m)`~df601$`Time (s)`)
modelm801<-lm(df801$`Rho (Ohm m)`~df801$`Time (s)`)
modelm1001<-lm(df1001$`Rho (Ohm m)`~df1001$`Time (s)`)
modelm1201<-lm(df1201$`Rho (Ohm m)`~df1201$`Time (s)`)
modelm1401<-lm(df1401$`Rho (Ohm m)`~df1401$`Time (s)`)
modelm2001<-lm(df2001$`Rho (Ohm m)`~df2001$`Time (s)`)
modelm2601<-lm(df2601$`Rho (Ohm m)`~df2601$`Time (s)`)
modelm3201<-lm(df3201$`Rho (Ohm m)`~df3201$`Time (s)`)
summary(modelm601)
modelm601$coefficients[2]
vectcorffdir<-rep(0,8)
for (i in 1:8){
  
  
  
}
TempsMoy
vectcoeffdir<-c(modelm601$coefficients[2],modelm801$coefficients[2],modelm1001$coefficients[2],modelm1201$coefficients[2],modelm1401$coefficients[2],modelm2001$coefficients[2],modelm2601$coefficients[2],modelm3201$coefficients[2])
vectTemps<-c(600,800,1000,1200,1400,2000,2600,3200)
plot(ylab="Coefficient directeur",xlab="durée oxydation",vectTemps,vectcoeffdir,main="Courbe des coefficients directeurs de la phase d'oxydation")



modelm4<-lm(vectcoeffdir~poly(TempsMoy,2))
modelm4<-lm(df1$vectcoeffdir~poly(df1$TempsMoy,2))
summary(modelm4)

plot(modelm4$fitted, modelm4$residuals, xlab="Valeurs ajustées", ylab="Résids",main="j")
plot(modelm4$fitted.values,type="l")
plot(modelm4)


plot(TempsMoy,vectcoeffdir,main="Approximation du coefficient directeur  en fonction du temps d'oxydation")
lines(TempsMoy,predict(modelm4),type="l",lty=2, lwd=2)
lines(wt.seq,predict11,type="l",col="red")
modelm4$coefficients
wt.seq<-seq(min(df1$TempsMoy),max(df1$TempsMoy),by=1)
predict1<-predict(modelm4,data.frame=(df1$TempsMoy=wt.seq))
plot(vectTemps,vect2)

df1<-data.frame(TempsMoy,vectcoeffdir)



vect2
modelm5<-lm(vect2~poly(TempsMoy,2))
summary(modelm5)
plot(TempsMoy,vect2,main="Courbe Résistivité moyenne en fonction du temps")
lines(TempsMoy,prdim5,type="l",col="red",lwd=2)
prdim5<-predict(modelm5)
plot(modelm5$fitted, modelm5$residuals, xlab="Valeurs ajustées", ylab="Résidus")
plot(modelm5$fitted.values,type="l")

