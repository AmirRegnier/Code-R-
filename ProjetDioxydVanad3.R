df<-Data_Figure5
cycle15<-df[,1:2]
cycle610<-df[,3:4]
cycle11<-df[,5:6]


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

plot(cycle$`T(°C)`,main="Profil de Température du cycle",xlab="Temps",ylab="Température")

plot(cycle15$`Rho (Ohm.m)`)

#Fusion des 3 data frame 
cycle15bis<-cycle15
cycle15bis<-rbind(cycle15bis,cycle610)
cycle<-rbind(cycle15bis,cycle11)
plot(cycle$`T(°C)`,xlab="Temps (en minutes)",ylab="Température (en °C)",main="Profil de température du second profil de température")


plot(cycle$`Rho (Ohm.m)`)
points(cycle$`T(°C)`,col="red")

floor((vectCreuxTemps[3]+vectCreuxTemps[3+1])/2)

vectPicTemp<-c(8,30,70,126,206,292,409,546,687,861,1057)
vectCreuxTemps<-c(0,1,14,45,92,161,239,343,471,602,766,951,1164)

MoyenneCourbe11<-rep(0,11)
SdCourbe11<-rep(0,11)
par(mfrow=c(3,4))



plot(ylab="Résistivité (en Ω.m)",xlab="Temps (en min)",cycle$`Rho (Ohm.m)`[vectCreuxTemps[2]:vectCreuxTemps[3]],main="Courbe pour chaque pic")
abline(v=floor((vectCreuxTemps[2]+vectCreuxTemps[3])/2)-vectCreuxTemps[1],col="red")
for (i in 2:11){
  plot(ylab="Résistivité (en Ω.m)",xlab="Temps (en min)",cycle$`Rho (Ohm.m)`[vectCreuxTemps[i+1]:vectCreuxTemps[i+2]],main="Courbe pour chaque pic")
  abline(v=floor((vectCreuxTemps[i+1]+vectCreuxTemps[i+2])/2)-vectCreuxTemps[i+1],col="red")
  MoyenneCourbe11[i+1]<-mean(cycle[vectCreuxTemps[i+1]:vectCreuxTemps[i+1],]$`Rho (Ohm.m)`)
  SdCourbe11[i]<-sd(cycle[vectCreuxTemps[i]:vectCreuxTemps[i+1],]$`Rho (Ohm.m)`)
}
cycle$`Rho (Ohm.m)`<-as.numeric(cycle$`Rho (Ohm.m)`)
cycle[vectCreuxTemps[i]:vectCreuxTemps[i+1],]$`Rho (Ohm.m)`
plot(main="Proifl de température du pic à 100°C",ylab="Température (en °C)",xlab="Temps (en min)",cycle$`T(°C)`[vectCreuxTemps[2]:vectCreuxTemps[2+1]])

VectTemp<-c(50,100,150,200,250,300,350,400,450,500,550)
plot(VectTemp,MoyenneCourbe11,main="Courbe des valurs moyenne de la résistivité pour chaque pic")
plot(VectTemp,SdCourbe11,main="Courbe des valeurs de l'écart-type de la résistivité pour chaque pic")

plot(xlab="Temps (en min)",ylab="Résistivité en  (en Ω.m)",cycle$`Rho (Ohm.m)`,main="Courbe de la résistivitré durzant l'intégralité du cycle")

tauxaccroiss<-function(df){
  ds<-rep(0,dim(df)[1]-1)
  for (i in 1:dim(df)[1]-1){
    
    ds[i]<-(df$`Rho (Ohm.m)`[i+1]-df$`Rho (Ohm.m)`[i])
    
  }
  return(ds)
}

#Taux Accroissement par apport à la température 
cycle[vectCreuxTemps[1]:vectCreuxTemps[2],]$`Rho (Ohm.m)`
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
ds1
par(mfrow=c(3,4))
for (i in 1:11){
  plot(cycle$`T(°C)`[vectCreuxTemps[i]:vectCreuxTemps[i+1]],cycle$`Rho (Ohm.m)`[vectCreuxTemps[i]:vectCreuxTemps[i+1]],xlab="Résistivité",ylab="Température",main="Résistivité en fonction de la température pour chaque pic")
  #MoyenneCourbe11[i]<-mean(cycle[vectCreuxTemps[i]:vectCreuxTemps[i+1],]$`Rho (Ohm.m)`)
}
ds<-c(ds1,ds2,ds3,ds4,ds5,ds6,ds7,ds8,ds9,ds10,ds11)
ds<-data.frame(nrow=)
par(mfrow=c(3,4))
plot(ds1,main="Taux d'accroissement pour chaque pic")
plot(ds2,main="Taux d'accroissement pour chaque pic")
plot(ds3,main="Taux d'accroissement pour chaque pic")
plot(ds4,main="Taux d'accroissement pour chaque pic")
plot(ds5,main="Taux d'accroissement pour chaque pic")
plot(ds6,main="Taux d'accroissement pour chaque pic")
plot(ds7,main="Taux d'accroissement pour chaque pic")
plot(ds8,main="Taux d'accroissement pour chaque pic")
plot(ds9,main="Taux d'accroissement pour chaque pic")
plot(ds10,main="Taux d'accroissement pour chaque pic")
plot(ds11,main="Taux d'accroissement pour chaque pic")
