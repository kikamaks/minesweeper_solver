#Koristimo oznake sa casa:
# zatvoreno polje : 0
# Broj 1          : 10
# Broj 2          : 20
# Broj 3          : 30
# Broj 4          : 40
# Broj 5          : 50
# Broj 6          : 60
# Broj 7          : 70
# Broj 8          : 80
# Mina            : -100
# Prazno polje    : 100

#Potrebni paketi:
library(imager)
library(MASS)
library(nnet)

#Ucitavamo potrebne slike:

im1 <- load.image("C:/Users/Korisnik/Desktop/Kristina_Maksimovic_230_12_ss4/treci_zadatak/treci_zadatak_obucavanje/MineSweeper3.png")
im2 <- load.image("C:/Users/Korisnik/Desktop/Kristina_Maksimovic_230_12_ss4/treci_zadatak/treci_zadatak_obucavanje/MineSweeper5.png")
im3 <- load.image("C:/Users/Korisnik/Desktop/Kristina_Maksimovic_230_12_ss4/treci_zadatak/treci_zadatak_obucavanje/Minolovac1.png")
im4 <- load.image("C:/Users/Korisnik/Desktop/Kristina_Maksimovic_230_12_ss4/treci_zadatak/treci_zadatak_obucavanje/Minolovac4.png")
im5 <- load.image("C:/Users/Korisnik/Desktop/Kristina_Maksimovic_230_12_ss4/treci_zadatak/treci_zadatak_obucavanje/Minolovac14.png")
im6 <- load.image("C:/Users/Korisnik/Desktop/Kristina_Maksimovic_230_12_ss4/treci_zadatak/treci_zadatak_obucavanje/Minolovac16.png")
im7 <- load.image("C:/Users/Korisnik/Desktop/Kristina_Maksimovic_230_12_ss4/treci_zadatak/treci_zadatak_obucavanje/Minolovac18.png")
im8 <- load.image("C:/Users/Korisnik/Desktop/Kristina_Maksimovic_230_12_ss4/treci_zadatak/treci_zadatak_obucavanje/Minolovac20.png")
im9 <- load.image("C:/Users/Korisnik/Desktop/Kristina_Maksimovic_230_12_ss4/treci_zadatak/treci_zadatak_obucavanje/Minolovac22.png")
im10 <- load.image("C:/Users/Korisnik/Desktop/Kristina_Maksimovic_230_12_ss4/treci_zadatak/treci_zadatak_obucavanje/Minolovac24.png")
im11 <- load.image("C:/Users/Korisnik/Desktop/Kristina_Maksimovic_230_12_ss4/treci_zadatak/treci_zadatak_obucavanje/Minolovac26.png")

test1 <- load.image("C:/Users/Korisnik/Desktop/Kristina_Maksimovic_230_12_ss4/treci_zadatak/treci_zadatak_kontrolni/Minolovac10.png")
test2 <- load.image("C:/Users/Korisnik/Desktop/Kristina_Maksimovic_230_12_ss4/treci_zadatak/treci_zadatak_kontrolni/Minolovac12.png")

#Rucno kodiramo slike koje smo odabrali 

Y1 <- c(0, 0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 20, 10, 10, -100, 10, 0,
        0, 0, 30, -100, 10, 10, 10, 10, 0,
        20, 30, -100, 20, 10, 100, 10, 10, 0,
        -100, 20, 10, 10, 100, 100, 10, -100, 0,
        10, 10, 100, 100, 10, 10, 20, 10, 0,
        100, 10, 10, 10, 10, -100, 20, 10, 0,
        100, 10, -100, 0, 0, 0, 0, 0, 0,
        100, 10, 0, 0, 0, 0, 0, 0, 0)

Y2 <- c(0, 0, 0, 10, 100, 10, 10, 10, 100,
        0, 20, 0, 20, 10, 10, -100, 10, 100,
        0, 0, 30, -100, 10, 10, 10, 10, 100,
        20, 30, -100, 20, 10, 100, 10, 10, 10, 
        -100, 20, 10, 10, 100, 100, 10, -100, 10,
        10, 10, 100, 100, 10, 10, 20, 10, 10,
        100, 10, 10, 10, 10, -100, 20, 10, 100,
        100, 10, -100, 10, 10, 20, 0, 10, 100,
        100, 10, 10, 10, 100, 10, 0, 10, 100)

Y3 <- c(100, 100, 100, 100, 100, 100, 10, 0, 0,
        10, 10, 100, 100, 100, 10, 20, 0, 0,
        0, 10, 100, 100, 100, 20, 0, 0, 0, 
        0, 10, 100, 100, 100, 20, 0, 30, 10,
        0, 10, 10, 10, 100, 10, 10, 10, 100,
        0, 0, 0, 10, 100, 100, 100, 100, 100, 
        0, 0, 0, 10, 100, 10, 10, 10, 100,
        0, 0, 0, 10, 10, 10, 0, 20, 10,
        0, 0, 0, 0, 0, 0, 0, 0,0)

Y4 <- c(100, 100, 100, 100, 100, 100, 10, 10, 10,
        10, 10, 100, 100, 100, 10, 20, -100, 20,
        -100, 10, 100, 100, 100, 20, -100, 40, -100,
        10, 10, 100, 100, 100, 20, -100, 30, 10,
        100, 10, 10, 10, 100, 10, 10, 10, 100,
        100, 10, -100, 10, 100, 100, 100, 100, 100, 
        100, 10, 10, 10, 100, 10, 10, 10, 100,
        10, 10, 20, 10, 10, 10, -100, 20, 10,
        10, -100, 20, -100, 10, 10, 10, 20, -100)

Y5 <-c(rep(100, 9),
       rep(100, 5), 10, 10, 10, 100,
       rep(100, 5), 20, 0, 30, 10,
       10, 10, 100, 100, 100, 20, 0, 0, 0,
       0, 10, 100, 10, 20, 30, 30, 30, 0,
       10, 10, 100, 10, 0, 0, 10, 10, 0,
       100, 10, 10, 20, 20, 20, 10, 10, 10,
       10, 20, 0, 10, 100, 100, 100, 10, 0,
       0, 20, 10, 10, 100, 100, 100, 10, 0)

Y10 <- c(rep(0, 45),
         0, 0, 0, 0, 40, 30, 30, 0, 0,
         0, 0, 0, 0, 10, 100, 20, 0 , 0,
         rep(0, 4), 40, 30, 40, 0, 0,
         rep(0, 9))

Y11<-c(rep(100, 4), 10, 0, 10, 100, 100,
       rep(100, 4), 10, 10, 20, 10, 10,
       rep(100, 6), 10, 0, 10,
       10, 10, 10, rep(100, 3), 10, 10, 10,
       20, 0, 10, 100, 100, 100, 10, 10, 10,
       0, 40, 30, 10, 100, 100, 10, 0,0,
       20, 0, 0, 30, 30, 20, 20, 10, 10, 
       10, 20, 30, 0, 0, 0, 10, 100, 100,
       100, 100, 10, 20, 30, 20, 10, 0, 0)

#U nastavku su funkcije za odredjivanje granica i njihovo sredjivanje

#Funkcije koje vracaju tamne vrste i kolone piksela:

granice <- function(matrica, epsilon, prob, by.row=T){
  if(by.row){
    Mean <- rowMeans
  } else {
    Mean <- colMeans
  }
  
  vec <- Mean(matrica < epsilon) >= prob
  return(which(vec!=F))
}

#funkcijom 'sredjivanje' dobijamo pocetak i kraj crnih granica

sredjivanje<-function(vektor)
{
  novi_vektor=c()
  for(i in 1:(length(vektor)-1))
  {
    
    if(vektor[i+1]-vektor[i]>5)
    {
      novi_vektor=c(novi_vektor, vektor[i])
    }
  }
  for(j in length(vektor):2)
  {
    if(vektor[j]-vektor[j-1]>5)
    {
      novi_vektor=c(novi_vektor, vektor[j])
    }
  }
  return(sort(novi_vektor))
}

#u funkciji 'prediktori' smesticemo tri prediktora

prediktori<-function(im){
  
  #Sliku prevodimo u matricni oblik:
  q=im[,,1,1]+im[,,1,2]+im[,,1,3] 
  q1=im[,,1,1]  #matrica intenzteta crvene boje po pikselima u slici
  q2=im[,,1,2]  #matrica inteziteta zelene boje
  q3=im[,,1,3]  #matricaintenziteta plave boje 
  
  M=pmax(q1,q2,q3) #izdvajamo najvece vrednosti od q1,q2 i q3
  
  #Nalazimo tamne vrste i kolone, koje potom sredjujemo:
  v1=granice(M, 0.2, 0.3)
  k1=granice(M, 0.2, 0.3, by.row = FALSE)
  
  v=sredjivanje(v1)
  k=sredjivanje(k1)
  
  #Prediktori:
  
  X1=c()  #prvi prediktor bice srednji udeo plave boje u polju
  X2=c()  #drugi: disperzija polja u crno-belom spektru
  X3=c()  #treci: srednji udeo crvene boje u polju

  
  l=1
  for(j in 1:9)
  {
    for(i in 1:9)
    {
      s=array(im[v[2*j-1]:v[2*j], k[2*i-1]:k[2*i], 1, 1:3], c(v[2*j]-v[2*j-1]+1,k[2*i]-k[2*i-1]+1, 1, 3))
      polje=cimg(s)
      red=as.vector(polje[ , , 1, 1])
      green=as.vector(polje[ , , 1, 2])
      blue=as.vector(polje[ , , 1, 3])
      
      X1[l]= mean(blue/(red+green+blue), na.rm = TRUE)
      X2[l]= var(as.vector(grayscale(cimg(s))[,,,1]))
      X3[l]= mean(red/(blue+green+red), na.rm=TRUE)

      
      l=l+1
    }
  }
  
  # Povratna vrednost funkcije bice vrednosti prediktra poslagane u matricu po kolonama
  
  return(cbind(X1,X2,X3))
}
Y <- c(Y1, Y2, Y3, Y4, Y5, Y10, Y11) #pravimo vektor od odabranih slika

ukupni=rbind(prediktori(im1), prediktori(im2), prediktori(im3),prediktori(im4), prediktori(im5),
             prediktori(im10), prediktori(im11))

#slazemo prediktore svih slika u matricu po vrstama

X1=ukupni[,1] #svi prvi prediktori
X2=ukupni[,2]
X3=ukupni[,3]

#Modelovanje velicine Y prediktorima X1,X2,X3 

#Multinomni logisticki model

model.multinom=multinom(Y~X1+X2+X3)
summary(model.multinom)  #Prikaz koeficijenata u modelu uz prediktore 

mulitnom.pred=predict(model.multinom)
#Tabeliramo vrednosti predvidjene modelom i stvarne vrednosti velicine Y:
table(mulitnom.pred,Y) 
#Vidimo da je model jako dobar: skoro sve vrednosti su se poklopile,uz jedno mesanje otvorenog polja i broja 1 
# i dva mesanja otvorenog i zatvorenog polja

#QDA model
model.qda=qda(Y~X1+X2+X3)  
summary(model.qda)

qda.pred=predict(model.qda)
table(qda.pred$class, Y)    #Dobar model, ponovo uz dva mesanja zatvorenog i praznog polja

#LDA model
model.lda=lda(Y~X1+X2+X3)
summary(model.lda)

lda.pred=predict(model.lda)
table(model.ldapred$class, Y) 
#Najlosiji od tri modela, 11 mesanja otvorenog polja sa brojem 1, 2 mesanja broja 1 sa otvorenim poljem i 2 mesanja otvorenog sa zatvorenim poljem

#Model na test skupu

Test1 <- c(rep(10,2), 20, -100, 10, rep(100,4), 20, -100, 30, rep(10,2),
           rep(100,4),20, -100, 20, rep(100,2), rep(10,3),100,0,20,10,
           rep(100,2), 10, -100, 20, 10, -100, 10, 100, rep(10,2), rep(20,2),
           rep(0,2),rep(10,2), 100, 10, -100, rep(10,2), rep(0,2), rep(100,3),
           rep(10,4),20, 0, rep(100,4), rep(10,2), 20, rep(0,2),rep(100,4),
           10, 0, 20, rep(0,2))

Test2 <- c(rep(0,2), 10, rep(0,2), -100, 10, rep(100,2),rep(10,4), rep(20,2),
           rep(10,3), rep(100,7), 10, 0, rep(100,7), rep(10,2),rep(100,5),
           rep(10,2),20,10,rep(100,5),20, -100, 30, -100, rep(100,3),rep(10,2),40,
           -100, 40, 10, rep(100,3),20, -100, 40, -100, 20, rep(100,4), 20, -100, 
           30, rep(10, 2), 100)


#Prediktori kontrolnih slika:
kont1=prediktori(test1)
kont2=prediktori(test2)

#Predvidjene vrednosti Multinomnim logistickim modelom:
predMult1=predict(model.multinom,newdata=data.frame(kont1))
predMult2=predict(model.multinom,newdata=data.frame(kont2))

#Tabeliramo vrednosti:
table(predMult1,Test1)
table(predMult2,Test2)
#Vidimo da je model dobar,  nijedno odstupanje. 




