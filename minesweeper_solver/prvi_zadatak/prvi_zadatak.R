#1. Za slike koje se nalaze u folderu prvi zadatak potrebno je:


# (a) Odrediti ugao rotacije geometrijske figure u odnosu na x-osu.

library(imager)

#Ucitavamo slike:

im1=load.image("C:/Users/Korisnik/Desktop/Kristina_Maksimovic_230_12_ss4/prvi_zadatak/prvi_zadatak3.png")
im2=load.image("C:/Users/Korisnik/Desktop/Kristina_Maksimovic_230_12_ss4/prvi_zadatak/prvi_zadatak4.png")
im3=load.image("C:/Users/Korisnik/Desktop/Kristina_Maksimovic_230_12_ss4/prvi_zadatak/prvi_zadatak5.png")
im4=load.image("C:/Users/Korisnik/Desktop/Kristina_Maksimovic_230_12_ss4/prvi_zadatak/slika_cetiri.png")

rotiranje<-function(im)
{
  
  m=im[,,1,1]+im[,,1,2]+im[,,1,3] #pravimo matricu od unete slike
  
  vektor=as.vector(m) #vektor od matrice
  mdif=diff(vektor)  #vektor razlike susednih elemenata(kolicine boja)
  #Prvi prelaz iz jedne boje u drugu boju je na ivicama kvadrata na ostalim mestima je nula
  
  M=matrix(mdif, nrow=dim(m)[1]) #Od toga ponovo pravimo matricu
  d1=dim(m)[1]  #koordinata x ose(broj kolona)
  d2=dim(m)[2]   #koordinata y ose(broj vrsta)
  
 #Trazimo dva temena kvadrata x i y
  
  y<-c(0,0)
  x<-c(d1, d1)
  for(i in 1:d1)
    for(j in 1:d2)
      
      if(M[i,j]){   #ako smo dosli do figure na slici
        if(i>y[1]) {
          y<-c(i,j)}
        
        if(j<x[2]){
          x<-c(i,j)
          
        } 
      }
  
  
  #Ugao rotacije:
  
  ugao=atan((y[2]-x[2])/(y[1]-x[1]))
  ugao=(180*ugao)/pi  #pretvaramo u stepene
  
  #(b) Izrotirati figuru, t.d. donja stranica bude paralelna sa x-osom.
  
  rotirana_slika=imrotate(im, -ugao)
  
  plot(rotirana_slika)
  
  #(v) Sacuvati novodobijenu sliku.
 
  save.image(rotirana_slika,"C:/Users/Korisnik/Desktop/Kristina_Maksimovic_230_12_ss4/prvi_zadatak/rotirana.png") 
  
  return(ugao)
  
}
  
  
  
  
  
  
