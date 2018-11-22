#(a) Napraviti funkciju generator table(dimenzija, broj mina) sa ulaznim argumentima dimenzije table (ne
#mora da bude kvadratna) i brojem mina. Funkcija treba da vrati matricu - jednu gotovu i potpuno
#popunjenu tablu Minolovca

generator_table <- function(dimenzija, broj_mina){
  
  v <- dimenzija[1]
  k <- dimenzija[2]
  vektorizacija <- rep(0, v*k) #vektor
  
  #Mine rasporedimo na slucajno odabrana mesta
  vektorizacija[sample(c(1:(v*k)), broj_mina)] <- -100
  
  
  tabla <- matrix(vektorizacija,nrow=v)
  
  #Da bismo dobro definisali okolinu svakog polja, prosirimo tablu
  
  tabla <- cbind(rep(1, v), tabla, rep(1, v))
  tabla <- rbind(rep(1, k+2), tabla, rep(1, k+2))
  
  for (i in 2:(v+1)){
    for (j in 2:(k+1))
    {   
      okolina <- c(tabla[i-1,j-1], tabla[i-1, j], tabla[i-1, j+1], tabla[i, j-1], tabla[i, j+1], 
                   tabla[i+1, j-1], tabla[i+1, j], tabla[i+1, j+1])
      
      if(tabla[i,j]!= -100) #Ukoliko na datom mestu nismo vec rasporedili minu
        
        
        #Prema datom broju mina u okolini, znamo da popunimo jos neka polja table
        
        tabla[i,j] <- sum(okolina== -100)*10
      
      #Preostala polja se mogu bezbedno otvoriti
      
      if (tabla[i,j]==0)
        tabla[i,j]=100
      
    }
  }
  #skidamo okolinu
  tabla <- tabla[-c(1,v+2), -c(1,k+2)]
  return(tabla)
}
#(b) Napraviti funkciju sakrivanje_polja(matrica, broj polja), koja za proizvo??nu matricu - tablu Minolovca,
#zatvori slucajno odabranih broj polja- polja koje nisu mine i sve mine.

sakrivanje_polja <- function(matrica, broj_polja){
  
  matrica[matrica== -100] <- 0 #Zatvaranje polja u kom su mine
  
  #Zatvaranje jos nekih slucajno odabranih polja
  matrica[sample(which(matrica!=0), broj_polja)] <- 0 
  
  return(matrica)
}

sakrivanje_polja_bez_mina <- function(matrica, broj_polja){
  
  #Zatvaranje slucajno odabranih polja
  
  matrica[sample(which(matrica!=0), broj_polja)] <- 0 
  
  return(matrica)
}
#(v) Oceniti koliko je u proseku potrebno da bude otvorenih po??a (u zavisnosti od dimenzije) da bi se
#tabla Minolovca jedinstveno resila

ocena<-function(dimenzija,broj_mina,C){
  
  tabla <- generator_table(dimenzija, broj_mina)
  b<-0     #broj polja koja je dovoljno otvoriti da bi se jedinstveno resilo
  N<-0
  while(C>0)    #uvodimo vise simulacija da bismo dobili bas prosek na kraju
  {       
    for(i in 1:81){
      sakrivena<-sakrivanje_polja_bez_mina(tabla,i)#sakriva i polja
      resena<-as.vector(resi_tablu(sakrivena,9,broj_mina))
      if(all(resena!=0))
      {              #ako uspeva da ga resi potpuno
        b<-b+(81-i)  #broji koliko je polja potrebno da se otvori(81-sakrivena polja=i)
        

      N<-N+1  #broji koliko takvih "resenih" slucajeva smo nasli
      }
      
   
    } 
    C<-C-1
}
  return(b/N)#prosecan broj otvorenih polja je kada ukupan broj otv. polja dovoljnih za sve matrice podelimo sa ukupnim brojem matrica kod kojih to vazi
  
}


