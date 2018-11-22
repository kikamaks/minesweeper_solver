#4. Napisati sledece funkcije:

#(a) resi_tablu(matrica, ...) - za prosledjenu matricu tabele Minolovca funkcija treba da za sva po??a, za koje
#je moguce sigurno odluciti da li je mina ili ne, da tacan odgovor.


resi_tablu<-function(matrica, dimenzija, broj_mina)
{
  
  m=matrica
  # dodamo po jos jedan red i kolonu sa jedinicama
  m=cbind(rep(1, dimenzija), m, rep(1, dimenzija))
  m=rbind(rep(1, dimenzija+2), m, rep(1, dimenzija+2))
  
  #prolazimo sve moguce vrednosti MineSweeper polja
  for(i in 2:(dimenzija+1))
  {
    for(j in 2:(dimenzija+1))
    {
      for(k in 1:8) # broj mina u okolini
      {
        # Prvo slucaj da je neki broj u tom polju [i,j].
        if(m[i, j]==k*10)
        {
          # Celu okolinu stavimo u jedan vektor
          vektor=c(m[i-1,j-1], m[i-1, j], m[i-1, j+1], m[i, j-1], m[i, j+1], m[i+1, j-1], m[i+1, j], m[i+1, j+1])
          # Ako je imamo vec obelezno k mina, onda sve preostale neotvorene mozemo otvoriti
          
          if(sum(vektor==-100)==k)
          {
            #w=which(vektor<9 & vektor!=-100)
            #vektor[w]=vektor[w]+1
            
            # ako polje moze da se otvori i tu sigurno nije mina povecavamo za jedan
            if(m[i-1, j-1]<9 & m[i-1, j-1]!=-100)
              m[i-1, j-1]=m[i-1, j-1]+1
            if(m[i-1, j]<9 & m[i-1,j]!=-100)
              m[i-1, j]=m[i-1, j]+1
            if(m[i-1, j+1]<9 & m[i-1,j+1]!=-100)
              m[i-1, j+1]=m[i-1, j+1]+1
            if(m[i, j-1]<9 & m[i,j-1]!=-100)
              m[i, j-1]=m[i, j-1]+1
            if(m[i, j+1]<9 & m[i,j+1]!=-100)
              m[i, j+1]=m[i, j+1]+1
            if(m[i+1, j-1]<9 & m[i+1,j-1]!=-100)
              m[i+1, j-1]=m[i+1, j-1]+1
            if(m[i+1, j]<9 & m[i+1,j]!=-100)
              m[i+1, j]=m[i+1, j]+1
            if(m[i+1, j+1]<9  &m[i+1,j+1]!=-100)
              m[i+1, j+1]=m[i+1, j+1]+1
          }
          
          
        }
        
      }
      # Ako je polje u koordinatama [i,j] bas prazno, onda sve neotvorene obelezavamo kao bezbedne,
      # tj povecacavamo broj za jedan.
      if(m[i,j]==100)
      {
        if(m[i-1, j-1]==0)
          m[i-1, j-1]=m[i-1, j-1]+1
        if(m[i-1, j]==0)
          m[i-1, j]=m[i-1, j]+1
        if(m[i-1, j+1]==0)
          m[i-1, j+1]=m[i-1,j+1]+1
        if(m[i, j-1]==0)
          m[i, j-1]=m[i,j-1]+1
        if(m[i, j+1]==0)
          m[i, j+1]=m[i,j+1]+1
        if(m[i+1, j-1]==0)
          m[i+1, j-1]=m[i+1,j-1]+1
        if(m[i+1, j]==0)
          m[i+1, j]=m[i+1,j]+1
        if(m[i+1, j+1]==0)
          m[i+1, j+1]=m[i+1,j+1]+1
      }
    }
  }
  
  # Ponovimo postupak:
  for(i in 2:(dimenzija+1))
  {
    for(j in 2:(dimenzija+1))
    {
      for(k in 1:8)
      {
        # ako je broj k u polju [i,j].
        if(m[i,j]==k*10)
        {
          # Opet pravimo vektor okoline.
          vektor=c(m[i-1,j-1], m[i-1, j], m[i-1, j+1], m[i, j-1], m[i, j+1], m[i+1, j-1], m[i+1, j], m[i+1, j+1])
          # Ako je broj polja obelezenih kao mina zajedno sa  brojem neotvorenih bas k, onda znamo gde su mine.
          if(sum(vektor==-100)+sum(vektor==0)==k)
          {
            if(m[i-1, j-1]==0)
              m[i-1, j-1]=-100
            if(m[i-1, j]==0)
              m[i-1, j]=-100
            if(m[i-1, j+1]==0)
              m[i-1, j+1]=-100
            if(m[i, j-1]==0)
              m[i, j-1]=-100
            if(m[i, j+1]==0)
              m[i, j+1]=-100
            if(m[i+1, j-1]==0)
              m[i+1, j-1]=-100
            if(m[i+1, j]==0)
              m[i+1, j]=-100
            if(m[i+1, j+1]==0)
              m[i+1, j+1]=-100
          }
        }
      }
    }
  }
  
  # Skinemo dodate nove vrste i kolone.
  m=m[-1, -1]
  m=m[-(dimenzija+1), -(dimenzija+1) ]
  
  # Gledamo da li smo pronasli sve mine, ako jesmo, odmah vracamo matricu m.
  if(sum(m==-100)==broj_mina)
  {   
    return(m)
  }
  
  # Ako pak nismo, ali uz to imamo da je broj neotvorenih + broj obelezih kao mine bas broj_mina, onda
  # opet znamo gde moraju biti mine
  
  if(sum(m==-100)<broj_mina)
  {
    if(sum(m==0)+sum(m==-100)==broj_mina)
    {
      for(i in 1:dimenzija)
      {
        for(j in 1:dimenzija)
        {
          if(m[i,j]==0)
          {
            m[i,j]=-100
          }
        }
      }
    }
  }
  # vratimo matricu m
  return(m)
}

#(v) prava matrica(matrica, dimenzija, broj mina) - za prosledjenu vec popu??enu do kraja (nema nijedne preostale
#neobelezene mine) matricu tabele Minolovca funkcija treba da vrati odgovor da li je ovakva
#matrica jedna moguca tabela Minolovca.

prava_matrica <- function(matrica)
{
  
  m=matrica
  dimenzija <- dim(m)[1] #U delu sa simulacijama cemo raditi samo sa kvadratnim matricama
  #Jer koristimo i funkciju resi_prosto
  
  #opet pravimo okolinu
  m=cbind(rep(1, dimenzija), m, rep(1, dimenzija))
  m=rbind(rep(1, dimenzija+2), m, rep(1, dimenzija+2))
  
  
  #mozemo argument broj_mina 
  
  #Broj u polju mora biti jednak broju obelezenih mina u okolini polja
  for(i in 2:(dimenzija+1))
  {
    for(j in 2:(dimenzija+1))
    {
      okolina <- c(m[i-1,j-1], m[i-1, j], m[i-1, j+1], m[i, j-1], m[i, j+1], m[i+1, j-1], m[i+1, j], m[i+1, j+1])
      
      for(k in 1:8)
      {
        
        if(m[i, j]==k*10) #Ako se na mestu (i,j) nalazi broj
          
          if(10*sum(okolina==-100)!=m[i,j])
            return(FALSE)
        #stop("Nije validna tabla")
      }
    }
  }
  
  m=m[-1, -1]
  m=m[-(dimenzija+1), -(dimenzija+1)]
  return(TRUE) #Ukoliko je sve proslo kako treba, polazna matrica
  #jeste jedna validna tabla Minolovca
  #im4 true
  
}

#(v) prava matrica(matrica, dimenzija, broj mina) - za prosledjenu vec popu??enu do kraja (nema nijedne preostale
#neobelezene mine) matricu tabele Minolovca funkcija treba da vrati odgovor da li je ovakva
#matrica jedna moguca tabela Minolovca.

MK_simulacija <-function(matrica, broj_mina, N){
  
  #Funkcijom resi_tablu smo dobili na kojim poljima su sigurno mine
  m <- matrica
  dimenzija <-dim(m)[1]
  A <- resi_tablu(m,9,broj_mina)
  a<- as.vector(A)
  
  #Racunamo koliko mina je ostalo nerasporedjeno
  preostalo <- broj_mina - sum(a==-100) #broj preostalih mina
  stanje <- rep(0, dimenzija^2)   #vektor nula
  NP <- 0
  
  #Uzimamo prost slucajan uzorak dozvoljenih pozicija na koje
  #postavljamo preostale mine
  
  for(i in 1:N) { #Vrsimo odredjen broj simulacija
     moguca_mesta<-sample(which(a==0), preostalo) #pozicije 
    trenutni <- a
    trenutni[moguca_mesta] <- -100 
    
    if(prava_matrica(matrix(trenutni,nrow=dimenzija))){ 
      
      stanje[moguca_mesta] <- stanje[moguca_mesta]+1 
      NP <- NP + 1 #broj validnih matrica
    }
  }
  
  S <- matrix(stanje, nrow = dimenzija)
  #Odredimo kolike su sanse da se u polju nalazi mina
  SM <- S/NP            #verovatnoca na kom se mestu nalazi mina 
  minmax <- c(which.min(SM), which.max(SM))
  return(SM)
}
