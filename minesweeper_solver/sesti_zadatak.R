
library(ISLR)
library(MASS)  # sadrzi funkcije za LDA i QDA
library(class) # sadrzi funkcije za KNN - K najblizih suseda.

#(a) Napraviti logisticki model za promenljivu Direction i prediktore Lag1, Lag2, Lag3, Lag4, Lag5, i
#Volume. Da li su neki od tih prediktora znacajni Proveriti koliko je dobar taj model primenjen na
#skupu za obucavanje. (Napomena: skup za obucavanje je bio ceo skup.)


attach(Weekly)


# funkcija pomocu koje koristimo logisticku regresiju
#family = binomial zelimo da R pokrene bas logisticku regresiju

model1=glm(Direction ~ Lag1 + Lag2 + Lag3 + Lag4 + Lag5 + Volume, data = Weekly, family = binomial)
summary(model1)

#Zakljucujemo da je jedino prediktor Lag2 znacajan cija p-vrednost ne prelazi prag znacajnosti 0.05

#Predvidjanje se vrsi nad istim skupom vrednosti nad kojima je sagradjen model

glm.probs=predict(model1,type="response") 

# Za one koji imaju verovatnocu >0.5 dodeljujemo Up, za preostale Down

glm.pred=ifelse(glm.probs>0.5,"Up","Down")
# Proveravamo koliko dobro se uklopio model
table(glm.pred,Direction)
mean(glm.pred==Direction)
# Vidimo da model nije preterano dobar

#(b) Izdvojiti skup od 1990. do 2008. godine kao skup za obucavanje i sprovesti model ponovo. Pronaci
#najbo??i moguci logisticki model za ovaj slucaj (u smislu da najmanje greske). Koje prediktore sadrzi
#taj model? 

#Iz dela a) znamo da je jedino prediktor Lag2 znacajan pa na osnovu toga pravimo model

#izdvajamo skup za obucavanje
obucavanje=Year<2009
model2=glm(Direction~Lag2, data=Weekly,family=binomial, subset=obucavanje)
summary(glm.fit)
#Pokrecemo predvidjanje za podskup koji nismo koristili i proveravamo koliko je dobra ocena
glm.probs=predict(model2,newdata=Weekly[!obucavanje,],type="response") 

glm.pred=ifelse(glm.probs >0.5,"Up","Down")
Direction.2008=Weekly$Direction[!obucavanje]
table(glm.pred,Direction.2008)
mean(glm.pred==Direction.2008)
#Ni ovo nije previse dobro

#(v) Ponoviti deo pod (b) samo za LDA, QDA i KNN za K = 1 modele.

#LDA model

lda_model = lda(Direction ~ Lag2, data = Weekly, subset =obucavanje)
lda_model # jer summary(lda_model)ne vraca ono sto ocekujemo
lda.pred = predict(lda_model, Weekly[!obucavanje, ])
table(lda.pred$class, Weekly[!obucavanje,]$Direction)
mean(lda.pred$class==Weekly[!obucavanje,]$Direction)
#Ima oko 63% dobrih procena 

#QDA model

qda_model = qda(Direction ~ Lag2, data = Weekly, subset = obucavanje)
qda.pred = predict(qda_model, Weekly[!obucavanje, ])
table(qda.pred$class, Weekly[!obucavanje,]$Direction)
mean(qda.pred$class==Weekly[!obucavanje,]$Direction)
#Ovde je losija procena (59%)

# KNN model za K=1

# Matrica koja sadrzi skup za poducavanje u prediktoru Lag2
obucavanje.X = as.matrix(Lag2[obucavanje])
# Matrica koja sadrzi kontrolni skup u Lag2
test.X = as.matrix(Lag2[!obucavanje])
# Kategorije koje postoje u skupu za obucavanje
obucavanje.Direction = Direction[obucavanje]
#Gledamo samo za k=1 
set.seed(1)
knn.pred = knn(obucavanje.X, test.X, obucavanje.Direction, k = 1)
table(knn.pred,  Weekly[!obucavanje,]$Direction)
mean(knn.pred == Weekly[!obucavanje,]$Direction)
#Ova metoda je slabija od prethodne dve

#Zakljucak je da najbolju ocenu dao model LDA

