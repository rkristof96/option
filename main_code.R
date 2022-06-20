##########################
#### Opcióárazás HF ######
##########################

##############
###0. lépés###
##############
#Ha nem olvasható jól a szöveg(elcsúsztak az ékezetek)->File->Reopen with encoding->UTF8
# be kell állítani a working directoryt annak a mappának, ahol a fájlok vannak
# nekem itt volt
# ha aza alpértelmezett mappában vannak a fájlok, akkor nem kell
# ez a getwd() függvénnyel ellenőrizhető
#setwd("H:/1házifeladat")
# betöltjük a packageket
source("load_packages.R")
load_packages()

# a warning message még nem jelenti azt, hogy baj van, de lehet
# a load_packages függvény telepíti azokat a packageket, amik kellenek, ez időbe telik
# ha nem töltődik be ezeket kell lefuttatni
# CTRL + Shift + C-vel ki lehet kommentelni

# install.packages("lpSolve", "lpSolveAPI","latex2exp","ggplot2")
# library(lpSolve)
# library(lpSolveAPI)
# library(latex2exp)
# library(ggplot2)
# source("read_input.R") # inputok beolvasása
# source("share_price.R") # részvényárazás
# source("lp_solve.R") # LP megoldó
# source("first_day_pricing.R") # első napi opcióárazó
# source("second_day_pricing.R") # második napi opcióárazó
# source("third_day_pricing.R") # harmadik napi opcióárazó
# source("parameter_sensitivity.R") # ábrákat készít a várható érték és a szórás függvényében
# source("nullTothree.R") # ha nem lehet átrendezni az 1. és 2. időszakban a portfóliót, akkor megmondja az opció árát
# source("nosecondreorder.R") # ha csak az első időszakban lehet átrendezni a portfóliót, de a másodikban nem, akkor megmondja az opció árát
# source("deterministic_share_price.R") # determinisztikus eszközárazás, hogy lefusson 3 időszakra

##################
###Alapgondolat###
##################
# A kód alapötlete az, hogy időszakonként árazom be az opciót, és a korábbi időszakok önfinanszírozási korlátjait felhasználom
# Egyfajta rekurziót csinálok és a továbbvitt mátrixokat kiegészítem nullákkal

# ha esetleg valami nem fut le,a mire azt mondtam hogy igen, akkor újra kell nyitni a kódot lefuttatni csak a load_packages() függvényt és a futtani kívánt részt

# rögzítsük a véletlenszámgenerátor kezdőértékét
#set.seed(12345)

## beolvassuk az inputváltozókat
# mu - lognormális eloszlás várható érték
# sigma - lognormális eloszlás szórás
#n1 - első időszakban hány felé ágazik az árfolyam
#n2 - második időszakban hány felé ágazik az árfolyam
#n3 - harmadik időszakban hány felé ágazik az árfolyam

###############
###Feladatok###
###############
# rögzítsük a véletlenszámgenerátor kezdőértékét
set.seed(12345)
#___________#
#Egy időszak#
#___________#

#100 fele ágaztatunk
i100 <- read_input(2,1,100,3,3)
step100 <- first_day_pricing(i100,FALSE)
step100$first_day_price$objective # derivatíva ára
# 10.97991
#kockázatsemlege valószínűségekkel
sum(step100$payoffs*step100$first_day_dual)
# 10.97991
#Lehet-e olcsóbban adni?
# Ha olcsóbban adom az opciót, akkor lehet előállítani olysn replikáló portfóliót, amivel arbitrálni lehet.
# Szóval nem érdemes olcsóbban kiírni az opciót.
# A King cikk alapján a writer's problem-et programoztam le, ahol az opció ára Fw
# jelölje Fb a buyer's problem esetén az opció árát
# Ekkor Fb <= Fw
# Az Fb a vásárlónak elfogadható maximális ár
# AZ Fw az opció kiírójának elfogadható minimális ár
# Szóval, ha Fb < Fw, akkor nincs kereskedés, ha ekkor olcsóbban adjuk az opciót, hogy Fb=Fw, akkor lesz csere (pénz-opció)
# Ebben az esetben lehet olcsóbban adni, így létrejöhet a tranzakció

# rögzítsük a véletlenszámgenerátor kezdőértékét
set.seed(12345)
#1.000 fele ágaztatunk
i1000 <- read_input(2,1,1000,3,3)
step1000 <- first_day_pricing(i1000,FALSE)
step1000$first_day_price$objective # derivatíva ára
# 11.72638
#kockázatsemlege valószínűségekkel
sum(step1000$payoffs*step1000$first_day_dual)
# 11.72638

# rögzítsük a véletlenszámgenerátor kezdőértékét
set.seed(12345)
# ez még lefut
iproba <- read_input(2,1,6500,3,3)
stepproba <- first_day_pricing(iproba,FALSE)
stepproba$first_day_price$objective # derivatíva ára
# 11.96869
#kockázatsemlege valószínűségekkel
sum(stepproba$payoffs*stepproba$first_day_dual)
# 11.96869

# Egy időszak esetén minél több a kiementel annál többet ér az opció
# A növekedés egyre kisebb mértékű


# # ez nem fut le!
# #Error: cannot allocate vector of size 3.0 Gb
# #10.000 fele ágaztatunk
# set.seed(12345)
# i10000 <- read_input(2,1,10000,3,3)
# step10000 <- first_day_pricing(i10000,FALSE)
# step10000$first_day_price$objective # derivatíva ára

# ez sem fut le
# Error: cannot allocate vector of size 298.0 Gb
# #100.000 fele ágaztatunk
# i100000 <- read_input(2,1,100000,3,3)
# step100000 <- first_day_pricing(i100000,FALSE)
# step100000$first_day_price$objective # derivatíva ára


#___________#
#Két időszak#
#___________#
# rögzítsük a véletlenszámgenerátor kezdőértékét
set.seed(12345)
# A feladat kiírással ellentétben csak 70 felé ágaztatok, mert:
# Error: cannot allocate vector of size 3.0 Gb
gc() # tisztítja a memóriát
twodays <- read_input(2,1,70,70,3)

step1_twodays <- first_day_pricing(twodays,FALSE)
step2_twodays <- second_day_pricing(step1_twodays,twodays)
step2_twodays$second_day_price$objective
# 24.39118
# Az ár nőtt az egy időszakihoz képest.Nagyobb a szabadság, hogy mikor hívhatjuk le, ami többet ér.
#kockázatsemleges valószínűséggel
# kifizetések és a kockázatsemleges valószínűség szorzatösszege
sum(step2_twodays$payoffs*step2_twodays$second_day_dual)
# 24.39118


#_____________#
#Három időszak#
#_____________#
#######################################################################
##Ehhez a részhez kevés a memóriám, hogy korlátos legyen a célfüggvény#
#######################################################################

# rögzítsük a véletlenszámgenerátor kezdőértékét
set.seed(12345)
# A feladat kiírással ellentétben csak 18 felé ágaztatok, mert kevés a memória
# nagyjából ez a 17*17*17-as a legnagyobb, amit a gépem bír memóriával
#  itt csak olyan eredményem van,hogy nem korlátos a célfüggvény
gc() # tisztítja a memóriát
threedays <- read_input(2,1,17,17,17)

step1_threedays <-  first_day_pricing(threedays,FALSE)
step2_threedays <- second_day_pricing(step1_threedays,threedays)
step3_threedays <- third_day_pricing(step1_threedays,step2_threedays,threedays, FALSE)

step3_threedays$third_day_price$objective # derivatíva ára
# nem korlátos
#kockázatsemleges valószínűséggel
# kifizetések és a kockázatsemleges valószínűség szorzatösszege
#sum(step3_threedays$payoffs*step3_threedays$third_day_dual)
#___________________________________________#
## determinisztikus árfolyam az utolsó napon#
#___________________________________________#
gc()
set.seed(12345)

set.seed(12345)
# de sajnos nem elég a memória, így itt csak olyan eredményem van, hogy nem korlátos a célfüggvény
gc() # tisztítja a memóriát
threedays <- read_input(2,1,14,16,20)

step1_threedays <-  first_day_pricing(threedays,FALSE)
step2_threedays <- second_day_pricing(step1_threedays,threedays)
step3_threedays <- third_day_pricing(step1_threedays,step2_threedays,threedays, TRUE)

step3_threedays$third_day_price$objective # derivatíva ára


#______________________________________________________________#
#Az egyes időszakokban eltérő számú irányba is lehet ágaztatni #
#______________________________________________________________#
# rögzítsük a véletlenszámgenerátor kezdőértékét
set.seed(12345)
# n1*n2*n3 szorzat kicsi, akkor nem korlátos a célfüggvény...
gc() # tisztítja a memóriát
threedays0 <- read_input(2,1,3,5,10)

step1_threedays0 <-  first_day_pricing(threedays0,FALSE)
step2_threedays0 <- second_day_pricing(step1_threedays0,threedays0)
step3_threedays0 <- third_day_pricing(step1_threedays0,step2_threedays0,threedays0, FALSE)

step3_threedays0$third_day_price$objective # derivatíva ára
# 1e+30
#_________________________________________________________________________________
# n1*n2*n3 szorzat nagy
# ha nem elég nagy nem lesz korlátos a célfüggvény
# rögzítsük a véletlenszámgenerátor kezdőértékét
set.seed(12345)
gc() # tisztítja a memóriát
threedays2 <- read_input(2,1,12,16,20)

step1_threedays2 <-  first_day_pricing(threedays2,FALSE)
step2_threedays2 <- second_day_pricing(step1_threedays2,threedays2)
step3_threedays2 <- third_day_pricing(step1_threedays2,step2_threedays2,threedays2, FALSE)

step3_threedays2$third_day_price$objective # derivatíva ára
# 1e+30
# nem korlátos a célfüggvény
# nincs elég memóriám lefuttani, hogy korlátos legyen a célfüggvény
# az alábbi inputokkal lefut, csak nem korlátos


###############################################################
###Opció ára 1 periódus esetén a várható érték függvényében####
###############################################################
plot_list <- parameter_sensitivity()
#(Megjegyzés: az ábra felirata angol, mert nem kezelte jól az ékezetes betűket.)
# a plotot 1 időszakra készítettem, mert az biztosan lefut
# 200 felé ágazott a fa
# és 25-ször áraztam be az opciót minden paraméter esetén
#z ábrán átlagos értékek vannak
# a várható érték függvényében az opció ára
plot_list$mu
# ceteris paribus nő a várható érték, akkor kis növekedés után csökken az opció ára
# a szórás függvényében az opció ára
plot_list$sigma
# ceteris paribus nő a szórás érték, akkor nő az opció ára is


#####################################################################
###Portfólióátrendezés sem az 1. sem a 2. időszakban nem lehetséges##
################################################################xx###
# rögzítsük a véletlenszámgenerátor kezdőértékét
set.seed(12345)

gc() # tisztíja a memóriát
noreorder <- read_input(2,1,15,17,20)

noreorder_price <- nullTothree(noreorder)
noreorder_price$third_day_price$objective # ennyi az opció ára
# 19.42329
# ugyanaz jön ki a kockázatsemleges valószínűségekkel is
sum(noreorder_price$payoffs*noreorder_price$third_day_dual)
#  19.42329
# Ahhoz képest, amikor csak 1 napot vettünk nőtt az opció ára
# Ami logikus, mert ugyannyi döntési pont van, de sokkal több kiementel, így nagyobb lehet a kifizetés
# a lejáratkor
#a szcenáriók növelése növelte az opció árát


##############################################################################
###Portfólióátrendezés az 1. periódusban igen, a 2. időszakban nem lehetséges#
################################################################xx############
# rögzítsük a véletlenszámgenerátor kezdőértékét
set.seed(12345)

gc() # tisztíja a memóriát
without_secondreorder <- read_input(2,1,15,17,20)

nosecondreorder_butfirst <- first_day_pricing(without_secondreorder,FALSE)
nosecondreorder_price <- nosecondreorder(nosecondreorder_butfirst, without_secondreorder)

nosecondreorder_price$third_day_price$objective # opció ára
# 28.03204
# ugyanaz jön ki a kockázatsemleges valószínűségekkel is
sum(nosecondreorder_price$payoffs*nosecondreorder_price$third_day_dual)
# 28.03204
# ez nagyobb, mint az 1 időszakos opció, a 2 időszakos opció ára, 
# sőt az előző 3 időszakosnál is, amikor nem lehetett átrendezni a portfóliót
# ami szerintem szintén logikus, mert az 1 naposnál bővebb a kimenetlek száma és a döntési pontok száma
#a  kettesnél nagyobb a kimenetelek száma, de ugyannyia  döntési pont
# míg az előzővel egyező kimenetel van, de több döntési pont (lehívom/nem hívom)
