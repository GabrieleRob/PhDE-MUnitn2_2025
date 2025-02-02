*
use "/Users/roberto/Dropbox/TuttoerCucuzzaro/Documents/Didattica/Corsi/PolicyEvaluation/DOs/abridgedDATA.dta", clear
* Define confounders, heterogeneity and treatment
global xvars emp cashflow_intensity tasso_indebitam knowledge group foreign ///
age i.ateco2digit i.geo i.dsize

global xhet emp cashflow_intensity tasso_indebitam knowledge group foreign age
global w treatment


* Behavioral additionality (y = Collaborations = "cop")
global y cop
qui xi: ivtreatreg $y $w $xvars , model(cf-ols) ///
hetero($xhet) graphic vce(robust)
sum ATE_x , d
scalar lower=r(p1)
scalar upper=r(p99)
xi: ivtreatreg $y $w $xvars if ATE_x>lower & ATE_x<upper , model(cf-ols) ///
hetero($xhet) graphic vce(robust)
cap drop ate_x_coop
gen ate_x_coop=ATE_x


* Input additionality (y = R&D intensity = "RD_intensity")
global y RD_intensity
qui xi: ivtreatreg $y $w $xvars , model(cf-ols) ///
hetero($xhet) graphic vce(robust)
sum ATE_x , d
scalar lower=r(p1)
scalar upper=r(p99)
xi: ivtreatreg $y $w $xvars if ATE_x>lower & ATE_x<upper , model(cf-ols) ///
hetero($xhet) graphic vce(robust)
cap drop ate_x_rs
gen ate_x_rs=ATE_x

order  id_anon anno ate_x_coop ate_x_rs
count if ate_x_coop <=0
count if ate_x_rs <= 0
cap drop int_ate_coop_rs

* Costruisco l'interazione tra "ate_x_coop" e "ate_x_rs"
gen int_ate_coop_rs = ate_x_coop * ate_x_rs


/* Facciamo una prova ****************************************************************
**************************************************************************************
* Costruiamo 4 gruppi di imprese con "ate_coop alto e basso" e "ate_rs alto e basso":
**************************************************************************************
centile ate_x_coop , c(50)
scalar med_coop=r(c_1)
di med_coop

centile ate_x_rs , c(50)
scalar med_rs=r(c_1)
di med_rs

drop indic
gen indic=.
replace indic=1 if ate_x_coop<med_coop & ate_x_coop<med_rs
replace indic=2 if ate_x_coop<med_coop & ate_x_coop>=med_rs
replace indic=3 if ate_x_coop>=med_coop & ate_x_coop<med_rs
replace indic=4 if ate_x_coop>=med_coop & ate_x_coop>=med_rs & ate_x_coop!=.
tab indic 
xi: regress turnin ate_x_coop ate_x_rs i.ateco2digit i.geo i.dsize if indic==4
xi: regress turnin ate_x_coop ate_x_rs i.ateco2digit i.geo i.dsize if indic==2
**************************************************************************************
*count if ATE_x!=. & turnin==0
*415
*count if ATE_x!=. & turnin!=0 & turnin!=.
*691
*di 415 + 691
*1106
*/
**************************************************************************************


* Genero una variabile dummy "inno" (innova vs. non-innova in prodotto)
* "turnin" = quota di fatturato totale del 2000 derivante 
* da prodotti tecnologicamente nuovi o migliorati

* --> Attenzione: ci sono imprese che non innovano in prodotto ma hanno
*     una R&D positiva, in quanto nella CIS hanno dichiarato di avere
*     almeno un altro tipo di innovazione (processo) o avere progetti
*     di innovazione in corso.   

cap drop inno
gen inno=0 if turnin==0
la var inno "1=innova di prodotto; 0=non-innova di prodotto"
replace inno=1 if turnin!=0 & turnin!=.
tab inno

* Considero una regressione lineare di "inno" su 
* "ate_x_coop", ate_x_rs ed "int_ate_coop_rs" (interazione tra le due addizionalit‡)

xi: reg inno ate_x_coop  ate_x_rs int_ate_coop_rs  i.ateco2digit i.geo i.dsize
* sia "ate_x_rs" che "int_ate_coop_rs" sono significativi.

* Genero la derivata di "inno" su "ate_x_rs"
cap drop deriv_inno_on_rs
gen deriv_inno_on_rs = _b[ate_x_rs] + _b[int_ate_coop_rs]* ate_x_coop
twoway (mspline deriv_inno_on_rs ate_x_coop , xline(0.45))
* Al crescere di "ate_x_coop" la derivata di inno su ate_x_rs cresce
* e siccome i coefficienti sono entrambi significativi 0> la cosa Ë significativa !!!

* Si osserva che: fino ad un livello di "ate_x_coop"=0.45 la derivata di "inno"
* su "ate_x_rs" Ë negativa, ma da 0.45 in poi diventa positiva.
* Questo vuol dire che: c'Ë una soglia di addizionalit‡ di comportamento
* oltre il quale l'addizionalit‡ di input ha un effetto positivo e non negativo. 

***********************************************************************************
* Provo a vedere con un modello con i quadtrati, ma viene non significativo.
* Genero coop al quafrato e rs al quadrato
cap drop sq_ate_x_coop sq_ate_x_rs
gen sq_ate_x_coop=ate_x_coop^2
gen sq_ate_x_rs=ate_x_rs^2
cap drop inter_sq
gen inter_sq = sq_ate_x_coop * sq_ate_x_rs

xi: reg inno ate_x_coop sq_ate_x_coop ate_x_rs sq_ate_x_rs  ///
int_ate_coop_rs inter_sq i.ateco2digit i.geo i.dsize
***********************************************************************************

* Costruisco un indicatore "th" indicatore di chi sta a
* dx e sx del threshold di ate_x_coop 

cap drop th
gen th=.
la var th "indicatore di chi sta a dx e sx del threshold di ate_x_coop"
replace th=1 if ate_x_coop <= 0.45
replace th=0 if ate_x_coop > 0.45 &  ate_x_coop!=.
tab th , mis

bys th: sum cop 
count if cop==0 & th==1
count if cop==0 & th==0

***********************************************************************************
*
*
*
****************************************************************
* SVILUPPO LA STESSA ANALISI CON STIMA "DID" INVECE DI "CF-OLS"
****************************************************************
/*
treatment=d_it
x1=emp 
x2=cap_intensity 
x3=cashflow_intensity 
x4=tasso_indebitam 
x5=knowledge
*/

* RD_intensity (y2) --> ATE_x_did_rs                        if d_it_1==0
qui xi: ivtreatreg delta_y2 d_it delta_x1-delta_x5 , model(cf-ols) ///
hetero(delta_x1-delta_x5) vce(robust)
sum ATE_x , d
scalar lower=r(p1)
scalar upper=r(p99)                                         
*d_it_1==0 &
xi: ivtreatreg delta_y2 d_it delta_x1-delta_x5 if  ATE_x>lower & ATE_x<upper , model(cf-ols) ///
hetero(delta_x1-delta_x5) graphic vce(robust)
cap drop ate_x_did_rs
gen ate_x_did_rs=ATE_x

* COOP (y5) --> ATE_x_did_coop
*cap drop y5 y5_1 delta_y5
*gen y5=cop
*sort id anno
*by id: gen y5_1 = y5[_n-1]
*gen delta_y5 = y5 - y5_1                               if d_it_1==0

xi: ivtreatreg delta_y5 d_it delta_x1-delta_x5 , model(cf-ols) ///
hetero(delta_x1-delta_x5) vce(robust)
centile ATE_x , c(1 99)
scalar lower = r(c_1)
scalar upper = r(c_2)                                                          
*                                                          d_it_1==0&
xi: ivtreatreg delta_y5 d_it delta_x1-delta_x5 if  ATE_x>lower & ATE_x<upper , ///
model(cf-ols) hetero(delta_x1-delta_x5) graphic vce(robust)
cap drop ate_x_did_coop
gen ate_x_did_coop=ATE_x

order  id_anon anno ate_x_coop ate_x_rs ate_x_did_coop ate_x_did_rs
cap drop int_ate_did_coop_rs
gen int_ate_did_coop_rs = ate_x_did_coop * ate_x_did_rs

xi: reg inno ate_x_did_coop ate_x_did_rs int_ate_did_coop_rs i.ateco2digit i.geo i.dsize

*** QUESTA REGRESSIONE NON E' SIGNIFICATIVA !!! ***

* Genero la derivata di "inno" su "ate_x_did_rs"
cap drop deriv_inno_did_on_rs
gen deriv_inno_did_on_rs = _b[ate_x_did_rs] + _b[int_ate_did_coop_rs]* ate_x_did_coop
twoway (mspline deriv_inno_did_on_rs ate_x_did_coop , xline())

*******************************************************************************************
* Risultati ATE con DID e CF-OLS
*******************************************************************************************
sum  ate_x_rs ate_x_did_rs ate_x_coop ate_x_did_coop

* FINE





