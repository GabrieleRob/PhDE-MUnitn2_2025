*
*
*	STIME PAPER EINT 2020
*
*

use "/Users/roberto/Dropbox/TuttoerCucuzzaro/Documents/Didattica/Corsi/PolicyEvaluation/DOs/POLEVALCONT.dta", clear
version 15

* S (S_LIVELLO):
gen S_LIVELLO=spesa_prog*perc_contrib/100

*list S_LIVELLO W0207

replace S_LIVELLO=0 if W0207!=1
*list S_LIVELLO W0207

sum S_LIVELLO if S_LIVELLO!=0
gen S=(S_LIVELLO-r(min))/(r(max)-r(min))*100
replace S=0 if W0207!=1
sum S


sum S_LIVELLO
gen S1=(S_LIVELLO-r(min))/(r(max)-r(min))*100
replace S=1 if W0207!=1
sum S S1


* P (P_LIVELLO):
gen P_LIVELLO=spesa_prog
replace P_LIVELLO=0 if W0207!=1
qui sum P_LIVELLO if P_LIVELLO!=0
gen P=(P_LIVELLO-r(min))/(r(max)-r(min))*100
replace P=0 if W0207!=1
* S_P (S_QUOTA):
gen S_QUOTA=S_LIVELLO/P_LIVELLO
replace S_QUOTA=0 if W0207!=1
qui sum S_QUOTA if S_QUOTA!=0
gen S_P=(S_QUOTA-r(min))/(r(max)-r(min))*100
replace S_P=0 if W0207!=1
*
*	***********************************************
*
gen logS=ln(S_LIVELLO)
gen LP=ln(P+1)
twoway  (scatter logS S, sort)
*
*	 generazione variabili per modelli classe B:
*
*gen addf2_S=0
gen addf2_S=addf2

*gen S_addf2=0

gen clav_nf2_S=0
gen cxaf2_S=0
gen capintf2_S=0
gen immat_df2_S=0
gen imimm_df2_S=0
gen innointf2_S=0
* 
gen addf4_S=0
gen clav_nf4_S=0
gen cxaf4_S=0
gen capintf4_S=0
gen immat_df4_S=0
gen imimm_df4_S=0
gen innointf4_S=0

*replace S_addf2=S_addf2/addf2 if addf2>0

replace addf2_S=addf2/S_LIVELLO if S_LIVELLO>0
replace clav_nf2_S=clav_nf2/S if S>0
replace cxaf2_S=cxaf2/S if S>0
replace capintf2_S=capintf2/S if S>0
replace immat_df2_S=immat_df2/S if S>0
replace imimm_df2_S=imimm_df2/S if S>0
replace innointf2_S=innointf2/S if S>0

replace addf4_S=addf4/S if S>0
replace clav_nf4_S=clav_nf4/S if S>0
replace cxaf4_S=cxaf4/S if S>0
replace capintf4_S=capintf4/S if S>0
replace immat_df4_S=immat_df4/S if S>0
replace imimm_df4_S=imimm_df4/S if S>0
replace innointf4_S=innointf4/S if S>0

gen ladd=ln(addf2)
* prova: 
* log degli addetti


  gen lcla=log(clav_nf1+1)
twoway (scatter spesa_amm lcla if W0207==1)

sum S_LIVELLO if W0207==1, det
sum spesa_prog if W0207==1, det

pwcorr spesa_amm lcla ladd immat_d immat_d, sig   


graph set window fontface "Times New Roman"

hist S_LIVELLO if W0207==1 ,bin(20) freq kden
hist S if W0207==1, freq 
***AGGIUNTA REV3:
sum S_LIVELLO if W0207==1


*
 * Tabella descrittive sulle osservazioni usate nelle regressioni su FA ed IA (1207):
 *
 tabstat S_LIVELLO spesa_prog if W0207==1, /// 
 stat(n mean sd min max) col(stat) 
 *by(W0207)

 
 XXX
**	***********************************************
**	***************** STIMA MODELLI COMPLETA 
**   STIME DELLA  VERSIONE DI EINT (R 2)     *************
**	***********************************************	
*
*	MODELLO B: OUTCOME IN TASSI:	 log(Y1)-log(Y0)	 
*	
*	B.1S S-LIVELLO, CONTROLLANDO X IL COSTO PROGETTO
*
* effetti a DUE anni:
*
		 xi: ctreatreg T20add W0207 eta clav_nxaddl1  addl1 addl1SQ  cash_resl1  capintl1   i.anno   , /// 
		 graphdrf  model(ct-ols) ct(S) delta(10)  ci(10)  m(3) s(10) 
		 *graphate	                                                                                      P_LIVELLO
		 *hetero(add) 
		 est store S1
		 
		 
		  
		 xi: ctreatreg T20claxadd_d W0207 eta clav_nxaddl1  addl1 addl1SQ  cash_resl1  capintl1   i.anno  , /// 
		 graphdrf  model(ct-ols) ct(S) delta(10)  ci(10)  m(3) s(10) 	 
		  est store S3
		  
		 xi: ctreatreg T20immat_d W0207 eta clav_nxaddl1  addl1 addl1SQ  cash_resl1  capintl1   i.anno  , /// 
		 graphdrf  model(ct-ols) ct(S) delta(10)  ci(10)  m(3) s(10) 	 
		  est store S4
		  
		 xi: ctreatreg T20imimm_d W0207 eta clav_nxaddl1  addl1 addl1SQ  cash_resl1   capintl1  i.anno    , /// 
		 graphdrf  model(ct-ols) ct(S) delta(10)  ci(10)  m(3) s(10) 	 
		  est store S5
		                           *                          immat_dl1  imimm_resl1
                                                                                         
*	  	 
*
* effetti a QUATTRO anni:
*	
*	 
		 xi: ctreatreg T40add W0207 eta clav_nxaddl1  addl1 addl1SQ  cash_resl1  capintl1   i.anno  , /// 
		 graphdrf  model(ct-ols) ct(S) delta(10)  ci(10)  m(3) s(10) 	 
		  est store T1
		  
		 
		  
		 xi: ctreatreg T40claxadd_d W0207 eta clav_nxaddl1  addl1 addl1SQ  cash_resl1  capintl1   i.anno  , /// 
		 graphdrf  model(ct-ols) ct(S) delta(10)  ci(10)  m(3) s(10) 	 
		  est store T3
																	
		 xi: ctreatreg T40immat_d W0207 eta clav_nxaddl1  addl1 addl1SQ  cash_resl1  capintl1   i.anno  , /// 
		 graphdrf  model(ct-ols) ct(S) delta(10)  ci(10)  m(3) s(10) 	
		  est store T4
		  
		 xi: ctreatreg T40imimm_d W0207 eta clav_nxaddl1  addl1 addl1SQ  cash_resl1  capintl1   i.anno  , /// 
		 graphdrf  model(ct-ols) ct(S) delta(10)  ci(10)  m(3) s(10) 	
		  est store T5
		  
	
*
*
outreg2 [S1  S3 S4 S5  T1  T3 T4 T5] using NEW10_2018,  sortvar(W0207 Tw T2w T3w eta clav_nxaddl1  addl1  addl1SQ cash_resl1  capintl1 capintl1SQ) /// 
  bdec(4) e(all) replace see
*
