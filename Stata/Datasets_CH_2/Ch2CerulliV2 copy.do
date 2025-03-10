********************************************************************************
* Book: Econometric Evaluation of Socio-Economic Programs Theory and Applications
* Author: Giovanni Cerulli
* Publisher: Springer
* Edition: Second
********************************************************************************
* CHAPTER 2 - Methods Based on Selection on Observables
********************************************************************************
* Stata codes
********************************************************************************
* DATASETS DOWNLOAD:
* https://www.dropbox.com/sh/49h2r2zugoav39h/AADDw1MvyoXfE5jWzEW7zjhxa?dl=0
********************************************************************************
clear all
* 2.6 Implementation and Application of Regression-Adjustment
net from http://www.stata.com/data/jwooldridge/
net describe eacsap
net get eacsap
cd "/Users/roberto/Dropbox/TuttoerCucuzzaro/Documents/Didattica/Corsi/PolicyEvaluation/PolicyEvaluation20242025/DOs/Datasets_CH_2"
use jtrain2.dta, clear
describe
gen y = re78
gen w = train
global xvars re74 re75 age agesq nodegree married black hisp
global xvarsh re74 re75 age agesq nodegree married black hisp
tabstat y w $xvars, columns(statistics) s(n mean sd min max)
bysort w: tabstat y $xvars, columns(statistics)
qui xi: reg y w
estimates store DIM
qui xi: ivtreatreg y w $xvars , model(cf-ols)
estimates store CFOLS1
xi: ivtreatreg y w $xvars , hetero($xvarsh) model(cf-ols)
estimates store CFOLS2
estimates table DIM CFOLS1 CFOLS2, b(%9.5f) star keep(w) stats(r2) ///
title("ATE comparison between DIM, CFOLS1, CFOLS2")
xi: bootstrap atet=e(atet) atent=e(atent), rep(200): ///
ivtreatreg y w $xvars, hetero($xvarsh) model(cf-ols)
teffects ra (y $xvars, linear) (w)
teffects ra (y $xvars, linear) (w), atet
teffects ra (y $xvars, linear) (w), pomeans
cap drop ATE_x
predict ATE_x, te
qui sum ATE_x
display r(mean)
sum ATE_x if w==1
display r(mean)
sum ATE_x if w==0
display r(mean)
teffects ra (y $xvars, linear) (w), coeflegend
nlcom _b[ATE:r1vs0.w]/_b[POmean:0.w]
ivtreatreg y w $xvars, hetero($xvarsh) model(cf-ols) graphic
teffects ra (y $xvars, linear) (w), aequations
teffects ra (unem78 $xvars, probit) (w)
teffects ra (unem78 $xvars, probit) (w), pomeans

* 2.7 Implementation and Application of Matching
* 2.7.1 Covariates Matching
teffects nnmatch (y $xvars) (w)
teffects nnmatch (y $xvars) (w), nneighbor(3) ematch(hisp black)
teffects nnmatch (y $xvars) (w), biasadj(age re74)

* 2.7.2 Propensity-Score Matching
* 2.7.2.1 PS Matching Using teffects psmatch
teffects psmatch (y) (w $xvars)
qui teffects psmatch (y) (w $xvars), generate(near_obs)
teffects overlap

* 2.7.2.2 PS Matching Using pscore
global mypath2 "/Users/roberto/Dropbox/TuttoerCucuzzaro/Documents/Didattica/Corsi/PolicyEvaluation/PolicyEvaluation20232024/Dos/Datasets_CH_2/"
use "${mypath2}/JTRAIN_CPS1.dta" , clear
global breps 100
global xvars_ps age agesq educ educsq marr nodegree black ///
hisp re74 re74sq re75 u74 u75 u74hisp
pscore w $xvars_ps, pscore(myscore) comsup ///
blockid(myblock) numblo(5) level(0.005) logit
set seed 10101
attnd re78 w $xvars_ps, comsup logit
set seed 10101
attr re78 w $xvars_ps, comsup logit radius(0.001)
set seed 10101
attr re78 w $xvars_ps, comsup logit radius(0.0001)
set seed 10101
attr re78 w $xvars_ps, comsup logit radius(0.00001)
set seed 10101
atts re78 w, pscore(myscore) blockid(myblock) comsup
set seed 10101
attk re78 w $xvars_ps, comsup boot reps($breps) dots logit

* 2.7.2.3 PS Matching Using psmatch2
psmatch2 w, out(re78) pscore(myscore) neighbor(3) common
pstest myscore, both
/* For each variable in varlist it calculates the following indicators (before and after matching if option both is specified):

        (a) t-tests for equality of means in the two samples. T-tests are based on a regression of the variable on a treatment indicator.
        Before matching or on raw samples this is an unweighted regression on the whole sample, after matching the regression is weighted
        using the matching weight variable _weight or user-given weight variable in mweight and based on the on-support sample;

        (b) the standardised percentage bias. If option both is specified, the standardised percentage bias is shown before and after
        matching, together with the achieved percentage reduction in abs(bias).  The standardised % bias is the % difference of the sample
        means in the treated and non-treated (full or matched) sub-samples as a percentage of the square root of the average of the sample
        variances in the treated and non-treated groups (formulae from Rosenbaum and Rubin, 1985).

        (c) the variance ratio (for continuous covariates) of treated over non-treated. This ratio should equal 1 if there is perfect
        balance. An asterisk is displayed for variables that have variance ratios that exceed the 2.5th and 97.5th percentiles of the
        F-distribution with (number of [matched] treated minus 1) and (number of [matched] treated minus 1) degrees of freedom.  These
        F-percentiles only offer a rough guide and are reported at the bottom of the table (see Austin, 2009).  Alternatively, if option
        rubin is specified, the ratio of the variance of the residuals orthogonal to the linear index of the propensity score in the
        treated group over the non-treated group is shown for each covariate. One asterisk is displayed for variables "of concern" - ratio
        in [0.5, 0.8) or (1.25, 2]; two asterisks are displayed for "bad" variables - ratio <0.5 or >2 (see Rubin, 2001).
*/
pstest $xvars_ps
sum myscore [aweight=_weight] if w==1
sum myscore [aweight=_weight] if w==0

* Some graphs:
psgraph
* Some tests:
pstest $xvars_ps, t(w) mw(_weight) onlysig graph both
*
/*  (a) Pseudo R2 from probit estimation of the conditional treatment probability (propensity score) on all the variables in varlist on raw samples, matched samples (default) or both before and after matching. Also displayed are the
corresponding P-values of the likelihood-ratio test of the joint insignificance of all the regressors (before and after matching if option both is specified);

        (b) the mean and median bias as summary indicators of the distribution of the abs(bias);

        (c) the percentage of continuous variables that have variance ratios that exceed the 2.5th and 97.5th percentiles of the F-distribution, or, if option rubin is specified, the percentage of all covariates orthogonal to the propensity score with the specified variance ratios (% of concern and % bad, with % good being the complement to 100);

        (d) Rubins' B (the absolute standardized difference of the means of the linear index of the propensity score in the treated and (matched) non-treated group) and Rubin's R (the ratio of treated to (matched) non-treated variances of the propensity score index).  Rubin (2001) recommends that B be less than 25 and that R be between 0.5 and 2 for the samples to be considered sufficiently balanced.  An asterisk is displayed next to B and R values that fall outside those limits.
*/
*
*********************************
**	GRAFICI PROPENSITY SCORE:
*********************************
histogram _pscore if _support==1 & _weight!=. [fweight = floor(_weight*100)], bin(20) kdensity  kdenopts(bw(0.02)) by(w)
histogram _pscore if _support==1  , bin(20) kdensity  kdenopts(bw(0.02)) by(w)

*********************************
**	Imbens tests on PS:
*********************************	
probit w $$xvars_ps if (_support==1)&(_weight!=.) [fweight=floor(_weight*100)]
*********************************
**	Imbens tests:
*********************************	
psmatch2 w $$xvars_ps , /// 
out(re78 re74)  /// 
 n(3) common
 *
 
 psmatch2 w re75 age agesq nodegree marr black hisp , /// 
out(re78 re74)  /// 
 n(3) common
*
label define tstatus 0 Comparison_sample 1 Treated_sample
label values w tstatus
label variable w "Treatment Status"

graph twoway (kdensity myscore if w==1, msize(small)) ///
(kdensity myscore if w==0, msize(small) lpattern(shortdash_dot)), ///
subtitle(, bfcolor(none)) ///
xtitle("propensity–score (Before)", size(medlarge)) ///
xscale(titlegap(*7)) ytitle("Density", size(medlarge)) yscale(titlegap(*5)) ///
legend(pos(12) ring(0) col(1)) ///
legend(label(1 "Treated") label(2 "Untreated")) saving(BEFORE,replace)

graph twoway (kdensity myscore [aweight=_weight] if w==1, msize(small)) ///
(kdensity myscore [aweight=_weight] if w==0, msize(small) lpattern(shortdash_dot)), ///
subtitle(, bfcolor(none)) ///
xtitle("propensity–score (After)", size(medlarge)) ///
xscale(titlegap(*7)) ytitle("Density", size(medlarge)) yscale(titlegap(*5)) ///
legend(pos(12) ring(0) col(1)) ///
legend(label(1 "Treated") label(2 "Untreated")) saving(AFTER,replace)

graph combine BEFORE.gph AFTER.gph

cap drop myscore
global xvars re74 re75 age agesq nodegree married black hisp
*example with balancing property not satisfied:
pscore w $xvars_ps, pscore(myscore) comsup
psmatch2 w, out(re78) pscore(myscore) common
gen delta = re78 - _re78 if _treated==1 & _support==1 
* Rosenbaum sensitivity test:
rbounds delta, gamma(1 (1) 3)

* 2.7.3 An Example of Coarsened-Exact Matching Using cem
imb age educ black nodegree re74, treatment(w)
cem age educ black nodegree re74, treatment(w)
regress re78 w [iweight=cem_weights]

* 2.8 Implementation and Application of Reweighting

* 2.8.1 The Stata Routine treatrew
use fertil2.dta , clear
reg children educ7
ivtreatreg children educ7 age agesq evermarr urban electric tv, ///
hetero(age agesq evermarr urban electric tv) model(cf-ols)
bootstrap atet=e(atet) atent=e(atent), rep(200): ///
ivtreatreg children educ7 age agesq evermarr urban electric tv, ///
hetero(age agesq evermarr urban electric tv) model(cf-ols)           // errore pag. 162

treatrew children educ7 age agesq evermarr urban electric tv, model(probit)
treatrew children educ7 age agesq evermarr urban electric tv, model(logit)
bootstrap e(ate) e(atet) e(atent), reps(200): ///
treatrew children educ7 age agesq evermarr urban electric tv, model(probit)
bootstrap e(ate) e(atet) e(atent), reps(200): ///
treatrew children educ7 age agesq evermarr urban electric tv, model(logit) graph

global xvars "age agesq evermarr urban electric tv"
psmatch2 educ7 age agesq evermarr urban electric tv, ate out(children) com
bootstrap r(ate) r(atu): psmatch2 educ7 $xvars, ate out(children) com
pstest _pscore

* 2.8.2 The Relation Between treatrew and Stata 13's teffects ipw
teffects ipw (children) (educ7 $xvars, probit), ate
global xvars age agesq evermarr urban electric tv
probit educ7 $xvars, robust 
predict _ps, p 
gen H=(1/_ps)*educ7+1/(1-_ps)*(1-educ7) 
reg children educ7 [pw=H], vce(robust) 

* 2.8.3 An Application of the Doubly-Robust Estimator
global xvars age agesq evermarr urban electric tv
teffects aipw (children $xvars) (educ7 $xvars) , ate  // errore pag. 170 
teffects aipw (children $xvars) (educ7 $xvars), pomeans aequations

* END
