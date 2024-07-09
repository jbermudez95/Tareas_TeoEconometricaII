
/*******************************************************************************
			PROBLEM SET 4  - ECONOMETRIC THEORY II
			M.A. ECONOMICS - PUC-CHILE			
			
Author	: Jose Carlo Bermúdez
Date    : June, 2024
*******************************************************************************/

	clear all
	set more off
	set varabbrev off
	mat drop _all
	cap est drop *
	
*============================== Paths and working directories ==================

	if "`c(username)'" == "Jose Carlo Bermúdez" {
		cd "C:/Users/bermu/OneDrive - Universidad Católica de Chile/Clases Magíster/Teoría Econométrica II/Tareas_TeoEconometricaII/Problem Set 4"
	}
	* For Nico and Vale replicability :)
	else if "`c(username)'" == "" {												// Insert username											
		cd ""																	// Insert location of your PC where the carpet has been stored
	}
	

/*******************************************************************************
********************************************************************************
					PROBLEM # 1: RANDOMIZATION
********************************************************************************
*******************************************************************************/

* Import working dataset
	use "Data/retro-data-kremer.dta", replace
	
* Create dummy for having at least one flip chart
	gen any_flip = (wallchar > 0 & wallchar != .)
	replace any_flip = cond(missing(wallchar), ., any_flip)
	lab var any_flip "$\mathbbm{1}$(Flip chart $=1$)"
	
* Global for covariates
	global covar stalvl struct rain deskp bkpup classsz
	

*============================== a) Correlation OLS ============================= 

* Histogram for test scores
	histogram nmsc, xline(0, lcolor(red) lw(medthick)) lcolor(blue%30) fcolor(blue%30) 
	graph export "Figures/histogram_scores.pdf", replace

* Run regression	
	cap est drop *
	eststo m1: reg nmsc any_flip, robust
	eststo m2: reg nmsc any_flip, cluster(schoolid)
	esttab m1 m2 using "Tables/item1a.tex", replace b(3) se(3) booktabs star(* 0.10 ** 0.05 *** 0.01) ///
	coeflabel(_cons "Constant" wc "$\mathbbm{1}$(Flip chart $=1$)") postfoot(\bottomrule) nodep nomtitles fragment
	
	
*==================== b) Matching estimation using nearest neighbor method =====

	* ------->>> BE AWARE THAT MATCHING ESTIMATION TAKES MORE THAN HALF AN HOUR TO BE RUN, SO BE PATIENT :P 
	
* Matching estimations
	cap est drop *
		
	* Average Treatment Effect (ATE)
	eststo ate_1:  teffects nnmatch (nmsc ${covar})(any_flip), ate nn(1)  metric(ivar)
	eststo ate_3:  teffects nnmatch (nmsc ${covar})(any_flip), ate nn(3)  metric(ivar)
	eststo ate_6:  teffects nnmatch (nmsc ${covar})(any_flip), ate nn(6)  metric(ivar)
	eststo ate_10: teffects nnmatch (nmsc ${covar})(any_flip), ate nn(10) metric(ivar)

	* Average Treatment Effect on the Treated (ATET)
	eststo att_1:  teffects nnmatch (nmsc ${covar})(any_flip), atet nn(1)  metric(ivar)
	eststo att_3:  teffects nnmatch (nmsc ${covar})(any_flip), atet nn(3)  metric(ivar)
	eststo att_6:  teffects nnmatch (nmsc ${covar})(any_flip), atet nn(6)  metric(ivar)
	eststo att_10: teffects nnmatch (nmsc ${covar})(any_flip), atet nn(10) metric(ivar)
		
	esttab ate_* att_* using "Tables/item1b.tex", replace mtitle("1 NN" "3 NN" "6 NN" "10 NN" "1 NN" "3 NN" "6 NN" "10 NN") b(3) se(3) booktabs ///
	star(* 0.10 ** 0.05 *** 0.01) postfoot(\bottomrule) nodep mgroups("Average Treatment Effect" "Average Treatment on Treated", span 			///
	prefix(\multicolumn{@span}{c}{) suffix(}) pattern(1 0 0 0 1 0 0 0) erepeat(\cmidrule(lr){@span})) coeflabel("$\mathbbm{1}$(Flip chart $=1$)") fragment

* Post balance 
	estimates restore att_10
	tebalance sum 
	matrix bal_post = r(table)
	frmttable using "Tables/post_balance_1b.tex", replace statmat(bal_post) tex fr sdec(3,3,3,3) multicol(1,2,2;1,4,2)		   				   ///
			  rtitles("Teacher training level"\ "Indoor classroom?"\ "Roof does not leak?"\ "Desk per pupil"\ "Books per pupil"\ "Class size") ///
			  ctitles("", "Standardized differences", "", "Variance ratio" ""\ "Covariate", "Raw" "Matched" "Raw" "Matched") hlines(101000001)

	foreach var of varlist $covar {
		tebalance density `var'
		graph export "Figures/balance_`var'.pdf", replace
	}
         

* ================================ c) Alternative method =======================

* Load required packages
	cap ado uninstall ftools
	net install ftools, from("https://raw.githubusercontent.com/sergiocorreia/ftools/master/src/") 
	cap ado uninstall reghdfe
	net install reghdfe, from("https://raw.githubusercontent.com/sergiocorreia/reghdfe/master/src/")

* Identify subjects that are treated or not
	gen stem = cond(inlist(sub,"sca","hsb","mat"),1,0)
	
	encode sub, gen(subject)
	
* Graphical evidence
	* SUTVA 
	preserve
		collapse (mean) wallchar, by(schoolid)
		gen x = _n
		scatter x wallchar, xtitle("Mean # of flip charts per school") ytitle("School ID")
		graph export "Figures/sutva.pdf", replace
	restore
	
	* T-test for the untreated region
	ttest nmsc if any_flip == 0, by(stem)
	loc pv: di %4.3fc `r(p)'
	loc t: di %4.2fc `r(t)'

	preserve
		collapse (mean) mean=nmsc (semean) se=nmsc, by(any_flip stem)
		drop if any_flip ==.
		gen up  = mean + (1.96*se)
		gen low = mean - (1.96*se)
		twoway (scatter mean any_flip if stem == 0, c(l) mc(blue) lc(blue)) (scatter mean any_flip if stem == 1, c(l) mc(red) lc(red)) ///
			   (rcap up low any_flip if stem == 0, lc(blue)) (rcap up low any_flip if stem == 1, lc(red)), 	 ///
			   text(0.14 0.2 "T-test: `t'") text(0.12 0.2 "P-value: `pv'") xtitle("Treatment Region")			 ///
			   legend(row(1) order(1 "No STEM" 2 "STEM") pos(6)) xlab(0 "Untreated" 1 "Treated") 
		graph export "Figures/item1c.pdf", replace
	restore

* Estimate TWFE models
	cap est drop *
	eststo m_1: reg nmsc i.any_flip##i.stem, cluster(subject)
	estadd loc sub_fe "No"
	estadd loc sch_fe "No"
	estadd loc covar  "No"
	eststo m_2: reg nmsc i.any_flip##i.stem $covar, cluster(subject)
	estadd loc sub_fe "No"
	estadd loc sch_fe "No"
	estadd loc covar  "Yes"
	eststo m_3: reghdfe nmsc i.any_flip##i.stem, a(subject schoolid) cluster(subject)
	estadd loc sub_fe "Yes"
	estadd loc sch_fe "Yes"
	estadd loc covar  "No"
	eststo m_4: reghdfe nmsc i.any_flip##i.stem $covar, a(subject schoolid) cluster(subject)
	estadd loc sub_fe "Yes"
	estadd loc sch_fe "Yes"
	estadd loc covar  "Yes"
	
	esttab m_* using "Tables/item1c.tex", replace nomtitles b(3) se(3) booktabs fragment gap nodep keep(1.any_flip#1.stem)       ///
	star(* 0.10 ** 0.05 *** 0.01) postfoot(\bottomrule) coeflabel("$\text{stem}\times\text{any\_flip}$") ///
	stats(N r2 sub_fe sch_fe covar, labels(Observations R-Squared "Subject FE?" "School FE?" "Controls?") fmt(%12.0fc %12.2fc))


* ================================ d) Estimating ATE ===========================

* Load data from the RCT
	use "Data/prosp-data-kremer", clear
	
* Estimating ATE
	cap est drop *
	eststo m1: reg nmsc wc, robust
	eststo m2: reg nmsc wc, cluster(schoolid)
	
	esttab m1 m2 using "Tables/item1d.tex", replace b(3) se(3) booktabs star(* 0.10 ** 0.05 *** 0.01)	 ///
	coeflabel(_cons "Constant" wc "$\mathbbm{1}$(Flip chart $=1$)") postfoot(\bottomrule) nodep nomtitles
	
* Heterogeneous effects by subject
	cap est drop *
	
	global subjects acm eng ghc hsb kis mat sca
	
	foreach s in $subjects {
		gen wc_`s' = wc
		eststo m_`s': reg nmsc wc_`s' if sub == "`s'", cluster(schoolid)
		drop wc_`s'
	}

	coefplot m_*, keep(wc_acm wc_eng wc_ghc wc_hsb wc_kis wc_mat wc_sca) legend(off) coeflabel(wc_acm = "Arts" wc_eng = "English" wc_ghc = "Geography" 	///
			 wc_hsb = "Technology" wc_kis = "Swahili" wc_mat = "Math" wc_sca = "Sciences") order(wc_eng wc_kis wc_ghc wc_acm wc_mat wc_sca wc_hsb)   	///
			 headings(wc_eng = "{bf: No STEM}" wc_mat = "{bf: STEM}") xlabel(-.25(0.05).25)	xline(0, lc(black) lp(dash)) ciopts(recast(rcap)) 		
	graph export "Figures/item1d.pdf", replace


	
/*******************************************************************************
********************************************************************************
					PROBLEM 2: INSTRUMENTAL VARIABLES 
********************************************************************************
*******************************************************************************/

* Install required papckage
	ssc install ivregress2, replace
	
* Load working dataset 
	use "Data/acemoglu_2001.dta", clear
	
* ============================== a) Replication Table 2 ========================

* Some Asian countries are not included in the dummy for Asia 
	replace asia = 1 if inlist(shortnam,"BTN","PRK")

* Dummy for "other" continents, but America is the baseline, so only Europe and Asia are included
	local countries "AUT BGR BIH BLR BEL CHE CZE DEU DNK ESP EST FIN FRA ISL LTU LUX LVA MLT MDA MKD NLD NOR POL PRT GBR GEO GRC HRV HUN IRL ITA ROU RUS SVK SVN SWE YUG AUS FJI MLT NZL"
	gen other = 0
	foreach country in `countries' {
		replace other = 1 if shortnam == "`country'"
	}
		
* Regressions
	cap est drop *
	eststo m_1: reg logpgp95 avexpr, robust
	eststo m_2: reg logpgp95 avexpr if baseco == 1, robust
	eststo m_3: reg logpgp95 avexpr lat_abst, robust
	eststo m_4: reg logpgp95 avexpr lat_abst africa asia other, robust
	eststo m_5: reg logpgp95 avexpr lat_abst if baseco==1, robust
	eststo m_6: reg logpgp95 avexpr lat_abst africa asia other if baseco==1, robust
	
	esttab m_* using "Tables/item2a.tex", replace mtitle("\shortstack{Whole\\World}" "\shortstack{Base\\Sample}" "\shortstack{Whole\\World}" 		 ///
		   "\shortstack{Whole\\World}" "\shortstack{Base\\Sample}" "\shortstack{Base\\Sample}") b(2) se(2) booktabs postfoot(\bottomrule)    		 ///
		   star(* 0.10 ** 0.05 *** 0.01) nodep drop(_cons) coeflabel(avexpr "\shortstack{Average protection against\\expropriation risk, 1985-1995}" ///
		   lat_abst "Latitude" africa "Africa dummy" asia "Asia dummy" other "Other continent dummy") order(avexpr lat_abst asia africa other) 		 ///
		   stat(r2 N, fmt(%9.2fc %12.0fc)) fragment
	

* ============================== c) Replication Table 4 ========================

* From now and on we'll keep base sample only	
	keep if baseco==1
	
* Variable for another continent
	gen other_cont=.
	replace other_cont=1 if (shortnam=="AUS" | shortnam=="MLT" | shortnam=="NZL")
	recode other_cont (.=0)
	tab other_cont
	
* I don't know how to store first stage :'( so I will run independent regressions and store the second stage from IV regresssions
	* PANEL A: 2SLS
	cap est drop *
	eststo m_1: ivreg logpgp95 (avexpr=logem4) 								  
	eststo m_2: ivreg logpgp95 lat_abst (avexpr=logem4) 						  
	eststo m_3: ivreg logpgp95 (avexpr=logem4) 								  if (rich4!=1)
	eststo m_4: ivreg logpgp95 lat_abst (avexpr=logem4) 					  if (rich4!=1)
	eststo m_5: ivreg logpgp95 (avexpr=logem4) 								  if (africa!=1)
	eststo m_6: ivreg logpgp95 lat_abst (avexpr=logem4) 					  if (africa!=1)
	eststo m_7: ivreg logpgp95 (avexpr=logem4) africa asia other_cont 		 
	eststo m_8: ivreg logpgp95 lat_abst (avexpr=logem4) africa asia other_cont 
	
	esttab m_* using "Tables/item2c.tex", replace mtitle("\shortstack{Base\\Sample}" "\shortstack{Base\\Sample}" "\shortstack{Base Sample\\without\\Neo-Europes}" ///
		   "\shortstack{Base Sample\\without\\Neo-Europes}" "\shortstack{Base\\Sample\\without\\Africa}" "\shortstack{Base\\Sample\\without\\Africa}" 			 ///
		   "\shortstack{Base\\Sample\\without\\Africa}" "\shortstack{Base\\Sample\\with\\continent\\dummies}") b(2) se(2) booktabs fragment     	 			 ///
		   star(* 0.10 ** 0.05 *** 0.01) nodep noobs drop(_cons) coeflabel(avexpr "\shortstack{Average protection against\\expropriation risk, 1985-1995}"		 ///
		   lat_abst "Latitude" africa "Africa dummy" asia "Asia dummy" other_cont "Other continent dummy") order(avexpr lat_abst asia africa other_cont) 		 ///
		   refcat(avexpr "\textbf{Panel A: Two-Stage Least Squares}", nolabel)

	* PANEL B: Firs stage
	cap est drop *
	eststo m_1: reg avexpr logem4 								  
	eststo m_2: reg avexpr lat_abst logem4 						 
	eststo m_3: reg avexpr logem4								  if (rich4!=1)
	eststo m_4: reg avexpr lat_abst logem4 						  if (rich4!=1)
	eststo m_5: reg avexpr logem4 								  if (africa!=1)
	eststo m_6: reg avexpr lat_abst logem4 						  if (africa!=1)
	eststo m_7: reg avexpr logem4 africa asia other_cont 		  
	eststo m_8: reg avexpr lat_abst logem4 africa asia other_cont  
	
	esttab m_* using "Tables/item2c.tex", append nomtitles b(2) se(2) booktabs nomtitles star(* 0.10 ** 0.05 *** 0.01)       ///
		   coeflabel(logem4 "Log European settler mortality" lat_abst "Latitude" africa "Africa dummy" asia "Asia dummy" 	///
		   other_cont "Other continent dummy") order(logem4 lat_abst asia africa other_cont) stat(r2, fmt(%9.2fc)) fragment	///
		   refcat(logem4 "\textbf{Panel B: First Stage}", nolabel) nodep noobs drop(_cons) nonumbers
		   
	* PANEL C: OLS 
	cap est drop *
	eststo m_1: reg logpgp95 avexpr 								   
	eststo m_2: reg logpgp95 lat_abst avexpr 					   
	eststo m_3: reg logpgp95 avexpr 							   if (rich4!=1)
	eststo m_4: reg logpgp95 lat_abst avexpr 					   if (rich4!=1)
	eststo m_5: reg logpgp95 avexpr 							   if (africa!=1)
	eststo m_6: reg logpgp95 lat_abst avexpr 					   if (africa!=1)
	eststo m_7: reg logpgp95 avexpr africa asia other_cont          
	eststo m_8: reg logpgp95 lat_abst avexpr africa asia other_cont 

	esttab m_* using "Tables/item2c.tex", append nomtitles b(2) se(2) booktabs nomtitles star(* 0.10 ** 0.05 *** 0.01) nodep keep(avexpr) ///
		   coeflabel(avexpr "\shortstack{Average protection against\\expropriation risk, 1985-1995}") stat(N, fmt(%12.0fc))	 			 ///
		   refcat(avexpr "\textbf{Panel C: Ordinary Least Squares}", nolabel) postfoot(\bottomrule) fragment nonumbers


* ============================== d) Weak Instruments Test ========================

	mat drop _all
	
* Running the test and saving results 
	qui ivregress2 2sls logpgp95 (avexpr=logem4)
	estat firststage 
	loc f_1 = `r(mineig)'
	qui ivregress2 2sls logpgp95 lat_abst (avexpr=logem4) 
	estat firststage
	loc f_2 = `r(mineig)'
	qui ivregress2 2sls logpgp95 (avexpr=logem4) if (rich4!=1)
	estat firststage
	loc f_3 = `r(mineig)'
	qui ivregress2 2sls logpgp95 lat_abst (avexpr=logem4) if (rich4!=1)
	estat firststage
	loc f_4 = `r(mineig)'
	qui ivregress2 2sls logpgp95 (avexpr=logem4) if (africa!=1)
	estat firststage
	loc f_5 = `r(mineig)'
	qui ivregress2 2sls logpgp95 lat_abst (avexpr=logem4) if (africa!=1)
	estat firststage
	loc f_6 = `r(mineig)'
	qui ivregress2 2sls logpgp95 (avexpr=logem4) africa asia other_cont 
	estat firststage
	loc f_7 = `r(mineig)'
	qui ivregress2 2sls logpgp95 lat_abst (avexpr=logem4) africa asia other_cont 
	estat firststage
	loc f_8 = `r(mineig)'
	mat A = r(mineigcv)
	mat A1 = A[2..3,1..4]
	
* Cragg & Donald minimum eigenvalue
	mat F = nullmat(F) \ (`f_1',`f_2',`f_3',`f_4',`f_5',`f_6',`f_7',`f_8')
	mat list F
	frmttable using "Tables/item2d.tex", replace statmat(F) tex fr sdec(2,2,2,2) rtitles("Min eigenvalue (F-stat)") 							///
	ctitles("","\shortstack{Base\\Sample}","\shortstack{Base\\Sample}","\shortstack{Base Sample\\without\\Neo-Europes}", 						///
	"\shortstack{Base Sample\\without\\Neo-Europes}","\shortstack{Base\\Sample\\without\\Africa}","\shortstack{Base\\Sample\\without\\Africa}", ///
	"\shortstack{Base\\Sample\\without\\Africa}","\shortstack{Base\\Sample\\with\\continent\\dummies}")

* Stock & Yogo table
	mat list A1
	frmttable using "Tables/item2d_yogo.tex", replace statmat(A1) tex fr sdec(2,2,2,2) ctitles("Critical values","5\%","10\%","20\%","30\%") ///
	rtitles("2SLS size of nominal 5\% Wald test"\"LIML size of nominal 5\% Wald test ") 
			  
	  
* ============================== e) Replication Table 8 ========================

* PANEL A: 2SLS
	cap est drop *
	eststo m_1:  ivreg logpgp95 (avexpr=euro1900) 		 		
	eststo m_2:  ivreg logpgp95 lat_abst (avexpr=euro1900) 		
	eststo m_3:  ivreg logpgp95 (avexpr=cons00a) 			 	
	eststo m_4:  ivreg logpgp95 lat_abst (avexpr=cons00a)  		
	eststo m_5:  ivreg logpgp95 (avexpr=democ00a) 		 		
	eststo m_6:  ivreg logpgp95 lat_abst (avexpr=democ00a) 		
	eststo m_7:  ivreg logpgp95 (avexpr=cons1) indtime 	 		
	eststo m_8:  ivreg logpgp95 lat_abst (avexpr=cons1) indtime 	
	eststo m_9:  ivreg logpgp95 (avexpr=democ1) indtime 		    
	eststo m_10: ivreg logpgp95 lat_abst (avexpr=democ1) indtime 

	esttab m_* using "Tables/item2e.tex", replace nomtitles b(2) se(2) booktabs fragment nodep noobs keep(avexpr lat_abst) order(avexpr lat_abst) ///
	star(* 0.10 ** 0.05 *** 0.01) coeflabel(avexpr "\shortstack{Average protection against\\expropriation risk, 1985-1995}" lat_abst "Latitude") ///
	refcat(avexpr "\textbf{Panel A: Two-Stage Least Squares}", nolabel)
	
* PANEL B: FIRST STAGE
	cap est drop *
	eststo m_1:  reg avexpr euro1900 		 	   
	eststo m_2:  reg avexpr euro1900 lat_abst  	   
	eststo m_3:  reg avexpr cons00a 			 	   
	eststo m_4:  reg avexpr lat_abst cons00a  	   
	eststo m_5:  reg avexpr democ00a 		 	   
	eststo m_6:  reg avexpr lat_abst democ00a 	   
	eststo m_7:  reg avexpr cons1 indtime 	 	   
	eststo m_8:  reg avexpr lat_abst cons1 indtime  
	eststo m_9:  reg avexpr democ1 indtime 		   
	eststo m_10: reg avexpr lat_abst democ1 indtime 

	esttab m_* using "Tables/item2e.tex", append nomtitles b(2) se(2) booktabs nomtitles nodep noobs nonumbers fragment star(* 0.10 ** 0.05 *** 0.01) ///
	coeflabel(euro1900 "European settlements in 1900" lat_abst "Latitude" cons00a "Constraint on executive in 1900" democ00a "Democracy in 1900" 	  ///
	cons1 "Constraint on executive in first year of indep." democ1 "Democracy in first year of independence") drop(_cons lat_abst indtime) 			  ///
	stat(r2, fmt(%9.2fc)) refcat(euro1900 "\textbf{Panel B: First Stage}", nolabel) order(euro1900 cons00a democ00a cons1 democ1) 
	
* PANEL D: Log mortality as an exogenous variable
	cap est drop *
	eststo m_1:  ivreg logpgp95 (avexpr=euro1900) logem4				   
	eststo m_2:  ivreg logpgp95 lat_abst (avexpr=euro1900) logem4	   
	eststo m_3:  ivreg logpgp95 (avexpr=cons00a) logem4				   
	eststo m_4:  ivreg logpgp95 lat_abst (avexpr=cons00a) logem4		   
	eststo m_5:  ivreg logpgp95 (avexpr=democ00a) logem4				   
	eststo m_6:  ivreg logpgp95 lat_abst (avexpr=democ00a) logem4	   
	eststo m_7:  ivreg logpgp95 (avexpr=cons1) indtime logem4		   
	eststo m_8:  ivreg logpgp95 lat_abst (avexpr=cons1) indtime logem4  
	eststo m_9:  ivreg logpgp95 (avexpr=democ1) indtime logem4		   
	eststo m_10: ivreg logpgp95 lat_abst (avexpr=democ1) indtime logem4 

	esttab m_* using "Tables/item2e.tex", append nomtitles b(2) se(2) booktabs nomtitles nodep noobs nonumbers fragment star(* 0.10 ** 0.05 *** 0.01) 	   ///
	coeflabel(avexpr "\shortstack{Average protection against\\expropriation risk, 1985-1995}" logem4 "Log European settler mortality" lat_abst "Latitude") ///
	keep(avexpr logem4 lat_abst) refcat(avexpr "\textbf{Panel D: 2SLS with Log-Mortality as Exogenous}", nolabel) order(avexpr logem4 lat_abst) postfoot(\bottomrule) 

	
* ============================== f) Hausman test ===============================

* First, let's recover each model
	ivreg logpgp95 (avexpr=logem4) 
	est sto iv_1
	reg logpgp95 avexpr 
	est sto ols_1
 
	ivreg logpgp95 lat_abst (avexpr=logem4) 
	est sto iv_2
	reg logpgp95 avexpr lat_abst
	est sto ols_2	
	 
	ivreg logpgp95 lat_abst (avexpr=logem4) africa asia other_cont 
	est sto iv_3
	reg logpgp95 avexpr lat_abst africa asia other
	est sto ols_3

* Secondly, let's apply hausman tests and store results in table
	mat drop _all
	forvalues i = 1/3 {
		hausman iv_`i' ols_`i'		
		mat chi2_`i' = `r(chi2)'
		mat pv_`i'   = `r(p)'
	}
	
	mat H = nullmat(H) \ (chi2_1,chi2_2,chi2_3\pv_1,pv_2,pv_3)
	
	frmttable using "Tables/item2f.tex", replace statmat(H) tex fr sdec(2,2,2\3,3,3) ///
	rtitles("$\chi^2$"\ "p-value") ctitles("Statistic", "(1)", "(2)", "(3)")		

	
* ============================== h) Table 2, Albouy (2012) =====================

	merge 1:1 shortnam using "Data/albouy_2012.dta"
	drop _merge 
	
* PANEL A: Original data
	cap est drop *
	mat drop _all
	
	* Column 1
	reg avexpr logem4 
	mat beta_1a = _b[logem4]
	mat se_1a   = _se[logem4]
	reg avexpr logem4, robust cluster(logem4)
	mat se_1b = _se[logem4]
	local t   = _b[logem4]/_se[logem4]
	mat pv_1b = 2*ttail(e(df_r),abs(`t'))
	
	* Column 2
	reg avexpr logem4 lat_abst
	mat beta_2a = _b[logem4]
	mat se_2a   = _se[logem4]
	reg avexpr logem4 lat_abst, robust cluster(logem4)
	mat se_2b   = _se[logem4]
	local t   = _b[logem4]/_se[logem4]
	mat pv_2b = 2*ttail(e(df_r),abs(`t'))
	test lat_abst
	mat f_2b = r(p)
	
	* Column 3
	reg avexpr logem4 if rich4 == 0
	mat beta_3a = _b[logem4]
	mat se_3a   = _se[logem4]	
	reg avexpr logem4 if rich4 == 0, robust cluster(logem4)
	mat se_3b = _se[logem4]
	local t   = _b[logem4]/_se[logem4]
	mat pv_3b = 2*ttail(e(df_r),abs(`t'))

	* Column 4
	reg avexpr logem4 africa asia other_cont 
	mat beta_4a = _b[logem4]
	mat se_4a   = _se[logem4]	
	reg avexpr logem4 africa asia other_cont, robust cluster(logem4)
	mat se_4b = _se[logem4]
	local t   = _b[logem4]/_se[logem4]
	mat pv_4b = 2*ttail(e(df_r),abs(`t'))
	test africa asia other_cont
	mat f_4b = r(p)
	
	* Building table in Latex format
	mat panel_A = nullmat(panel_A)\(beta_1a,beta_2a,beta_3a,beta_4a\se_1a,se_2a,se_3a,se_4a\ ///
									se_1b,se_2b,se_3b,se_4b\pv_1b,pv_2b,pv_3b,pv_4b\0,f_2b,0,f_4b)
	mat list panel_A
	
	frmttable using "Tables/item2h.tex", replace statmat(panel_A) tex fr sdec(2,2,2,2) rtitles("Log mortality ($\beta$)"\ ///
	"homocedastic standard error"\"heterocedastic-clustered SE"\"p-value of log mortality"\"p-value of controls") 		  ///
	ctitles("Control variables","\shortstack{No\\controls}","\shortstack{Latitude\\control}","\shortstack{Without\\Neo-\\Europes}", ///
	"\shortstack{Continent\\indicators}") 
	
* PANEL B: Removing conjectured mortality rates
	gen logem = logem4
	replace logem =. if source0==0
	
	cap est drop *
	mat drop _all
	
	* Column 1
	reg avexpr logem, robust
	mat beta_1a = _b[logem]
	mat se_1a   = _se[logem]
	local t   	= _b[logem]/_se[logem]
	mat pv_1a 	= 2*ttail(e(df_r),abs(`t'))	
	
	* Column 2
	reg avexpr logem lat_abst, robust
	mat beta_2a = _b[logem]
	mat se_2a 	= _se[logem]
	local t   	= _b[logem]/_se[logem]
	mat pv_2a 	= 2*ttail(e(df_r),abs(`t'))
	test lat_abst
	mat f_2a = r(p)	
	
	* Column 3
	reg avexpr logem if rich4 == 0, robust
	mat beta_3a = _b[logem]
	mat se_3a 	= _se[logem]
	local t   	= _b[logem]/_se[logem]
	mat pv_3a 	= 2*ttail(e(df_r),abs(`t'))	
	
	* Column 4
	reg avexpr logem africa asia other_cont, robust
	mat beta_4a = _b[logem]
	mat se_4a 	= _se[logem]
	local t   	= _b[logem]/_se[logem]
	mat pv_4a 	= 2*ttail(e(df_r),abs(`t'))
	test africa asia other_cont
	mat f_4a = r(p)	
	
	* Building table in Latex format
	mat panel_B = nullmat(panel_B)\(beta_1a,beta_2a,beta_3a,beta_4a\se_1a,se_2a,se_3a,se_4a\ ///
									pv_1a,pv_2a,pv_3a,pv_4a\0,f_2a,0,f_4a)
	mat list panel_B
	
	frmttable using "Tables/item2h.tex", append statmat(panel_B) tex fr sdec(2,2,2,2) rtitles("Log mortality ($\beta$)"\ ///
	"heterocedastic standard error"\"p-value of log mortality"\"p-value of controls") 
	
* PANEL C: Original data, adding campaign and laborer indicators
	cap est drop *
	mat drop _all
	
	* Column 1
	reg avexpr logem4 campaign slave, robust cluster(logem4)
	mat beta_1a = _b[logem4]
	mat se_1a   = _se[logem4]
	local t   	= _b[logem4]/_se[logem4]
	mat pv_1a 	= 2*ttail(e(df_r),abs(`t'))		
	test campaign slave
	mat f_1a = r(p)	
	
	* Column 2
	reg avexpr logem4 lat_abst campaign slave, robust cluster(logem4)
	mat beta_2a = _b[logem4]
	mat se_2a   = _se[logem4]
	local t   	= _b[logem4]/_se[logem4]
	mat pv_2a 	= 2*ttail(e(df_r),abs(`t'))		
	test campaign slave
	mat f_2a = r(p)	
	test lat_abst 
	mat f_2b = r(p)
	
	* Column 3
	reg avexpr logem4 campaign slave if rich4 == 0, robust cluster(logem4)
	mat beta_3a = _b[logem4]
	mat se_3a   = _se[logem4]
	local t   	= _b[logem4]/_se[logem4]
	mat pv_3a 	= 2*ttail(e(df_r),abs(`t'))		
	test campaign slave
	mat f_3a = r(p)	

	* Column 4
	reg avexpr logem4 africa asia other_cont campaign slave, robust cluster(logem4)
	mat beta_4a = _b[logem4]
	mat se_4a   = _se[logem4]
	local t   	= _b[logem4]/_se[logem4]
	mat pv_4a 	= 2*ttail(e(df_r),abs(`t'))		
	test campaign slave
	mat f_4a = r(p)	
	test africa asia other_cont
	mat f_4b = r(p)

	* Building table in Latex format
	mat panel_C = nullmat(panel_C)\(beta_1a,beta_2a,beta_3a,beta_4a\se_1a,se_2a,se_3a,se_4a\ ///
									pv_1a,pv_2a,pv_3a,pv_4a\f_1a,f_2a,f_3a,f_4a\0,f_2b,0,f_4b)
	mat list panel_C
	
	frmttable using "Tables/item2h.tex", append statmat(panel_C) tex fr sdec(2,2,2,2) rtitles("Log mortality ($\beta$)"\ ///
	"heterocedastic clustered SE"\"p-value of log mortality"\"p-value of indicators"\"p-value of controls") 

* PANEL D: Removing conjectured mortality
	cap est drop *
	mat drop _all
	
	* Column 1
	reg avexpr logem campaign slave, robust 
	mat beta_1a = _b[logem]
	mat se_1a   = _se[logem]
	local t   	= _b[logem]/_se[logem]
	mat pv_1a 	= 2*ttail(e(df_r),abs(`t'))		
	test campaign slave
	mat f_1a = r(p)	

	* Column 2
	reg avexpr logem lat_abst campaign slave, robust 
	mat beta_2a = _b[logem]
	mat se_2a   = _se[logem]
	local t   	= _b[logem]/_se[logem]
	mat pv_2a 	= 2*ttail(e(df_r),abs(`t'))		
	test campaign slave
	mat f_2a = r(p)	
	test lat_abst 
	mat f_2b = r(p)

	* Column 3
	reg avexpr logem campaign slave if rich4 == 0, robust 
	mat beta_3a = _b[logem]
	mat se_3a   = _se[logem]
	local t   	= _b[logem]/_se[logem]
	mat pv_3a 	= 2*ttail(e(df_r),abs(`t'))		
	test campaign slave
	mat f_3a = r(p)

	* Column 4
	reg avexpr logem africa asia other_cont campaign slave, robust 
	mat beta_4a = _b[logem]
	mat se_4a   = _se[logem]
	local t   	= _b[logem]/_se[logem]
	mat pv_4a 	= 2*ttail(e(df_r),abs(`t'))		
	test campaign slave
	mat f_4a = r(p)	
	test africa asia other_cont
	mat f_4b = r(p)
	
	* Building table in Latex format
	mat panel_D = nullmat(panel_D)\(beta_1a,beta_2a,beta_3a,beta_4a\se_1a,se_2a,se_3a,se_4a\ ///
									pv_1a,pv_2a,pv_3a,pv_4a\f_1a,f_2a,f_3a,f_4a\0,f_2b,0,f_4b)
	mat list panel_D
	
	frmttable using "Tables/item2h.tex", append statmat(panel_D) tex fr sdec(2,2,2,2) rtitles("Log mortality ($\beta$)"\ ///
	"heterocedastic standard error"\"p-value of log mortality"\"p-value of indicators"\"p-value of controls") 


	
	
/*******************************************************************************
********************************************************************************
					PROBLEM 3: REGRESSION DISCONTINUITY
********************************************************************************
*******************************************************************************/

	* Load dataset
	use "Data/rdd_otero_rau.dta", clear
	
	* Install the new version of the required package
	ssc install rdrobust, replace
	
	* Create additional outcomes
	gen week_norm1 = week - 2708
	gen week_norm2 = week - 2709
	gen post 	   = (week_norm1 > 0)
	gen police_lag = l1.police
	

* ============================== d) Figure 1 ===================================

	preserve
	drop if week > 2859
	
	* Figure 1a - accidents, alcohol related 
	rdplot acc_alc week_norm2, c(0) p(3) nbins(160 160) graph_options(scheme(s2color) legend(position(6)) ylabel(0(50)155) ///
	title("") xlabel(-160(40)160) legend(off) graphr(color(white)) xtitle(Weeks relative to the law's approval) ylab(,nogrid) xscale(titlegap(3))) 
	graph export "Figures/item5d_a.pdf", replace
	
	* Figure 1b - accidents, non-alcohol related
	rdplot acc_rel week_norm2, c(0) p(3) nbins(160 160) graph_options(scheme(s2color) legend(position(6)) ylabel(400(200)1200) ///
	title("") xlabel(-160(40)160) legend(off) graphr(color(white)) xtitle(Weeks relative to the law's approval) ylab(,nogrid) xscale(titlegap(3)))
	graph export "Figures/item5d_b.pdf", replace

	* Figure 1c - deaths, alcohol related
	rdplot dead_alc week_norm2, c(0) p(3) nbins(160 160) graph_options(scheme(s2color) legend(position(6)) ylabel(0(5)15) ///
	title("") xlabel(-160(40)160) legend(off) graphr(color(white)) xtitle(Weeks relative to the law's approval) ylab(,nogrid) xscale(titlegap(3)))
	graph export "Figures/item5d_c.pdf", replace

	* Figure 1d - deaths, non-alcohol related
	rdplot dead_rel week_norm2, c(0) p(3) nbins(160 160) graph_options(scheme(s2color) legend(position(6)) ylabel(0(10)40) ///
	title("") xlabel(-160(40)160) legend(off) graphr(color(white)) xtitle(Weeks relative to the law's approval) ylab(,nogrid) xscale(titlegap(3)))
	graph export "Figures/item5d_d.pdf", replace

	* Figure 2a - all injuries, alcohol related
	rdplot inj_alc week_norm2, c(0) p(3) nbins(160 160) graph_options(scheme(s2color) legend(position(6)) ylabel(20(20)160) ///
	title("") xlabel(-160(40)160) legend(off) graphr(color(white)) xtitle(Weeks relative to the law's approval) ylab(,nogrid) xscale(titlegap(3)))
	graph export "Figures/item5d_e.pdf", replace
	restore


* ======================== e) Replication Table 5 of the paper =================
	cap est drop *

	* Alcohol related accidents
	eststo m_1: rdrobust acc_alc week_norm2 , c(0) covs(police_lag gas post) h(38.27 38.27)
	estadd scalar band `e(b_r)'
	qui sum acc_alc if week_norm1 >= -4 & week_norm1 < 0
	estadd scalar pre_law_mean `r(mean)'

	* Alcohol related injuries 
	eststo m_2: rdrobust inj_alc week_norm2, c(0) covs(police_lag gas post) h(40.95 40.95) 
	estadd scalar band `e(b_r)'
	qui sum inj_alc if week_norm1 >= -4 & week_norm1 < 0
	estadd scalar pre_law_mean `r(mean)'
	
	* Alcohol related deaths
	eststo m_3: rdrobust dead_alc week_norm2, c(0) covs(police_lag gas post) h(39.14 39.14) 
	estadd scalar band `e(b_r)'
	qui sum dead_alc if week_norm1 >= -4 & week_norm1 < 0
	estadd scalar pre_law_mean `r(mean)'
	
	esttab m_* using "Tables/item3e.tex", replace booktabs nodep noobs nonumbers fragment se(2) b(2) ///
	mtitles("Accidents" "Injuries" "Deaths") postfoot(\bottomrule) coeflabel(RD_Estimate "RD estimate") ///
	stats(band pre_law_mean, labels("Bandwidth" "Pre-law level") fmt(%4.2fc))
	

* ============= f) Replication Table 5 of the paper changing Kernel ============
	cap est drop *
	
	* Triangular kernel
	eststo m_1: rdrobust acc_alc week_norm2 , c(0) covs(police_lag gas post) h(38.27 38.27) kernel(triangular)
	estadd scalar band `e(b_r)'
	qui sum acc_alc if week_norm1 >= -4 & week_norm1 < 0
	estadd scalar pre_law_mean `r(mean)'
	
	* Uniform kernel 
	eststo m_2: rdrobust acc_alc week_norm2 , c(0) covs(police_lag gas post) h(38.27 38.27) kernel(uniform)
	estadd scalar band `e(b_r)'
	qui sum acc_alc if week_norm1 >= -4 & week_norm1 < 0
	estadd scalar pre_law_mean `r(mean)'

	* Epanechnikov kernel
	eststo m_3: rdrobust acc_alc week_norm2 , c(0) covs(police_lag gas post) h(38.27 38.27) kernel(epanechnikov)
	estadd scalar band `e(b_r)'
	qui sum acc_alc if week_norm1 >= -4 & week_norm1 < 0
	estadd scalar pre_law_mean `r(mean)'

	esttab m_* using "Tables/item3f.tex", replace booktabs nodep noobs nonumbers fragment se(2) b(2) ///
	mtitles("Triangular" "Uniform" "Epanechnikov") postfoot(\bottomrule) coeflabel(RD_Estimate "Alcohol related accidents") ///
	stats(band pre_law_mean, labels("Bandwidth" "Pre-law level") fmt(%4.2fc))

	
* ============= g) Replication Table 5 of the paper using IK ===================

	* The latest version for RD does not include the IK version, then I will use the old version
	net install st0366.pkg, from("http://www.stata-journal.com/software/sj14-4/") replace
	
	* Estimations
	cap est drop *

	* Uniform kernel 
	eststo m_1: rdrobust acc_alc week_norm2 gas police_lag post, c(0) bwselect(IK) kernel(triangular) p(2)
	estadd scalar band `e(b_bw)'
	qui sum acc_alc if week_norm1 >= -4 & week_norm1 < 0
	estadd scalar pre_law_mean `r(mean)'
	
	* Triangular kernel
	eststo m_2: rdrobust acc_alc week_norm2 gas police_lag post, c(0) bwselect(IK) kernel(uniform) p(2)
	estadd scalar band `e(b_bw)'
	qui sum acc_alc if week_norm1 >= -4 & week_norm1 < 0
	estadd scalar pre_law_mean `r(mean)'
	
	* Epanechnikov kernel
	eststo m_3: rdrobust acc_alc week_norm2 gas police_lag post, c(0) bwselect(IK) kernel(epanechnikov) p(3)
	estadd scalar band `e(b_bw)'
	qui sum acc_alc if week_norm1 >= -4 & week_norm1 < 0
	estadd scalar pre_law_mean `r(mean)'

	esttab m_* using "Tables/item3g.tex", replace booktabs nodep noobs nonumbers fragment se(2) b(2) ///
	mtitles("Triangular" "Uniform" "Epanechnikov") postfoot(\bottomrule) coeflabel(RD_Estimate "Alcohol related accidents") ///
	stats(band pre_law_mean, labels("Bandwidth" "Pre-law level") fmt(%4.2fc))

