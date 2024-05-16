/*******************************************************************************
			PROBLEM SET 2  - ECONOMETRIC THEORY II
			M.A. ECONOMICS - PUC-CHILE			
			
Author	: Jose Carlo Bermúdez
Name  	: 3_problem.do
Task  	: Do file running solution to problem 3 on GMM.
Date    : May, 2024
*******************************************************************************/


/*******************************************************************************
		Prepare working dataset
*******************************************************************************/

* Import data 
	use "Data/nls.dta", replace
	
* Prepare outcomes for estimations
	gen expr	= edad - 6 - educ
	gen expr2   = expr^2
	gen ln_earn = ln(earn)
	
	lab var educ 	"Education"
	lab var earn 	"Earnings"
	lab var iq   	"IQ"
	lab var edad 	"Age"
	lab var expr 	"Experience"
	lab var expr2   "Experience$^2$"
	lab var ln_earn "Ln(Earnings)"
	
	
/*******************************************************************************
		Globals for moment conditions
*******************************************************************************/

	global c1 "ln_earn - {xb: educ expr expr2 iq} - {b0}"
	global c2 "ln_earn - 2.0647"
	global c3 "ln_earn*ln_earn - 4.5258"
	global c4 "educ - 13.97"
	global c5 "ln_earn*educ - 29.099"
	

/*******************************************************************************
		b) Comparisson between OLS and GMM models
*******************************************************************************/

	cap est drop *

* Run OLS model with Eicker-Huber-White correction
	eststo m1: reg ln_earn educ expr expr2 iq, robust
	
* Run GMM model using robust weighting matrix	
	eststo m2: gmm (${c1}), instruments(educ expr expr2 iq) wmatrix(robust) twostep 
	
	esttab m1 m2 using "Tables/gmm1.tex", replace booktabs f b(5) se(5) star(* 0.10 ** 0.05 *** 0.01) gaps label ///
		   mtitles("OLS" "GMM") stats(N, labels("N") fmt(%12.0fc %12.2fc)) postfoot(\bottomrule) coeflabels(b0 "Constant") 
	
	

/*******************************************************************************
		c) GMM estimation using population moments
*******************************************************************************/	

	eststo m3: gmm (${c1}) (${c2}) (${c3}) (${c4}) (${c5}), instruments(educ expr expr2 iq) wmatrix(robust) winitial(unadjusted, independent) twostep
	
	esttab m1 m2 m3 using "Tables/gmm2.tex", replace booktabs f b(5) se(5) star(* 0.10 ** 0.05 *** 0.01) gaps label 				///
		   mtitles("OLS" "GMM" "GMM") stats(N, labels("N") fmt(%12.0fc %12.2fc)) postfoot(\bottomrule) coeflabels(b0 "Constant")	///
		   mgroups("Identified" "Over-Identified", span prefix(\multicolumn{@span}{c}{) suffix(}) pattern(1 0 1) erepeat(\cmidrule(lr){@span})) 
		   
	
/*******************************************************************************
		e) Test for over-identification
*******************************************************************************/
	cap est drop *
	mat drop _all
	
	estat overid
	mat j 	 = r(J)
	mat j_df = r(J_df) 
	mat j_p  = r(J_p)
	
	mat A = nullmat(A)\(j\j_df\j_p)
	
	frmttable using "Tables/hansen.tex", replace statmat(A) tex fr 				///
			  rtitles("Hansen's J-statistic"\ "Degrees of freedom"\"P-value") 	///
			  ctitles("Statistic", "Value")	sdec(2\0\4)	
		   
	