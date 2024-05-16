/*******************************************************************************
			PROBLEM SET 2  - ECONOMETRIC THEORY II
			M.A. ECONOMICS - PUC-CHILE			
			
Author	: Jose Carlo Bermúdez
Name  	: 1_problem.do
Task  	: Do file running solution to problem 1 of counting models
Date    : May, 2024
*******************************************************************************/


/*******************************************************************************
		Prepare working dataset
*******************************************************************************/
	
* Import data on ofensas
	use "Data/ofensas.dta", clear
	
* Adding labels to variables so the'd look nice in the tables :) 
	lab var ofensas "Offenses" 
	lab var Casado  "$\mathbbm{1}(\text{Married}=1)$"
	lab var Edad 	"Age"
	lab var Edad_2  "Age$^2$"
	lab var Urbano  "$\mathbbm{1}(\text{Urban}=1)$"
	
* Global for covariates
	global covariates1 Casado Edad Edad_2 Urbano
	
	
/*******************************************************************************
		a) Summary statistics
*******************************************************************************/

* Table
	eststo drop *
	estpost tabstat ofensas $covariates1, statistics(mean Var sd min p50 max count) columns(statistics)
	
	esttab using "Tables/summary.tex", replace collabels("Mean" "Var" "SD" "Min" "Median" "Max" "N") label gap booktabs f ///
		   cells("mean(fmt(%12.2fc)) variance(fmt(%12.2fc)) sd(fmt(%12.2fc)) min(fmt(%12.2fc)) p50(fmt(%12.2fc)) max(fmt(%12.2fc)) count(fmt(%12.0fc))") ///
		   refcat(Casado "\underline{\emph{Covariates}}", nolabel) noobs nonumber nomtitles postfoot(\bottomrule)
		   
		   
* Histogram for the number of crimes
	qui sum ofensas, d
	loc x_p25 = `r(p25)' + 3
	loc x_p50 = `r(p50)' + 3
	loc x_p75 = `r(p75)' + 3
	loc skew: di %4.2fc `r(skewness)'
	loc kurt: di %4.2fc `r(kurtosis)'
	
	hist ofensas, legend(off) percent ylabel(0(5)25) lcolor(gray%50) fcolor(gray%50) ///
		 xline(`r(p25)', lp(dash) lc(red) lw(thin)) text(27 `x_p25' "p25", size(vsmall) color(red)) ///
		 xline(`r(p50)', lp(dash) lc(red) lw(thin)) text(25 `x_p50' "p50", size(vsmall) color(red)) ///
	     xline(`r(p75)', lp(dash) lc(red) lw(thin)) text(27 `x_p75' "p75", size(vsmall) color(red)) ///
		 text(23 100 "Skewness = `skew'") text(21 100 "Kurtosis = `kurt'") ytitle("") 				/// 
		 subtitle("Percent (%)", placement(7) span) xtitle("Number of Crimes")
	graph export "Figures/hist_crimes.pdf", replace
		 
		 
* Correlation matrix
	corr ofensas $covariates1
	mat corrmat = r(C)
	heatplot corrmat, values(format(%4.3f) size(medium)) legend(off) color(hcl diverging, intensity(.7)) 
	graph export "Figures/heatmap.pdf", replace
	
		   
		  
/*******************************************************************************
		b) Poisson and negative binomia regressions
*******************************************************************************/

* Poisson model
	eststo drop *
	poisson ofensas $covariates1, robust
	outreg2 using "Tables/poisson_reg.tex", replace ctitle("Estimates for $\hat{\beta}$") label
	
	margins, dydx(*) atmeans post
	outreg2 using "Tables/poisson_reg.tex", append ctitle("Marginal effects") label

	poisson ofensas $covariates1, robust irr
	outreg2 using "Tables/poisson_reg.tex", append ctitle("Incidence rate ratios") label eform
	
	
* Negative binomial model
	eststo drop *
	nbreg ofensas $covariates1, robust 
	outreg2 using "Tables/neg_binom_reg.tex", replace ctitle("Estimates for $\hat{\beta}$") label

	margins, dydx(*) atmeans post
	outreg2 using "Tables/neg_binom_reg.tex", append ctitle("Marginal effects") label

	nbreg ofensas $covariates1, robust irr
	outreg2 using "Tables/neg_binom_reg.tex", append ctitle("Incidence rate ratios") label eform
	
	
	
/*******************************************************************************
		Measuring overdispersion
*******************************************************************************/
	
* First way: Goodness of fit test
	mat drop _all
	qui poisson ofensas $covariates1, robust
	estat gof
	mat pear   = r(chi2_p)
	mat dev    = r(chi2_d)
	mat p_pear = r(p_p)
	mat p_dev  = r(p_d)
	mat A = nullmat(A)\(pear, dev \ p_pear, p_dev)
	mat list A
	
	frmttable using "Tables/overdispersion.tex", replace statmat(A) tex fr ///
			  rtitles("$\chi^2$"\ "p-value") sdec(2,2\3,3) ///
			  ctitles("Statistic", "Pearson goodness-of-fit", "Deviance goodness-of-fit")	
			  
			  
* Second way: À la Cameron & Trivedi
	qui poisson ofensas $covariates1, vce(robust)
	predict errors
	gen ystar = ((ofensas-errors)^2-ofensas)/errors
	reg ystar errors, nocons
	outreg2 using "Tables/cameron.tex", replace ctitle("", "Offenses")
	
		   