/*******************************************************************************
			PROBLEM SET 2  - ECONOMETRIC THEORY II
			M.A. ECONOMICS - PUC-CHILE			
			
Author	: Jose Carlo Bermúdez
Name  	: 2_problem.do
Task  	: Do file running solution to problem 2 of duration models
Date    : May, 2024
*******************************************************************************/


/*******************************************************************************
		Prepare working dataset
*******************************************************************************/

* Import data on ofensas
	use "Data/sample.dta", clear
	*drop if mi(spell)
	*drop if (sitoc == 1 & spell == 0) | (sitoc == 2 & spell == 0) | factor_p == 0
	
* Set the dataset in working format, failures are people coming out of unemployment (ocupados)
	stset spell [fw=factor_p], failure(sitoc==1)
	
	
/*******************************************************************************
		Adding labels to covariates
*******************************************************************************/

	lab var dh 		"Hombre" 
	lab var esc 	"Años de Escolaridad" 
	lab var dind 	"Indígena" 
	lab var edad    "Edad"  	
	lab var dedad1  "25-34 años de edad" 
	lab var dedad2  "35-44 años de edad" 
	lab var dedad4  "55-64 años de edad" 				       
	lab var dedad5  "Mayor de 65 años" 
	lab var despido "Despedido" 
	lab var btpv 	"Busca trabajo por primera vez"
	
	
/*******************************************************************************
		Globals for include covariates in regressions
*******************************************************************************/

	global cov1 dh
	global cov2 dh esc
	global cov3 dh esc dind
	global cov4 dh esc dind edad
	global cov5 dh esc dind dedad1 dedad2 dedad4 dedad5 despido
	global cov6 dh esc dind dedad1 dedad2 dedad4 dedad5 despido btpv
	
	
/*******************************************************************************
Replicate table 3 of the paper. This shows that we are working with different 
data so we should not expect to replicate the exact same results.
*******************************************************************************/

	cap est drop *
	
	foreach var of varlist durb sexo dind tedad tesc {
		eststo all_`var' : estpost tabstat spell, s(mean) by(`var')
		eststo ocup_`var': estpost tabstat spell if sitoc == 1, s(mean) by(`var')
		eststo deso_`var': estpost tabstat spell if sitoc == 2, s(mean) by(`var')
	}
	
	esttab ocup_durb deso_durb all_durb using "Tables/summary_desempleo.tex", replace mtitles("Ocupados" "Desocupados" "Todos") ///
		   label booktabs f alignment(r) gaps nonumbers compress nonotes noobs plain collabels(none) unstack cells("mean(fmt(%4.1fc))") ///
		   refcat(0 "\emph{Zona}", nolabel) coeflabels(0 "Rural" 1 "Urbano") drop(Total)
	
	esttab ocup_sexo deso_sexo all_sexo using "Tables/summary_desempleo.tex", append nomtitles ///
		   booktabs f alignment(r) gaps nonumbers compress nonotes noobs plain collabels(none) unstack cells("mean(fmt(%4.1fc))") ///
		   refcat(hombre "\emph{Género}", nolabel) drop(Total) coeflabels(hombre "Hombre" mujer "Mujer")
	
	esttab ocup_dind deso_dind all_dind using "Tables/summary_desempleo.tex", append nomtitles ///
		   booktabs f alignment(r) gaps nonumbers compress nonotes noobs plain collabels(none) unstack cells("mean(fmt(%4.1fc))") ///
		   refcat(No_indigena "\emph{Etnia}", nolabel) drop(Total) coeflabels(indigena "Indígena" No_indigena "No Indígena")
		   
	esttab ocup_tedad deso_tedad all_tedad using "Tables/summary_desempleo.tex", append nomtitles ///
		   booktabs f alignment(r) gaps nonumbers compress nonotes noobs plain collabels(none) unstack cells("mean(fmt(%4.1fc))") ///
		   refcat(25-34 "\emph{Edad}", nolabel) drop(Total) coeflabels(25-34 "25-34 años" 35-44 "35-44 años" 45-54 "45-54 años" 55-64 "55-64 años" 65_y_+ "65 años y más")

	esttab ocup_tesc deso_tesc all_tesc using "Tables/summary_desempleo.tex", append nomtitles ///
		   booktabs f alignment(r) gaps nonumbers compress nonotes noobs plain collabels(none) unstack cells("mean(fmt(%4.1fc))") ///
		   refcat(0 "\emph{Escolaridad}", nolabel) drop(Total) coeflabels(0 "Sin escolaridad" 1-8 "1-8 años" 9-12 "9-12 años" 13-17 "13-17 años") postfoot(\bottomrule)

		   
	
/*******************************************************************************
		b) Replicate table 4 of the paper: log-normal distribution
*******************************************************************************/
	
	cap est drop *
	
	eststo m0: streg, 		distribution(lognormal)
	eststo m1: streg $cov1, distribution(lognormal)
	eststo m2: streg $cov2, distribution(lognormal)
	eststo m3: streg $cov3, distribution(lognormal)
	eststo m4: streg $cov4, distribution(lognormal)
	eststo m5: streg $cov5, distribution(lognormal)
	eststo m6: streg $cov6, distribution(lognormal)
	
	esttab m* using "Tables/duracion_lnormal.tex", replace booktabs f b(3) star(* 0.10 ** 0.05 *** 0.01) nomtitles not gaps ///
		   drop(lnsigma) stats(N p sigma, labels("N observaciones" "Prob>Chi2" "Sigma ($\sigma$)") fmt(%12.0fc %12.2fc))    ///
		   coeflabels(_cons "Constante") postfoot(\bottomrule) label
	
	
	
/*******************************************************************************
		c) Replicate table 4 of the paper: weibull distribution
*******************************************************************************/

	cap est drop *
	
	eststo m0: streg, 		distribution(weibull) nohr time
	eststo m1: streg $cov1, distribution(weibull) nohr time
	eststo m2: streg $cov2, distribution(weibull) nohr time
	eststo m3: streg $cov3, distribution(weibull) nohr time
	eststo m4: streg $cov4, distribution(weibull) nohr time
	eststo m5: streg $cov5, distribution(weibull) nohr time
	eststo m6: streg $cov6, distribution(weibull) nohr time						// La log-likelihood no converge, así que Stata deja de correr si se reproduce el do file manualmente
	
	esttab m* using "Tables/duracion_weibull.tex", replace booktabs f b(3) star(* 0.10 ** 0.05 *** 0.01) nomtitles not gaps label ///
		   drop(ln_p) stats(N p, labels("N observaciones" "Prob>Chi2") fmt(%12.0fc %12.2fc)) coeflabels(_cons "Constante") postfoot(\bottomrule) 

	
/*******************************************************************************
		d) Informaiton criterion
*******************************************************************************/

	cap est drop *
	mat drop _all
	
	qui streg, distribution(lognormal)
	qui estat ic
	mat S_0_a = r(S)
	mat aic_0_a = S_0_a[1,5]
	mat bic_0_a = S_0_a[1,6]
	
	qui streg, distribution(weibull) nohr time	
	qui estat ic
	mat S_0_b = r(S)
	mat aic_0_b = S_0_b[1,5]
	mat bic_0_b = S_0_b[1,6]
	
	qui streg, distribution(ggamma)	
	qui estat ic
	mat S_0_c = r(S)
	mat aic_0_c = S_0_c[1,5]
	mat bic_0_c = S_0_c[1,6]
	
	forvalues i=1/5 {
		streg ${cov`i'}, distribution(lognormal)
		estat ic
		mat S_`i'_a = r(S)
		mat aic_`i'_a = S_`i'_a[1,5]
		mat bic_`i'_a = S_`i'_a[1,6]
		
		streg ${cov`i'}, distribution(weibull) nohr time	
		estat ic
		mat S_`i'_b = r(S)
		mat aic_`i'_b = S_`i'_b[1,5]
		mat bic_`i'_b = S_`i'_b[1,6]
		
		streg ${cov`i'}, distribution(ggamma)	
		estat ic
		mat S_`i'_c = r(S)
		mat aic_`i'_c = S_`i'_c[1,5]
		mat bic_`i'_c = S_`i'_c[1,6]
	}

	mat A = nullmat(A)\(aic_0_a,aic_1_a,aic_2_a,aic_3_a,aic_4_a,aic_5_a\ ///
						aic_0_b,aic_1_b,aic_2_b,aic_3_b,aic_4_b,aic_5_b\ ///
						aic_0_c,aic_1_c,aic_2_c,aic_3_c,aic_4_c,aic_5_c\ ///
						bic_0_a,bic_1_a,bic_2_a,bic_3_a,bic_4_a,bic_5_a\ ///
						bic_0_b,bic_1_b,bic_2_b,bic_3_b,bic_4_b,bic_5_b\ ///
						bic_0_c,bic_1_c,bic_2_c,bic_3_c,bic_4_c,bic_5_c)
	
	frmttable using "Tables/aic.tex", replace statmat(A) tex fr 				///
			  rtitles("Log-normal"\ "Weibull"\"Generalized gamma"\				///
			  "Log-normal"\ "Weibull"\"Generalized gamma") 						///
			  ctitles("Distribution", "(1)", "(2)", "(3)", "(4)", "(5)", "(6)")	///
			  sdec(2,2,2\2,2,2\2,2,2\2,2,2\2,2,2\2,2,2) 



/*******************************************************************************
		e) Survival and hazard functions
*******************************************************************************/

* Survival functions
	* Parametric version for log-normal, weibull, and generalized gamma distributions
	qui streg, distribution(lognormal)
	predict s_lnormal, surv
	lab var s_lnormal "Log-normal"
	
	qui streg, distribution(weibull)
	predict s_weibull, surv
	lab var s_weibull "Weibull"
	
	qui streg, distribution(ggamma)
	predict s_ggamma, surv
	lab var s_ggamma "Generalized gamma"

	sts graph, addplot(line s_lnormal _t, sort lc(red) lp(--) lw(medthick)  || 	///
			   line s_weibull _t, sort lc(green) lp(shortdash) lw(medthick) || 	///
			   line s_ggamma _t, sort lc(yellow) lp(longdash_dot) lw(medthick)) ///
			   title("") xtitle("Number of weeks") plotopts(lc(blue) lw(thick)) legend(row(4) ring(0) pos(1))
	graph export "Figures/survival.pdf", replace
	
	
* Hazard functions: I first compute each one, store them and finally build one single file to make the figure
	* Parametric
	qui streg, distribution(lognormal)
	stcurve, hazard outfile("Data/h_lnormal.dta", replace) nodraw
	
	qui streg, distribution(weibull)
	stcurve, hazard outfile("Data/h_weibull.dta", replace) nodraw

	qui streg, distribution(ggamma)
	qui stcurve, hazard outfile("Data/h_ggamma.dta", replace) nodraw
	
	preserve
		use "Data/h_lnormal.dta", replace
		rename haz1 haz_lnormal
		
		merge 1:1 _t using "Data/h_weibull.dta", gen(m_weib)
		rename haz1 haz_weibull

		merge 1:1 _t using "Data/h_ggamma.dta", gen(m_gamma)
		rename haz1 haz_ggamma
		
		twoway (line haz_lnormal _t, lc(red) lp(--) lw(thick)) (line haz_weibull _t, lc(green) lp(shortdash) lw(thick)) (line haz_ggamma _t, lc(blue) lp(longdash_dot) lw(thick)), ///
			   legend(row(3) order(1 "Log-normal" 2 "Weibull" 3 "Generalized gamma") ring(0) pos(1)) xtitle(Number of weeks)
		graph export "Figures/hazard.pdf", replace
	restore

	* Non-Parametric (Kaplan-Meier)
	sts graph, hazard title("") xtitle("Number of weeks") ytitle("Hazard function") plotopts(lc(blue) lw(thick))
	graph export "Figures/hazard_nonpar.pdf", replace
	
	* Cumulative hazard curve
	sts graph, na title("") xtitle("Number of weeks") ytitle("Cumulative hazard function") plotopts(lc(blue) lw(thick))
	graph export "Figures/hazard_cum.pdf", replace
	
	

/*******************************************************************************
		Appendix: Univariate Analysis for selected categorical covariates
*******************************************************************************/

	* Survival by gender
	sts graph, by(dh) ci legend(row(2) order(2 "Mujer" 4 "Hombre") ring(0) pos(1) size(medlarge)) ///
		xtitle("# of Weeks") subtitle("Probability (%)", placement(7) span) title("")
	graph export "Figures/surv_dh.pdf", replace
	
	* Survival by indigenous
	sts graph, by(dind) ci legend(row(2) order(2 "No indígena" 4 "Indígena") ring(0) pos(1) size(medlarge)) ///
		xtitle("# of Weeks") subtitle("Probability (%)", placement(7) span) title("")
	graph export "Figures/surv_dind.pdf", replace
	
	* Survival by being fired from previous job
	sts graph, by(despido) ci legend(row(2) order(2 "No Despedido" 4 "Despedido") ring(0) pos(1) size(medlarge)) ///
		xtitle("# of Weeks") subtitle("Probability (%)", placement(7) span) title("")
	graph export "Figures/surv_despido.pdf", replace
	
	* Survival by urban zone
	sts graph, by(durb) ci legend(row(2) order(2 "Zona No Urbana" 4 "Zona Urbana") ring(0) pos(1) size(medlarge)) ///
		xtitle("# of Weeks") subtitle("Probability (%)", placement(7) span) title("")
	graph export "Figures/surv_urbano.pdf", replace
	
	
