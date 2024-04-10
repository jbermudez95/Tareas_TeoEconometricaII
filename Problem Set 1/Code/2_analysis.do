/*******************************************************************************
			PROBLEM SET 1  - ECONOMETRIC THEORY II
			M.A. ECONOMICS - PUC-CHILE			
			
Author	: Jose Carlo Bermúdez
Name  	: 2_analysis.do
Task  	: Do file running empirical analysis. 
Date    : April, 2024
*******************************************************************************/


* Globals for covariates to be included in estimations
	global covariates1 i.gen_alu i.rango_edad_2018 i.region_metrop i.tipo_inst_1_2018 i.nivel_carrera_2_2018 i.area_conocimiento_2018 i.beneficio_econ
	global covariates2 i.gen_alu i.rango_edad_2018 i.region_metrop i.tipo_inst_1_2018 i.nivel_carrera_2_2018 i.area_conocimiento_2018 i.beneficio_econ i.quintil_se4
	global covariates3 i.gen_alu i.edad i.region_metrop i.tipo_inst_1_2018 i.nivel_carrera_2_2018 i.area i.beneficio_econ i.quintil
	global covariates4 i.gen_alu i.region_metrop i.beneficio_econ

	
/*******************************************************************************	
							QUESTION 1
*******************************************************************************/

* Item a: stay in the system in 2019 (Table 1)
	use "Data/permanece_2018.dta", replace
	
	gen unit = 1
	gcollapse (count) unit, by(permanece)
		
		qui sum unit, d
		gen share = (unit / `r(sum)') * 100
		format share %3.1f 
		
		set obs 3
		qui sum unit, d
		replace unit = `r(sum)' in 3
		qui sum share, d
		replace share = `r(sum)' in 3
		
		order unit share
		mkmat unit share, matrix(stay)
		
		frmttable using "Tables/table1_stay.tex", replace statmat(stay) tex fr ///
		rtitles("Not continue"\ "Continues"\"Total") sdec(0,1) ///
		ctitles("Condition", "Count", "Fraction (\%)")
		
		
* Item b: Logit model (Table 2)
	* Keep unique students and merge with my secondary register of persistent students
	use "Data/matricula_2018.dta", replace
	duplicates drop mrun, force
	
	merge 1:1 mrun using "Data/permanece_2018.dta"
	drop if _merge == 2
	drop _merge 	
	
	* Logit models (coefficients and marginal efects), and OLS for comparisson
	eststo drop *

	eststo m1: logit permanece $covariates3, robust
	eststo m3: margins, dydx(*) atmeans post
	
	eststo m1: logit permanece $covariates3, robust
	eststo m3: margins, dydx(*) atmeans post
		
	eststo m2: logit permanece $covariates4, robust
	eststo m4: margins, dydx(*) atmeans post
		
	eststo m5: reg permanece $covariates1, robust 
	eststo m6: reg permanece $covariates2, robust 
	
	esttab m1 m2 m3 m4 m5 m6 using "Tables/logit_model_beta.tex", replace booktabs f se(2) b(3) star(* 0.10 ** 0.05 *** 0.01) nomtitles postfoot(\bottomrule) ///
		   stats(N r2 r2_p ll, labels(N "R-Squared" "Pseudo R-Squared" "Log-likelihood") fmt(%12.0fc %12.2fc %12.2fc)) label ///
		   mgroups("$\mathbb{P}$(Persist=1)" "Margins" "OLS", span prefix(\multicolumn{@span}{c}{) suffix(}) pattern(1 0 1 0 1 0) erepeat(\cmidrule(lr){@span})) ///
		   drop(1.gen_alu 1.rango_edad_2018 0.region_metrop 1.tipo_inst_1_2018 1.nivel_carrera_2_2018 1.area_conocimiento_2018 0.beneficio_econ 1.quintil_se4) ///
		   refcat(2.gen_alu "\textbf{\underline{Panel A: Sociodemographics}}" 2.tipo_inst_1_2018 "\textbf{\underline{Panel B: Education-Related}}" ///
		   1.beneficio_econ "\textbf{\underline{Panel C: Economic-Related}}", nolabel) 



/*******************************************************************************	
								QUESTION 2
*******************************************************************************/

* Item a: Number of students staying, changing and leaving (Table 3)
	use "Data/trayectoria_2018.dta", clear
	
	gen unit = 1
	gcollapse (count) unit, by(trayectoria)
		
		qui sum unit, d
		gen share = (unit / `r(sum)') * 100
		format share %3.1f 
		
		set obs 4
		qui sum unit, d
		replace unit = `r(sum)' in 4
		qui sum share, d
		replace share = `r(sum)' in 4
		
		order unit share
		mkmat unit share, matrix(trajectory)
		
		frmttable using "Tables/table3_trajectory.tex", replace statmat(trajectory) tex fr ///
		rtitles("Stays in same degree"\ "Changes degree"\"Dropout"\"Total") sdec(0,1) ///
		ctitles("Condition", "Count", "Fraction (\%)")	
		
		
* Items b & c: Multinomial probit and marginal effects (Table 4)
	* Keep unique students and merge with my secondary register of trajectories
	use "Data/matricula_2018.dta", replace
	duplicates drop mrun, force
	
	merge 1:1 mrun using "Data/trayectoria_2018.dta"
	drop if _merge == 2
	drop _merge 
	
	* Multinomial probit models (coefficients, marginal efects)
	eststo drop *
	
	eststo m0: mprobit trayectoria $covariates3, robust base(1)
	eststo m1: margins, dydx(*) predict(outcome(1)) atmeans post
	
	qui mprobit trayectoria $covariates3, robust base(1)
	eststo m2: margins, dydx(*) predict(outcome(2)) atmeans post
	
	qui mprobit trayectoria $covariates3, robust base(1)
	eststo m3: margins, dydx(*) predict(outcome(3)) atmeans post

	esttab m0 m1 m2 m3 using "Tables/multi_probit.tex", replace booktabs f se(2) b(3) star(* 0.10 ** 0.05 *** 0.01) postfoot(\bottomrule) ///
		   stats(N ll, labels(N "Log-likelihood") fmt(%12.0fc %12.2fc %12.2fc)) label noomitted compress unstack nobase ///
		   mtitles("" "Quedarse" "Cambiarse" "Desertar") mgroups("Estimates for $\hat{\beta}$" "Marginal Effects", ///
		   span prefix(\multicolumn{@span}{c}{) suffix(}) pattern(1 1 0 0) erepeat(\cmidrule(lr){@span})) ///
		   refcat(2.gen_alu "\textbf{\underline{Panel A: Sociodemographics}}" 2.tipo_inst_1_2018 "\textbf{\underline{Panel B: Education-Related}}" ///
		   1.beneficio_econ "\textbf{\underline{Panel C: Economic-Related}}", nolabel) 
	
	
* Hausman test from multinomial logit (for Appendix)
	eststo drop *
	mat drop _all
	
	qui mlogit trayectoria $covariates
	estimates store complete
	
	forvalues i = 1/2 {
		forvalues j = 1/3 {
		
			qui mlogit trayectoria ${covariates`i'} if trayectoria != `j'
			estimates store m`i'_without_`j'
			
			eststo m_`i'_`j': qui hausman m`i'_without_`j' complete, alleqs constant force
			mat chi_`j' = r(chi2)
			mat p_`j'	= r(p)
		}
	
	mat A = nullmat(A)\(chi_1, chi_2, chi_3 \ p_1, p_2, p_3)
	mat list A
	}
	
	frmttable using "Tables/hausman.tex", replace statmat(A) tex fr ///
			  rtitles("$\chi^2$"\ "p-value"\"$\chi^2$"\"p-value") sdec(2,2,2\3,3,3\2,2,2\3,3,3) ///
			  ctitles("Statistic", "(1)", "(2)", "(3)")	

	
	
/*******************************************************************************	
								QUESTION 3
*******************************************************************************/

	* Keep unique students and merge with my secondary register of trajectories
	use "Data/matricula_2018.dta", replace
	duplicates drop mrun, force
	
	merge 1:1 mrun using "Data/trayectoria_2018.dta"
	drop if _merge == 2
	drop _merge 	

	
* Item a: Creating group-specific regressors 
	* Rectangularize the dataset first
	forvalues i = 1/3 {
		gen opciones`i' = `i'
	}
	
	reshape long opciones, i(mrun)
	drop _j
	label val opciones trayectoria
	
	* Dummy identifying individual specific choice
	gen individual_choice = (trayectoria == opciones)

	* Create group-specific covariates
	gen tiempo = cond(opciones == 1, 0, ///
			     cond(opciones == 2, 2, ///
			     cond(opciones == 3, 7, .)))
				 
	gen impto = cond(opciones == 1, 0, ///
			    cond(opciones == 2, 30, ///
			    cond(opciones == 3, 100, .)))

			  
* Items b and c: Conditional logit and marginal effects (Table 5)
	eststo drop *
	eststo m1: clogit individual_choice tiempo impto, group(mrun) robust
	eststo m2: clogit individual_choice tiempo impto, group(mrun) robust or
	eststo m3: margins, dydx(tiempo impto) atmeans post
	
	esttab m1 m2 m3 using "Tables/conditional_logit.tex", replace booktabs f se(2) b(3) star(* 0.10 ** 0.05 *** 0.01) postfoot(\bottomrule) ///
		   stats(N ll, labels(N "Log-likelihood") fmt(%12.0fc %12.2fc)) mtitles("Coefficients $\hat{\beta}$" "Odds Ratios" "Marginal Effects") ///
		   coeflabels(tiempo "Time" impto "Tax") eform(0 1)

	
	
/*******************************************************************************	
								QUESTION 4
*******************************************************************************/	 

* Items b & c: Mixed logit and marginal effects
	eststo drop *
	eststo m0: asclogit individual_choice tiempo impto, case(mrun) alternatives(opciones) casevars($covariates4) base(1) nobase vce(robust) collinear nocons
	eststo m1: margins gen_alu 
	
	qui asclogit individual_choice tiempo impto, case(mrun) alternatives(opciones) casevars($covariates4) base(1) nobase vce(robust) collinear nocons
	eststo m2: margins region_metrop
	
	qui asclogit individual_choice tiempo impto, case(mrun) alternatives(opciones) casevars($covariates4) base(1) nobase vce(robust) collinear nocons
	eststo m3: margins beneficio_econ
	
	eststo m4: margins, dydx(tiempo impto)

	esttab m0 using "Tables/mixed_logit_alternative.tex", replace 
	
	booktabs f se(2) b(3) star(* 0.10 ** 0.05 *** 0.01) postfoot(\bottomrule) ///
		   stats(N ll, labels(N "Log-likelihood") fmt(%12.0fc %12.2fc)) label ///
		   coeflabels(tiempo "Time" impto "Tax") eform(0 1) noomitted nobase unstack
		   
		   mtitles("Coefficients $\hat{\beta}$" "Marginal Effects") 

		   
		   esttab m1 m2 m3 using "Tables/mixed_logit_marginals.tex", append
