
/*******************************************************************************
			PROBLEM SET 3  - ECONOMETRIC THEORY II
			M.A. ECONOMICS - PUC-CHILE			
			
Author	: Jose Carlo Bermúdez
Name  	: 2_problem.do
Task  	: Do file running solution to problem 3 on Differences-in-Differences
Date    : June, 2024
*******************************************************************************/


/*******************************************************************************
		c) Replication of table 3 of the paper
*******************************************************************************/

* -------------------------------- Estimations excluding control cohort

	* Import working dataset
	use "Data/study_cohort.dta", clear
	
	keep if (girl == 1)
	keep if (updateR2 > 0.5)

	* Building variables of interest	
	gen date1 = cond(timegroup==1, d04v1, cond(timegroup==2, d04v2, .))
	gen gap   = cond(timegroup==2, d05v2-date1, d05v1-date1)
	
	encode district, gen(district_a)
	encode division, gen(division_a)
	
	global individual  "age agemissing double8"
	global prim_school "sdkcpe missingKCPE clsize girl8perboy8 G_promorate teacherperpupil timegroup d05v1 d05v2"  
	global location    "i.district_a i.division_a total_2km"

	* Regressions
	cap est drop *
	
	eststo m1: reg fertafter12 sampleSD HIVtreat $individual $prim_school $location, clust(schoolid)
	qui sum fertafter12 if sampleSD == 0
	estadd scalar mean `r(mean)'
	estadd loc cohort "\xmark" 
	estadd loc indivi "\cmark"
	estadd loc school "\cmark"
	estadd loc sch_fe "\xmark"
	
	qui probit fertafter12 sampleSD HIVtreat $individual $prim_school $location, asis clust(schoolid)
	eststo m2: margins, dydx(*) post
	qui sum fertafter12 if sampleSD == 0
	estadd scalar mean `r(mean)'
	estadd loc cohort "\xmark" 
	estadd loc indivi "\cmark"
	estadd loc school "\cmark"
	estadd loc sch_fe "\xmark"
	
	eststo m5: reg unmarpreg sampleSD HIVtreat $individual $prim_school $location, clust(schoolid)
	qui sum unmarpreg if sampleSD == 0
	estadd scalar mean `r(mean)'
	estadd loc cohort "\xmark" 
	estadd loc indivi "\cmark"
	estadd loc school "\cmark"
	estadd loc sch_fe "\xmark"
	
	eststo m7: reg marpreg sampleSD HIVtreat $individual $prim_school $location, clust(schoolid)
	qui sum marpreg if sampleSD == 0
	estadd scalar mean `r(mean)'
	estadd loc cohort "\xmark" 
	estadd loc indivi "\cmark"
	estadd loc school "\cmark"
	estadd loc sch_fe "\xmark"


* -------------------------------- Estimations including control cohort

	* Import working dataset
	use "Data/control_cohort.dta", clear
	append using "Data/study_cohort.dta"
	
	keep if (girl == 1)
	keep if (updateR2 > 0.5)
	
	sort schoolid

	* Building variables of interest
	gen sampleSDT = sampleSD*cohort
	
	replace timegroup = cond(cohort == 0, 1, timegroup)
	
	gen date1 	 = cond(cohort==0, d03v1, cond(cohort==1, d04v1, .))
	gen date2 	 = cond(cohort==0, d04v1, cond(cohort==1, d05v2, .))
	gen datebase = cond(cohort==1, date1-365, date1)
	gen gap	  	 = date2-date1	
	
	encode district, gen(district_a)
	encode division, gen(division_a)

	global prim_school = "sdkcpe missingKCPE clsize girl8perboy8 G_promorate teacherperpupil d04v1 d04v2 d05v1 d05v2"  
	
	eststo m3: reg fertafter12 sampleSD cohort sampleSDT HIVtreat $individual $prim_school $location, cluster(schoolid)
	qui sum fertafter12 if cohort==1 & sampleSD==0
	estadd scalar mean `r(mean)'
	estadd loc cohort "\cmark" 
	estadd loc indivi "\cmark"
	estadd loc school "\cmark"
	estadd loc sch_fe "\xmark"
	
	eststo m4: xtreg fertafter12 sampleSD cohort sampleSDT HIVtreat $individual $prim_school $location, cluster(schoolid) fe i(schoolid)
	qui sum fertafter12 if cohort==1 & sampleSD==0
	estadd scalar mean `r(mean)'
	estadd loc cohort "\cmark" 
	estadd loc indivi "\cmark"
	estadd loc school "\xmark"
	estadd loc sch_fe "\cmark"
	
	eststo m6: reg unmarpreg sampleSD cohort sampleSDT HIVtreat $individual $prim_school $location, cluster(schoolid)
	qui sum unmarpreg if cohort==1 & sampleSD==0
	estadd scalar mean `r(mean)'
	estadd loc cohort "\cmark" 
	estadd loc indivi "\cmark"
	estadd loc school "\cmark"
	estadd loc sch_fe "\xmark"
	
	eststo m8: reg marpreg sampleSD cohort sampleSDT HIVtreat $individual $prim_school $location, cluster(schoolid)
	qui sum marpreg if cohort==1 & sampleSD==0
	estadd scalar mean `r(mean)'
	estadd loc cohort "\cmark" 
	estadd loc indivi "\cmark"
	estadd loc school "\cmark"
	estadd loc sch_fe "\xmark"

	esttab m1 m2 m3 m4 m5 m6 m7 m8 using "Tables/table2_did.tex", replace parentheses se(3) b(3) star(* 0.10 ** 0.05 *** 0.01)    	 				   ///
		   label booktabs f alignment(r) gaps compress nonotes noobs plain collabels(none) keep(sampleSD sampleSDT HIVtreat)      	 				   ///
		   order(sampleSD sampleSDT HIVtreat) mtitles("\shortstack{SD\\OLS}" "\shortstack{SD\\PROBIT}" "\shortstack{DD\\OLS}"  	  	 				   ///
		   "\shortstack{DD-FE\\OLS}" "\shortstack{SD\\OLS}" "\shortstack{DD\\OLS}" "\shortstack{SD\\OLS}" "\shortstack{DD\\OLS}") 	 				   ///
		   mgroups("Has started childbearing" "\shortstack{Has started\\childbearing,\\unmarried}" "\shortstack{Has started\\childbearing,\\married}", ///
		   span prefix(\multicolumn{@span}{c}{) suffix(}) pattern(1 0 0 0 1 0 1 0) erepeat(\cmidrule(lr){@span})) numbers		  	 				   ///
		   coeflabel(sampleSD "RR information" sampleSDT "RR information $\times$ 2004 cohort" HIVtreat "TT on HIV/AIDS curriculum") 				   ///
		   stats(mean N cohort indivi school sch_fe, labels("Mean of dependent variable" "Observations" "Control cohort included"    				   ///
		   "Individual characteristics" "Primary school characteristics" "Primary school fixed effects") fmt(%9.3fc %12.0fc)) postfoot(\bottomrule)

	
/*******************************************************************************
		d) Replication of table 4 of the paper
*******************************************************************************/

	* Import working dataset
	use "Data/homesurvey.dta", clear
	
	* We need to adjust agefather_27
	replace agefather_27 = cond(agefather_27>56, 2005-agefather_27, agefather_27)
	
	* Age gap between the mother and father
	gen agegap     = (agefather_27-age)
	replace agegap =. if (agegap > 39)
	
	gen gapabove5	  = 0 if (agegap!=. | yrsolder_27a<3)
	replace gapabove5 = 1 if (agegap>5 & agegap!=.)
	replace gapabove5 = 1 if (yrsolder_27a==1 | yrsolder_27b==1)
	
	gen gapabove10	   = 0 if (agegap!=. | yrsolder_27b<3)
	replace gapabove10 = 0 if (gapabove10==. & gapabove5==0)	
	replace gapabove10 = 1 if (agegap>10 & agegap!=.)
	replace gapabove10 = 1 if (yrsolder_27b==1)
	replace gapabove5  = 1 if (gapabove5==. & gapabove10==0)

	encode district, gen(district_a) 
	encode zone, 	 gen(zone_a)
	
	global school   "sdkcpe girl8perboy8_2004"
	global location "i.district_a  i.zone_a"
	
	* Regressions
	cap est drop *
	
	eststo m1: reg agegap sampleSD SDtreat cohort HIVtreat age $school $location if class==8, clust(schoolid)
	qui sum agegap if class==8 & SDtreat==0
	estadd scalar mean `r(mean)'
	estadd loc cohort "\xmark" 
	estadd loc indivi "\cmark"
	estadd loc school "\cmark"

	eststo m2: reg agegap sampleSD SDtreat cohort HIVtreat age  $school $location if agegap!=40, clust(schoolid)
	qui sum agegap if class==8 & SDtreat==0
	estadd scalar mean `r(mean)'
	estadd loc cohort "\cmark" 
	estadd loc indivi "\cmark"
	estadd loc school "\cmark"
	
	eststo m3: reg gapabove5 sampleSD SDtreat cohort HIVtreat age  $school $location if class==8, clust(schoolid)
	qui sum gapabove5 if class==8 & SDtreat==0
	estadd scalar mean `r(mean)'
	estadd loc cohort "\xmark" 
	estadd loc indivi "\cmark"
	estadd loc school "\cmark"

	qui probit gapabove5 sampleSD SDtreat cohort HIVtreat age  $school $location if class==8, asis clust(schoolid)
	eststo m4: margins, dydx(*) post
	qui sum gapabove5 if class==8 & SDtreat==0
	estadd scalar mean `r(mean)'
	estadd loc cohort "\xmark" 
	estadd loc indivi "\cmark"
	estadd loc school "\cmark"

	eststo m5: reg gapabove5 sampleSD SDtreat cohort HIVtreat age  $school $location, clust(schoolid)
	qui sum gapabove5 if class==8 & SDtreat==0
	estadd scalar mean `r(mean)'
	estadd loc cohort "\cmark" 
	estadd loc indivi "\cmark"
	estadd loc school "\cmark"
	
	eststo m6: reg gapabove10 sampleSD SDtreat cohort HIVtreat age   $school $location if class==8, clust(schoolid)
	qui sum gapabove10 if class==8&SDtreat==0
	estadd scalar mean `r(mean)'
	estadd loc cohort "\xmark" 
	estadd loc indivi "\cmark"
	estadd loc school "\cmark"

	qui probit gapabove10 sampleSD SDtreat cohort HIVtreat age  $school $location if class==8, asis clust(schoolid)
	eststo m7: margins, dydx(*) post
	qui sum gapabove10 if class==8&SDtreat==0
	estadd scalar mean `r(mean)'
	estadd loc cohort "\xmark" 
	estadd loc indivi "\cmark"
	estadd loc school "\cmark"
	
	eststo m8: reg gapabove10 sampleSD SDtreat cohort HIVtreat age $school $location, clust(schoolid)
	qui sum gapabove10 if class==8&SDtreat==0
	estadd scalar mean `r(mean)'
	estadd loc cohort "\cmark" 
	estadd loc indivi "\cmark"
	estadd loc school "\cmark"
	
	esttab m1 m2 m3 m4 m5 m6 m7 m8 using "Tables/table4_did.tex", replace parentheses se(3) b(3) star(* 0.10 ** 0.05 *** 0.01) numbers ///
		   label booktabs f alignment(r) gaps compress nonotes noobs plain collabels(none) keep(sampleSD SDtreat HIVtreat)      	   ///
		   order(sampleSD SDtreat HIVtreat) mtitles("\shortstack{SD\\OLS}" "\shortstack{DD\\OLS}" "\shortstack{SD\\OLS}"  	  	   	   ///
		   "\shortstack{SD\\PROBIT}" "\shortstack{DD\\OLS}" "\shortstack{SD\\OLS}" "\shortstack{SD\\PROBIT}" "\shortstack{DD\\OLS}")   ///
		   mgroups("\shortstack{Age difference between\\teenage girl and her\\partner}" "Age gap > 5 years" "Age gap > 10 years", span ///
		   prefix(\multicolumn{@span}{c}{) suffix(}) pattern(1 0 1 0 0 1 0 0) erepeat(\cmidrule(lr){@span})) postfoot(\bottomrule)	   ///
		   coeflabel(sampleSD "RR information" SDtreat "RR information $\times$ 2004 cohort" HIVtreat "TT on HIV/AIDS curriculum")     ///
		   stats(mean N cohort indivi school, labels("Mean of dependent variable" "Observations" "Control cohort included"     		   ///
		   "Individual characteristics" "Primary school characteristics") fmt(%9.2fc %12.0fc)) 
