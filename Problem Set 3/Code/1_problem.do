
/*******************************************************************************
			PROBLEM SET 3  - ECONOMETRIC THEORY II
			M.A. ECONOMICS - PUC-CHILE			
			
Author	: Jose Carlo Bermúdez
Name  	: 1_problem.do
Task  	: Do file running solution to problem 2 on matching
Date    : June, 2024
*******************************************************************************/


/*******************************************************************************
		Prepare working dataset at the city level
*******************************************************************************/

	* Import working dataset 
	use "Data/Data_CityLevel_ReplicationQJE.dta", replace
	
	* Build variables of interest
		* Population and population growth
		gen growth_1750_1850  = ln(pop_1850/pop_1750)
		gen growth_1750_1850b = ((pop_1850/pop_1750)^(1/100)-1)*100
		gen growth_1700_1750  = ln(pop_1750/pop_1700)
		
		gen lnpop1750 = ln(pop_1750)
		gen lnpop1700 = ln(pop_1700)	
	
		*Subscriptions to Encyclopedie
		gen subcriptions_pc = subs/pop_1750
		gen lnsub 			= ln(1+subs)
		gen lnsub_pc 		= ln(1+subs/pop_1750)
		gen sub_dummy 		= (subs>0 & !missing(subs))

		*Dummies for increasing subscriber density
		xtile sub_percentil   = subcriptions_pc if subs>0, nq(2)
		replace sub_percentil = cond(sub_percentil == . & subcriptions_pc == 0, 0, sub_percentil) 
		tab sub_percentil, gen(d_percentil)

		*Dummy for Paris
		gen dummy_paris = (city_id==873)
		
		*Dummy for non-French speaking departments
		gen non_french_dpt = (department_id == 66 | department_id == 67 | department_id == 28 | ///
							  department_id == 19 | department_id == 65 | department_id == 63)
		
		*Early printing
		gen ln_num_edit = ln(1+num_edit)
		
		*STN Books
		gen ln_stn_total_pc = ln(1+stn_total/pop_1750)
		
		*Distance nearest coal location
		gen ln_coal_field = ln(dist_nearest_coal_field)
		
		*Scientific Societies
		gen lnss_members_pc = ln(1+ss_members/pop_1750)
		gen lnarts_metiers_pc = ln(1+arts_metiers/pop_1750)
		gen artsmetiers_dummy = 0 if arts_metiers !=.
		replace artsmetiers_dummy = 1 if arts_metiers>0 & arts_metiers!=.

	* Label variables
	lab var growth_1750_1850  "Growth 1750-1850"
	lab var growth_1750_1850b "Growth 1750-1850"
	lab var lnpop1750 		  "ln(population 1750)"
	lab var non_french_dpt 	  "Non French speaking"
	lab var print_cities	  "Printing press"
	lab var ln_num_edit		  "ln(books printed 1500)"
	lab var ln_stn_total_pc	  "ln(STN books density)"
	lab var ln_coal_field	  "ln(distance coal)"
	
	* Global for covariates, estimations at the city level
	global covariates1 dummy_paris atlantic_dept_dummy medport_dept_dummy navriver_dept_dummy uni_dept_dummy print_cities_dept_dummy lndept_num_edit non_french_dpt
	global covariates2 atlanticport medport navigable_river dummy_paris non_french_dpt uni_dummy print_cities ln_num_edit	

	
	
/*******************************************************************************
		b) Replication of figure 3 of the paper
*******************************************************************************/

	twoway (kdensity growth_1750_1850 if d_percentil1==1 & pop_1750!=. & dummy_paris==0, bwidth(0.2) lcolor(navy) lpattern(solid) range(-0.5 1.5) lwidth(thick)) ///
		   (kdensity growth_1750_1850 if d_percentil2==1 & pop_1750!=. & dummy_paris==0, bwidth(0.2) lcolor(blue) lpattern(dash) lwidth(thick) lpattern(solid))  ///
		   (kdensity growth_1750_1850 if d_percentil3==1 & pop_1750!=. & dummy_paris==0, bwidth(0.2) lcolor(ebblue) lpattern(dashdot) lwidth(thick)), 			 ///
		   ytitle(Kernel density) legend(order(1 "No subscriptions" 2 "Subscriptions p.c. > 0, below-median" 3 "Subscriptions p.c. > 0, above-median") 			 ///
		   cols(1) ring(0) pos(2) size(small)) xtitle(City growth 1750-1850) 
	 graph export "Figures/figure3.pdf", replace
	 

	 
/*******************************************************************************
		e) Replication of table 2 of the paper
*******************************************************************************/

	* Dummy for outliers
	sum growth_1750_1850, d
	gen outlier_1750_1850 = 1 if (growth_1750_1850>=1.722767 | growth_1750_1850<=-1.386294)
	
	sum growth_1700_1750, d
	gen outlier_1700_1750 = 1 if (growth_1700_1750>=1.386294 | growth_1700_1750<=-.5108256)
	
	* Estimations
	cap est drop *
	
	eststo: nnmatch growth_1750_1850 sub_dummy pop_1750 if (non_french_dpt==0 & outlier_1750_1850!=1), tc(att) m(3) robust(3) 
	estadd loc pop   "\cmark"
	estadd loc locat "\xmark"
	
	eststo: nnmatch growth_1750_1850 sub_dummy pop_1750 if (non_french_dpt==0 & pop_1750>=4 & pop_1750<=26 & outlier_1750_1850!=1), tc(att) m(3) robust(3)
	estadd loc pop   "\cmark"
	estadd loc locat "\xmark"
	
	eststo: nnmatch growth_1750_1850 sub_dummy pop_1750 latitude longitude if (non_french_dpt==0 & outlier_1750_1850!=1), tc(att) m(3) robust(3) 
	estadd loc pop   "\cmark"
	estadd loc locat "\cmark"
	
	eststo: nnmatch growth_1750_1850 sub_dummy pop_1750 latitude longitude if (pop_1750>=4 & pop_1750<=26 & non_french_dpt==0 & outlier_1750_1850!=1), tc(att) m(3) robust(3)
	estadd loc pop   "\cmark"
	estadd loc locat "\cmark"
	
	eststo: nnmatch growth_1700_1750 sub_dummy pop_1700 latitude longitude if (non_french_dpt==0 & outlier_1700_1750!=1), tc(att) m(3) robust(3)
	estadd loc pop   "\cmark"
	estadd loc locat "\cmark"
	
	eststo: nnmatch growth_1700_1750 sub_dummy pop_1700 latitude longitude if (pop_1750>=4 & pop_1750<=26 & non_french_dpt==0 & outlier_1700_1750!=1), tc(att) m(3) robust(3)
	estadd loc pop   "\cmark"
	estadd loc locat "\cmark"
	
	esttab using "Tables/table2_matching.tex", replace booktabs f se(2) b(3) star(* 0.10 ** 0.05 *** 0.01) postfoot(\bottomrule)   ///
		   stats(pop locat N, labels("Population" "Location" "Observations") fmt(%12.0fc)) mgroups("Period 1750–1850" "1700–1750", ///
		   span prefix(\multicolumn{@span}{c}{) suffix(}) pattern(1 0 0 0  1 0) erepeat(\cmidrule(lr){@span})) 					   ///
		   mtitles("All" "10–90 pct" "All" "10–90 pct" "All" "10–90 pct") coeflabel(SATT "$\mathbbm{1}[Subs>0]$")
		   
		   
		   
/*******************************************************************************
		f) Replication of table 8 of the paper
*******************************************************************************/
	
	global cond1 non_french_dpt==0 & outlier_1750_1850!=1 & scient_soc_found_year<=1750 | scient_soc_found_year==.
	global cond2 non_french_dpt==0 & outlier_1700_1750!=1 & scient_soc_found_year<=1750 | scient_soc_found_year==.
	global cond3 non_french_dpt==0 & outlier_1750_1850!=1
	global cond4 scient_soc_found_year<=1750 | scient_soc_found_year==.
	
	* Panel A
	cap est drop *
	eststo: nnmatch growth_1750_1850 scient_soc pop_1750 if ($cond1), tc(att) m(3) robust(3)
	estadd loc pop   "\cmark"
	estadd loc locat "\xmark"

	eststo: nnmatch growth_1750_1850 scient_soc pop_1750 latitude longitude if ($cond1), tc(att) m(3) robust(3)
	estadd loc pop   "\cmark"
	estadd loc locat "\cmark"

	eststo: nnmatch growth_1700_1750 scient_soc pop_1700 latitude longitude if ($cond2), tc(att) m(3) robust(3)
	estadd loc pop   "\cmark"
	estadd loc locat "\cmark"

	eststo: nnmatch growth_1750_1850 artsmetiers_dummy pop_1750 if ($cond3), tc(att) m(3) robust(3)
	estadd loc pop   "\cmark"
	estadd loc locat "\xmark"

	eststo: nnmatch growth_1750_1850 artsmetiers_dummy pop_1750 latitude longitude if ($cond3), tc(att) m(3) robust(3) 
	estadd loc pop   "\cmark"
	estadd loc locat "\cmark"
	
	esttab using "Tables/table8_matching.tex", replace booktabs f se(3) b(3) star(* 0.10 ** 0.05 *** 0.01)    ///
		   stats(pop locat N, labels("Population" "Location" "Observations") fmt(%12.0fc)) 					  ///
		   mgroups("Pre-1750 scientific societies" "Desc. Arts et Metiers", span 							  ///
		   prefix(\multicolumn{@span}{c}{) suffix(}) pattern(1 0 0 1 0) erepeat(\cmidrule(lr){@span})) 		  ///
		   mtitles("1750–1850" "1750–1850" "1700–1750" "1750–1850" "1750–1850") coeflabel(SATT "$\mathbbm{1}[x>0]$") ///
		   refcat(SATT "\textbf{Panel A: Matching estimation}", nolabel)
	
	* Panel B
	cap est drop *
	eststo: reg growth_1750_1850 lnss_members_pc literacy_males_1786 lnpop1750 $covariates2 if ($cond4) [aweight=pop_1750], cl(department_id)
	estadd loc controls "\cmark"
	
	eststo: reg growth_1750_1850 lnss_members_pc scient_soc literacy_males_1786 lnpop1750 $covariates2 if ($cond4) [aweight=pop_1750], cl(department_id)
	estadd loc controls "\cmark"
	
	eststo: reg growth_1700_1750 lnss_members_pc literacy_males_1686 lnpop1700 $covariates2 if ($cond4) [aweight=pop_1700], cl(department_id)
	estadd loc controls "\cmark"
	
	eststo: reg growth_1750_1850 lnarts_metiers_pc literacy_males_1786 lnpop1750 $covariates2 [aweight=pop_1750], cl(department_id) 
	estadd loc controls "\cmark"
	
	eststo: reg growth_1750_1850 lnarts_metiers_pc artsmetiers_dummy literacy_males_1786 lnpop1750 $covariates2 [aweight=pop_1750], cl(department_id)
	estadd loc controls "\cmark"
	
	esttab using "Tables/table8_matching.tex", append booktabs f se(3) b(3) star(* 0.10 ** 0.05 *** 0.01) /// 
		   stats(r2 controls N, labels("$R^2$" "Controls" "Observations") fmt(%9.2fc %12.0fc)) nomtitles nonumbers	///
		   keep(lnss_members_pc scient_soc lnarts_metiers_pc artsmetiers_dummy) postfoot(\bottomrule) refcat(lnss_members_pc "\textbf{Panel B: OLS}", nolabel) ///
		   coeflabel(lnss_members_pc "ln[density(x)]" lnarts_metiers_pc "ln[density(x)]" artsmetiers_dummy "$\mathbbm{1}[x>0]$" scient_soc "$\mathbbm{1}[x>0]$") 		   


	 
/*******************************************************************************
		d) Replication of table 1 of the paper
*******************************************************************************/

* -------------------------------- Estimations using data at the department level	
	* Panels A & B
	cap est drop *
	
	foreach var in lnpop1750 atlanticport medport navigable_river non_french_dpt uni_dummy print_cities ln_num_edit {
		eststo m1_`var': reg lnsub_pc `var' dummy_paris, robust
		eststo m2_`var': reg lnsub_pc `var' dummy_paris if subs > 0, robust
	}	
	
	esttab m1_lnpop1750 m2_lnpop1750 using "Tables/table1a.tex", replace mtitles("All (no controls)" "Subs$>$0") parentheses ///
		   label booktabs f alignment(r) gaps nonumbers compress nonotes noobs plain collabels(none) unstack ///
		   refcat(lnpop1750 "\textbf{Panel A: Baseline controls}", nolabel) drop(_cons dummy_paris) se(3) b(3) star(* 0.10 ** 0.05 *** 0.01)
	
	esttab m1_atlanticport m2_atlanticport using "Tables/table1a.tex", append label nomtitles booktabs f se(3) b(3) gaps parentheses ///
		   alignment(r) nonumbers compress nonotes noobs plain collabels(none) unstack drop(_cons dummy_paris) star(* 0.10 ** 0.05 *** 0.01)

	esttab m1_medport m2_medport using "Tables/table1a.tex", append label nomtitles booktabs f se(3) b(3) gaps parentheses ///
		   alignment(r) nonumbers compress nonotes noobs plain collabels(none) unstack drop(_cons dummy_paris) star(* 0.10 ** 0.05 *** 0.01)

	esttab m1_navigable_river m2_navigable_river using "Tables/table1a.tex", append label nomtitles booktabs f se(3) b(3) gaps parentheses ///
		   alignment(r) nonumbers compress nonotes noobs plain collabels(none) unstack drop(_cons dummy_paris) star(* 0.10 ** 0.05 *** 0.01)

	esttab m1_non_french_dpt m2_non_french_dpt using "Tables/table1a.tex", append label nomtitles booktabs f se(3) b(3) gaps parentheses ///
		   alignment(r) nonumbers compress nonotes noobs plain collabels(none) unstack drop(_cons dummy_paris) star(* 0.10 ** 0.05 *** 0.01)

	esttab m1_uni_dummy m2_uni_dummy using "Tables/table1a.tex", append label nomtitles booktabs f se(3) b(3) gaps parentheses ///
		   alignment(r) nonumbers compress nonotes noobs plain collabels(none) unstack drop(_cons dummy_paris) star(* 0.10 ** 0.05 *** 0.01) ///
		   refcat(uni_dummy "\textbf{Panel B: Early knowledge controls}", nolabel)
		   
	esttab m1_print_cities m2_print_cities using "Tables/table1a.tex", append label nomtitles booktabs f se(3) b(3) gaps parentheses ///
		   alignment(r) nonumbers compress nonotes noobs plain collabels(none) unstack drop(_cons dummy_paris) star(* 0.10 ** 0.05 *** 0.01)
		   
	esttab m1_ln_num_edit m2_ln_num_edit using "Tables/table1a.tex", append label nomtitles booktabs f se(3) b(3) gaps parentheses ///
		   alignment(r) nonumbers compress nonotes noobs plain collabels(none) unstack drop(_cons dummy_paris) star(* 0.10 ** 0.05 *** 0.01) postfoot(\bottomrule)		   
		   
	* Panel D
	foreach var in ln_stn_total_pc pays_election ln_coal_field {
		eststo m1_`var': reg lnsub_pc `var', robust
		eststo m2_`var': reg lnsub_pc `var' $covariates2, robust
		eststo m3_`var': reg lnsub_pc `var' $covariates2 if subs>0, robust		
	} 
		   
		   
* -------------------------------- Estimations using data at the department level	
	use "Data/Data_DeptLevel_ReplicationQJE.dta", replace
	
	* Globals for covariates, estimations at the department level
	global covariates3 dummy_paris atlantic_dept_dummy medport_dept_dummy navriver_dept_dummy uni_dept_dummy print_cities_dept_dummy lndept_num_edit non_french_dpt
	
	* Create variables of interest
	gen lnsub_dept_pc_avg   = ln(1+sub_dept_pc_avg)
	gen lndept_num_edit	    = ln(dept_num_edit+1)
	gen school_rate1837	    = students_1837/pop_school_age_1837
	replace school_rate1837 = 1 if school_rate1837>1
	gen school_rate1856     = students_1856/pop_school_age_1856
	replace school_rate1837 = (.4636725/.5843091)*school_rate1856 if missing(school_rate1837)
	
	gen lnproto_ind_tot_pc = ln(proto_ind_total/citypop_dept_1750+1)
	recode lnproto_ind_tot_pc .=0 if proto_ind_total==0
	
	gen ln_nobles_1750_pc=ln(nobles_1750_dep/citypop_dept_1750+1)
	recode ln_nobles_1750_pc .=0 if nobles_1750_dep==0
	
	gen dummy_paris 	  = (department_id==72)
	gen non_french_dpt    = (department_id == 66 | department_id == 67 | department_id == 28 | ///
							 department_id == 19 | department_id == 65 | department_id == 63)	
							 
	lab var school_rate1837    "School rate 1837"
	lab var lnproto_ind_tot_pc "ln(pre industrial density)"
	lab var ln_nobles_1750_pc  "ln(nobles density)"
							 
	* Panels C & D
	foreach var in literacy_males_1686 literacy_males_1786 school_rate1837 lnproto_ind_tot_pc ln_nobles_1750_pc {
		eststo m1_`var': reg lnsub_dept_pc_avg `var', robust
		eststo m2_`var': reg lnsub_dept_pc_avg `var' $covariates3, robust
		eststo m3_`var': reg lnsub_dept_pc_avg `var' $covariates3 if sub_dept_dummy > 0, robust 
	}
	
	esttab m1_literacy_males_1686 m2_literacy_males_1686 m3_literacy_males_1686 using "Tables/table1b.tex", replace label booktabs f se(3) b(3) gaps parentheses ///
		   alignment(r) nonumbers compress nonotes noobs plain collabels(none) unstack keep(literacy_males_1686) star(* 0.10 ** 0.05 *** 0.01) ///
		   refcat(literacy_males_1686 "\textbf{Panel C: Worker skills}", nolabel) mtitles("All (no controls)" "All (controls)" "Subs$>$0")
		   
	esttab m1_literacy_males_1786 m2_literacy_males_1786 m3_literacy_males_1786 using "Tables/table1b.tex", append label nomtitles booktabs f se(3) b(3) gaps parentheses ///
		   alignment(r) nonumbers compress nonotes noobs plain collabels(none) unstack keep(literacy_males_1786) star(* 0.10 ** 0.05 *** 0.01)

	esttab m1_school_rate1837 m2_school_rate1837 m3_school_rate1837 using "Tables/table1b.tex", append label nomtitles booktabs f se(3) b(3) gaps parentheses ///
		   alignment(r) nonumbers compress nonotes noobs plain collabels(none) unstack keep(school_rate1837) star(* 0.10 ** 0.05 *** 0.01)
		   
	esttab m1_ln_stn_total_pc m2_ln_stn_total_pc m3_ln_stn_total_pc using "Tables/table1b.tex", append label nomtitles booktabs f se(3) b(3) gaps parentheses ///
		   alignment(r) nonumbers compress nonotes noobs plain collabels(none) unstack keep(ln_stn_total_pc) star(* 0.10 ** 0.05 *** 0.01) ///
		   refcat(ln_stn_total_pc "\textbf{Panel D: Additional controls}", nolabel) coeflabel(ln_stn_total_pc "ln(STN books density)")
		   
	esttab m1_pays_election m2_pays_election m3_pays_election using "Tables/table1b.tex", append label nomtitles booktabs f se(3) b(3) gaps parentheses ///
		   alignment(r) nonumbers compress nonotes noobs plain collabels(none) unstack keep(pays_election) star(* 0.10 ** 0.05 *** 0.01) coeflabel(pays_election "Pays d'election")
		   
	esttab m1_lnproto_ind_tot_pc m2_lnproto_ind_tot_pc m3_lnproto_ind_tot_pc using "Tables/table1b.tex", append label nomtitles booktabs f se(3) b(3) gaps parentheses ///
		   alignment(r) nonumbers compress nonotes noobs plain collabels(none) unstack keep(lnproto_ind_tot_pc) star(* 0.10 ** 0.05 *** 0.01)
	   
	esttab m1_ln_coal_field m2_ln_coal_field m3_ln_coal_field using "Tables/table1b.tex", append label nomtitles booktabs f se(3) b(3) gaps parentheses ///
		   alignment(r) nonumbers compress nonotes noobs plain collabels(none) unstack keep(ln_coal_field) star(* 0.10 ** 0.05 *** 0.01) coeflabel(ln_coal_field "ln(distance coal)")

	esttab m1_ln_nobles_1750_pc m2_ln_nobles_1750_pc m3_ln_nobles_1750_pc using "Tables/table1b.tex", append label nomtitles booktabs f se(3) b(3) gaps parentheses ///
		   alignment(r) nonumbers compress nonotes noobs plain collabels(none) unstack keep(ln_nobles_1750_pc) star(* 0.10 ** 0.05 *** 0.01) postfoot(\bottomrule)		   
		   
