/*******************************************************************************
			PROBLEM SET 1  - ECONOMETRIC THEORY II
			M.A. ECONOMICS - PUC-CHILE			
			
Author: Jose Carlo Bermúdez
Name  : 1_cleaning.do
Task  : Do file cleaning raw datasets from Ministry of Education, and converts 
		them into final working datasets. 
Date  : April, 2024
*******************************************************************************/

	
/*******************************************************************************	
 Matricula 2018: This is my core dataset, so I will clean it as best as possible, 
				 and then I'm going to merge any further dataset to a cleaned 
				 version of matricula.
*******************************************************************************/
	
* Loading raw dataset in 2018
	import delimited "Data/20230802_Matrícula_Ed_Superior_2018_PUBL_MRUN", clear
	
	
* Keep students who started in 2018 an undergrad degree only
	order mrun cat_periodo, first
	sort  mrun cat_periodo
	keep if (anio_ing_carr_ori == 2018 & nivel_global == "Pregrado")
	
	
* Drop students that have missing RUN in 2018 (barely account for 0.08% of the sample)

	* Table for Appendix
	preserve
		keep if mi(mrun)
		gen unit = 1
		gcollapse (count) unit, by(forma_ingreso)
		gsort -unit
		
		qui sum unit, d
		gen share = (unit / `r(sum)') * 100
		format share %3.1f 
		
		set obs 4
		qui sum unit, d
		replace unit = `r(sum)' in 4
		qui sum share, d
		replace share = `r(sum)' in 4
		
		order unit share forma_ingreso
		mkmat unit share, matrix(missing_cases)
		
		frmttable using "Tables/missing.tex", replace statmat(missing_cases) tex fr ///
		rtitles("Foreign student"\ "Directly (regular student)"\"Other"\"Total") sdec(0,1) ///
		ctitles("Accessed through", "Count", "Fraction (\%)") hlines(1010011) 
	restore
	
	drop if mi(mrun)
	
	
* For analysis I keep duplicates in 2018, so unique id is at the level of RUN-Codigo Unico.
* However, I can drop duplicates convinently when needed. 
* If I drop duplicates I get 338,336 students, which is consistent with records from MINEDUC.
	duplicates tag mrun, gen(tag_duplicates)

	
	
/*******************************************************************************	
Adding those who got some financial benefit at the begining of studies, so I 
only keep category "Estudiante que ingresa por primera vez a educación superior 
y obtiene un beneficio"	for 2018.
*******************************************************************************/

	preserve
		import delimited "Data/Asignacion 2018_PA_PUBL.csv", clear
		
		duplicates drop mrun, force
		keep if tipo_alumno == 1
		
		gen beneficio_econ = 1
		drop decil_dfe beneficio_beca_fscu
		
		tempfile beneficiarios 
		save "`beneficiarios'"
	restore
	
	merge m:1 mrun using "`beneficiarios'", gen(merge_beneficiario)
	drop if merge_beneficiario == 2
	
	replace beneficio_econ = cond(merge_beneficiario == 1 & mi(beneficio_econ), 0, beneficio_econ)
	lab def beneficio_econ 0 "Sin beneficio" 1 "Beneficiario"
	lab val beneficio_econ beneficio_econ
	
	

/*******************************************************************************	
Adding Formulario Único de Acreditación Socioeconómica (FUAS)
I'm not gonna use this dataset with the solely purpose of getting some additional 
covariates such as the quintile of income. If the student is duplicated (applied
to FUAS twice) I keep records from 2018, because most of them seem to be 
manipulating their income, probably because they were not awarded with any 
financial support in the prior year.
*******************************************************************************/

	* Load and process FUAS in 2018
	preserve
		import delimited "Data/BBDD FUAS 2018_PUBL.csv", clear
		keep anio_proceso mrun quintil_se4 decil_se4
		tempfile fuas_2018
		save "`fuas_2018'"		
	restore
	
	
	* Load and process FUAS in 2019, and harmonize with records form 2018
	preserve
		import delimited "Data/BBDD FUAS 2019_PUBL.csv", clear
		rename quinti_se4 quintil_se4
		keep anio_proceso mrun quintil_se4 decil_se4
		append using "`fuas_2018'"	
		duplicates tag mrun, g(t)
		keep if t == 0 | (t == 1 & anio_proceso == 2018)
		drop t
		tempfile income_distr
		save "`income_distr'"
	restore
	
	merge m:1 mrun using "`income_distr'", gen(merge_income)
	drop if merge_income == 2
	

	
/*******************************************************************************	
Destring. In this section of the code I re-build some variables of intererst that
are in string format, which is too heavy for Stata to handle efficiently. 
*******************************************************************************/

	foreach var of varlist rango_edad tipo_inst_1 tipo_inst_2 tipo_inst_3 nomb_inst nomb_sede  		   			   ///
						   modalidad jornada tipo_plan_carr region_sede provincia_sede comuna_sede nivel_carrera_1 ///
						   nivel_carrera_2 requisito_ingreso area_conocimiento area_carrera_generica forma_ingreso ///
						   cine_f_97_area cine_f_97_subarea {
								local varname `var'
								encode `var', gen(`var'_lab)
								drop `var'
								rename `var'_lab `varname'_2018
						   }
						   
						   
	* Drop outcomes that are irrelevant
	drop vigencia_carrera formato_valores valor_matricula valor_arancel codigo_demre cine_f_13_area ///
		 cine_f_13_subarea acreditada_carr acreditada_inst acre_inst_desde_hasta acre_inst_anio
		 
	* Create variable for Region Metropolitana
	gen region_metrop = (region_sede_2018 == 13)
	lab def region_metrop 0 "Fuera de la RM" 1 "En la RM"
	lab val region_metrop region_metrop
	
	* Label for gender variable 
	lab def gen_alu 1 "Hombre" 2 "Mujer"
	lab val gen_alu gen_alu
	
	* Income quintile
	destring quintil_se4, replace
	lab def quintil_se4 1 "Quintil 1" 2 "Quintil 2" 3 "Quintil 3" 4 "Quintil 4" 5 "Quintil 5"
	lab val quintil_se4 quintil_se4
	
	* Clean age data 
	replace rango_edad_2018 = . if rango_edad_2018 == 7
	
	gen year = 2018
	gen cod_carrera_2018  = cod_carrera 
	gen codigo_unico_2018 = codigo_unico
	gen nomb_carrera_2018 = nomb_carrera
	
	gen edad = (rango_edad_2018 == 59)
	lab def edad 0 "Mayor a 19" 1 "15 a 19 años"
	lab val edad edad
	
	gen area = (area_conocimiento_2018 == 10)
	lab def area 0 "Otra" 1 "Tecnología"
	lab val area area
	
	gen quintil = (quintil_se4 == 5 & !mi(quintil_se4))
	replace quintil = . if mi(quintil_se4)
	lab def quintil 0 "Bajo el quintil 5" 1 "Quintil 5"
	lab val quintil quintil
	
	gen mujer = (gen_alu == 2)
	lab def mujer 0 "Hombre" 1 "Mujer"
	lab val mujer mujer
	
	compress
	save "Data/matricula_2018.dta", replace
	

	
/*******************************************************************************	
Dataset identifying stayers 2018-2019
*******************************************************************************/

	* Keep unique students in 2019
	preserve
		import delimited "Data/20230802_Matrícula_Ed_Superior_2019_PUBL_MRUN", clear		
		keep if (anio_ing_carr_ori == 2018 & nivel_global == "Pregrado")
		drop if mi(mrun)
		gcollapse (mean) cat_periodo, by(mrun)
		tempfile unique_2019
		save "`unique_2019'"	
	restore
	
	* Keep unique students in 2018`and merge with 2019
	gcollapse (mean) cat_periodo, by(mrun)
	merge 1:1 mrun using "`unique_2019'"
	drop if _merge == 2
	
	gen permanece = (_merge == 3)
	drop _merge
	
	compress
	save "Data/permanece_2018.dta", replace
	
	
	
/*******************************************************************************	
Dataset identifying stayers, changers and drop outs 2018-2019.
Now my strategy is turning my dataset from a wide to a long format, so I should
be able to easily identify status and deal with duplicates. I follow the next criteria:

* Cambiarse: tenía una sola carrera y se cambió o tenía más de una carrera y no continuó en ninguna anque sigue estudiando.
* Permanece: tenía una sola carrera y continuó en ella o tenía más de una carrera y continuó en al menos una.
* Desertor: tenía una sola carrera y no continuó o tenía más de una carrera y no continuó en ninguna.
*******************************************************************************/

* Loading raw dataset in 2018
	import delimited "Data/20230802_Matrícula_Ed_Superior_2018_PUBL_MRUN", clear
	
* Keep students who started in 2018 an undergrad degree only
	keep if (anio_ing_carr_ori == 2018 & nivel_global == "Pregrado")
	drop if mi(mrun)
	
	gen year = 2018
	gen cod_carrera_2018  = cod_carrera 
	gen codigo_unico_2018 = codigo_unico
	gen nomb_carrera_2018 = nomb_carrera
	
	keep mrun codigo_unico year cod_carrera_2018 codigo_unico_2018 nomb_carrera_2018
	
* Loading dataset in 2019
	preserve
		import delimited "Data/20230802_Matrícula_Ed_Superior_2019_PUBL_MRUN", clear
		keep if (anio_ing_carr_ori == 2018 & nivel_global == "Pregrado")
		drop if mi(mrun)
		duplicates drop mrun codigo_unico, force
		gen year = 2019
		gen cod_carrera_2019  = cod_carrera 
		gen codigo_unico_2019 = codigo_unico
		gen nomb_carrera_2019 = nomb_carrera
		keep mrun codigo_unico year cod_carrera_2019 codigo_unico_2019 nomb_carrera_2019
		tempfile yr2019
		save "`yr2019'"
	restore
	
* Merge with 2019 at the RUN-Código level	
	merge m:1 mrun codigo_unico using "`yr2019'" 
	drop if _merge == 2
	
* Duplicate is "stayer" if stayed in at least one degree (max_merge will be = 3)
* Duplicate is "leaver" if did not continue in any degree (max_merge will be = 1)
	bys mrun: egen max_merge = max(_merge)
	
* Now I can drop duplicates as random 
	duplicates tag mrun, g(t)
	duplicates drop mrun, force
	drop _merge 
	
* Merge with dataset of stayers so I can disentangle switcher from pure stayers
	merge 1:1 mrun using "Data/permanece_2018.dta"
	drop if _merge == 2
	drop _merge
	
* Define category (stayer, switcher, drop outs)
	gen trayectoria = 0
	replace trayectoria = 1 if (max_merge == 3 & permanece == 1)
	replace trayectoria = 2 if (max_merge == 1 & permanece == 1)
	replace trayectoria = 3 if (max_merge == 1 & permanece == 0)
	
	lab def trayectoria 1 "Permanece" 2 "Cambiarse" 3 "Desertar" 
	lab val trayectoria trayectoria
	
* Save dataset for analysis
	keep codigo_unico mrun year trayectoria max_merge
	
	compress
	save "Data/trayectoria_2018.dta", replace
	
