/*******************************************************************************
			PROBLEM SET 2  - ECONOMETRIC THEORY II
			M.A. ECONOMICS - PUC-CHILE			
			
Author: Jose Carlo Bermúdez
Name  : 0_master.do
Task  : Master script to run all the codes implemented in analysis 
Date  : May, 2024
*******************************************************************************/

	clear all
	set more off
	set varabbrev off
	set seed 123456
	
	
/*******************************************************************************	
 Install required packages
*******************************************************************************/

	global packs "eststo estout ftools erepost egenmore gtools heatplot palettes colrspace"
				
	foreach p of global packs {
		cap which `p'
		if (_rc) ssc install `p', replace
	}


/*******************************************************************************
Paths and working directories (CHANGE ACCORDING TO USER SPECIFICATION)
*******************************************************************************/

	if "`c(username)'" == "Jose Carlo Bermúdez" {
		cd "C:/Users/bermu/OneDrive - Universidad Católica de Chile/Clases Magíster/Teoría Econométrica II/Tareas_TeoEconometricaII/Problem Set 2"
	}
	* For Nico and Vale replicability :)
	else if "`c(username)'" == "" {												// Insert username											
		cd ""																	// Insert location of your PC where the carpet has been stored
	}
	
	
/*******************************************************************************
Running do files
*******************************************************************************/

	forvalues i = 1/3 {
		do "Code/`i'_problem.do", nostop
	}


