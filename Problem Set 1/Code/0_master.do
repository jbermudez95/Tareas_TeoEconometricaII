/*******************************************************************************
			PROBLEM SET 1  - ECONOMETRIC THEORY II
			M.A. ECONOMICS - PUC-CHILE			
			
Author: Jose Carlo Bermúdez
Name  : 0_master.do
Task  : Master script to run all the codes implemented in cleaning and analysis. 
Date  : April, 2024
*******************************************************************************/

	clear all
	set more off
	set varabbrev off
	
	
/*******************************************************************************	
 Install required packages
*******************************************************************************/

	global packs "eststo estout ftools erepost egenmore gtools"
				
	foreach p of global packs {
		cap which `p'
		if (_rc) ssc install `p'
	}


/*******************************************************************************
Paths and working directories (CHANGE ACCORDING TO USER SPECIFICATION)
*******************************************************************************/

	if "`c(username)'" == "Jose Carlo Bermúdez" {
		cd "C:/Users/bermu/OneDrive - Universidad Católica de Chile/Clases Magíster/Teoría Econométrica II/Tareas_TeoEconometricaII/Problem Set 1"
	}
	* For Nico and Vale replicability :)
	else if "`c(username)'" == "" {												// Insert username											
		cd ""																	// Insert location of your PC where the carpet has been stored
	}
	
	
/*******************************************************************************
Running do files
*******************************************************************************/

	* 1. Cleaning raw data
	do "Code/1_cleaning.do"
	
	* 2. Analysis
	do "Code/2_analysis.do"
	
	