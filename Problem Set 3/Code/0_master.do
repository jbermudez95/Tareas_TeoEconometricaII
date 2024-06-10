/*******************************************************************************
			PROBLEM SET 3  - ECONOMETRIC THEORY II
			M.A. ECONOMICS - PUC-CHILE			
			
Author: Jose Carlo Bermúdez
Name  : 0_master.do
Task  : Master script to run all the codes implemented in analysis 
Date  : June, 2024
*******************************************************************************/

	clear all
	set more off
	set varabbrev off


/*******************************************************************************
Paths and working directories (CHANGE ACCORDING TO USER SPECIFICATION)
*******************************************************************************/

	if "`c(username)'" == "Jose Carlo Bermúdez" {
		cd "C:/Users/bermu/OneDrive - Universidad Católica de Chile/Clases Magíster/Teoría Econométrica II/Tareas_TeoEconometricaII/Problem Set 3"
	}
	* For Nico and Vale replicability :)
	else if "`c(username)'" == "" {												// Insert username											
		cd ""																	// Insert location of your PC where the carpet has been stored
	}
	
	
/*******************************************************************************
Running do files
*******************************************************************************/

	forvalues i = 1/2 {
		do "Code/`i'_problem.do"
	}
	