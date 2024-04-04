/*******************************************************************************
			PROBLEM SET 1 -	ECONOMETRIC THEORY II
			M.A. ECONOMIC - PUC-CHILE			
			
Author: Jose Carlo Bermúdez
Name  : 0_master.do
Task  : Master script to run all the codes implemented in cleaning and analysis
		process. 
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
