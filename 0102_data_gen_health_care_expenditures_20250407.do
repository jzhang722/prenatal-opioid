
********************************************************************************
********************************************************************************

*******
* DAD *
*******

* Identify people who appear in birth records *

use "$teamfold\gaelle\data\births_clean.dta", clear
keep studyid0 studyid1 studyid2

gen long id = _n
reshape long studyid, i(id) j(person)
drop id person

bysort studyid: keep if _n == 1

tempfile t1
save "`t1'", replace

********************************************************************************

* Bring in raw hospitalisation data *

use "$teamfold\gaelle\data\hospital.dta", clear

* Remove superfluous observations *

** Remove unknown studyid's **

drop if studyid == "u000000000"

** Clean duplicates **

unab allvars: _all
bysort `allvars': keep if _n == 1

* Restrict to necessary variables *

keep studyid adyear ///
		diag1 diag2 diag3 diag4 diag5 diagx1 diagx2 diagx3 diagx4 diagx5 ///
		source1 ///
		criw01 driw01 ///
		/*cmg*/ grpr_mthd cmgp_riw dpgp_riw cmgp_dpg_riw
		
* Merge hospital costs data for 1991-2021 *

rename adyear year
merge n:1 year using "$teamfold\gaelle\data\hospital_costs.dta"
drop if _merge == 2
drop _merge
		
********************************************************************************

* Restrict to sample of people who appear in birth records *

merge n:1 studyid using "`t1'"
keep if _merge == 3
drop _merge

********************************************************************************

* Restrict year range *

keep if inrange(year,1991,2021)

********************************************************************************

* Format costing variables *

destring criw01 driw01 grpr_mthd cmgp_riw dpgp_riw cmgp_dpg_riw, replace

replace cmgp_riw = criw01 if cmgp_riw == . & source1 == "A"
replace dpgp_riw = driw01 if dpgp_riw == . & source1 == "A"
drop criw01 driw01 source1

gen		riw = cmgp_riw if cmgp_riw != 0
replace riw = dpgp_riw if riw == .

gen expdamt = cmg * riw
drop grpr_mthd cmg *riw

********************************************************************************

* Clean up ICD-9/ICD-10 codes and remove prefixes from ICD9 codes *

forvalues i = 1/5 {
	rename diag`i' icd9_`i'
	replace icd9_`i' = subinstr(icd9_`i'," ","",.)
	gen icd9_first1 = substr(icd9_`i',1,1)
	replace icd9_`i' = substr(icd9_`i',2,.) if inlist(icd9_first1,"C","M","Q") == 1
	drop icd9_first1
	rename diagx`i' icd10_`i'
	replace icd10_`i' = subinstr(icd10_`i'," ","",.)
}

********************************************************************************

* Generate different versions of ICD-9/ICD-10 codes based on number of digits *

forvalues i = 1/5 {
	replace icd9_`i' = subinstr(icd9_`i'," ","",.)
	forvalues d = 3/5 {
		gen icd9_`i'_`d'd = substr(icd9_`i',1,`d')
	}
	drop icd9_`i'
}

forvalues i = 1/5 {
	replace icd10_`i' = subinstr(icd10_`i'," ","",.)
	forvalues d = 3/5 {
		gen icd10_`i'_`d'd = substr(icd10_`i',1,`d')
	}
	drop icd10_`i'
}

********************************************************************************

* Remove child birth *
	/* From both mother and newborn records. */

gen birth = 0
forvalues i = 1/5 {
	replace birth = 1 if inlist(icd9_`i'_3d,"V27","V30","V31","V32","V33")
	replace birth = 1 if inlist(icd9_`i'_3d,"V34","V35","V36","V37","V39")
	replace birth = 1 if inrange(icd9_`i'_3d,"650","669")
	
	replace birth = 1 if inlist(icd10_`i'_3d,"Z37","Z38")
	replace birth = 1 if inrange(icd10_`i'_3d,"O60","O77")
	replace birth = 1 if inrange(icd10_`i'_3d,"O80","O82")
}
drop if birth == 1
drop birth

********************************************************************************

* Ignore diagnoses related to drug use (right-hand side variables) *

local drug_use_icd9 ///
	3055 9650 9701 965 967 970 E9350 E9351 E9352 E9401 E850 E852 E855 /*304*/

local drug_use_icd10 ///
	F11 F13 F14 F15 F16 T39 T40 T41 T436

forvalues i = 1/5 {
	forvalues d = 3/5 {
		gen		icd9_`i'_`d'd_nodrug = icd9_`i'_`d'd
		replace icd9_`i'_`d'd_nodrug = "" if icd9_`i'_`d'd == "`code'"
	}
	replace icd9_`i'_3d_nodrug = "" if icd9_`i'_3d == "304" & icd9_`i'_4d != "3043"
}

forvalues i = 1/5 {
	forvalues d = 3/5 {
		gen		icd10_`i'_`d'd_nodrug = icd10_`i'_`d'd
		replace icd10_`i'_`d'd_nodrug = "" if icd10_`i'_`d'd == "`code'"
	}
}

********************************************************************************

* Generate expenditure variables FOR PARENTS *
	
** Total health EXCLUDING DRUG EXPENDITURES **

forvalues i = 1/5 {
	gen 	total_`i'_nodrug = 0
	replace total_`i'_nodrug = 1 if icd9_`i'_3d_nodrug  != ""
	replace total_`i'_nodrug = 1 if icd10_`i'_3d_nodrug != ""
}

egen total_nodrug = rowmax(total_*_nodrug)
replace total_nodrug = total_nodrug * expdamt
drop total_*_nodrug

** Mental health EXCLUDING DRUG EXPENDITURES **

forvalues i = 1/5 {
	gen 	mental_`i'_nodrug = 0
	replace mental_`i'_nodrug = 1 if icd9_`i'_3d_nodrug  >= "290" & icd9_`i'_3d_nodrug  <= "319"
	replace mental_`i'_nodrug = 1 if icd10_`i'_3d_nodrug >= "F00" & icd10_`i'_3d_nodrug <= "F99"
}

egen mental_nodrug = rowmax(mental_*_nodrug)
replace mental_nodrug = mental_nodrug * expdamt
drop mental_*_nodrug

** Physical (non-mental) health EXCLUDING DRUG EXPENDITURES **

forvalues i = 1/5 {
	gen 	physical_`i'_nodrug = 0
    replace physical_`i'_nodrug = 1 if (icd9_`i'_3d_nodrug  < "290" | icd9_`i'_3d_nodrug  > "319") & icd9_`i'_3d_nodrug != ""
	replace physical_`i'_nodrug = 1 if (icd10_`i'_3d_nodrug < "F00" | icd10_`i'_3d_nodrug > "F99") & icd10_`i'_3d_nodrug != ""
}

egen physical_nodrug = rowmax(physical_*_nodrug)
replace physical_nodrug = physical_nodrug * expdamt
drop physical_*_nodrug

********************************************************************************

* Generate expenditure variables FOR CHILDREN *

** Total health **

gen total = expdamt

** Mental health **
	
forvalues i = 1/5 {
	gen 	mental_`i'_wdrug = 0
	replace mental_`i'_wdrug = 1 if icd9_`i'_3d  >= "290" & icd9_`i'_3d  <= "319"
	replace mental_`i'_wdrug = 1 if icd10_`i'_3d >= "F00" & icd10_`i'_3d <= "F99"
}

egen mental = rowmax(mental_*_wdrug)
replace mental = mental * expdamt
drop mental_*_wdrug

** Injuries (and poisoning) **

forvalues i = 1/5 {
	gen 	injury_`i' = 0
	replace injury_`i' = 1 if icd9_`i'_3d  >= "800" & icd9_`i'_3d  <= "999"
	replace injury_`i' = 1 if icd10_`i'_3d >= "S00" & icd10_`i'_3d <= "T98"
}

egen injury = rowmax(injury_*)
replace injury = injury * expdamt
drop injury_*

********************************************************************************

** Clean up and save **

compress

collapse ///
	(sum) exp_total_nodrug_dad = total_nodrug exp_mental_nodrug_dad = mental_nodrug exp_physical_nodrug_dad = physical_nodrug ///
	(sum) exp_total_dad = total exp_mental_dad = mental exp_injury_dad = injury ///
	, by(studyid year)

save "$teamfold\gaelle\data\dad_expenditures_for_nas.dta", replace

********************************************************************************
********************************************************************************

*******
* MSP *
*******

* Identify people who appear in birth records *

use "$teamfold\gaelle\data\births_clean.dta", clear
keep studyid0 studyid1 studyid2

gen long id = _n
reshape long studyid, i(id) j(person)
drop id person

bysort studyid: keep if _n == 1

tempfile t1
save "`t1'", replace

********************************************************************************

forvalues year1 = 1990/2021 {

	local year2 = `year1' + 1
	local yr2 = substr("`year2'",3,2)
	
	* Bring in data for calendar year *

	use "R:\working\gaelle\data\msp_`year1'_`yr2'.dta", clear
	gen year = year(servdate)
	drop servdate
	
	********************************************************************************

	** Remove superfluous variables **
	
	drop pracnum servcode feeitem servloc clmtype
	
	********************************************************************************

	** Identify physicians who see people who appear in birth records **
	
	merge n:1 studyid using "`t1'"
	keep if _merge == 3
	drop _merge
	
	********************************************************************************

	** Remove non-physicians (change in coverage over time) **

	gen		physician_indicator = 1
	replace physician_indicator = 0 if inlist(spec,21,25,30,31,32,34,35,38,39,43,68,73)
	replace physician_indicator = 0 if inrange(spec,79,97)
	replace physician_indicator = 0 if spec == 99
		
	keep if physician_indicator == 1	
	drop physician_indicator spec
	
	********************************************************************************

	* Generate different versions of ICD-9 codes based on number of digits *

	forvalues i = 1/5 {
		replace icd9_`i' = subinstr(icd9_`i'," ","",.)
		forvalues d = 3/5 {
			gen icd9_`i'_`d'd = substr(icd9_`i',1,`d')
		}
		drop icd9_`i'
	}
	
	********************************************************************************

	* Remove child birth *
		/* From both mother and newborn records. */

	gen birth = 0
	forvalues i = 1/5 {
		replace birth = 1 if inlist(icd9_`i'_3d,"V27","V30","V31","V32","V33")
		replace birth = 1 if inlist(icd9_`i'_3d,"V34","V35","V36","V37","V39")
		replace birth = 1 if inrange(icd9_`i'_3d,"650","669")
	}
	drop if birth == 1
	drop birth
	
	********************************************************************************

	* Identify MSP-only codes *

	local icd9_msp ///
			"A250" "A414" "A428" "A430" "A491" "A585" ///
			"C491" "C573" "C585" ///
			"D430" "D491" "D573" "D585" ///
			"E01" "E02" "E03" "E04" "E05" "E06" "E07" "E09" "E10" ///
			"E92" "E93" "E94" "E95" "E96" "E97" "E98" "E99" ///
			"H250" "H430" "H491" "H573" "H585" "I250" ///
			"I428" ///
			"I430" ///
			"I491" ///
			"I573" ///
			"I585" ///
			"K573" ///
			"N250" "N414" "N428" "N430" "N519" "N573" "N585" ///
			"R250" "R414" "R428" "R430" ///
			"R491" ///
			"R573" "R585" ///
			"01A" ///
			"01B" "01E" "01F" ///
			"01L" "01X" ///
			"02A" ///
			"02B" ///
			"03A" ///
			"03B" ///
			"04A" ///
			"05A" "06A" "06B" ///
			"07A" ///
			"08A" ///
			"08B" ///
			"10A" ///
			"10B" ///
			"11B" ///
			"12A" ///
			"12B" "15B" "16B" "17B" "18B" "19B" "20B" ///
			"21B" ///
			"22B" ///
			"23B" ///
			"30B" ///
			"31A" ///
			"31B" ///
			"32B" ///
			"33B" ///
			"34A" ///
			"34B" ///
			"35A" ///
			"35B" ///
			"36A" ///
			"36B" "37B" "38B" ///
			"42A" "43A" "44A" ///
			"45A" ///
			"50B" ///
			"55B" "60B" "65B" "66B" ///
			"78C" ///
			"E91" "01H" "01Z" "11A" "32A" "33A"
	
	forvalues i = 1/5 {
		gen msp_only_`i' = 0
		foreach code of local icd9_msp {
			replace msp_only_`i' = 1 if icd9_`i'_3d == "`code'" | icd9_`i'_4d == "`code'"
		}
	}
		
	********************************************************************************

	* Ignore diagnoses related to drug use (right-hand side variables) *

	local drug_use_icd9 ///
		3055 9650 9701 965 967 970 E9350 E9351 E9352 E9401 E850 E852 E855 /*304*/

	forvalues i = 1/5 {
		forvalues d = 3/5 {
			gen		icd9_`i'_`d'd_nodrug = icd9_`i'_`d'd
			replace icd9_`i'_`d'd_nodrug = "" if msp_only_`i' == 0 & icd9_`i'_`d'd == "`code'"
		}
		replace icd9_`i'_3d_nodrug = "" if msp_only_`i' == 0 & icd9_`i'_3d == "304" & icd9_`i'_4d != "3043"
	}

	********************************************************************************

	* Generate expenditure variables *
		
	** Total health EXCLUDING DRUG EXPENDITURES **

	forvalues i = 1/5 {
		gen 	total_`i'_nodrug = 0
		replace total_`i'_nodrug = 1 if icd9_`i'_3d_nodrug  != ""
	}

	egen total_nodrug = rowmax(total_*_nodrug)
	replace total_nodrug = total_nodrug * expdamt
	drop total_*_nodrug

	** Mental health EXCLUDING DRUG EXPENDITURES **

	forvalues i = 1/5 {
		gen 	mental_`i'_nodrug = 0
		replace mental_`i'_nodrug = 1 if msp_only_`i' == 0 & icd9_`i'_3d_nodrug  >= "290" & icd9_`i'_3d_nodrug  <= "319"
		replace mental_`i'_nodrug = 1 if msp_only_`i' == 1 & icd9_`i'_3d_nodrug == "04A"
		replace mental_`i'_nodrug = 1 if msp_only_`i' == 1 & icd9_`i'_3d_nodrug == "50B"
	}

	egen mental_nodrug = rowmax(mental_*_nodrug)
	replace mental_nodrug = mental_nodrug * expdamt
	drop mental_*_nodrug

	** Physical (non-mental) health EXCLUDING DRUG EXPENDITURES **

	forvalues i = 1/5 {
		gen		physical_`i'_nodrug = 0
		replace physical_`i'_nodrug = 1 if msp_only_`i' == 0 & (icd9_`i'_3d_nodrug  < "290" | icd9_`i'_3d_nodrug  > "319") & icd9_`i'_3d_nodrug != ""
		replace physical_`i'_nodrug = 1 if msp_only_`i' == 1 & icd9_`i'_3d_nodrug != "04A" & icd9_`i'_3d_nodrug != "50B"
	}

	egen physical_nodrug = rowmax(physical_*_nodrug)
	replace physical_nodrug = physical_nodrug * expdamt
	drop physical_*_nodrug

	********************************************************************************

	* Generate expenditure variables FOR CHILDREN *

	** Total health **

	gen total = expdamt

	** Mental health **
		
	forvalues i = 1/5 {
		gen 	mental_`i'_wdrug = 0
		replace mental_`i'_wdrug = 1 if msp_only_`i' == 0 & icd9_`i'_3d >= "290" & icd9_`i'_3d <= "319"
		replace mental_`i'_wdrug = 1 if msp_only_`i' == 1 & icd9_`i'_3d == "04A"
		replace mental_`i'_wdrug = 1 if msp_only_`i' == 1 & icd9_`i'_3d == "50B"
	}

	egen mental = rowmax(mental_*_wdrug)
	replace mental = mental * expdamt
	drop mental_*_wdrug

	** Injuries (and poisoning) **

	forvalues i = 1/5 {
		gen		injury_`i' = 0
		replace injury_`i' = 1 if msp_only_`i' == 0 & icd9_`i'_3d  >= "800" & icd9_`i'_3d  <= "999"
		replace injury_`i' = 1 if msp_only_`i' == 1 & inlist(icd9_`i'_3d,"10A","55B","60B","65B","66B")
	}

	egen injury = rowmax(injury_*)
	replace injury = injury * expdamt
	drop injury_*

	********************************************************************************

	** Clean up and save **
	
	compress
	
	collapse ///
		(sum) exp_total_nodrug_msp = total_nodrug exp_mental_nodrug_msp = mental_nodrug exp_physical_nodrug_msp = physical_nodrug ///
		(sum) exp_total_msp = total exp_mental_msp = mental exp_injury_msp = injury ///
		, by(studyid year)
	
	save "$teamfold\gaelle\data\msp_expenditures_for_nas_`year1'_`yr2'.dta", replace

}

********************************************************************************

local round = 1
forvalues year1 = 1990/2021 {

	local year2 = `year1' + 1
	local yr2 = substr("`year2'",3,2)
	
	use "$teamfold\gaelle\data\msp_expenditures_for_nas_`year1'_`yr2'.dta", clear
	
	if `round' == 1 {
		save "$teamfold\gaelle\data\msp_expenditures_for_nas.dta", replace
	}
	else {
	    append using "$teamfold\gaelle\data\msp_expenditures_for_nas.dta"
		save "$teamfold\gaelle\data\msp_expenditures_for_nas.dta", replace
	}
	
	erase "$teamfold\gaelle\data\msp_expenditures_for_nas_`year1'_`yr2'.dta"

	local round = `round' + 1
}

use "$teamfold\gaelle\data\msp_expenditures_for_nas.dta", clear

keep if inrange(year,1991,2021)

collapse (sum) exp_total_nodrug_msp exp_mental_nodrug_msp exp_physical_nodrug_msp ///
				exp_total_msp exp_mental_msp exp_injury_msp ///
	, by(studyid year)

save "$teamfold\gaelle\data\msp_expenditures_for_nas.dta", replace
