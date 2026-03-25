
********************************************************************************
********************************************************************************

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

local round = 1
foreach file1 in "data_ido_sdprV23_1989_2017_bcea_cases_20230706" ///
					"data_ido_sdprV23_2018_2019_bcea_cases_20250410" ///
					"data_ido_sdprV23_2020_bcea_cases_20250410" ///
					"data_ido_sdprV23_2021_bcea_cases_20250410" {
						
	local file2 = subinstr("`file1'","cases","involvement",.)
	
	* Get pay variable by fileid *
	
	do "U:\Code\01_Data_fundamentals\\`file1'.do"

	keep ym fileid pay

	gen year = substr(ym,1,4)
	gen month = substr(ym,5,2)
	drop ym
	destring year month, replace

	destring pay, replace
	gen ia = (pay > 0) if pay != .

	tempfile t2
	save "`t2'", replace
	
	* Link fileid to studyid *

	do "U:\Code\01_Data_fundamentals\\`file2'.do"

	keep studyid ym fileid

	gen year = substr(ym,1,4)
	gen month = substr(ym,5,2)
	drop ym
	destring year month, replace

	keep studyid year month fileid

	merge n:1 year month fileid using "`t2'"
	drop _merge

	collapse (max) ia, by(studyid year)
	
	* Restrict to sample for siblings paper *
	
	merge n:1 studyid using "`t1'"
	keep if _merge == 3
	drop _merge
	
	* Clean up and save *
	
	if `round' == 1 {
		save "$teamfold\gaelle\data\ia_for_nas.dta", replace
	}
	else {
		append using "$teamfold\gaelle\data\ia_for_nas.dta"
		save "$teamfold\gaelle\data\ia_for_nas.dta", replace
	}

	local round = `round' + 1

}
