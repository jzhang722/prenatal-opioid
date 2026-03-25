
********************************************************************************
********************************************************************************

local r = 1
forvalues year = 1986/2019 {
	
	n disp("`year'")
	
	*** Bring in data ***

	infix ///
		double 	contract_no 1-9 ///
		str		studyid		78-87 ///
		using "$teamfold\unzipped_raw_data\rpblite`year'.D.dat", clear
	
	gen year = `year'

	drop if contract_no == .
	
	*** Generate familyid ***

	merge n:1 studyid using "$teamfold\gaelle\data\dems_new.dta"
	drop if _merge == 2
	drop _merge

	gen age = year - yob
	replace age = . if age < 0

	sort contract_no
	by contract_no: egen age_oldest = max(age)

	gen child_rp = (inrange(age,0,18))
	gen adult_child_rp = (inrange(age,19,24) & age_oldest - age > 16)
	gen adult_rp = ((inrange(age,19,24) & age_oldest - age <= 16) | (age > 24 & age != .))

	*** Eliminate duplicates ***
	
	bysort studyid contract_no: keep if _n == 1

	*** Expand list to create registry of all pairs ***
	
	keep contract_no year studyid child_rp adult_child_rp adult_rp
	
	sort contract_no studyid
	by contract_no: gen double fmid = _n
	by contract_no: gen double famsize = _N

	preserve
		keep contract_no year studyid fmid child_rp adult_child_rp adult_rp
		rename * *2
		rename contract_no2 contract_no
		rename year2 year
		tempfile t1
		save "`t1'", replace
	restore
	
	rename * *1
	rename contract_no1 contract_no
	rename year1 year

	expand famsize
	bysort contract_no fmid1: gen double fmid2 = _n
	merge n:1 contract_no fmid2 using "`t1'"
	drop fmid1 famsize fmid2 _merge
	
	*** Identify parents of children born in BC ***
	
	rename year rpb_year
	
	keep studyid1 studyid2 rpb_year
	bysort studyid1 studyid2: keep if _n == 1
	merge 1:n studyid1 studyid2 using "$teamfold\gaelle\data\births_clean.dta", keepusing (studyid* year)
	drop if _merge == 1
	
	replace rpb_year = `year' if rpb_year == .
	
	drop if _merge == 2 & (rpb_year < year | rpb_year > year + 18)
	gen parents_together = (_merge == 3)
	drop _merge year
	rename rpb_year year

	*** Clean up and save ***
	
	keep studyid0 year parents_together
	rename studyid0 studyid
		
	compress

	if `r' == 1 {
		save "$teamfold\gaelle\data\parents_together.dta", replace
	}
	else {
		append using "$teamfold\gaelle\data\parents_together.dta"
		save "$teamfold\gaelle\data\parents_together.dta", replace
	}
	
	local r = `r' + 1
	
}
