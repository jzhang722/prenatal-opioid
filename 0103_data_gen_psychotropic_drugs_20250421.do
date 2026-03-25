
********************************************************************************
********************************************************************************

* Identify psychotropic drugs *

** Bring in information by DIN **

infix ///
	str	din_pin				1-10	///
	str	drug_brand_nm		11-74	///
	str	gen_drug 			75-114	///
	str	gcn_seq_num			115-134	///
	str	gen_drug_strgth_val	135-144	///
	str	unit_of_msr			145-159	///
	str	gen_dsg_form_cd		160-161	///
	str	gen_dsg_form		162-181	///
	str	ahfs_3_cd			182-191	///
	str	tc5_cd				192-211	///
	using "R:\working\unzipped_raw_data\pharmanet_health-products.A.dat", clear

gen has_code = (ahfs_3_cd != "")

gen antidepressant = (substr(ahfs_3_cd,1,6) == "281604")
gen benzo = (substr(ahfs_3_cd,1,6) == "281208" | substr(ahfs_3_cd,1,6) == "282408")
gen anxiolytic = (substr(ahfs_3_cd,1,6) == "282492")
gen barbiturate = (inlist(substr(ahfs_3_cd,1,6),"280404","281204","282404"))
gen antimanic = (substr(ahfs_3_cd,1,4) == "2828")
gen anticonvulsant = (substr(ahfs_3_cd,1,4) == "2812")
gen antipsychotic = (substr(ahfs_3_cd,1,6) == "281608")

** Clean up and save **

egen psych_drug = rowmax(antidepressant benzo anxiolytic barbiturate antimanic anticonvulsant antipsychotic)

keep din_pin has_code psych_drug
destring din_pin, replace force
drop if din_pin == .

save "$teamfold\gaelle\data\psychotropic_drugs.dta", replace

********************************************************************************
********************************************************************************

* Identify people who appear in birth records *

use "$teamfold\gaelle\data\births_clean.dta", clear
keep studyid0 studyid1 studyid2 year

rename year birthyear
keep if birthyear >= 1991

bysort studyid0: keep if _n == 1

gen long id = _n

expand (18 - -1 + 1)
bysort studyid0: gen child_age = _n - 2
gen year = birthyear + child_age
keep if inrange(year,1990,2021)
drop birthyear

reshape long studyid, i(id year) j(person)

keep studyid year
order studyid year
bysort studyid year: keep if _n == 1

save "$teamfold\gaelle\data\nas_sample.dta", replace

********************************************************************************
********************************************************************************

global yrrge
foreach yrrge in 1996_1998 1999_2001 2002_2004 2005_2007 2008_2009 ///
					2010_2011 2012_2013 2014 2015 2016 2017 2018 2019 2020 2021 {
						
	disp("`yrrge'")
						
	global yrrge `yrrge'			    
	do "U:\Code\01_Data_fundamentals\data_pharmanet_claims_20230628.do"
	
    keep studyid srv_date hlth_prod_label dspd_days_sply ///
			bld_prod_cost bld_prof_fee
			
	gen year = substr(srv_date,1,4)
	gen month = substr(srv_date,6,2)
	gen day = substr(srv_date,9,2)
	drop srv_date
	destring year month day, replace
	
	rename hlth_prod_label din_pin
	rename dspd_days_sply days_supply
	destring din_pin days_supply bld_prod_cost bld_prof_fee, replace
	
	egen drug_cost = rowtotal(bld_prod_cost bld_prof_fee), miss
	drop bld_prod_cost bld_prof_fee
	
	merge n:1 studyid year using "$teamfold\gaelle\data\nas_sample.dta"
	keep if _merge == 3
	drop _merge
	
	merge n:1 din_pin using "$teamfold\gaelle\data\psychotropic_drugs.dta"
	drop if _merge == 2
	
	gen merged = (_merge == 3)
	drop _merge
		
	tab merged
	tab has_code if merged == 1
	
	keep if psych_drug == 1

	collapse (sum) drug_cost days_supply, by(studyid year)

	compress *

	save "$teamfold\gaelle\data\pharmaceuticals_for_nas_`yrrge'.dta", replace
		
}

local round = 1
foreach yrrge in 1996_1998 1999_2001 2002_2004 2005_2007 2008_2009 ///
					2010_2011 2012_2013 2014 2015 2016 2017 2018 2019 2020 2021 {
	
	if `round' == 1	{
	    use "$teamfold\gaelle\data\pharmaceuticals_for_nas_`yrrge'.dta", clear
		save "$teamfold\gaelle\data\pharmaceuticals_for_nas.dta", replace
	}
	else {
	    append using "$teamfold\gaelle\data\pharmaceuticals_for_nas_`yrrge'.dta"
		save "$teamfold\gaelle\data\pharmaceuticals_for_nas.dta", replace		
	}
	
	erase "$workdata\Temp\sn_outcomes_pharm_${fm}_`yrrge'.dta"
	
	local round = `round' + 1
	
}

erase "$teamfold\gaelle\data\nas_sample.dta"
