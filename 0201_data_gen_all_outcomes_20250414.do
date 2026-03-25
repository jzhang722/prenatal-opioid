
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
keep if inrange(year,1991,2021)
drop birthyear

reshape long studyid, i(id year) j(person)

gen child = (person == 0)
gen parent = (person == 1 | person == 2)
drop id person

order studyid year child_age child parent
drop child_age

bysort studyid year: keep if _n == 1

* Add health care outcomes *

merge 1:1 studyid year using "$teamfold\gaelle\data\dad_expenditures_for_nas.dta"
drop if _merge == 2
drop _merge

merge 1:1 studyid year using "$teamfold\gaelle\data\msp_expenditures_for_nas.dta"
drop if _merge == 2
drop _merge

foreach var in exp_total_nodrug exp_mental_nodrug exp_physical_nodrug exp_total exp_mental exp_injury exp_disability {
	egen `var' = rowtotal(`var'_dad `var'_msp), miss
	replace `var' = 0 if `var' == .
	drop `var'_dad `var'_msp
}

* Add IA *

merge 1:1 studyid year using "$teamfold\gaelle\data\ia_for_nas.dta"
drop if _merge == 2
drop _merge

replace ia = 0 if ia == .

* Add parents together *

merge 1:1 studyid year using "$teamfold\gaelle\data\parents_together.dta"
drop if _merge == 2
drop _merge

* Add crime *

merge 1:1 studyid year using "$teamfold\gaelle\data\pssg_outcomes.dta", keepusing(charged)
drop if _merge == 2
drop _merge

replace charged = 0 if charged == . & year >= 2001

* Add psychotropic drugs *

merge 1:1 studyid year using "$teamfold\gaelle\data\pharmaceuticals_for_nas.dta"
drop if _merge == 2
drop _merge

replace drug_cost = 0 if drug_cost == . & year >= 1996
replace days_supply = 0 if days_supply == . & year >= 1996

* Save as csv *

save "$teamfold\gaelle\data\siblings_additional_outcomes.dta", replace
export delimited using "R:\working\gaelle\data\siblings_additional_outcomes.csv", replace
