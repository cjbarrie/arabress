cap cd "/Users/cbarrie6/Dropbox/edbrgh_projects/arabress_medcrit2"
set more off
clear

*Bring in csv file created in R script "24_synth_did.R"
import delimited "data/output/cos_sims_mastun/diffindiffdata.csv", bindquote(strict) varnames(1) clear

*Create week year panel
rename newspaper source
bysort source: keep if _n==1
keep source

set obs 52

gen week = _n
gen year = 2011 + _n-1
replace year = . if year > 2019

fillin source week year
drop if missing(source)
drop if missing(year)
drop _fillin

sort source year week

save data/output/cos_sims_mastun/stata_files/source_week_year.dta, replace


*merge in source characteristics
import delimited "data/output/cos_sims_mastun/diffindiffdata.csv", bindquote(strict) varnames(1) clear
rename newspaper source
bysort source: keep if _n==1
keep source country_id

save data/output/cos_sims_mastun/stata_files/source_country, replace

use data/output/cos_sims_mastun/stata_files/source_week_year, clear
merge m:1 source using data/output/cos_sims_mastun/stata_files/source_country
drop _merge

save data/output/cos_sims_mastun/stata_files/source_week_year.dta, replace

*merge in DV
import delimited "data/output/cos_sims_mastun/diffindiffdata.csv", bindquote(strict) varnames(1) clear
rename val cos_sim
rename group seqvar
rename newspaper source
keep cos_sim seqvar source

gen int year_week = daily(seqvar, "YMD")
format year_week %tw
gen year = year(year_week)
gen week = week(year_week)

keep cos_sim source year week
sort source year week
drop if year > 2019

*some week sequence duplicates
duplicates drop source year week, force

save data/output/cos_sims_mastun/stata_files/dv_source_week_year, replace

use data/output/cos_sims_mastun/stata_files/source_week_year, clear
merge 1:1 source year week using data/output/cos_sims_mastun/stata_files/dv_source_week_year
drop if _merge == 2
drop _merge 

*clean up DV
gen dv = cos_sim
bysort source: gen running_time = _n

*code treatment
encode country_id, gen(id)
gen coup_treatment = 1 if running_time >= 131 & id == 1
recode coup_treatment (miss=0)

encode source, gen(source_id)

save data/output/cos_sims_mastun/stata_files/dv_source_week_year, replace


*merge in V-dem
import delimited "/Users/cbarrie6/Dropbox/edbrgh_projects/arabress_medcrit2/data/raw/masress/vdem_media_critical.csv", clear 

keep if year > 2010 & year < 2020
rename egypt arabress
gen country = "Egypt"

save data/output/cos_sims_mastun/stata_files/egyptvdemcrit, replace

import delimited "/Users/cbarrie6/Dropbox/edbrgh_projects/arabress_medcrit2/data/raw/turess/vdem_media_critical.csv", clear 

keep if year > 2010 & year < 2020
rename tunisia arabress
gen country = "Tunisia"

append using data/output/cos_sims_mastun/stata_files/egyptvdemcrit
encode country, gen(id)

gen coup = 1 if year >= 2013 & country == "Egypt"
recode coup (miss=0)

save data/output/cos_sims_mastun/stata_files/vdem_to_merge, replace

use data/output/cos_sims_mastun/stata_files/dv_source_week_year, clear
merge m:1 year id using data/output/cos_sims_mastun/stata_files/vdem_to_merge
drop _merge

export delimited using "data/output/cos_sims_mastun/source_week.csv", replace




