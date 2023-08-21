// Create an empty file to store the combined results
clear
gen str32 country = ""
gen int year = .
gen float ll = .
gen float ul = .
save data/output/bootstrap_results.dta, replace

// Assuming your data is saved in the following CSV file
local file "data/output/cos_sims_all.csv"

// Import the CSV file
import delimited "`file'", varnames(1) clear

// Convert the date
gen year_week = date(yearwk, "YMD")
format year_week %tw
gen year = year(year_week)
gen week = week(year_week)

// Loop through each country
levelsof country, local(countries)
foreach c of local countries {
    preserve
    
    // Keep only data for the current country
    keep if country == "`c'"
	replace cos_sim = "" if cos_sim == "NA"
    summarize cos_sim, meanonly

    // Create the rescaled price
	destring cos_sim, replace
	sum cos_sim, meanonly
    gen cos_sim_rs = (cos_sim - r(min)) / (r(max) - r(min)) * 3

    // Perform the bootstrap CI
    drop if year < 2008

    levelsof year, local(year)
    tempname tempmat 
    gen ll = .
    gen ul = .
	foreach y of local year {
		di "Processing year `y'"
		bootstrap _b, reps(1000) bca: mean cos_sim_rs if year == `y'
		estat bootstrap
		matrix `tempmat' = e(ci_bca) 
		matrix list `tempmat' 
		replace ll = `tempmat'[1, 1] if year == `y'
		replace ul = `tempmat'[2, 1] if year == `y'
	}


    bysort year: keep if _n == 1
    keep year ll ul
	// Generate a country variable with the current country name
    gen str32 country_var = "`c'"

    // Save the results for this country to a temporary file
    tempfile tempresults
    save "`tempresults'", replace

    // Load the main results file
    use data/output/bootstrap_results.dta, clear

    // Append the new results
    append using "`tempresults'"

    // Save the combined results
    save data/output/bootstrap_results.dta, replace

    
    restore
}

// Load the combined results
use data/output/bootstrap_results.dta, clear
drop country
rename country_var country

// Save the combined results to a file
export delimited "data/output/bootstrap_results.csv", replace


** Repeat for pretrained

// Create an empty file to store the combined results
clear
gen str32 country = ""
gen int year = .
gen float ll = .
gen float ul = .
save data/output/bootstrap_results_pretrained.dta, replace

// Assuming your data is saved in the following CSV file
local file "data/output/cos_sims_all_pretrained.csv"

// Import the CSV file
import delimited "`file'", varnames(1) clear

// Convert the date
gen year_week = date(yearwk, "YMD")
format year_week %tw
gen year = year(year_week)
gen week = week(year_week)

// Loop through each country
levelsof country, local(countries)
foreach c of local countries {
    preserve
    
    // Keep only data for the current country
    keep if country == "`c'"
	replace cos_sim = "" if cos_sim == "NA"
    summarize cos_sim, meanonly

    // Create the rescaled price
	destring cos_sim, replace
	sum cos_sim, meanonly
    gen cos_sim_rs = (cos_sim - r(min)) / (r(max) - r(min)) * 3

    // Perform the bootstrap CI
    drop if year < 2008

    levelsof year, local(year)
    tempname tempmat 
    gen ll = .
    gen ul = .
	foreach y of local year {
		di "Processing year `y'"
		bootstrap _b, reps(1000) bca: mean cos_sim_rs if year == `y'
		estat bootstrap
		matrix `tempmat' = e(ci_bca) 
		matrix list `tempmat' 
		replace ll = `tempmat'[1, 1] if year == `y'
		replace ul = `tempmat'[2, 1] if year == `y'
	}


    bysort year: keep if _n == 1
    keep year ll ul
	// Generate a country variable with the current country name
    gen str32 country_var = "`c'"

    // Save the results for this country to a temporary file
    tempfile tempresults
    save "`tempresults'", replace

    // Load the main results file
    use data/output/bootstrap_results_pretrained.dta, clear

    // Append the new results
    append using "`tempresults'"

    // Save the combined results
    save data/output/bootstrap_results_pretrained.dta, replace

    
    restore
}

// Load the combined results
use data/output/bootstrap_results_pretrained.dta, clear
drop country
rename country_var country

// Save the combined results to a file
export delimited "data/output/bootstrap_results_pretrained.csv", replace
















