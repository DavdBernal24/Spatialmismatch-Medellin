// ======================================================================
// Project: Better or worse job accessibility? Understanding changes in 
//          spatial mismatch at the intra-urban level: evidence from Medellín, Colombia
// Description: This script analyzes job accessibility by age groups and produces Figure B6 
//              across different transport modes in Medellín.
// Author: []
// Date: []
// ======================================================================

clear
set more off

// ----------------------------------------------------------------------
// 1. Setting the Working Directory
// ----------------------------------------------------------------------



global project    "C:\Github_Folders\Spatial_Mismatch_Rep"
global datadir    "$project/Base"
global figdir     "$project/Output/Figures"

cd "$datadir"

// ----------------------------------------------------------------------
// 2. Data Import and Cleaning
// ----------------------------------------------------------------------

import delimited "$datadir/Heterogeneity.csv", encoding(utf8) varnames(1) clear

// Rename age group variables for clarity
rename page_0_5 age_0_5_p
rename page_6_10 age_6_10_p
rename page_11_15 age_11_15_p
rename page_16_17 age_16_17_p
rename page_18_24 age_18_24_p
rename page_25_40 age_25_40_p
rename page_41_65 age_41_65_p
rename page_65m age_65plus_p

// Create variables to store `percap` values for 2012 and 2017
gen percap_2012 = .
gen percap_2017 = .

// Assign `percap` values based on the year
replace percap_2012 = percap if year == 2012
replace percap_2017 = percap if year == 2017

// Ensure that each analysis unit has values for both 2012 and 2017
bysort mode (year): replace percap_2012 = percap_2012[_n-1] if missing(percap_2012) & year == 2017
bysort mode (year): replace percap_2017 = percap_2017[_n+1] if missing(percap_2017) & year == 2012

// Calculate the change in `percap`
gen change_percap = percap_2017 - percap_2012

// Convert `mode` into a numeric format
encode mode, generate(mode_num)
gen mode_x = mode_num  // Numeric copy for x-axis



// ----------------------------------------------------------------------
// 3. Accessibility Analysis by Age Groups: Private transport 
// ----------------------------------------------------------------------


binsreg percap age_18_24_p if mode_num == 1, by(year) polyreg(1) savedata(test) replace

preserve
use test, clear

twoway ///
    (scatter dots_fit dots_x if year == 2012, msize(medium) mcolor(navy) msymbol(O)) ///
    (line poly_fit poly_x if year == 2012, lcolor(navy)) ///
    (scatter dots_fit dots_x if year == 2017, msize(large) mcolor(navy) msymbol(+)) ///
    (line poly_fit poly_x if year == 2017, lcolor(navy) lpattern(dash)), ///
    legend(order(1 3) label(1 "2012") label(3 "2017") size(medium)) ///
    title("a) Private transport for share of population (18–24 years)", size(large)) ///
    xtitle("Share of population (18–24 years)", size(large)) ///
    ytitle("Accessibility", size(large)) ///
    graphregion(margin(zero)) ///
    ysize(5) xsize(7)
	
graph export "$figdir/Figure_B6_panel_a_age_18_24.pdf", replace

restore


binsreg percap age_25_40_p if mode_num == 1, by(year) polyreg(1) savedata(test_d) replace

preserve
use test_d, clear

twoway ///
    (scatter dots_fit dots_x if year == 2012, msize(medium) mcolor(navy) msymbol(O)) ///
    (line poly_fit poly_x if year == 2012, lcolor(navy)) ///
    (scatter dots_fit dots_x if year == 2017, msize(large) mcolor(navy) msymbol(+)) ///
    (line poly_fit poly_x if year == 2017, lcolor(navy) lpattern(dash)), ///
    legend(order(1 3) label(1 "2012") label(3 "2017") size(medium)) ///
    title("d) Private transport for share of population (25–40 years)", size(large)) ///
    xtitle("Share of population (25–40 years)", size(large)) ///
    ytitle("Accessibility", size(large)) ///
    xlabel(, labsize(medium)) ///
    ylabel(, labsize(medium)) ///
    graphregion(margin(zero)) ///
    ysize(5) xsize(7)

graph export "$figdir/Figure_B6_panel_a_age_25_40.pdf", replace

restore


binsreg percap age_41_65_p if mode_num == 1, by(year) polyreg(1) savedata(test_g) replace

preserve
use test_g, clear

twoway ///
    (scatter dots_fit dots_x if year == 2012, msize(medium) mcolor(navy) msymbol(O)) ///
    (line poly_fit poly_x if year == 2012, lcolor(navy)) ///
    (scatter dots_fit dots_x if year == 2017, msize(large) mcolor(navy) msymbol(+)) ///
    (line poly_fit poly_x if year == 2017, lcolor(navy) lpattern(dash)), ///
    legend(order(1 3) label(1 "2012") label(3 "2017") size(medium)) ///
    title("g) Private transport for share of population (41–65 years)", size(large)) ///
    xtitle("Share of population (41–65 years)", size(large)) ///
    ytitle("Accessibility", size(large)) ///
    xlabel(, labsize(medium)) ///
    ylabel(, labsize(medium)) ///
    graphregion(margin(zero)) ///
    ysize(5) xsize(7)

graph export "$figdir/Figure_B6_panel_a_age_41_65.pdf", replace

restore

// ----------------------------------------------------------------------
// 4. Accessibility Analysis by Age Groups: Public transport 
// ----------------------------------------------------------------------

 binsreg percap age_18_24_p if mode_num == 2, by(year) polyreg(1) savedata(test_b) replace

preserve
use test_b, clear

twoway ///
    (scatter dots_fit dots_x if year == 2012, msize(large) mcolor(maroon) msymbol(O)) ///
    (line poly_fit poly_x if year == 2012, lcolor(maroon)) ///
    (scatter dots_fit dots_x if year == 2017, msize(large) mcolor(maroon) msymbol(X)) ///
    (line poly_fit poly_x if year == 2017, lcolor(maroon) lpattern(dash)), ///
    legend(order(1 3) label(1 "2012") label(3 "2017") size(medium)) ///
    title("b) Public transport for share of population (18–24 years)", size(large)) ///
    xtitle("Share of population (18–24 years)", size(large)) ///
    ytitle("Accessibility", size(large)) ///
    xlabel(, labsize(medium)) ///
    ylabel(, labsize(medium)) ///
    graphregion(margin(zero)) ///
    ysize(5) xsize(7)


graph export "$figdir/Figure_B6_panel_b_age_18_24.pdf", replace
restore

* 25–40 años
binsreg percap age_25_40_p if mode_num == 2, by(year) polyreg(1) savedata(test_25_40) replace
preserve
use test_25_40, clear

twoway ///
    (scatter dots_fit dots_x if year == 2012, msize(large) mcolor(maroon) msymbol(O)) ///
    (line poly_fit poly_x if year == 2012, lcolor(maroon)) ///
    (scatter dots_fit dots_x if year == 2017, msize(large) mcolor(maroon) msymbol(X)) ///
    (line poly_fit poly_x if year == 2017, lcolor(maroon) lpattern(dash)), ///
    legend(order(1 3) label(1 "2012") label(3 "2017") size(medium)) ///
    title("b) Public transport for share of population (25–40 years)", size(large)) ///
    xtitle("Share of population (25–40 years)", size(large)) ///
    ytitle("Accessibility", size(large)) ///
    xlabel(, labsize(medium)) ///
    ylabel(, labsize(medium)) ///
    graphregion(margin(zero)) ///
    ysize(5) xsize(7)

graph export "$figdir/Figure_B6_panel_b_age_25_40.pdf", replace
restore

* 41–65 años
binsreg percap age_41_65_p if mode_num == 2, by(year) polyreg(1) savedata(test_41_65) replace
preserve
use test_41_65, clear

twoway ///
    (scatter dots_fit dots_x if year == 2012, msize(large) mcolor(maroon) msymbol(O)) ///
    (line poly_fit poly_x if year == 2012, lcolor(maroon)) ///
    (scatter dots_fit dots_x if year == 2017, msize(large) mcolor(maroon) msymbol(X)) ///
    (line poly_fit poly_x if year == 2017, lcolor(maroon) lpattern(dash)), ///
    legend(order(1 3) label(1 "2012") label(3 "2017") size(medium)) ///
    title("b) Public transport for share of population (41–65 years)", size(large)) ///
    xtitle("Share of population (41–65 years)", size(large)) ///
    ytitle("Accessibility", size(large)) ///
    xlabel(, labsize(medium)) ///
    ylabel(, labsize(medium)) ///
    graphregion(margin(zero)) ///
    ysize(5) xsize(7)

graph export "$figdir/Figure_B6_panel_b_age_41_65.pdf", replace
restore


// ----------------------------------------------------------------------
// 5. Accessibility Analysis by Age Groups: Change in accessibility
// ----------------------------------------------------------------------

binsreg change_percap age_18_24_p, by(mode_x) polyreg(1) savedata(panel_c) replace

preserve
use panel_c, clear

twoway ///
    (scatter dots_fit dots_x if mode_x == 1, msymbol(O) mcolor(navy) msize(medium)) ///
    (line poly_fit poly_x if mode_x == 1, lcolor(navy)) ///
    (scatter dots_fit dots_x if mode_x == 2, msymbol(D) mcolor(maroon) msize(medium)) ///
    (line poly_fit poly_x if mode_x == 2, lcolor(maroon) lpattern(solid)), ///
    legend(order(1 3) label(1 "Private") label(3 "Public") size(medium)) ///
    title("c) Change in accessibility for population (18–24 years by mode", size(large)) ///
    xtitle("Share of population (18–24 years)", size(large)) ///
    ytitle("Change in accessibility", size(large)) ///
    xlabel(, labsize(medium)) ylabel(, labsize(medium)) ///
    graphregion(margin(zero)) ///
    ysize(5) xsize(7)

graph export "$figdir/Figure_B6_panel_c_age_18_24.pdf", replace
restore



binsreg change_percap age_25_40_p, by(mode_x) polyreg(1) savedata(panel_f) replace

preserve
use panel_f, clear

twoway ///
    (scatter dots_fit dots_x if mode_x == 1, msymbol(O) mcolor(navy) msize(medium)) ///
    (line poly_fit poly_x if mode_x == 1, lcolor(navy)) ///
    (scatter dots_fit dots_x if mode_x == 2, msymbol(D) mcolor(maroon) msize(medium)) ///
    (line poly_fit poly_x if mode_x == 2, lcolor(maroon) lpattern(solid)), ///
    legend(order(1 3) label(1 "Private") label(3 "Public") size(medium)) ///
    title("f) Change in accessibility for population (25–40 years) by mode", size(large)) ///
    xtitle("Share of population (25–40 years)", size(large)) ///
    ytitle("Change in accessibility", size(large)) ///
    xlabel(, labsize(medium)) ylabel(, labsize(medium)) ///
    graphregion(margin(zero)) ///
    ysize(5) xsize(7)

graph export "$figdir/Figure_B6_panel_c_age_25_40.pdf", replace
restore


binsreg change_percap age_41_65_p, by(mode_x) polyreg(1) savedata(panel_i) replace

preserve
use panel_i, clear

twoway ///
    (scatter dots_fit dots_x if mode_x == 1, msymbol(O) mcolor(navy) msize(medium)) ///
    (line poly_fit poly_x if mode_x == 1, lcolor(navy)) ///
    (scatter dots_fit dots_x if mode_x == 2, msymbol(D) mcolor(maroon) msize(medium)) ///
    (line poly_fit poly_x if mode_x == 2, lcolor(maroon) lpattern(solid)), ///
    legend(order(1 3) label(1 "Private") label(3 "Public") size(medium)) ///
    title("i) Change in accessibility for population (41–65 years) by mode", size(large)) ///
    xtitle("Share of population (41–65 years)", size(large)) ///
    ytitle("Change in accessibility", size(large)) ///
    xlabel(, labsize(medium)) ylabel(, labsize(medium)) ///
    graphregion(margin(zero)) ///
    ysize(5) xsize(7)

graph export "$figdir/Figure_B6_panel_c_age_41_65.pdf", replace
restore


