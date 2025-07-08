// ======================================================================
// Project: Better or worse job accessibility? Understanding changes in 
//          spatial mismatch at the intra-urban level: evidence from Medellín, Colombia
// Description: This script analyzes job accessibility by age groups and produces Figure B5 
//              across different transport modes in Medellín.
// Author: []
// Date: []
// ======================================================================

clear
set more off
// ----------------------------------------------------------------------
// 1. Set Project Paths (Dropbox)
// ----------------------------------------------------------------------

global project "C:/Github_Folders/Spatial_Mismatch_Rep"
global datadir "$project/Data"
global figdir "$project/Output/Figures"


// ----------------------------------------------------------------------
// 2. Import and Prepare Data
// ----------------------------------------------------------------------

import delimited "$datadir/Heterogeneity.csv", encoding(utf8) varnames(1) clear

// Rename age group share variables
rename v23 age_0_5_p
rename v24 age_6_10_p
rename v25 age_11_15_p
rename v26 age_16_17_p
rename v27 age_18_24_p
rename v28 age_25_40_p
rename v29 age_41_65_p
rename v30 age_65plus_p

// Create separate variables for 2012 and 2017 accessibility
gen percap_2012 = .
gen percap_2017 = .
replace percap_2012 = percap if year == 2012
replace percap_2017 = percap if year == 2017

// Fill missing values to ensure each unit has both years
bysort mode (year): replace percap_2012 = percap_2012[_n-1] if missing(percap_2012) & year == 2017
bysort mode (year): replace percap_2017 = percap_2017[_n+1] if missing(percap_2017) & year == 2012

// Calculate change in accessibility between 2012 and 2017
gen change_percap = percap_2017 - percap_2012

// Convert 'mode' to numeric for plotting
encode mode, generate(mode_num)
gen mode_x = mode_num  // duplicate for use with binsreg by(mode_x)

// ----------------------------------------------------------------------
// 3. Accessibility Analysis by Gender 
// ----------------------------------------------------------------------

* -------------------------------
* Panel a: Private transport — women
* -------------------------------
binsreg percap muj if mode_num == 1, by(year) polyreg(1) savedata(test_muj_a) replace

preserve
use test_muj_a, clear

twoway ///
    (scatter dots_fit dots_x if year == 2012, msymbol(O) mcolor(navy) msize(medium)) ///
    (line poly_fit poly_x if year == 2012, lcolor(navy)) ///
    (scatter dots_fit dots_x if year == 2017, msymbol(+) mcolor(navy) msize(large)) ///
    (line poly_fit poly_x if year == 2017, lcolor(navy) lpattern(dash)), ///
    legend(order(1 3) label(1 "2012") label(3 "2017")) ///
    title("a) Private transport for share of women", size(large)) ///
    xtitle("Share of women", size(large)) ///
    ytitle("Accessibility", size(large)) ///
    xlabel(, labsize(medium)) ylabel(, labsize(medium)) ///
    graphregion(margin(zero)) ysize(5) xsize(7)

graph export "$figdir/Figure_B6_panel_a_women.pdf", replace
restore


* -------------------------------
* Panel b: Public transport — women
* -------------------------------
binsreg percap muj if mode_num == 2, by(year) polyreg(1) savedata(test_muj_b) replace

preserve
use test_muj_b, clear

twoway ///
    (scatter dots_fit dots_x if year == 2012, msymbol(O) mcolor(maroon) msize(large)) ///
    (line poly_fit poly_x if year == 2012, lcolor(maroon)) ///
    (scatter dots_fit dots_x if year == 2017, msymbol(X) mcolor(maroon) msize(large)) ///
    (line poly_fit poly_x if year == 2017, lcolor(maroon) lpattern(dash)), ///
    legend(order(1 3) label(1 "2012") label(3 "2017")) ///
    title("b) Public transport for share of women", size(large)) ///
    xtitle("Share of women", size(large)) ///
    ytitle("Accessibility", size(large)) ///
    xlabel(, labsize(medium)) ylabel(, labsize(medium)) ///
    graphregion(margin(zero)) ysize(5) xsize(7)

graph export "$figdir/Figure_B6_panel_b_women.pdf", replace
restore


* -------------------------------
* Panel c: Change in accessibility — women
* -------------------------------
binsreg change_percap muj, by(mode_x) polyreg(1) savedata(test_muj_c) replace

preserve
use test_muj_c, clear

twoway ///
    (scatter dots_fit dots_x if mode_x == 1, msymbol(O) mcolor(navy) msize(medium)) ///
    (line poly_fit poly_x if mode_x == 1, lcolor(navy)) ///
    (scatter dots_fit dots_x if mode_x == 2, msymbol(D) mcolor(maroon) msize(medium)) ///
    (line poly_fit poly_x if mode_x == 2, lcolor(maroon)), ///
    legend(order(1 3) label(1 "Private") label(3 "Public")) ///
    title("c) Change in accessibility by mode for share of women", size(large)) ///
    xtitle("Share of women", size(large)) ///
    ytitle("Change in accessibility", size(large)) ///
    xlabel(, labsize(medium)) ylabel(, labsize(medium)) ///
    graphregion(margin(zero)) ysize(5) xsize(7)

graph export "$figdir/Figure_B6_panel_c_women.pdf", replace
restore
