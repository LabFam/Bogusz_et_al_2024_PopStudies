/* Copyright (c) 2024 Honorata Bogusz

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OTHERWISE, ARISING FROM,
OUT OF IN CONNECTION WITH THE SOFTWARE THE USE OTHER DEALINGS IN THE
SOFTWARE.*/

version 18
graph set window fontface "Times New Roman"	
	
* CORRELATIONS OF TASK MEASURES WITH OTHER JOB CHARACTERISTICS

use "../generated_data/data_for_correlations.dta", replace

misstable sum

label var sex "Gender"
label def sex 1 "Men" 2 "Women", modify
label val sex sex

label var age "Age"
label var age2 "Age Squared"

label var edu "Education"
label def edu 1 "Education: Low" 2 "Education: Middle" 3 "Education: High", modify
label val edu edu

label var occ "Occupation"
label var bula "Bundesland"
label var analytic "Analytic"
label var interactive "Interactive"

label var wfh "Work from Home"
label def wfh 1 "Work from Home: Yes" 0 "Work from Home: No", modify
label val wfh wfh

label var overtime "Working Overtime"
label def overtime 1 "Working Overtime: Yes" 0 "Working Overtime: No", modify
label val overtime overtime

label var pressure "Working under Pressure"
label def pressure 1 "Working under Pressure: Never" 2 "Working under Pressure: Seldom" 3 "Working under Pressure: Sometimes" 4 "Working under Pressure: Frequently", modify
label val pressure pressure

label var monthly_wage "Monthly Wage"
label var year "Year"

* Express monthly wage in thousands for numerical stability.
replace monthly_wage = monthly_wage/1000

* Without work from home which is not contained in the 2012 data release.
reg analytic i.overtime i.pressure monthly_wage age i.edu i.year if sex==1
estimates store est_1_men
reg analytic i.overtime i.pressure monthly_wage age i.edu i.year if sex==2
estimates store est_1_women
reg interactive i.overtime i.pressure monthly_wage age i.edu i.year if sex==1
estimates store est_2_men
reg interactive i.overtime i.pressure monthly_wage age i.edu i.year if sex==2
estimates store est_2_women

* With work from home.
reg analytic i.wfh i.overtime i.pressure monthly_wage age i.edu i.year if sex==1
estimates store est_3_men
reg analytic i.wfh i.overtime i.pressure monthly_wage age i.edu i.year if sex==2
estimates store est_3_women

reg interactive i.wfh i.overtime i.pressure monthly_wage age i.edu i.year if sex==1
estimates store est_4_men
reg interactive i.wfh i.overtime i.pressure monthly_wage age i.edu i.year if sex==2
estimates store est_4_women

esttab est_1_men est_2_men est_3_men est_4_men using "../tables/Table_1_men.csv", se label star(* 0.10 ** 0.05 *** 0.01) aic scalar(ll) replace 
esttab est_1_women est_2_women est_3_women est_4_women using "../tables/Table_1_women.csv", se label star(* 0.10 ** 0.05 *** 0.01) aic scalar(ll) replace 

* TASK TRAJECTORIES BY HIGHEST EVER ACHIEVED AT AGE 35+

* Analytic

use "../final_data/first_birth_for_analysis_labeled.dta" if migrant==0, clear
bys pid: egen max_cat_analytic = max(cat_analytic) if age >= 35
bys pid: egen max_cat_analytic2 = max(max_cat_analytic)

keep if max_cat_analytic2 != .
keep if max_cat_analytic2 > 1

label var max_cat_analytic2 "Max analytic at age 35+"
lab def max_cat_analytic2 2 "Low" 3 "Medium" 4 "High", modify
label val max_cat_analytic2 max_cat_analytic2

collapse analytic, by(age sex max_cat_analytic2) 

reshape wide analytic, i(age max_cat_analytic2) j(sex)

xtset age max_cat_analytic2, yearly

graph twoway (line analytic2 age if max_cat_analytic2==2, lwidth(thick) lcolor("1 81 0")) || ///
(line analytic2 age if max_cat_analytic2==3, lwidth(thick) lcolor("254 70 254")) || ///
(line analytic2 age if max_cat_analytic2==4, lwidth(thick) lcolor("217 194 99")), ///
title("Analytic", nospan size(large)) ytitle("Task measure") xtitle("Age") ylabel(0(50)100, labsize(medlarge)) yscale(range(0(50)100)) xlabel(20(10)50, labsize(medlarge)) xscale(range(20(10)55)) ///
legend(label(1 "Low") label(2 "Medium") label(3 "High") row(1) size(small) subtitle("Task category", size(small))) name(women_analytic, replace)

graph twoway (line analytic1 age if max_cat_analytic2==2, lwidth(thick) lcolor("1 81 0")) || ///
(line analytic1 age if max_cat_analytic2==3, lwidth(thick) lcolor("254 70 254")) || ///
(line analytic1 age if max_cat_analytic2==4, lwidth(thick) lcolor("217 194 99")), ///
title("Analytic", nospan size(large)) ytitle("Task measure") xtitle("Age") ylabel(0(50)100, labsize(medlarge)) yscale(range(0(50)100)) xlabel(20(10)50, labsize(medlarge)) xscale(range(20(10)55)) ///
legend(label(1 "Low") label(2 "Medium") label(3 "High") row(1) size(small) subtitle("Task category", size(small))) name(men_analytic, replace)

* Interactive

use "../final_data/first_birth_for_analysis_labeled.dta" if migrant==0, clear
bys pid: egen max_cat_interactive = max(cat_interactive) if age >= 35
bys pid: egen max_cat_interactive2 = max(max_cat_interactive)

keep if max_cat_interactive2 != .
keep if max_cat_interactive2 > 1

label var max_cat_interactive2 "Max interactive at age 35+"
lab def max_cat_interactive2 2 "Low" 3 "Medium" 4 "High", modify
label val max_cat_interactive2 max_cat_interactive2

collapse interactive, by(age sex max_cat_interactive2) 

reshape wide interactive, i(age max_cat_interactive2) j(sex)

xtset age max_cat_interactive2, yearly

graph twoway (line interactive2 age if max_cat_interactive2==2, lwidth(thick) lcolor("1 81 0")) || ///
(line interactive2 age if max_cat_interactive2==3, lwidth(thick) lcolor("254 70 254")) || ///
(line interactive2 age if max_cat_interactive2==4, lwidth(thick) lcolor("217 194 99")), ///
title("Interactive", nospan size(large)) ytitle("Task measure") xtitle("Age") ylabel(0(50)100, labsize(medlarge)) yscale(range(0(50)100)) xlabel(20(10)50, labsize(medlarge)) xscale(range(20(10)55)) ///
legend(label(1 "Low") label(2 "Medium") label(3 "High") row(1) size(small) subtitle("Task category", size(small))) name(women_interactive, replace)

graph twoway (line interactive1 age if max_cat_interactive2==2, lwidth(thick) lcolor("1 81 0")) || ///
(line interactive1 age if max_cat_interactive2==3, lwidth(thick) lcolor("254 70 254")) || ///
(line interactive1 age if max_cat_interactive2==4, lwidth(thick) lcolor("217 194 99")), ///
title("Interactive", nospan size(large)) ytitle("Task measure") xtitle("Age") ylabel(0(50)100, labsize(medlarge)) yscale(range(0(50)100)) xlabel(20(10)50, labsize(medlarge)) xscale(range(20(10)55)) ///
legend(label(1 "Low") label(2 "Medium") label(3 "High") row(1) size(small) subtitle("Task category", size(small))) name(men_interactive, replace)

grc1leg women_analytic women_interactive, title("Women", size(medlarge)) name(women, replace)
grc1leg men_analytic men_interactive, title("Men", size(medlarge)) name(men, replace)
grc1leg women men, rows(2)
graph display, ysize(15) xsize(12)
graph export "../plots/Figure_A1.png", height(1500) width(1200) replace
graph export "../plots/Figure_A1.eps", replace

* DEVELOPMENT OF CATEGORICAL TASK VARIABLES OVER TIME

use "../final_data/first_birth_for_analysis_labeled.dta" if migrant==0, clear

keep if syear!=2019

gen year = .
replace year = 1 if syear < 1990
replace year = 2 if syear >= 1990 & syear < 1995
replace year = 3 if syear >= 1995 & syear < 2000
replace year = 4 if syear >= 2000 & syear < 2005
replace year = 5 if syear >= 2005 & syear < 2010
replace year = 6 if syear >= 2010

keep pid year sex cat_analytic_new cat_interactive_new

lab def year 1 "1984-89" 2 "1990-94" 3 "1995-99" 4 "2000-04" 5 "2005-09" 6 "2010-18", modify
label val year year

* analytic

g analytic_res = 1 if cat_analytic_new == 0
replace analytic_res = 0 if analytic_res == .

g analytic_in_edu = 1 if cat_analytic_new == 1
replace analytic_in_edu = 0 if analytic_in_edu == .

g analytic_low = 1 if cat_analytic_new == 2
replace analytic_low = 0 if analytic_low == .

g analytic_medium = 1 if cat_analytic_new == 3
replace analytic_medium = 0 if analytic_medium == .

g analytic_high = 1 if cat_analytic_new == 4
replace analytic_high = 0 if analytic_high == .

* interactive

g interactive_res = 1 if cat_interactive_new == 0
replace interactive_res = 0 if interactive_res == .

g interactive_in_edu = 1 if cat_interactive_new == 1
replace interactive_in_edu = 0 if interactive_in_edu == .

g interactive_low = 1 if cat_interactive_new == 2
replace interactive_low = 0 if interactive_low == .

g interactive_medium = 1 if cat_interactive_new == 3
replace interactive_medium = 0 if interactive_medium == .

g interactive_high = 1 if cat_interactive_new == 4
replace interactive_high = 0 if interactive_high == .

* collapse

gcollapse analytic_res analytic_in_edu analytic_low analytic_medium analytic_high interactive_res interactive_in_edu interactive_low interactive_medium interactive_high, by(year sex)

* analytic

graph bar analytic_res analytic_in_edu analytic_low analytic_medium analytic_high if sex==2, over(year, gap(0) lab(angle(45) labsize(medlarge))) stack ///
title("Analytic", nospan size(large)) legend(off label(1 "Residual") label(2 "In education") label(3 "Low") label(4 "Medium") label(5 "High") row(1) size(small) subtitle("Task category", size(small))) ///
ytitle("Share of people") b1title("Period") ///
bar(1, fcolor("5 0 244") lcolor("black") lwidth(medthick) fintensity(inten100)) ///
bar(2, fcolor("254 115 0") lcolor("black") lwidth(medthick) fintensity(inten100)) ///
bar(3, fcolor("1 81 0") lcolor("1 81 0") fintensity(inten100)) ///
bar(4, fcolor("254 70 254") lcolor("254 70 254") fintensity(inten100)) ///
bar(5, fcolor(sand) lcolor(sand) fintensity(inten100)) ///
ylabel(0 "0.0" 0.5 "0.5" 1 "1.0", format(%03.2f) labsize(medlarge)) name(women_analytic, replace)

graph bar analytic_res analytic_in_edu analytic_low analytic_medium analytic_high if sex==1, over(year, gap(0) lab(angle(45) labsize(medlarge))) stack ///
title("Analytic", nospan size(large)) legend(off label(1 "Residual") label(2 "In education") label(3 "Low") label(4 "Medium") label(5 "High") row(1) size(small) subtitle("Task category", size(small))) ///
ytitle("Share of people") b1title("Period") ///
bar(1, fcolor("5 0 244") lcolor("black") lwidth(medthick) fintensity(inten100)) ///
bar(2, fcolor("254 115 0") lcolor("black") lwidth(medthick) fintensity(inten100)) ///
bar(3, fcolor("1 81 0") lcolor("1 81 0") fintensity(inten100)) ///
bar(4, fcolor("254 70 254") lcolor("254 70 254") fintensity(inten100)) ///
bar(5, fcolor(sand) lcolor(sand) fintensity(inten100)) ///
ylabel(0 "0.0" 0.5 "0.5" 1 "1.0", format(%03.2f) labsize(medlarge)) name(men_analytic, replace)

* interactive

graph bar interactive_res interactive_in_edu interactive_low interactive_medium interactive_high if sex==2, over(year, gap(0) lab(angle(45) labsize(medlarge))) stack ///
title("Interactive", nospan size(large)) legend(off label(1 "Residual") label(2 "In education") label(3 "Low") label(4 "Medium") label(5 "High") row(1) size(small) subtitle("Task category", size(small))) ///
ytitle("Share of people") b1title("Period") ///
bar(1, fcolor("5 0 244") lcolor("black") lwidth(medthick) fintensity(inten100)) ///
bar(2, fcolor("254 115 0") lcolor("black") lwidth(medthick) fintensity(inten100)) ///
bar(3, fcolor("1 81 0") lcolor("1 81 0") fintensity(inten100)) ///
bar(4, fcolor("254 70 254") lcolor("254 70 254") fintensity(inten100)) ///
bar(5, fcolor(sand) lcolor(sand) fintensity(inten100)) ///
ylabel(0 "0.0" 0.5 "0.5" 1 "1.0", format(%03.2f) labsize(medlarge)) name(women_interactive, replace)

graph bar interactive_res interactive_in_edu interactive_low interactive_medium interactive_high if sex==1, over(year, gap(0) lab(angle(45) labsize(medlarge))) stack ///
title("Interactive", nospan size(large)) legend(off label(1 "Residual") label(2 "In education") label(3 "Low") label(4 "Medium") label(5 "High") row(1) size(small) subtitle("Task category", size(small))) ///
ytitle("Share of people") b1title("Period") ///
bar(1, fcolor("5 0 244") lcolor("black") lwidth(medthick) fintensity(inten100)) ///
bar(2, fcolor("254 115 0") lcolor("black") lwidth(medthick) fintensity(inten100)) ///
bar(3, fcolor("1 81 0") lcolor("1 81 0") fintensity(inten100)) ///
bar(4, fcolor("254 70 254") lcolor("254 70 254") fintensity(inten100)) ///
bar(5, fcolor(sand) lcolor(sand) fintensity(inten100)) ///
ylabel(0 "0.0" 0.5 "0.5" 1 "1.0", format(%03.2f) labsize(medlarge)) name(men_interactive, replace)

grc1leg women_analytic women_interactive, title("Women", size(medlarge)) name(women, replace)
grc1leg men_analytic men_interactive, title("Men", size(medlarge)) name(men, replace)
grc1leg women men, rows(2)
graph display, ysize(15) xsize(12)
graph export "../plots/Figure_1.eps", replace
graph export "../plots/Figure_1.png", height(1500) width(1200) replace

* CUMULATIVE INCIDENCE CALCULATED IN R

// Analtytic

use "../generated_data/cum_inc.dta" if tm == "analytic", clear
replace h = 0.8 if h > 0.8

// Women, 1984-1999

graph twoway (line h baseline if period=="1984-1999" & sex==2 & case=="3", lwidth(thick) lcolor("1 81 0")) || ///
(line h baseline if period=="1984-1999" & sex==2 & case=="2", lwidth(thick) lcolor("254 70 254")), ///
title("1984-99", nospan size(large)) ytitle("Cumulative incidence") xtitle("Age") ///
ylabel(0 "0.0" 0.4 "0.4" 0.8 "0.8", labsize(medlarge)) yscale(range(0(0.4)0.8)) ///
xlabel(1 "20-24" 2 "25-29" 3 "30-34" 4 "35-39" 5 "40-49", angle(45) labsize(medlarge)) ///
legend(label(1 "Case 1 (Low)") label(2 "Case 2 (Medium)") label(3 "Case 3 (High)") row(1) size(small) subtitle("Task scenario", size(small))) name(women_period1, replace)


// Men, 1984-1999

graph twoway (line h baseline if period=="1984-1999" & sex==1 & case=="3", lwidth(thick) lcolor("1 81 0")) || ///
(line h baseline if period=="1984-1999" & sex==1 & case=="2", lwidth(thick) lcolor("254 70 254")), ///
title("1984-99", nospan size(large)) ytitle("Cumulative incidence") xtitle("Age") ///
ylabel(0 "0.0" 0.4 "0.4" 0.8 "0.8", labsize(medlarge)) yscale(range(0(0.4)0.8)) ///
xlabel(1 "20-24" 2 "25-29" 3 "30-34" 4 "35-39" 5 "40-49", angle(45) labsize(medlarge)) ///
legend(label(1 "Case 1 (Low)") label(2 "Case 2 (Medium)") label(3 "Case 3 (High)") row(1) size(small) subtitle("Task scenario", size(small))) name(men_period1, replace)


// Women, 2000-2018

graph twoway (line h baseline if period=="2000-2018" & sex==2 & case=="3", lwidth(thick) lcolor("1 81 0")) || ///
(line h baseline if period=="2000-2018" & sex==2 & case=="2", lwidth(thick) lcolor("254 70 254")) || ///
(line h baseline if period=="2000-2018" & sex==2 & case=="1", lwidth(thick) lcolor("217 194 99")), ///
title("2000-18", nospan size(large)) ytitle("Cumulative incidence") xtitle("Age") ///
ylabel(0 "0.0" 0.4 "0.4" 0.8 "0.8", labsize(medlarge)) yscale(range(0(0.4)0.8)) ///
xlabel(1 "20-24" 2 "25-29" 3 "30-34" 4 "35-39" 5 "40-49", angle(45) labsize(medlarge)) ///
legend(label(1 "Case 1 (Low)") label(2 "Case 2 (Medium)") label(3 "Case 3 (High)") row(1) size(small) subtitle("Task scenario", size(small))) name(women_period2, replace)

// Men, 2000-2018

graph twoway (line h baseline if period=="2000-2018" & sex==1 & case=="3", lwidth(thick) lcolor("1 81 0")) || ///
(line h baseline if period=="2000-2018" & sex==1 & case=="2", lwidth(thick) lcolor("254 70 254")) || ///
(line h baseline if period=="2000-2018" & sex==1 & case=="1", lwidth(thick) lcolor("217 194 99")), ///
title("2000-18", nospan size(large)) ytitle("Cumulative incidence") xtitle("Age") ///
ylabel(0 "0.0" 0.4 "0.4" 0.8 "0.8", labsize(medlarge)) yscale(range(0(0.4)0.8)) ///
xlabel(1 "20-24" 2 "25-29" 3 "30-34" 4 "35-39" 5 "40-49", angle(45) labsize(medlarge)) ///
legend(label(1 "Case 1 (Low)") label(2 "Case 2 (Medium)") label(3 "Case 3 (High)") row(1) size(small) subtitle("Task scenario", size(small))) name(men_period2, replace)

// combine

grc1leg women_period1 women_period2, title("Women", size(medlarge)) legendfrom(women_period2) name(women, replace)
grc1leg men_period1 men_period2, title("Men", size(medlarge)) legendfrom(men_period2) name (men, replace)
grc1leg women men, rows(2)
graph display, ysize(15) xsize(12)
graph export "../plots/Figure_5.eps", replace
graph export "../plots/Figure_5.png", height(1500) width(1200) replace

// Interactive

use "../generated_data/cum_inc.dta" if tm == "interactive", clear

// Women, 1984-1999

graph twoway (line h baseline if period=="1984-1999" & sex==2 & case=="3", lwidth(thick) lcolor("1 81 0")) || ///
(line h baseline if period=="1984-1999" & sex==2 & case=="2", lwidth(thick) lcolor("254 70 254")), ///
title("1984-99", nospan size(large)) ytitle("Cumulative incidence") xtitle("Age") ///
ylabel(0 "0.0" 0.4 "0.4" 0.8 "0.8", labsize(medlarge)) yscale(range(0(0.4)0.8)) ///
xlabel(1 "20-24" 2 "25-29" 3 "30-34" 4 "35-39" 5 "40-49", angle(45) labsize(medlarge)) ///
legend(label(1 "Case 1 (Low)") label(2 "Case 2 (Medium)") label(3 "Case 3 (High)") row(1) size(small) subtitle("Task scenario", size(small))) name(women_period1, replace)


// Men, 1984-1999

graph twoway (line h baseline if period=="1984-1999" & sex==1 & case=="3", lwidth(thick) lcolor("1 81 0")) || ///
(line h baseline if period=="1984-1999" & sex==1 & case=="2", lwidth(thick) lcolor("254 70 254")), ///
title("1984-99", nospan size(large)) ytitle("Cumulative incidence") xtitle("Age") ///
ylabel(0 "0.0" 0.4 "0.4" 0.8 "0.8", labsize(medlarge)) yscale(range(0(0.4)0.8)) ///
xlabel(1 "20-24" 2 "25-29" 3 "30-34" 4 "35-39" 5 "40-49", angle(45) labsize(medlarge)) ///
legend(label(1 "Case 1 (Low)") label(2 "Case 2 (Medium)") label(3 "Case 3 (High)") row(1) size(small) subtitle("Task scenario", size(small))) name(men_period1, replace)


// Women, 2000-2018

graph twoway (line h baseline if period=="2000-2018" & sex==2 & case=="3", lwidth(thick) lcolor("1 81 0")) || ///
(line h baseline if period=="2000-2018" & sex==2 & case=="2", lwidth(thick) lcolor("254 70 254")) || ///
(line h baseline if period=="2000-2018" & sex==2 & case=="1", lwidth(thick) lcolor("217 194 99")), ///
title("2000-18", nospan size(large)) ytitle("Cumulative incidence") xtitle("Age") ///
ylabel(0 "0.0" 0.4 "0.4" 0.8 "0.8", labsize(medlarge)) yscale(range(0(0.4)0.8)) ///
xlabel(1 "20-24" 2 "25-29" 3 "30-34" 4 "35-39" 5 "40-49", angle(45) labsize(medlarge)) ///
legend(label(1 "Case 1 (Low)") label(2 "Case 2 (Medium)") label(3 "Case 3 (High)") row(1) size(small) subtitle("Task scenario", size(small))) name(women_period2, replace)

// Men, 2000-2018

graph twoway (line h baseline if period=="2000-2018" & sex==1 & case=="3", lwidth(thick) lcolor("1 81 0")) || ///
(line h baseline if period=="2000-2018" & sex==1 & case=="2", lwidth(thick) lcolor("254 70 254")) || ///
(line h baseline if period=="2000-2018" & sex==1 & case=="1", lwidth(thick) lcolor("217 194 99")), ///
title("2000-18", nospan size(large)) ytitle("Cumulative incidence") xtitle("Age") ///
ylabel(0 "0.0" 0.4 "0.4" 0.8 "0.8", labsize(medlarge)) yscale(range(0(0.4)0.8)) ///
xlabel(1 "20-24" 2 "25-29" 3 "30-34" 4 "35-39" 5 "40-49", angle(45) labsize(medlarge)) ///
legend(label(1 "Case 1 (Low)") label(2 "Case 2 (Medium)") label(3 "Case 3 (High)") row(1) size(small) subtitle("Task scenario", size(small))) name(men_period2, replace)

// combine

grc1leg women_period1 women_period2, title("Women", size(medlarge)) legendfrom(women_period2) name(women, replace)
grc1leg men_period1 men_period2, title("Men", size(medlarge)) legendfrom(men_period2) name (men, replace)
grc1leg women men, rows(2)
graph display, ysize(15) xsize(12)
graph export "../plots/Figure_6.eps", replace
graph export "../plots/Figure_6.png", height(1500) width(1200) replace
