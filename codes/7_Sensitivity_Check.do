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
* declare a global with controls
global controls_s1 "i.period i.baseline i.num_sib i.l2_residence i.migrant"
global controls_s2 "i.period i.baseline i.num_sib i.l2_residence"
	
* SENSITIVITY 1: FIRST BIRTH BASIC MODELS, INCLUDING MIGRANTS AND CONTROLLING FOR MIGRATION BACKGROUND

use "../final_data/first_birth_for_analysis_labeled.dta", clear

*** ANALYTIC ***

vcemway cloglog child_1 ib2.cat_l2_analytic_new $controls_s1 if sex==2, cluster(pid l2_occupation_kldb92) nolog eform
estimates store analytic_women_fb

margins cat_l2_analytic_new
marginsplot, legend(size(medium)) title("Analytic", nospan size(large)) xtitle("Task category") ytitle("Predicted probability") ylabel(0(0.05)0.10, format(%04.2f) labsize(medlarge)) yscale(range(0(0.05)0.10)) xlabel(, angle(45) labsize(medlarge)) plot1opts(lwidth(thick) lcolor(black) mcolor(black)) ciopts(lwidth(thick) lcolor(black) mcolor(black)) level(83) recast(scatter) name(margins_analytic_women_fb, replace)

vcemway cloglog child_1 ib2.cat_l2_analytic_new $controls_s1 if sex==1, cluster(pid l2_occupation_kldb92) nolog eform
estimates store analytic_men_fb

margins cat_l2_analytic_new
marginsplot, legend(size(medium)) title("Analytic", nospan size(large)) xtitle("Task category") ytitle("Predicted probability") ylabel(0(0.05)0.10, format(%04.2f) labsize(medlarge)) yscale(range(0(0.05)0.10)) xlabel(, angle(45) labsize(medlarge)) plot1opts(lwidth(thick) lcolor(black) mcolor(black)) ciopts(lwidth(thick) lcolor(black) mcolor(black)) level(83) recast(scatter) name(margins_analytic_men_fb, replace)

*** INTERACTIVE ***

vcemway cloglog child_1 ib2.cat_l2_interactive_new $controls_s1 if sex==2, cluster(pid l2_occupation_kldb92) nolog eform
estimates store interactive_women_fb

margins cat_l2_interactive_new
marginsplot, legend(size(medium)) title("Interactive", nospan size(large)) xtitle("Task category") ytitle("Predicted probability") ylabel(0(0.05)0.10, format(%04.2f) labsize(medlarge)) yscale(range(0(0.05)0.10)) xlabel(, angle(45) labsize(medlarge)) plot1opts(lwidth(thick) lcolor(black) mcolor(black)) ciopts(lwidth(thick) lcolor(black) mcolor(black)) level(83) recast(scatter) name(margins_interactive_women_fb, replace)

vcemway cloglog child_1 ib2.cat_l2_interactive_new $controls_s1 if sex==1, cluster(pid l2_occupation_kldb92) nolog eform
estimates store interactive_men_fb

margins cat_l2_interactive_new
marginsplot, legend(size(medium)) title("Interactive", nospan size(large)) xtitle("Task category") ytitle("Predicted probability") ylabel(0(0.05)0.10, format(%04.2f) labsize(medlarge)) yscale(range(0(0.05)0.10)) xlabel(, angle(45) labsize(medlarge)) plot1opts(lwidth(thick) lcolor(black) mcolor(black)) ciopts(lwidth(thick) lcolor(black) mcolor(black)) level(83) recast(scatter) name(margins_interactive_men_fb, replace)

graph combine margins_analytic_women_fb margins_interactive_women_fb, title("Women", size(medlarge)) name(women, replace)
graph combine margins_analytic_men_fb margins_interactive_men_fb, title("Men", size(medlarge)) name(men, replace)
graph combine women men, rows(2)
graph display, ysize(15) xsize(12)
graph export "../plots/Figure_A2.png", height(1500) width(1200) replace
graph export "../plots/Figure_A2.eps", replace

* SENSITIVITY 2: FIRST BIRTH BASIC MODELS, TASK MEASURES INTERPOLATED INDEPENDENT OF THE LABOUR MARKET STATUS

use "../generated_data/first_birth_for_analysis_rob.dta", clear

label var pid "ID"
label var syear "Year"
label var period "Year"
label var gebjahr "Birth Year"
label var age "Age"
label var cohort "Cohort"
label var baseline "Age"
label var sex "Gender"
label var migrant "Migrant"
label var num_sib "Number of Siblings"
label var residence "East Germany"
label var l1_residence "East Germany L1"
label var l2_residence "East Germany L2"
label var child_1 "First Child"
label var occupation_kldb92 "Occupation"
label var l1_occupation_kldb92 "Occupation L1"
label var l2_occupation_kldb92 "Occupation L2"
label var education "Education"
label var l1_education "Education L1"
label var l2_education "Education L2"
label var lfs "Employment Status"
label var l1_lfs "Employment Status L1"
label var l2_lfs "Employment Status L2"
label var analytic "Analytic (Continuous)"
label var l1_analytic "Analytic (Continuous) L1"
label var l2_analytic "Analytic (Continuous) L2"
label var interactive "Interactive (Continuous)"
label var l1_interactive "Interactive (Continuous) L1"
label var l2_interactive "Interactive (Continuous) L2"
label var union_status "Union Status"
label var l1_union_status "Union Status L1"
label var l2_union_status "Union Status L2"
label var couple_type "Couple Type"
label var par_pid "Partner ID"
label var par_sex "Partner Gender"
label var par_lfs "Partner Employment Status"
label var l1_par_lfs "Partner Employment Status L1"
label var l2_par_lfs "Partner Employment Status L2"
label var par_occupation_kldb92 "Partner Occupation"
label var l1_par_occupation_kldb92 "Partner Occupation L1"
label var l2_par_occupation_kldb92 "Partner Occupation L2"
label var par_analytic "Partner Analytic (Continuous)"
label var l1_par_analytic "Partner Analytic (Continuous) L1"
label var l2_par_analytic "Partner Analytic (Continuous) L2"
label var par_interactive "Partner Interactive (Continuous)"
label var l1_par_interactive "Partner Interactive (Continuous) L1"
label var l2_par_interactive "Partner Interactive (Continuous) L2"

lab def period 1 "Period: 1984-1999" 2 "Period: 2000-2007" 3 "Period: 2008-2018", modify
lab val period period

lab def cohort 1 "1930-1949" 2 "1950-1959" 3 "1960-1969" 4 "1970-1979" 5 "1980-1989" 6 "1990-1999", modify
label val cohort cohort

lab def baseline 1 "Age: 20-24" 2 "Age: 25-29" 3 "Age: 30-34" 4 "Age: 35-39" 5 "Age: 40-49", modify
label val baseline baseline

lab def sex 1 "Men" 2 "Women", modify
label val sex sex

lab def par_sex 1 "Partner: Men" 2 "Partner: Women" -2 "Partner: No Partner", modify
label val par_sex par_sex

lab def migrant 1 "Migrant" 0 "German", modify
label val migrant migrant

label def num_sib 1 "0" 2 "1" 3 "2 or more"
label val num_sib num_sib

lab def residence 1 "West Germany" 2 "East Germany", modify
lab val residence residence
lab val l1_residence residence
lab val l2_residence residence

lab def education 1 "Education: Low" 2 "Education: medlarge" 3 "Education: High", modify
label val education education
label val l1_education education
label val l2_education education

lab def empl 1 "Employed" 2 "In education" 3 "Unemployed" 4 "Inactive", modify
label val lfs empl
label val l1_lfs empl
label val l2_lfs empl

lab def par_empl 1 "Partner: Employed" 2 "Partner: In education" 3 "Partner: Unemployed" 4 "Partner: Inactive" -2 "Partner: No Partner", modify
label val par_lfs par_empl
label val l1_par_lfs par_empl
label val l2_par_lfs par_empl

lab def union_status 1 "In Partnership" 0 "No Partner", modify
label val union_status union_status
label val l1_union_status union_status
label val l2_union_status union_status

lab def couple_type 1 "Different-Sex" 2 "Men Same-Sex" 3 "Women Same-Sex" -2 "No Partner", modify
label val couple_type couple_type

// REDEFINED

gen cat_analytic_new = .
replace cat_analytic_new = 2 if !missing(analytic) & analytic < 33
replace cat_analytic_new = 3 if !missing(analytic) & analytic >= 33 & analytic < 66
replace cat_analytic_new = 4 if !missing(analytic) & analytic >= 66 & analytic <= 100
replace cat_analytic_new = 1 if lfs == 2
replace cat_analytic_new = 0 if missing(analytic)
tab cat_analytic_new

gen cat_l1_analytic_new = .
replace cat_l1_analytic_new = 2 if !missing(l1_analytic) & l1_analytic < 33
replace cat_l1_analytic_new = 3 if !missing(l1_analytic) & l1_analytic >= 33 & l1_analytic < 66
replace cat_l1_analytic_new = 4 if !missing(l1_analytic) & l1_analytic >= 66 & l1_analytic <= 100
replace cat_l1_analytic_new = 1 if l1_lfs == 2
replace cat_l1_analytic_new = 0 if missing(l1_analytic)
tab cat_l1_analytic_new

gen cat_l2_analytic_new = .
replace cat_l2_analytic_new = 2 if !missing(l2_analytic) & l2_analytic < 33
replace cat_l2_analytic_new = 3 if !missing(l2_analytic) & l2_analytic >= 33 & l2_analytic < 66
replace cat_l2_analytic_new = 4 if !missing(l2_analytic) & l2_analytic >= 66 & l2_analytic <= 100
replace cat_l2_analytic_new = 1 if l2_lfs == 2
replace cat_l2_analytic_new = 0 if missing(l2_analytic)
tab cat_l2_analytic_new

gen cat_interactive_new = .
replace cat_interactive_new = 2 if !missing(interactive) & interactive < 33
replace cat_interactive_new = 3 if !missing(interactive) & interactive >= 33 & interactive < 66
replace cat_interactive_new = 4 if !missing(interactive) & interactive >= 66 & interactive <= 100
replace cat_interactive_new = 1 if lfs == 2
replace cat_interactive_new = 0 if missing(interactive)
tab cat_interactive_new

gen cat_l1_interactive_new = .
replace cat_l1_interactive_new = 2 if !missing(l1_interactive) & l1_interactive < 33
replace cat_l1_interactive_new = 3 if !missing(l1_interactive) & l1_interactive >= 33 & l1_interactive < 66
replace cat_l1_interactive_new = 4 if !missing(l1_interactive) & l1_interactive >= 66 & l1_interactive <= 100
replace cat_l1_interactive_new = 1 if l1_lfs == 2
replace cat_l1_interactive_new = 0 if missing(l1_interactive)
tab cat_l1_interactive_new

gen cat_l2_interactive_new = .
replace cat_l2_interactive_new = 2 if !missing(l2_interactive) & l2_interactive < 33
replace cat_l2_interactive_new = 3 if !missing(l2_interactive) & l2_interactive >= 33 & l2_interactive < 66
replace cat_l2_interactive_new = 4 if !missing(l2_interactive) & l2_interactive >= 66 & l2_interactive <= 100
replace cat_l2_interactive_new = 1 if l2_lfs == 2
replace cat_l2_interactive_new = 0 if missing(l2_interactive)
tab cat_l2_interactive_new

label var cat_analytic_new "Analytic"
label var cat_l1_analytic_new "Analytic L1"
label var cat_l2_analytic_new "Analytic L2"

label var cat_interactive_new "Interactive"
label var cat_l1_interactive_new "Interactive L1"
label var cat_l2_interactive_new "Interactive L2"

label def task 0 "Residual" 1 "In education" 2 "Low" 3 "Medium" 4 "High", modify
label val cat_analytic_new task
label val cat_l1_analytic_new task
label val cat_l2_analytic_new task
label val cat_interactive_new task
label val cat_l1_interactive_new task
label val cat_l2_interactive_new task

// REDEFINED PARTNER

gen cat_par_analytic_new = .
replace cat_par_analytic_new = 2 if !missing(par_analytic) & par_analytic >= 0 & par_analytic < 33
replace cat_par_analytic_new = 3 if !missing(par_analytic) & par_analytic >= 33 & par_analytic < 66
replace cat_par_analytic_new = 4 if !missing(par_analytic) & par_analytic >= 66 & par_analytic <= 100
replace cat_par_analytic_new = 1 if par_lfs == 2
replace cat_par_analytic_new = 99 if par_analytic == -2
replace cat_par_analytic_new = 0 if missing(par_analytic)
tab cat_par_analytic_new

gen cat_l1_par_analytic_new = .
replace cat_l1_par_analytic_new = 2 if !missing(l1_par_analytic) & l1_par_analytic >= 0 & l1_par_analytic < 33
replace cat_l1_par_analytic_new = 3 if !missing(l1_par_analytic) & l1_par_analytic >= 33 & l1_par_analytic < 66
replace cat_l1_par_analytic_new = 4 if !missing(l1_par_analytic) & l1_par_analytic >= 66 & l1_par_analytic <= 100
replace cat_l1_par_analytic_new = 1 if l1_par_lfs == 2
replace cat_l1_par_analytic_new = 99 if l1_par_analytic == -2
replace cat_l1_par_analytic_new = 0 if missing(l1_par_analytic)
tab cat_l1_par_analytic_new

gen cat_l2_par_analytic_new = .
replace cat_l2_par_analytic_new = 2 if !missing(l2_par_analytic) & l2_par_analytic >= 0 & l2_par_analytic < 33
replace cat_l2_par_analytic_new = 3 if !missing(l2_par_analytic) & l2_par_analytic >= 33 & l2_par_analytic < 66
replace cat_l2_par_analytic_new = 4 if !missing(l2_par_analytic) & l2_par_analytic >= 66 & l2_par_analytic <= 100
replace cat_l2_par_analytic_new = 1 if l2_par_lfs == 2
replace cat_l2_par_analytic_new = 99 if l2_par_analytic == -2
replace cat_l2_par_analytic_new = 0 if missing(l2_par_analytic)
tab cat_l2_par_analytic_new

gen cat_par_interactive_new = .
replace cat_par_interactive_new = 2 if !missing(par_interactive) & par_interactive >= 0 & par_interactive < 33
replace cat_par_interactive_new = 3 if !missing(par_interactive) & par_interactive >= 33 & par_interactive < 66
replace cat_par_interactive_new = 4 if !missing(par_interactive) & par_interactive >= 66 & par_interactive <= 100
replace cat_par_interactive_new = 1 if par_lfs == 2
replace cat_par_interactive_new = 99 if par_interactive == -2
replace cat_par_interactive_new = 0 if missing(par_interactive)
tab cat_par_interactive_new

gen cat_l1_par_interactive_new = .
replace cat_l1_par_interactive_new = 2 if !missing(l1_par_interactive) & l1_par_interactive >= 0 & l1_par_interactive < 33
replace cat_l1_par_interactive_new = 3 if !missing(l1_par_interactive) & l1_par_interactive >= 33 & l1_par_interactive < 66
replace cat_l1_par_interactive_new = 4 if !missing(l1_par_interactive) & l1_par_interactive >= 66 & l1_par_interactive <= 100
replace cat_l1_par_interactive_new = 1 if l1_par_lfs == 2
replace cat_l1_par_interactive_new = 99 if l1_par_interactive == -2
replace cat_l1_par_interactive_new = 0 if missing(l1_par_interactive)
tab cat_l1_par_interactive_new

gen cat_l2_par_interactive_new = .
replace cat_l2_par_interactive_new = 2 if !missing(l2_par_interactive) & l2_par_interactive >= 0 & l2_par_interactive < 33
replace cat_l2_par_interactive_new = 3 if !missing(l2_par_interactive) & l2_par_interactive >= 33 & l2_par_interactive < 66
replace cat_l2_par_interactive_new = 4 if !missing(l2_par_interactive) & l2_par_interactive >= 66 & l2_par_interactive <= 100
replace cat_l2_par_interactive_new = 1 if l2_par_lfs == 2
replace cat_l2_par_interactive_new = 99 if l2_par_interactive == -2
replace cat_l2_par_interactive_new = 0 if missing(l2_par_interactive)
tab cat_l2_par_interactive_new

label var cat_par_analytic_new "Partner Analytic"
label var cat_l1_par_analytic_new "Partner Analytic L1"
label var cat_l2_par_analytic_new "Partner Analytic L2"

label var cat_par_interactive_new "Partner Interactive"
label var cat_l1_par_interactive_new "Partner Interactive L1"
label var cat_l2_par_interactive_new "Partner Interactive L2"

label def task_par 99 "No partner" 0 "Partner: Residual" 1 "Partner: In education" 2 "Partner: Low" 3 "Partner: Medium" 4 "Partner: High", modify
label val cat_par_analytic_new task_par
label val cat_l1_par_analytic_new task_par
label val cat_l2_par_analytic_new task_par
label val cat_par_interactive_new task_par
label val cat_l1_par_interactive_new task_par
label val cat_l2_par_interactive_new task_par

misstable sum

foreach var of varlist education-l2_par_lfs {
	bys pid: replace `var' = `var'[n-1] if `var' == .
}

foreach var of varlist par_analytic-l2_par_interactive {
	bys pid: replace `var' = `var'[n-1] if `var' == .
}

bys pid: replace num_sib = num_sib[n-1] if num_sib == .
bys pid: replace l1_residence = l1_residence[n-1] if l1_residence == .
bys pid: replace l2_residence = l2_residence[n-1] if l2_residence == .

misstable sum

keep if for_analysis == 1
save "../final_data/first_birth_for_analysis_rob_labeled.dta", replace

use "../final_data/first_birth_for_analysis_rob_labeled.dta" if migrant==0, clear

*** ANALYTIC ***

vcemway cloglog child_1 ib2.cat_l2_analytic_new $controls_s2 if sex==2, cluster(pid l2_occupation_kldb92) nolog eform
estimates store analytic_women_fb

margins cat_l2_analytic_new
marginsplot, legend(size(medlarge)) title("Analytic", nospan size(large)) xtitle("Task category") ytitle("Predicted probability") ylabel(0(0.05)0.10, format(%04.2f) labsize(medlarge)) yscale(range(0(0.05)0.10)) xlabel(, angle(45) labsize(medlarge)) plot1opts(lwidth(thick) lcolor(black) mcolor(black)) ciopts(lwidth(thick) lcolor(black) mcolor(black)) level(83) recast(scatter) name(margins_analytic_women_fb, replace)

vcemway cloglog child_1 ib2.cat_l2_analytic_new $controls_s2 if sex==1, cluster(pid l2_occupation_kldb92) nolog eform
estimates store analytic_men_fb

margins cat_l2_analytic_new
marginsplot, legend(size(medlarge)) title("Analytic", nospan size(large)) xtitle("Task category") ytitle("Predicted probability") ylabel(0(0.05)0.10, format(%04.2f) labsize(medlarge)) yscale(range(0(0.05)0.10)) xlabel(, angle(45) labsize(medlarge)) plot1opts(lwidth(thick) lcolor(black) mcolor(black)) ciopts(lwidth(thick) lcolor(black) mcolor(black)) level(83) recast(scatter) name(margins_analytic_men_fb, replace)

*** INTERACTIVE ***

vcemway cloglog child_1 ib2.cat_l2_interactive_new $controls_s2 if sex==2, cluster(pid l2_occupation_kldb92) nolog eform
estimates store interactive_women_fb

margins cat_l2_interactive_new
marginsplot, legend(size(medlarge)) title("Interactive", nospan size(large)) xtitle("Task category") ytitle("Predicted probability") ylabel(0(0.05)0.10, format(%04.2f) labsize(medlarge)) yscale(range(0(0.05)0.10)) xlabel(, angle(45) labsize(medlarge)) plot1opts(lwidth(thick) lcolor(black) mcolor(black)) ciopts(lwidth(thick) lcolor(black) mcolor(black)) level(83) recast(scatter) name(margins_interactive_women_fb, replace)

vcemway cloglog child_1 ib2.cat_l2_interactive_new $controls_s2 if sex==1, cluster(pid l2_occupation_kldb92) nolog eform
estimates store interactive_men_fb

margins cat_l2_interactive_new
marginsplot, legend(size(medlarge)) title("Interactive", nospan size(large)) xtitle("Task category") ytitle("Predicted probability") ylabel(0(0.05)0.10, format(%04.2f) labsize(medlarge)) yscale(range(0(0.05)0.10)) xlabel(, angle(45) labsize(medlarge)) plot1opts(lwidth(thick) lcolor(black) mcolor(black)) ciopts(lwidth(thick) lcolor(black) mcolor(black)) level(83) recast(scatter) name(margins_interactive_men_fb, replace)

graph combine margins_analytic_women_fb margins_interactive_women_fb, title("Women", size(medlarge)) name(women, replace)
graph combine margins_analytic_men_fb margins_interactive_men_fb, title("Men", size(medlarge)) name(men, replace)
graph combine women men, rows(2)
graph display, ysize(15) xsize(12)
graph export "../plots/Figure_A3.png", height(1500) width(1200) replace
graph export "../plots/Figure_A3.eps", replace
