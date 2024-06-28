* Codes by Daniela Rohrbach-Schmidt and Michael Tiemann sent to Honorata Bogusz in autumn 2021.

*************************************************************************************
*********************************Harmonisierte Stichprobe ***************************
*****Syntax zur Bildung der Analysestichprobe der BIBB/IAB-BAuA 1979-2006 *******

*Michael Tiemann, Daniela Rohrbach-Schmidt
*Reproduktion anhand der ZA-Files
*************************************************************************************

version 18
clear all

************************************* 1979 *************************************

* Im ZA-Codebuch: Stichprobe=BRD (einschl. West-Berlin), deutsche Erwerbspersonen (Erwerbstätige und Arbeitslose), Alter 15-65, 
* ohne Auszubildende, Soldaten, Personen des Bundesgrenzschutzes, Personen in Anstalten (n=29,737)

* DRS/MT: zusätzliche Selektion: nur Erwerbstätige (n=28,828) und Alter <66 (2 mit Geburtstjahr 1913), keine mithelfenden Familienangehörige, Arbeitszeit 10-168 Std.
* Soll: 28,088

use "../original_data/BIBB-Erwerbstaetige_1-7/ZA1243 - 1979/ZA1243.dta", clear

keep if (V64==1)				/*nur Erwerbstätige */
keep if (V437>=14 & V437!=1913) /*15-65*/
keep if V68!=30 				/*keine mithelfenden Familienangehörige*, weitere vielleicht aufgrund Stellung raugeflogen, kann aber anhand ZA-DAetn nicht nachvollzogen werden*/
keep if (V71>=10 & V71<=168)	/*Arbeitszeit in Synopse nur zwischen 10 und 168 Stunden */

d, short

save "../generated_data/bibb1979_har.dta", replace

* es sind noch 88 zuviel

************************************* 1985 *************************************

* Im ZA-Codebuch: Stichprobe=BRD (einschl. West-Berlin), deutsche Erwerbstätige, Alter 15-65, 
* ohne Auszubildende, Soldaten, mithelfende Familienangehörige (n=26,361)

* DRS/MT: zusätzliche Selektion: , Alter<66Arbeitszeit 10-168 Std. 
* Soll: 25,933

use "../original_data/BIBB-Erwerbstaetige_1-7/ZA1790 - 1986/ZA1790_v1-0-1.dta", clear

keep if (v176>=21) 				/*15-65*/
keep if (v4>=10 & v4<=168)		/*Arbeitszeit in Synopse nur zwischen 10 und 168 Stunden */
keep if v4!=99.9				/*in Synopse TNS Bildung: Hinweis: Fehler Vercodung Arbeitszeit: 99.9=9999*/

d, short

save "../generated_data/bibb1986_har.dta", replace

* es sind noch 115 zuviel

************************************* 1991 *************************************

* Im ZA-Codebuch: Stichprobe=BRD (Ost und West), Erwerbstätige, Ausländer mit ausreichend Deutschkenntnissen, Alter 15-65, 
* ohne Auszubildende, Wehrpflichtige, Zivildienstleistende,  mithelfende Familienangehörige, (n=26,361)

* DRS/MT: zusätzliche Selektion: Nur deutsche Westdeutsche, Alter<66, Arbeitszeit 10-168 Std. 
* Soll: 22,900

use "../original_data/BIBB-Erwerbstaetige_1-7/ZA2565 - 1992/ZA2565.dta", clear

keep if v301==1				/*deutsche Staatsangehörigkeit*/
keep if v315==2 			/*Westdeutschland*/
keep if v298>=27 & v298<75	/*Alter 15-65*/
keep if v15!=.				/*Arbeitszeit in Synopse nur zwischen 10 und 168 Stunden */ 

d, short

save "../generated_data/bibb1992_har.dta", replace

* es sind noch 148 zuviel

************************************* 1998 *************************************
 
* Im ZA-Codebuch: Stichprobe=BRD (Ost und West), Erwerbstätige und Praktikanten, Volontärem etc. mit mehr als 10 Wochenstunden, Ausländer mit ausreichend Deutschkenntnissen, Alter ab 15 Jahre, 
* ohne Auszubildenden (und ähnliche Gruppen, z.B. in Beamtenausbildung etc.), Wehrpflichtige, Zivildienstleistende,  kaserniert lebende Berufsgruppen, (n=34,343)

* DRS/MT: zusätzliche Selektion: Nur deutsche Westdeutsche, Alter<66; Arbeitszeit 10-168 Stunden, keine mithelfenden Familienangehörige, keine Arbeitslosen und Azubis
* Soll: 25,513

use "../original_data/BIBB-Erwerbstaetige_1-7/ZA3379 - 1999/ZA3379_v1-1-0.dta", clear

keep if v3==1 				/*Westdeutschland*/
keep if v8<66 				/*Alter 15-65*/
keep if v780==1				/*deutsche Staatsangehörigkeit*/
keep if v29>=1 & v29!=99.9	/*Arbeitszeit in Synopse nur zwischen 10 und 168 Stunden, 99.9=9999 */ 
keep if v117!=6				/*Mithelfende Familienangehörige*/
keep if v762!=1				/*Arbeitslose raus*/
keep if v763!=1				/*Azubis raus*/

d, short

save "../generated_data/bibb1999_har.dta", replace

* es sind noch 24 zuviel 

************************************* 2006 *************************************
 
* Stichprobe: BRD Ost und West, Kernerwerbstätige, Alter 15+ (20,000)

* DRS/MT: zusätzliche Selektion: Nur Westdeutsche, Alter<66, deutsche Staatsangehörigkeit
* Soll: 15,671

*In der ETB 2005/2006 sind die Erwerbstätigen nicht mehr einfach nach Bundes-
*ländern West (inkl. Westberlin) und Ost (inkl. Ostberlin) zu trennen. 
*In den Originaldaten gibt es dazu die Variable "regbez", in der mit den
*Ausprägungen 111 (Berlin, ehemalige westliche Bezirke) und 112 (Berlin, ehe-
*malige östliche Bezirke) unterschieden wird. 
/*

use "O:\BIBB-Baua\Daten, Programme\bibb_baua_et0506_080108.dta", clear

d, short

keep if bula>=1 & bula<=10 | (bula==11 & regbez==111) /*Westdeutschland*/
gen ausb_sel=1
replace ausb_sel=0 if f1109==1 | f1109==3 | f1109==9 
keep if ausb_sel==1                                   /*keine Auszubildenden*/
keep if nation==1                                     /*deutsche Staatsangehörigkeit*/
keep if zpalter>=15 & zpalter<=65                     /*Alter 15-65*/

d, short

* damit wird die Fallzahl aus der Synopse erreicht (15,671)
*/

use "../original_data/BIBB-Erwerbstaetige_1-7/ZA4820 - 2006/ZA4820_v3-0-0.dta", clear

d, short

keep if bula>=1 & bula<=10                            /*Westdeutschland, ohne Westberlin (siehe oben)*/
gen ausb_sel=1
replace ausb_sel=0 if f1109==1 | f1109==3 | f1109==9 
keep if ausb_sel==1                                   /*keine Auszubildenden*/
keep if f1607_01==1                                   /*deutsche Staatsangehörigkeit*/
keep if zpalter>=15 & zpalter<=65                     /*Alter 15-65*/

d, short
* das sind 15,185 Fälle, also 486 zuwenig
* über die regbez-Variable würden 341 Fälle herausgenommen
* 517 Befragte leben in "Westberlin"

save "../generated_data/bibb2006_har.dta", replace

************************************* 2012 *************************************

use "../original_data/BIBB-Erwerbstaetige_1-7/ZA5657 - 2012/ZA5657_v6-0-0.dta", clear

d, short

keep if Bula>=1 & Bula<=10                            /*Westdeutschland, ohne Westberlin (siehe oben)*/
gen ausb_sel=1
replace ausb_sel=0 if F1109==1 | F1109==3 | F1109==9 
keep if ausb_sel==1                                   /*keine Auszubildenden*/
keep if F1607_01==1                                   /*deutsche Staatsangehörigkeit*/
keep if Zpalter>=15 & Zpalter<=65                     /*Alter 15-65*/

d, short

save "../generated_data/bibb2012_har.dta", replace

************************************* 2018 *************************************

use "../original_data/BIBB-Erwerbstaetige_1-7/ZA7574 - 2018/ZA7574_v1-0-0.dta", clear

d, short

keep if Bula>=1 & Bula<=10                            /*Westdeutschland, ohne Westberlin (siehe oben)*/
keep if F1607_01==1                                   /*deutsche Staatsangehörigkeit*/
keep if zpalter>=15 & zpalter<=65                     /*Alter 15-65*/

d, short

save "../generated_data/bibb2018_har.dta", replace
