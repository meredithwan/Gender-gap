set more off, permanently
set matsize 11000
cd "insert working directory name"
// log file to document everything done
log using gender_gap.log, append

// use "epcg17.dta", clear
use "gender_gap.dta", clear

// recoding variables

// gender
gen female = 1 if gender == "F"
replace female = 0 if gender == "M"

// place of birth
encode bthst, gen(birthplace)

// institution type
encode bacarn, gen(inst_type)
replace inst_type = . if bacarn == "L" | bacarn == "M"

// year of award for degree
replace bayr = . if bayr == 9998

// create citizenship variable
// 998 is for US citizenship
encode fnccd, gen(citizen)

// location of school awarding first bachelor's degree
encode bast, gen(location)
replace location = . if bast == "998"

// if have children in the household
gen children = 1 if chlvin == "Y"
replace children = 0 if chlvin == "N"

// if have physical difficulties
gen physical_dif = 1 if difno == "N"
replace physical_dif = 0 if difno == "Y"

// father's education level
encode eddad, gen(father_edu)
replace father_edu = . if eddad == "8"

// mother's education level
encode edmom, gen(mother_edu)
replace mother_edu = . if edmom == "8"

// employer sector
encode emsecdt, gen(emp_sector)
replace emp_sector = . if emsecdt == "L"

// company size
encode emsize, gen(emp_size)
replace emp_size = . if emsize == "L"

// employer location
encode emst, gen(emp_location)
replace emp_location = . if emst == "998"

// if have disability
gen disability = 1 if hcapin == "Y"
replace disability = 0 if hcapin == "N"

// marital status
gen marst = 1 if marind == "Y"
replace marst = 0 if marind == "N"

// race
encode racem, gen(race)

// survey cohort
encode cohort, gen(svy_cohort)

// majors
// unknown fields are excluded
encode nbamed, gen(major)
replace major = . if nbamed == "769950" | nbamed == "999989"

// fields of study
encode nbamemg, gen(field)
replace field = . if nbameng == "98"

// outcome variables of interest
// annual earnings (salary from principal job)
replace salary = . if salary == 9999998
// converted to real terms using 2017 CPI
// chained base year from 1982 to 1984
gen real_inc = (salary/245.120)*100
gen lnInc = ln(real_inc)

// unemployment
gen unemp = 1 if lfstat == "2"
replace unemp = 0 if lfstat == "1"

// full-time permanent employment
gen FT_perm = 0 if lfstat == "1" | lfstat == "2"
replace hrswk = . if hrswk == 98
replace FT_perm = 1 if hrswk >= 35

// job satisfaction
// very satisfied and somewhat satisfied are considered as satisfied
gen job_satis = 1 if jobsatis == "1" | jobsatis == "2"
replace job_satis = 0 if jobsatis == "3" | jobsatis == "4"

// job match
// closely related and somewhat related are considered as job match
gen JM = 1 if ocedrlp == "1" | ocedrlp == "2"
replace JM = 0 if ocedrlp == "3"

// level to cluster standard errors on
// no data of exact school that awarded the degree
// no data on year enrolled in college
// cohort refers to year that they received the degree (3 year intervals)
// school type-field of study-cohort level
encode nbamemg, gen(faculty)
replace faculty = . if nbamemg == "8"

replace baayr3 = . if baayr3 == 9998

egen school_field_cohort = group(inst_type faculty baayr3)

// double major or minor programmes
gen double_maj = 1 if nbasemg != "8" & !missing(nbasemg)
replace double_maj = 0 if nbasemg == "8"

// potential experience variables are dropped due to multicollinearity

// homogeneous effects
// interested only in population with only a single bachelor's degree
// meaning no additional bachelor's degree, no graduate degrees
keep if bsdgn == 1
tab dgrdg
// only keep those with Bachelor's as highest degree type
keep if dgrdg == "1"

save gender_gap, replace

// run from here each time
global controls age i.birthplace i.inst_type i.bayr i.citizen i.location /// 
children physical_dif i.father_edu i.mother_edu i.emp_sector i.emp_size /// 
i.emp_location disability marst i.race i.svy_cohort i.major double_maj

global ylist lnInc FT_perm job_satis JM

foreach y in $ylist {
eststo: quietly reg `y' female $controls [pw = wtsurvy], cluster(school_field_cohort)
}

esttab, keep(female) b se ar2, using homo_reg.tex, star(* 0.10 ** 0.05 *** 0.01) replace

// heterogeneous effects by fields of study (major group)
tab faculty

// Computer and mathematical sciences
eststo clear

keep if faculty == 1

foreach y in $ylist {
eststo: quietly reg `y' female $controls [pw = wtsurvy], cluster(school_field_cohort)
}

esttab, keep(female) b se ar2, using het_comp_math_reg.tex, star(* 0.10 ** 0.05 *** 0.01) replace

// Biological, agricultural and environmental life sciences
use "gender_gap.dta", clear

eststo clear

keep if faculty == 2

foreach y in $ylist {
eststo: quietly reg `y' female $controls [pw = wtsurvy], cluster(school_field_cohort)
}

esttab, keep(female) b se ar2, using het_natural_sci_reg.tex, star(* 0.10 ** 0.05 *** 0.01) replace

// Physical and related sciences
use "gender_gap.dta", clear

eststo clear

keep if faculty == 3

foreach y in $ylist {
eststo: quietly reg `y' female $controls [pw = wtsurvy], cluster(school_field_cohort)
}

esttab, keep(female) b se ar2, using het_physical_sci_reg.tex, star(* 0.10 ** 0.05 *** 0.01) replace

// Social and related sciences
use "gender_gap.dta", clear

eststo clear

keep if faculty == 4

foreach y in $ylist {
eststo: quietly reg `y' female $controls [pw = wtsurvy], cluster(school_field_cohort)
}

esttab, keep(female) b se ar2, using het_social_sci_reg.tex, star(* 0.10 ** 0.05 *** 0.01) replace

// Engineering
use "gender_gap.dta", clear

eststo clear

keep if faculty == 5

foreach y in $ylist {
eststo: quietly reg `y' female $controls [pw = wtsurvy], cluster(school_field_cohort)
}

esttab, keep(female) b se ar2, using het_eng_reg.tex, star(* 0.10 ** 0.05 *** 0.01) replace

// Science and engineering related fields
use "gender_gap.dta", clear

eststo clear

keep if faculty == 6

foreach y in $ylist {
eststo: quietly reg `y' female $controls [pw = wtsurvy], cluster(school_field_cohort)
}

esttab, keep(female) b se ar2, using het_sci_eng_related_reg.tex, star(* 0.10 ** 0.05 *** 0.01) replace

// Non-science and engineering related fields
use "gender_gap.dta", clear

eststo clear

keep if faculty == 7

foreach y in $ylist {
eststo: quietly reg `y' female $controls [pw = wtsurvy], cluster(school_field_cohort)
}

esttab, keep(female) b se ar2, using het_non_sci_eng_related_reg.tex, star(* 0.10 ** 0.05 *** 0.01) replace

log close
