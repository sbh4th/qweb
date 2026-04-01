insheet race year rate count pop using "annual-mort-1999-2013.txt", clear

* create a few labels
label define race 0 White 1 Black, modify
label values race race
* numlabel race, add
label var race "Race"

* labels for years
forvalues i=0/14 {
  local ylabel `i'
  local yvalue = `i'+1999
  label define year `ylabel' "`yvalue'", modify
  }
label values year year

* generate log mortality rate
gen lnrate = ln(rate)
label var lnrate "Log mortality rate"

* year values without labels
gen year0 = year + 1999
label var year0 "Year of death"

* time-series set-up
tsset race year0

* ITS for blacks in Delaware
itsa lnrate if race==1, treatid(1) trperiod(2003) single replace lag(1) ///
  figure cformat(%4.3f)
  
* now with whites as control group
itsa lnrate, treatid(1) trperiod(2003) replace lag(1) ///
  figure posttrend cformat(%4.3f)




insheet state4 race year3 rate count pop using ///
  "ann-mort-4-states-3yavg.txt", clear

label define state4 0 "Delaware" 1 "Maryland" 2 "Virginia" ///
  3 "West Virginia", modify
label values state4 state4
label var state4 "State"

label define race 0 White 1 Black, modify
label values race race
numlabel race, add
label var race "Race"

forvalues i=0/14 {
  local ylabel `i'
  local yvaluef = `i'+1999
  local yvaluel = `i'+2001
  label define year3 `ylabel' "`yvaluef'-`yvaluel'", modify
  }
label values year3 year3

tw (connected rate year3 if race==0) (connected rate year3 if race==1), ///
  by(state4) 
  
keep if state4==0

tsset race year
  
gen lnrate = ln(rate)

itsa lnrate, treatid(1) trperiod(4) replace lag(1) figure



  
insheet state race year rate count pop using ///
  "ann-mort-states.txt", clear
