clear all
*Note that a lot of memory is required to run the linear probability model regressions
set mem 800m
set matsize 800
*CHANGED SO IT CAN RUN
*set matsize 1000
set more off
*cd "~/bls/empirical"
cd "C:\Users\sarah\Documents\R_projects\democracy_war\stata"
capture log close
* log using "./log.files/bls.tables.2.3.6.8.log", replace
use bls.data.dta

********************************************************************************************
********************************************************************************************
***************************BASELINE REGRESSION TABLE 3**************************************
********************************************************************************************
********************************************************************************************

*Spline terms for years since last MID (uses btscs command from Beck, Katz and Tucker in btscs folder)
btscs mzmid1 year dcode, gen(py) nspline(3)

cd "C:\Users\sarah\Documents\R_projects\democracy_war\output"
save bls.data.full.dta, replace
log close
