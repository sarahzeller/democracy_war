clear all
set more off
set mem 500m
*cd "C:\bls\empirical.2"
cd "C:\Users\sarah\Documents\R_projects\democracy_war\stata"
capture log close
*log using "./log.files/bls.data.log", replace

use bls.data.raw.dta

* Unused variables in BLS

drop autoc? democ? xrreg* xrcomp* xropen* parreg* parcomp* exrec* exconst* polcomp* xconst* ///
	depend* *trade* numstate mzhost* mzjomid*  mzhiactd mzcowwar mzfatal* mzongonm mzjomid  ///
	mzmidn mzkeynum  numGPs syscon


*Defines dcode, the unique dyad identifier
gen dcode=ccode1*1000+ccode2
*this checks if there are unique identifiers for each year (?) and dcode
isid ye dcode

*****************************************
*Transforming variables for regressions
****************************************
*Log of capability ratio
gen logcapr=log(max(cap_1,cap_2)/min(cap_1,cap_2))
drop cap_1 cap_2
*Log of distance
gen logdist=log(distance)
drop distance

*Contiguity dummy variable
/* Note:  
1) land contiguity; 2) contiguous for up to 12 miles of water; 3)
contiguous for 13-24 miles of water; 4) contiguous for 25-150 miles of water; 5)
contiguous for 151-400 miles of water.
*/
codebook contig
gen dircont=(contig<6)
drop contig

*Major power dummy variable
inspect majpow1 majpow2 
gen majpow=(majpow1==1 | majpow2==1)
drop majpow?
*Alliance dummy
gen allianced=(alliance~=4)
replace allianced=. if alliance==.
tab alliance allianced, mis
drop alliance


*Militarized disputes

/*
Drop year-dyads pairs with an ongoing dispute or a joiner dispute
By using the "drop" option in the EUgene one looses observations such that 
in the current period there is an ongoing dispute but in the following there
isn't one.
*/
replace mzmid=. if mzongo==1 | mzjoany==1
drop mzongo mzjoany
*The dependent variables if mzmid at t+1
*tssettime dcode ye, ye
* NEW CODE LINE FOR NEW STATA VERSION
tsset dcode ye, ye
gen mzmid1=f.mzmid
*this first sets the identifier and year for time series stuff, then generates a lead 



********************************************************
***Regime type dummies
*********************************************************
*twoway histogram dem1
*Uses PolityIV data rather than PolityIII defaults:
pwcorr polity1 dem1
pwcorr polity2 dem2
drop dem1 dem2
rename polity1 dem1
rename polity2 dem2

*Dummy variables for each net-democracy level
qui tab dem1, gen (d1c1n)
qui tab dem2, gen (d1c2n)

*The d7 dummies are baseline  in the paper
*where an anocracy is between [-3,3]
gen d7c1n1=d1c1n1+d1c1n2+d1c1n3+d1c1n4+d1c1n5+d1c1n6+d1c1n7
gen d7c1n2=d1c1n8+d1c1n9+d1c1n10+d1c1n11+d1c1n12+d1c1n13+d1c1n14
gen d7c1n3=d1c1n15+d1c1n16+d1c1n17+d1c1n18+d1c1n19+d1c1n20+d1c1n21

gen d7c2n1=d1c2n1+d1c2n2+d1c2n3+d1c2n4+d1c2n5+d1c2n6+d1c2n7
gen d7c2n2=d1c2n8+d1c2n9+d1c2n10+d1c2n11+d1c2n12+d1c2n13+d1c2n14
gen d7c2n3=d1c2n15+d1c2n16+d1c2n17+d1c2n18+d1c2n19+d1c2n20+d1c2n21
*The dbis variables are the alternative definition in Mansfield and Snyder(2002)
*where an anocracy is between [-6,6]
gen dbisc1n1=d1c1n1+d1c1n2+d1c1n3+d1c1n4
gen dbisc1n2=d1c1n5+d1c1n6+d1c1n7+d1c1n8+d1c1n9+d1c1n10+d1c1n11+d1c1n12+d1c1n13+d1c1n14+d1c1n15+d1c1n16+d1c1n17
gen dbisc1n3=d1c1n18+d1c1n19+d1c1n20+d1c1n21

gen dbisc2n1=d1c2n1+d1c2n2+d1c2n3+d1c2n4
gen dbisc2n2=d1c2n5+d1c2n6+d1c2n7+d1c2n8+d1c2n9+d1c2n10+d1c2n11+d1c2n12+d1c2n13+d1c2n14+d1c2n15+d1c2n16+d1c2n17
gen dbisc2n3=d1c2n18+d1c2n19+d1c2n20+d1c2n21


*Mansfield and Snyder regime type transition dummy marking a transition from n1 to n2
*between t-5 and t for their (dbis) and our (d7) anocracy definition
foreach c of numlist 1/2{
         gen dbisanoctransc`c'=(dbisc`c'n2==1 & l5.dbisc`c'n1==1)
         replace dbisanoctransc`c'=. if dbisc`c'n2==. | l5.dbisc`c'n1==.
         gen d7anoctransc`c'=(d7c`c'n2==1 & l5.d7c`c'n1==1)
         replace d7anoctransc`c'=. if d7c`c'n2==. | l5.d7c`c'n1==.

         }
*At least one country in the dyad had a transition
gen dbisanoctransij=(dbisanoctransc1==1 | dbisanoctransc2==1)
replace dbisanoctransij=. if (dbisanoctransc1==. | dbisanoctransc2==.)
gen d7anoctransij=(d7anoctransc1==1 | d7anoctransc2==1)
replace d7anoctransij=. if (d7anoctransc1==. | d7anoctransc2==.)
drop dbisanoctransc?
*Regime matrix combination
foreach c1 of numlist 1/3{
	foreach c2 of numlist 1/3{

	gen d7n`c1'`c2'=d7c1n`c1'+d7c2n`c2'
	replace d7n`c1'`c2'=0 if d7n`c1'`c2'==1
	replace d7n`c1'`c2'=1 if d7n`c1'`c2'==2

	gen dbisn`c1'`c2'=dbisc1n`c1'+dbisc2n`c2'
	replace dbisn`c1'`c2'=0 if dbisn`c1'`c2'==1
	replace dbisn`c1'`c2'=1 if dbisn`c1'`c2'==2

	}
}

**Symmetric definitions for mixed-type dyads in nondirected data
gen d7n21s=d7n21+d7n12
tab d7n21 d7n12 if d7n21s==1
gen d7n32s=d7n32+d7n23
tab d7n32 d7n23 if d7n32s==1
gen d7n31s=d7n31+d7n13
tab d7n31 d7n13 if d7n31s==1
gen d7n11s=d7n11
gen d7n22s=d7n22
gen d7n33s=d7n33

gen dbisn21s=dbisn21+dbisn12
gen dbisn32s=dbisn32+dbisn23
gen dbisn31s=dbisn31+dbisn13
gen dbisn11s=dbisn11
gen dbisn22s=dbisn22
gen dbisn33s=dbisn33

drop d7n?? dbisn?? d1c* d7c?n? d7anoctransc? dbisc?n?

aorder
order dcode ye
*/
*Mzmid does not exist after 2000, this saves space: 
drop if ye>2000
save bls.data.dta, replace
log close
