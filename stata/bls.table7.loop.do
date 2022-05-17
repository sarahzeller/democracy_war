capture log close

clear all
set more off
set mem 500m
cd "~/bls/empirical/"
log using "./log.files/bls.table7.loop.log", replace
use bls.data.dta
keep dcode ye mzmid1 dem1 dem2 d7n*s majpow logcapr allianced 
aorder


* Prepares control variables for the clogit regression (see bls.tables.2.3.6.8.do for more detail)

btscs mzmid1    year dcode, gen(py) nspline(3)
bysort ye: egen mmzmid1=mean(mzmid1) if dem1~=. & dem2~=.  & majpow~=. & logcapr~=. & alliance~=. & py ~=.
gen dmmzmid1=(mmzmid1==0 | mmzmid1==.)
sum dmmzmid1, de
tab ye if dmmzmid1==1
qui tab ye if dmmzmid1==0, gen(dd)
drop dd1
keep if mzmid1~=. & dem1~=. & dem2~=. & majpow~=. & logcapr~=. & allianced~=.



* Builds regime type dummies, estimates corresponding regressions, and saves estimates in ./table7.loop/ folder
qui tab dem1, gen (d1c1n)
qui tab dem2, gen (d1c2n)


local count = 0
forval low = -6/-2{
	forval high = 2/6{
		di `low'`high'
	if `high'>`low'{
		
		
		local count = `count' + 1
		di "model number: `count'"
		
		capture drop d*s	
		global low_2 = `low'
		global high_2 = `high'
		
		di "min is: `low' and max is:`high'"
		global low_1 = -10
		global high_1 = $low_2 - 1
		
		global low_3 = $high_2 + 1
		global high_3 = +10
		
		gen d1n1 = ($low_1<=dem1 & dem1<=$high_1)
		gen d1n2 = ($low_2<=dem1 & dem1<=$high_2)
		gen d1n3 = ($low_3<=dem1 & dem1<=$high_3)
		gen d2n1 = ($low_1<=dem2 & dem2<=$high_1)
		gen d2n2 = ($low_2<=dem2 & dem2<=$high_2)
		gen d2n3 = ($low_3<=dem2 & dem2<=$high_3)
		gen d11s = (d1n1==1 & d2n1==1) 
		*gen d22s = (d1n2==1 & d2n2==1)
		gen d33s = (d1n3==1 & d2n3==1)
		gen d21s = ((d1n1==1 & d2n2==1)|(d1n2==1 & d2n1==1) )
		gen d31s = ((d1n1==1 & d2n3==1)|(d1n3==1 & d2n1==1) )
		gen d32s = ((d1n2==1 & d2n3==1)|(d1n3==1 & d2n2==1) ) 
		drop d*n*
		clogit mzmid1   d11s d21s d31s d32s d33s  majpow logcapr   alliance   dd* py _sp* if dmmzmid1==0, group(dcode) rob	
		sum d*s if e(sample)	
		estimates save ./table7.loop/ml`low'h`high'.ster, replace
		
		}
	
	}
}	
*

*Sets P-value threshold 
global pvthreshold = .10


*Initializes matrices that contain Table 7 results
matrix num_coeff_negat = J(20,20,.)
matrix num_coeff_negat_sig = J(20,20,.)




forval num = -9/9{
	local posit = `num' + 11
	matrix num_coeff_negat[1,`posit'] = `num'
	matrix num_coeff_negat[`posit',1] = `num'
	
	matrix num_coeff_negat_sig[1,`posit'] = `num'
	matrix num_coeff_negat_sig[`posit',1] = `num'

	
	}
*	


forval low = -6/-2{
	forval high = 2/6{
		di `low'`high'
		
	if `high'>`low'{
		
		
		di "ml`low'h`high'"
		estimates use ./table7.loop/ml`low'h`high'.ster
		estimates replay
		matrix V = e(V)
		local count_negat = 0
		local count_negat_sig = 0
		foreach coeff in d11s d21s d31s d32s d33s{
			matrix v = V["mzmid1:`coeff'","mzmid1:`coeff'"]
			local se_`coeff' = sqrt(v[1,1])
			local z_`coeff' = _b["`coeff'"]/`se_`coeff''
			local pval_`coeff' = 2 * (1 - normal(abs(`z_`coeff'')))
			local b_`coeff' = _b["`coeff'"]
			di `z_`coeff'' "   " `pval_`coeff'' "  " `b_`coeff''
			local count_negat = `count_negat' + (`b_`coeff''<0)
			local count_negat_sig = `count_negat_sig' + (`b_`coeff''<0 & `pval_`coeff''<$pvthreshold )
			}
			
		local row = `low' + 11
		local col = `high' + 11
		matrix num_coeff_negat[`row',`col'] = `count_negat'
		matrix num_coeff_negat_sig[`row',`col'] = `count_negat_sig'

		}
	}
}

*Saves results in Table 7 in ./table7.loop/table7.txt


mat2txt, matrix(num_coeff_negat) sav("./table7.loop/table7.txt") title("num_coeff_negat") replace 
mat2txt, matrix(num_coeff_negat_sig) sav("./table7.loop/table7.txt") title("num_coeff_negat_sig") append 


log close
