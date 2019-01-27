*MASON & MYERS: THE EFFECTS OF THE FOOD RESERVE AGENCY ON MAIZE MARKET PRICES IN ZAMBIA
*Syntax for all analysis except for threshold estimation and testing, which was done in GAUSS.
*GAUSS syntax file is Mason_&_Myers_GAUSS_code.prg, and GAUSS data file is Mason_&_Myers_dataset_for_GAUSS.csv.
*Stata 11.2 for Mac
*Code written by Nicole Mason, adapted from code for Jayne et al. (2008) - "The effects of NCPB marketing policies on maize market prices in Kenya".

clear
clear matrixset matsize 700drop _allset memory 200mset more off

*Set working directory
cd "/Users/nicolemason/Documents/Dissertation/Essay1_FRA_maize_prices/"

*set graphics schemeset scheme s1mono
graph set window fontface "Times New Roman"
graph set eps fontface "Times New Roman"


***************************************************************************************************************************************************************************
***************************************************************************************************************************************************************************
***************************************************************************** PREPARE DATASET *****************************************************************************
***************************************************************************************************************************************************************************
***************************************************************************************************************************************************************************


*Get data file
use "tmp/FRA_effects_on_market_prices_dataset.dta", clear

des

*tsset data
tsset date, format(%tmm._CY)

*FRA began its activities in July 1996 (date 438); data on FRA sales available through December 2008 only (date 587). 
*Drop observations outside of this period.
keep if date>=438 & date<=587 
gen date_num=date

order year month date date_num mktyear

*** Compute net purchases ***
gen NETPURCH=frapurchmt-frasalesmt
lab var NETPURCH "FRA net purchases (MT)"
sum NETPURCH, det

*** Compute cumulative FRA purchases by marketing year ***
sort mktyear date
by mktyear: gen cumpurch=sum(frapurchmt)
lab var cumpurch "Cumulative FRA marketing year purchases (MT)"

*** Compute cumulative FRA purchases up to t-1 by marketing year ***
sort date
gen cumpurch_1=L.cumpurch
*Replace May values with 0 (restart marketing year) & set July 1996 value = 0 (beginning of time series -- no FRA before that)
replace cumpurch_1=0 if month==5 | date==438
lab var cumpurch_1 "Cumulative FRA marketing year purchases through month t-1 (MT)"

*** Compute FRA buy price premium ***
gen BPP= pfrabuymr-CHOMA
lab var BPP "FRA buy price premium (ZMK/kg) (FRA buy price minus Choma wholesale price)"
replace BPP=0 if date>=460 & date<=509

sum BPP

*** Compute FRA sell price premia *** >>>> zero for date<455 because FRA didn't sell maize before that
gen SPP=pfrasellmr-LUSAKA
replace SPP =0 if date<455
lab var SPP "FRA sell price premium (ZMK/kg) (FRA sell price minus Lusaka wholesale price)"

** Convert Mchinji and SAFEX prices from Malawian Kwacha and South African Rand to ZMK (and adjust by tariff) **
gen SAFEX=(wsafex_RAND/rand_to_usd)*zmk_to_usd*(1+(tariff/100))
lab var SAFEX "SAFEX wholesale maize price including tariff (ZMK/kg)"

gen MCHINJI=(rmchinji_MWK/mwk_to_usd)*zmk_to_usd*(1+(tariff_fta/100))
lab var MCHINJI "Mchinji retail maize price including tarff (ZMK/kg)"

*** Construct candidate (non-time) threshold variables ***

*Cumulative FRA smallholder market share through month t-1 = cumulative FRA marketing year purchases through month t-1 / expected smallholder maize sales during mkt year
gen mktshare= (cumpurch_1/esales)*100
lab var mktshare "Cumulative FRA smallholder market share through month (t-1, %)"

*Remaining smallholder marketable surplus available to private sector as of t-1 PER CAPITA (kg)
gen msleft= esales-cumpurch_1
lab var msleft "Smallholder marketable surplus remaining after FRA purchases (t-1, MT)"
gen msleftpc= (msleft*1000)/population
lab var msleftpc "Smallholder marketable surplus remaining after FRA purchases (t-1, kg/capita)"

*Smallholder maize quantity harvested (kg/capita)
lab var aprod "Smallholder maize quantity harvested (MT)"
gen aprodpc=(aprod*1000)/population
lab var aprodpc "Smallholder maize quantity harvested (kg/capita)"



*Keep only key variables
keep year month date date_num mktyear frapurchmt frasalesmt NETPURCH BPP SPP /// 
LUSAKA CHOMA SAFEX MCHINJI ///
mktshare msleftpc aprodpc exportban mznetimports 

order year month date date_num mktyear frapurchmt frasalesmt NETPURCH BPP SPP /// 
LUSAKA CHOMA SAFEX MCHINJI ///
mktshare msleftpc aprodpc exportban mznetimports 

*Save full dataset
save "tmp/Mason_&_Myers_dataset_full.dta", replace

*Save dataset for Gauss (Choma, Lusaka, SAFEX, MCHINJI, BPP, SPP, mktshare, msleftpc, aprodpc)
use "tmp/Mason_&_Myers_dataset_full.dta", clear
keep date CHOMA LUSAKA SAFEX MCHINJI BPP SPP mktshare msleftpc aprodpc
order date CHOMA LUSAKA SAFEX MCHINJI BPP SPP mktshare msleftpc aprodpc
save "tmp/Mason_&_Myers_dataset_for_GAUSS.dta", replace



**** Get full dataset
use "tmp/Mason_&_Myers_dataset_full.dta", clear

*tsset data
tsset date, format(%tmm._CY)

** SUMMARY STATS **
tabstat LUSAKA CHOMA SPP BPP SAFEX MCHINJI NETPURCH, stats(n mean sd p1 p5 p10 p25 p50 p75 p90 p95 p99) 



***************************************************************************************************************************************************************************
***************************************************************************************************************************************************************************
****************************************************************************** UNIT ROOT TESTS ****************************************************************************
***************************************************************************************************************************************************************************
***************************************************************************************************************************************************************************

*(a) KPSS H0: trend stationary; H1: unit root (using automatic bandwidth selection and autocovariance function weighted by quadratic spectral kernel)
kpss CHOMA, auto qs
kpss LUSAKA, auto qs
kpss SAFEX, auto qs
kpss MCHINJI, auto qs
kpss BPP, auto qs
kpss SPP, auto qs
kpss NETPURCH, auto qs

*(b) KPSS H0: level stationary; H1: unit root (using automatic bandwidth selection and autocovariance function weighted by quadratic spectral kernel)
kpss CHOMA, auto qs notrend
kpss LUSAKA, auto qs notrend
kpss SAFEX, auto qs notrend
kpss MCHINJI, auto qs notrend
kpss BPP, auto qs notrend
kpss SPP, auto qs notrend
kpss NETPURCH, auto qs notrend

*(c) ADF H0: unit root; H1: trend stationary
dfuller CHOMA, regress trend lag(1)
dfuller LUSAKA, regress trend lag(1)dfuller SAFEX, regress trend lag(1)
dfuller MCHINJI, regress trend lag(1)dfuller BPP, regress trend lag(1)
dfuller SPP, regress trend lag(1)dfuller NETPURCH, regress trend lag(1)

*(d) ADF H0: unit root; H1: level stationary
dfuller CHOMA,  regress lag(1)
dfuller LUSAKA,  regress lag(1)dfuller SAFEX,  regress lag(1)
dfuller MCHINJI,  regress lag(1)dfuller BPP,  regress lag(1)dfuller SPP,  regress lag(1)dfuller NETPURCH,  regress lag(1)
*(e) PP H0: unit root; H1: trend stationary
pperron CHOMA,  trend
pperron LUSAKA,  trend
pperron SAFEX,  trendpperron MCHINJI,  trendpperron BPP,  trend
pperron SPP,  trendpperron NETPURCH,  trend

*(f) PP H0: unit root; H1: level stationary
pperron CHOMA
pperron LUSAKApperron SAFEX
pperron MCHINJIpperron BPPpperron SPPpperron NETPURCH



***************************************************************************************************************************************************************************
***************************************************************************************************************************************************************************
*************************************************************************** DETERMINE LAG ORDER ***************************************************************************
***************************************************************************************************************************************************************************
***************************************************************************************************************************************************************************

set more off
varsoc CHOMA LUSAKA BPP SPP SAFEX MCHINJI, maxlag(4)

*var CHOMA LUSAKA BPP SPP SAFEX MCHINJI, lags(1/1)
*Autocorrelation

*var CHOMA LUSAKA BPP SPP SAFEX MCHINJI, lags(1/2)
*Autocorrelation

var CHOMA LUSAKA BPP SPP SAFEX MCHINJI, lags(1/3)
*No autocorrelation

test [CHOMA]L1.MCHINJI [CHOMA]L2.MCHINJI [CHOMA]L3.MCHINJI  ///
[LUSAKA]L1.MCHINJI [LUSAKA]L2.MCHINJI [LUSAKA]L3.MCHINJI ///
[BPP]L1.MCHINJI [BPP]L2.MCHINJI [BPP]L3.MCHINJI ///
[SPP]L1.MCHINJI [SPP]L2.MCHINJI [SPP]L3.MCHINJI  ///
[SAFEX]L1.MCHINJI [SAFEX]L2.MCHINJI [SAFEX]L3.MCHINJI 

test [CHOMA]L1.SAFEX [CHOMA]L2.SAFEX [CHOMA]L3.SAFEX  ///
[LUSAKA]L1.SAFEX [LUSAKA]L2.SAFEX [LUSAKA]L3.SAFEX  ///
[BPP]L1.SAFEX [BPP]L2.SAFEX [BPP]L3.SAFEX  ///
[SPP]L1.SAFEX [SPP]L2.SAFEX [SPP]L3.SAFEX  ///
[MCHINJI]L1.SAFEX [MCHINJI]L2.SAFEX [MCHINJI]L3.SAFEX 

*Predict residuals for each equation
predict uprodarea, equation(#1) resid
predict uconsarea, equation(#2) resid
predict ubpp, equation(#3) resid
predict uspp, equation(#4) resid
predict usafex, equation(#5) resid
predict umchinji, equation(#6) resid

*Test for autocorrelation
corrgram uprodarea, lags(12)
corrgram uconsarea, lags(12)
corrgram usafex, lags(12)
corrgram umchinji, lags(12)
corrgram ubpp, lags(12)
corrgram uspp, lags(12)

drop uprodarea uconsarea ubpp uspp usafex umchinji 


***************************************************************************************************************************************************************************
***************************************************************************************************************************************************************************
***************************************************************** IMPULSE RESPONSE FUNCTIONS - LINEAR VAR *****************************************************************
***************************************************************************************************************************************************************************
***************************************************************************************************************************************************************************

set more off
var SAFEX MCHINJI CHOMA LUSAKA BPP SPP, lags(1/3)
irf set tmp/LINEAR, replace
irf create LINEAR, order(SAFEX MCHINJI CHOMA LUSAKA BPP SPP) step(24)
irf table oirf, noci impulse(BPP SPP) response(CHOMA LUSAKA) 
irf graph oirf, noci impulse(BPP SPP) response(CHOMA LUSAKA) /// 
				byopts(rows(3) rescale note("")) xlabel(0(2)24, labsize(medsmall)) xmtick(0(1)24, labsize(medsmall)) xtitle("Months", size(medium)) ///
				ylabel(, labsize(medsmall)) ytitle("ZMK/kg", size(medium)) 
graph export "graphs/Journal_article/IRF_Choma_Lusaka_BPP_SPP_SAFEX_Mchinji.eps", as(eps) preview(on) replace
*With 95% confidence interval
irf graph oirf, impulse(BPP SPP) response(CHOMA LUSAKA) /// 
				byopts(rows(3) rescale note("") ) xlabel(0(2)24, labsize(medsmall)) xmtick(0(1)24, labsize(medsmall)) xtitle("Months", size(medium)) ///
				ylabel(, labsize(medsmall)) ytitle("ZMK/kg", size(medium)) 
graph export "graphs/Journal_article/IRF_Choma_Lusaka_BPP_SPP_SAFEX_Mchinji-confidence_intervals.eps", as(eps) preview(on) replace


*****Change recursive ordering
set more off
var SAFEX MCHINJI CHOMA LUSAKA BPP SPP, lags(1/3)
irf set tmp/LINEAR_REVERSE, replace
irf create LINEAR_REVERSE, order(SAFEX MCHINJI CHOMA LUSAKA SPP BPP) step(24)
irf table oirf, noci impulse(BPP SPP) response(CHOMA LUSAKA) 
irf graph oirf, noci impulse(BPP SPP) response(CHOMA LUSAKA) /// 
				byopts(rows(3) rescale note("")) xlabel(0(2)24, labsize(medsmall)) xmtick(0(1)24, labsize(medsmall)) xtitle("Months", size(medium)) ///
				ylabel(, labsize(medsmall)) ytitle("ZMK/kg", size(medium)) 
graph export "graphs/Journal_article/IRF_Choma_Lusaka_BPP_SPP_SAFEX_Mchinji-reverse.eps", as(eps) preview(on) replace
*With 95% confidence interval
irf graph oirf, impulse(BPP SPP) response(CHOMA LUSAKA) /// 
				byopts(rows(3) rescale note("")) xlabel(0(2)24, labsize(medsmall)) xmtick(0(1)24, labsize(medsmall)) xtitle("Months", size(medium)) ///
				ylabel(, labsize(medsmall)) ytitle("ZMK/kg", size(medium)) 
graph export "graphs/Journal_article/IRF_Choma_Lusaka_BPP_SPP_SAFEX_Mchinji-reverse-confidence_intervals.eps", as(eps) preview(on) replace



***************************************************************************************************************************************************************************
***************************************************************************************************************************************************************************
*********************************************** REDUCED FORM LINEAR VAR ESTIMATION & "NO FRA" MARKET PRICES SIMULATION ****************************************************
***************************************************************************************************************************************************************************
***************************************************************************************************************************************************************************

**************************************************
***************     BASE MODEL     ***************
**************************************************

*Estimate RF VAR
set more off
var CHOMA LUSAKA SAFEX MCHINJI BPP SPP , lags(1/3)
outreg2 using outreg, excel label tstat pdec (3) bdec (3) replace

*Predict residuals 
predict UCHOMA, equation(#1) resid
predict ULUSAKA, equation(#2) resid

/* Compute Simulated Prices in the Absence of FRA - Adapted from Jayne et al. (2008) */

capture program drop varcom
program define varcom

      mkmat CHOMA LUSAKA UCHOMA ULUSAKA BPP SPP SAFEX MCHINJI
      matrix pchosim = CHOMA
      matrix plsksim = LUSAKA
      matrix psfxsim = SAFEX
      matrix pmchsim = MCHINJI
      

      scalar i = 4
      while i <= 150 {
	
            matrix pchosim[i,1] = [#1]_b[_cons]                                                          /*
		  */ +[#1]_b[L.CHOMA]*pchosim[i-1,1]+[#1]_b[L2.CHOMA]*pchosim[i-2,1]+[#1]_b[L3.CHOMA]*pchosim[i-3,1] /*
		  */ +[#1]_b[L.LUSAKA]*plsksim[i-1,1]+[#1]_b[L2.LUSAKA]*plsksim[i-2,1]+[#1]_b[L3.LUSAKA]*plsksim[i-3,1] /*
		  */ +[#1]_b[L.SAFEX]*psfxsim[i-1,1]+[#1]_b[L2.SAFEX]*psfxsim[i-2,1]+[#1]_b[L3.SAFEX]*psfxsim[i-3,1] /*
		  */ +[#1]_b[L.MCHINJI]*pmchsim[i-1,1]+[#1]_b[L2.MCHINJI]*pmchsim[i-2,1]+[#1]_b[L3.MCHINJI]*pmchsim[i-3,1] /*
		  */ +UCHOMA[i,1]

            matrix plsksim[i,1] = [#2]_b[_cons]                                                          /*
		  */ +[#2]_b[L.CHOMA]*pchosim[i-1,1]+[#2]_b[L2.CHOMA]*pchosim[i-2,1]+[#2]_b[L3.CHOMA]*pchosim[i-3,1] /*
		  */ +[#2]_b[L.LUSAKA]*plsksim[i-1,1]+[#2]_b[L2.LUSAKA]*plsksim[i-2,1]+[#2]_b[L3.LUSAKA]*plsksim[i-3,1]  /*
		  */ +[#2]_b[L.SAFEX]*psfxsim[i-1,1]+[#2]_b[L2.SAFEX]*psfxsim[i-2,1]+[#2]_b[L3.SAFEX]*psfxsim[i-3,1] /*
		  */ +[#2]_b[L.MCHINJI]*pmchsim[i-1,1]+[#2]_b[L2.MCHINJI]*pmchsim[i-2,1]+[#2]_b[L3.MCHINJI]*pmchsim[i-3,1] /*
		  */ +ULUSAKA[i,1]

/* Make Readable for Accuracy Test 

matrix pchosim[i,1] = pchosim[i,1]                                                               /*
          */ +[#1]_b[L.BPP]*BPP[i-1,1]+[#1]_b[L2.BPP]*BPP[i-2,1]+[#1]_b[L3.BPP]*BPP[i-3,1] /*
		  */ +[#1]_b[L.SPP]*SPP[i-1,1]+[#1]_b[L2.SPP]*SPP[i-2,1]+[#1]_b[L3.SPP]*SPP[i-3,1] 

matrix plsksim[i,1] = plsksim[i,1]                                                               /*
          */ +[#2]_b[L.BPP]*BPP[i-1,1]+[#2]_b[L2.BPP]*BPP[i-2,1]+[#2]_b[L3.BPP]*BPP[i-3,1] /*
		  */ +[#2]_b[L.SPP]*SPP[i-1,1]+[#2]_b[L2.SPP]*SPP[i-2,1]+[#2]_b[L3.SPP]*SPP[i-3,1]

 End of accuracy test */

      	scalar i = i + 1 
      	}

    svmat pchosim, name(CHOMASIM)
	svmat plsksim, name(LUSAKASIM)

end

varcom

*Graph Choma historical and simulated prices
twoway (line CHOMA date in 4/150, lpattern(solid) clcolor(black) clwidth(medthick)  cmissing(n)) ///
(line CHOMASIM1 date in 4/150, lpattern(shortdash) clcolor(black) clwidth(medthick)  cmissing(n)), ///
ysize (3.5) xsize(6) ///ytitle("Choma wholesale maize price (ZMK/kg)", size(medsmall) ) ///ylabel(#10, angle(horizontal) format(%9.0gc) labsize(medsmall)) /// xtitle("") ///xlabel(#14,  angle(45) labsize(medsmall) tstyle(major) valuelabel) ///
xmtick(##12) ///legend(row(1) size(medsmall) ///lab(1 "Historical") ///lab(2 "Simulated"))
graph export "graphs/Journal_article/CHOMASIM_Choma_Lusaka_BPP_SPP_SAFEX_Mchinji_linear.eps", as(eps) preview(on) replace


*Graph Lusaka historical and simulated prices
twoway (line LUSAKA date in 4/150, lpattern(solid) clcolor(black) clwidth(medthick)  cmissing(n)) ///
(line LUSAKASIM1 date in 4/150, lpattern(shortdash) clcolor(black) clwidth(medthick)  cmissing(n)), ///
ysize (3.5) xsize(6) ///ytitle("Lusaka wholesale maize price (ZMK/kg)", size(medsmall) ) ///ylabel(#10, angle(horizontal) format(%9.0gc) labsize(medsmall)) /// xtitle("") ///xlabel(#14,  angle(45) labsize(medsmall) tstyle(major) valuelabel) ///
xmtick(##12) ///legend(row(1) size(medsmall) ///lab(1 "Historical") ///lab(2 "Simulated"))
graph export "graphs/Journal_article/LUSAKASIM_Choma_Lusaka_BPP_SPP_SAFEX_Mchinji_linear.eps", as(eps) preview(on) replace

tabstat CHOMA CHOMASIM1 LUSAKA LUSAKASIM1 in 4/150, stat(n mean sd cv) col(var)
tabstat CHOMA CHOMASIM1 LUSAKA LUSAKASIM1 in 4/150 if date<522, stat(n mean sd cv) col(var)
tabstat CHOMA CHOMASIM1 LUSAKA LUSAKASIM1 if date>=522, stat(n mean sd cv) col(var)


**************************************************
*************** INCLUDING NETPURCH ***************
**************************************************

clear
clear matrixset matsize 700drop _allset memory 200mset more off

*set graphics schemeset scheme s1mono
graph set window fontface "Times New Roman"
graph set eps fontface "Times New Roman"

*Set working directory
cd "/Users/nicolemason/Documents/Dissertation/Essay1_FRA_maize_prices/"

**** Get full dataset
use "tmp/Mason_&_Myers_dataset_full.dta", clear

generate t=_n

*tsset data
tsset date, format(%tmm._CY)

*Estimate RF VAR
set more off
var CHOMA LUSAKA SAFEX MCHINJI BPP SPP NETPURCH, lags(1/3)

*Predict residuals 
predict UCHOMA, equation(#1) resid
predict ULUSAKA, equation(#2) resid

/* Compute Simulated Prices in the Absence of FRA - Adapted from Jayne et al. (2008) */

capture program drop varcom
program define varcom

      mkmat CHOMA LUSAKA UCHOMA ULUSAKA BPP SPP NETPURCH SAFEX MCHINJI
      matrix pchosim = CHOMA
      matrix plsksim = LUSAKA
      matrix psfxsim = SAFEX
      matrix pmchsim = MCHINJI
      

      scalar i = 4
      while i <= 150 {
	
            matrix pchosim[i,1] = [#1]_b[_cons]                                                          /*
		  */ +[#1]_b[L.CHOMA]*pchosim[i-1,1]+[#1]_b[L2.CHOMA]*pchosim[i-2,1]+[#1]_b[L3.CHOMA]*pchosim[i-3,1] /*
		  */ +[#1]_b[L.LUSAKA]*plsksim[i-1,1]+[#1]_b[L2.LUSAKA]*plsksim[i-2,1]+[#1]_b[L3.LUSAKA]*plsksim[i-3,1] /*
		  */ +[#1]_b[L.SAFEX]*psfxsim[i-1,1]+[#1]_b[L2.SAFEX]*psfxsim[i-2,1]+[#1]_b[L3.SAFEX]*psfxsim[i-3,1] /*
		  */ +[#1]_b[L.MCHINJI]*pmchsim[i-1,1]+[#1]_b[L2.MCHINJI]*pmchsim[i-2,1]+[#1]_b[L3.MCHINJI]*pmchsim[i-3,1] /*
		  */ +UCHOMA[i,1]

            matrix plsksim[i,1] = [#2]_b[_cons]                                                          /*
		  */ +[#2]_b[L.CHOMA]*pchosim[i-1,1]+[#2]_b[L2.CHOMA]*pchosim[i-2,1]+[#2]_b[L3.CHOMA]*pchosim[i-3,1] /*
		  */ +[#2]_b[L.LUSAKA]*plsksim[i-1,1]+[#2]_b[L2.LUSAKA]*plsksim[i-2,1]+[#2]_b[L3.LUSAKA]*plsksim[i-3,1]  /*
		  */ +[#2]_b[L.SAFEX]*psfxsim[i-1,1]+[#2]_b[L2.SAFEX]*psfxsim[i-2,1]+[#2]_b[L3.SAFEX]*psfxsim[i-3,1] /*
		  */ +[#2]_b[L.MCHINJI]*pmchsim[i-1,1]+[#2]_b[L2.MCHINJI]*pmchsim[i-2,1]+[#2]_b[L3.MCHINJI]*pmchsim[i-3,1] /*
		  */ +ULUSAKA[i,1]

/* Make Readable for Accuracy Test 

matrix pchosim[i,1] = pchosim[i,1]                                                               /*
          */ +[#1]_b[L.BPP]*BPP[i-1,1]+[#1]_b[L2.BPP]*BPP[i-2,1]+[#1]_b[L3.BPP]*BPP[i-3,1] /*
		  */ +[#1]_b[L.SPP]*SPP[i-1,1]+[#1]_b[L2.SPP]*SPP[i-2,1]+[#1]_b[L3.SPP]*SPP[i-3,1] /*
		  */ +[#1]_b[L.NETPURCH]*NETPURCH[i-1,1]+[#1]_b[L2.NETPURCH]*NETPURCH[i-2,1]+[#1]_b[L3.NETPURCH]*NETPURCH[i-3,1]

matrix plsksim[i,1] = plsksim[i,1]                                                               /*
          */ +[#2]_b[L.BPP]*BPP[i-1,1]+[#2]_b[L2.BPP]*BPP[i-2,1]+[#2]_b[L3.BPP]*BPP[i-3,1] /*
		  */ +[#2]_b[L.SPP]*SPP[i-1,1]+[#2]_b[L2.SPP]*SPP[i-2,1]+[#2]_b[L3.SPP]*SPP[i-3,1] /*
		  */ +[#2]_b[L.NETPURCH]*NETPURCH[i-1,1]+[#2]_b[L2.NETPURCH]*NETPURCH[i-2,1]+[#2]_b[L3.NETPURCH]*NETPURCH[i-3,1]

 End of accuracy test */

      	scalar i = i + 1 
      	}

    svmat pchosim, name(CHOMASIM)
	svmat plsksim, name(LUSAKASIM)

end

varcom

*Graph Choma historical and simulated prices
twoway (line CHOMA date in 4/150, lpattern(solid) clcolor(black) clwidth(medthick)  cmissing(n)) ///
(line CHOMASIM1 date in 4/150, lpattern(shortdash) clcolor(black) clwidth(medthick)  cmissing(n)), ///
ysize (3.5) xsize(6) ///ytitle("Choma wholesale maize price (ZMK/kg)", size(medsmall) ) ///ylabel(#10, angle(horizontal) format(%9.0gc) labsize(medsmall)) /// xtitle("") ///xlabel(#14,  angle(45) labsize(medsmall) tstyle(major) valuelabel) ///
xmtick(##12) ///legend(row(1) size(medsmall) ///lab(1 "Historical") ///lab(2 "Simulated"))


*Graph Lusaka historical and simulated prices
twoway (line LUSAKA date in 4/150, lpattern(solid) clcolor(black) clwidth(medthick)  cmissing(n)) ///
(line LUSAKASIM1 date in 4/150, lpattern(shortdash) clcolor(black) clwidth(medthick)  cmissing(n)), ///
ysize (3.5) xsize(6) ///ytitle("Lusaka wholesale maize price (ZMK/kg)", size(medsmall) ) ///ylabel(#10, angle(horizontal) format(%9.0gc) labsize(medsmall)) /// xtitle("") ///xlabel(#14,  angle(45) labsize(medsmall) tstyle(major) valuelabel) ///
xmtick(##12) ///legend(row(1) size(medsmall) ///lab(1 "Historical") ///lab(2 "Simulated"))

tabstat CHOMA CHOMASIM1 LUSAKA LUSAKASIM1 in 4/150, stat(n mean sd cv) col(var)
tabstat CHOMA CHOMASIM1 LUSAKA LUSAKASIM1 in 4/150 if date<522, stat(n mean sd cv) col(var)
tabstat CHOMA CHOMASIM1 LUSAKA LUSAKASIM1 if date>=522, stat(n mean sd cv) col(var)




**************************************************
************** INCLUDING EXPORT BAN **************
**************************************************

clear
clear matrixset matsize 700drop _allset memory 200mset more off

*set graphics schemeset scheme s1mono
graph set window fontface "Times New Roman"
graph set eps fontface "Times New Roman"

*Set working directory
cd "/Users/nicolemason/Documents/Dissertation/Essay1_FRA_maize_prices/"

**** Get full dataset
use "tmp/Mason_&_Myers_dataset_full.dta", clear

generate t=_n

*tsset data
tsset date, format(%tmm._CY)

*Estimate RF VAR
set more off
var CHOMA LUSAKA SAFEX MCHINJI BPP SPP exportban, lags(1/3)

*Predict residuals 
predict UCHOMA, equation(#1) resid
predict ULUSAKA, equation(#2) resid

/* Compute Simulated Prices in the Absence of FRA - Adapted from Jayne et al. (2008) */

capture program drop varcom
program define varcom

      mkmat CHOMA LUSAKA UCHOMA ULUSAKA BPP SPP exportban SAFEX MCHINJI
      matrix pchosim = CHOMA
      matrix plsksim = LUSAKA
      matrix psfxsim = SAFEX
      matrix pmchsim = MCHINJI
      matrix expbansim = exportban
      

      scalar i = 4
      while i <= 150 {
	
            matrix pchosim[i,1] = [#1]_b[_cons]                                                          /*
		  */ +[#1]_b[L.CHOMA]*pchosim[i-1,1]+[#1]_b[L2.CHOMA]*pchosim[i-2,1]+[#1]_b[L3.CHOMA]*pchosim[i-3,1] /*
		  */ +[#1]_b[L.LUSAKA]*plsksim[i-1,1]+[#1]_b[L2.LUSAKA]*plsksim[i-2,1]+[#1]_b[L3.LUSAKA]*plsksim[i-3,1] /*
		  */ +[#1]_b[L.SAFEX]*psfxsim[i-1,1]+[#1]_b[L2.SAFEX]*psfxsim[i-2,1]+[#1]_b[L3.SAFEX]*psfxsim[i-3,1] /*
		  */ +[#1]_b[L.MCHINJI]*pmchsim[i-1,1]+[#1]_b[L2.MCHINJI]*pmchsim[i-2,1]+[#1]_b[L3.MCHINJI]*pmchsim[i-3,1] /*
		  */ +[#1]_b[L.exportban]*expbansim[i-1,1]+[#1]_b[L2.exportban]*expbansim[i-2,1]+[#1]_b[L3.exportban]*expbansim[i-3,1] /*
		  */ +UCHOMA[i,1]

            matrix plsksim[i,1] = [#2]_b[_cons]                                                          /*
		  */ +[#2]_b[L.CHOMA]*pchosim[i-1,1]+[#2]_b[L2.CHOMA]*pchosim[i-2,1]+[#2]_b[L3.CHOMA]*pchosim[i-3,1] /*
		  */ +[#2]_b[L.LUSAKA]*plsksim[i-1,1]+[#2]_b[L2.LUSAKA]*plsksim[i-2,1]+[#2]_b[L3.LUSAKA]*plsksim[i-3,1]  /*
		  */ +[#2]_b[L.SAFEX]*psfxsim[i-1,1]+[#2]_b[L2.SAFEX]*psfxsim[i-2,1]+[#2]_b[L3.SAFEX]*psfxsim[i-3,1] /*
		  */ +[#2]_b[L.MCHINJI]*pmchsim[i-1,1]+[#2]_b[L2.MCHINJI]*pmchsim[i-2,1]+[#2]_b[L3.MCHINJI]*pmchsim[i-3,1] /*
		  */ +[#2]_b[L.exportban]*expbansim[i-1,1]+[#2]_b[L2.exportban]*expbansim[i-2,1]+[#2]_b[L3.exportban]*expbansim[i-3,1] /*
		  */ +ULUSAKA[i,1]

/* Make Readable for Accuracy Test 

matrix pchosim[i,1] = pchosim[i,1]                                                               /*
          */ +[#1]_b[L.BPP]*BPP[i-1,1]+[#1]_b[L2.BPP]*BPP[i-2,1]+[#1]_b[L3.BPP]*BPP[i-3,1] /*
		  */ +[#1]_b[L.SPP]*SPP[i-1,1]+[#1]_b[L2.SPP]*SPP[i-2,1]+[#1]_b[L3.SPP]*SPP[i-3,1] 

matrix plsksim[i,1] = plsksim[i,1]                                                               /*
          */ +[#2]_b[L.BPP]*BPP[i-1,1]+[#2]_b[L2.BPP]*BPP[i-2,1]+[#2]_b[L3.BPP]*BPP[i-3,1] /*
		  */ +[#2]_b[L.SPP]*SPP[i-1,1]+[#2]_b[L2.SPP]*SPP[i-2,1]+[#2]_b[L3.SPP]*SPP[i-3,1] 

 End of accuracy test */

      	scalar i = i + 1 
      	}

    svmat pchosim, name(CHOMASIM)
	svmat plsksim, name(LUSAKASIM)

end

varcom

*Graph Choma historical and simulated prices
twoway (line CHOMA date in 4/150, lpattern(solid) clcolor(black) clwidth(medthick)  cmissing(n)) ///
(line CHOMASIM1 date in 4/150, lpattern(shortdash) clcolor(black) clwidth(medthick)  cmissing(n)), ///
ysize (3.5) xsize(6) ///ytitle("Choma wholesale maize price (ZMK/kg)", size(medsmall) ) ///ylabel(#10, angle(horizontal) format(%9.0gc) labsize(medsmall)) /// xtitle("") ///xlabel(#14,  angle(45) labsize(medsmall) tstyle(major) valuelabel) ///
xmtick(##12) ///legend(row(1) size(medsmall) ///lab(1 "Historical") ///lab(2 "Simulated"))


*Graph Lusaka historical and simulated prices
twoway (line LUSAKA date in 4/150, lpattern(solid) clcolor(black) clwidth(medthick)  cmissing(n)) ///
(line LUSAKASIM1 date in 4/150, lpattern(shortdash) clcolor(black) clwidth(medthick)  cmissing(n)), ///
ysize (3.5) xsize(6) ///ytitle("Lusaka wholesale maize price (ZMK/kg)", size(medsmall) ) ///ylabel(#10, angle(horizontal) format(%9.0gc) labsize(medsmall)) /// xtitle("") ///xlabel(#14,  angle(45) labsize(medsmall) tstyle(major) valuelabel) ///
xmtick(##12) ///legend(row(1) size(medsmall) ///lab(1 "Historical") ///lab(2 "Simulated"))

tabstat CHOMA CHOMASIM1 LUSAKA LUSAKASIM1 in 4/150, stat(n mean sd cv) col(var)
tabstat CHOMA CHOMASIM1 LUSAKA LUSAKASIM1 in 4/150 if date<522, stat(n mean sd cv) col(var)
tabstat CHOMA CHOMASIM1 LUSAKA LUSAKASIM1 if date>=522, stat(n mean sd cv) col(var)


****************************************************************
***************** INCLUDING MAIZE NET IMPORTS  *****************
****************************************************************

clear
clear matrixset matsize 700drop _allset memory 200mset more off

*set graphics schemeset scheme s1mono
graph set window fontface "Times New Roman"
graph set eps fontface "Times New Roman"

*Set working directory
cd "/Users/nicolemason/Documents/Dissertation/Essay1_FRA_maize_prices/"

**** Get full dataset
use "tmp/Mason_&_Myers_dataset_full.dta", clear

*drop cases before Jan. 1997 (monthly maize trade data not available)
keep if date>443

generate t=_n

*tsset data
tsset date, format(%tmm._CY)

*Estimate RF VAR
set more off
var CHOMA LUSAKA SAFEX MCHINJI BPP SPP mznetimports, lags(1/3)

*Predict residuals 
predict UCHOMA, equation(#1) resid
predict ULUSAKA, equation(#2) resid

/* Compute Simulated Prices in the Absence of FRA - Adapted from Jayne et al. (2008) */

capture program drop varcom
program define varcom

      mkmat CHOMA LUSAKA UCHOMA ULUSAKA BPP SPP SAFEX MCHINJI mznetimports
      matrix pchosim = CHOMA
      matrix plsksim = LUSAKA
      matrix psfxsim = SAFEX
      matrix pmchsim = MCHINJI
      matrix netimpsim = mznetimports
      

      scalar i = 4
      while i <= 144 {
	
            matrix pchosim[i,1] = [#1]_b[_cons]                                                          /*
		  */ +[#1]_b[L.CHOMA]*pchosim[i-1,1]+[#1]_b[L2.CHOMA]*pchosim[i-2,1]+[#1]_b[L3.CHOMA]*pchosim[i-3,1] /*
		  */ +[#1]_b[L.LUSAKA]*plsksim[i-1,1]+[#1]_b[L2.LUSAKA]*plsksim[i-2,1]+[#1]_b[L3.LUSAKA]*plsksim[i-3,1] /*
		  */ +[#1]_b[L.SAFEX]*psfxsim[i-1,1]+[#1]_b[L2.SAFEX]*psfxsim[i-2,1]+[#1]_b[L3.SAFEX]*psfxsim[i-3,1] /*
		  */ +[#1]_b[L.MCHINJI]*pmchsim[i-1,1]+[#1]_b[L2.MCHINJI]*pmchsim[i-2,1]+[#1]_b[L3.MCHINJI]*pmchsim[i-3,1] /*
		  */ +[#1]_b[L.mznetimports]*netimpsim[i-1,1]+[#1]_b[L2.mznetimports]*netimpsim[i-2,1]+[#1]_b[L3.mznetimports]*netimpsim[i-3,1] /*
		  */ +UCHOMA[i,1]

            matrix plsksim[i,1] = [#2]_b[_cons]                                                          /*
		  */ +[#2]_b[L.CHOMA]*pchosim[i-1,1]+[#2]_b[L2.CHOMA]*pchosim[i-2,1]+[#2]_b[L3.CHOMA]*pchosim[i-3,1] /*
		  */ +[#2]_b[L.LUSAKA]*plsksim[i-1,1]+[#2]_b[L2.LUSAKA]*plsksim[i-2,1]+[#2]_b[L3.LUSAKA]*plsksim[i-3,1]  /*
		  */ +[#2]_b[L.SAFEX]*psfxsim[i-1,1]+[#2]_b[L2.SAFEX]*psfxsim[i-2,1]+[#2]_b[L3.SAFEX]*psfxsim[i-3,1] /*
		  */ +[#2]_b[L.MCHINJI]*pmchsim[i-1,1]+[#2]_b[L2.MCHINJI]*pmchsim[i-2,1]+[#2]_b[L3.MCHINJI]*pmchsim[i-3,1] /*
		  */ +[#2]_b[L.mznetimports]*netimpsim[i-1,1]+[#2]_b[L2.mznetimports]*netimpsim[i-2,1]+[#2]_b[L3.mznetimports]*netimpsim[i-3,1] /*
		  */ +ULUSAKA[i,1]

/* Make Readable for Accuracy Test 

matrix pchosim[i,1] = pchosim[i,1]                                                               /*
          */ +[#1]_b[L.BPP]*BPP[i-1,1]+[#1]_b[L2.BPP]*BPP[i-2,1]+[#1]_b[L3.BPP]*BPP[i-3,1] /*
		  */ +[#1]_b[L.SPP]*SPP[i-1,1]+[#1]_b[L2.SPP]*SPP[i-2,1]+[#1]_b[L3.SPP]*SPP[i-3,1] 

matrix plsksim[i,1] = plsksim[i,1]                                                               /*
          */ +[#2]_b[L.BPP]*BPP[i-1,1]+[#2]_b[L2.BPP]*BPP[i-2,1]+[#2]_b[L3.BPP]*BPP[i-3,1] /*
		  */ +[#2]_b[L.SPP]*SPP[i-1,1]+[#2]_b[L2.SPP]*SPP[i-2,1]+[#2]_b[L3.SPP]*SPP[i-3,1] 

 End of accuracy test */

      	scalar i = i + 1 
      	}

    svmat pchosim, name(CHOMASIM)
	svmat plsksim, name(LUSAKASIM)

end

varcom

*Graph Choma historical and simulated prices
twoway (line CHOMA date in 4/144, lpattern(solid) clcolor(black) clwidth(medthick)  cmissing(n)) ///
(line CHOMASIM1 date in 4/144, lpattern(shortdash) clcolor(black) clwidth(medthick)  cmissing(n)), ///
ysize (3.5) xsize(6) ///ytitle("Choma wholesale maize price (ZMK/kg)", size(medsmall) ) ///ylabel(#10, angle(horizontal) format(%9.0gc) labsize(medsmall)) /// xtitle("") ///xlabel(#14,  angle(45) labsize(medsmall) tstyle(major) valuelabel) ///
xmtick(##12) ///legend(row(1) size(medsmall) ///lab(1 "Historical") ///lab(2 "Simulated"))


*Graph Lusaka historical and simulated prices
twoway (line LUSAKA date in 4/144, lpattern(solid) clcolor(black) clwidth(medthick)  cmissing(n)) ///
(line LUSAKASIM1 date in 4/144, lpattern(shortdash) clcolor(black) clwidth(medthick)  cmissing(n)), ///
ysize (3.5) xsize(6) ///ytitle("Lusaka wholesale maize price (ZMK/kg)", size(medsmall) ) ///ylabel(#10, angle(horizontal) format(%9.0gc) labsize(medsmall)) /// xtitle("") ///xlabel(#14,  angle(45) labsize(medsmall) tstyle(major) valuelabel) ///
xmtick(##12) ///legend(row(1) size(medsmall) ///lab(1 "Historical") ///lab(2 "Simulated"))

tabstat CHOMA CHOMASIM1 LUSAKA LUSAKASIM1 in 4/144, stat(n mean sd cv) col(var)
tabstat CHOMA CHOMASIM1 LUSAKA LUSAKASIM1 in 4/144 if date<522, stat(n mean sd cv) col(var)
tabstat CHOMA CHOMASIM1 LUSAKA LUSAKASIM1 if date>=522, stat(n mean sd cv) col(var)





***************************************************************************************************************************************************************************
***************************************************************************************************************************************************************************
********************************************* REDUCED FORM THRESHOLD VAR ESTIMATION & "NO FRA" MARKET PRICES SIMULATION ***************************************************
***************************************************************************************************************************************************************************
***************************************************************************************************************************************************************************


****Note that threshold estimation and testing was done in GAUSS. See Mason_&_Myers_GAUSS_code.prg. 
*** Threshold VARs estimated below are using the optimal threshold level estimated in GAUSS




**************************************************
******* THRESHOLD VAR: MKTSHARE 9.058635712 ******
**************************************************


clear
clear matrixset matsize 700drop _allset memory 200mset more off

*set graphics schemeset scheme s1mono
graph set window fontface "Times New Roman"
graph set eps fontface "Times New Roman"

*Set working directory
cd "/Users/nicolemason/Documents/Dissertation/Essay1_FRA_maize_prices/"

**** Get full dataset
use "tmp/Mason_&_Myers_dataset_full.dta", clear

generate t=_n

*tsset data
tsset date, format(%tmm._CY)

*Estimate RF VAR FOR LOW REGIME
var CHOMA LUSAKA SAFEX BPP SPP MCHINJI if mktshare<= 9.058635712, lags(1/3)

*Save residuals and coefficients
predict lUCHOMA, equation(#1) resid
predict lULUSAKA, equation(#2) resid

scalar lb1cons=[#1]_b[_cons]   
scalar lb1LCHOMA=[#1]_b[L.CHOMA]
scalar lb1L2CHOMA=[#1]_b[L2.CHOMA]
scalar lb1L3CHOMA=[#1]_b[L3.CHOMA]
scalar lb1LLUSAKA=[#1]_b[L.LUSAKA]
scalar lb1L2LUSAKA=[#1]_b[L2.LUSAKA]
scalar lb1L3LUSAKA=[#1]_b[L3.LUSAKA]
scalar lb1LSAFEX=[#1]_b[L.SAFEX]
scalar lb1L2SAFEX=[#1]_b[L2.SAFEX]
scalar lb1L3SAFEX=[#1]_b[L3.SAFEX]
scalar lb1LBPP=[#1]_b[L.BPP]
scalar lb1L2BPP=[#1]_b[L2.BPP]
scalar lb1L3BPP=[#1]_b[L3.BPP]
scalar lb1LSPP=[#1]_b[L.SPP]
scalar lb1L2SPP=[#1]_b[L2.SPP]
scalar lb1L3SPP=[#1]_b[L3.SPP]
scalar lb1LMCHINJI=[#1]_b[L.MCHINJI]
scalar lb1L2MCHINJI=[#1]_b[L2.MCHINJI]
scalar lb1L3MCHINJI=[#1]_b[L3.MCHINJI]

scalar lb2cons=[#2]_b[_cons]   
scalar lb2LCHOMA=[#2]_b[L.CHOMA]
scalar lb2L2CHOMA=[#2]_b[L2.CHOMA]
scalar lb2L3CHOMA=[#2]_b[L3.CHOMA]
scalar lb2LLUSAKA=[#2]_b[L.LUSAKA]
scalar lb2L2LUSAKA=[#2]_b[L2.LUSAKA]
scalar lb2L3LUSAKA=[#2]_b[L3.LUSAKA]
scalar lb2LSAFEX=[#2]_b[L.SAFEX]
scalar lb2L2SAFEX=[#2]_b[L2.SAFEX]
scalar lb2L3SAFEX=[#2]_b[L3.SAFEX]
scalar lb2LBPP=[#2]_b[L.BPP]
scalar lb2L2BPP=[#2]_b[L2.BPP]
scalar lb2L3BPP=[#2]_b[L3.BPP]
scalar lb2LSPP=[#2]_b[L.SPP]
scalar lb2L2SPP=[#2]_b[L2.SPP]
scalar lb2L3SPP=[#2]_b[L3.SPP]
scalar lb2LMCHINJI=[#2]_b[L.MCHINJI]
scalar lb2L2MCHINJI=[#2]_b[L2.MCHINJI]
scalar lb2L3MCHINJI=[#2]_b[L3.MCHINJI]

set more off
*Estimate RF VAR FOR HIGH REGIME
var CHOMA LUSAKA SAFEX BPP SPP MCHINJI if mktshare> 9.058635712, lags(1/3)

*Save residuals and coefficients
predict hUCHOMA, equation(#1) resid
predict hULUSAKA, equation(#2) resid

scalar hb1cons=[#1]_b[_cons]   
scalar hb1LCHOMA=[#1]_b[L.CHOMA]
scalar hb1L2CHOMA=[#1]_b[L2.CHOMA]
scalar hb1L3CHOMA=[#1]_b[L3.CHOMA]
scalar hb1LLUSAKA=[#1]_b[L.LUSAKA]
scalar hb1L2LUSAKA=[#1]_b[L2.LUSAKA]
scalar hb1L3LUSAKA=[#1]_b[L3.LUSAKA]
scalar hb1LSAFEX=[#1]_b[L.SAFEX]
scalar hb1L2SAFEX=[#1]_b[L2.SAFEX]
scalar hb1L3SAFEX=[#1]_b[L3.SAFEX]
scalar hb1LBPP=[#1]_b[L.BPP]
scalar hb1L2BPP=[#1]_b[L2.BPP]
scalar hb1L3BPP=[#1]_b[L3.BPP]
scalar hb1LSPP=[#1]_b[L.SPP]
scalar hb1L2SPP=[#1]_b[L2.SPP]
scalar hb1L3SPP=[#1]_b[L3.SPP]
scalar hb1LMCHINJI=[#1]_b[L.MCHINJI]
scalar hb1L2MCHINJI=[#1]_b[L2.MCHINJI]
scalar hb1L3MCHINJI=[#1]_b[L3.MCHINJI]

scalar hb2cons=[#2]_b[_cons]   
scalar hb2LCHOMA=[#2]_b[L.CHOMA]
scalar hb2L2CHOMA=[#2]_b[L2.CHOMA]
scalar hb2L3CHOMA=[#2]_b[L3.CHOMA]
scalar hb2LLUSAKA=[#2]_b[L.LUSAKA]
scalar hb2L2LUSAKA=[#2]_b[L2.LUSAKA]
scalar hb2L3LUSAKA=[#2]_b[L3.LUSAKA]
scalar hb2LSAFEX=[#2]_b[L.SAFEX]
scalar hb2L2SAFEX=[#2]_b[L2.SAFEX]
scalar hb2L3SAFEX=[#2]_b[L3.SAFEX]
scalar hb2LBPP=[#2]_b[L.BPP]
scalar hb2L2BPP=[#2]_b[L2.BPP]
scalar hb2L3BPP=[#2]_b[L3.BPP]
scalar hb2LSPP=[#2]_b[L.SPP]
scalar hb2L2SPP=[#2]_b[L2.SPP]
scalar hb2L3SPP=[#2]_b[L3.SPP]
scalar hb2LMCHINJI=[#2]_b[L.MCHINJI]
scalar hb2L2MCHINJI=[#2]_b[L2.MCHINJI]
scalar hb2L3MCHINJI=[#2]_b[L3.MCHINJI]

/* Compute Simulated Prices in the Absence of FRA - Adapted from Jayne et al. (2008) */
      
capture program drop varcomsb
program define varcomsb

      mkmat CHOMA LUSAKA SAFEX BPP SPP lUCHOMA lULUSAKA hUCHOMA hULUSAKA mktshare MCHINJI
      matrix pchosim = CHOMA
      matrix plsksim = LUSAKA
      matrix psfxsim = SAFEX
      matrix pmchsim = MCHINJI
      matrix mktshare = mktshare

      scalar i = 4
      while i <= 150 {

/*LOW REGIME*/

	  if mktshare[i,1]<= 9.058635712	{
	
            matrix pchosim[i,1] = lb1cons                                                          /*
		  */ + lb1LCHOMA*pchosim[i-1,1]+lb1L2CHOMA*pchosim[i-2,1]+lb1L3CHOMA*pchosim[i-3,1] /*
		  */ + lb1LLUSAKA*plsksim[i-1,1]+lb1L2LUSAKA*plsksim[i-2,1]+lb1L3LUSAKA*plsksim[i-3,1] /*
		  */ + lb1LSAFEX*psfxsim[i-1,1]+lb1L2SAFEX*psfxsim[i-2,1]+lb1L3SAFEX*psfxsim[i-3,1] /*
		  */ + lb1LMCHINJI*pmchsim[i-1,1]+lb1L2MCHINJI*pmchsim[i-2,1]+lb1L3MCHINJI*pmchsim[i-3,1] /*
		  */ + lUCHOMA[i,1]
		  
            matrix plsksim[i,1] = lb2cons                                                          /*
		  */ + lb2LCHOMA*pchosim[i-1,1]+lb2L2CHOMA*pchosim[i-2,1]+lb2L3CHOMA*pchosim[i-3,1] /*
		  */ + lb2LLUSAKA*plsksim[i-1,1]+lb2L2LUSAKA*plsksim[i-2,1]+lb2L3LUSAKA*plsksim[i-3,1] /*
		  */ + lb2LSAFEX*psfxsim[i-1,1]+lb2L2SAFEX*psfxsim[i-2,1]+lb2L3SAFEX*psfxsim[i-3,1] /*
		  */ + lb2LMCHINJI*pmchsim[i-1,1]+lb2L2MCHINJI*pmchsim[i-2,1]+lb2L3MCHINJI*pmchsim[i-3,1] /*
		  */ + lULUSAKA[i,1]
		  
		  }
		  
/*HIGH REGIME*/

		 if mktshare[i,1]> 9.058635712		{
	
            matrix pchosim[i,1] = hb1cons                                                          /*
		  */ + hb1LCHOMA*pchosim[i-1,1]+hb1L2CHOMA*pchosim[i-2,1]+hb1L3CHOMA*pchosim[i-3,1] /*
		  */ + hb1LLUSAKA*plsksim[i-1,1]+hb1L2LUSAKA*plsksim[i-2,1]+hb1L3LUSAKA*plsksim[i-3,1] /*
		  */ + hb1LSAFEX*psfxsim[i-1,1]+hb1L2SAFEX*psfxsim[i-2,1]+hb1L3SAFEX*psfxsim[i-3,1] /*
		  */ + hb1LMCHINJI*pmchsim[i-1,1]+hb1L2MCHINJI*pmchsim[i-2,1]+hb1L3MCHINJI*pmchsim[i-3,1] /*
		  */ + hUCHOMA[i,1]
		  
            matrix plsksim[i,1] = hb2cons                                                          /*
		  */ + hb2LCHOMA*pchosim[i-1,1]+hb2L2CHOMA*pchosim[i-2,1]+hb2L3CHOMA*pchosim[i-3,1] /*
		  */ + hb2LLUSAKA*plsksim[i-1,1]+hb2L2LUSAKA*plsksim[i-2,1]+hb2L3LUSAKA*plsksim[i-3,1] /*
		  */ + hb2LSAFEX*psfxsim[i-1,1]+hb2L2SAFEX*psfxsim[i-2,1]+hb2L3SAFEX*psfxsim[i-3,1] /*
		  */ + hb2LMCHINJI*pmchsim[i-1,1]+hb2L2MCHINJI*pmchsim[i-2,1]+hb2L3MCHINJI*pmchsim[i-3,1] /*
		  */ + hULUSAKA[i,1]
		  
		  }
		    
		 

/* Make Readable for Accuracy Test 

/*LOW REGIME*/

	  if mktshare[i,1]<= 9.058635712	{


			matrix pchosim[i,1] = pchosim[i,1]                                                               /*
          */ + lb1LBPP*BPP[i-1,1]+lb1L2BPP*BPP[i-2,1]+lb1L3BPP*BPP[i-3,1] /*
		  */ + lb1LSPP*SPP[i-1,1]+lb1L2SPP*SPP[i-2,1]+lb1L3SPP*SPP[i-3,1] 

			matrix plsksim[i,1] = plsksim[i,1]                                                               /*
          */ +lb2LBPP*BPP[i-1,1]+lb2L2BPP*BPP[i-2,1]+lb2L3BPP*BPP[i-3,1] /*
		  */ +lb2LSPP*SPP[i-1,1]+lb2L2SPP*SPP[i-2,1]+lb2L3SPP*SPP[i-3,1] 

		  }

/*HIGH REGIME*/

	  if mktshare[i,1]> 9.058635712	{

			matrix pchosim[i,1] = pchosim[i,1]                                                               /*
          */ + hb1LBPP*BPP[i-1,1]+hb1L2BPP*BPP[i-2,1]+hb1L3BPP*BPP[i-3,1] /*
		  */ + hb1LSPP*SPP[i-1,1]+hb1L2SPP*SPP[i-2,1]+hb1L3SPP*SPP[i-3,1] 

			matrix plsksim[i,1] = plsksim[i,1]                                                               /*
          */ +hb2LBPP*BPP[i-1,1]+hb2L2BPP*BPP[i-2,1]+hb2L3BPP*BPP[i-3,1] /*
		  */ +hb2LSPP*SPP[i-1,1]+hb2L2SPP*SPP[i-2,1]+hb2L3SPP*SPP[i-3,1] 

		  }

 End of accuracy test */

      	scalar i = i + 1 
      	}

    svmat pchosim, name(CHOMASIMsb)
	svmat plsksim, name(LUSAKASIMsb)

end

varcomsb


*Graph Choma historical and simulated prices
twoway (line CHOMA date, lpattern(solid) clcolor(black) cmissing(n)) ///
(line CHOMASIMsb1 date, lpattern(shortdash) clcolor(black) cmissing(n)), ///
ysize (3.5) xsize(6) ///ytitle("Wholesale maize price (ZMK/kg)", size(medsmall) ) ///ylabel(#10, angle(horizontal) format(%9.0gc) labsize(medsmall)) /// 
xline(509) ///xtitle("") ///xlabel(#14,  angle(45) labsize(medsmall) tstyle(major) valuelabel) ///
xmtick(##12) ///legend(row(1) size(medsmall) ///lab(1 "Historical") ///lab(2 "Simulated"))


*Graph Lusaka historical and simulated prices
twoway (line LUSAKA date, lpattern(solid) clcolor(black) cmissing(n)) ///
(line LUSAKASIMsb1 date, lpattern(shortdash) clcolor(black) cmissing(n)), ///
ysize (3.5) xsize(6) ///ytitle("Wholesale maize price (ZMK/kg)", size(medsmall) ) ///ylabel(#10, angle(horizontal) format(%9.0gc) labsize(medsmall)) /// 
xline(509) ///xtitle("") ///xlabel(#14,  angle(45) labsize(medsmall) tstyle(major) valuelabel) ///
xmtick(##12) ///legend(row(1) size(medsmall) ///lab(1 "Historical") ///lab(2 "Simulated"))

**************** SIMULATED *NOT* PRICES IN REASONABLE RANGE ****************




***********************************************************************************************************************************
******* THRESHOLD VAR: MKTSURPLUS 18.26018 -- HIGH REGIME VAR/COV MATRIX NOT POS. DEF. SO LOG LIKELIHOOD CANNOT BE COMPUTED *******
***********************************************************************************************************************************


clear
clear matrixset matsize 700drop _allset memory 200mset more off

*set graphics schemeset scheme s1mono
graph set window fontface "Times New Roman"
graph set eps fontface "Times New Roman"

*Set working directory
cd "/Users/nicolemason/Documents/Dissertation/Essay1_FRA_maize_prices/"

**** Get full dataset
use "tmp/Mason_&_Myers_dataset_full.dta", clear

generate t=_n

*tsset data
tsset date, format(%tmm._CY)

*Estimate RF VAR FOR LOW REGIME
var CHOMA LUSAKA SAFEX BPP SPP MCHINJI if msleftpc<= 18.26018, lags(1/3)

*Save residuals and coefficients
predict lUCHOMA, equation(#1) resid
predict lULUSAKA, equation(#2) resid

scalar lb1cons=[#1]_b[_cons]   
scalar lb1LCHOMA=[#1]_b[L.CHOMA]
scalar lb1L2CHOMA=[#1]_b[L2.CHOMA]
scalar lb1L3CHOMA=[#1]_b[L3.CHOMA]
scalar lb1LLUSAKA=[#1]_b[L.LUSAKA]
scalar lb1L2LUSAKA=[#1]_b[L2.LUSAKA]
scalar lb1L3LUSAKA=[#1]_b[L3.LUSAKA]
scalar lb1LSAFEX=[#1]_b[L.SAFEX]
scalar lb1L2SAFEX=[#1]_b[L2.SAFEX]
scalar lb1L3SAFEX=[#1]_b[L3.SAFEX]
scalar lb1LBPP=[#1]_b[L.BPP]
scalar lb1L2BPP=[#1]_b[L2.BPP]
scalar lb1L3BPP=[#1]_b[L3.BPP]
scalar lb1LSPP=[#1]_b[L.SPP]
scalar lb1L2SPP=[#1]_b[L2.SPP]
scalar lb1L3SPP=[#1]_b[L3.SPP]
scalar lb1LMCHINJI=[#1]_b[L.MCHINJI]
scalar lb1L2MCHINJI=[#1]_b[L2.MCHINJI]
scalar lb1L3MCHINJI=[#1]_b[L3.MCHINJI]

scalar lb2cons=[#2]_b[_cons]   
scalar lb2LCHOMA=[#2]_b[L.CHOMA]
scalar lb2L2CHOMA=[#2]_b[L2.CHOMA]
scalar lb2L3CHOMA=[#2]_b[L3.CHOMA]
scalar lb2LLUSAKA=[#2]_b[L.LUSAKA]
scalar lb2L2LUSAKA=[#2]_b[L2.LUSAKA]
scalar lb2L3LUSAKA=[#2]_b[L3.LUSAKA]
scalar lb2LSAFEX=[#2]_b[L.SAFEX]
scalar lb2L2SAFEX=[#2]_b[L2.SAFEX]
scalar lb2L3SAFEX=[#2]_b[L3.SAFEX]
scalar lb2LBPP=[#2]_b[L.BPP]
scalar lb2L2BPP=[#2]_b[L2.BPP]
scalar lb2L3BPP=[#2]_b[L3.BPP]
scalar lb2LSPP=[#2]_b[L.SPP]
scalar lb2L2SPP=[#2]_b[L2.SPP]
scalar lb2L3SPP=[#2]_b[L3.SPP]
scalar lb2LMCHINJI=[#2]_b[L.MCHINJI]
scalar lb2L2MCHINJI=[#2]_b[L2.MCHINJI]
scalar lb2L3MCHINJI=[#2]_b[L3.MCHINJI]

set more off
*Estimate RF VAR FOR HIGH REGIME
var CHOMA LUSAKA SAFEX BPP SPP MCHINJI if msleftpc> 18.26018, lags(1/3)

*Save residuals and coefficients
predict hUCHOMA, equation(#1) resid
predict hULUSAKA, equation(#2) resid

scalar hb1cons=[#1]_b[_cons]   
scalar hb1LCHOMA=[#1]_b[L.CHOMA]
scalar hb1L2CHOMA=[#1]_b[L2.CHOMA]
scalar hb1L3CHOMA=[#1]_b[L3.CHOMA]
scalar hb1LLUSAKA=[#1]_b[L.LUSAKA]
scalar hb1L2LUSAKA=[#1]_b[L2.LUSAKA]
scalar hb1L3LUSAKA=[#1]_b[L3.LUSAKA]
scalar hb1LSAFEX=[#1]_b[L.SAFEX]
scalar hb1L2SAFEX=[#1]_b[L2.SAFEX]
scalar hb1L3SAFEX=[#1]_b[L3.SAFEX]
scalar hb1LBPP=[#1]_b[L.BPP]
scalar hb1L2BPP=[#1]_b[L2.BPP]
scalar hb1L3BPP=[#1]_b[L3.BPP]
scalar hb1LSPP=[#1]_b[L.SPP]
scalar hb1L2SPP=[#1]_b[L2.SPP]
scalar hb1L3SPP=[#1]_b[L3.SPP]
scalar hb1LMCHINJI=[#1]_b[L.MCHINJI]
scalar hb1L2MCHINJI=[#1]_b[L2.MCHINJI]
scalar hb1L3MCHINJI=[#1]_b[L3.MCHINJI]

scalar hb2cons=[#2]_b[_cons]   
scalar hb2LCHOMA=[#2]_b[L.CHOMA]
scalar hb2L2CHOMA=[#2]_b[L2.CHOMA]
scalar hb2L3CHOMA=[#2]_b[L3.CHOMA]
scalar hb2LLUSAKA=[#2]_b[L.LUSAKA]
scalar hb2L2LUSAKA=[#2]_b[L2.LUSAKA]
scalar hb2L3LUSAKA=[#2]_b[L3.LUSAKA]
scalar hb2LSAFEX=[#2]_b[L.SAFEX]
scalar hb2L2SAFEX=[#2]_b[L2.SAFEX]
scalar hb2L3SAFEX=[#2]_b[L3.SAFEX]
scalar hb2LBPP=[#2]_b[L.BPP]
scalar hb2L2BPP=[#2]_b[L2.BPP]
scalar hb2L3BPP=[#2]_b[L3.BPP]
scalar hb2LSPP=[#2]_b[L.SPP]
scalar hb2L2SPP=[#2]_b[L2.SPP]
scalar hb2L3SPP=[#2]_b[L3.SPP]
scalar hb2LMCHINJI=[#2]_b[L.MCHINJI]
scalar hb2L2MCHINJI=[#2]_b[L2.MCHINJI]
scalar hb2L3MCHINJI=[#2]_b[L3.MCHINJI]

/* Compute Simulated Prices in the Absence of FRA - Adapted from Jayne et al. (2008) */
      
capture program drop varcomsb
program define varcomsb

      mkmat CHOMA LUSAKA SAFEX BPP SPP lUCHOMA lULUSAKA hUCHOMA hULUSAKA msleftpc MCHINJI
      matrix pchosim = CHOMA
      matrix plsksim = LUSAKA
      matrix psfxsim = SAFEX
      matrix pmchsim = MCHINJI
      matrix msleftpc = msleftpc

      scalar i = 4
      while i <= 150 {

/*LOW REGIME*/

	  if msleftpc[i,1]<= 18.26018	{
	
            matrix pchosim[i,1] = lb1cons                                                          /*
		  */ + lb1LCHOMA*pchosim[i-1,1]+lb1L2CHOMA*pchosim[i-2,1]+lb1L3CHOMA*pchosim[i-3,1] /*
		  */ + lb1LLUSAKA*plsksim[i-1,1]+lb1L2LUSAKA*plsksim[i-2,1]+lb1L3LUSAKA*plsksim[i-3,1] /*
		  */ + lb1LSAFEX*psfxsim[i-1,1]+lb1L2SAFEX*psfxsim[i-2,1]+lb1L3SAFEX*psfxsim[i-3,1] /*
		  */ + lb1LMCHINJI*pmchsim[i-1,1]+lb1L2MCHINJI*pmchsim[i-2,1]+lb1L3MCHINJI*pmchsim[i-3,1] /*
		  */ + lUCHOMA[i,1]
		  
            matrix plsksim[i,1] = lb2cons                                                          /*
		  */ + lb2LCHOMA*pchosim[i-1,1]+lb2L2CHOMA*pchosim[i-2,1]+lb2L3CHOMA*pchosim[i-3,1] /*
		  */ + lb2LLUSAKA*plsksim[i-1,1]+lb2L2LUSAKA*plsksim[i-2,1]+lb2L3LUSAKA*plsksim[i-3,1] /*
		  */ + lb2LSAFEX*psfxsim[i-1,1]+lb2L2SAFEX*psfxsim[i-2,1]+lb2L3SAFEX*psfxsim[i-3,1] /*
		  */ + lb2LMCHINJI*pmchsim[i-1,1]+lb2L2MCHINJI*pmchsim[i-2,1]+lb2L3MCHINJI*pmchsim[i-3,1] /*
		  */ + lULUSAKA[i,1]
		  
		  }
		  
/*HIGH REGIME*/

		 if msleftpc[i,1]> 18.26018		{
	
            matrix pchosim[i,1] = hb1cons                                                          /*
		  */ + hb1LCHOMA*pchosim[i-1,1]+hb1L2CHOMA*pchosim[i-2,1]+hb1L3CHOMA*pchosim[i-3,1] /*
		  */ + hb1LLUSAKA*plsksim[i-1,1]+hb1L2LUSAKA*plsksim[i-2,1]+hb1L3LUSAKA*plsksim[i-3,1] /*
		  */ + hb1LSAFEX*psfxsim[i-1,1]+hb1L2SAFEX*psfxsim[i-2,1]+hb1L3SAFEX*psfxsim[i-3,1] /*
		  */ + hb1LMCHINJI*pmchsim[i-1,1]+hb1L2MCHINJI*pmchsim[i-2,1]+hb1L3MCHINJI*pmchsim[i-3,1] /*
		  */ + hUCHOMA[i,1]
		  
            matrix plsksim[i,1] = hb2cons                                                          /*
		  */ + hb2LCHOMA*pchosim[i-1,1]+hb2L2CHOMA*pchosim[i-2,1]+hb2L3CHOMA*pchosim[i-3,1] /*
		  */ + hb2LLUSAKA*plsksim[i-1,1]+hb2L2LUSAKA*plsksim[i-2,1]+hb2L3LUSAKA*plsksim[i-3,1] /*
		  */ + hb2LSAFEX*psfxsim[i-1,1]+hb2L2SAFEX*psfxsim[i-2,1]+hb2L3SAFEX*psfxsim[i-3,1] /*
		  */ + hb2LMCHINJI*pmchsim[i-1,1]+hb2L2MCHINJI*pmchsim[i-2,1]+hb2L3MCHINJI*pmchsim[i-3,1] /*
		  */ + hULUSAKA[i,1]
		  
		  }
		    
		 

/* Make Readable for Accuracy Test 

/*LOW REGIME*/

	  if msleftpc[i,1]<= 18.26018	{


			matrix pchosim[i,1] = pchosim[i,1]                                                               /*
          */ + lb1LBPP*BPP[i-1,1]+lb1L2BPP*BPP[i-2,1]+lb1L3BPP*BPP[i-3,1] /*
		  */ + lb1LSPP*SPP[i-1,1]+lb1L2SPP*SPP[i-2,1]+lb1L3SPP*SPP[i-3,1] 

			matrix plsksim[i,1] = plsksim[i,1]                                                               /*
          */ +lb2LBPP*BPP[i-1,1]+lb2L2BPP*BPP[i-2,1]+lb2L3BPP*BPP[i-3,1] /*
		  */ +lb2LSPP*SPP[i-1,1]+lb2L2SPP*SPP[i-2,1]+lb2L3SPP*SPP[i-3,1] 

		  }

/*HIGH REGIME*/

	  if msleftpc[i,1]> 18.26018	{

			matrix pchosim[i,1] = pchosim[i,1]                                                               /*
          */ + hb1LBPP*BPP[i-1,1]+hb1L2BPP*BPP[i-2,1]+hb1L3BPP*BPP[i-3,1] /*
		  */ + hb1LSPP*SPP[i-1,1]+hb1L2SPP*SPP[i-2,1]+hb1L3SPP*SPP[i-3,1] 

			matrix plsksim[i,1] = plsksim[i,1]                                                               /*
          */ +hb2LBPP*BPP[i-1,1]+hb2L2BPP*BPP[i-2,1]+hb2L3BPP*BPP[i-3,1] /*
		  */ +hb2LSPP*SPP[i-1,1]+hb2L2SPP*SPP[i-2,1]+hb2L3SPP*SPP[i-3,1] 

		  }

 End of accuracy test */

      	scalar i = i + 1 
      	}

    svmat pchosim, name(CHOMASIMsb)
	svmat plsksim, name(LUSAKASIMsb)

end

varcomsb


*Graph Choma historical and simulated prices
twoway (line CHOMA date, lpattern(solid) clcolor(black) cmissing(n)) ///
(line CHOMASIMsb1 date, lpattern(shortdash) clcolor(black) cmissing(n)), ///
ysize (3.5) xsize(6) ///ytitle("Wholesale maize price (ZMK/kg)", size(medsmall) ) ///ylabel(#10, angle(horizontal) format(%9.0gc) labsize(medsmall)) /// 
xline(509) ///xtitle("") ///xlabel(#14,  angle(45) labsize(medsmall) tstyle(major) valuelabel) ///
xmtick(##12) ///legend(row(1) size(medsmall) ///lab(1 "Historical") ///lab(2 "Simulated"))


*Graph Lusaka historical and simulated prices
twoway (line LUSAKA date, lpattern(solid) clcolor(black) cmissing(n)) ///
(line LUSAKASIMsb1 date, lpattern(shortdash) clcolor(black) cmissing(n)), ///
ysize (3.5) xsize(6) ///ytitle("Wholesale maize price (ZMK/kg)", size(medsmall) ) ///ylabel(#10, angle(horizontal) format(%9.0gc) labsize(medsmall)) /// 
xline(509) ///xtitle("") ///xlabel(#14,  angle(45) labsize(medsmall) tstyle(major) valuelabel) ///
xmtick(##12) ///legend(row(1) size(medsmall) ///lab(1 "Historical") ///lab(2 "Simulated"))

**************** SIMULATED *NOT* PRICES IN REASONABLE RANGE ****************



****************************************************
******* THRESHOLD VAR: APRODPC AT 105.262787 *******
****************************************************


clear
clear matrixset matsize 700drop _allset memory 200mset more off

*set graphics schemeset scheme s1mono
graph set window fontface "Times New Roman"
graph set eps fontface "Times New Roman"

*Set working directory
cd "/Users/nicolemason/Documents/Dissertation/Essay1_FRA_maize_prices/"

**** Get full dataset
use "tmp/Mason_&_Myers_dataset_full.dta", clear

generate t=_n

*tsset data
tsset date, format(%tmm._CY)

*Estimate RF VAR FOR LOW REGIME
var CHOMA LUSAKA SAFEX BPP SPP MCHINJI if aprodpc<= 105.262787, lags(1/3)

*Save residuals and coefficients
predict lUCHOMA, equation(#1) resid
predict lULUSAKA, equation(#2) resid

scalar lb1cons=[#1]_b[_cons]   
scalar lb1LCHOMA=[#1]_b[L.CHOMA]
scalar lb1L2CHOMA=[#1]_b[L2.CHOMA]
scalar lb1L3CHOMA=[#1]_b[L3.CHOMA]
scalar lb1LLUSAKA=[#1]_b[L.LUSAKA]
scalar lb1L2LUSAKA=[#1]_b[L2.LUSAKA]
scalar lb1L3LUSAKA=[#1]_b[L3.LUSAKA]
scalar lb1LSAFEX=[#1]_b[L.SAFEX]
scalar lb1L2SAFEX=[#1]_b[L2.SAFEX]
scalar lb1L3SAFEX=[#1]_b[L3.SAFEX]
scalar lb1LBPP=[#1]_b[L.BPP]
scalar lb1L2BPP=[#1]_b[L2.BPP]
scalar lb1L3BPP=[#1]_b[L3.BPP]
scalar lb1LSPP=[#1]_b[L.SPP]
scalar lb1L2SPP=[#1]_b[L2.SPP]
scalar lb1L3SPP=[#1]_b[L3.SPP]
scalar lb1LMCHINJI=[#1]_b[L.MCHINJI]
scalar lb1L2MCHINJI=[#1]_b[L2.MCHINJI]
scalar lb1L3MCHINJI=[#1]_b[L3.MCHINJI]

scalar lb2cons=[#2]_b[_cons]   
scalar lb2LCHOMA=[#2]_b[L.CHOMA]
scalar lb2L2CHOMA=[#2]_b[L2.CHOMA]
scalar lb2L3CHOMA=[#2]_b[L3.CHOMA]
scalar lb2LLUSAKA=[#2]_b[L.LUSAKA]
scalar lb2L2LUSAKA=[#2]_b[L2.LUSAKA]
scalar lb2L3LUSAKA=[#2]_b[L3.LUSAKA]
scalar lb2LSAFEX=[#2]_b[L.SAFEX]
scalar lb2L2SAFEX=[#2]_b[L2.SAFEX]
scalar lb2L3SAFEX=[#2]_b[L3.SAFEX]
scalar lb2LBPP=[#2]_b[L.BPP]
scalar lb2L2BPP=[#2]_b[L2.BPP]
scalar lb2L3BPP=[#2]_b[L3.BPP]
scalar lb2LSPP=[#2]_b[L.SPP]
scalar lb2L2SPP=[#2]_b[L2.SPP]
scalar lb2L3SPP=[#2]_b[L3.SPP]
scalar lb2LMCHINJI=[#2]_b[L.MCHINJI]
scalar lb2L2MCHINJI=[#2]_b[L2.MCHINJI]
scalar lb2L3MCHINJI=[#2]_b[L3.MCHINJI]

set more off
*Estimate RF VAR FOR HIGH REGIME
var CHOMA LUSAKA SAFEX BPP SPP MCHINJI if aprodpc> 105.262787, lags(1/3)

*Save residuals and coefficients
predict hUCHOMA, equation(#1) resid
predict hULUSAKA, equation(#2) resid

scalar hb1cons=[#1]_b[_cons]   
scalar hb1LCHOMA=[#1]_b[L.CHOMA]
scalar hb1L2CHOMA=[#1]_b[L2.CHOMA]
scalar hb1L3CHOMA=[#1]_b[L3.CHOMA]
scalar hb1LLUSAKA=[#1]_b[L.LUSAKA]
scalar hb1L2LUSAKA=[#1]_b[L2.LUSAKA]
scalar hb1L3LUSAKA=[#1]_b[L3.LUSAKA]
scalar hb1LSAFEX=[#1]_b[L.SAFEX]
scalar hb1L2SAFEX=[#1]_b[L2.SAFEX]
scalar hb1L3SAFEX=[#1]_b[L3.SAFEX]
scalar hb1LBPP=[#1]_b[L.BPP]
scalar hb1L2BPP=[#1]_b[L2.BPP]
scalar hb1L3BPP=[#1]_b[L3.BPP]
scalar hb1LSPP=[#1]_b[L.SPP]
scalar hb1L2SPP=[#1]_b[L2.SPP]
scalar hb1L3SPP=[#1]_b[L3.SPP]
scalar hb1LMCHINJI=[#1]_b[L.MCHINJI]
scalar hb1L2MCHINJI=[#1]_b[L2.MCHINJI]
scalar hb1L3MCHINJI=[#1]_b[L3.MCHINJI]

scalar hb2cons=[#2]_b[_cons]   
scalar hb2LCHOMA=[#2]_b[L.CHOMA]
scalar hb2L2CHOMA=[#2]_b[L2.CHOMA]
scalar hb2L3CHOMA=[#2]_b[L3.CHOMA]
scalar hb2LLUSAKA=[#2]_b[L.LUSAKA]
scalar hb2L2LUSAKA=[#2]_b[L2.LUSAKA]
scalar hb2L3LUSAKA=[#2]_b[L3.LUSAKA]
scalar hb2LSAFEX=[#2]_b[L.SAFEX]
scalar hb2L2SAFEX=[#2]_b[L2.SAFEX]
scalar hb2L3SAFEX=[#2]_b[L3.SAFEX]
scalar hb2LBPP=[#2]_b[L.BPP]
scalar hb2L2BPP=[#2]_b[L2.BPP]
scalar hb2L3BPP=[#2]_b[L3.BPP]
scalar hb2LSPP=[#2]_b[L.SPP]
scalar hb2L2SPP=[#2]_b[L2.SPP]
scalar hb2L3SPP=[#2]_b[L3.SPP]
scalar hb2LMCHINJI=[#2]_b[L.MCHINJI]
scalar hb2L2MCHINJI=[#2]_b[L2.MCHINJI]
scalar hb2L3MCHINJI=[#2]_b[L3.MCHINJI]

/* Compute Simulated Prices in the Absence of FRA - Adapted from Jayne et al. (2008) */
      
capture program drop varcomsb
program define varcomsb

      mkmat CHOMA LUSAKA SAFEX BPP SPP lUCHOMA lULUSAKA hUCHOMA hULUSAKA aprodpc MCHINJI
      matrix pchosim = CHOMA
      matrix plsksim = LUSAKA
      matrix psfxsim = SAFEX
      matrix pmchsim = MCHINJI
      matrix aprodpc = aprodpc

      scalar i = 4
      while i <= 150 {

/*LOW REGIME*/

	  if aprodpc[i,1]<= 105.262787	{
	
            matrix pchosim[i,1] = lb1cons                                                          /*
		  */ + lb1LCHOMA*pchosim[i-1,1]+lb1L2CHOMA*pchosim[i-2,1]+lb1L3CHOMA*pchosim[i-3,1] /*
		  */ + lb1LLUSAKA*plsksim[i-1,1]+lb1L2LUSAKA*plsksim[i-2,1]+lb1L3LUSAKA*plsksim[i-3,1] /*
		  */ + lb1LSAFEX*psfxsim[i-1,1]+lb1L2SAFEX*psfxsim[i-2,1]+lb1L3SAFEX*psfxsim[i-3,1] /*
		  */ + lb1LMCHINJI*pmchsim[i-1,1]+lb1L2MCHINJI*pmchsim[i-2,1]+lb1L3MCHINJI*pmchsim[i-3,1] /*
		  */ + lUCHOMA[i,1]
		  
            matrix plsksim[i,1] = lb2cons                                                          /*
		  */ + lb2LCHOMA*pchosim[i-1,1]+lb2L2CHOMA*pchosim[i-2,1]+lb2L3CHOMA*pchosim[i-3,1] /*
		  */ + lb2LLUSAKA*plsksim[i-1,1]+lb2L2LUSAKA*plsksim[i-2,1]+lb2L3LUSAKA*plsksim[i-3,1] /*
		  */ + lb2LSAFEX*psfxsim[i-1,1]+lb2L2SAFEX*psfxsim[i-2,1]+lb2L3SAFEX*psfxsim[i-3,1] /*
		  */ + lb2LMCHINJI*pmchsim[i-1,1]+lb2L2MCHINJI*pmchsim[i-2,1]+lb2L3MCHINJI*pmchsim[i-3,1] /*
		  */ + lULUSAKA[i,1]
		  
		  }
		  
/*HIGH REGIME*/

		 if aprodpc[i,1]> 105.262787		{
	
            matrix pchosim[i,1] = hb1cons                                                          /*
		  */ + hb1LCHOMA*pchosim[i-1,1]+hb1L2CHOMA*pchosim[i-2,1]+hb1L3CHOMA*pchosim[i-3,1] /*
		  */ + hb1LLUSAKA*plsksim[i-1,1]+hb1L2LUSAKA*plsksim[i-2,1]+hb1L3LUSAKA*plsksim[i-3,1] /*
		  */ + hb1LSAFEX*psfxsim[i-1,1]+hb1L2SAFEX*psfxsim[i-2,1]+hb1L3SAFEX*psfxsim[i-3,1] /*
		  */ + hb1LMCHINJI*pmchsim[i-1,1]+hb1L2MCHINJI*pmchsim[i-2,1]+hb1L3MCHINJI*pmchsim[i-3,1] /*
		  */ + hUCHOMA[i,1]
		  
            matrix plsksim[i,1] = hb2cons                                                          /*
		  */ + hb2LCHOMA*pchosim[i-1,1]+hb2L2CHOMA*pchosim[i-2,1]+hb2L3CHOMA*pchosim[i-3,1] /*
		  */ + hb2LLUSAKA*plsksim[i-1,1]+hb2L2LUSAKA*plsksim[i-2,1]+hb2L3LUSAKA*plsksim[i-3,1] /*
		  */ + hb2LSAFEX*psfxsim[i-1,1]+hb2L2SAFEX*psfxsim[i-2,1]+hb2L3SAFEX*psfxsim[i-3,1] /*
		  */ + hb2LMCHINJI*pmchsim[i-1,1]+hb2L2MCHINJI*pmchsim[i-2,1]+hb2L3MCHINJI*pmchsim[i-3,1] /*
		  */ + hULUSAKA[i,1]
		  
		  }
		    
		 

/* Make Readable for Accuracy Test 

/*LOW REGIME*/

	  if aprodpc[i,1]<= 105.262787	{


			matrix pchosim[i,1] = pchosim[i,1]                                                               /*
          */ + lb1LBPP*BPP[i-1,1]+lb1L2BPP*BPP[i-2,1]+lb1L3BPP*BPP[i-3,1] /*
		  */ + lb1LSPP*SPP[i-1,1]+lb1L2SPP*SPP[i-2,1]+lb1L3SPP*SPP[i-3,1] 

			matrix plsksim[i,1] = plsksim[i,1]                                                               /*
          */ +lb2LBPP*BPP[i-1,1]+lb2L2BPP*BPP[i-2,1]+lb2L3BPP*BPP[i-3,1] /*
		  */ +lb2LSPP*SPP[i-1,1]+lb2L2SPP*SPP[i-2,1]+lb2L3SPP*SPP[i-3,1] 

		  }

/*HIGH REGIME*/

	  if aprodpc[i,1]> 105.262787	{

			matrix pchosim[i,1] = pchosim[i,1]                                                               /*
          */ + hb1LBPP*BPP[i-1,1]+hb1L2BPP*BPP[i-2,1]+hb1L3BPP*BPP[i-3,1] /*
		  */ + hb1LSPP*SPP[i-1,1]+hb1L2SPP*SPP[i-2,1]+hb1L3SPP*SPP[i-3,1] 

			matrix plsksim[i,1] = plsksim[i,1]                                                               /*
          */ +hb2LBPP*BPP[i-1,1]+hb2L2BPP*BPP[i-2,1]+hb2L3BPP*BPP[i-3,1] /*
		  */ +hb2LSPP*SPP[i-1,1]+hb2L2SPP*SPP[i-2,1]+hb2L3SPP*SPP[i-3,1] 

		  }

 End of accuracy test */

      	scalar i = i + 1 
      	}

    svmat pchosim, name(CHOMASIMsb)
	svmat plsksim, name(LUSAKASIMsb)

end

varcomsb


*Graph Choma historical and simulated prices
twoway (line CHOMA date, lpattern(solid) clcolor(black) cmissing(n)) ///
(line CHOMASIMsb1 date, lpattern(shortdash) clcolor(black) cmissing(n)), ///
ysize (3.5) xsize(6) ///ytitle("Wholesale maize price (ZMK/kg)", size(medsmall) ) ///ylabel(#10, angle(horizontal) format(%9.0gc) labsize(medsmall)) /// xtitle("") ///xlabel(#14,  angle(45) labsize(medsmall) tstyle(major) valuelabel) ///
xmtick(##12) ///legend(row(1) size(medsmall) ///lab(1 "Historical") ///lab(2 "Simulated"))


*Graph Lusaka historical and simulated prices
twoway (line LUSAKA date, lpattern(solid) clcolor(black) cmissing(n)) ///
(line LUSAKASIMsb1 date, lpattern(shortdash) clcolor(black) cmissing(n)), ///
ysize (3.5) xsize(6) ///ytitle("Wholesale maize price (ZMK/kg)", size(medsmall) ) ///ylabel(#10, angle(horizontal) format(%9.0gc) labsize(medsmall)) /// 
xline(509) ///xtitle("") ///xlabel(#14,  angle(45) labsize(medsmall) tstyle(major) valuelabel) ///
xmtick(##12) ///legend(row(1) size(medsmall) ///lab(1 "Historical") ///lab(2 "Simulated"))

**************** SIMULATED *NOT* PRICES IN REASONABLE RANGE ****************





*******************************************************************************
************** STRUCTURAL BREAK VAR: BREAK AT 525 (October 2003) **************
*******************************************************************************


clear
clear matrixset matsize 700drop _allset memory 200mset more off

*set graphics schemeset scheme s1mono
graph set window fontface "Times New Roman"
graph set eps fontface "Times New Roman"

*Set working directory
cd "/Users/nicolemason/Documents/Dissertation/Essay1_FRA_maize_prices/"

**** Get full dataset
use "tmp/Mason_&_Myers_dataset_full.dta", clear

generate t=_n

*tsset data
tsset date, format(%tmm._CY)

*Estimate RF VAR FOR LOW REGIME
var CHOMA LUSAKA SAFEX BPP SPP MCHINJI if date<=525, lags(1/3)

*Save residuals and coefficients
predict lUCHOMA, equation(#1) resid
predict lULUSAKA, equation(#2) resid

scalar lb1cons=[#1]_b[_cons]   
scalar lb1LCHOMA=[#1]_b[L.CHOMA]
scalar lb1L2CHOMA=[#1]_b[L2.CHOMA]
scalar lb1L3CHOMA=[#1]_b[L3.CHOMA]
scalar lb1LLUSAKA=[#1]_b[L.LUSAKA]
scalar lb1L2LUSAKA=[#1]_b[L2.LUSAKA]
scalar lb1L3LUSAKA=[#1]_b[L3.LUSAKA]
scalar lb1LSAFEX=[#1]_b[L.SAFEX]
scalar lb1L2SAFEX=[#1]_b[L2.SAFEX]
scalar lb1L3SAFEX=[#1]_b[L3.SAFEX]
scalar lb1LBPP=[#1]_b[L.BPP]
scalar lb1L2BPP=[#1]_b[L2.BPP]
scalar lb1L3BPP=[#1]_b[L3.BPP]
scalar lb1LSPP=[#1]_b[L.SPP]
scalar lb1L2SPP=[#1]_b[L2.SPP]
scalar lb1L3SPP=[#1]_b[L3.SPP]
scalar lb1LMCHINJI=[#1]_b[L.MCHINJI]
scalar lb1L2MCHINJI=[#1]_b[L2.MCHINJI]
scalar lb1L3MCHINJI=[#1]_b[L3.MCHINJI]

scalar lb2cons=[#2]_b[_cons]   
scalar lb2LCHOMA=[#2]_b[L.CHOMA]
scalar lb2L2CHOMA=[#2]_b[L2.CHOMA]
scalar lb2L3CHOMA=[#2]_b[L3.CHOMA]
scalar lb2LLUSAKA=[#2]_b[L.LUSAKA]
scalar lb2L2LUSAKA=[#2]_b[L2.LUSAKA]
scalar lb2L3LUSAKA=[#2]_b[L3.LUSAKA]
scalar lb2LSAFEX=[#2]_b[L.SAFEX]
scalar lb2L2SAFEX=[#2]_b[L2.SAFEX]
scalar lb2L3SAFEX=[#2]_b[L3.SAFEX]
scalar lb2LBPP=[#2]_b[L.BPP]
scalar lb2L2BPP=[#2]_b[L2.BPP]
scalar lb2L3BPP=[#2]_b[L3.BPP]
scalar lb2LSPP=[#2]_b[L.SPP]
scalar lb2L2SPP=[#2]_b[L2.SPP]
scalar lb2L3SPP=[#2]_b[L3.SPP]
scalar lb2LMCHINJI=[#2]_b[L.MCHINJI]
scalar lb2L2MCHINJI=[#2]_b[L2.MCHINJI]
scalar lb2L3MCHINJI=[#2]_b[L3.MCHINJI]

set more off
*Estimate RF VAR FOR HIGH REGIME
var CHOMA LUSAKA SAFEX BPP SPP MCHINJI if date>525, lags(1/3)

*Save residuals and coefficients
predict hUCHOMA, equation(#1) resid
predict hULUSAKA, equation(#2) resid

scalar hb1cons=[#1]_b[_cons]   
scalar hb1LCHOMA=[#1]_b[L.CHOMA]
scalar hb1L2CHOMA=[#1]_b[L2.CHOMA]
scalar hb1L3CHOMA=[#1]_b[L3.CHOMA]
scalar hb1LLUSAKA=[#1]_b[L.LUSAKA]
scalar hb1L2LUSAKA=[#1]_b[L2.LUSAKA]
scalar hb1L3LUSAKA=[#1]_b[L3.LUSAKA]
scalar hb1LSAFEX=[#1]_b[L.SAFEX]
scalar hb1L2SAFEX=[#1]_b[L2.SAFEX]
scalar hb1L3SAFEX=[#1]_b[L3.SAFEX]
scalar hb1LBPP=[#1]_b[L.BPP]
scalar hb1L2BPP=[#1]_b[L2.BPP]
scalar hb1L3BPP=[#1]_b[L3.BPP]
scalar hb1LSPP=[#1]_b[L.SPP]
scalar hb1L2SPP=[#1]_b[L2.SPP]
scalar hb1L3SPP=[#1]_b[L3.SPP]
scalar hb1LMCHINJI=[#1]_b[L.MCHINJI]
scalar hb1L2MCHINJI=[#1]_b[L2.MCHINJI]
scalar hb1L3MCHINJI=[#1]_b[L3.MCHINJI]

scalar hb2cons=[#2]_b[_cons]   
scalar hb2LCHOMA=[#2]_b[L.CHOMA]
scalar hb2L2CHOMA=[#2]_b[L2.CHOMA]
scalar hb2L3CHOMA=[#2]_b[L3.CHOMA]
scalar hb2LLUSAKA=[#2]_b[L.LUSAKA]
scalar hb2L2LUSAKA=[#2]_b[L2.LUSAKA]
scalar hb2L3LUSAKA=[#2]_b[L3.LUSAKA]
scalar hb2LSAFEX=[#2]_b[L.SAFEX]
scalar hb2L2SAFEX=[#2]_b[L2.SAFEX]
scalar hb2L3SAFEX=[#2]_b[L3.SAFEX]
scalar hb2LBPP=[#2]_b[L.BPP]
scalar hb2L2BPP=[#2]_b[L2.BPP]
scalar hb2L3BPP=[#2]_b[L3.BPP]
scalar hb2LSPP=[#2]_b[L.SPP]
scalar hb2L2SPP=[#2]_b[L2.SPP]
scalar hb2L3SPP=[#2]_b[L3.SPP]
scalar hb2LMCHINJI=[#2]_b[L.MCHINJI]
scalar hb2L2MCHINJI=[#2]_b[L2.MCHINJI]
scalar hb2L3MCHINJI=[#2]_b[L3.MCHINJI]

/* Compute Simulated Prices in the Absence of FRA - Adapted from Jayne et al. (2008) */
      
capture program drop varcomsb
program define varcomsb

      mkmat CHOMA LUSAKA SAFEX BPP SPP lUCHOMA lULUSAKA hUCHOMA hULUSAKA date MCHINJI
      matrix pchosim = CHOMA
      matrix plsksim = LUSAKA
      matrix psfxsim = SAFEX
      matrix pmchsim = MCHINJI
      matrix DATE = date

      scalar i = 4
      while i <= 150 {

/*LOW REGIME*/

	  if DATE[i,1]<= 525	{
	
            matrix pchosim[i,1] = lb1cons                                                          /*
		  */ + lb1LCHOMA*pchosim[i-1,1]+lb1L2CHOMA*pchosim[i-2,1]+lb1L3CHOMA*pchosim[i-3,1] /*
		  */ + lb1LLUSAKA*plsksim[i-1,1]+lb1L2LUSAKA*plsksim[i-2,1]+lb1L3LUSAKA*plsksim[i-3,1] /*
		  */ + lb1LSAFEX*psfxsim[i-1,1]+lb1L2SAFEX*psfxsim[i-2,1]+lb1L3SAFEX*psfxsim[i-3,1] /*
		  */ + lb1LMCHINJI*pmchsim[i-1,1]+lb1L2MCHINJI*pmchsim[i-2,1]+lb1L3MCHINJI*pmchsim[i-3,1] /*
		  */ + lUCHOMA[i,1]
		  
            matrix plsksim[i,1] = lb2cons                                                          /*
		  */ + lb2LCHOMA*pchosim[i-1,1]+lb2L2CHOMA*pchosim[i-2,1]+lb2L3CHOMA*pchosim[i-3,1] /*
		  */ + lb2LLUSAKA*plsksim[i-1,1]+lb2L2LUSAKA*plsksim[i-2,1]+lb2L3LUSAKA*plsksim[i-3,1] /*
		  */ + lb2LSAFEX*psfxsim[i-1,1]+lb2L2SAFEX*psfxsim[i-2,1]+lb2L3SAFEX*psfxsim[i-3,1] /*
		  */ + lb2LMCHINJI*pmchsim[i-1,1]+lb2L2MCHINJI*pmchsim[i-2,1]+lb2L3MCHINJI*pmchsim[i-3,1] /*
		  */ + lULUSAKA[i,1]
		  
		  }
		  
/*HIGH REGIME*/

		 if DATE[i,1]> 525		{
	
            matrix pchosim[i,1] = hb1cons                                                          /*
		  */ + hb1LCHOMA*pchosim[i-1,1]+hb1L2CHOMA*pchosim[i-2,1]+hb1L3CHOMA*pchosim[i-3,1] /*
		  */ + hb1LLUSAKA*plsksim[i-1,1]+hb1L2LUSAKA*plsksim[i-2,1]+hb1L3LUSAKA*plsksim[i-3,1] /*
		  */ + hb1LSAFEX*psfxsim[i-1,1]+hb1L2SAFEX*psfxsim[i-2,1]+hb1L3SAFEX*psfxsim[i-3,1] /*
		  */ + hb1LMCHINJI*pmchsim[i-1,1]+hb1L2MCHINJI*pmchsim[i-2,1]+hb1L3MCHINJI*pmchsim[i-3,1] /*
		  */ + hUCHOMA[i,1]
		  
            matrix plsksim[i,1] = hb2cons                                                          /*
		  */ + hb2LCHOMA*pchosim[i-1,1]+hb2L2CHOMA*pchosim[i-2,1]+hb2L3CHOMA*pchosim[i-3,1] /*
		  */ + hb2LLUSAKA*plsksim[i-1,1]+hb2L2LUSAKA*plsksim[i-2,1]+hb2L3LUSAKA*plsksim[i-3,1] /*
		  */ + hb2LSAFEX*psfxsim[i-1,1]+hb2L2SAFEX*psfxsim[i-2,1]+hb2L3SAFEX*psfxsim[i-3,1] /*
		  */ + hb2LMCHINJI*pmchsim[i-1,1]+hb2L2MCHINJI*pmchsim[i-2,1]+hb2L3MCHINJI*pmchsim[i-3,1] /*
		  */ + hULUSAKA[i,1]
		  
		  }
		    
		 

/* Make Readable for Accuracy Test 

/*LOW REGIME*/

	  if DATE[i,1]<= 525	{


			matrix pchosim[i,1] = pchosim[i,1]                                                               /*
          */ + lb1LBPP*BPP[i-1,1]+lb1L2BPP*BPP[i-2,1]+lb1L3BPP*BPP[i-3,1] /*
		  */ + lb1LSPP*SPP[i-1,1]+lb1L2SPP*SPP[i-2,1]+lb1L3SPP*SPP[i-3,1] 

			matrix plsksim[i,1] = plsksim[i,1]                                                               /*
          */ +lb2LBPP*BPP[i-1,1]+lb2L2BPP*BPP[i-2,1]+lb2L3BPP*BPP[i-3,1] /*
		  */ +lb2LSPP*SPP[i-1,1]+lb2L2SPP*SPP[i-2,1]+lb2L3SPP*SPP[i-3,1] 

		  }

/*HIGH REGIME*/

	  if DATE[i,1]> 525	{

			matrix pchosim[i,1] = pchosim[i,1]                                                               /*
          */ + hb1LBPP*BPP[i-1,1]+hb1L2BPP*BPP[i-2,1]+hb1L3BPP*BPP[i-3,1] /*
		  */ + hb1LSPP*SPP[i-1,1]+hb1L2SPP*SPP[i-2,1]+hb1L3SPP*SPP[i-3,1] 

			matrix plsksim[i,1] = plsksim[i,1]                                                               /*
          */ +hb2LBPP*BPP[i-1,1]+hb2L2BPP*BPP[i-2,1]+hb2L3BPP*BPP[i-3,1] /*
		  */ +hb2LSPP*SPP[i-1,1]+hb2L2SPP*SPP[i-2,1]+hb2L3SPP*SPP[i-3,1] 

		  }

 End of accuracy test */

      	scalar i = i + 1 
      	}

    svmat pchosim, name(CHOMASIMsb)
	svmat plsksim, name(LUSAKASIMsb)

end

varcomsb


*Graph Choma historical and simulated prices
twoway (line CHOMA date, lpattern(solid) clcolor(black) cmissing(n)) ///
(line CHOMASIMsb1 date, lpattern(shortdash) clcolor(black) cmissing(n)), ///
ysize (3.5) xsize(6) ///ytitle("Wholesale maize price (ZMK/kg)", size(medsmall) ) ///ylabel(#10, angle(horizontal) format(%9.0gc) labsize(medsmall)) /// 
xline(525) ///xtitle("") ///xlabel(#14,  angle(45) labsize(medsmall) tstyle(major) valuelabel) ///
xmtick(##12) ///legend(row(1) size(medsmall) ///lab(1 "Historical") ///lab(2 "Simulated"))


*Graph Lusaka historical and simulated prices
twoway (line LUSAKA date, lpattern(solid) clcolor(black) cmissing(n)) ///
(line LUSAKASIMsb1 date, lpattern(shortdash) clcolor(black) cmissing(n)), ///
ysize (3.5) xsize(6) ///ytitle("Wholesale maize price (ZMK/kg)", size(medsmall) ) ///ylabel(#10, angle(horizontal) format(%9.0gc) labsize(medsmall)) /// 
xline(525) ///xtitle("") ///xlabel(#14,  angle(45) labsize(medsmall) tstyle(major) valuelabel) ///
xmtick(##12) ///legend(row(1) size(medsmall) ///lab(1 "Historical") ///lab(2 "Simulated"))

**************** SIMULATED PRICES *NOT* IN REASONABLE RANGE ****************



