set matsize 2000
set more off

  * ssc install ivreg2 


import delimited "C:\Users\Administrator\iCloudDrive\Year5\Research Projects\ZambiaPrice\data\clean\dataset.csv",clear

 *import delimited "/Users/yujunzhou/Box Sync/Research/ZambiaPrice/data/clean/dataset.csv", encoding(ISO-8859-1) clear
 

 encode mkt_name,gen(mkt_code)
 encode region,gen(region_code)
 encode province,gen(prov_code)
   
gen date_string=date(date,"YMD###")

format date_string %td

 xtset date_string mkt_code

gen rainsq = raincytot^2
gen prod_rainsq = prod_rain^2
gen na_rainsq = na_rain^2

save full_data.dta,replace

*drop if prod_region==1
*save consu_subdata.dta,replace


*drop if prod_region==0
*save prod_subdata.dta,replace
 

 * ssc install ivreg2 


 
 *****************************************************************
* Table 1: Summary statistics
*****************************************************************

 
use full_data,clear
eststo clear
replace price_deviation = price_deviation/1000


estpost tabstat price price_deviation fra_purchase fra_sales  prod_maxdays prod_rain prod_rainsq prod_day1rain prod_tmean  safex   heatday  buy_iv sell_iv2,  ///
     statistics(mean sd min max) columns(statistics) listwise 
 *****************************************************************
* Table 2: Price regression table 
*****************************************************************

* OLS
use full_data,clear

eststo clear
* eststo: xtreg price fra_purchase fra_sales  maxdays raincytot tmean safex stock_end heatday  year i.month i.mkt_code

* Use production region vars 

eststo: reg price  fra_purchase fra_sales  prod_maxdays prod_rain prod_rainsq   prod_day1rain     prod_tmean   safex  i.month i.mkt_code, vce (cluster prov_code) 
eststo:ivreg2  price prod_maxdays  prod_rain  prod_rainsq   prod_day1rain     prod_tmean   safex    prov_code i.mkt_code i.month (fra_purchase fra_sales = buy_iv sell_iv2),  partial( prov_code) 
eststo:ivreg2  price safex   prov_code i.year i.mkt_code i.month (fra_purchase fra_sales = buy_iv sell_iv2),  partial( prov_code) 
eststo:ivreg2  price  l.price  safex    prov_code i.mkt_code  i.year i.month (fra_purchase fra_sales = buy_iv sell_iv2),  partial( prov_code) 

esttab est1 est2 est3 est4 using price_reg.rtf, se replace ///
	keep(fra_purchase fra_sales L.price prod_maxdays prod_rain prod_rainsq  prod_day1rain      prod_tmean   safex   )  ///
	star(* 0.10 ** 0.05 *** 0.01) b(3) se(3) ///
	stat(N N_clust idstat widstat, fmt(0 0 3 3 ) labels("N" "Cluster" "Underidentification" "Weak identification"))  



* using local weather as robusness check 
/*
eststo: reg price  fra_purchase fra_sales  maxdays raincytot rainsq    day1rain     gdd  heatday safex  i.month i.mkt_code, vce (cluster prov_code) 
eststo:ivreg2  price maxdays  raincytot rainsq    day1rain     gdd heatday safex    prov_code i.mkt_code i.month (fra_purchase fra_sales = buy_iv sell_iv2),  partial( prov_code) 
eststo:ivreg2  price safex   prov_code i.year i.mkt_code i.month (fra_purchase fra_sales = buy_iv sell_iv2),  partial( prov_code) 
eststo:ivreg2  price  l.price  safex    prov_code i.mkt_code i.month (fra_purchase fra_sales = buy_iv sell_iv2),  partial( prov_code) 


esttab est1 est2 est3 est4 using price_reg.rtf, se replace ///
	keep(fra_purchase fra_sales L.price maxdays raincytot rainsq day1rain      gdd heatday safex   )  ///
	star(* 0.10 ** 0.05 *** 0.01) b(3) se(3) ///
	stat(N N_clust idstat widstat, fmt(0 0 3 3 ) labels("N" "Cluster" "Underidentification" "Weak identification"))  

*/

*use prod_subdata,clear
*eststo:ivreg2  price l.price  maxdays  raincytot tmean   safex heatday  trend  prov_code i.mkt_code i.month (fra_purchase fra_sales = buy_iv sell_iv2),  partial( prov_code)

*use consu_subdata,clear
*eststo:ivreg2  price   l.price maxdays  raincytot tmean   safex heatday  trend  prov_code i.mkt_code i.month (fra_purchase fra_sales = buy_iv sell_iv2),  partial( prov_code)

	   
   

*****************************************************************
*Table 3: Price deviation regression table 
*****************************************************************
	   
	   
* OLS
use full_data,clear

eststo clear

replace price_deviation = price_deviation/1000
 
eststo: reg price_deviation  fra_purchase fra_sales  prod_maxdays prod_rain prod_rainsq   prod_day1rain     prod_tmean   safex  i.month i.mkt_code, vce (cluster prov_code) 
eststo:ivreg2  price_deviation prod_maxdays  prod_rain  prod_rainsq   prod_day1rain     prod_tmean   safex    prov_code i.mkt_code i.month (fra_purchase fra_sales = buy_iv sell_iv2),  partial( prov_code) 
eststo:ivreg2  price_deviation safex   prov_code i.year i.mkt_code i.month (fra_purchase fra_sales = buy_iv sell_iv2),  partial( prov_code) 
eststo:ivreg2  price_deviation  l.price  safex    prov_code i.mkt_code  i.year i.month (fra_purchase fra_sales = buy_iv sell_iv2),  partial( prov_code) 

esttab est1 est2 est3 est4 using dev_reg.rtf, se replace ///
	keep(fra_purchase fra_sales L.price prod_maxdays prod_rain prod_rainsq  prod_day1rain      prod_tmean   safex   )  ///
	star(* 0.10 ** 0.05 *** 0.01) b(3) se(3) ///
	stat(N N_clust idstat widstat, fmt(0 0 3 3 ) labels("N" "Cluster" "Underidentification" "Weak identification"))  


 

*****************************************************************
* Table 4:  ancilliary regression using annual data 
* Annual regressions: mean, CV and sd 
*****************************************************************
import delimited "C:\Users\Administrator\iCloudDrive\Year5\Research Projects\ZambiaPrice\data\clean\annual.csv",clear

 encode mkt_name,gen(mkt_code)
  encode region,gen(region_code)
 encode province,gen(prov_code)
 
 gen  fra_sals_rain = fra_sales * maxdays
 gen sell_iv_rain= sell_iv2 * maxdays
 
  gen  fra_buy_rain = fra_purchase * maxdays
  gen  buy_iv_rain = buy_iv * maxdays

  replace price_cv = price_cv *10000
 
 xtset year mkt_code
 save annual_data.dta,replace

* OLS
use annual_data,clear
gen rainsq= rain^2

eststo clear
eststo: reg price_cv fra_purchase fra_buy_rain   fra_sales  maxdays   raincytot rainsq day1rain tmean         i.mkt_code  
eststo:ivreg2  price_cv      prov_code maxdays   raincytot rainsq day1rain tmean        i.mkt_code   ( fra_sales fra_purchase  = buy_iv sell_iv2    ),  partial( prov_code) 
eststo:ivreg2  price_cv      prov_code maxdays   raincytot rainsq day1rain tmean        i.mkt_code   ( fra_purchase fra_buy_rain  = buy_iv buy_iv_rain    ),  partial( prov_code) 
eststo:ivreg2  price_cv      prov_code i.year i.mkt_code   ( fra_purchase fra_buy_rain  = buy_iv buy_iv_rain    ),  partial( prov_code) 

esttab est1 est2 est3 est4 using annual_reg.rtf, se replace ///
	keep(fra_purchase fra_buy_rain fra_sales maxdays raincytot rainsq day1rain tmean      )  ///
	star(* 0.10 ** 0.05 *** 0.01) b(3) se(3) ///
	stat(N N_clust idstat widstat, fmt(0 0 3 3 ) labels("N" "Cluster" "Underidentification" "Weak identification"))  


 
*****************************************************************
* Table 5:  Robustness checks:  
* 1.	Local vs country average vs production region
*****************************************************************
	   
import delimited "C:\Users\Administrator\iCloudDrive\Year5\Research Projects\ZambiaPrice\data\clean\data_check.csv",clear

 
 encode mkt_name,gen(mkt_code)
 encode region,gen(region_code)
 encode province,gen(prov_code)
 

eststo clear
* eststo: xtreg price fra_purchase fra_sales  maxdays raincytot tmean safex stock_end heatday  year i.month i.mkt_code
eststo:ivreg2  price maxdays  raincytot day1rain tmean    heatday safex  annual_import  prov_code i.mkt_code i.month (fra_purchase fra_sales = buy_iv sell_iv2),  partial( prov_code) 
eststo:ivreg2  price na_maxdays  na_rain  na_day1rain na_tmean  na_heatday safex  annual_import  prov_code i.mkt_code i.month (fra_purchase fra_sales = buy_iv sell_iv2),  partial( prov_code) 
eststo:ivreg2  price prod_maxdays  prod_rain  prod_day1rain prod_tmean    prod_heatday safex  annual_import  prov_code i.mkt_code i.month (fra_purchase fra_sales = buy_iv sell_iv2),  partial( prov_code) 
eststo:ivreg2  price maxdays  raincytot day1rain tmean    heatday safex  annual_import exportban stock_end  prov_code   i.mkt_code i.month (fra_purchase fra_sales = buy_iv sell_iv2),  partial( prov_code) 
 


esttab est1 est2 est3 est4 using price_reg_check.rtf, se replace ///
	keep(fra_purchase fra_sales maxdays raincytot day1rain tmean heatday na_maxdays  na_rain  na_day1rain na_tmean  na_heatday prod_maxdays  prod_rain  prod_day1rain prod_tmean    prod_heatday safex annual_import  )  ///
	star(* 0.10 ** 0.05 *** 0.01) b(3) se(3) ///
	stat(N N_clust idstat widstat, fmt(0 0 3 3 ) labels("N" "Cluster" "Underidentification" "Weak identification"))  
 		
	
		
gen inter_fra_buy = raincytot * fra_purchase
gen inter_fra_sale = raincytot * fra_sale

 *****************************************************************
* Table 5:  Robustness checks:  
** 2. interactions between FRA + weather
*****************************************************************

* expect years with more rain,  the sales to have a smaller effect on the price 
* so less negative or more postive than a regular year	
* expect years with more rain,  the purchase to have a larger effect (maybe) on the price 
* so more postive than a regular year	


* past purchases - sales over the past year(s) 
 

*****************************************************************
* Table 5:  Robustness checks:  
* 3.	 Split sample by region, by transport cost 
*****************************************************************
	   
* Key market centers by USAID

use full_data,clear

tab mkt_name
 
 *                     
 *               Kalulushi        
  *         Kasempa    Kawambwa       Kitwe 
 *     Luangwa    Luanshya       
  *   Luwingu           Mazabuka        
   *          Monze    Mufulira       
    *  Mwense     Nchelenge      
     *                

 
 
 gen mkt_center = 1 if mkt_name == "Isoka"|mkt_name == "Mbala"| mkt_name == "Kasama" ///
 |mkt_name == "Mansa"|mkt_name == "Serenje"|mkt_name == "Choma"|mkt_name == "Chipata" ///
 |mkt_name == "Petauke"|mkt_name == "Kabwe"|mkt_name == "Lusaka"|mkt_name == "Solwezi" ///
 |mkt_name == "Mumbwa"|mkt_name == "Kalomo"|mkt_name == "Livingstone"|mkt_name == "Samfya" ///
  |mkt_name == "Mkushi"|mkt_name == "Mwinilunga"|mkt_name == "Chingola" |mkt_name == "Kaoma" ///
 
 
 replace mkt_center = 0 if mkt_center==.

 save split_data,replace

 
use split_data,clear
keep if mkt_center == 0
save remote.dta,replace 


use split_data,clear
keep if mkt_center == 1
save center.dta,replace 
 

* Regression by Key market centers  subset 

use center,clear
eststo clear
* eststo: xtreg price fra_purchase fra_sales  maxdays raincytot tmean safex stock_end heatday  year i.month i.mkt_code
* Use production region vars 
eststo: reg price  fra_purchase fra_sales  prod_maxdays prod_rain prod_rainsq   prod_day1rain     prod_tmean   safex  i.month i.mkt_code, vce (cluster prov_code) 
eststo:ivreg2  price prod_maxdays  prod_rain  prod_rainsq   prod_day1rain     prod_tmean   safex    prov_code i.mkt_code i.month (fra_purchase fra_sales = buy_iv sell_iv2),  partial( prov_code) 
eststo:ivreg2  price safex   prov_code i.year i.mkt_code i.month (fra_purchase fra_sales = buy_iv sell_iv2),  partial( prov_code) 
eststo:ivreg2  price  l.price  safex    prov_code i.mkt_code  i.year i.month (fra_purchase fra_sales = buy_iv sell_iv2),  partial( prov_code) 

esttab est1 est2 est3 est4 using center_price_reg.rtf, se replace ///
	keep(fra_purchase fra_sales L.price prod_maxdays prod_rain prod_rainsq  prod_day1rain      prod_tmean   safex   )  ///
	star(* 0.10 ** 0.05 *** 0.01) b(3) se(3) ///
	stat(N N_clust idstat widstat, fmt(0 0 3 3 ) labels("N" "Cluster" "Underidentification" "Weak identification"))  

* Regression by Key market centers  subset 

use remote,clear

eststo clear
eststo: reg price  fra_purchase fra_sales  prod_maxdays prod_rain prod_rainsq   prod_day1rain     prod_tmean   safex  i.month i.mkt_code, vce (cluster prov_code) 
eststo:ivreg2  price prod_maxdays  prod_rain  prod_rainsq   prod_day1rain     prod_tmean   safex    prov_code i.mkt_code i.month (fra_purchase fra_sales = buy_iv sell_iv2),  partial( prov_code) 
eststo:ivreg2  price safex   prov_code i.year i.mkt_code i.month (fra_purchase fra_sales = buy_iv sell_iv2),  partial( prov_code) 
eststo:ivreg2  price  l.price  safex    prov_code i.mkt_code  i.year i.month (fra_purchase fra_sales = buy_iv sell_iv2),  partial( prov_code) 

esttab est1 est2 est3 est4 using remote_price_reg.rtf, se replace ///
	keep(fra_purchase fra_sales L.price prod_maxdays prod_rain prod_rainsq  prod_day1rain      prod_tmean   safex   )  ///
	star(* 0.10 ** 0.05 *** 0.01) b(3) se(3) ///
	stat(N N_clust idstat widstat, fmt(0 0 3 3 ) labels("N" "Cluster" "Underidentification" "Weak identification"))  


	
 *****************************************************************
* Table A2: First stage 
*****************************************************************

use full_data,clear

eststo clear
eststo: reg price  fra_purchase fra_sales  prod_maxdays prod_rain prod_rainsq   prod_day1rain     prod_tmean   safex  i.month i.mkt_code, vce (cluster prov_code) 
eststo:ivreg2  price prod_maxdays  prod_rain  prod_rainsq   prod_day1rain     prod_tmean   safex    prov_code i.mkt_code i.month (fra_purchase fra_sales = buy_iv sell_iv2),  partial( prov_code) first savefirst savefprefix(st2)
eststo:ivreg2  price safex   prov_code i.year i.mkt_code i.month (fra_purchase fra_sales = buy_iv sell_iv2),  partial( prov_code) first savefirst savefprefix(st3)
eststo:ivreg2  price  l.price  safex    prov_code i.mkt_code  i.year i.month (fra_purchase fra_sales = buy_iv sell_iv2),  partial( prov_code) first savefirst savefprefix(st4)

 
esttab   st2* st3* st4* using first_stage.rtf, se replace ///
 	keep(buy_iv sell_iv2 L.price prod_maxdays prod_rain prod_rainsq  prod_day1rain      prod_tmean   safex   )  ///
	star(* 0.10 ** 0.05 *** 0.01) b(3) se(3) ///
	stat(N N_clust  r2, fmt(0 0 3 3 ) labels("N" "Cluster"   "r2"))  


 
 

