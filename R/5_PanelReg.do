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

estpost tabstat price price_deviation fra_purchase fra_sales  maxdays raincytot day1rain tmean  safex   heatday  buy_iv sell_iv2, by(prod_region) ///
     statistics(mean sd min max) columns(statistics) listwise
 
 *****************************************************************
* Table 2: Price regression table 
*****************************************************************

* OLS
use full_data,clear

eststo clear
* eststo: xtreg price fra_purchase fra_sales  maxdays raincytot tmean safex stock_end heatday  year i.month i.mkt_code
eststo: reg price  fra_purchase fra_sales  maxdays raincytot day1rain tmean     heatday safex annual_import i.month i.mkt_code, vce (cluster prov_code) 
eststo:ivreg2  price maxdays  raincytot day1rain tmean    heatday safex  annual_import  prov_code i.mkt_code i.month (fra_purchase fra_sales = buy_iv sell_iv2),  partial( prov_code) 
eststo:ivreg2  price safex   annual_import  prov_code i.year i.mkt_code i.month (fra_purchase fra_sales = buy_iv sell_iv2),  partial( prov_code) 
eststo:ivreg2  price  l.price   safex annual_import   prov_code i.mkt_code i.month (fra_purchase fra_sales = buy_iv sell_iv2),  partial( prov_code) 



esttab est1 est2 est3 est4 using price_reg.rtf, se replace ///
	keep(fra_purchase fra_sales L.price maxdays raincytot day1rain tmean heatday safex annual_import  )  ///
	star(* 0.10 ** 0.05 *** 0.01) b(3) se(3) ///
	stat(N N_clust idstat widstat, fmt(0 0 3 3 ) labels("N" "Cluster" "Underidentification" "Weak identification"))  


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
eststo: reg price_deviation  fra_purchase fra_sales  maxdays raincytot day1rain tmean     heatday safex annual_import i.month i.mkt_code, vce (cluster prov_code) 
eststo:ivreg2  price_deviation maxdays  raincytot day1rain tmean    heatday safex  annual_import  prov_code i.mkt_code i.month (fra_purchase fra_sales = buy_iv sell_iv2),  partial( prov_code) 
eststo:ivreg2  price_deviation safex   annual_import  prov_code i.year i.mkt_code i.month (fra_purchase fra_sales = buy_iv sell_iv2),  partial( prov_code) 
eststo:ivreg2  price_deviation  l.price_deviation   safex annual_import   prov_code i.mkt_code i.month (fra_purchase fra_sales = buy_iv sell_iv2),  partial( prov_code) 



esttab est1 est2 est3 est4 using dev_reg.rtf, se replace ///
	keep(fra_purchase fra_sales L.price_deviation maxdays raincytot day1rain tmean heatday safex annual_import  )  ///
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
 
 xtset year mkt_code
 save annual_data.dta,replace

* OLS
use annual_data,clear

eststo clear
eststo: reg price_cv fra_purchase   fra_sales  maxdays   raincytot day1rain tmean     heatday  annual_import i.year i.mkt_code, vce (cluster prov_code) 
eststo:ivreg2  price_cv maxdays  raincytot day1rain tmean    heatday    annual_import  prov_code  i.mkt_code   (fra_purchase fra_sales = buy_iv sell_iv2) ,  partial( prov_code)  
eststo: reg price_sd fra_purchase   fra_sales maxdays   raincytot day1rain tmean     heatday  annual_import i.year i.mkt_code, vce (cluster prov_code) 
eststo:ivreg2  price_sd maxdays  raincytot day1rain tmean    heatday    annual_import  prov_code  i.mkt_code   (fra_purchase fra_sales = buy_iv sell_iv2) ,  partial( prov_code)  


esttab est1 est2 est3 est4 using annual_reg.rtf, se replace ///
	keep(fra_purchase fra_sales maxdays raincytot day1rain tmean heatday annual_import  )  ///
	star(* 0.10 ** 0.05 *** 0.01) b(3) se(3) ///
	stat(N N_clust idstat widstat, fmt(0 0 3 3 ) labels("N" "Cluster" "Underidentification" "Weak identification"))  


 
*****************************************************************
* Table 4:  Robustness checks:  
* 1.	Local vs country average vs production region
* 2. Interaction between FRA and weather 
* 3.	 Split sample by region, by transport cost 
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


** 2. interactions between FRA + weather


* expect years with more rain,  the sales to have a smaller effect on the price 
* so less negative or more postive than a regular year	


* expect years with more rain,  the purchase to have a larger effect (maybe) on the price 
* so more postive than a regular year	

 


* past purchases - sales over the past year(s) 
 
 
 *****************************************************************
* Table A1: cointegration analysis  
*****************************************************************



 
*****************************************************************
* Table A2: First stage 
*****************************************************************

use full_data,clear

eststo clear
eststo: reg price  fra_purchase fra_sales  maxdays raincytot tmean     heatday safex annual_import i.month i.mkt_code, vce (cluster prov_code) 
eststo:ivreg2  price maxdays  raincytot tmean    heatday safex  annual_import  prov_code i.mkt_code i.month (fra_purchase fra_sales = buy_iv sell_iv2),  partial( prov_code)  first savefirst savefprefix(st2)
eststo:ivreg2  price safex   annual_import  prov_code i.year i.mkt_code i.month (fra_purchase fra_sales = buy_iv sell_iv2),  partial( prov_code) first savefirst savefprefix(st3)
eststo:ivreg2  price  l.price   safex annual_import   prov_code i.mkt_code i.month (fra_purchase fra_sales = buy_iv sell_iv2),  partial( prov_code) first savefirst savefprefix(st4)



esttab   st2* st3* st4* using first_stage.rtf, se replace ///
	keep( buy_iv sell_iv2 L.price maxdays raincytot tmean heatday safex annual_import  )  ///
	star(* 0.10 ** 0.05 *** 0.01) b(3) se(3) ///
	stat(N N_clust ar2, fmt(0 0 3 3 ) labels("N" "Cluster"   "r2"))  

 
 

 

