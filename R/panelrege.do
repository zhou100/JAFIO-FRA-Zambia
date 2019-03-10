set matsize 2000
set more off

 import delimited "C:\Users\Administrator\iCloudDrive\Year5\Research Projects\ZambiaPrice\data\clean\dataset.csv",clear

 
*use full_data,clear
 
 encode mkt_name,gen(mkt_code)
 encode region,gen(region_code)
 encode province,gen(prov_code)
   
gen date_string=date(date,"YMD###")

format date_string %td

 xtset date_string mkt_code

* save full_data.dta,replace

*drop if prod_region==1
*save consu_subdata.dta,replace


*drop if prod_region==0
*save prod_subdata.dta,replace
 
* 1.	First Stage
*fra_sales ~ monthly share + predict FRA stock + distance_weight + expected monthly share * the predicted FRA stocks from last year * distance-weights
*fra_buy ~ district_share + deviation_prod + monthly_share  + district_share* monthly_share*deviation_prod* 

*2.	List of IVs (need something that affects the FRA sales but not the quantity produced)

*FRA purchase:
*a.	Monthly share * deviation from average production in year t * average share purchased in each district    
*b.	Monthly share = split 2/9 in July, 3/9 in Aug, 3/9 and Sept and 1/9 in Oct

*FRA sales: 
*a.	distance-weighted predicted monthly sales to millers: 
* expected monthly share * the predicted FRA stocks for that year * distance-weights
*b.	monthly share: mostly of them sold in Dec to March
*c.	expected monthly share * expected total harvest from the CFS for that year * distance-weights
*d.	predicts the average amount and location of sales over all year’s times the changes in total FRA stocks
*e.	Total national stock: 
*i.	stock end   from last year 
*ii.	stock end + cfs prediction 



 * The IV is expected monthly purchase target( from CFS but not actual purchase)
 

 * ssc install ivreg2 


 
 *****************************************************************
* Table 1: Summary statistics
*****************************************************************

 
use full_data,clear
eststo clear

estpost tabstat price price_deviation fra_purchase fra_sales  maxdays raincytot tmean  safex   heatday  buy_iv_dev sell_iv2, by(prod_region) ///
     statistics(mean sd min max) columns(statistics) listwise
 
 esttab 
*****************************************************************
* Table 2: Price regression table 
*****************************************************************

* OLS
use full_data,clear

eststo clear
* eststo: xtreg price fra_purchase fra_sales  maxdays raincytot tmean safex stock_end heatday  year i.month i.mkt_code
eststo: reg price  fra_purchase fra_sales  maxdays raincytot tmean  safex   heatday  i.month i.mkt_code, vce (cluster prov_code)
eststo:ivreg2  price maxdays  raincytot tmean    heatday safex    prov_code i.mkt_code i.month (fra_purchase fra_sales = buy_iv sell_iv2),  partial( prov_code) 
eststo:ivreg2  price l.price safex   prov_code i.mkt_code i.month (fra_purchase fra_sales = buy_iv sell_iv2),  partial( prov_code) 
eststo:ivreg2  price  l.price annual_import i.year safex    prov_code i.mkt_code i.month (fra_purchase fra_sales = buy_iv sell_iv2),  partial( prov_code) 



esttab est1 est2 est3 est4 using price_reg.rtf, se replace ///
	keep(fra_purchase fra_sales L.price maxdays raincytot tmean heatday safex annual_import  )  ///
	star(* 0.10 ** 0.05 *** 0.01) b(3) se(3) ///
	stat(N N_clust idstat widstat, fmt(0 0 3 3 ) labels("N" "Cluster" "Underidentification" "Weak identification"))  


*use prod_subdata,clear
*eststo:ivreg2  price l.price  maxdays  raincytot tmean   safex heatday  trend  prov_code i.mkt_code i.month (fra_purchase fra_sales = buy_iv sell_iv2),  partial( prov_code)

*use consu_subdata,clear
*eststo:ivreg2  price   l.price maxdays  raincytot tmean   safex heatday  trend  prov_code i.mkt_code i.month (fra_purchase fra_sales = buy_iv sell_iv2),  partial( prov_code)

	   
   

*****************************************************************
* Price regression table 
*****************************************************************
	   
	   
* OLS
use full_data,clear

eststo clear
* eststo: xtreg price fra_purchase fra_sales  maxdays raincytot tmean safex stock_end heatday  year i.month i.mkt_code

eststo: reg price_deviation  fra_purchase fra_sales  maxdays raincytot tmean  safex   heatday  i.month i.mkt_code, vce (cluster prov_code)
eststo:ivreg2  price_deviation maxdays  raincytot tmean    heatday safex    prov_code i.mkt_code i.month (fra_purchase fra_sales = buy_iv sell_iv2),  partial( prov_code) 
eststo:ivreg2  price_deviation l.price_deviation safex   prov_code i.mkt_code i.month (fra_purchase fra_sales = buy_iv sell_iv2),  partial( prov_code) 
eststo:ivreg2  price_deviation  l.price_deviation annual_import i.year safex    prov_code i.mkt_code i.month (fra_purchase fra_sales = buy_iv sell_iv2),  partial( prov_code) 


esttab est1 est2 est3 est4 using dev_reg.rtf, se replace ///
	keep(fra_purchase fra_sales L.price_deviation maxdays raincytot tmean heatday safex annual_import)  ///
	star(* 0.10 ** 0.05 *** 0.01) b(3) se(3) ///
	stat(N N_clust idstat widstat, fmt(0 0 3 3 ) labels("N" "Cluster" "Underidentification" "Weak identification"))  
	    

 
 


