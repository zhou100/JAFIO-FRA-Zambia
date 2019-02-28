set matsize 2000
set more off

 import delimited "C:\Users\Administrator\iCloudDrive\Year5\Research Projects\ZambiaPrice\data\clean\dataset.csv",clear
 
 encode mkt_name,gen(mkt_code)
gen date_string=date(date,"YMD###")

format date_string %td

 xtset date_string mkt_code

 
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


 gen distance_weight =1/(distance_km+1)
 
 gen weighted_buy_dev = long_run_share * pred_dev_prod * month_share_buy
 gen weighted_buy_dev2 = long_run_share * pred_national_prod * month_share_buy

 gen weighted_sale_dev = month_share_sale * pred_national_prod * distance_weight
 
 
 * ssc install ivreg2
 ivreg2   i.mkt_code i.month , ffirst first
 ivreg2  dev_price_square  maxdays raincytot tmean  safex stock_end heatday  year i.mkt_code i.month (logpurchase weighted_fra_sales = weighed_dev weighted_sale_dev weighted_buy_dev2), ffirst first

 
 * 3 IVS
eststo clear
eststo: xi:  ivreg2  dev_price_square  maxdays raincytot tmean   safex stock_end heatday  year i.mkt_code i.month (logpurchase weighted_fra_sales = weighted_buy_dev weighted_sale_dev weighted_buy_dev2), ffirst first savefirst savefprefix(st1) 
esttab st1* using dev_1st.xls, replace
esttab est1 using dev_2nd.xls, replace

 eststo clear
eststo: xi:  ivreg2  price  maxdays raincytot tmean mchinji safex stock_end heatday  year i.mkt_code i.month (logpurchase weighted_fra_sales = weighted_buy_dev weighted_sale_dev weighted_buy_dev2), ffirst first savefirst savefprefix(st1) 
esttab st1* using price_1st.xls, replace
esttab est1 using price_2nd.xls, replace

* Two iv  
 
eststo clear
eststo: xi:  ivreg2  dev_price_square  maxdays raincytot tmean mchinji safex stock_end heatday  year i.mkt_code i.month (logpurchase weighted_fra_sales = weighed_dev weighted_sale_dev), ffirst first savefirst savefprefix(st2) 
esttab st2* using dev_1st_2iv.xls, replace
esttab est1 using dev_2nd_2iv.xls, replace

eststo clear
eststo: xi:  ivreg2  price  maxdays raincytot tmean mchinji safex stock_end heatday  year i.mkt_code i.month (logpurchase weighted_fra_sales = weighed_dev weighted_sale_dev), ffirst first savefirst savefprefix(st2) 
esttab st2* using price_1st_2iv.xls, replace
esttab est1 using price_2nd_2iv.xls, replace




* OLS

 
eststo clear
eststo: reg price logpurchase weighted_fra_sales  maxdays raincytot tmean mchinji safex stock_end heatday  year i.mkt_code i.month  
esttab est1 using price_OLS.xls, replace

eststo clear
eststo: reg dev_price_square logpurchase weighted_fra_sales  maxdays raincytot tmean mchinji safex stock_end heatday  year i.mkt_code i.month  
esttab est1 using dev_OLS.xls, replace


* One endogenous var (buy)

eststo clear
eststo: xi:  ivreg2  dev_price_square  maxdays raincytot tmean mchinji safex stock_end heatday  year i.mkt_code i.month (logpurchase  = weighed_dev weighted_sale_dev weighted_buy_dev2), ffirst first savefirst savefprefix(st1_buy) 
esttab st1_buy* using dev_1st_buy.xls, replace
esttab est1 using dev_2nd_buy.xls, replace

 eststo clear
eststo: xi:  ivreg2  price  maxdays raincytot tmean mchinji safex stock_end heatday  year i.mkt_code i.month (logpurchase = weighed_dev weighted_sale_dev weighted_buy_dev2), ffirst first savefirst savefprefix(st1) 
esttab st1* using price_1st.xls, replace
esttab est1 using price_2nd.xls, replace

* Two iv  
 
eststo clear
eststo: xi:  ivreg2  dev_price_square  maxdays raincytot tmean mchinji safex stock_end heatday  year i.mkt_code i.month (logpurchase  = weighed_dev weighted_sale_dev), ffirst first savefirst savefprefix(st2) 
esttab est1 using dev_2nd_2iv.xls, replace

eststo clear
eststo: xi:  ivreg2  price  maxdays raincytot tmean mchinji safex stock_end heatday  year i.mkt_code i.month (logpurchase  = weighed_dev weighted_sale_dev), ffirst first savefirst savefprefix(st2) 
 esttab est1 using price_2nd_2iv.xls, replace





* OLS

 
eststo clear
eststo: reg  logpurchase  maxdays raincytot tmean mchinji safex stock_end heatday  year i.mkt_code i.month  
esttab est1 using price_OLS.xls, replace

eststo clear
eststo: reg dev_price_square logpurchase  maxdays raincytot tmean mchinji safex stock_end heatday  year i.mkt_code i.month  
esttab est1 using dev_OLS.xls, replace




* One endogenous var (sale)

eststo clear
eststo: xi:  ivreg2  dev_price_square  maxdays raincytot tmean mchinji safex stock_end heatday  year i.mkt_code i.month (weighted_fra_sales  = weighed_dev weighted_sale_dev weighted_buy_dev2), ffirst first savefirst savefprefix(st1_buy) 
esttab st1_buy* using dev_1st_buy.xls, replace
esttab est1 using dev_2nd_buy.xls, replace

 eststo clear
eststo: xi:  ivreg2  price  maxdays raincytot tmean mchinji safex stock_end heatday  year i.mkt_code i.month (weighted_fra_sales = weighed_dev weighted_sale_dev weighted_buy_dev2), ffirst first savefirst savefprefix(st1) 
esttab st1* using price_1st.xls, replace
esttab est1 using price_2nd.xls, replace

* Two iv  
 
eststo clear
eststo: xi:  ivreg2  dev_price_square  maxdays raincytot tmean mchinji safex stock_end heatday  year i.mkt_code i.month (weighted_fra_sales  = weighed_dev weighted_sale_dev), ffirst first savefirst savefprefix(st2) 
esttab est1 using dev_2nd_2iv.xls, replace

eststo clear
eststo: xi:  ivreg2  price  maxdays raincytot tmean mchinji safex stock_end heatday  year i.mkt_code i.month (weighted_fra_sales  = weighed_dev weighted_sale_dev), ffirst first savefirst savefprefix(st2) 
 esttab est1 using price_2nd_2iv.xls, replace



* OLS

 
eststo clear
eststo: reg price weighted_fra_sales  maxdays raincytot tmean mchinji safex stock_end heatday  year i.mkt_code i.month  
esttab est1 using price_OLS.xls, replace

eststo clear
eststo: reg dev_price_square weighted_fra_sales  maxdays raincytot tmean mchinji safex stock_end heatday  year i.mkt_code i.month  
esttab est1 using dev_OLS.xls, replace








eststo clear
eststo: reg logpurchase   maxdays raincytot tmean mchinji safex stock_end heatday  year i.mkt_code i.month  
esttab est1 using price_OLS.xls, replace

eststo clear
eststo: reg weighted_fra_sales  maxdays raincytot tmean mchinji safex stock_end heatday  year i.mkt_code i.month  
esttab est1 using dev_OLS.xls, replace

