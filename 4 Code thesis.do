global tempdata="F:\data\workdata\709523\HousePricesRegional\02_TempData"
global savedata= "F:\data\workdata\709523\HousePricesRegional\ThesisEdoardo\TempData"


* merging HOUSING and OWNERS
forvalues i=1996/2016{
	use $tempdata\housing`i', clear
	merge 1:1 kom_1 ejdnr using $tempdata\owners`i'
	drop if _merge==2
	drop _merge 
	save $savedata\housing_owner_`i', replace
}

*append data
use $savedata\housing_owner_1996, clear 
forvalues i=1997/2016{
	append using $savedata\housing_owner_`i'
}

*duplicates drop kom_2 bopikom_2 year, force
sort kom_2 bopikom_2 year

save $savedata\master_housing_owners, replace



use $savedata\hedonicdata, clear
drop if repeat==0
cap drop id
egen id = group(kom_2 bopikom_2)
sort id year 
rename sold_date_sale pdate
gen pmonth = mofd(pdate)
format pmonth %tm

sort kom_2 bopikom_2 year

merge m:1 kom_2 bopikom_2 year using $savedata\master_housing_owners
keep if _merge==3
drop _merge

save $savedata\datasetmerged, replace





use $savedata\datasetmerged, clear

* we know that ind_finwealth_2total is define as:
* ind_finwealth_2  = bankakt + kursakt + oblakt + pantakt - bankgaeld - pantgaeld
* we need to subtract the mortgage (oblgaeld) 

* assets :  bankakt + kursakt + oblakt + pantakt
* liabilities : bankgaeld + pantgaeld + obgaeld
* ind_finwealth_2 = assets - liabilities


xtile wealth_dec = (ind_finwealth_2total-ind_mortgagetotal), nq(10)
xtile DTI = (ind_mortgagetotal+ind_bankdebttotal)/(ind_incometotal), nq (10)
xtile liquidity_dec = ind_liquiditytotal, nq (10)

* creating mortgage ca+tegories
gen LTV= (ind_mortgagetotal)/(sold_price_R)
gen LTV_cat=0
replace LTV_cat=1 if LTV < 0.6
replace LTV_cat=2 if LTV >= 0.6 & LTV < 0.8
replace LTV_cat=3 if LTV >= 0.8 & LTV < 1
replace LTV_cat=4 if LTV >= 1 

*here we are dropping the observations that we don't care about (same as previous researchers)
keep if drop_owner==0 & drop_bol==0

save $savedata\mergetransactionsowners, replace



********************************************************************************
*filtering and creating relevant variables
*********************************************************************************
use $savedata\mergetransactionsowners, clear

*now we drop observations that have missing info about educ and income 
drop if ind_NoInfo==1
drop if edu_NoInfo==1
drop if MissingEduc==1

rename Living living
rename repeat Repeat
rename pnr1 pnr
rename (sold_price_R Lsold_price_R) (sold_price_X Lsold_price_X)
rename (ejer_ThreeOrMore ejer_startdate1) (ThreeOrMore startdate1)
rename (ind_bankdebttotal_R ind_bondstotal_R ind_cashtotal_R ind_finasstotal_R ind_finwealth_1total_R ind_finwealth_2total_R ind_incometotal_R ind_liquiditytotal_R ind_netwealthtotal_R ind_mortgagetotal_R ind_stockstotal_R) (bankdebttotal bondstotal cashtotal finasstotal finwealth_1total finwealth_2total incometotal liquiditytotal netwealthtotal mortgagetotal stockstotal) 

drop *Tax* *Living* dapartment* homogeneous* apartment* drop_ejsa sold_type drop_bol drop_owner sold_area_m2 D2013 flag_multpurchase day bol* sold_date* vk* *T01 *T05 *T1 repeat* id serialowner LBuilding* homedeductiontotal ind_NoInfo edu_NoInfo ejerlejnr *_R  pnr2 bopikom_1 ejerlejnr_bol opgikom alink kom_1 household1 household2 household h_bopikom_2first h_bopikom_2last h_pnr1 h_pnr2 kom Basement Showers Historic Rooms_aptm Bathrooms_aptm unoccupied_aptm BuildingAge_aptm Historic_aptm pyear ejer_person1 startdate1 ejer_person2 ejer_startdate2 ind* h_kom_2first h_kom_2last *min ejdnr priceindex ejer_company miss_homededuc MissingEduc MissingEducmax

rename (sold_price_X Lsold_price_X) (sold_price_R Lsold_price_R)
rename living Living 
rename Repeat repeat
rename (ThreeOrMore) (ejer_ThreeOrMore)
rename  (bankdebttotal bondstotal cashtotal finasstotal finwealth_1total finwealth_2total incometotal liquiditytotal netwealthtotal mortgagetotal stockstotal) (ind_bankdebttotal ind_bondstotal ind_cashtotal ind_finasstotal ind_finwealth_1total ind_finwealth_2total ind_incometotal ind_liquiditytotal ind_netwealthtotal ind_mortgagetotal ind_stockstotal)

order repeat kom_2 bopikom_2 pdate sold_price sold_price_R Lsold_price Lsold_price_R Living Rooms Bathrooms BuildingAge year month sold_apartment unoccupied ejer_owners h_members h_female h_agemax h_age educlenmax educ1 educ2 educ3 educ4 educ5 ind_incometotal ind_netwealthtotal ind_liquiditytotal ind_finwealth_1total ind_finwealth_2total ind_finasstotal ind_mortgagetotal ind_bankdebttotal ind_cashtotal ind_bondstotal ind_stockstotal splithousehold properties wealth_dec DTI liquidity_dec 


sort year month ind_finwealth_2total


*identify the bubble
gen date=ym(year, month)
format date %tm

*FIRST BUBBLE
gen before_historical_minimum=0
replace before_historical_minimum=1 if date<tm(2012m1)

gen bubble=0
replace bubble=1 if date>=tm(2005m4) & date<=tm(2007m1) & !missing(date)

*-------------------------------------------------------------------------------
* creating areas
*-------------------------------------------------------------------------------

*drop bornholm
drop if komstr==400

*generating areas
gen area=.

******************
* AREA N 1
******************
replace area=1 if komstr==101
replace area=1 if komstr==147
replace area=1 if komstr==155
replace area=1 if komstr==185

******************
* AREA N 2
******************
replace area=2 if komstr==151
replace area=2 if komstr==153
replace area=2 if komstr==161
replace area=2 if komstr==163
replace area=2 if komstr==165
replace area=2 if komstr==167
replace area=2 if komstr==169
replace area=2 if komstr==175
replace area=2 if komstr==183
replace area=2 if komstr==187
replace area=2 if komstr==240
replace area=2 if komstr==250

******************
* AREA N 3
******************
replace area=3 if komstr==157
replace area=3 if komstr==159
replace area=3 if komstr==173
replace area=3 if komstr==190
replace area=3 if komstr==201
replace area=3 if komstr==223
replace area=3 if komstr==230


******************
* AREA N 4
******************
replace area=4 if komstr==210
replace area=4 if komstr==217
replace area=4 if komstr==219
replace area=4 if komstr==260
replace area=4 if komstr==270

******************
* AREA N 5
******************
replace area=5 if komstr==253
replace area=5 if komstr==259
replace area=5 if komstr==265
replace area=5 if komstr==269
replace area=5 if komstr==329
replace area=5 if komstr==350

******************
* AREA N 6
******************
replace area=6 if komstr==306
replace area=6 if komstr==316
replace area=6 if komstr==326
replace area=6 if komstr==330
replace area=6 if komstr==340


******************
* AREA N 7
******************
replace area=7 if komstr==320
replace area=7 if komstr==336
replace area=7 if komstr==360
replace area=7 if komstr==370
replace area=7 if komstr==376
replace area=7 if komstr==390

******************
* AREA N 8
******************
replace area=8 if komstr==410
replace area=8 if komstr==420
replace area=8 if komstr==430
replace area=8 if komstr==440
replace area=8 if komstr==450
replace area=8 if komstr==461
replace area=8 if komstr==479
replace area=8 if komstr==480
replace area=8 if komstr==482
replace area=8 if komstr==492


******************
* AREA N 9
******************
replace area=9 if komstr==510
replace area=9 if komstr==540
replace area=9 if komstr==550
replace area=9 if komstr==561
replace area=9 if komstr==563
replace area=9 if komstr==573
replace area=9 if komstr==580

******************
* AREA N 10
******************
replace area=10 if komstr==530
replace area=10 if komstr==575
replace area=10 if komstr==607
replace area=10 if komstr==621
replace area=10 if komstr==630

******************
* AREA N 11
******************
replace area=11 if komstr==615
replace area=11 if komstr==727
replace area=11 if komstr==740
replace area=11 if komstr==741
replace area=11 if komstr==746
replace area=11 if komstr==766


******************
* AREA N 12
******************
replace area=12 if komstr==657
replace area=12 if komstr==661
replace area=12 if komstr==665
replace area=12 if komstr==671
replace area=12 if komstr==756
replace area=12 if komstr==760

******************
* AREA N 13
******************
replace area=13 if komstr==706
replace area=13 if komstr==707
replace area=13 if komstr==710
replace area=13 if komstr==730
replace area=13 if komstr==779
replace area=13 if komstr==791

******************
* AREA N 14
******************
replace area=14 if komstr==751

******************
* AREA N 15
******************
replace area=15 if komstr==773
replace area=15 if komstr==787
replace area=15 if komstr==810
replace area=15 if komstr==813
replace area=15 if komstr==825
replace area=15 if komstr==849
replace area=15 if komstr==860


******************
* AREA N 16
******************
replace area=16 if komstr==820
replace area=16 if komstr==840
replace area=16 if komstr==846
replace area=16 if komstr==851

drop if area==.


save $savedata\filtered_variables, replace



* now we need to construct the area growth using the index price level over the 
* same period. We need to merge the data of the price level... in order to do 
* that we need a unique identiloss that will be our pdate (used in the other file)

* LET'S RECALL THE INDEX DATASET AND MODIFY IT TO DO THE MERGE THIS WILL BE MERGED
* AFTERWARDS WITH THE REAL GROWTH OF THE TRANSACTIONS TO IDENTIFY loss SALES

use $savedata\areaxmonth_final_index_A, clear

keep area pmonth price_index_rep
rename pmonth pmonth_1
rename area area_1
rename price_index_rep price_index

clonevar pmonth_2=pmonth_1

order pmonth_1 pmonth_2 area_1 price_index

save $savedata\area_index_price_levels, replace



********************************************************************************
* creating pairs
********************************************************************************

use $savedata\filtered_variables, clear

*TRACKING POTENTIAL WINNERS 
egen id = group (kom_2 bopikom_2)

* make sure is ordered
sort id pdate 

* number of sellings for house and order of selling 
by id: gen n_sales = _N
by id: gen sale_no = _n 

preserve
	*keep if sale_no<n_sales
	gen pair_id=sale_no
	gen ord=1
	rename * *_1
	rename (id_1 pair_id_1) (id pair_id)
	save $savedata\first_sales, replace
restore

preserve
	keep if sale_no>1 
	gen pair_id=sale_no-1
	gen ord=2
	rename * *_2 
	rename (id_2 pair_id_2) (id pair_id)
	save $savedata\second_sales, replace
restore

use $savedata\first_sales, clear

merge 1:1 id pair_id using $savedata\second_sales
keep if _merge==3 | _merge==1

drop if bubble_1 !=1

*creating variable for holding lenght from buying period to Trough
gen peak=tm(2007m8)
gen trough=tm(2012m1)

gen k_months=.

replace k_months=(year_2 - year_1)*12 + (month_2 - month_1)  if _merge==3 
replace k_months=(2012 - year_1)*12 + (1 - month_1)  if _merge==1 | pmonth_2 > trough
gen k_years=k_months/12

order pmonth* year* k_*

*creating variable for holding lenght from Peak to Trough
gen exit_crisis = .
replace exit_crisis = pmonth_2-peak if pmonth_2 < trough 
replace exit_crisis = trough-peak if pmonth_2 >= trough | pmonth_2 == .

gen duration_crisis=exit_crisis/12 

gen drop_model_4=1 if exit_crisis < 0 

*computing the growth only for obs within the event window
gen event_window=0
replace event_window=1 if _merge==3 & pmonth_2 <= trough

drop _merge

gen total_growth=sold_price_R_2/sold_price_R_1 if event_window==1
gen ann_growth=(total_growth^(1/k_years))-1 if event_window==1


* now we merge with the price_index from area index to create the area growth
merge m:1 area_1 pmonth_1 using $savedata\area_index_price_levels
drop if _merge==2
drop _merge

rename price_index price_index_1

merge m:1 area_1 pmonth_2 using $savedata\area_index_price_levels
drop if _merge==2
drop _merge

rename price_index price_index_2

gen total_growth_area=price_index_2/price_index_1 if event_window==1
gen ann_growth_area=(total_growth_area^(1/k_years))-1 if event_window==1


gen growth_below_avg=0
replace growth_below_avg=1 if ann_growth<ann_growth_area 

*-------------------------------------------------------------------------------
* if you want to look filter some individuals based on the performance of the 
* growth you can run the following instead of the previous
*replace growth_below_avg=1 if (ann_growth-ann_growth_area)<-0.1 
*-------------------------------------------------------------------------------

drop price_index_1 price_index_2 total_growth ann_growth total_growth_area ann_growth_area


* defining loss sales
gen loss_sale=0
replace loss_sale=1 if growth_below_avg==1 & (sold_price_2<sold_price_1) & before_historical_minimum_2==1

order *date* *exit* k* peak trough

save $savedata\finaldataset, replace 




********************************************************************************
* regressions
********************************************************************************
global savedata= "F:\data\workdata\709523\HousePricesRegional\ThesisEdoardo\TempData"

use $savedata\finaldataset, clear


*--------------------------------------
* summary tables
*--------------------------------------

*over the whole sample
preserve

	logout, save("F:\data\workdata\709523\HousePricesRegional\ThesisEdoardo\Do_files\Export\Statistics\Liq_Statistics_model_1_and_2") excel replace: tabstat loss_sale, by (liquidity_dec_1) statistics(mean count) 
	logout, save("F:\data\workdata\709523\HousePricesRegional\ThesisEdoardo\Do_files\Export\Statistics\Wealth_Statistics_model_1_and_2") excel replace: tabstat loss_sale, by (wealth_dec_1) statistics(mean count)
	logout, save("F:\data\workdata\709523\HousePricesRegional\ThesisEdoardo\Do_files\Export\Statistics\DTI_Statistics_model_1_and_2") excel replace: tabstat loss_sale, by (DTI_1) statistics(mean count)
	logout, save("F:\data\workdata\709523\HousePricesRegional\ThesisEdoardo\Do_files\Export\Statistics\LTV_Statistics_model_1_and_2") excel replace: tabstat loss_sale, by (LTV_cat_1) statistics(mean count)

restore

*over 2005
preserve
	keep if year_1==2005

	logout, save("F:\data\workdata\709523\HousePricesRegional\ThesisEdoardo\Do_files\Export\Statistics\Liq_Statistics_model_3_and_4") excel replace: tabstat loss_sale, by (liquidity_dec_1) statistics(mean count) 
	logout, save("F:\data\workdata\709523\HousePricesRegional\ThesisEdoardo\Do_files\Export\Statistics\Wealth_Statistics_model_3_and_4") excel replace: tabstat loss_sale, by (wealth_dec_1) statistics(mean count)
	logout, save("F:\data\workdata\709523\HousePricesRegional\ThesisEdoardo\Do_files\Export\Statistics\DTI_Statistics_model_3_and_4") excel replace: tabstat loss_sale, by (DTI_1) statistics(mean count)
	logout, save("F:\data\workdata\709523\HousePricesRegional\ThesisEdoardo\Do_files\Export\Statistics\LTV_Statistics_model_3_and_4") excel replace: tabstat loss_sale, by (LTV_cat_1) statistics(mean count)

restore

*over 2006
preserve
	keep if year_1==2006

	logout, save("F:\data\workdata\709523\HousePricesRegional\ThesisEdoardo\Do_files\Export\Statistics\Liq_Statistics_model_5_and_6") excel replace: tabstat loss_sale, by (liquidity_dec_1) statistics(mean count) 
	logout, save("F:\data\workdata\709523\HousePricesRegional\ThesisEdoardo\Do_files\Export\Statistics\Wealth_Statistics_model_5_and_6") excel replace: tabstat loss_sale, by (wealth_dec_1) statistics(mean count)
	logout, save("F:\data\workdata\709523\HousePricesRegional\ThesisEdoardo\Do_files\Export\Statistics\DTI_Statistics_model_5_and_6") excel replace: tabstat loss_sale, by (DTI_1) statistics(mean count)
	logout, save("F:\data\workdata\709523\HousePricesRegional\ThesisEdoardo\Do_files\Export\Statistics\LTV_Statistics_model_5_and_6") excel replace: tabstat loss_sale, by (LTV_cat_1) statistics(mean count)

restore

drop edu_collegemax_1 educlen_1 educlenmax_1


rename area_1 area
rename kom_2_1 kom_2 

* here we create the variables that Guren used in his paper to run the hedonic model
gen Living2=Living_1^2
gen Living3=Living_1^3
gen Rooms2=Rooms_1^2
gen Rooms3=Rooms_1^3
gen Bathrooms2=Bathrooms_1^2
gen Bathrooms3=Bathrooms_1^3
gen BuildingAge2=BuildingAge_1^2	
gen BuildingAge3=BuildingAge_1^3

* squaring other variables to test specifications 
gen h_agemax_1_sq = h_agemax_1^2
gen h_agemax_1_cb = h_agemax_1^3

* creating globals
global ind_vars ib10.wealth_dec_1 ib10.DTI_1 unoccupied_1 h_agemax_1 h_agemax_1_sq h_agemax_1_cb educ*_1  i.properties_1
global house_vars BuildingAge_1 BuildingAge2 BuildingAge3 Bathrooms_1 Bathrooms2 Bathrooms3 Rooms_1 Rooms2 Rooms3

program drop _all


program define output

args model

outreg2 using "F:\data\workdata\709523\HousePricesRegional\ThesisEdoardo\Do_files\Export\Regressions\Result_`model'.xls", excel replace eform stats(coef se pval) sideway

end


program define robustness_checks_phtest

args model

estat phtest, detail
logout, save("F:\data\workdata\709523\HousePricesRegional\ThesisEdoardo\Do_files\Export\Statistics\phtest_`model'.xlsx") excel replace: estat phtest, detail

end


program define robustness_checks_linktest

args model

linktest
logout, save("F:\data\workdata\709523\HousePricesRegional\ThesisEdoardo\Do_files\Export\Statistics\linktest_`model'.xlsx") excel replace: linktest

end



program define plots

args model

stphplot, by (wealth_dec_1)
graph export "F:\data\workdata\709523\HousePricesRegional\ThesisEdoardo\Do_files\Export\Graphs\z PH_wealth_`model'.pdf", replace

stphplot, by (DTI_1)
graph export "F:\data\workdata\709523\HousePricesRegional\ThesisEdoardo\Do_files\Export\Graphs\z DTI_wealth_`model'.pdf", replace

stphplot, by (liquidity_dec_1)
graph export "F:\data\workdata\709523\HousePricesRegional\ThesisEdoardo\Do_files\Export\Graphs\z LIQ_wealth_`model'.pdf", replace

stphplot, by (LTV_cat_1)
graph export "F:\data\workdata\709523\HousePricesRegional\ThesisEdoardo\Do_files\Export\Graphs\z LTV_wealth_`model'.pdf", replace


end



* starting by checking multicollinearity between variables
regress loss_sale ib10.liquidity_dec_1 i.LTV_cat_1 $ind_vars $house_vars i.year_1, vce(cluster kom_2)
logout, save("F:\data\workdata\709523\HousePricesRegional\ThesisEdoardo\Do_files\Export\Statistics\VIF") excel replace: vif 

count if year_1==2007

local model `model'


* YOU WILL SEE MANY OUTPUTS AND PLOTS COMMENTED... I COMMENTED THEM BECAUSE 
* THEY DON'T SATISFY THE PROPORTIONAL HARZARD ASSUMPTION

*-------------------------------------------------------------------------------
*analysis from when the house was bought until the tough
*-------------------------------------------------------------------------------

preserve

	stset k_years, failure(loss_sale==1)

	* regression 1
	stcox ib10.liquidity_dec_1 ib1.LTV_cat_1 $ind_vars $house_vars i.year_1, strata(kom_2) vce(cluster kom_2)
	output "model_1_regression_1"
	robustness_checks_phtest "model_1_regression_1"
	quietly stcox ib10.liquidity_dec_1 ib1.LTV_cat_1 $ind_vars $house_vars i.year_1, strata(kom_2) vce(cluster kom_2)
	robustness_checks_linktest "model_1_regression_1"
	

	* regression 2
    stcox ib10.liquidity_dec_1##ib1.LTV_cat_1 $ind_vars $house_vars i.year_1, strata(kom_2) vce(cluster kom_2)
	output "model_1_regression_2"
	robustness_checks_phtest "model_1_regression_2"
	quietly stcox ib10.liquidity_dec_1##ib1.LTV_cat_1 $ind_vars $house_vars i.year_1, strata(kom_2) vce(cluster kom_2)
	robustness_checks_linktest "model_1_regression_2"
	
restore

*-------------------------------------------------------------------------------

preserve 

	drop if drop_model_4==1
	stset duration_crisis, failure(loss_sale==1)

	* regression 1
	stcox ib10.liquidity_dec_1 ib1.LTV_cat_1 $ind_vars $house_vars i.year_1, strata(kom_2) vce(cluster kom_2)
	output "model_2_regression_1"
	robustness_checks_phtest "model_2_regression_1"
	quietly stcox ib10.liquidity_dec_1##ib1.LTV_cat_1 $ind_vars $house_vars i.year_1, strata(kom_2) vce(cluster kom_2)
	robustness_checks_linktest "model_2_regression_1"
	
	

	* regression 2
    stcox ib10.liquidity_dec_1##ib1.LTV_cat_1 $ind_vars $house_vars i.year_1, strata(kom_2) vce(cluster kom_2)
	output "model_2_regression_2"
	robustness_checks_phtest "model_2_regression_2"
	quietly stcox ib10.liquidity_dec_1##ib1.LTV_cat_1 $ind_vars $house_vars i.year_1, strata(kom_2) vce(cluster kom_2)
	robustness_checks_linktest "model_2_regression_2"
	
restore

*-------------------------------------------------------------------------------
*analysis from when the house was bought until the tough filtered by year of purchase
*-------------------------------------------------------------------------------

preserve 
	
	keep if year_1==2005
	stset k_years, failure(loss_sale==1)


	* regression 1
	stcox ib10.liquidity_dec_1 ib1.LTV_cat_1 $ind_vars $house_vars i.year_1, strata(kom_2) vce(cluster kom_2)
	output "model_3_regression_1" 
	robustness_checks_phtest "model_3_regression_1"
	quietly stcox ib10.liquidity_dec_1 ib1.LTV_cat_1 $ind_vars $house_vars i.year_1, strata(kom_2) vce(cluster kom_2)
	robustness_checks_linktest "model_3_regression_1"

	* regression 2
	stcox ib10.liquidity_dec_1##ib1.LTV_cat_1 $ind_vars $house_vars i.year_1, strata(kom_2) vce(cluster kom_2)
	output "model_3_regression_2"
	robustness_checks_phtest "model_3_regression_2"
	quietly stcox ib10.liquidity_dec_1##ib1.LTV_cat_1 $ind_vars $house_vars i.year_1, strata(kom_2) vce(cluster kom_2)
	robustness_checks_linktest "model_3_regression_2"
restore


preserve

	keep if year_1==2005
	drop if drop_model_4==1
	stset duration_crisis, failure(loss_sale==1)

	
	sts graph, failure by(LTV_cat_1) ylabel (0(0.02)0.08) title("loss sales x LTV")
	graph export "F:\data\workdata\709523\HousePricesRegional\ThesisEdoardo\Do_files\Export\Graphs\Failure loss sales x LTV model 4.pdf", replace
	sts graph if liquidity_dec_1==1 | liquidity_dec_1==5 | liquidity_dec_1==10, failure by(liquidity_dec_1) ylabel (0(0.02)0.08) title("loss sales x liquidity")
	graph export "F:\data\workdata\709523\HousePricesRegional\ThesisEdoardo\Do_files\Export\Graphs\Failure loss sales x Liquidity model 4.pdf", replace
	
	sts graph, hazard by(LTV_cat_1) ylabel (0(0.02)0.08) title("loss sales x LTV")
	graph export "F:\data\workdata\709523\HousePricesRegional\ThesisEdoardo\Do_files\Export\Graphs\Hazard loss sales x LTV model 4.pdf", replace
	sts graph if liquidity_dec_1==1 | liquidity_dec_1==5 | liquidity_dec_1==10, hazard by(liquidity_dec_1) ylabel (0(0.02)0.08) title("loss sales x liquidity")
	graph export "F:\data\workdata\709523\HousePricesRegional\ThesisEdoardo\Do_files\Export\Graphs\Hazard loss sales x Liquidity model 4.pdf", replace

	
	* regression 1
	stcox ib10.liquidity_dec_1 ib1.LTV_cat_1 $ind_vars $house_vars i.year_1, strata(kom_2) vce(cluster kom_2)
	output "model_4_regression_1"
	plots "model_4_regression_1"
	robustness_checks_phtest "model_4_regression_1"
	quietly stcox ib10.liquidity_dec_1##ib1.LTV_cat_1 $ind_vars $house_vars i.year_1, strata(kom_2) vce(cluster kom_2)
	robustness_checks_linktest "model_4_regression_1"

	* regression 2
	 stcox ib10.liquidity_dec_1##ib1.LTV_cat_1 $ind_vars $house_vars i.year_1, strata(kom_2) vce(cluster kom_2)
	 output "model_4_regression_2_sig"
	 plots "model_4_regression_2_sig"
	 robustness_checks_phtest "model_4_regression_2_sig"
	 quietly stcox ib10.liquidity_dec_1##ib1.LTV_cat_1 $ind_vars $house_vars i.year_1, strata(kom_2) vce(cluster kom_2)
	 robustness_checks_linktest "model_4_regression_2_sig"

restore 
	 
	
*-------------------------------------------------------------------------------
	
preserve 

	keep if year_1==2006
	stset k_years, failure(loss_sale==1)


	sts graph, failure by(LTV_cat_1) ylabel (0(0.02)0.08) title("loss sales x LTV")
	graph export "F:\data\workdata\709523\HousePricesRegional\ThesisEdoardo\Do_files\Export\Graphs\Failure loss sales x LTV model 5.pdf", replace
	sts graph if liquidity_dec_1==1 | liquidity_dec_1==5 | liquidity_dec_1==10, failure by(liquidity_dec_1) ylabel (0(0.02)0.08) title("loss sales x liquidity")
	graph export "F:\data\workdata\709523\HousePricesRegional\ThesisEdoardo\Do_files\Export\Graphs\Failure loss sales x Liquidity model 5.pdf", replace
	
	sts graph, hazard by(LTV_cat_1) ylabel (0(0.02)0.08) title("loss sales x LTV")
	graph export "F:\data\workdata\709523\HousePricesRegional\ThesisEdoardo\Do_files\Export\Graphs\Hazard loss sales x LTV model 5.pdf", replace
	sts graph if liquidity_dec_1==1 | liquidity_dec_1==5 | liquidity_dec_1==10, hazard by(liquidity_dec_1) ylabel (0(0.02)0.08) title("loss sales x liquidity")
	graph export "F:\data\workdata\709523\HousePricesRegional\ThesisEdoardo\Do_files\Export\Graphs\Hazard loss sales x Liquidity model 5.pdf", replace

	
	* regression 1
	stcox ib10.liquidity_dec_1 ib1.LTV_cat_1 $ind_vars $house_vars i.year_1, strata(kom_2) vce(cluster kom_2)
	output "model_5_regression_1_sig"
	plots "model_5_regression_1_sig"
	robustness_checks_phtest  "model_5_regression_1_sig"
	quietly stcox ib10.liquidity_dec_1 ib1.LTV_cat_1 $ind_vars $house_vars i.year_1, strata(kom_2) vce(cluster kom_2)
	robustness_checks_linktest "model_5_regression_1_sig"

	* regression 2
	stcox ib10.liquidity_dec_1##ib1.LTV_cat_1 $ind_vars $house_vars i.year_1, strata(kom_2) vce(cluster kom_2)
	output "model_5_regression_2_sig"
	plots "model_5_regression_2_sig
	robustness_checks_phtest "model_5_regression_2_sig"
	quietly stcox ib10.liquidity_dec_1##ib1.LTV_cat_1 $ind_vars $house_vars i.year_1, strata(kom_2) vce(cluster kom_2)
	robustness_checks_linktest "model_5_regression_2_sig"
	
restore



preserve

	keep if year_1==2006
	drop if drop_model_4==1
	stset duration_crisis, failure(loss_sale==1)
	

	sts graph, failure by(LTV_cat_1) ylabel (0(0.02)0.08) title("loss sales x LTV")
	graph export "F:\data\workdata\709523\HousePricesRegional\ThesisEdoardo\Do_files\Export\Graphs\Failure loss sales x LTV model 6.pdf", replace
	sts graph if liquidity_dec_1==1 | liquidity_dec_1==5 | liquidity_dec_1==10, failure by(liquidity_dec_1) ylabel (0(0.02)0.08) title("loss sales x liquidity")
	graph export "F:\data\workdata\709523\HousePricesRegional\ThesisEdoardo\Do_files\Export\Graphs\Failure loss sales x Liquidity model 6.pdf", replace
	
	sts graph, hazard by(LTV_cat_1) ylabel (0(0.02)0.08) title("loss sales x LTV")
	graph export "F:\data\workdata\709523\HousePricesRegional\ThesisEdoardo\Do_files\Export\Graphs\Hazard loss sales x LTV model 6.pdf", replace
	sts graph if liquidity_dec_1==1 | liquidity_dec_1==5 | liquidity_dec_1==10, hazard by(liquidity_dec_1) ylabel (0(0.02)0.08) title("loss sales x liquidity")
	graph export "F:\data\workdata\709523\HousePricesRegional\ThesisEdoardo\Do_files\Export\Graphs\Hazard loss sales x Liquidity model 6.pdf", replace

	
	* regression 1
	stcox ib10.liquidity_dec_1 ib1.LTV_cat_1 $ind_vars $house_vars i.year_1, strata(kom_2) vce(cluster kom_2)
	output "model_6_regression_1_sig"
	plots "model_6_regression_1_sig"
	robustness_checks_phtest "model_6_regression_1_sig"
	quietly stcox ib10.liquidity_dec_1 ib1.LTV_cat_1 $ind_vars $house_vars i.year_1, strata(kom_2) vce(cluster kom_2)
	robustness_checks_linktest "model_6_regression_1_sig"

	* regression 2
	stcox ib10.liquidity_dec_1##ib1.LTV_cat_1 $ind_vars $house_vars  i.year_1, strata(kom_2) vce(cluster kom_2)
	output "model_6_regression_2_sig"
	plots "model_6_regression_2_sig"
	robustness_checks_phtest "model_6_regression_2_sig"
	quietly stcox ib10.liquidity_dec_1##ib1.LTV_cat_1 $ind_vars $house_vars  i.year_1, strata(kom_2) vce(cluster kom_2)
	robustness_checks_linktest "model_6_regression_2_sig"

restore
