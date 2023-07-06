/*********************************

Last updated: June 20, 2023

Statistical Analyses

Kenneth Shores

Stata code for statistical analyses found in "Unreviewed science in the news: The evolution of preprint media coverage from 2014-2021"


Related Publication:
Fleerackers, A., Shores, K., Chtena, N. & Alperin, J.P. (2023). Unreviewed science in the news: The evolution of preprint media verage from 2014-2021. *bioarxiv*. 

Related Dataset:
Alperin, J. P., Fleerackers, A., & Shores, K. (2023). Data for: Unreviewed science in the news [Data set]. Harvard Dataverse. https://doi.org/10.7910/DVN/ZHQUFD

***********************************/


import delimited using "research_mentions_all_final.csv", varn(1) clear
save "preprints.dta", replace


u preprints, clear

/**************************
FIGURE 1 CODE: COVID EFFECTS OF PREPRINT SHARE
**************************/

sum posted_on_day_number if research_venue=="medrxiv"
gen firstmedarxiv = posted_on_day_number>=2029

sum posted_on_day_number if is_covid==1
gen firstcovid = posted_on_day_number>=2200

gen dayscov = posted_on_day_number - 2200
replace dayscov = 0 if dayscov<0

gen is_covid_real = is_covid==1 & posted_on_day_number>=2200
gen time = posted_on_day_number
gen time2 = posted_on_day_number*posted_on_day_number
gen time3 = posted_on_day_number*posted_on_day_number*posted_on_day_number

** seasonality in the data 
preserve
	bys posted_on_day_number : egen mnpreprint2 = mean(is_preprint)
	*replace mnpreprint = .18 if mnpreprint>.18 & mnpreprint!=.
	keep posted_on_day_number mnpreprint2
	duplicates drop
	sort posted_on_day_n
	graph tw (line mnpreprint2 posted_on_day_number if posted_on_day_n<2200, lw(vvthin) lc(%75)) (lowess mnpreprint2 posted_on_day_number if posted_on_day_n<2200, bw(.0325) lw(thin) lc(%75)), leg(off) name(cyclical, replace)
restore

preserve
	gen month = substr(posted_on,6,2)
	gen day   = substr(posted_on,9,2)
	gen year  = substr(posted_on,1,4)
	keep posted_on_day_n month day year
	duplicates drop
	destring month, replace
	destring day, replace
	destring year, replace
	egen weekyears = seq(), from(1) to(52) block(7)
	bys month: egen maxweek = max(weekyears)
	replace weekyears = maxweek+1 if weekyears==1 & maxweek==52 
	
	gen wednesday = 1 if posted_on_day_n==0
	gen thursday = 1 if posted_on_day_n==1
	gen friday = 1 if posted_on_day_n==2
	gen saturday = 1 if posted_on_day_n==3
	gen sunday = 1 if posted_on_day_n==4
	gen monday = 1 if posted_on_day_n==5
	gen tuesday = 1 if posted_on_day_n==6
	sum posted_on_day_n
	forval i = 7(7)`r(max)' {
		replace wednesday = 1 if posted_on_day_n==`i'
	}
	forval i = 8(7)`r(max)' {
		replace thursday = 1 if posted_on_day_n==`i'
	}
	forval i = 9(7)`r(max)' {
		replace friday = 1 if posted_on_day_n==`i'
	}
	forval i = 10(7)`r(max)' {
		replace saturday= 1 if posted_on_day_n==`i'
	}	
	forval i = 11(7)`r(max)' {
		replace sunday= 1 if posted_on_day_n==`i'
	}	
	forval i = 12(7)`r(max)' {
		replace monday= 1 if posted_on_day_n==`i'
	}	
	forval i = 13(7)`r(max)' {
		replace tuesday= 1 if posted_on_day_n==`i'
	}	
	foreach v in monday tues wed thur fri sat sun {
		replace `v' = 0 if `v'==.
	}
	egen weeks = seq(), block(7)
	bys weeks: gen days = _n
	tempfile hold
	save `hold'
restore
mer m:1 posted_on_day_n using `hold', nogen
egen clustervar = group(month days)

eststo m1: reghdfe is_preprint i.firstmedarxiv i.is_covid_real c.time##c.time##c.time  i.firstcov c.dayscov##c.dayscov##c.dayscov , abs(weekyears month##days, savefe) residuals		
predict preprinthat

est restore m1
margins, dydx(time) at(dayscov=0 time=365 is_covid_real=0 firstmedarxiv=0) post asobserved				// marginal change in preprint share precovid era (days) 
lincom _b[time]*365 

est restore m1
margins, dydx(firstmedarxiv) post atmeans																// marginal change when medarxiv introduced at means

est restore m1
margins, dydx(firstcov) at(dayscov=0 time=2200 is_covid_real=0 firstmedarxiv=1) post asobserved 		// marginal change when covid era comes online but excluding covid language

est restore m1
margins, dydx(is_covid_real) at(dayscov=0 time=2200 is_covid_real=1 firstmedarxiv=1) post asobserved 	// marginal change when covid era comes online including covid language

est restore m1
margins, dydx(dayscov) at(dayscov=250 time=2450 firstcov=1 is_covid_real=0 firstmedarxiv=1) post atmeans		// marginal change in preprint share covid era excluding covid language
lincom _b[dayscov]*365 

est restore m1
margins, at(dayscov=0 time=2710 is_covid_real=0 firstmedarxiv=1 firstcov=1) ///
		 at(dayscov=510 time=2710 is_covid_real=1 firstmedarxiv=1 firstcov=1)  post asobserved					// prediction of yhat end of sample without Covid effect compared to with covid effect
lincom _b[2._at]-_b[1._at]
		 
est restore m1
margins, at(dayscov=0 time=2710 is_covid_real=0 firstmedarxiv=1 firstcov=1) ///
		 at(dayscov=510 time=2710 is_covid_real=0 firstmedarxiv=1 firstcov=1)  post asobserved				// prediction of yhat end of sample without Covid effect compared to with covid effect
lincom _b[2._at]-_b[1._at]

est restore m1
margins, at(dayscov=510 time=2710 is_covid_real=0 firstmedarxiv=1 firstcov=1) ///
		 at(dayscov=0   time=2710 is_covid_real=0 firstmedarxiv=1 firstcov=1)  post asobserved				// prediction of yhat end of sample minus covid language and based on predicted based on pretrends
lincom _b[2._at]-_b[1._at]

est restore m1		 
margins, at(dayscov=0 time=2200 is_covid_real=0 firstmedarxiv=1 firstcov=0) 								// pre-Covid base rates
		 
** residualize preprint mentions, net of week and day*month effects
reghdfe is_preprint , abs(weekyears month##days) residuals
predict is_preprint_resid, r

** residualize days since (not really necessary), net of week and day*month effects
reghdfe posted_on_day_number , abs(weekyears month##days) residuals
predict posted_on_day_number_resid,r

** regress residualized preprint mentions against days since --> seasonality adjusted mean preprint mentions per day
reghdfe is_preprint_resid , abs(posted_on_day_number, savefe) residuals
predict mnpreprint, d
sum is_preprint
replace mnpreprint = mnpreprint+`r(mean)'		// add back in constant

** estimate 3rd order poly model from above , allowing for intercept shift in medarxiv for pre-covid era and then out of sample prediction of covid era --> gives counterfactual 
reghdfe is_preprint c.time##c.time##c.time i.firstmedarxiv if posted_on_day_number<2200,  abs(weekyears month##days, savefe) residuals		// same as collapsed; seems can drop the medarxiv coeffient, not at all sig
predict preprinthat_cf

** estimate 3rd order poly model from above , allowing for intercept shift in medarxiv for pre-covid era and covid era, not separating covid lang from noncovd lang
eststo m2: reghdfe is_preprint i.firstmedarxiv c.time##c.time##c.time  i.firstcov /*c.dayscov##c.dayscov##c.dayscov */ , abs(weekyears month##days, savefe) residuals		
predict preprinthat_all
est restore m2
margins,  at(firstcov=1 time=2710 firstmedarxiv=1) post asobserved 		// marginal change when covid era comes online but excluding covid language


** figure 1
set scheme white_tableau
preserve
	bys posted_on_day_number : egen mnpreprint2 = mean(is_preprint)
	replace mnpreprint = .18 if mnpreprint>.18 & mnpreprint!=.
	keep preprinthat preprinthat_cf mnpreprint posted_on_day_number is_covid_real mnpreprint2 preprinthat_all firstcov
	sort posted_on_day_n
	duplicates drop 
	# del ;
	graph tw 
		(lowess mnpreprint posted_on_day_number if posted_on_day_n<2200, bw(.01) lw(vthin) lc(blue%90))
		(line preprinthat_cf posted_on_day_number if posted_on_day_number >=2200, lp(dash) lw(medthick) lc("77 77 77"%75)) 		// 197 27 125
		(line preprinthat posted_on_day_number if is_covid_real==0 , lw(medthick) lc("77 77 77"%75)) 		// 197 27 125
		(line preprinthat posted_on_day_number if is_covid_real==1 , lw(medthick) lc("197 27 125")) 			// 148 103 189
		(line mnpreprint posted_on_day_number, lw(vvthin) lc(%75))
		(lowess mnpreprint posted_on_day_number if posted_on_day_n>=2200, bw(.0426) lw(vthin) lc(blue%90))			// bw is scaled by sample, so bw here is 4.2x
		(line preprinthat_all posted_on_day_number if firstcov==1,  lw(medthick) lc(blue%90))			// bw is scaled by sample, so bw here is 4.2x
		, 	
		text(.16 2210 `"WHO uses "2019-NCoV""', just(left) size(small) place(e))
		text(.1525 2210 "1.10.20", just(left) size(small) place(e))
		text(.12 2020 "medRxiv comes online", just(right) size(small) place(w))
		text(.1125 2020 "7.23.19", just(right) size(small) place(w))
		name(figure1, replace) 
		ylab(0 "0" .03 "3" .06 "6" .09 "9" .12 "12" .15 "15" .18 "18", labsize(vsmall)) 
		xlab(90 "1.4.14"  304 "1.11.14" 485 "1.5.15" 669 "1.11.15" 851 "1.5.16" 1035 "1.11.16" 1247 "1.6.17" 1430 "1.12.17" 1642 "1.7.18" 1857 "1.2.19" 
			 2069 "1.9.19" 2251 "1.3.20" 2465 "1.10.20" 2647 "1.4.21", labsize(vsmall))
		xline(2029 2200, lc(maroon) lw(medium) lp(dash))  xtitle("") leg(order(3 "Predicted Preprints excluding Covid" 4 "Predicted Preprints including Covid" 2 "Predicted Preprints based on pre-Covid Trends" 1 "Mean Preprints per Day (lowess fitted)" ) symxsize(*1.5) pos(11) rows(2)) 
		plotregion(m(r+2.5))
		ytitle("Share of Media Preprint Mentions (in percentage points 0-100)", size(small))
	; 
	# del cr
	graph export figure1.png, as(png) replace
	graph export figure1.tif, as(tif) replace
*	scatter mnpreprint mnpreprint2, ms(p)
restore

/**************************
FIGURE 2 CODE: GROWTH RATES IN PREPRINTS AND WOS MENTIONS OVER TIME
**************************/
u preprints, clear
egen article_id_num = group(article_id)
drop article_id
rename article_id_num article_id
preserve
	gen sunday = 1 if posted_on_day_n==4
	gen month = substr(posted_on,6,2)
	gen day   = substr(posted_on,9,2)
	gen year  = substr(posted_on,1,4)
	keep posted_on_day_n month day year
	duplicates drop
	drop if posted_on_day_n<4					// drop part week	
	sort posted_on_day_number
	egen group = seq(), block(28)
	drop if inlist(group,1,2)					// drop first BLOCK days
	*egen group = seq(), block(30)
	tempfile hold
	save `hold'
restore

mer m:1 posted_on_day_n using `hold', keep(3)
levelsof group if posted_on_day_n==2200, loc(xline)
global xline = `xline'
bys is_preprint group: egen nvals = nvals(article_id)

collapse (count) mention_id (firstnm) nvals (firstnm) posted_on (lastnm) lastdate=posted_on, by(group is_preprint)
gen date = substr(posted_on,1,10)
preserve
	keep if is_preprint==0
	labmask group, val(date)
	tempfile wos
	save `wos'
restore
preserve
	keep if is_preprint==1
	labmask group, val(date)
	tempfile pre
	save `pre'
restore
preserve
	u `wos', clear
	append using `pre'
	clonevar timevar= group 
	tempfile hold
	save `hold'
restore
mer 1:1 group is_preprint using `hold'


xtset is_preprint timevar
gen dmentions = d.mention_id
bys is_preprint (timevar) : gen mentions_bl = 1*mention_id/mention_id[1] 		// baseline growth rate for expenditure; base 100 = 1960
bys is_preprint (timevar) : gen articles_bl = 1*nvals/nvals[1] 					// baseline growth rate for expenditure; base 100 = 1960
sum timevar
*drop if timevar==`r(max)' | timevar==`r(max)'-1

sum timevar
global roundedtimevar = round(`r(max)',5)

reg mentions_bl timevar if is_preprint==0 & timevar<${xline}
predict yhat_0
reg mentions_bl timevar if is_preprint==1 & timevar<${xline}
predict yhat_1

reg mentions_bl i.is_preprint##c.timevar if timevar<${xline}, vce(hc2)

# del 
graph tw 
	(line mentions_bl timevar if is_preprint==0, lc("77 77 77") lw(medthick)) 
	(line mentions_bl timevar if is_preprint==1, lc("197 27 125") lw(medthick))
	(line yhat_0 timevar if is_preprint==0, lc("77 77 77%50") lw(vthin))
	(line yhat_1 timevar if is_preprint==1, lc("197 27 125%50") lw(vthin))	
	,
	text(10.2 ${xline} `"WHO uses "2019-NCoV""', just(left) size(small) place(w))
	text(9 ${xline} "1.10.20", just(left) size(small) place(w))
	plotregion(m(r+1.5))  xline(${xline}, lp(dash) lc(maroon))
	xlab(5(10)${roundedtimevar}, val labsize(vsmall))
	ylab(1 "Base" 2 "2x" 4 "4x" 6 "6x" 8 "8x" 10 "10x" 12 "12x" 14 "14x" 16 "16x" 18 "18x" 20 "20x" 22 "22x" 24 "24x" 26 "26x" 28 "28x" 30 "30x")
	leg(order(2 "Preprint mentions" 1 "WoS mentions"  )rows(1) pos(11) symxsize(*2)) xtitle("Dates") ytitle("Total Media Mentions per 28-Days Relative to Initial 28 Day Total")
;
# del cr
graph export figure2.png, as(png) replace
graph export figure2.tif, as(tif) replace

/**************************
FIGURE 3 CODE: REGRESSION EFFECTS , SERVERS 
**************************/
u preprints, clear
egen article_id_num = group(article_id)
drop article_id
rename article_id_num article_id
gen is_covid_real = is_covid==1 & posted_on_day_number>=2200

preserve
	gen month = substr(posted_on,6,2)
	gen day   = substr(posted_on,9,2)
	gen year  = substr(posted_on,1,4)
	drop if research_venue=="wos"
	keep posted_on_day_n month day year
	duplicates drop
	destring month, replace
	destring day, replace
	destring year, replace
	egen weekyears = seq(), from(1) to(52) block(7)
	bys month: egen maxweek = max(weekyears)
	replace weekyears = maxweek+1 if weekyears==1 & maxweek==52 
	
	gen wednesday = 1 if posted_on_day_n==0
	gen thursday = 1 if posted_on_day_n==1
	gen friday = 1 if posted_on_day_n==2
	gen saturday = 1 if posted_on_day_n==3
	gen sunday = 1 if posted_on_day_n==4
	gen monday = 1 if posted_on_day_n==5
	gen tuesday = 1 if posted_on_day_n==6
	sum posted_on_day_n
	forval i = 7(7)`r(max)' {
		replace wednesday = 1 if posted_on_day_n==`i'
	}
	forval i = 8(7)`r(max)' {
		replace thursday = 1 if posted_on_day_n==`i'
	}
	forval i = 9(7)`r(max)' {
		replace friday = 1 if posted_on_day_n==`i'
	}
	forval i = 10(7)`r(max)' {
		replace saturday= 1 if posted_on_day_n==`i'
	}	
	forval i = 11(7)`r(max)' {
		replace sunday= 1 if posted_on_day_n==`i'
	}	
	forval i = 12(7)`r(max)' {
		replace monday= 1 if posted_on_day_n==`i'
	}	
	forval i = 13(7)`r(max)' {
		replace tuesday= 1 if posted_on_day_n==`i'
	}	
	foreach v in monday tues wed thur fri sat sun {
		replace `v' = 0 if `v'==.
	}
	egen weeks = seq(), block(7)
	bys weeks: gen days = _n
	tempfile hold
	save `hold'
restore

gen postcovid = posted_on_day_n>=2200 
drop if research_venue=="wos"

gcollapse (nansum) is_preprint, by(posted_on_day_n research_venue  is_covid_real)
mer m:1 posted_on_day_n using `hold' //, keep(3)

gen firstmedarxiv = posted_on_day_number>=2029

gen firstcovid = posted_on_day_number>=2200

gen dayscov = posted_on_day_number - 2200
replace dayscov = 0 if dayscov<0

gen time = posted_on_day_number
gen time2 = posted_on_day_number*posted_on_day_number
gen time3 = posted_on_day_number*posted_on_day_number*posted_on_day_number

encode research_venue, gen(temp)
rename research_venue research 
rename temp research_venue

eststo m1a: reghdfe is_preprint i.firstmedarxiv i.research_venue##(i.firstcov is_covid_real), abs(weekyears  month##days month##year , savefe) resid
save server.dta, replace


tempname memhold
tempfile results
postfile `memhold' str100 server str100 covidlang str100 time beta se str100 star using `results'

forval i = 1/4 {
	est restore m1a
	margins, dydx(firstcov) at(research_venue=`i' is_covid_real=0 firstmedarxiv=1) post asobserved				// marginal change in preprint share precovid era (days) 
	mat define a = r(table)
	loc z = a[3,2]
	if abs(`z') < 1.65  loc nlstar " "
	if abs(`z') >= 1.65 loc nlstar "*"
	if abs(`z') >= 1.96 loc nlstar "**"
	if abs(`z') >= 2.58 loc nlstar "***"

	est restore m1a
	margins, dydx(is_covid_real) at(research_venue=`i' firstcov=1 firstmedarxiv=1) post asobserved				// marginal change in preprint share precovid era (days) 
	mat define a = r(table)
	loc z = a[3,2]
	if abs(`z') < 1.65  loc ylstar " "
	if abs(`z') >= 1.65 loc ylstar "*"
	if abs(`z') >= 1.96 loc ylstar "**"
	if abs(`z') >= 2.58 loc ylstar "***"

	est restore m1a
	margins, at(research_venue=`i' firstcov=0 is_covid_real=0 firstmedarxiv=1) post asobserved				// marginal change in preprint share precovid era (days) 
	mat define beta = e(b)
	loc beta = beta[1,1]
	mat define se = e(V)
	loc se = sqrt(se[1,1])
	levelsof research if research_venue==`i', loc(name) clean
	post `memhold' ("`name'") ("no lang") ("pre") (`beta') (`se') ("") 
	
	est restore m1a
	margins, at(research_venue=`i' firstcov=0 is_covid_real=0 firstmedarxiv=1) post asobserved				// marginal change in preprint share precovid era (days) 
	mat define beta = e(b)
	loc beta = beta[1,1]
	mat define se = e(V)
	loc se = sqrt(se[1,1])
	levelsof research if research_venue==`i', loc(name) clean
	post `memhold' ("`name'") ("yes lang") ("pre") (`beta') (`se')	("") 
	
	est restore m1a
	margins, at(research_venue=`i' firstcov=1 is_covid_real=0 firstmedarxiv=1) post asobserved				// marginal change in preprint share precovid era (days) 
	mat define beta = e(b)
	loc beta = beta[1,1]
	mat define se = e(V)
	loc se = sqrt(se[1,1])
	levelsof research if research_venue==`i', loc(name) clean
	post `memhold' ("`name'") ("no lang") ("post") (`beta') (`se')	("`nlstar'")
	
	est restore m1a
	margins, at(research_venue=`i' firstcov=1 is_covid_real=1 firstmedarxiv=1) post asobserved				// marginal change in preprint share precovid era (days) 
	mat define beta = e(b)
	loc beta = beta[1,1]
	mat define se = e(V)
	loc se = sqrt(se[1,1])
	levelsof research if research_venue==`i', loc(name) clean
	post `memhold' ("`name'") ("yes lang") ("post") (`beta') (`se') ("`ylstar'")
	
	

}
postclose `memhold'
u `results', clear
compress
gen betahi = beta+1.96*se
gen betalo = beta-1.96*se

gen xaxis = 1 if time=="pre"
replace xaxis =2 if time=="post"

save servers_results.dta, replace


u servers_results, clear

foreach v in arxiv biorxiv medrxiv ssrn {
	levelsof beta if server=="`v'" & covidlang=="no lang" & time=="post", loc(`v'y) clean
	levelsof star if server=="`v'" & covidlang=="no lang" & time=="post", loc(`v's) clean
}

# del ;
graph tw 
	(connected beta xaxis if server=="arxiv" & covidlang=="no lang")
	(connected beta xaxis if server=="biorxiv" & covidlang=="no lang")
	(connected beta xaxis if server=="medrxiv" & covidlang=="no lang")
	(connected beta xaxis if server=="ssrn" & covidlang=="no lang"),
	text(`arxivy' 2.01 "`arxivs'", just(left) size(small) place(e))
	text(`biorxivy' 2.01 "`biorxivs'", just(left) size(small) place(e))
	text(`medrxivy' 2.01 "`medrxivs'", just(left) size(small) place(e))
	text(`ssrny' 2.01 "`ssrns'", just(left) size(small) place(e))
	xlab(1 "Pre-Covid" 2 "Covid Era", labsize(medium)) xsca(range(.95 2.05))
	ylab(0(5)20, labsize(medium)) ytitle("Counts of Media Mentions")
	leg(order( 3 "medRxiv" 2 "bioRxiv"  4 "SSRN" 1 "arXiv") pos(11) rows(1) span) xtitle("") 
	name(nolang, replace)
	xsize(4.25) ysize(4.25)
	graphregion(m(l=0 b=0 t=0 r=5))	
;
# del cr
graph export figure3a.png, as(png) replace
graph export figure3a.tif, as(tif) replace

foreach v in arxiv biorxiv medrxiv ssrn {
	levelsof beta if server=="`v'" & covidlang=="yes lang" & time=="post", loc(`v'y) clean
	levelsof star if server=="`v'" & covidlang=="yes lang" & time=="post", loc(`v's) clean
}

# del ;
graph tw 
	(connected beta xaxis if server=="arxiv" & covidlang=="yes lang")
	(connected beta xaxis if server=="biorxiv" & covidlang=="yes lang")
	(connected beta xaxis if server=="medrxiv" & covidlang=="yes lang")
	(connected beta xaxis if server=="ssrn" & covidlang=="yes lang")
	,
	text(`arxivy' 2.01 "`arxivs'", just(left) size(small) place(e))
	text(`biorxivy' 2.01 "`biorxivs'", just(left) size(small) place(e))
	text(`medrxivy' 2.01 "`medrxivs'", just(left) size(small) place(e))
	text(`ssrny' 2.01 "`ssrns'", just(left) size(small) place(e))	
	xlab(1 "Pre-Covid" 2 "Covid Era", labsize(medium)) xsca(range(.95 2.05))
	ylab(0(5)20, labsize(medium)) ytitle("Counts of Media Mentions")
	leg(order( 3 "medRxiv" 2 "bioRxiv"  4 "SSRN" 1 "arXiv") pos(11) rows(1) span) xtitle("") 
	name(yeslang, replace)
	xsize(4.25) ysize(4.25)
	graphregion(m(l=0 b=0 t=0 r=5))	
;
# del cr
graph export figure3b.png, as(png) replace
graph export figure3b.tif, as(tif) replace

grc1leg nolang yeslang, ycommon imargin(medium) name(topic, replace) plotregion(m(zero))
graph display, xsize(6) ysize(3.75)
graph export figure3ab.png, as(png) replace
graph export figure3ab.tif, as(tif) replace


/**************************
FIGURE 4 CODE: REGRESSION EFFECTS , TOPICS AND OUTLETS
**************************/
u preprints, clear
egen article_id_num = group(article_id)
drop article_id
rename article_id_num article_id
gen is_covid_real = is_covid==1 & posted_on_day_number>=2200

gen topic_consolidated = outlet_topic
replace topic_consolidated = "Other (specify)" if ~inlist(outlet_topic,"Health/Medicine","General News","Science/Technology")


preserve
	gen month = substr(posted_on,6,2)
	gen day   = substr(posted_on,9,2)
	gen year  = substr(posted_on,1,4)
	drop if research_venue=="wos"
	keep posted_on_day_n month day year
	duplicates drop
	destring month, replace
	destring day, replace
	destring year, replace
	egen weekyears = seq(), from(1) to(52) block(7)
	bys month: egen maxweek = max(weekyears)
	replace weekyears = maxweek+1 if weekyears==1 & maxweek==52 
	
	gen wednesday = 1 if posted_on_day_n==0
	gen thursday = 1 if posted_on_day_n==1
	gen friday = 1 if posted_on_day_n==2
	gen saturday = 1 if posted_on_day_n==3
	gen sunday = 1 if posted_on_day_n==4
	gen monday = 1 if posted_on_day_n==5
	gen tuesday = 1 if posted_on_day_n==6
	sum posted_on_day_n
	forval i = 7(7)`r(max)' {
		replace wednesday = 1 if posted_on_day_n==`i'
	}
	forval i = 8(7)`r(max)' {
		replace thursday = 1 if posted_on_day_n==`i'
	}
	forval i = 9(7)`r(max)' {
		replace friday = 1 if posted_on_day_n==`i'
	}
	forval i = 10(7)`r(max)' {
		replace saturday= 1 if posted_on_day_n==`i'
	}	
	forval i = 11(7)`r(max)' {
		replace sunday= 1 if posted_on_day_n==`i'
	}	
	forval i = 12(7)`r(max)' {
		replace monday= 1 if posted_on_day_n==`i'
	}	
	forval i = 13(7)`r(max)' {
		replace tuesday= 1 if posted_on_day_n==`i'
	}	
	foreach v in monday tues wed thur fri sat sun {
		replace `v' = 0 if `v'==.
	}
	egen weeks = seq(), block(7)
	bys weeks: gen days = _n
	tempfile hold
	save `hold'
restore

gen postcovid = posted_on_day_n>=2200 
*drop if research_venue=="wos"


*gcollapse (nansum) is_preprint, by(posted_on_day_n topic_consolidated  is_covid_real)
mer m:1 posted_on_day_n using `hold' //, keep(3)

gen firstmedarxiv = posted_on_day_number>=2029

gen firstcovid = posted_on_day_number>=2200

gen dayscov = posted_on_day_number - 2200
replace dayscov = 0 if dayscov<0

gen time = posted_on_day_number
gen time2 = posted_on_day_number*posted_on_day_number
gen time3 = posted_on_day_number*posted_on_day_number*posted_on_day_number

encode topic_consolidated, gen(temp)
rename topic_consolidated topic 
rename temp topic_consolidated

encode outlet_type, gen(temp)
rename outlet_type outlet
rename temp outlet_type

save outlettopic.dta, replace

eststo m2a: reghdfe is_preprint firstmedarxiv b3.topic_consolidated##(i.firstcov is_covid_real), abs(weekyears  month##days month##year , savefe) resid

eststo m3a: reghdfe is_preprint firstmedarxiv outlet_type##(i.firstcov is_covid_real), abs(weekyears  month##days month##year , savefe) resid

tempname memhold
tempfile results
postfile `memhold' str100 server str100 covidlang str100 time beta se str100 star using `results'

forval i = 1/4 {
	est restore m2a
	margins, dydx(firstcov) at(topic_consolidated=`i' is_covid_real=0 firstmedarxiv=1) post asobserved				// marginal change in preprint share precovid era (days) 
	mat define a = r(table)
	loc z = a[3,2]
	if abs(`z') < 1.65  loc nlstar " "
	if abs(`z') >= 1.65 loc nlstar "*"
	if abs(`z') >= 1.96 loc nlstar "**"
	if abs(`z') >= 2.58 loc nlstar "***"

	est restore m2a
	margins, dydx(is_covid_real) at(topic_consolidated=`i' firstcov=1 firstmedarxiv=1) post asobserved				// marginal change in preprint share precovid era (days) 
	mat define a = r(table)
	loc z = a[3,2]
	if abs(`z') < 1.65  loc ylstar " "
	if abs(`z') >= 1.65 loc ylstar "*"
	if abs(`z') >= 1.96 loc ylstar "**"
	if abs(`z') >= 2.58 loc ylstar "***"

	est restore m2a
	margins, at(topic_consolidated=`i' firstcov=0 is_covid_real=0 firstmedarxiv=1) post asobserved				// marginal change in preprint share precovid era (days) 
	mat define beta = e(b)
	loc beta = beta[1,1]
	mat define se = e(V)
	loc se = sqrt(se[1,1])
	levelsof topic if topic_consolidated==`i', loc(name) clean
	post `memhold' ("`name'") ("no lang") ("pre") (`beta') (`se') ("") 
	
	est restore m2a
	margins, at(topic_consolidated=`i' firstcov=0 is_covid_real=0 firstmedarxiv=1) post asobserved				// marginal change in preprint share precovid era (days) 
	mat define beta = e(b)
	loc beta = beta[1,1]
	mat define se = e(V)
	loc se = sqrt(se[1,1])
	levelsof topic if topic_consolidated==`i', loc(name) clean
	post `memhold' ("`name'") ("yes lang") ("pre") (`beta') (`se')	("") 
	
	est restore m2a
	margins, at(topic_consolidated=`i' firstcov=1 is_covid_real=0 firstmedarxiv=1) post asobserved				// marginal change in preprint share precovid era (days) 
	mat define beta = e(b)
	loc beta = beta[1,1]
	mat define se = e(V)
	loc se = sqrt(se[1,1])
	levelsof topic if topic_consolidated==`i', loc(name) clean
	post `memhold' ("`name'") ("no lang") ("post") (`beta') (`se')	("`nlstar'")
	
	est restore m2a
	margins, at(topic_consolidated=`i' firstcov=1 is_covid_real=1 firstmedarxiv=1) post asobserved				// marginal change in preprint share precovid era (days) 
	mat define beta = e(b)
	loc beta = beta[1,1]
	mat define se = e(V)
	loc se = sqrt(se[1,1])
	levelsof topic if topic_consolidated==`i', loc(name) clean
	post `memhold' ("`name'") ("yes lang") ("post") (`beta') (`se') ("`ylstar'")
	
}


forval i = 5/7 {
	est restore m3a
	margins, dydx(firstcov) at(outlet_type=`i' is_covid_real=0 firstmedarxiv=1) post asobserved				// marginal change in preprint share precovid era (days) 
	mat define a = r(table)
	loc z = a[3,2]
	if abs(`z') < 1.65  loc nlstar " "
	if abs(`z') >= 1.65 loc nlstar "*"
	if abs(`z') >= 1.96 loc nlstar "**"
	if abs(`z') >= 2.58 loc nlstar "***"

	est restore m3a
	margins, dydx(is_covid_real) at(outlet_type=`i' firstcov=1 firstmedarxiv=1) post asobserved				// marginal change in preprint share precovid era (days) 
	mat define a = r(table)
	loc z = a[3,2]
	if abs(`z') < 1.65  loc ylstar " "
	if abs(`z') >= 1.65 loc ylstar "*"
	if abs(`z') >= 1.96 loc ylstar "**"
	if abs(`z') >= 2.58 loc ylstar "***"

	est restore m3a
	margins, at(outlet_type=`i' firstcov=0 is_covid_real=0 firstmedarxiv=1) post asobserved				// marginal change in preprint share precovid era (days) 
	mat define beta = e(b)
	loc beta = beta[1,1]
	mat define se = e(V)
	loc se = sqrt(se[1,1])
	levelsof outlet if outlet_type==`i', loc(name) clean
	post `memhold' ("`name'") ("no lang") ("pre") (`beta') (`se') ("") 
	
	est restore m3a
	margins, at(outlet_type=`i' firstcov=0 is_covid_real=0 firstmedarxiv=1) post asobserved				// marginal change in preprint share precovid era (days) 
	mat define beta = e(b)
	loc beta = beta[1,1]
	mat define se = e(V)
	loc se = sqrt(se[1,1])
	levelsof outlet if outlet_type==`i', loc(name) clean
	post `memhold' ("`name'") ("yes lang") ("pre") (`beta') (`se')	("") 
	
	est restore m3a
	margins, at(outlet_type=`i' firstcov=1 is_covid_real=0 firstmedarxiv=1) post asobserved				// marginal change in preprint share precovid era (days) 
	mat define beta = e(b)
	loc beta = beta[1,1]
	mat define se = e(V)
	loc se = sqrt(se[1,1])
	levelsof outlet if outlet_type==`i', loc(name) clean
	post `memhold' ("`name'") ("no lang") ("post") (`beta') (`se')	("`nlstar'")
	
	est restore m3a
	margins, at(outlet_type=`i' firstcov=1 is_covid_real=1 firstmedarxiv=1) post asobserved				// marginal change in preprint share precovid era (days) 
	mat define beta = e(b)
	loc beta = beta[1,1]
	mat define se = e(V)
	loc se = sqrt(se[1,1])
	levelsof outlet if outlet_type==`i', loc(name) clean
	post `memhold' ("`name'") ("yes lang") ("post") (`beta') (`se') ("`ylstar'")
	
}

postclose `memhold'
u `results', clear
compress
gen betahi = beta+1.96*se
gen betalo = beta-1.96*se

gen xaxis = 1 if time=="pre"
replace xaxis =2 if time=="post"
gen temp = 1 if server=="General News"
replace temp = 2 if server=="Health/Medicine"
replace temp = 3 if server=="Science/Technology"
replace temp = 4 if server=="Other (specify)"

replace temp = 5 if server=="Legacy"
replace temp = 6 if server=="Non-journalism"
replace temp = 7 if server=="Peripheral" 
labmask temp, val(server)
rename server server_old
rename temp server

save outlets_and_topics.dta, replace

u outlets_and_topics, clear
set scheme white_tableau
foreach v in 1 2 3 4 {
	levelsof beta if server==`v'  & covidlang=="no lang" & time=="post", loc(`v'y) clean
	levelsof star if server==`v' & covidlang=="no lang" & time=="post", loc(`v's) clean
}

# del ;
graph tw 
	(connected beta xaxis if server==1 & covidlang=="no lang")
	(connected beta xaxis if server==2 & covidlang=="no lang")
	(connected beta xaxis if server==3 & covidlang=="no lang")
	(connected beta xaxis if server==4 & covidlang=="no lang"),
	text(`1y' 2.01 "`1s'", just(left) size(small) place(e))
	text(`2y' 2.01 "`2s'", just(left) size(small) place(e))
	text(`3y' 2.01 "`3s'", just(left) size(small) place(e))
	text(`4y' 2.01 "`4s'", just(left) size(small) place(e))
	xlab(1 "Pre-Covid" 2 "Covid Era", labsize(medium)) xsca(range(.95 2.05))
	ylab(0 "0" .05 "5" .1 "10" .15 "15" , labsize(medium)) ytitle("Share of Preprint Mentions (in percentage points 0-100)")
	leg(order(2 "Health/Medicine" 3 "Science/Technology" 1 "General News"  4 "Other") pos(11) rows(1) span) xtitle("") 
	name(nolang, replace)
	xsize(4.25) ysize(4.25)
	graphregion(m(l=0 b=0 t=0 r=5))	
;
# del cr
graph export figure4a.png, as(png) replace
graph export figure4a.tif, as(tif) replace

foreach v in 1 2 3 4 {
	levelsof beta if server==`v'  & covidlang=="yes lang" & time=="post", loc(`v'y) clean
	levelsof star if server==`v' & covidlang=="yes lang" & time=="post", loc(`v's) clean
}
# del ;
graph tw 
	(connected beta xaxis if server==1 & covidlang=="yes lang")
	(connected beta xaxis if server==2 & covidlang=="yes lang")
	(connected beta xaxis if server==3 & covidlang=="yes lang")
	(connected beta xaxis if server==4 & covidlang=="yes lang")
	,
	text(`1y' 2.01 "`1s'", just(left) size(small) place(e))
	text(`2y' 2.01 "`2s'", just(left) size(small) place(e))
	text(`3y' 2.01 "`3s'", just(left) size(small) place(e))
	text(`4y' 2.01 "`4s'", just(left) size(small) place(e))
	xlab(1 "Pre-Covid" 2 "Covid Era", labsize(medium)) xsca(range(.95 2.05))
	ylab(0 "0" .05 "5" .1 "10" .15 "15" , labsize(medium)) ytitle("Share of Preprint Mentions (in percentage points 0-100)")
	leg(order(2 "Health/Medicine" 3 "Science/Technology" 1 "General News"  4 "Other") pos(11) rows(1) span) xtitle("") 
	name(yeslang, replace)
	xsize(4.25) ysize(4.25)
	graphregion(m(l=0 b=0 t=0 r=5))
;
# del cr
graph export figure4b.png, as(png) replace
graph export figure4b.tif, as(tif) replace

grc1leg nolang yeslang, ycommon imargin(medium) name(topic, replace) plotregion(m(zero))
graph display, xsize(5.5) ysize(3.43)
graph export figure4ab.png, as(png) replace
graph export figure4ab.tif, as(tif) replace


foreach v in 5 6 7  {
	levelsof beta if server==`v'  & covidlang=="no lang" & time=="post", loc(`v'y) clean
	levelsof star if server==`v' & covidlang=="no lang" & time=="post", loc(`v's) clean
}

# del ;
graph tw 
	(connected beta xaxis if server==1 & covidlang=="no lang")
	(connected beta xaxis if server==2 & covidlang=="no lang")
	(connected beta xaxis if server==3 & covidlang=="no lang")
	,
	text(`5y' 2.01 "`5s'", just(left) size(small) place(e))
	text(`6y' 2.01 "`6s'", just(left) size(small) place(e))
	text(`7y' 2.01 "`7s'", just(left) size(small) place(e))
	xlab(1 "Pre-Covid" 2 "Covid Era", labsize(medium)) xsca(range(.95 2.05))
	ylab(0 "0" .05 "5" .1 "10" .15 "15" .2 "20", labsize(medium)) ytitle("Share of Preprint Mentions (in percentage points 0-100)")
	leg(order( 2 "Non-Journalism" 3 "Peripheral" 1 "Legacy") pos(11) rows(1)) xtitle("") 
	name(nolang, replace)
	xsize(4.25) ysize(4.25)
	graphregion(m(l=0 b=0 t=0 r=5))
;
# del cr
graph export figure4c.png, as(png) replace
graph export figure4c.tif, as(tif) replace

foreach v in 5 6 7 {
	levelsof beta if server==`v'  & covidlang=="yes lang" & time=="post", loc(`v'y) clean
	levelsof star if server==`v' & covidlang=="yes lang" & time=="post", loc(`v's) clean
}
# del ;
graph tw 
	(connected beta xaxis if server==5 & covidlang=="yes lang")
	(connected beta xaxis if server==6 & covidlang=="yes lang")
	(connected beta xaxis if server==7 & covidlang=="yes lang")
	,
	text(`5y' 2.01 "`5s'", just(left) size(small) place(e))
	text(`6y' 2.01 "`6s'", just(left) size(small) place(e))
	text(`7y' 2.01 "`7s'", just(left) size(small) place(e))
	xlab(1 "Pre-Covid" 2 "Covid Era", labsize(medium)) xsca(range(.95 2.05))
	ylab(0 "0" .05 "5" .1 "10" .15 "15" .2 "20" , labsize(medium)) ytitle("Share of Preprint Mentions (in percentage points 0-100)")
	leg(order( 2 "Non-Journalism" 3 "Peripheral" 1 "Legacy") pos(11) rows(1)) xtitle("") 
	name(yeslang, replace)
	xsize(4.25) ysize(4.25)
	graphregion(m(l=0 b=0 t=0 r=5))
;
# del cr
graph export figure4d.png, as(png) replace
graph export figure4d.tif, as(tif) replace

grc1leg nolang yeslang, ycommon imargin(medium) name(topic, replace) plotregion(m(zero))
graph display, xsize(5.5) ysize(3.43)
graph export figure4cd.png, as(png) replace
graph export figure4cd.tif, as(tif) replace


u server, clear
qui eststo m1a: reghdfe is_preprint i.firstmedarxiv i.research_venue##(i.firstcov is_covid_real), abs(weekyears  month##days month##year , savefe) resid

forval i = 1/4 {
	est restore m1a
	*margins, dydx(firstcov) at(research_venue=`i' is_covid_real=0 firstmedarxiv=1) post asobserved				// marginal change in preprint share precovid era (days) 
	margins, dydx(is_covid_real) at(firstcov=1 research_venue=`i' firstmedarxiv=1) post asobserved				// marginal change in preprint share precovid era (days) 
}

u outlettopic, clear
eststo m2a: reghdfe is_preprint firstmedarxiv b3.topic_consolidated##(i.firstcov is_covid_real), abs(weekyears  month##days month##year , savefe) resid
forval i = 1/4 {
	est restore m2a
	*margins, dydx(is_covid_real) at(topic_consolidated=`i' firstcov=1 firstmedarxiv=1) post asobserved				// marginal change in preprint share precovid era (days) 
	margins, dydx(firstcov) at(topic_consolidated=`i' is_covid_real=0 firstmedarxiv=1) post asobserved				// marginal change in preprint share precovid era (days) 
}

u outlettopic, clear
eststo m3a: reghdfe is_preprint firstmedarxiv outlet_type##(i.firstcov is_covid_real), abs(weekyears  month##days month##year , savefe) resid
margins, dydx(firstcov) at(outlet_type=5 is_covid_real=0 firstmedarxiv=1) post asobserved				// marginal change in preprint share precovid era (days) 
forval i = 5/7 {
	est restore m3a
	margins, dydx(is_covid_real) at(outlet_type=`i' firstcov=1 firstmedarxiv=1) post asobserved				// marginal change in preprint share precovid era (days) 
	est restore m3a
	margins, dydx(firstcov) at(outlet_type=`i' is_covid_real=0 firstmedarxiv=1) post asobserved				// marginal change in preprint share precovid era (days) 
}


/***************************
TABLE 6 CODE: OUTLET-LEVEL PREPRINT SHARE 
***************************/
gen preprint = research_venue!="wos"
gcollapse (count) mention_id, by(preprint is_covid outlet_name)
rename mention_id counts
bys outlet_name is_covid: egen total = total(counts)
gen share = counts/total
keep if preprint==1

gsort outlet_name is_covid 
egen totaltotal = total(counts)							// total number of articles by outlet	
bys outlet_name: egen outlettotal= total(counts)		// total number of articles period 	
gen totalshare = outlettotal/totaltotal					// who has the biggest share of articles 

preserve
	keep if is_covid==1
	gsort -totalshare
	gen top20 = _n
	keep if top20<=25
	keep outlet_name
	duplicates drop
	tempfile hold
	save `hold'
restore
mer m:1 outlet_name using `hold'
keep if _m==3
sort outlet_name is_covid
keep share outlet_name is_covid totalshare


reshape wide share totalshare, i(outlet_name) j(is_covid)
br outlet_name totalshare1 share0 share1
order outlet_name totalshare1 share0 share1




