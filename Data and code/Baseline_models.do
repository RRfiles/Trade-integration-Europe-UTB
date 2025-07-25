** GRAVITY
clear all
use Data.dta
gen pairid=i+j

drop if ee2==.
drop if i=="IS00"
drop if j=="IS00"
bys year i: egen gdp=total(gdp_j)
gen E_m=(ee2*gdp)/(gdp_i*gdp_j)


*Table 1: Gravity model
ppmlhdfe E_m intra inter lndist contig_r contig_c comlang_off EU UEM island coast inst outermost island_c coast_c nocoast_c mega mega_both, noabsorb cluster (pairid)
outreg2 using baseline.xls, excel keep(intra inter lndist contig_r contig_c comlang_off EU UEM island coast inst outermost island_c coast_c  nocoast_c mega mega_both) replace addstat(Pseudo R2, e(r2_p)) addtext(Year FE, NO, Country FE, NO, Contry-Year FE, NO, Region FE, NO, Region-Year FE, NO) ctitle(M1_1)

ppmlhdfe E_m intra inter lndist contig_r contig_c comlang_off EU UEM island coast inst outermost island_c coast_c  nocoast_c mega mega_both, absorb (year ci2 cj2)  cluster (pairid)
outreg2 using baseline.xls, excel keep(intra inter lndist contig_r contig_c comlang_off EU UEM island coast inst outermost island_c coast_c  nocoast_c mega mega_both) append addstat(Pseudo R2, e(r2_p)) addtext(Year FE, YES, Country FE, YES, Contry-Year FE, NO, Region FE, NO, Region-Year FE, NO) ctitle(M1_2)

ppmlhdfe E_m intra inter lndist contig_r contig_c comlang_off EU UEM island coast inst outermost island_c coast_c  nocoast_c mega mega_both, absorb (year#ci2 year#cj2)  cluster (pairid)
outreg2 using baseline.xls, excel keep(intra inter lndist contig_r contig_c comlang_off EU UEM island coast inst outermost island_c coast_c  nocoast_c mega mega_both) append addstat(Pseudo R2, e(r2_p)) addtext(Year FE, YES, Country FE, YES, Contry-Year FE, YES, Region FE, NO, Region-Year FE, NO) ctitle(M1_3)

ppmlhdfe E_m intra inter lndist contig_r contig_c comlang_off EU UEM island coast inst outermost island_c coast_c  nocoast_c mega mega_both, absorb (year i2 j2)  cluster (pairid)
outreg2 using baseline.xls, excel keep(intra inter lndist contig_r contig_c comlang_off EU UEM island coast inst outermost island_c coast_c  nocoast_c mega mega_both) append addstat(Pseudo R2, e(r2_p)) addtext(Year FE, YES, Country FE, NO, Contry-Year FE, NO, Region FE, YES, Region-Year FE, NO) ctitle(M1_4)

ppmlhdfe E_m intra inter lndist contig_r contig_c comlang_off EU UEM island coast inst outermost island_c coast_c  nocoast_c mega mega_both, absorb (year#i2 year#j2)  cluster (pairid)
outreg2 using baseline.xls, excel keep(intra inter lndist contig_r contig_c comlang_off EU UEM island coast inst outermost island_c coast_c nocoast_c mega mega_both) append addstat(Pseudo R2, e(r2_p)) addtext(Year FE, YES, Country FE, NO, Contry-Year FE, NO, Region FE, YES, Region-Year FE, YES) ctitle(M1_5)






**** UTB - Baseline **** 
clear all
use Data.dta
replace ee2=0 if ee2<0.001
bys year i: egen gdp=total(gdp_j)
gen E_m=(ee2*gdp)/(gdp_i*gdp_j)

gen pairid=i+j


*Outliers
drop if outermost==1
drop if i=="ES63"
drop if j=="ES63"
drop if i=="ES64"
drop if j=="ES64"
drop if i=="FI20"
drop if j=="FI20"
drop if i=="IS00"
drop if j=="IS00"

bys i j (year): gen trend=_n
gen intrapr_t=trend*intra 
gen interpr_t=trend*inter
drop trend

gen intexp=0
replace intexp=i2 if intra==0 & inter==0

gen intimp=0
replace intimp=j2 if intra==0 & inter==0


*PAIR FE GRAVITY
ppml_panel_sg E_m intrapr_t interpr_t, ex(i2) im(j2) y(year) dropsing ro max(1000000) genD(FE)
outreg2 using baseline_utb.xls, excel keep(intrapr_t interpr_t) replace ctitle(Structural)
drop if FE==.
replace FE=ln(FE)


*Accomodation of some of the independent variables
replace lndist=0 if intra==1
gen ROW_i=0
replace ROW_i=1 if i=="ROW"
gen ROW_j=0
replace ROW_j=1 if j=="ROW"
replace lndist=0 if i=="ROW" | j=="ROW"

*Gravity variables estimation
ppmlhdfe E_m intrapr_t interpr_t lndist contig_r contig_c, absorb(HB=intra#i2  year#i2 year#j2 ROW_i#j2 ROW_j#i2) cluster (i2#j2)
outreg2 using baseline_utb.xls, excel keep(intrapr_t interpr_t lndist contig_r contig_c inter) append ctitle(Gravity)


*gravity trade cost
gen grav=(_b[lndist]*lndist+_b[contig_r]*contig_r+_b[contig_c]*contig_c)


*"home bias" of origin (i) and destination (j)
gen intrai=0
forvalues aa=1/298{
gen HB1_`aa'=HB if intra==1 & i2==`aa'
egen HB1_`aa'_mean=mean(HB1_`aa')
gen HB0_`aa'=HB if intra==0 & i2==`aa'
egen HB0_`aa'_mean=mean(HB0_`aa')
gen HB_`aa'=HB1_`aa'_mean-HB0_`aa'_mean

replace intrai=HB_`aa' if i2==`aa'
drop HB1_`aa' HB0_`aa' HB1_`aa'_mean HB0_`aa'_mean HB_`aa'
}

preserve
keep i intrai
bys i: drop if i==i[_n-1]
rename i j
rename intrai intraj
save intraj, replace
restore
*Merge of the home bias
merge m:1 j using intraj
drop _merge


*Keep only the observations needed in the estimation (Inter-regional european flows)
gen inter_reg=1
replace inter_reg=0 if intra==1
replace inter_reg=0 if i=="ROW" | j=="ROW"
replace FE=. if inter_reg==0 
replace grav=. if FE==.

*We keep only one year (the results are the same for all years, an average)
keep if year==2016

*Model for the estimation of the UTB
bootstrap, reps(50): reg FE grav if inter_reg==1
outreg2 using UTB.xls, excel replace ctitle(UTB)
bootstrap, reps(50): reg FE grav intrai intraj if inter_reg==1
outreg2 using UTB.xls, excel append ctitle(UTB_2)

*Final computation of the UTB
gen lnUTB=_b[_cons]+((_b[grav]-1)*grav)+(_b[intrai]+_b[intrai]/(_b[intrai]+_b[intraj]))*intrai+(_b[intraj]+_b[intraj]/(_b[intrai]+_b[intraj]))*intraj if inter_reg==1
gen UTB=exp(lnUTB)

*Results
preserve
keep i j UTB
bys i j: drop if j==j[_n-1]
reshape wide UTB, i(i) j(j, s)
export excel using UTB_matrix.xlsx, firstrow(variables) replace
restore


keep i j lnUTB UTB intrai 
save UTB_results_europe.dta, replace



*** DATA FOR FIGURES AND MAPS
clear all
use Data

*compute total values for trade
drop if (i=="ROW" | j=="ROW")
gen flow="International"
replace flow="Intra-national" if c_i==c_j
replace flow="Intra-regional" if i==j

bys year i flow: egen trade=total(ee2)
bys year i: egen total_trade=total(ee2)
bys j flow i (year): egen trade_m=mean(trade)
bys j i (year): egen total_trade_m=mean(total_trade)
drop trade total_trade

*compute number of partners
replace ee2=. if ee2==0
bys year i flow: egen n=count(ee2)
bys year i flow: egen n_total=count(trade)
bys j i: egen n_reg=mean(n)
gen n_share=n_reg/n_total
drop n

*drop years
bys i j: drop if j==j[_n-1]
drop year

*merge UTBs
merge 1:1 i j using UTB_results_europe
drop if (i=="ROW" | j=="ROW")
drop if _merge==1
drop _merge

rename trade_m trade
rename total_trade_m total_trade

keep i j flow intrai lnUTB UTB trade n_reg n_share total_trade
bys i j: drop if j==j[_n-1]
drop if i==j

bys i flow: egen UTB_mean=mean(UTB)
bys i flow: egen UTB_median=median(UTB)
replace UTB=0 if UTB==.
bys i flow: egen UTB_mean2=mean(UTB)
bys i flow: egen UTB_median2=median(UTB)


bys i flow: drop if flow==flow[_n-1]
drop j lnUTB UTB

rename i NUTS2

replace flow="Intranat" if flow=="Intra-national"
reshape wide UTB_mean UTB_median UTB_mean2 UTB_median2 intrai trade n_reg n_share total_trade, i(NUTS2) j(flow, s)
drop intraiIntranat
rename intraiInternational intra
drop total_tradeIntranat
rename total_tradeInternational total_trade
rename UTB_meanInternational UTB_international
rename UTB_meanIntranat UTB_intranational
rename UTB_mean2International UTB2_international
rename UTB_mean2Intranat UTB2_intranational

rename UTB_medianInternational UTB_med_international
rename UTB_medianIntranat UTB_med_intranational
rename UTB_median2International UTB2_med_international
rename UTB_median2Intranat UTB2_med_intranational

rename tradeIntranat trade_intranational
rename tradeInternational trade_international
rename n_shareInternational n_share_international
rename n_regInternational n_reg_international
rename n_shareIntranat n_share_intranational
rename n_regIntranat n_reg_intranational
save data_maps, replace