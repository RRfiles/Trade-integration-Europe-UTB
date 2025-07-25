*FIGURE 2
clear all
use Data

gen flow="International"
replace flow="Intranational" if c_i==c_j & i!=j
replace flow="Intraregional" if c_i==c_j & i==j
replace flow="International" if i=="ROW" & j=="ROW"

drop if i=="ROW"
bys year flow: egen double ee=total(ee2)
bys year flow: drop if flow==flow[_n-1]
keep year ee flow

reshape wide ee, i(year) j(flow,s) 

graph bar eeInternational eeIntranational eeIntraregional,over(year) percentages stack  bar(1, color("0 0 0")) bar(2, color("95 95 95")) bar(3, color("200 200 200")) ysize() nofill ylabel(, nogrid labsize(small)) graphregion(ifcolor(white) fcolor(white) lcolor(white) icolor(white)) ytitle(% Total trade, size(mmall)) legend(ring() position(6) region(lcolor(black)) cols(3) order (1 "International" 2 "Intra-national" 3 "Intra-regional") size(small)) scheme() title() saving()
*graph export FIGURE1.emf, as(emf) replace


*FIGURE 3
clear all
use Data

gen flow="International"
replace flow="Intranational" if c_i==c_j & i!=j
replace flow="Intraregional" if c_i==c_j & i==j
replace flow="International" if i=="ROW" & j=="ROW"

drop if i=="ROW" & j=="ROW"
drop if c_i=="ROW"
bys year c_i flow: egen double ee=total(ee2)
bys year c_i flow: drop if flow==flow[_n-1]
keep year c_i flow ee

bys c_i flow: egen double ee2=mean(ee)
bys c_i flow: drop if flow==flow[_n-1]
drop ee year

reshape wide ee2, i(c_i) j(flow,s) 

drop if c_i=="IS"|c_i=="LI"
graph bar ee2International ee2Intranational ee2Intraregional,over(c_i, label(angle(45))) percentages stack  bar(1, color("0 0 0")) bar(2, color("95 95 95")) bar(3, color("200 200 200")) ysize() nofill ylabel(, nogrid labsize(small)) graphregion(ifcolor(white) fcolor(white) lcolor(white) icolor(white)) ytitle(% Total trade, size(small)) legend(ring() position(6) region(lcolor(black)) cols(3) order (1 "International" 2 "Intra-national" 3 "Intra-regional") size(small)) scheme() title() saving()
*graph export FIGURE2.emf, as(emf) replace




*FIGURE 4
clear all
use Data
keep if year==2018
drop if c_i==c_j
bys i: egen int_export=total(ee2)
gen export_gdp=int_export/gdp_i
drop if outermost==1
keep i export_gdp
bys i: drop if i==i[_n-1]
save data_figure3, replace

clear all
use NUTS_RG_01M_2016_4326_LEVL_2, clear
rename NUTS_ID i
merge 1:1 i using data_figure3
drop if _merge==1
drop if _merge==2
drop _merge
rename i NUTS_ID
xtset, clear
format %9.2fc export_gdp
grmap export_gdp, clnum(8) fcolor(YlOrBr) title() subtitle() legstyle(2) legend(title(International exports/GDP, size(small)) size(*1.25) position(9))
*graph export FIGURE3.emf, as(emf) replace





*********************************************
clear all
use NUTS_RG_01M_2016_4326_LEVL_2, clear
rename NUTS_ID NUTS2
merge 1:1 NUTS2 using data_maps
drop if _merge==1
drop if _merge==2
drop _merge
rename NUTS2 NUTS_ID


gen intensive_international=trade_international/n_reg_international
gen share_international=trade_international/total_trade
gen share_intranational=trade_intranational/total_trade
gen share_intra=(total_trade-trade_international-trade_intranational)/total_trade

xtset, clear
format %9.1g UTB_international
format %9.1g UTB_intranational
format %9.1g UTB_med_international
format %9.1g UTB_med_intranational
format %9.1g UTB2_international
format %9.1g UTB2_intranational
format %9.1g intra
format %12.0fc total_trade
format %12.0fc trade_international
format %12.0fc trade_intranational
format %8.0g n_reg_international
format %9.1g n_share_international
format %9.0g n_reg_intranational
format %9.1g n_share_intranational
format %12.0fc intensive_international
format %9.1g share_international
format %9.1g share_intranational
format %9.1g share_intra



*FIGURE 6A
grmap UTB_international, clnum(9) fcolor(RdYlGn) title() subtitle() legstyle(2) legend(title(UTB mean value, size(small)) size(*1.25) position(11))
*graph export UTB_mean.emf, as(emf) replace

*FIGURE 6B
grmap UTB_intranational, clnum(9) fcolor(RdYlGn) title() subtitle() legstyle(2) legend(title(UTB mean value, size(small)) size(*1.25) position(11))
*graph export UTB_inter_mean.emf, as(emf) replace



*FIGURE 7
egen UTB_internat_mean=mean(UTB_international)
egen UTB_interreg_mean=mean(UTB_intranational)
gen outlier=0
replace outlier=1 if (NUTS_ID=="LT01"|NUTS_ID=="LT02"|NUTS_ID=="IE05"|NUTS_ID=="IE06"|NUTS_ID=="HU11"|NUTS_ID=="HU12")
twoway (scatter UTB_international UTB_intranational if outlier==0) (scatter UTB_international UTB_intranational if outlier==1, mlabel(NUTS_ID) mlabposition(0) msymbol(i) mlabcolor(stc1)) (lfit UTB_international UTB_intranational, color(stc2)) (lfit UTB_international UTB_intranational if outlier==0, color(red)) (line UTB_internat_mean UTB_intranational,color(black)) (line UTB_international UTB_interreg_mean,color(black)),ytitle(International UTB, size()) yscale(range()) xscale(range()) ylabel(,angle(horizontal)  labsize()) xtitle(Intra-national UTB, size()) xlabel(,angle(horizontal) labsize()) title() legend(pos() col() label(1 "UTB Values") label(3 "Fitted values") label(4 "Fitted values" "(without intra-national" "UTB values over 3.5)") order(1 3 4)) scheme() saving()
*graph save Graph scatter1, replace
*graph export scatter1.emf, as(emf) replace



*Figure 8A
grmap n_share_international, clnum(9) fcolor(RdYlGn) title() subtitle() legstyle(2) legend(title(Share number regions, size(small)) size(*1.25) position(11))
*graph save Graph share_reg, replace
*graph export share_reg.emf, as(emf) replace

*Figure 8B
grmap intensive_international, clnum(9) fcolor(RdYlGn) title() subtitle() legstyle(2) legend(title(million euros, size(small)) size(*1.25) position(11))
*graph save Graph intensive, replace
*graph export intensive.emf, as(emf) replace


*Figure 8C
twoway (scatter UTB_international n_share_international) (lfit UTB_international n_share_international),ytitle(International UTB, size()) yscale(range()) xscale(range()) ylabel(,angle(horizontal)  labsize()) xtitle(Share number of regions, size()) xlabel(,angle(horizontal) labsize()) title() legend(pos() col() order (1 "UTB Values" 2 "Fitted values")) scheme() saving()
*graph save Graph scatter2, replace
*graph export scatter2.emf, as(emf) replace


*Figure 8D
gen lnti=ln(intensive_international)
twoway (scatter UTB_international lnti) (lfit UTB_international lnti),ytitle(International UTB, size()) yscale(range()) xscale(range()) ylabel(,angle(horizontal)  labsize()) xtitle(ln(Intensive margin), size()) xlabel(,angle(horizontal) labsize()) title() legend(pos() col() order (1 "UTB Values" 2 "Fitted values")) scheme() saving()
*graph save Graph scatter3, replace
*graph export scatter3.emf, as(emf) replace