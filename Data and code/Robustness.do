**** UTB Baseline with deciles in distance **** 
clear all
use Data.dta

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

replace distance=. if intra==1
replace distance=. if i=="ROW" | j=="ROW"

*Accomodation of distance variables
forv i=10(10)90{
	egen pctile`i'= pctile(distance) , p(`i')
}

gen distance_q=irecode(distance, 0, pctile10, pctile20, pctile30, pctile40, pctile50, pctile60, pctile70, pctile80, pctile90)

gen lndist1=ln(distance+1) if distance_q==1
gen lndist2=ln(distance+1) if distance_q==2
gen lndist3=ln(distance+1) if distance_q==3
gen lndist4=ln(distance+1) if distance_q==4
gen lndist5=ln(distance+1) if distance_q==5
gen lndist6=ln(distance+1) if distance_q==6
gen lndist7=ln(distance+1) if distance_q==7
gen lndist8=ln(distance+1) if distance_q==8
gen lndist9=ln(distance+1) if distance_q==9
gen lndist10=ln(distance+1) if distance_q==10

replace lndist1=0 if lndist1==.
replace lndist2=0 if lndist2==.
replace lndist3=0 if lndist3==.
replace lndist4=0 if lndist4==.
replace lndist5=0 if lndist5==.
replace lndist6=0 if lndist6==.
replace lndist7=0 if lndist7==.
replace lndist8=0 if lndist8==.
replace lndist9=0 if lndist9==.
replace lndist10=0 if lndist10==.
drop lndist

*PAIR FE GRAVITY
ppml_panel_sg E_m intrapr_t interpr_t, ex(i2) im(j2) y(year) dropsing ro max(1000000) genD(FE)
outreg2 using robust.xls, excel keep(intrapr_t interpr_t) replace ctitle(Baseline_FE)

drop if FE==.
replace FE=ln(FE)


*Accomodation of some of the independent variables
gen ROW_i=0
replace ROW_i=1 if i=="ROW"
gen ROW_j=0
replace ROW_j=1 if j=="ROW"

*Gravity variables estimation
ppmlhdfe E_m intrapr_t interpr_t lndist* contig_r contig_c, absorb(HB=intra#i2  year#i2 year#j2 ROW_i#j2 ROW_j#i2) cluster (i2#j2)
outreg2 using robust.xls, excel keep(intrapr_t interpr_t lndist* contig_r contig_c) append ctitle(Baseline_dist10)


*gravity trade cost
gen grav=(_b[lndist1]*lndist1+_b[lndist2]*lndist2+_b[lndist3]*lndist3+_b[lndist4]*lndist4+_b[lndist5]*lndist5+_b[lndist6]*lndist6+_b[lndist7]*lndist7+_b[lndist8]*lndist8+_b[lndist9]*lndist9+_b[lndist10]*lndist10+_b[contig_r]*contig_r+_b[contig_c]*contig_c)


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
save intraj2, replace
restore
*Merge of the home bias
merge m:1 j using intraj2
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
bootstrap, reps(50): reg FE grav intrai intraj if inter_reg==1
outreg2 using UTB.xls, excel append ctitle(UTB_3)

*Final computation of the UTB
gen lnUTB=_b[_cons]+((_b[grav]-1)*grav)+(_b[intrai]+_b[intrai]/(_b[intrai]+_b[intraj]))*intrai+(_b[intraj]+_b[intraj]/(_b[intrai]+_b[intraj]))*intraj if inter_reg==1
gen UTB=exp(lnUTB)

keep year i j UTB lnUTB
rename UTB UTB2
rename lnUTB lnUTB2
save UTB_results_europe2.dta, replace













**** UTB Baseline with quantiles in distance **** 


clear all
use Data.dta

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

replace distance=. if intra==1
replace distance=. if i=="ROW" | j=="ROW"

*Accomodation of distance variables
forv i=25(25)75{
	egen pctile`i'= pctile(distance) , p(`i')
}

gen distance_q=irecode(distance, 0, pctile25, pctile50, pctile75)

gen lndist1=ln(distance+1) if distance_q==1
gen lndist2=ln(distance+1) if distance_q==2
gen lndist3=ln(distance+1) if distance_q==3
gen lndist4=ln(distance+1) if distance_q==4

replace lndist1=0 if lndist1==.
replace lndist2=0 if lndist2==.
replace lndist3=0 if lndist3==.
replace lndist4=0 if lndist4==.
drop lndist


*PAIR FE GRAVITY
ppml_panel_sg E_m intrapr_t interpr_t, ex(i2) im(j2) y(year) dropsing ro max(1000000) genD(FE)
outreg2 using robust.xls, excel keep(intrapr_t interpr_t) append ctitle(Baseline_FE)

drop if FE==.
replace FE=ln(FE)


*Accomodation of some of the independent variables
gen ROW_i=0
replace ROW_i=1 if i=="ROW"
gen ROW_j=0
replace ROW_j=1 if j=="ROW"

*Gravity variables estimation
ppmlhdfe E_m intrapr_t interpr_t lndist* contig_r contig_c, absorb(HB=intra#i2  year#i2 year#j2 ROW_i#j2 ROW_j#i2) cluster (i2#j2)
outreg2 using robust.xls, excel keep(intrapr_t interpr_t lndist* contig_r contig_c) append ctitle(Baseline_dist4)


*gravity trade cost
gen grav=(_b[lndist1]*lndist1+_b[lndist2]*lndist2+_b[lndist3]*lndist3+_b[lndist4]*lndist4+_b[contig_r]*contig_r+_b[contig_c]*contig_c)


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
save intraj3, replace
restore
*Merge of the home bias
merge m:1 j using intraj3
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
bootstrap, reps(50): reg FE grav intrai intraj if inter_reg==1
outreg2 using UTB.xls, excel append ctitle(UTB_4)

*Final computation of the UTB
gen lnUTB=_b[_cons]+((_b[grav]-1)*grav)+(_b[intrai]+_b[intrai]/(_b[intrai]+_b[intraj]))*intrai+(_b[intraj]+_b[intraj]/(_b[intrai]+_b[intraj]))*intraj if inter_reg==1
gen UTB=exp(lnUTB)

keep year i j UTB lnUTB
rename UTB UTB3
rename lnUTB lnUTB3

save UTB_results_europe3.dta, replace



**************************************************
***   ALTERNATIVE DEP. VARIABLE (alternative size adjusted trade)         
**************************************************

**** UTB Baseline with deciles in distance **** 
clear all
use Data.dta

bys year i: egen te=total(ee2)
bys year j: egen ti=total(ee2)
bys year: egen tt=total(ee2)
gen E_mod=(ee2*tt)/(te*ti)
replace E_mod=0 if E_mod==.

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

replace distance=. if intra==1
replace distance=. if i=="ROW" | j=="ROW"

*PAIR FE GRAVITY
ppml_panel_sg E_mod intrapr_t interpr_t, ex(i2) im(j2) y(year) dropsing ro max(1000000) genD(FE)
outreg2 using robust.xls, excel keep(intrapr_t interpr_t) append ctitle(GDP_FE)
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
ppmlhdfe E_mod intrapr_t interpr_t lndist contig_r contig_c, absorb(HB=intra#i2  year#i2 year#j2 ROW_i#j2 ROW_j#i2) cluster (i2#j2)
outreg2 using robust.xls, excel keep(intrapr_t interpr_t lndist contig_r contig_c) append ctitle(GDP_Grav)


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
save intraj4, replace
restore
*Merge of the home bias
merge m:1 j using intraj4
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
bootstrap, reps(50): reg FE grav intrai intraj if inter_reg==1
outreg2 using UTB.xls, excel append ctitle(UTB_6)

*Final computation of the UTB
gen lnUTB=_b[_cons]+((_b[grav]-1)*grav)+(_b[intrai]+_b[intrai]/(_b[intrai]+_b[intraj]))*intrai+(_b[intraj]+_b[intraj]/(_b[intrai]+_b[intraj]))*intraj if inter_reg==1
gen UTB=exp(lnUTB)

keep year i j UTB lnUTB
rename UTB UTB4
rename lnUTB lnUTB4

save UTB_results_europe4.dta, replace














**** UTB alternative dep. var. with deciles in distance **** 
clear all
use Data.dta

bys year i: egen te=total(ee2)
bys year j: egen ti=total(ee2)
bys year: egen tt=total(ee2)
gen E_mod=(ee2*tt)/(te*ti)
replace E_mod=0 if E_mod==.

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

replace distance=. if intra==1
replace distance=. if i=="ROW" | j=="ROW"

*Accomodation of distance variables
forv i=10(10)90{
	egen pctile`i'= pctile(distance) , p(`i')
}

gen distance_q=irecode(distance, 0, pctile10, pctile20, pctile30, pctile40, pctile50, pctile60, pctile70, pctile80, pctile90)

gen lndist1=ln(distance+1) if distance_q==1
gen lndist2=ln(distance+1) if distance_q==2
gen lndist3=ln(distance+1) if distance_q==3
gen lndist4=ln(distance+1) if distance_q==4
gen lndist5=ln(distance+1) if distance_q==5
gen lndist6=ln(distance+1) if distance_q==6
gen lndist7=ln(distance+1) if distance_q==7
gen lndist8=ln(distance+1) if distance_q==8
gen lndist9=ln(distance+1) if distance_q==9
gen lndist10=ln(distance+1) if distance_q==10

replace lndist1=0 if lndist1==.
replace lndist2=0 if lndist2==.
replace lndist3=0 if lndist3==.
replace lndist4=0 if lndist4==.
replace lndist5=0 if lndist5==.
replace lndist6=0 if lndist6==.
replace lndist7=0 if lndist7==.
replace lndist8=0 if lndist8==.
replace lndist9=0 if lndist9==.
replace lndist10=0 if lndist10==.
drop lndist


*PAIR FE GRAVITY
ppml_panel_sg E_mod intrapr_t interpr_t, ex(i2) im(j2) y(year) dropsing ro max(1000000) genD(FE)
outreg2 using robust.xls, excel keep(intrapr_t interpr_t) append ctitle(GDP_FE)

drop if FE==.
replace FE=ln(FE)


*Accomodation of some of the independent variables
gen ROW_i=0
replace ROW_i=1 if i=="ROW"
gen ROW_j=0
replace ROW_j=1 if j=="ROW"

*Gravity variables estimation
ppmlhdfe E_mod intrapr_t interpr_t lndist* contig_r contig_c, absorb(HB=intra#i2  year#i2 year#j2 ROW_i#j2 ROW_j#i2) cluster (i2#j2)
outreg2 using robust.xls, excel keep(intrapr_t interpr_t lndist* contig_r contig_c) append ctitle(GDP_dist10)


*gravity trade cost
gen grav=(_b[lndist1]*lndist1+_b[lndist2]*lndist2+_b[lndist3]*lndist3+_b[lndist4]*lndist4+_b[lndist5]*lndist5+_b[lndist6]*lndist6+_b[lndist7]*lndist7+_b[lndist8]*lndist8+_b[lndist9]*lndist9+_b[lndist10]*lndist10+_b[contig_r]*contig_r+_b[contig_c]*contig_c)


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
save intraj5, replace
restore
*Merge of the home bias
merge m:1 j using intraj5
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
bootstrap, reps(50): reg FE grav intrai intraj if inter_reg==1
outreg2 using UTB.xls, excel append ctitle(UTB_7)

*Final computation of the UTB
gen lnUTB=_b[_cons]+((_b[grav]-1)*grav)+(_b[intrai]+_b[intrai]/(_b[intrai]+_b[intraj]))*intrai+(_b[intraj]+_b[intraj]/(_b[intrai]+_b[intraj]))*intraj if inter_reg==1
gen UTB=exp(lnUTB)

keep year i j UTB lnUTB
rename UTB UTB5
rename lnUTB lnUTB5

save UTB_results_europe5.dta, replace













**** UTB alternative dep. var. with quantiles in distance **** 


clear all
use Data.dta

bys year i: egen te=total(ee2)
bys year j: egen ti=total(ee2)
bys year: egen tt=total(ee2)
gen E_mod=(ee2*tt)/(te*ti)
replace E_mod=0 if E_mod==.

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

replace distance=. if intra==1
replace distance=. if i=="ROW" | j=="ROW"

*Accomodation of distance variables
forv i=25(25)75{
	egen pctile`i'= pctile(distance) , p(`i')
}

gen distance_q=irecode(distance, 0, pctile25, pctile50, pctile75)

gen lndist1=ln(distance+1) if distance_q==1
gen lndist2=ln(distance+1) if distance_q==2
gen lndist3=ln(distance+1) if distance_q==3
gen lndist4=ln(distance+1) if distance_q==4

replace lndist1=0 if lndist1==.
replace lndist2=0 if lndist2==.
replace lndist3=0 if lndist3==.
replace lndist4=0 if lndist4==.
drop lndist


*PAIR FE GRAVITY
ppml_panel_sg E_mod intrapr_t interpr_t, ex(i2) im(j2) y(year) dropsing ro max(1000000) genD(FE)
outreg2 using robust.xls, excel keep(intrapr_t interpr_t) append ctitle(GDP_FE)

drop if FE==.
replace FE=ln(FE)


*Accomodation of some of the independent variables
gen ROW_i=0
replace ROW_i=1 if i=="ROW"
gen ROW_j=0
replace ROW_j=1 if j=="ROW"

*Gravity variables estimation
ppmlhdfe E_mod intrapr_t interpr_t lndist* contig_r contig_c, absorb(HB=intra#i2  year#i2 year#j2 ROW_i#j2 ROW_j#i2) cluster (i2#j2)
outreg2 using robust.xls, excel keep(intrapr_t interpr_t lndist* contig_r contig_c) append ctitle(GDP_dist4)


*gravity trade cost
gen grav=(_b[lndist1]*lndist1+_b[lndist2]*lndist2+_b[lndist3]*lndist3+_b[lndist4]*lndist4+_b[contig_r]*contig_r+_b[contig_c]*contig_c)


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
save intraj6, replace
restore
*Merge of the home bias
merge m:1 j using intraj6
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
bootstrap, reps(50): reg FE grav intrai intraj if inter_reg==1
outreg2 using UTB.xls, excel append ctitle(UTB_8)

*Final computation of the UTB
gen lnUTB=_b[_cons]+((_b[grav]-1)*grav)+(_b[intrai]+_b[intrai]/(_b[intrai]+_b[intraj]))*intrai+(_b[intraj]+_b[intraj]/(_b[intrai]+_b[intraj]))*intraj if inter_reg==1
gen UTB=exp(lnUTB)

keep year i j UTB lnUTB
rename UTB UTB6
rename lnUTB lnUTB6

save UTB_results_europe6.dta, replace







**************************************************
***   Baseline with symmetry         
**************************************************

**** UTB Baseline with deciles in distance **** 
clear all
use Data.dta

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

replace distance=. if intra==1
replace distance=. if i=="ROW" | j=="ROW"

*PAIR FE GRAVITY
ppml_panel_sg E_m intrapr_t interpr_t, ex(i2) im(j2) y(year) dropsing ro max(1000000) genD(FE) sym
outreg2 using robust.xls, excel keep(intrapr_t interpr_t) append ctitle(Sym_FE)

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
ppmlhdfe E_m intrapr_t interpr_t lndist contig_r contig_c, absorb(HB=intra#i2 year#i2 year#j2 ROW_i#j2 ROW_j#i2) cluster (i2#j2)
outreg2 using robust.xls, excel keep(intrapr_t interpr_t lndist contig_r contig_c) append ctitle(Sym_Grav)


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
save intraj7, replace
restore
*Merge of the home bias
merge m:1 j using intraj7
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
bootstrap, reps(50): reg FE grav intrai intraj if inter_reg==1
outreg2 using UTB.xls, excel append ctitle(UTB_10)

*Final computation of the UTB
gen lnUTB=_b[_cons]+((_b[grav]-1)*grav)+(_b[intrai]+_b[intrai]/(_b[intrai]+_b[intraj]))*intrai+(_b[intraj]+_b[intraj]/(_b[intrai]+_b[intraj]))*intraj if inter_reg==1
gen UTB=exp(lnUTB)

keep year i j UTB lnUTB
rename UTB UTB7
rename lnUTB lnUTB7

save UTB_results_europe7.dta, replace














**** Baseline with symmetry and deciles in distance **** 
clear all
use Data.dta

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

replace distance=. if intra==1
replace distance=. if i=="ROW" | j=="ROW"

*Accomodation of distance variables
forv i=10(10)90{
	egen pctile`i'= pctile(distance) , p(`i')
}

gen distance_q=irecode(distance, 0, pctile10, pctile20, pctile30, pctile40, pctile50, pctile60, pctile70, pctile80, pctile90)

gen lndist1=ln(distance+1) if distance_q==1
gen lndist2=ln(distance+1) if distance_q==2
gen lndist3=ln(distance+1) if distance_q==3
gen lndist4=ln(distance+1) if distance_q==4
gen lndist5=ln(distance+1) if distance_q==5
gen lndist6=ln(distance+1) if distance_q==6
gen lndist7=ln(distance+1) if distance_q==7
gen lndist8=ln(distance+1) if distance_q==8
gen lndist9=ln(distance+1) if distance_q==9
gen lndist10=ln(distance+1) if distance_q==10

replace lndist1=0 if lndist1==.
replace lndist2=0 if lndist2==.
replace lndist3=0 if lndist3==.
replace lndist4=0 if lndist4==.
replace lndist5=0 if lndist5==.
replace lndist6=0 if lndist6==.
replace lndist7=0 if lndist7==.
replace lndist8=0 if lndist8==.
replace lndist9=0 if lndist9==.
replace lndist10=0 if lndist10==.
drop lndist


*PAIR FE GRAVITY
ppml_panel_sg E_m intrapr_t interpr_t, ex(i2) im(j2) y(year) dropsing ro max(1000000) genD(FE) sym
outreg2 using robust.xls, excel keep(intrapr_t interpr_t) append ctitle(Sym_FE)

drop if FE==.
replace FE=ln(FE)


*Accomodation of some of the independent variables
gen ROW_i=0
replace ROW_i=1 if i=="ROW"
gen ROW_j=0
replace ROW_j=1 if j=="ROW"

*Gravity variables estimation
ppmlhdfe E_m intrapr_t interpr_t lndist* contig_r contig_c, absorb(HB=intra#i2  year#i2 year#j2 ROW_i#j2 ROW_j#i2) cluster (i2#j2)
outreg2 using robust.xls, excel keep(intrapr_t interpr_t lndist* contig_r contig_c) append ctitle(Sym_dist10)


*gravity trade cost
gen grav=(_b[lndist1]*lndist1+_b[lndist2]*lndist2+_b[lndist3]*lndist3+_b[lndist4]*lndist4+_b[lndist5]*lndist5+_b[lndist6]*lndist6+_b[lndist7]*lndist7+_b[lndist8]*lndist8+_b[lndist9]*lndist9+_b[lndist10]*lndist10+_b[contig_r]*contig_r+_b[contig_c]*contig_c)


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
save intraj8, replace
restore
*Merge of the home bias
merge m:1 j using intraj8
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
bootstrap, reps(50): reg FE grav intrai intraj if inter_reg==1
outreg2 using UTB.xls, excel append ctitle(UTB_11)

*Final computation of the UTB
gen lnUTB=_b[_cons]+((_b[grav]-1)*grav)+(_b[intrai]+_b[intrai]/(_b[intrai]+_b[intraj]))*intrai+(_b[intraj]+_b[intraj]/(_b[intrai]+_b[intraj]))*intraj if inter_reg==1
gen UTB=exp(lnUTB)

keep year i j UTB lnUTB
rename UTB UTB8
rename lnUTB lnUTB8

save UTB_results_europe8.dta, replace













**** Baseline with symmetry and quantiles in distance **** 


clear all
use Data.dta

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

replace distance=. if intra==1
replace distance=. if i=="ROW" | j=="ROW"

*Accomodation of distance variables
forv i=25(25)75{
	egen pctile`i'= pctile(distance) , p(`i')
}

gen distance_q=irecode(distance, 0, pctile25, pctile50, pctile75)

gen lndist1=ln(distance+1) if distance_q==1
gen lndist2=ln(distance+1) if distance_q==2
gen lndist3=ln(distance+1) if distance_q==3
gen lndist4=ln(distance+1) if distance_q==4

replace lndist1=0 if lndist1==.
replace lndist2=0 if lndist2==.
replace lndist3=0 if lndist3==.
replace lndist4=0 if lndist4==.
drop lndist


*PAIR FE GRAVITY
ppml_panel_sg E_m intrapr_t interpr_t, ex(i2) im(j2) y(year) dropsing ro max(1000000) genD(FE) sym
outreg2 using robust.xls, excel keep(intrapr_t interpr_t lndist* contig_r contig_c) append ctitle(Sym_FE)

drop if FE==.
replace FE=ln(FE)


*Accomodation of some of the independent variables
gen ROW_i=0
replace ROW_i=1 if i=="ROW"
gen ROW_j=0
replace ROW_j=1 if j=="ROW"

*Gravity variables estimation
ppmlhdfe E_m intrapr_t interpr_t lndist* contig_r contig_c, absorb(HB=intra#i2  year#i2 year#j2 ROW_i#j2 ROW_j#i2) cluster (i2#j2)
outreg2 using robust.xls, excel keep(intrapr_t interpr_t lndist* contig_r contig_c) append ctitle(Sym_dist4)



*gravity trade cost
gen grav=(_b[lndist1]*lndist1+_b[lndist2]*lndist2+_b[lndist3]*lndist3+_b[lndist4]*lndist4+_b[contig_r]*contig_r+_b[contig_c]*contig_c)


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
save intraj9, replace
restore
*Merge of the home bias
merge m:1 j using intraj9
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
bootstrap, reps(50): reg FE grav intrai intraj if inter_reg==1
outreg2 using UTB.xls, excel append ctitle(UTB_12)

*Final computation of the UTB
gen lnUTB=_b[_cons]+((_b[grav]-1)*grav)+(_b[intrai]+_b[intrai]/(_b[intrai]+_b[intraj]))*intrai+(_b[intraj]+_b[intraj]/(_b[intrai]+_b[intraj]))*intraj if inter_reg==1
gen UTB=exp(lnUTB)

keep year i j UTB lnUTB
rename UTB UTB9
rename lnUTB lnUTB9

save UTB_results_europe9.dta, replace





*Merge of all data

clear all
use UTB_results_europe.dta
forvalues i=2/9{
	merge 1:1 i j using UTB_results_europe`i'
	drop _merge
}
corr lnUTB*
save UTB_results_all, replace
