*====================================================================
****《绿色低碳发展目标下财政政策促进企业转型升级研究——来自"节能减排财政政策综合示范城市"试点的证据》，财政研究，2022-10
****作者：田淑英 孙磊 许文立 范子英
****单位：安徽大学经济学院
****版本：Stata 17
****数据来源：CSMAR数据库-上市公司数据板块.
****引用格式：田淑英,孙磊,许文立,范子英.绿色低碳发展目标下财政政策促进企业转型升级研究——来自"节能减排财政政策综合示范城市"试点的证据[J].财政研究,2022(08):79-96.
*====================================================================

use data.dta ,clear

****************预处理****************
//预处理//
destring scode l m, replace force
drop if province == "开曼群岛"
egen city_=group(city)
egen tclas_=group(tclas)
gen treat_1=1 if city=="北京市" | city=="深圳市" | city=="重庆市" | city=="杭州市" | city=="长沙市" | city=="贵阳市" | city=="吉林市" | city=="新余市"
replace treat_1=0 if treat_1==.
gen treat_2=1 if city=="石家庄市" | city=="唐山市" | city=="铁岭市" | city=="齐齐哈尔市" | city=="铜陵市" | city=="南平市" | city=="荆门市" | city=="韶关市" | city=="东莞市" | city=="铜川市"
replace treat_2=0 if treat_2==.
gen treat_3=1 if city=="天津市" | city=="临汾市" | city=="包头市" | city=="徐州市" | city=="聊城市" | city=="鹤壁市" | city=="梅州市" | city=="南宁市" | city=="德阳市" | city=="兰州市" | city=="乌鲁木齐市" | city=="海东市"
replace treat_3=0 if treat_3==.
gen post_1=1 if year<=2014 & year>=2012
replace post_1=0 if post_1==.
gen post_2=1 if  year<=2016 & year>=2014
replace post_2=0 if post_2==.
gen post_3=1 if  year<=2017 & year>=2015
replace post_3=0 if post_3==.
gen DID_1=treat_1*post_1
gen DID_2=treat_2*post_2
gen DID_3=treat_3*post_3
gen treat=1 if treat_1 ==1 | treat_2 ==1 | treat_3 ==1
replace treat=0 if treat==.
gen post=1 if post_1 ==1 | post_2 ==1 | post_3 ==1
replace post=0 if post==.
gen Policy=1 if DID_1 ==1 | DID_2 ==1 | DID_3 ==1
replace Policy=0 if Policy==.

//选择样本区间//
drop if year<2006 | year>2017

//剔除部分数据//
drop if stock =="#N/A"
drop if stock ==""
drop if tclas=="J" 
drop if strpos(stock ,"ST")>0
drop if strpos(stock ,"PT")>0
drop if city =="海东市"

xtset scode year

//TFP测算//
*变量定义
gen lny=log(y)
gen lnk=log(k)
gen lnl=log(l)
gen lnm=log(m)
*温莎处理
winsor2 lny lnl lnk lnm, cut (1 99) replace by(year)
*LP
levpet lny, free(lnl) proxy(lnm) capital(lnk) i(scode) t(year) revenue reps(5)
est store LP_all
gen TFP_LP=lny-_b[lnl]*lnl-_b[lnk]*lnk
label variable TFP_LP "LP法估计的TFP"
*ACF
acfest lny, free(lnl) state(lnk) proxy(lnm) i(scode) t(year) nbs(5)
**需手动计算TFP**
gen TFP_ACF=lny-0.1420475*lnl-0.37143*lnk
label variable TFP_ACF "ACF法估计的TFP"

//控制变量设定//
gen PROFIT=log(gprofit+1)
gen RD=prd
gen PRD=rd/nrd/10000
gen UTILITY=log(patent_uti+1)
gen INVENTION=log(patent_inv+1)
gen LogSIZE=size
gen LogSD=log(sd)
gen LogROA=sign( roa )*log(abs( roa )+1)
gen LogPPE=log(ppe+1)
gen LogTOBIN=log(tobin+1)
gen LogBM=log(bm+1)
gen LogPRA=log(practitioners)
gen LogISTR=log(istr+1)
gen LogPGDP=log(pgdp)
gen LogFEXP=log(fexp)
gen LogFDI=log(fdi)

//温莎处理//
winsor2 UTILITY INVENTION PROFIT RD PRD LogSIZE LogSD LogROA LogPPE LogTOBIN LogBM LogPRA LogISTR LogPGDP LogFEXP LogFDI, cut (1 99) replace by(year)

glob Xs LogSIZE LogSD LogROA LogPPE LogTOBIN LogBM LogPRA LogISTR LogPGDP LogFEXP LogFDI

save yuchulidata.dta,replace


****************基准回归****************

use yuchulidata.dta ,clear

//选择样本区间//   ///参考吴文值等（2022）的做法
drop if (city=="北京市" | city=="深圳市" | city=="重庆市" | city=="杭州市" | city=="长沙市" | city=="贵阳市" | city=="吉林市" | city=="新余市") & (year==2015 | year==2016 | year==2017)
drop if (city=="石家庄市" | city=="唐山市" | city=="铁岭市" | city=="齐齐哈尔市" | city=="铜陵市" | city=="南平市" | city=="荆门市" | city=="韶关市" | city=="东莞市" | city=="铜川市") &  (year==2017)

//描述性统计//
sum2docx TFP_LP TFP_ACF UTILITY INVENTION PROFIT Policy $Xs using summary.docx ,replace stats(N mean(%9.3f) sd(%9.3f) min(%9.3f) median(%9.3f) max(%9.3f))

//基准回归//
reghdfe TFP_LP Policy ,absorb(i.scode i.year) vce(cluster tcode)
reghdfe TFP_LP Policy $Xs ,absorb(i.scode i.year) vce(cluster tcode)
reghdfe TFP_ACF Policy ,absorb(i.scode i.year) vce(cluster tcode)
reghdfe TFP_ACF Policy $Xs ,absorb(i.scode i.year) vce(cluster tcode)
reghdfe UTILITY Policy ,absorb(i.scode i.year) vce(cluster tcode)
reghdfe UTILITY Policy $Xs ,absorb(i.scode i.year) vce(cluster tcode)
reghdfe INVENTION Policy ,absorb(i.scode i.year) vce(cluster tcode)
reghdfe INVENTION Policy $Xs ,absorb(i.scode i.year) vce(cluster tcode)
reghdfe PROFIT Policy ,absorb(i.scode i.year) vce(cluster tcode)
reghdfe PROFIT Policy $Xs ,absorb(i.scode i.year) vce(cluster tcode)

save baseregdata.dta,replace


**************平行趋势检验**********************

use baseregdata.dta,clear

//平行趋势检验//      ///可将TFP_LP替换为TFP_ACF、UTILITY、PROFIT
gen reform=2012 if (city =="北京市" | city=="深圳市" | city=="重庆市" | city =="杭州市" | city =="长沙市" | city =="贵阳市" | city=="吉林市" | city =="新余市") 
replace reform=2014 if (city =="石家庄市" | city =="唐山市" | city =="铁岭市" | city =="齐齐哈尔市" | city =="铜陵市" | city =="南平市" | city =="荆门市" | city =="韶关市" | city =="东莞市" | city =="铜川市") 
replace reform=2015 if (city =="天津市" | city =="临汾市" | city =="包头市" | city =="徐州市" | city =="聊城市" | city =="鹤壁市" | city =="梅州市" | city =="南宁市" | city=="德阳市" | city =="兰州市" | city =="海东市" | city =="乌鲁木齐市")

gen tDID= year- reform
replace tDID=-6 if tDID<-6
replace tDID=2 if tDID>2

tab tDID,missing

forvalues i=1/6{
gen d_`i' =0
replace d_`i' = 1 if treat == 1 & tDID == -`i'
}

forvalues i=0/2{
gen d`i' =0
replace d`i' = 1 if treat == 1 & tDID == `i'
}

order d_6 d_5 d_4 d_3 d_2 d_1 d0 d1 d2
reghdfe TFP_LP d_6-d_2 d0-d2 $Xs ,absorb(i.scode i.year) vce(cluster tcode) 

gen t=invttail(19,0.05)

*生成b_j的系数和置信区间
forvalues i=2/6 { 
gen b_`i' = _b[d_`i'] 
gen se_b_`i' = _se[d_`i']
gen b_`i'LB = b_`i' - t * se_b_`i'
gen b_`i'UB = b_`i' + t * se_b_`i'
}

*生成bj的系数与置信区间
forvalues i=0/2 { 
gen b`i' = _b[d`i'] 
gen se_b`i' = _se[d`i']
gen b`i'LB = b`i' - t * se_b`i'
gen b`i'UB = b`i' + t * se_b`i'
}

gen b = .
gen LB = .
gen UB = .

*生成系数（政策前）
forvalues i=2/6 {
replace b = b_`i'  if tDID == -`i'
}

*生成系数（政策后）
forvalues i=0/2{
replace b = b`i'  if tDID == `i'
}

*生成系数置信区间下限（政策前）
forvalues i=2/6 {
replace LB = b_`i'LB if tDID == -`i'
}

*生成系数置信区间下限（政策后）
forvalues i=0/2 {
replace LB = b`i'LB if tDID == `i'
}

*生成系数置信区间上限（政策前）
forvalues i=2/6 {
replace UB = b_`i'UB if tDID == -`i'
}

*生成系数置信区间上限（政策后）
forvalues i=0/2 {
replace UB = b`i'UB if tDID == `i'
}

*生成基期
replace b = 0  if tDID == -1
replace LB = 0 if tDID == -1
replace UB = 0 if tDID == -1

keep tDID b LB UB
duplicates drop tDID,force
sort tDID

twoway (connected b tDID, sort lcolor(black) mcolor(black) msymbol(circle_hollow) cmissing(n))(rcap LB UB tDID, lcolor(black)lpattern(dash) msize(medium)),ytitle("政策动态效应",size(medium)) yline(0, lwidth(vthin) lpattern(dash) lcolor(teal)) ylabel(-0.12 "-0.12" -0.10 "-0.10" -0.08 "-0.08" -0.06 "-0.06" -0.04 "-0.04" -0.02 "-0.02" 0 "0.00" 0.02 "0.02" 0.04 "0.04" 0.06 "0.06" 0.08 "0.08" 0.1 "0.10" 0.12 "0.12", labsize(medium) angle(horizontal) nogrid) xtitle("政策时点",size(medium)) xline(0, lwidth(vthin) lpattern(dash) lcolor(teal))  xlabel(-6 "pre_6" -5 "pre_5" -4 "pre_4" -3 "pre_3" -2 "pre_2" -1 "pre_1" 0 "current" 1 "post_1" 2 "post_2", labsize(medium))  legend(off)graphregion(fcolor(white) lcolor(white) ifcolor(white) ilcolor(white)) scheme(s1mono) title("（{it:TFP_LP}）")



**************安慰剂检验**********************

*TFP_LP 安慰剂检验     ///可将TFP_LP替换为TFP_ACF、UTILITY、PROFIT
use baseregdata.dta,clear
permute Policy beta=_b[Policy] se=_se[Policy] df_m=e(df_r),reps(500) seed(123) strata(scode) saving("anweiji.dta",replace) : reghdfe TFP_LP Policy $Xs ,absorb(i.scode i.year) vce(cluster tcode)

use "anweiji.dta",clear
gen t_value=beta/se
gen p_value=2*ttail(df,abs(beta/se))

**系数和p值结合
twoway (kdensity beta, msymbol(circle_hollow))(scatter p_value beta, yaxis(2)), xline(0.056, lc(black*0.5) lp(dash)) xline(0, lc(black*0.5) lp(solid)) yline(0.1, lc(black*0.5) lp(dash) axis(2)) xtitle("系数值", size(*0.8)) xlabel(-0.03 "-0.03" -0.02 "-0.02" -0.01 "-0.01" 0 "0.00" 0.01 "0.01" 0.02 "0.02" 0.03 "0.03" 0.04 "0.04" 0.05 "0.05" 0.06 "0.06",grid format(%4.1f) labsize(small)) ytitle("核密度", size(*0.8)) ylabel(, nogrid format(%4.1f) labsize(small)) ytitle("P值", size(*0.8) axis(2)) ylabel(,grid nogrid format(%4.1f) labsize(small) axis(2)) legend(r(1) order(1 "核密度分布" 2 "P值")) graphregion(color(white)) title("（{it:TFP_LP}）")



**************异质性处理效应检验**********************

***DIDM(基准回归样本)    ///可将TFP_LP替换为TFP_ACF、UTILITY、PROFIT
use baseregdata.dta,clear
twowayfeweights TFP_LP scode year Policy,type(feTR) controls($Xs)
did_multiplegt TFP_LP scode year Policy,robust_dynamic dynamic(3) placebo(6) breps(50) average_effect controls($Xs)
ereturn list

***DIDM(2006—2017年样本)   ///可将TFP_LP替换为TFP_ACF、UTILITY、PROFIT
use yuchulidata.dta,clear
twowayfeweights TFP_LP scode year Policy,type(feTR) controls($Xs)
did_multiplegt TFP_LP scode year Policy,robust_dynamic dynamic(3) placebo(6) breps(50) average_effect controls($Xs)
ereturn list
event_plot e(estimates)#e(variances), default_look graph_opt(xtitle("政策时点") ytitle("动态处理效应",size(medium)) title("（{it:TFP_LP}）") xlabel(-6 "pre_6" -5 "pre_5" -4 "pre_4" -3 "pre_3" -2 "pre_2" -1 "pre_1" 0 "current" 1 "post_1" 2 "post_2" 3 "post_3")) stub_lag(Effect_#) stub_lead(Placebo_#) together

***DIDM(2006—2019年样本)   ///可将TFP_LP替换为TFP_ACF、UTILITY、PROFIT
use data.dta ,clear

//预处理//
destring scode l m, replace force
drop if province == "开曼群岛"
egen city_=group(city)
egen tclas_=group(tclas)
gen treat_1=1 if city=="北京市" | city=="深圳市" | city=="重庆市" | city=="杭州市" | city=="长沙市" | city=="贵阳市" | city=="吉林市" | city=="新余市"
replace treat_1=0 if treat_1==.
gen treat_2=1 if city=="石家庄市" | city=="唐山市" | city=="铁岭市" | city=="齐齐哈尔市" | city=="铜陵市" | city=="南平市" | city=="荆门市" | city=="韶关市" | city=="东莞市" | city=="铜川市"
replace treat_2=0 if treat_2==.
gen treat_3=1 if city=="天津市" | city=="临汾市" | city=="包头市" | city=="徐州市" | city=="聊城市" | city=="鹤壁市" | city=="梅州市" | city=="南宁市" | city=="德阳市" | city=="兰州市" | city=="乌鲁木齐市" | city=="海东市"
replace treat_3=0 if treat_3==.
gen post_1=1 if year<=2014 & year>=2012
replace post_1=0 if post_1==.
gen post_2=1 if  year<=2016 & year>=2014
replace post_2=0 if post_2==.
gen post_3=1 if  year<=2017 & year>=2015
replace post_3=0 if post_3==.
gen DID_1=treat_1*post_1
gen DID_2=treat_2*post_2
gen DID_3=treat_3*post_3
gen treat=1 if treat_1 ==1 | treat_2 ==1 | treat_3 ==1
replace treat=0 if treat==.
gen post=1 if post_1 ==1 | post_2 ==1 | post_3 ==1
replace post=0 if post==.
gen Policy=1 if DID_1 ==1 | DID_2 ==1 | DID_3 ==1
replace Policy=0 if Policy==.

//选择样本区间//
drop if year<2006 | year>2019

//剔除部分样本//
drop if stock =="#N/A"
drop if stock ==""
drop if tclas=="J" 
drop if strpos(stock ,"ST")>0
drop if strpos(stock ,"PT")>0
drop if city =="海东市"

xtset scode year

//TFP测算//
*变量定义
gen lny=log(y)
gen lnk=log(k)
gen lnl=log(l)
gen lnm=log(m)
*温莎处理
winsor2 lny lnl lnk lnm, cut (1 99) replace by(year)
*LP
levpet lny, free(lnl) proxy(lnm) capital(lnk) i(scode) t(year) revenue reps(5)
est store LP_all
gen TFP_LP=lny-_b[lnl]*lnl-_b[lnk]*lnk
label variable TFP_LP "LP法估计的TFP"
*ACF
acfest lny, free(lnl) state(lnk) proxy(lnm) i(scode) t(year) nbs(5)
******需手动计算TFP******
gen TFP_ACF=lny-0.1963548*lnl-0.3076319*lnk
label variable TFP_ACF "ACF法估计的TFP"

//控制变量设定//
gen PROFIT=log(gprofit+1)
gen RD=prd
gen PRD=rd/nrd/10000
gen UTILITY=log(patent_uti+1)
gen INVENTION=log(patent_inv+1)
gen LogSIZE=size
gen LogSD=log(sd)
gen LogROA=sign( roa )*log(abs( roa )+1)
gen LogPPE=log(ppe+1)
gen LogTOBIN=log(tobin+1)
gen LogBM=log(bm+1)
gen LogPRA=log(practitioners)
gen LogISTR=log(istr+1)
gen LogPGDP=log(pgdp)
gen LogFEXP=log(fexp)
gen LogFDI=log(fdi)

//温莎处理//
winsor2 UTILITY INVENTION PROFIT RD PRD LogSIZE LogSD LogROA LogPPE LogTOBIN LogBM LogPRA LogISTR LogPGDP LogFEXP LogFDI, cut (1 99) replace by(year)

glob Xs LogSIZE LogSD LogROA LogPPE LogTOBIN LogBM LogPRA LogISTR LogPGDP LogFEXP LogFDI

twowayfeweights TFP_LP scode year Policy,type(feTR) controls($Xs)
did_multiplegt TFP_LP scode year Policy,robust_dynamic dynamic(3) placebo(6) breps(50) average_effect controls($Xs)
ereturn list
event_plot e(estimates)#e(variances), default_look graph_opt(xtitle("政策时点") ytitle("动态处理效应",size(medium)) title("（{it:TFP_LP}）") xlabel(-6 "pre_6" -5 "pre_5" -4 "pre_4" -3 "pre_3" -2 "pre_2" -1 "pre_1" 0 "current" 1 "post_1" 2 "post_2" 3 "post_3")) stub_lag(Effect_#) stub_lead(Placebo_#) together



*****************机制检验******************

use baseregdata.dta,clear

*财政激励的“资源配置效应”  
**内部融资约束
gen GSUB=gsub/100000000
reghdfe GSUB Policy $Xs ,absorb(i.scode i.year) vce(cluster tcode)
gen CCF=ccf_a/100000000
reghdfe CCF Policy $Xs ,absorb(i.scode i.year) vce(cluster tcode)

**外部融资约束KZ指数
destring cfa diva casha lev tobin yage k, replace force
winsor2 cfa diva casha lev tobin yage k, cuts(1 99) replace by(year)
egen MED_CFA=median(cfa),by(year)
gen NCFA=1 if cfa<MED_CFA
replace NCFA=0 if NCFA==.
egen MED_DIVA=median(diva),by(year)
gen NDIVA=1 if diva<MED_DIVA
replace NDIVA=0 if NDIVA==.
egen MED_CASHA=median(casha),by(year)
gen NCASHA=1 if casha<MED_CASHA
replace NCASHA=0 if NCASHA==.
egen MED_LEV=median(lev),by(year)
gen NLEV=1 if lev>MED_LEV
replace NLEV=0 if NLEV==.
egen MED_TOBIN=median(tobin),by(year)
gen NTOBIN=1 if tobin>MED_TOBIN
replace NTOBIN=0 if NTOBIN==.
gen NKZ=NCFA+NDIVA+NCASHA+NLEV+NTOBIN
ologit NKZ cfa diva casha lev tobin
est store reg
predict KZ, xb
esttab reg , nogap replace star(* 0.1 ** 0.05 *** 0.01) b(3) t(3) pr2    
reghdfe KZ Policy $Xs ,absorb(i.scode i.year) vce(cluster tcode)

gen EMD=emd
reghdfe EMD Policy $Xs ,absorb(i.scode i.year) vce(cluster tcode)


*环境规制的“创新补偿效应”  
reghdfe RD Policy $Xs ,absorb(i.scode i.year) vce(cluster tcode)

gen DIGITAL=log(1+digital)
reghdfe DIGITAL Policy $Xs ,absorb(i.scode i.year) vce(cluster tcode)



***********进一步研究：异质性分析*************

use baseregdata.dta,clear

//地方政府财政压力(50分位数)// 
use baseregdata.dta,clear
gen median_fpress=1.0780284
reghdfe TFP_LP Policy $Xs if fpress>=median_fpress ,absorb(i.scode i.year) vce(cluster tcode)
reghdfe TFP_ACF Policy $Xs if fpress>=median_fpress ,absorb(i.scode i.year) vce(cluster tcode)
reghdfe UTILITY Policy $Xs if fpress>=median_fpress ,absorb(i.scode i.year) vce(cluster tcode)
reghdfe PROFIT Policy $Xs if fpress>=median_fpress ,absorb(i.scode i.year) vce(cluster tcode)
reghdfe TFP_LP Policy $Xs if fpress<median_fpress ,absorb(i.scode i.year) vce(cluster tcode)
reghdfe TFP_ACF Policy $Xs if fpress<median_fpress ,absorb(i.scode i.year) vce(cluster tcode)
reghdfe UTILITY Policy $Xs if fpress<median_fpress ,absorb(i.scode i.year) vce(cluster tcode)
reghdfe PROFIT Policy $Xs if fpress<median_fpress ,absorb(i.scode i.year) vce(cluster tcode)


//环保重视程度//
use baseregdata.dta,clear
gen median_ewf=0.0034035
reghdfe TFP_LP Policy $Xs if ewf>=median_ewf ,absorb(i.scode i.year) vce(cluster tcode)
reghdfe TFP_ACF Policy $Xs if ewf>=median_ewf ,absorb(i.scode i.year) vce(cluster tcode)
reghdfe UTILITY Policy $Xs if ewf>=median_ewf ,absorb(i.scode i.year) vce(cluster tcode)
reghdfe PROFIT Policy $Xs if ewf>=median_ewf ,absorb(i.scode i.year) vce(cluster tcode)
reghdfe TFP_LP Policy $Xs if ewf<median_ewf ,absorb(i.scode i.year) vce(cluster tcode)
reghdfe TFP_ACF Policy $Xs if ewf<median_ewf ,absorb(i.scode i.year) vce(cluster tcode)
reghdfe UTILITY Policy $Xs if ewf<median_ewf ,absorb(i.scode i.year) vce(cluster tcode)
reghdfe PROFIT Policy $Xs if ewf<median_ewf ,absorb(i.scode i.year) vce(cluster tcode)


//企业所有制//
use baseregdata.dta,clear
destring enid , replace force
gen SOE=1 if enid==1
reghdfe TFP_LP Policy $Xs if SOE==1 ,absorb(i.scode i.year) vce(cluster tcode)
reghdfe TFP_ACF Policy $Xs if SOE==1 ,absorb(i.scode i.year) vce(cluster tcode)
reghdfe UTILITY Policy $Xs if SOE==1 ,absorb(i.scode i.year) vce(cluster tcode)
reghdfe PROFIT Policy $Xs if SOE==1 ,absorb(i.scode i.year) vce(cluster tcode)
reghdfe TFP_LP Policy $Xs if SOE!=1 ,absorb(i.scode i.year) vce(cluster tcode)
reghdfe TFP_ACF Policy $Xs if SOE!=1 ,absorb(i.scode i.year) vce(cluster tcode)
reghdfe UTILITY Policy $Xs if SOE!=1 ,absorb(i.scode i.year) vce(cluster tcode)
reghdfe PROFIT Policy $Xs if SOE!=1 ,absorb(i.scode i.year) vce(cluster tcode)


//"三高"行业//
use baseregdata.dta,clear
*高耗能行业
gen Type_HEC=1 if (tcode=="D44" | tcode=="C25" | tcode=="C26" | tcode=="C32" | tcode=="C31" | tcode=="C30")
replace Type_HEC=0 if Type_HEC==.
*重污染行业
gen Type_HP=1 if (tcode=="B06" | tcode=="BO7" | tcode=="B08" | tcode=="B09" | tcode=="B11" | tcode=="B12" | tcode=="C17" | tcode=="C18" | tcode=="C19" | tcode=="C22" | tcode=="C25" | tcode=="C26" | tcode=="C27" | tcode=="C28" | tcode=="C29" | tcode=="C31" | tcode=="C32" | tcode=="D44")
replace Type_HP=0 if Type_HP==.
*高排放行业
gen Type_HE=1 if (tcode=="C25" | tcode=="C26" | tcode=="C30" | tcode=="C31" | tcode=="C32" | tcode=="C22" | tcode=="D44" | tcode=="G56")
replace Type_HE=0 if Type_HE==.
gen Type=1 if Type_HEC==1 | Type_HP==1 | Type_HE==1
replace Type=0 if Type==.
reghdfe TFP_LP Policy $Xs if Type==1 ,absorb(i.scode i.year) vce(cluster tcode)
reghdfe TFP_ACF Policy $Xs if Type==1 ,absorb(i.scode i.year) vce(cluster tcode)
reghdfe UTILITY Policy $Xs if Type==1 ,absorb(i.scode i.year) vce(cluster tcode)
reghdfe PROFIT Policy $Xs if Type==1 ,absorb(i.scode i.year) vce(cluster tcode)
reghdfe TFP_LP Policy $Xs if Type==0 ,absorb(i.scode i.year) vce(cluster tcode)
reghdfe TFP_ACF Policy $Xs if Type==0 ,absorb(i.scode i.year) vce(cluster tcode)
reghdfe UTILITY Policy $Xs if Type==0 ,absorb(i.scode i.year) vce(cluster tcode)
reghdfe PROFIT Policy $Xs if Type==0 ,absorb(i.scode i.year) vce(cluster tcode)