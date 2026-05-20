clear all
set more off
set linesize 255
cd "D:\导师\残疾伙伴与父母关系\Newceps"

********************************************************************************
* 异质性分析
********************************************************************************

* 1. 数据合并
use "CEPS基线调查学生数据.dta", clear
merge 1:1 ids using "CEPS基线调查家长数据.dta", nogen keep(3)
merge m:1 schids clsids using "CEPS基线调查班级数据.dta", nogen keep(3)
merge m:1 schids using "CEPS基线调查学校数据.dta", nogen keep(3)

* 2. 随机分班样本筛选
keep if ple1503 == 1
keep if ple16 == 2
gen teacher_tracking = (hra05==1 | chna05==1 | mata05==1 | enga05==1)
bysort schids grade9: egen has_score_class = max(teacher_tracking)
keep if has_score_class == 0

*=====================================================================
* 3. 被解释变量：纯个人亲社会（仅C17）
*=====================================================================
* 同伴亲和性
rename c1706 peer_friendly
rename c1707 peer_easygoing
gen peer_affinity = peer_friendly + peer_easygoing

* 集体参与
rename c1709 participate
gen collective_participation = participate

* 自身行为规范
rename c1701 late
rename c1702 skip_class
rename c1703 teacher_criticism
foreach v in late skip_class teacher_criticism {
    gen `v'_r = 5 - `v'
}
egen self_discipline = rowtotal(late_r skip_class_r teacher_criticism_r)

* PCA 综合指数
egen z1 = std(peer_affinity)
egen z2 = std(collective_participation)
egen z3 = std(self_discipline)
pca z1 z2 z3
predict prosocial_raw, score
egen prosocial = std(prosocial_raw)

corr prosocial z1
if r(rho) < 0 {
    replace prosocial = -prosocial
}

*=====================================================================
* 4. 核心解释变量
*=====================================================================
replace stprhedu = . if stprhedu <1 | stprhedu >9
gen high_edu = (stprhedu >=7)
bysort schids clsids: egen college_ratio = mean(high_edu)

*=====================================================================
* 5. 控制变量
*=====================================================================
* 学生
gen male = (stsex ==1)
gen age = 2013 - a02a
gen rural_hukou = (sthktype ==1)
gen only_child = (stonly ==1)
gen boarding = (stbrd ==1)

* 家庭
gen parent_edu = stprhedu
gen parent_age = be01
gen parent_health = (be09>=4)
gen party_member = (be06 ==1)

* 家庭经济：steco_5c
gen family_econ = steco_5c
tab family_econ, gen(family_econ_)
drop family_econ_3

* 班级全套控制
gen class_size = clsn
gen teacher_exp = hrc07
gen teacher_title = hrc12
gen normal_college = (hrc05 ==1)
gen public_position = (hrc11 ==1)

egen school_grade = group(schids grade9)

global controls ///
age male rural_hukou only_child boarding ///
parent_edu parent_age parent_health party_member ///
family_econ_1 family_econ_2 family_econ_4 family_econ_5 ///
class_size teacher_exp teacher_title normal_college public_position

*=====================================================================
* 6. 异质性分组变量
*=====================================================================
gen high_parent_edu = (parent_edu >=7) if !missing(parent_edu)
gen high_income = (steco_5c >=4) if !missing(steco_5c)  //

*=====================================================================
* 7. 异质性交互项回归
*=====================================================================
est clear
reghdfe prosocial c.college_ratio##i.male $controls, absorb(school_grade) cluster(clsids)
est store het_gender

reghdfe prosocial c.college_ratio##i.rural_hukou $controls, absorb(school_grade) cluster(clsids)
est store het_hukou

reghdfe prosocial c.college_ratio##i.high_parent_edu $controls, absorb(school_grade) cluster(clsids)
est store het_paredu

reghdfe prosocial c.college_ratio##i.high_income $controls, absorb(school_grade) cluster(clsids)
est store het_income

reghdfe prosocial c.college_ratio##i.only_child $controls, absorb(school_grade) cluster(clsids)
est store het_onlychild

*=====================================================================
* 8. 输出表格
*=====================================================================
esttab het_gender het_hukou het_paredu het_income het_onlychild ///
using "TableA5_Heterogeneity_Final.rtf", replace ///
b(%6.3f) se(%6.3f) star(* 0.1 ** 0.05 *** 0.01) ///
keep(*college_ratio*) scalars(N r2_a) ///
mtitles("Gender" "Hukou" "ParentEdu" "Income" "OnlyChild") ///
title("Table A5: Heterogeneity Analysis") ///
addnotes("Dependent: Prosocial (C17 only)") ///
nocons nonumbers compress nogaps

display "异质性分析 完成！"