clear all
set more off
set linesize 255

*=====================================================================
* 稳健性检验 1：替换核心解释变量（剔除自己后计算班级比例）
*=====================================================================

*====================
* 1. 路径设置
*====================
cd "D:\导师\残疾伙伴与父母关系\Newceps"

*====================
* 2. 导入并合并数据
*====================
use "CEPS基线调查学生数据.dta", clear
merge 1:1 ids using "CEPS基线调查家长数据.dta"
keep if _merge == 3
drop _merge

merge m:1 schids clsids using "CEPS基线调查班级数据.dta"
keep if _merge == 3
drop _merge

merge m:1 schids using "CEPS基线调查学校数据.dta"
keep if _merge == 3
drop _merge

*====================
* 3. 筛选随机分班样本
*====================
keep if ple1503 == 1
keep if ple16 == 2

gen teacher_tracking = (hra05 == 1 | chna05 == 1 | mata05 == 1 | enga05 == 1)
bysort schids grade9: egen has_score_class = max(teacher_tracking)
keep if has_score_class == 0

*====================
* 4. 被解释变量：个人亲社会（C17）
*====================

* 4.1 同伴亲和性
rename c1706 peer_friendly
rename c1707 peer_easygoing
gen peer_affinity = peer_friendly + peer_easygoing

* 4.2 集体参与
rename c1709 participate
gen collective_participation = participate

* 4.3 自身行为规范（C17反向）
rename c1701 late
rename c1702 skip_class
rename c1703 teacher_criticism

foreach v in late skip_class teacher_criticism {
    gen `v'_r = 5 - `v'
}
egen self_discipline = rowtotal(late_r skip_class_r teacher_criticism_r)

* 4.4 PCA 纯个人亲社会指数
egen peer_affinity_z = std(peer_affinity)
egen collective_participation_z = std(collective_participation)
egen self_discipline_z = std(self_discipline)

pca peer_affinity_z collective_participation_z self_discipline_z
predict prosocial_raw, score
egen prosocial = std(prosocial_raw)

corr prosocial peer_affinity_z
if r(rho) < 0 {
    replace prosocial = -prosocial
}

*====================
* 5. 核心：剔除自己的班级高学历占比（leave-out mean）
*====================
replace stprhedu = . if stprhedu <1 | stprhedu >9
gen high_edu = (stprhedu >=7) if !missing(stprhedu)

bysort schids clsids: egen total_high = total(high_edu)
bysort schids clsids: egen n = count(high_edu)
gen college_ratio_ex = (total_high - high_edu) / (n - 1)
label var college_ratio_ex "剔除自身后的班级父母高学历占比"

*====================
* 6. 控制变量
*====================
* 学生层面
gen male = (stsex == 1)
gen age = 2013 - a02a
gen rural_hukou = (sthktype == 1)
gen only_child = (stonly == 1)
gen boarding = (stbrd == 1)

* 家庭层面
gen parent_edu = stprhedu
gen parent_age = be01
gen parent_health = (be09 >=4)
gen party_member = (be06 == 1)

* 家庭经济（ steco_5c）
gen family_econ = steco_5c
tab family_econ, gen(family_econ_)
drop family_econ_3

* 班级层面（稳健性检验必须加）
gen class_size = clsn
gen teacher_exp = hrc07
gen teacher_title = hrc12
gen normal_college = (hrc05 == 1)
gen public_position = (hrc11 == 1)

* 固定效应
egen school_grade = group(schids grade9)

* 控制项全局宏
global controls ///
age male rural_hukou only_child boarding ///
parent_edu parent_age parent_health party_member ///
family_econ_1 family_econ_2 family_econ_4 family_econ_5 ///
class_size teacher_exp teacher_title normal_college public_position

*====================
* 7. 稳健性回归
*====================
est clear

* Col1 同伴亲和性
oprobit peer_affinity college_ratio_ex $controls i.school_grade, vce(cluster clsids)
est store r1

* Col2 集体参与
oprobit collective_participation college_ratio_ex $controls i.school_grade, vce(cluster clsids)
est store r2

* Col3 自身行为规范
oprobit self_discipline college_ratio_ex $controls i.school_grade, vce(cluster clsids)
est store r3

* Col4 亲社会综合指数
reghdfe prosocial college_ratio_ex $controls, absorb(school_grade) cluster(clsids)
est store r4

*====================
* 8. 输出表格
*====================
esttab r1 r2 r3 r4 using "Table4_Robust1_ExcludeSelf.rtf", replace ///
    b(%6.3f) se(%6.3f) star(* 0.1 ** 0.05 *** 0.01) ///
    keep(college_ratio_ex) ///
    scalars(N) ///
    mtitles("Peer" "Collective" "Discipline" "Prosocial") ///
    title("Table 4: Robustness (Exclude Self from Class Ratio)") ///
    addnotes("Col1-3: Ordered Probit; Col4: OLS" ///
             "Controls & School-Grade FE included" ///
             "SE clustered at class level") ///
    nocons nonumbers compress nogaps

display "稳健性检验1 已完成！表格已保存！"