clear all
set more off
set linesize 255
cd "D:\导师\残疾伙伴与父母关系\Newceps"

********************************************************************************
* Table 5：稳健性检验——重新定义班级父母学历（三种阈值）
********************************************************************************

*====================
* 1. 导入并合并数据
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
* 2. 筛选随机分班样本
*====================
keep if ple1503 == 1
keep if ple16 == 2

gen teacher_tracking = (hra05 == 1 | chna05 == 1 | mata05 == 1 | enga05 == 1)
bysort schids grade9: egen has_score_class = max(teacher_tracking)
keep if has_score_class == 0

*====================
* 3. 父母学历统一处理（缺失值标记）
*====================
replace stprhedu = . if stprhedu < 1 | stprhedu > 9

*====================
* 4. 被解释变量：个人亲社会行为（仅C17）
*====================

* 维度1: 同伴亲和性
rename c1706 peer_friendly
rename c1707 peer_easygoing
gen peer_affinity = peer_friendly + peer_easygoing

* 维度2: 学校归属感（原始总分）
rename c1708 class_atmosphere
rename c1709 participate
rename c1710 school_affinity

gen school_belong_raw = class_atmosphere + participate + school_affinity
label var school_belong_raw "School belonging (raw total score)"

* 维度3: 自身行为规范（C17反向编码）
rename c1701 late
rename c1702 skip_class
rename c1703 teacher_criticism
foreach v in late skip_class teacher_criticism {
    gen `v'_r = 5 - `v'
}
egen self_discipline = rowtotal(late_r skip_class_r teacher_criticism_r)

* PCA 纯个人亲社会综合指数
egen peer_affinity_z = std(peer_affinity)
egen school_belong_raw_z = std(school_belong_raw)
egen self_discipline_z = std(self_discipline)

pca peer_affinity_z school_belong_raw_z self_discipline_z
predict pca_score, score
egen prosocial = std(pca_score)

* 方向检验
corr prosocial peer_affinity_z
if r(rho) < 0 {
    replace prosocial = -prosocial
}

*====================
* 5. 三种核心解释变量（剔除自身）
*====================

*-------------------------------------------
* Panel A：大专及以上（≥7）
*-------------------------------------------
gen high_edu_A = (stprhedu >= 7) if !missing(stprhedu)
bysort schids clsids: egen sum_A = total(high_edu_A)
bysort schids clsids: egen n_A = count(high_edu_A)
gen ratio_A = (sum_A - high_edu_A) / (n_A - 1)
label var ratio_A "Panel A: 大专及以上（剔除自身）"

*-------------------------------------------
* Panel B：本科及以上（≥8）
*-------------------------------------------
gen high_edu_B = (stprhedu >= 8) if !missing(stprhedu)
bysort schids clsids: egen sum_B = total(high_edu_B)
bysort schids clsids: egen n_B = count(high_edu_B)
gen ratio_B = (sum_B - high_edu_B) / (n_B - 1)
label var ratio_B "Panel B: 本科及以上（剔除自身）"

*-------------------------------------------
* Panel C：仅普通高中（=6）
*-------------------------------------------
gen high_edu_C = (stprhedu == 6) if !missing(stprhedu)
bysort schids clsids: egen sum_C = total(high_edu_C)
bysort schids clsids: egen n_C = count(high_edu_C)
gen ratio_C = (sum_C - high_edu_C) / (n_C - 1)
label var ratio_C "Panel C: 仅高中（剔除自身）"

*====================
* 6. 控制变量（全部使用 if !missing() 确保缺失值为系统缺失 .）
*====================

* 学生层面
gen male = (stsex == 1) if !missing(stsex)
gen age = 2013 - a02a if !missing(a02a)
gen rural_hukou = (sthktype == 1) if !missing(sthktype)
gen only_child = (stonly == 1) if !missing(stonly)
gen boarding = (stbrd == 1) if !missing(stbrd)

* 家庭层面
gen parent_edu = stprhedu if !missing(stprhedu)
gen parent_age = be01 if !missing(be01)
gen parent_health = (be09 >= 4) if !missing(be09)
gen party_member = (be06 == 1) if !missing(be06)

* 家庭经济状况（steco_5c）
gen family_econ = steco_5c if !missing(steco_5c)
tab family_econ, gen(family_econ_)
drop family_econ_3

* 班级层面（稳健性检验保留）
gen class_size = clsn if !missing(clsn)
gen teacher_exp = hrc07 if !missing(hrc07)
gen teacher_title = hrc12 if !missing(hrc12)
gen normal_college = (hrc05 == 1) if !missing(hrc05)
gen public_position = (hrc11 == 1) if !missing(hrc11)

* 固定效应
egen school_grade = group(schids grade9)

* 控制项全局宏
global controls ///
    age male rural_hukou only_child boarding ///
    parent_edu parent_age parent_health party_member ///
    family_econ_1 family_econ_2 family_econ_4 family_econ_5 ///
    class_size teacher_exp teacher_title normal_college public_position

*====================
* 7. 回归与输出
*====================

* ---------- Panel A: 大专及以上 ----------
est clear
oprobit peer_affinity          ratio_A $controls i.school_grade, vce(cluster clsids)
est store A1
oprobit school_belong_raw      ratio_A $controls i.school_grade, vce(cluster clsids)
est store A2
oprobit self_discipline        ratio_A $controls i.school_grade, vce(cluster clsids)
est store A3
reghdfe prosocial              ratio_A $controls, absorb(school_grade) cluster(clsids)
est store A4

esttab A1 A2 A3 A4 using "Table5_PanelA_v2.rtf", replace ///
    b(%6.3f) se(%6.3f) star(* 0.10 ** 0.05 *** 0.01) ///
    keep(ratio_A) scalars(N) ///
    mtitles("Peer" "School Belong" "Discipline" "Prosocial") ///
    title("Table 5 Panel A: Associate Degree or Above") ///
    addnotes("Col1-3: Ordered Probit; Col4: OLS" ///
             "Controls: Student, Family & Class Characteristics" ///
             "School-Grade FE included, SE clustered at class level") ///
    nocons nonumbers compress nogaps

* ---------- Panel B: 本科及以上 ----------
est clear
oprobit peer_affinity          ratio_B $controls i.school_grade, vce(cluster clsids)
est store B1
oprobit school_belong_raw      ratio_B $controls i.school_grade, vce(cluster clsids)
est store B2
oprobit self_discipline        ratio_B $controls i.school_grade, vce(cluster clsids)
est store B3
reghdfe prosocial              ratio_B $controls, absorb(school_grade) cluster(clsids)
est store B4

esttab B1 B2 B3 B4 using "Table5_PanelB_v2.rtf", replace ///
    b(%6.3f) se(%6.3f) star(* 0.10 ** 0.05 *** 0.01) ///
    keep(ratio_B) scalars(N) ///
    mtitles("Peer" "School Belong" "Discipline" "Prosocial") ///
    title("Table 5 Panel B: Bachelor Degree or Above") ///
    addnotes("Col1-3: Ordered Probit; Col4: OLS" ///
             "Controls: Student, Family & Class Characteristics" ///
             "School-Grade FE included, SE clustered at class level") ///
    nocons nonumbers compress nogaps

* ---------- Panel C: 仅高中 ----------
est clear
oprobit peer_affinity          ratio_C $controls i.school_grade, vce(cluster clsids)
est store C1
oprobit school_belong_raw      ratio_C $controls i.school_grade, vce(cluster clsids)
est store C2
oprobit self_discipline        ratio_C $controls i.school_grade, vce(cluster clsids)
est store C3
reghdfe prosocial              ratio_C $controls, absorb(school_grade) cluster(clsids)
est store C4

esttab C1 C2 C3 C4 using "Table5_PanelC_v2.rtf", replace ///
    b(%6.3f) se(%6.3f) star(* 0.10 ** 0.05 *** 0.01) ///
    keep(ratio_C) scalars(N) ///
    mtitles("Peer" "School Belong" "Discipline" "Prosocial") ///
    title("Table 5 Panel C: High School Only") ///
    addnotes("Col1-3: Ordered Probit; Col4: OLS" ///
             "Controls: Student, Family & Class Characteristics" ///
             "School-Grade FE included, SE clustered at class level") ///
    nocons nonumbers compress nogaps

*====================
* 8. 完成提示
*====================
display ""
display "==================== 稳健性检验2（已更新）完成 ===================="
display "三种学历阈值全部完成，结果已输出："
display "  1. Table5_PanelA_v2.rtf  (大专及以上)"
display "  2. Table5_PanelB_v2.rtf  (本科及以上)"
display "  3. Table5_PanelC_v2.rtf  (仅高中)"
display "=========================================================="