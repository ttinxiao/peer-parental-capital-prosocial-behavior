clear all
set more off
set linesize 255
cd "D:\导师\残疾伙伴与父母关系\Newceps"

********************************************************************************
* Table5：稳健性检验——重新定义班级父母学历（三种阈值）
********************************************************************************

* 1. 数据合并
use "CEPS基线调查学生数据.dta", clear
merge 1:1 ids using "CEPS基线调查家长数据.dta", nogen keep(3)
merge m:1 schids clsids using "CEPS基线调查班级数据.dta", nogen keep(3)
merge m:1 schids using "CEPS基线调查学校数据.dta", nogen keep(3)

* 2. 随机分班样本
keep if ple1503 == 1
keep if ple16 == 2
gen teacher_tracking = (hra05==1 | chna05==1 | mata05==1 | enga05==1)
bysort schids grade9: egen has_score_class = max(teacher_tracking)
keep if has_score_class == 0

* 3. 父母学历处理
replace stprhedu = . if stprhedu < 1 | stprhedu > 9

*=============================================================================
* 被解释变量：纯个人亲社会行为（仅C17）
*=============================================================================

* 维度1: 同伴亲和性
rename c1706 peer_friendly
rename c1707 peer_easygoing
gen peer_affinity = peer_friendly + peer_easygoing

* 维度2: 集体参与
rename c1709 participate
gen collective_participation = participate

* 维度3: 自身行为规范（C17反向编码）
rename c1701 late
rename c1702 skip_class
rename c1703 teacher_criticism
foreach v in late skip_class teacher_criticism {
    gen `v'_r = 5 - `v'
}
egen self_discipline = rowtotal(late_r skip_class_r teacher_criticism_r)

* PCA 纯个人亲社会综合指数
egen z1 = std(peer_affinity)
egen z2 = std(collective_participation)
egen z3 = std(self_discipline)
pca z1 z2 z3
predict pca_score, score
egen prosocial = std(pca_score)

* 方向检验
corr prosocial z1
if r(rho) < 0 {
    replace prosocial = -prosocial
}

*=============================================================================
* 三种自变量定义（剔除自身）
*=============================================================================

* 班级人数
bysort schids clsids: gen class_n = _N

* Panel A：大专及以上（剔除自己）
gen high_edu_A = (stprhedu >= 7)
bysort schids clsids: egen sum_A = total(high_edu_A)
gen ratio_A = (sum_A - high_edu_A) / (class_n - 1)

* Panel B：本科及以上（剔除自己）
gen high_edu_B = (stprhedu >= 8)
bysort schids clsids: egen sum_B = total(high_edu_B)
gen ratio_B = (sum_B - high_edu_B) / (class_n - 1)

* Panel C：普通高中（剔除自己）
gen high_edu_C = (stprhedu == 6)
bysort schids clsids: egen sum_C = total(high_edu_C)
gen ratio_C = (sum_C - high_edu_C) / (class_n - 1)

*=============================================================================
* 控制变量
*=============================================================================

* 学生层面
gen age = 2013 - a02a
gen male = (stsex == 1)
gen rural_hukou = (sthktype == 1)
gen only_child = (stonly == 1)
gen boarding = (stbrd == 1)

* 家庭层面
gen parent_age = be01
gen parent_health = (be09 >= 4)
gen parent_educ = stprhedu
gen party = (be06 == 1)

* 家庭经济状况（steco_5c）
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

* 控制项宏
global controls age male rural_hukou only_child boarding ///
    parent_age parent_health parent_educ party ///
    family_econ_1 family_econ_2 family_econ_4 family_econ_5 ///
    class_size teacher_exp teacher_title normal_college public_position

*=============================================================================
* 回归输出（4列：同伴 / 集体 / 行为规范 / 综合指数）
*=============================================================================

* ---------- Panel A: 大专及以上 ----------
est clear
oprobit peer_affinity         ratio_A $controls i.school_grade, vce(cluster clsids)
est store A1
oprobit collective_participation ratio_A $controls i.school_grade, vce(cluster clsids)
est store A2
oprobit self_discipline       ratio_A $controls i.school_grade, vce(cluster clsids)
est store A3
reghdfe prosocial             ratio_A $controls, absorb(school_grade) cluster(clsids)
est store A4

esttab A1 A2 A3 A4 using "Table5_PanelA.rtf", replace ///
    b(%6.3f) se(%6.3f) star(* 0.10 ** 0.05 *** 0.01) ///
    keep(ratio_A) scalars(N) ///
    mtitles("Peer" "Collective" "Discipline" "Prosocial") ///
    title("Table 5 Panel A: Associate Degree or Above") ///
    addnotes("Col1-3: Ordered Probit; Col4: OLS") ///
    nocons nonumbers compress nogaps

* ---------- Panel B: 本科及以上 ----------
est clear
oprobit peer_affinity         ratio_B $controls i.school_grade, vce(cluster clsids)
est store B1
oprobit collective_participation ratio_B $controls i.school_grade, vce(cluster clsids)
est store B2
oprobit self_discipline       ratio_B $controls i.school_grade, vce(cluster clsids)
est store B3
reghdfe prosocial             ratio_B $controls, absorb(school_grade) cluster(clsids)
est store B4

esttab B1 B2 B3 B4 using "Table5_PanelB.rtf", replace ///
    b(%6.3f) se(%6.3f) star(* 0.10 ** 0.05 *** 0.01) ///
    keep(ratio_B) scalars(N) ///
    mtitles("Peer" "Collective" "Discipline" "Prosocial") ///
    title("Table 5 Panel B: Bachelor Degree or Above") ///
    addnotes("Col1-3: Ordered Probit; Col4: OLS") ///
    nocons nonumbers compress nogaps

* ---------- Panel C: 普通高中 ----------
est clear
oprobit peer_affinity         ratio_C $controls i.school_grade, vce(cluster clsids)
est store C1
oprobit collective_participation ratio_C $controls i.school_grade, vce(cluster clsids)
est store C2
oprobit self_discipline       ratio_C $controls i.school_grade, vce(cluster clsids)
est store C3
reghdfe prosocial             ratio_C $controls, absorb(school_grade) cluster(clsids)
est store C4

esttab C1 C2 C3 C4 using "Table5_PanelC.rtf", replace ///
    b(%6.3f) se(%6.3f) star(* 0.10 ** 0.05 *** 0.01) ///
    keep(ratio_C) scalars(N) ///
    mtitles("Peer" "Collective" "Discipline" "Prosocial") ///
    title("Table 5 Panel C: High School Only") ///
    addnotes("Col1-3: Ordered Probit; Col4: OLS") ///
    nocons nonumbers compress nogaps

display ""
display "==================== 稳健性检验2 完成 ===================="
display "个人亲社会行为"
display "三种学历定义全部完成"
display "表格已输出"
display "=========================================================="