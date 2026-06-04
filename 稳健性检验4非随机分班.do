clear all
set more off
set linesize 255
cd "D:\导师\残疾伙伴与父母关系\Newceps"

********************************************************************************
* 稳健性检验4 Panel B：Placebo 检验 —— 非随机分班样本
********************************************************************************

* 1. 数据合并
use "CEPS基线调查学生数据.dta", clear
merge 1:1 ids using "CEPS基线调查家长数据.dta", nogen keep(3)
merge m:1 schids clsids using "CEPS基线调查班级数据.dta", nogen keep(3)
merge m:1 schids using "CEPS基线调查学校数据.dta", nogen keep(3)

*=====================================================================
* 2. 被解释变量：纯个人亲社会
*=====================================================================

* 同伴亲和性
rename c1706 peer_friendly
rename c1707 peer_easygoing
gen peer_affinity = peer_friendly + peer_easygoing

* 学校归属感（原始总分）
rename c1708 class_atmosphere
rename c1709 participate
rename c1710 school_affinity

gen school_belong_raw = class_atmosphere + participate + school_affinity
label var school_belong_raw "School belonging (raw total score)"

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
egen z2 = std(school_belong_raw)
egen z3 = std(self_discipline)

pca z1 z2 z3
predict prosocial_raw, score
egen prosocial = std(prosocial_raw)

corr prosocial z1
if r(rho) < 0 {
    replace prosocial = -prosocial
}

*=====================================================================
* 3. 核心解释变量
*=====================================================================
replace stprhedu = . if stprhedu < 1 | stprhedu > 9
gen high_edu = (stprhedu >= 7) if !missing(stprhedu)
bysort schids clsids: egen college_ratio = mean(high_edu)

*=====================================================================
* 4. 控制变量（全部使用 if !missing() 确保缺失值为系统缺失 .）
*=====================================================================

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

* 家庭经济：steco_5c 哑变量
gen family_econ = steco_5c if !missing(steco_5c)
tab family_econ, gen(family_econ_)
drop family_econ_3

* 班级控制（全套）
gen class_size = clsn if !missing(clsn)
gen teacher_exp = hrc07 if !missing(hrc07)
gen teacher_title = hrc12 if !missing(hrc12)
gen normal_college = (hrc05 == 1) if !missing(hrc05)
gen public_position = (hrc11 == 1) if !missing(hrc11)

egen school_grade = group(schids grade9)

global controls ///
    age male rural_hukou only_child boarding ///
    parent_edu parent_age parent_health party_member ///
    family_econ_1 family_econ_2 family_econ_4 family_econ_5 ///
    class_size teacher_exp teacher_title normal_college public_position

*=====================================================================
* Panel B：筛选【非随机分班样本】Placebo
*=====================================================================
gen random_criteria1 = (ple1503 == 1)
gen random_criteria2 = (ple16 == 2)

gen teacher_tracking = (hra05==1 | chna05==1 | mata05==1 | enga05==1)
bysort schids grade9: egen has_score_class = max(teacher_tracking)
gen random_criteria3 = (has_score_class == 0)

gen is_random = (random_criteria1 & random_criteria2 & random_criteria3)

* 保留：非随机样本
keep if is_random == 0

*=====================================================================
* 回归：reghdfe
*=====================================================================
est clear
reghdfe peer_affinity         college_ratio $controls, absorb(school_grade) cluster(clsids)
est store B1
reghdfe school_belong_raw     college_ratio $controls, absorb(school_grade) cluster(clsids)
est store B2
reghdfe self_discipline       college_ratio $controls, absorb(school_grade) cluster(clsids)
est store B3
reghdfe prosocial             college_ratio $controls, absorb(school_grade) cluster(clsids)
est store B4

*=====================================================================
* 输出表格
*=====================================================================
esttab B1 B2 B3 B4 using "Table7_PanelB_Placebo.rtf", replace ///
    b(%6.3f) se(%6.3f) star(* 0.1 ** 0.05 *** 0.01) ///
    keep(college_ratio) scalars(N r2_a) ///
    mtitles("Affinity" "School Belong" "Discipline" "Prosocial") ///
    title("Table 7 Panel B: Non-Random Class (Placebo Test)") ///
    addnotes("Placebo: No peer effect expected in non-random sample" ///
             "Controls: Student, Family & Class Characteristics" ///
             "School-Grade FE absorbed, SE clustered at class level") ///
    nocons nonumbers compress nogaps

display "Panel B Placebo 检验完成！"