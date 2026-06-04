clear all
set more off
set linesize 255

*=====================
* 1. 设置工作路径
*=====================
cd "D:\导师\残疾伙伴与父母关系\Newceps"

*=====================
* 2. 合并数据
*=====================
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

*=====================================================================
* 3. 筛选随机分班样本
*=====================================================================
keep if ple1503 == 1
keep if ple16 == 2

gen teacher_tracking = (hra05 == 1 | chna05 == 1 | mata05 == 1 | enga05 == 1)
bysort schids grade9: egen has_score_class = max(teacher_tracking)
keep if has_score_class == 0

*=====================================================================
* 4. 构建被解释变量：个人亲社会行为（C17）
*=====================================================================

* ----- 4.1 维度1: 同伴亲和性 -----
rename c1706 peer_friendly
rename c1707 peer_easygoing
gen peer_affinity = peer_friendly + peer_easygoing
label var peer_affinity "同伴亲和性"

* ----- 4.2 维度2: 学校归属感（原始总分）-----
rename c1708 class_atmosphere
rename c1709 participate
rename c1710 school_affinity

gen school_belong_raw = class_atmosphere + participate + school_affinity
label var school_belong_raw "School belonging (raw total score)"

* ----- 4.3 维度3: 自身行为规范（反向编码） -----
rename c1701 late
rename c1702 skip_class
rename c1703 teacher_criticism

foreach v in late skip_class teacher_criticism {
    gen `v'_r = 5 - `v'
}

egen self_discipline = rowtotal(late_r skip_class_r teacher_criticism_r)
label var self_discipline "自身行为规范"

* ----- 4.4 PCA综合指数 -----
egen peer_affinity_z = std(peer_affinity)
egen school_belong_raw_z = std(school_belong_raw)
egen self_discipline_z = std(self_discipline)

pca peer_affinity_z school_belong_raw_z self_discipline_z
predict prosocial_raw, score
egen prosocial = std(prosocial_raw)

corr prosocial peer_affinity_z
if r(rho) < 0 {
    replace prosocial = -prosocial
}
label var prosocial "亲社会行为指数(纯个体，仅C17)"

*=====================================================================
* 5. 核心解释变量
*=====================================================================
replace stprhedu = . if stprhedu < 1 | stprhedu > 9
gen high_edu = (stprhedu >= 7) if !missing(stprhedu)
bysort schids clsids: egen college_ratio = mean(high_edu)
label var college_ratio "班级父母大专及以上占比"

*=====================================================================
* 6. 控制变量（全部使用 if !missing() 确保缺失值为系统缺失 .）
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
gen parent_health = (be09 == 4 | be09 == 5) if !missing(be09)
gen party_member = (be06 == 1) if !missing(be06)

* 家庭经济状况（变量：steco_5c）
gen family_econ = steco_5c if !missing(steco_5c)
tab family_econ, gen(family_econ_)
drop family_econ_3

* ----- 6.5 班级层面控制变量 -----
gen class_size = clsn if !missing(clsn)
label var class_size "班级规模"

gen teacher_exp = hrc07 if !missing(hrc07)
label var teacher_exp "班主任教龄（年）"

gen teacher_title = hrc12 if !missing(hrc12)
label var teacher_title "班主任职称等级"

gen normal_college = (hrc05 == 1) if !missing(hrc05)
label var normal_college "班主任是否师范院校毕业"

gen public_position = (hrc11 == 1) if !missing(hrc11)
label var public_position "班主任是否担任行政职务"

*=====================================================================
* 7. 学校-年级固定效应
*=====================================================================
egen school_grade = group(schids grade9)

*=====================================================================
* ===================== 【基准回归：加入班级层面控制变量】 =====================
* 控制：学生 + 家庭 + 班级层面 + 学校-年级固定效应
*=====================================================================
est clear

* Col1 同伴亲和性
oprobit peer_affinity college_ratio age male rural_hukou only_child boarding ///
    parent_edu parent_age parent_health party_member ///
    family_econ_1 family_econ_2 family_econ_4 family_econ_5 ///
    class_size teacher_exp teacher_title normal_college public_position ///
    i.school_grade, vce(cluster clsids)
est store m1

* Col2 学校归属感
oprobit school_belong_raw college_ratio age male rural_hukou only_child boarding ///
    parent_edu parent_age parent_health party_member ///
    family_econ_1 family_econ_2 family_econ_4 family_econ_5 ///
    class_size teacher_exp teacher_title normal_college public_position ///
    i.school_grade, vce(cluster clsids)
est store m2

* Col3 自身行为规范
oprobit self_discipline college_ratio age male rural_hukou only_child boarding ///
    parent_edu parent_age parent_health party_member ///
    family_econ_1 family_econ_2 family_econ_4 family_econ_5 ///
    class_size teacher_exp teacher_title normal_college public_position ///
    i.school_grade, vce(cluster clsids)
est store m3

* Col4 亲社会指数
reghdfe prosocial college_ratio age male rural_hukou only_child boarding ///
    parent_edu parent_age parent_health party_member ///
    family_econ_1 family_econ_2 family_econ_4 family_econ_5 ///
    class_size teacher_exp teacher_title normal_college public_position, ///
    absorb(school_grade) cluster(clsids)
est store m4

* 输出：加入班级层面控制后的回归
esttab m1 m2 m3 m4 using "Table3_Baseline.rtf", replace ///
    b(%6.3f) se(%6.3f) star(* 0.1 ** 0.05 *** 0.01) ///
    keep(college_ratio) ///
    mtitles("Peer" "School Belong" "Discipline" "Prosocial") ///
    title("Table 3: Baseline Results ") ///
    addnotes("Class-level controls: class size, teacher experience, title, normal college, admin position" ///
             "Controls: Student & Family & Class Characteristics" ///
             "School-Grade FE & Class-clustered SE") ///
    nocons nonumbers nogaps compress scalar(N)

display "======================================================"
display "加入班级层面控制变量的基准回归已输出：Table3_Baseline.rtf"