clear all
set more off
set linesize 255

*=====================================================================
* 1. 设置工作路径
*=====================================================================
cd "D:\导师\残疾伙伴与父母关系\Newceps"

*=====================================================================
* 2. 合并数据
*=====================================================================
use "CEPS基线调查学生数据.dta", clear
merge 1:1 ids using "CEPS基线调查家长数据.dta", nogen keep(3)
merge m:1 schids clsids using "CEPS基线调查班级数据.dta", nogen keep(3)
merge m:1 schids using "CEPS基线调查学校数据.dta", nogen keep(3)

*=====================================================================
* 3. 筛选随机分班样本
*=====================================================================
* 保留明确表示"就近入学"和"非重点班"的样本
keep if ple1503 == 1
keep if ple16 == 2

* 识别出"按成绩分班"的学校-年级
gen teacher_tracking = (hra05==1 | chna05==1 | mata05==1 | enga05==1)
bysort schids grade9: egen has_score_class = max(teacher_tracking)

* 只保留不存在"按成绩分班"的随机样本 (has_score_class == 0)
keep if has_score_class == 0

*=====================================================================
* 4. 被解释变量：纯个人亲社会行为（仅C17）
*=====================================================================
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
predict prosocial_raw, score
egen prosocial = std(prosocial_raw)

* 方向检验，确保指数方向正确
corr prosocial peer_affinity_z
if r(rho) < 0 {
    replace prosocial = -prosocial
}
label var prosocial "亲社会行为指数(纯个体)"

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
gen parent_health = (be09 >= 4) if !missing(be09)
gen party_member = (be06 == 1) if !missing(be06)

* 家庭经济（steco_5c，哑变量）
gen family_econ = steco_5c if !missing(steco_5c)
tab family_econ, gen(family_econ_)
drop family_econ_3

* 班级层面
gen class_size = clsn if !missing(clsn)
gen teacher_exp = hrc07 if !missing(hrc07)
gen teacher_title = hrc12 if !missing(hrc12)
gen normal_college = (hrc05 == 1) if !missing(hrc05)
gen public_position = (hrc11 == 1) if !missing(hrc11)

* 固定效应
egen school_grade = group(schids grade9)

*=====================================================================
* 7. 构造父母社交 & 父母参与变量（全部使用 if !missing()）
*=====================================================================

* 1) 父母社交互动 (来自家长问卷)
gen social_know_parents = (ba12 == 1) if !missing(ba12)
label var social_know_parents "认识同学家长"

gen social_info_parents = (bc0105 == 1) if !missing(bc0105)
label var social_info_parents "通过家长获取入学信息"

* 2) 父母学校参与 (来自家长问卷)
gen engage_meeting = (bb01 == 1) if !missing(bb01)
label var engage_meeting "参加家长会"

gen engage_contact_teacher = (bb02 >= 2) if !missing(bb02)
label var engage_contact_teacher "联系班主任(非从不)"

*=====================================================================
* 8. 稳健性检验3：7列逐步回归
*=====================================================================
est clear

* 定义控制变量宏，方便调用
global student_controls age male rural_hukou only_child
global family_controls parent_edu parent_age parent_health party_member family_econ_1 family_econ_2 family_econ_4 family_econ_5
global class_controls class_size teacher_exp teacher_title normal_college public_position
global social_controls social_know_parents social_info_parents
global engage_controls engage_meeting engage_contact_teacher

* (1) 仅固定效应
reghdfe prosocial college_ratio, absorb(school_grade) cluster(clsids)
est store m1

* (2) + 学生控制
reghdfe prosocial college_ratio $student_controls, absorb(school_grade) cluster(clsids)
est store m2

* (3) + 家庭控制
reghdfe prosocial college_ratio $student_controls $family_controls, absorb(school_grade) cluster(clsids)
est store m3

* (4) + 班级控制
reghdfe prosocial college_ratio $student_controls $family_controls $class_controls, absorb(school_grade) cluster(clsids)
est store m4

* (5) + 住校
reghdfe prosocial college_ratio $student_controls $family_controls $class_controls boarding, absorb(school_grade) cluster(clsids)
est store m5

* (6) + 父母社交
reghdfe prosocial college_ratio $student_controls $family_controls $class_controls boarding $social_controls, absorb(school_grade) cluster(clsids)
est store m6

* (7) + 父母参与
reghdfe prosocial college_ratio $student_controls $family_controls $class_controls boarding $social_controls $engage_controls, absorb(school_grade) cluster(clsids)
est store m7

*=====================================================================
* 9. 输出表格
*=====================================================================
esttab m1 m2 m3 m4 m5 m6 m7 using "Table6_Robustness_Stepwise.rtf", replace ///
    b(%6.3f) se(%6.3f) star(* 0.1 ** 0.05 *** 0.01) ///
    keep(college_ratio) scalars(N r2_a) ///
    mtitles("FE" "+Student" "+Family" "+Class" "+Board" "+Social" "+Engage") ///
    title("Table 6: Robustness - Stepwise Controls") ///
    addnotes("Dependent: Prosocial Index (C17 only, school belonging updated)" ///
             "Class-clustered SE; School-Grade FE in all models") ///
    nocons nonumbers compress nogaps

display ""
display "==================== 稳健性检验3完成 ===================="
display "表格已输出至: Table6_Robustness_Stepwise.rtf"
display "======================================================="