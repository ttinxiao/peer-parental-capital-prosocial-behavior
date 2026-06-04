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
* 筛选随机分班样本
*=====================================================================
keep if ple1503 == 1
keep if ple16 == 2

gen teacher_tracking = (hra05 == 1 | chna05 == 1 | mata05 == 1 | enga05 == 1)
bysort schids grade9: egen has_score_class = max(teacher_tracking)
keep if has_score_class == 0

*=====================================================================
* 一、构建被解释变量 - 个人亲社会行为
*=====================================================================

* --- 步骤1：处理行为变量的异常缺失值
foreach v in c1701 c1702 c1703 {
    replace `v' = . if `v' >= 98 | `v' <= 0
}

* --- 维度1：同伴亲和性（原始总分）
rename c1706 peer_friendly
rename c1707 peer_easygoing
gen peer_affinity_raw = peer_friendly + peer_easygoing
label var peer_affinity_raw "Peer affinity (raw total score)"

* --- 维度2：集体参与与学校归属感（原始总分）
rename c1708 class_atmosphere
rename c1709 participate
rename c1710 school_affinity
gen school_belong_raw = class_atmosphere + participate + school_affinity
label var school_belong_raw "School belonging (raw total score)"

* --- 维度3：自身行为规范（反向编码）
rename c1701 late
rename c1702 skip_class
rename c1703 teacher_criticism

foreach v in late skip_class teacher_criticism {
    gen `v'_r = 5 - `v'
}

egen self_discipline_raw = rowtotal(late_r skip_class_r teacher_criticism_r) if !missing(late_r, skip_class_r, teacher_criticism_r)
label var self_discipline_raw "Self-discipline (raw total score, 3 items)"

* --- 生成标准化版本（用于PCA）
egen peer_affinity_z = std(peer_affinity_raw)
egen school_belong_z = std(school_belong_raw)
egen self_discipline_z = std(self_discipline_raw)

* --- PCA 构建亲社会行为综合指数
pca peer_affinity_z school_belong_z self_discipline_z
predict prosocial_factor_raw, score
egen prosocial_individual = std(prosocial_factor_raw)

* 方向校正
corr prosocial_individual peer_affinity_z
if r(rho) < 0 {
    replace prosocial_individual = -prosocial_individual
    replace prosocial_factor_raw = -prosocial_factor_raw
}
label var prosocial_individual "Prosocial behavior index (standardized, mean=0, SD=1)"

*=====================================================================
* 二、核心解释变量：班级父母高学历占比
*=====================================================================
gen high_edu = (stprhedu >= 7)
bysort schids clsids: egen college_ratio = mean(high_edu)
label var college_ratio "Class-level ratio of high education parents"

*=====================================================================
* 三、中介变量
*=====================================================================
* 3.1 家长学业管教严格度
gen parental_strictness = ba0801 if !missing(ba0801)
label var parental_strictness "Parental strictness (academic supervision)"

* 3.2 同伴导向指数（B26: 三个题目）
gen seek_peer_chat    = (b2601 == 1) if !missing(b2601)
gen seek_peer_trouble = (b2602 == 1) if !missing(b2602)
gen seek_peer_help    = (b2603 == 1) if !missing(b2603)
egen peer_orientation_index = rowtotal(seek_peer_chat seek_peer_trouble seek_peer_help)
label var peer_orientation_index "Peer orientation index (0-3, from B26)"

*=====================================================================
* 四、控制变量（按学生、家庭、班级层面）
*=====================================================================
* 学生层面
gen male = (stsex == 1)
gen age = 2013 - a02a
gen rural_hukou = (sthktype == 1)
gen only_child = (stonly == 1)
gen boarding = (stbrd == 1)

* 家庭层面
gen parent_edu = stprhedu
gen parent_age = 2013 - be01
gen parent_health = (be09 >= 4)
gen party_member = (be06 == 1)
gen family_econ = steco_5c        // 原始5分类

* 班级层面（稳健性检验）
gen class_size = clsn
gen teacher_exp = hrc07
gen teacher_title = hrc12
gen normal_college = (hrc05 == 1)
gen public_position = (hrc11 == 1)

* 为控制变量添加标签
label var male "Gender (1=male)"
label var age "Age (years)"
label var rural_hukou "Rural hukou (1=rural)"
label var only_child "Only child (1=yes)"
label var boarding "Boarding status (1=yes)"
label var parent_edu "Parental education level"
label var parent_age "Parental age"
label var parent_health "Parental health (1=good)"
label var party_member "Party member (1=yes)"
label var family_econ "Family economic status (1-5)"
label var class_size "Class size"
label var teacher_exp "Teacher experience (years)"
label var teacher_title "Teacher title"
label var normal_college "Normal college graduate (1=yes)"
label var public_position "Public position (1=yes)"

* 学校-年级固定效应（用于回归，不放入描述性表格）
egen school_grade = group(schids grade9)

*=====================================================================
* 五、描述性统计表格
*=====================================================================

* 表1：核心变量（自变量、因变量、中介变量）
local core_vars ///
    college_ratio           ///
    peer_affinity_raw       ///
    school_belong_raw       ///
    self_discipline_raw     ///
    prosocial_individual    ///
    parental_strictness     ///
    peer_orientation_index

estpost summarize `core_vars', detail
esttab using Table1_Descriptive.rtf, replace ///
    cells("count(fmt(%10.0f)) mean(fmt(%10.3f)) sd(fmt(%10.3f)) min(fmt(%10.3f)) max(fmt(%10.3f))") ///
    collabels("N" "Mean" "SD" "Min" "Max") ///
    title("Table 1 Descriptive Statistics (Core Variables)") ///
    nonumber nomtitle

* 表2：控制变量（学生、家庭、班级层面）
local control_vars ///
    male age rural_hukou only_child boarding ///
    parent_edu parent_age parent_health party_member family_econ ///
    class_size teacher_exp teacher_title normal_college public_position

estpost summarize `control_vars', detail
esttab using Table2_Controls.rtf, replace ///
    cells("count(fmt(%10.0f)) mean(fmt(%10.3f)) sd(fmt(%10.3f)) min(fmt(%10.3f)) max(fmt(%10.3f))") ///
    collabels("N" "Mean" "SD" "Min" "Max") ///
    title("Table 2 Descriptive Statistics (Control Variables)") ///
    nonumber nomtitle

*=====================================================================
* 输出提示
*=====================================================================
display "描述性统计已完成："
display " - Table1_Descriptive.rtf (核心变量)"
display " - Table2_Controls.rtf (控制变量)"