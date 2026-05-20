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
* 一、构建被解释变量-个人亲社会行为
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

* --- 维度3：自身行为规范（反向编码，要求三个变量均非缺失）
rename c1701 late
rename c1702 skip_class
rename c1703 teacher_criticism

foreach v in late skip_class teacher_criticism {
    gen `v'_r = 5 - `v'   // 原1->4, 2->3, 3->2, 4->1，使高分表示行为规范
}

* 仅当三个变量全部非缺失时才计算总分，避免因忽略缺失导致分数偏倚
egen self_discipline_raw = rowtotal(late_r skip_class_r teacher_criticism_r) if !missing(late_r, skip_class_r, teacher_criticism_r)
label var self_discipline_raw "Self-discipline (raw total score, 3 items)"

* --- 生成标准化版本（用于后续PCA）
egen peer_affinity_z = std(peer_affinity_raw)
egen school_belong_z = std(school_belong_raw)
egen self_discipline_z = std(self_discipline_raw)

* --- PCA 构建亲社会行为综合指数（使用标准化后的维度）
pca peer_affinity_z school_belong_z self_discipline_z
predict prosocial_factor_raw, score   // 原始因子得分（未标准化）
egen prosocial_individual = std(prosocial_factor_raw)   // 二次标准化：均值0，标准差1

* 确保方向一致：亲社会行为高时，因子得分也应高（若有负相关则反转）
corr prosocial_individual peer_affinity_z
if r(rho) < 0 {
    replace prosocial_individual = -prosocial_individual
    replace prosocial_factor_raw = -prosocial_factor_raw
}

label var prosocial_individual   "Prosocial behavior index (standardized, mean=0, SD=1)"
label var prosocial_factor_raw   "Prosocial behavior factor score (raw, mean=0)"

*=====================================================================
* 二、核心解释变量：班级父母高学历占比
*=====================================================================
gen high_edu = (stprhedu >= 7)
bysort schids clsids: egen college_ratio = mean(high_edu)
label var college_ratio "Class-level ratio of high education parents"

*=====================================================================
* 三、控制变量 & 机制变量
*=====================================================================
gen study_weekday = b15a1 * 60 + b15a2
gen study_weekend = b16a1 * 60 + b16a2
gen study_time_min = (study_weekday * 5 + study_weekend * 2) / 7

gen sleep_min = b18a * 60 + b18b

gen tutoring_count = (b1901 == 1) + (b1902 == 1) + (b1903 == 1) + (b1904 == 1)

pca ba18 ba20
predict parent_expectation, score

pca c22 c24 c25
predict student_motivation, score

gen age = 2013 - a02a

*=====================================================================
* 变量标签
*=====================================================================
label var peer_affinity_raw   "Peer affinity (raw)"
label var school_belong_raw   "School belonging (raw)"
label var self_discipline_raw "Self-discipline (raw)"
label var study_time_min      "Study time (minutes per day)"
label var sleep_min           "Sleep duration (minutes per day)"
label var tutoring_count      "Number of tutoring subjects"
label var parent_expectation  "Parental expectation (PCA score)"
label var student_motivation  "Student motivation (PCA score)"
label var stsex               "Gender (1=male)"
label var age                 "Age (years)"
label var sthktype            "Household registration (1=rural)"
label var stonly              "Only child (1=yes)"
label var stbrd               "Boarding status (1=yes)"
label var stprhedu            "Parental education"
label var steco_5c            "Family income"
label var clsn                "Class size"

*=====================================================================
* 描述性统计：原始变量 + 标准化后的 prosocial_individual
*=====================================================================
local desc_raw_vars ///
    peer_affinity_raw       ///
    school_belong_raw       ///
    self_discipline_raw     ///
    prosocial_individual    ///
    college_ratio           ///
    high_edu                ///
    study_time_min          ///
    sleep_min               ///
    tutoring_count          ///
    parent_expectation      ///
    student_motivation      ///
    stsex                   ///
    age                     ///
    sthktype                ///
    stonly                  ///
    stbrd                   ///
    stprhedu                ///
    steco_5c                ///
    clsn

estpost summarize `desc_raw_vars', detail
esttab using Table1_Descriptive.rtf, replace ///
    cells("count(fmt(%10.0f)) mean(fmt(%10.3f)) sd(fmt(%10.3f)) min(fmt(%10.3f)) max(fmt(%10.3f))") ///
    collabels("N" "Mean" "SD" "Min" "Max") ///
    title("Table 1 Descriptive Statistics") ///
    nonumber nomtitle

*=====================================================================
* 输出提示
*=====================================================================
display "数据准备完成。描述性表格已输出为 Table1_Descriptive.rtf"