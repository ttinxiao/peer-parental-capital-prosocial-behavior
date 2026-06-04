clear all
set more off
set linesize 255
cd "D:\导师\残疾伙伴与父母关系\Newceps"

********************************************************************************
* 机制分析：中介效应三步法 (Baron & Kenny, 1986)
* 中介变量 M1: 家长学业管教严格度 (parental strictness)
* 中介变量 M2: 同伴导向指数 (peer orientation index) 
********************************************************************************

* 1. 数据合并与样本筛选
use "CEPS基线调查学生数据.dta", clear
merge 1:1 ids using "CEPS基线调查家长数据.dta", nogen keep(3)
merge m:1 schids clsids using "CEPS基线调查班级数据.dta", nogen keep(3)
merge m:1 schids using "CEPS基线调查学校数据.dta", nogen keep(3)

keep if ple1503 == 1
keep if ple16 == 2
gen teacher_tracking = (hra05==1 | chna05==1 | mata05==1 | enga05==1)
bysort schids grade9: egen has_score_class = max(teacher_tracking)
keep if has_score_class == 0

*=====================================================================
* 2. 被解释变量：亲社会行为指数（PCA）
*=====================================================================

rename c1706 peer_friendly
rename c1707 peer_easygoing
gen peer_affinity = peer_friendly + peer_easygoing

* 维度2：学校归属感（原始总分）
rename c1708 class_atmosphere
rename c1709 participate
rename c1710 school_affinity

gen school_belong_raw = class_atmosphere + participate + school_affinity
label var school_belong_raw "School belonging (raw total score)"

rename c1701 late
rename c1702 skip_class
rename c1703 teacher_criticism
foreach v in late skip_class teacher_criticism {
    gen `v'_r = 5 - `v'
}
egen self_discipline = rowtotal(late_r skip_class_r teacher_criticism_r)

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
label var prosocial "Prosocial behavior index"

*=====================================================================
* 3. 核心解释变量：班级父母高学历占比
*=====================================================================
replace stprhedu = . if stprhedu < 1 | stprhedu > 9
gen high_edu = (stprhedu >= 7) if !missing(stprhedu)
bysort schids clsids: egen college_ratio = mean(high_edu)
label var college_ratio "Class-level ratio of high education parents"

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

* 家庭经济
gen family_econ = steco_5c if !missing(steco_5c)
tab family_econ, gen(family_econ_)
drop family_econ_3

* 班级层面
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
* 5. 中介变量
*=====================================================================

* 5.1 家长学业管教严格度
gen parent_strict = ba0801 if !missing(ba0801)
label var parent_strict "Parental strictness (academic supervision)"

*--------------------------------------------------------------------
* 5.2 同伴导向指数（来自 B26，3个题目构建）
*--------------------------------------------------------------------
gen seek_peer_chat    = (b2601 == 1) if !missing(b2601)
gen seek_peer_trouble = (b2602 == 1) if !missing(b2602)
gen seek_peer_help    = (b2603 == 1) if !missing(b2603)

egen peer_orientation_index = rowtotal(seek_peer_chat seek_peer_trouble seek_peer_help)
label var peer_orientation_index "Peer orientation index (0-3, B26)"

*=====================================================================
* 6. 中介效应三步法
*=====================================================================

* -------------------------------------------------------------------
* 中介1：家长严格管教
* -------------------------------------------------------------------
est clear
reghdfe prosocial college_ratio $controls, absorb(school_grade) cluster(clsids)
est store step1_p

reghdfe parent_strict college_ratio $controls, absorb(school_grade) cluster(clsids)
est store step2_p

reghdfe prosocial college_ratio parent_strict $controls, absorb(school_grade) cluster(clsids)
est store step3_p

esttab step1_p step2_p step3_p using "Table9_Mechanism_ParentalStrictness.rtf", replace ///
    b(%6.3f) se(%6.3f) star(* 0.1 ** 0.05 *** 0.01) ///
    mtitles("Step1" "Step2" "Step3") keep(college_ratio parent_strict) ///
    title("Table 9: Parental Strictness Channel") nonumbers compress

* -------------------------------------------------------------------
* 中介2：同伴导向指数
* -------------------------------------------------------------------
est clear
reghdfe prosocial college_ratio $controls, absorb(school_grade) cluster(clsids)
est store step1_peer

reghdfe peer_orientation_index college_ratio $controls, absorb(school_grade) cluster(clsids)
est store step2_peer

reghdfe prosocial college_ratio peer_orientation_index $controls, absorb(school_grade) cluster(clsids)
est store step3_peer

esttab step1_peer step2_peer step3_peer using "Table10_Mechanism_PeerOrientation.rtf", replace ///
    b(%6.3f) se(%6.3f) star(* 0.1 ** 0.05 *** 0.01) ///
    mtitles("Step1" "Step2" "Step3") keep(college_ratio peer_orientation_index) ///
    title("Table 10: Peer Orientation Channel") nonumbers compress

*=====================================================================
display "中介检验完成！"
