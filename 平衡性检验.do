clear all
set more off
set linesize 255

cd "D:\导师\残疾伙伴与父母关系\Newceps"

*--------------------------------------------------------------------
* 1. 数据准备 
*--------------------------------------------------------------------
use "CEPS基线调查学生数据.dta", clear
merge 1:1 ids using "CEPS基线调查家长数据.dta"
keep if _merge==3
drop _merge

merge m:1 schids clsids using "CEPS基线调查班级数据.dta"
keep if _merge==3
drop _merge

merge m:1 schids using "CEPS基线调查学校数据.dta"
keep if _merge==3
drop _merge

*--------------------------------------------------------------------
* 2. 筛选随机分班样本 
*--------------------------------------------------------------------
keep if ple1503==1
keep if ple16==2
gen teacher_tracking = (hra05==1 | chna05==1 | mata05==1 | enga05==1)
bysort schids grade9: egen has_score_class = max(teacher_tracking)
keep if has_score_class==0

*--------------------------------------------------------------------
* 3. 生成所有需要的变量
*--------------------------------------------------------------------
* 核心自变量
replace stprhedu = . if stprhedu<1 | stprhedu>9
gen high_edu = (stprhedu>=7)
bysort schids clsids: egen college_ratio = mean(high_edu)

* 平衡检验变量
rename hrc07 teach_exp
rename hrc12 teach_title
rename hrc11 teach_public
rename hrc05 teach_normal
rename clsn class_size
gen female = (stsex==0)
bysort schids clsids: egen female_ratio = mean(female)

* 学校固定效应的标识
egen school_grade = group(schids grade9)

*--------------------------------------------------------------------
* 4. 转换为班级层面数据
*--------------------------------------------------------------------
bysort schids clsids: keep if _n==1
display "数据已成功转换为班级层面, 当前观测值数量 (班级数): " _N

*--------------------------------------------------------------------
* 5. 平衡检验回归（班级层面 + 学校固定效应）
*--------------------------------------------------------------------
est clear

* 回归时必须控制 i.school_grade (或 i.schids)
* 使用 robust 选项获得稳健标准误
reg teach_exp     college_ratio i.school_grade, robust
est store m1

reg teach_title   college_ratio i.school_grade, robust
est store m2

reg teach_public  college_ratio i.school_grade, robust
est store m3

reg teach_normal  college_ratio i.school_grade, robust
est store m4

reg class_size    college_ratio i.school_grade, robust
est store m5

reg female_ratio  college_ratio i.school_grade, robust
est store m6

*--------------------------------------------------------------------
* 6. 输出结果
*--------------------------------------------------------------------
esttab m1 m2 m3 m4 m5 m6 using Table2_Balance.rtf, replace ///
    b(%6.3f) se(%6.3f) scalars(N) nocons nostar nonumbers ///
    title("Table 2: Balance Test (at Class Level with School-Grade FE)")

display "平衡检验完成！"
