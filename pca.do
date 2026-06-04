clear all
set more off
set linesize 255

cd "D:\导师\残疾伙伴与父母关系\Newceps"

* Install necessary external commands (factortest and logout)
capture which factortest
if _rc != 0 {
    ssc install factortest, replace
}
capture which logout
if _rc != 0 {
    ssc install logout, replace
}


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

*=======================================================================
* 3. Keep random assignment classes
*=======================================================================
keep if ple1503 == 1          
keep if ple16 == 2            
gen teacher_tracking = (hra05 == 1 | chna05 == 1 | mata05 == 1 | enga05 == 1)
bysort schids grade9: egen has_score_class = max(teacher_tracking)
keep if has_score_class == 0  

*=======================================================================
* 4. Construct the three dimensions of prosocial behavior
*=======================================================================
rename c1706 peer_friendly    
rename c1707 peer_easygoing  
gen peer_affinity = peer_friendly + peer_easygoing
egen peer_affinity_z = std(peer_affinity)

rename c1708 class_atmosphere 
rename c1709 participate       
rename c1710 school_affinity  
gen school_belong = class_atmosphere + participate + school_affinity
egen school_belong_z = std(school_belong)

rename c1701 late              
rename c1702 skip_class        
rename c1703 teacher_criticism 
foreach v in late skip_class teacher_criticism {
    gen `v'_r = 5 - `v'
}
egen self_discipline = rowtotal(late_r skip_class_r teacher_criticism_r)
egen self_discipline_z = std(self_discipline)

* English labels
label var peer_affinity_z "Peer affinity (standardized)"
label var school_belong_z "School belonging (standardized)"
label var self_discipline_z "Self-discipline (standardized)"

global pca_vars peer_affinity_z school_belong_z self_discipline_z

*=======================================================================
* 5. PCA and generate prosocial behavior index
*=======================================================================
pca $pca_vars
predict prosocial_individual_raw, score  
egen prosocial_individual = std(prosocial_individual_raw)
corr prosocial_individual peer_affinity_z
if r(rho) < 0 replace prosocial_individual = -prosocial_individual
label var prosocial_individual "Prosocial behavior index (PCA)"

*=======================================================================
* 6. Output five separate RTF files (no merging)
*=======================================================================
quietly alpha peer_friendly peer_easygoing class_atmosphere participate school_affinity late_r skip_class_r teacher_criticism_r
local alpha_8item = r(alpha)
display "8个原始题项 Cronbach's α 系数 = " %6.3f `alpha_8item'
* 6.1 KMO and Bartlett's test of sphericity
quietly factortest $pca_vars
logout, save(PCA_1_kmo_bartlett) word replace: factortest $pca_vars

* 6.2 Eigenvalues and variance explained
logout, save(PCA_2_eigen) word replace: pca $pca_vars

* 6.3 Factor loadings
pca $pca_vars
logout, save(PCA_3_loadings) word replace: estat loadings

* 6.4 Communalities (squared loadings)
pca $pca_vars
matrix loading = e(L)
local comm1 = loading[1,1]^2
local comm2 = loading[2,1]^2
local comm3 = loading[3,1]^2
clear matrix
logout, save(PCA_4_communality) word replace: ///
    di "Communality (squared loadings) for PC1:" _newline ///
    "peer_affinity_z:     " `comm1' _newline ///
    "school_belong_z:     " `comm2' _newline ///
    "self_discipline_z:   " `comm3'

* 6.5 Descriptive statistics of the prosocial index
logout, save(PCA_5_desc) word replace: summarize prosocial_individual, detail


*=======================================================================
display " "
display "====================== SUCCESS ======================"
display "Final variable 'prosocial_individual' created."
display "PCA results saved as separate RTF files:"
display "  1. PCA_1_kmo_bartlett.rtf   (KMO + Bartlett's test)"
display "  2. PCA_2_eigen.rtf          (Eigenvalues & variance explained)"
display "  3. PCA_3_loadings.rtf       (Factor loadings)"
display "  4. PCA_4_communality.rtf    (Communalities = squared loadings)"
display "  5. PCA_5_desc.rtf           (Descriptive statistics of the index)"
display "====================================================="