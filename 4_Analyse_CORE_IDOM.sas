*specify file to which log is written;
%global dat_log;
%let dat_log = %sysfunc(today(),date9.);
%let levothyroxine=%sysget(USERPROFILE)\levothyroxine;
%put &=levothyroxine;
filename newlog "&levothyroxine\logs\defineovertreatment&dat_log..log";
proc printto log=newlog new;
run;


*save and write time of start of program run;
data _null_;call symput('anfang',put(datetime(),datetime16.));run;
data _null_;call symput('banfang',put(datetime(),Best.));run;
%put Anfang: &anfang;

************************************;
* Project: NAKO 319      			;
*									;
* Author: Brandes, B. 				;
* Date: Oct 2025        			;
*									;
************************************;


DM output 'clear';
DM log 'clear';

options nofmterr orientation = landscape nocenter ls=132 ps=132 nodate;

libname library '&levothyroxine\dats\';

*load IDOM dataset;
data medication;
set library.medication_neu;
run;


*load CORE dataset;
data core_cleaned;
set library.core_cleaned;
run;

*merge CORE and IDOM datasets;
proc sort data=core_cleaned; by ID; run;
proc sort data=medication; by ID; run;

data core_med;
merge core_cleaned (in=a) medication;
by ID;
if a;
if idom_exp_dauer = -999 then idom_exp_dauer = .;
run;


*flow chart;
data core_med;
    set core_med;
    f_missing_tsh = (sa_tsh_info = .);
    f_wrong_tsh   = (sa_tsh_info in (900001, 900002, 111111, 222222, 333333, 
                                     444444, 555555, 666666, 999999));
    f_wrong_med    = (idom_med_plus in (1,2));
    f_med_missing = (missing(idom_med_plus));
    f_no_levo     = (sa_tsh_info = 900000 and idom_med_plus in (0, -9) 
                     and missing(levothyroxine));
    
    /* Wenn keine der Bedingungen 1-5 erfüllt ist */
    f_valid       = (sum(of f_missing_tsh -- f_no_levo) = 0);
run;

proc means data=core_med;
var basis_age;
where f_missing_tsh = 0 and f_wrong_tsh = 0 and f_wrong_med = 0 and f_med_missing = 0;
run;

data levo_user;
set core_med;
where f_valid = 1;

	/*Exclusion criteria */
	f2_no_disease  = (d_an_met_4 = 2);
	f2_unsure_disease = (d_an_met_4 = 9999);
	f2_missing_disease = (missing(d_an_met_4));
    f2_lith = (Exclude_lith = 1);
	f2_cor = (Exclude_cor = 1);
	f2_am = (Exclude_am = 1);

	/* Wenn keine der Bedingungen 1-6 erfüllt ist */
    f2_valid       = (sum(of f2_no_disease -- f2_am) = 0);
run;


proc freq data=core_med;
table f_missing_tsh f_wrong_tsh f_med_missing f_wrong_med f_no_levo f_valid;
run;
proc freq data=levo_user;
table  f2_no_disease f2_unsure_disease f2_missing_disease f2_lith f2_cor f2_am f2_valid;
run;


*Apply inclusion and exclusion criteria;
data final;
set levo_user;
where  f2_valid = 1; 
run;

data final;
set final;
poly = .; *new variable for polypharmacy (>= 5 prescribed drugs);
if idom_count_med in (.) then poly = -9;
else if idom_count_med in (0,1,2,3,4) then poly = 0;
else if idom_count_med GE 5 then poly = 1;
run;

*Rename variable names and reduce dataset to variables needed for full model --> R Script;
data r_final (keep= ID basis_sex basis_age basis_ageclass basis_uort a_ses_isced11_level 
a_ses_mig_status a_smok_stat_qn a_packyears a_alk_riskant a_alk_audit_c 
a_anthro_bmi_eig a_anthro_bmi_eig_q a_anthro_gew_eig a_anthro_gew_eig_q a_anthro_groe_eig a_anthro_groe_eig_q
atc_combi d_an_metthyr3_5 
Jodetten Thyreostatika idom_exp_dauer a_an_metthyr1_age
poly d_an_metthyr3_2 d_an_metthyr3_3 d_an_metthyr3_4 d_an_metthyr2 d_im1 complete_dose sa_tsh sa_ft3 sa_ft4);
set final;
run;

data r_final;
set r_final;
rename basis_sex = sex;
rename basis_age = age;
rename basis_ageclass = ageclass;
rename basis_uort = center;
rename a_ses_isced11_level = education;
rename a_ses_mig_status = migration;
rename a_anthro_bmi_eig = bmi_stet;
rename a_anthro_bmi_eig_q = bmi_stet_q;
rename a_anthro_gew_eig = weight;
rename a_anthro_gew_eig_q = weight_q;
rename a_anthro_groe_eig = height;
rename a_anthro_groe_eig_q = height_q;
rename a_smok_stat_qn = smoking;
rename a_packyears = smoking_stet;
rename a_alk_riskant = alcohol;
rename a_alk_audit_c =alcohol_stet;
rename a_an_metthyr1_age = diagnosis_age;
rename idom_exp_dauer = intake_duration;
rename d_an_metthyr2 = current_treatment;
rename d_an_metthyr3_2 = radioiodine;
rename d_an_metthyr3_3 = radiation;
rename d_an_metthyr3_4 = surgery;
rename d_an_metthyr3_5 = iodine_sf;
rename d_im1 = general_practitioner;
run;

data r_final;
set r_final;
*Label vergeben;
LABEL 	sex = "Sex"
		age = "Age (years)"
		ageclass ="Age group (years)"
		center = "Study center"
		migration = "Migration background"
		education = "Education level"
		bmi_stet = "Body-mass-index (kg/m2)"
		intake_duration = "Duration of levothyroxine use (years)"
		atc_combi = "ATC code"
		Jodetten = "H03C intake"
		Thyreostatika = "H03B intake"
		sa_tsh = "TSH value"
		sa_ft3 = "FT3 value"
		sa_ft4 = "FT4 value"
		diagnosis_age = "Age at first thyroid diagnosis (years)"
		current_treatment = "Treatment within last 12 months"
		radioiodine = "History of radioiodine therapy"
		radiation = "Radiation in the neck area"
		surgery = "Thyroid surgery"
		iodine_sf = "Iodine intake"
		general_practitioner = "Time since last general practitioner visit (months)"
		poly = "Use of =5 different medications"
		smoking = "Smoking status"
	    smoking_stet = "Pack-years"
 		alcohol_stet = "AUDIT-C score"
		alcohol = "Risky alcohol consumption";
run;

PROC SORT data=r_final; by ID; run;

Data library.r_final;
set r_final;
run;

/*** Stops writing on LOG and OUTPUT file ***/
proc printto;
run;
