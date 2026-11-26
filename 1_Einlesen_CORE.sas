*specify file to which log is written;
%global dat_log;
%let dat_log = %sysfunc(today(),date9.);
%let levothyroxine=%sysget(USERPROFILE)\levothyroxine;
%put &=levothyroxine;
filename newlog "&levothyroxine\logs\core&dat_log..log";
proc printto log=newlog new;
run;


*save and write time of start of program run;
data _null_;call symput('anfang',put(datetime(),datetime16.));run;
data _null_;call symput('banfang',put(datetime(),Best.));run;

%put Anfang: &anfang;
DM output 'clear';

************************************;
* Project: NAKO 319      			;
*									;
* Author: Brandes, B. 				;
* Date: July 2024        			;
*									;
************************************;

libname library '&levothyroxine\dats\';

PROC IMPORT DATAFILE= "&levothyroxine\dats\restore\export_baseline.csv"
OUT=nako_319
DBMS=csv
REPLACE;
delimiter=';';
GETNAMES=YES;
RUN; 

PROC IMPORT DATAFILE= "&levothyroxine\dats\restore\export_baseline_1.csv"
OUT=nako_319_1
DBMS=csv
REPLACE;
delimiter=';';
GETNAMES=YES;
RUN; 

proc sort data=nako_319; by ID; run;
proc sort data=nako_319_1; by ID; run;

data thyroid;
merge nako_319 (in=a) nako_319_1;
by ID;
if a;
run;


************************************************Formate;
Proc format library=library;
value	alk_konsum	0	=	"}kein Alkoholkonsum"
		1	=	"fr?herer Alkoholkonsum"
		2	=	"aktueller Alkoholkonsum"
; value	Altersklasse	10	=	"<= 19"
		20, 30	=	"}20 - 39"
		40	=	"40 - 49"
		50	=	"50 - 59"
		60, 70, 80, 90, 100	=	">=60"
; value	anthro_qual	0	=	"gemessen und automatisch ?bertragen"
		1	=	"gemessen und manuell ?bertragen"
		2	=	"Eigenangabe"
		7777	=	"nicht ermittelbar (fehlende Werte, keine Angabe, wei? nicht)"
; value	anthro_qual2a	0	=	"gemessen und automatisch ?bertragen"
		1	=	"gemessen und manuell ?bertragen"
		7777	=	"nicht ermittelbar (fehlende Werte, keine Angabe, wei? nicht)"
; value	Arztbesuch	1	=	"}Innerhalb der letzten 4 Wochen"
		2	=	"Innerhalb der letzten 2 bis 3 Monate"
		3	=	"Innerhalb der letzten 4 bis 12 Monate"
		4	=	"Vor mehr als einem Jahr"
		8	=	"Keine Angabe"
		9	=	"Wei? nicht"
; value	audit_c	0	=	"0"
		1	=	"1"
		2	=	"2"
		3	=	"3"
		4	=	"4"
		5	=	"5"
		6	=	"6"
		7	=	"7"
		8	=	"8"
		9	=	"9"
		10	=	"10"
		11	=	"11"
		12	=	"12"
; value	Beeintraechtigung	2	=	"Nicht beeintr?chtigt"
		11	=	"Wenig beeintr?chtigt"
		12	=	"Stark beeintr?chtigt"
		88	=	"Keine Angabe"
		99	=	"Wei? nicht"
; value	geschlecht	1	=	"}M?nnlich"
		2	=	"Weiblich"
; value	Gewichtsklassen	0	=	"Untergewicht"
		1	=	"}Normalgewicht"
		2	=	"Pr?adipositas"
		3,4,5	=	"Adipositas Grad I-III"
		7777	=	"nicht ermittelbar (fehlende Werte, keine Angabe, wei? nicht)"
; value	global_missing	7775	=	"Erlaubt ?bersprungen"
		7776	=	"Berechnung nicht m?glich wegen unplausibler Werte"
		7777	=	"Berechnung nicht m?glich wegen fehlender Daten"
		8886	=	"Unplausible Angabe, daher im Rahmen der QS gel?scht"
		8888	=	"Keine Angabe"
		9999	=	"Wei? Nicht"
; value	j_n_nm	0	=	"Nein"
		1	=	"Ja"
		7777	=	"Berechnung nicht m?glich wegen fehlender oder unplausibler Daten"
; value	ja_nein	0	=	"nein"
		1	=	"ja"
; value	jn	1	=	"ja"
		2	=	"nein"
; value	Mehrfachauswahl	1	=	"Ja"
		2	=	"}Nein"
		8888	=	"Keine Angabe"
		9999	=	"Wei? nicht"
; value	njne	0	=	"nein"
		1	=	"ja"
; value	rauch_status	1	=	"}Kein Raucher, auch nicht ehemalig"
		2	=	"Ehemaliger Raucher"
		3	=	"Raucher"
		4	=	"Rauchstatus unbekannt"
; value	sofa_info	111111	=	"Probe zu alt"
		222222	=	"H?molytische Probe"
		333333	=	"Lip?mische Probe"
		444444	=	"Technisch nicht m?glich"
		555555	=	"Planm??ig nicht bestimmt"
		666666	=	"Durch Expertengruppe eliminiert"
		900000	=	"Exakter Messwert"
		900001	=	"Messwert unterhalb der angef?hrten Bestimmungsgrenze"
		900002	=	"Messwert oberhalb der angef?hrten Bestimmungsgrenze"
		999999	=	"Fehlender Wert, keine g?ltigen Angaben vorhanden"
; value	Soziodemographi10a	1	=	"low"
		2	=	"}medium"
		3	=	"high"
; value	Soziodemographi14a	0	=	"}kein Migrationshintergrund"
		1	=	"Migrationshintergrund"
		-9 	=   "Migrationshintergrund nicht bestimmbar"
; value	Soziodemographi9a	1,2	=	"Low"
		3,4	=	"}Medium"
		5,6,7,8	=	"High"
		-99, -88, -9, 0	=	"Still in training or Unknown"
; value	ts_JaNein_WnKaHid	1	=	"Ja"
		2	=	"Nein"
		8	=	"Keine Angabe"
		9	=	"Wei? nicht"
; value	ts_key_wnHid_kaHid	7777	=	"unplausible Angabe"
		8888	=	"Keine Angabe"
		9999	=	"Wei? nicht"
; value	uort	0	=	"nicht zugeordnet"
		11	=	"Augsburg"
		12	=	"Regensburg"
		21	=	"Mannheim"
		22	=	"Freiburg"
		23	=	"}Saarbr?cken"
		31	=	"Essen"
		32	=	"M?nster"
		33	=	"D?sseldorf"
		41	=	"Halle"
		42	=	"Leipzig"
		51	=	"Berlin Nord"
		52	=	"Berlin Mitte"
		53	=	"Berlin S?d"
		61	=	"Hannover"
		62	=	"Hamburg"
		63	=	"Bremen"
		71	=	"Kiel"
		81	=	"Neubrandenburg"
		811	=	"Neustrelitz"
		812	=	"Waren (M?ritz)"
		813	=	"Demmin"
; 	
run;

data thyroid (keep= ID basis_sex basis_age basis_ageclass basis_uort a_ses_isced11_level 
a_ses_mig_status a_smok_stat_qn a_packyears a_alk_riskant a_alk_audit_c 
a_anthro_gew_eig
a_anthro_gew_eig_q
a_anthro_groe_eig
a_anthro_groe_eig_q
a_anthro_bmi_eig  
a_anthro_bmi_eig_q
a_anthro_bmi_eig_gkl
d_an_met_4 
a_an_metthyr1_age 
d_an_metthyr2 
d_an_metthyr3_1 
d_an_metthyr3_2 
d_an_metthyr3_3 
d_an_metthyr3_4 
d_an_metthyr3_5 
d_an_metthyr3_6
d_im1 d_im2_2
sa_tsh sa_ft3 sa_ft4
sa_tsh_info
idom_count_med idom_med_plus);
set thyroid;

*Label vergeben;
LABEL 	basis_sex = "Sex"
		basis_age = "Age at sample (years)"
		basis_ageclass ="Ageclass"
		basis_uort = "Study center"

		a_ses_mig_status = "Migrationshintergrund (J/N)"
		a_ses_isced11_level = "H?chster Bildungsabschluss nach ISCED 11"

		sa_tsh = "TSH value"
		sa_tsh_info = "tsh: Zusatzinfo zum Messwert"
		sa_ft3 = "FT3 value"
		sa_ft4 = "FT4 value"
	
		d_an_met_4 = "Schilddr?senerkrankung"
		a_an_metthyr1_age = "Age at diagnosis (years)"
		d_an_metthyr2 = "?rztliche Behandlung der Schilddr?senerkrankung innerhalb der letzten 12 Monate"
		d_an_metthyr3_1 = "Schilddr?senhormone"
		d_an_metthyr3_2 = "Radiojodtherapie"
		d_an_metthyr3_3 = "Bestrahlung im Halsbereich"
		d_an_metthyr3_4 = "Schilddr?sen-OP"
		d_an_metthyr3_5 = "Jodtabletten"
		d_an_metthyr3_6 = "Andere Behandlung"

		idom_count_med = "Anzahl der Medikamente, die der Proband in den letzten 7 Tagen eingenommen hat"
		idom_med_plus = "Hinweise auf Unstimmigkeiten oder weitere Medikamente"

		a_smok_stat_qn = "Rauchstatus"
	    a_packyears = "Verpackungsjahre"

 		a_alk_audit_c = "AUDIT-C Score"
		a_alk_riskant = "Riskanter Alkoholkonsum"
		
		d_im1  = "Wann waren Sie zum letzten Mal bei einem niedergelassenen Arzt?"
		d_im2_2  = "Arzt f?r Innere Medizin / Internist"

		a_anthro_gew_eig  = "Gewicht (aus bereinigten Messwerten und Eigenangaben)"
		a_anthro_gew_eig_q = "Qualit?t Gewicht (aus bereinigten Messwerten und Eigenangaben)"
		a_anthro_groe_eig = "Groesse (aus bereinigten Messwerten und Eigenangaben)"
        a_anthro_groe_eig_q = "Qualitaet Groesse (aus bereinigten Messwerten und Eigenangaben)"
		a_anthro_bmi_eig  = "BMI (aus bereinigten Messwerten und Eigenangaben)"
		a_anthro_bmi_eig_q = "Qualit?t BMI (aus bereinigten Messwerten und Eigenangaben)"
		a_anthro_bmi_eig_gkl  = "Gewichtsklassen (anhand BMI aus Messwerten und Eigenangaben, bereinigt)"
;


FORMAT
basis_ageclass	Altersklasse.
d_im1	Arztbesuch2.
a_alk_audit_c	audit_c.
basis_sex	geschlecht.
a_anthro_bmi_eig_gkl	Gewichtsklassen.
a_anthro_bmi_eig
a_anthro_gew_eig
a_anthro_groe_eig
a_an_metthyr1_age	global_missing.
a_anthro_gew_eig_q
a_anthro_groe_eig_q
a_anthro_bmi_eig_q anthro_qual.
d_an_met_4	
d_an_metthyr2	
d_an_metthyr3_1	
d_an_metthyr3_2	
d_an_metthyr3_3	
d_an_metthyr3_4	
d_an_metthyr3_5	
d_an_metthyr3_6 	Mehrfachauswahl.
a_alk_riskant	njne.
a_smok_stat_qn	rauch_status.
sa_tsh	
sa_tsh_info	
sa_ft3	
sa_ft4	sofa_info.
a_ses_mig_status	Soziodemographi14a.
a_ses_isced11_level Soziodemographi9a.
d_im2_2	ts_key_wnHid_kaHid.
basis_uort	uort.
;
run;

proc means data=work.thyroid n nmiss min max;
    var _numeric_;
    output out=work.missings
        n= nmiss= min= max=;
run;


*****************************************bereinigten Datensatz ablegen;
data library.core_cleaned;
set thyroid;

array bbb basis_sex basis_ageclass a_ses_mig_status;
do over bbb;
if bbb = -9 or bbb = -8 then bbb = .;
end;

array eee d_im1;
do over eee;
if eee = 8 or eee = 9 then eee = .;
end;

array ddd a_alk_riskant a_alk_audit_c;
do over ddd;
if ddd = -99 then ddd =.;
end;

array ccc d_im2_2;
do over ccc;
if ccc = 7777 or ccc = 8888 or ccc = 9999 then ccc = .;
end;

array iii a_an_metthyr1_age a_packyears a_anthro_bmi_eig a_anthro_gew_eig a_anthro_groe_eig a_anthro_groe_eig_q a_anthro_bmi_eig_q a_anthro_gew_eig_q a_anthro_bmi_eig_gkl;
do over iii;
if iii = 7775 or iii = 7776 or iii = 7777 or iii = 8886 or iii = 8888 or iii = 9999 then iii = .;
end;

array zzz d_an_met_4 d_an_metthyr2 d_an_metthyr3_1  d_an_metthyr3_2  d_an_metthyr3_3  d_an_metthyr3_4  d_an_metthyr3_5 d_an_metthyr3_6;
do over zzz;
if zzz = 8888 or zzz = 9999 then zzz = .;
end; 

if a_smok_stat_qn = 4 then a_smok_stat_qn =.;
if a_ses_isced11_level = -99 or a_ses_isced11_level = -88 or a_ses_isced11_level =-9 then a_ses_isced11_level =.;

run;

proc means data=library.core_cleaned n nmiss min max;
    var _numeric_;
    output out=work.missings_clean
        n= nmiss= min= max=;
run;

/*** Stops writing on LOG and OUTPUT file ***/
proc printto;
run;
*** Deletes WORKSPACE ***;
proc datasets lib=work nolist;
	delete nako_319 nako_319_1 thyroid;
quit;
