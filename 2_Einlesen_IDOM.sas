*specify file to which log is written;
%global dat_log;
%let dat_log = %sysfunc(today(),date9.);
%let levothyroxine=%sysget(USERPROFILE)\levothyroxine;
%put &=levothyroxine;

filename newlog "&levothyroxine\logs\idom&dat_log..log";
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
* Date: July 2024        			;
*									;
************************************;

DM output 'clear';
DM log 'clear';

libname library '&levothyroxine\dats\';

PROC IMPORT DATAFILE= "&levothyroxine\dats\restore\export_IDOM Auswertung_baseline.csv"
OUT=nako_319_idom
DBMS=csv
REPLACE;
delimiter=';';
GETNAMES=YES;
RUN; 

PROC IMPORT DATAFILE= "&levothyroxine\dats\restore\export_IDOM Auswertung_baseline_1.csv"
OUT=nako_319_idom_1
DBMS=csv
REPLACE;
delimiter=';';
GETNAMES=YES;
RUN; 

proc sort data=nako_319_idom; by ID; run;
proc sort data=nako_319_idom_1; by ID; run;

data idom;
merge nako_319_idom (in=a) nako_319_idom_1;
by ID;
if a;
run;


************************************************Formate;
Proc format library=library;
value	idom_ein_intv	1	=	"t?glich"
		2	=	"jeden 2.Tag"
		3	=	"jeden 3.Tag"
		4	=	"jeden 4.Tag = 2 x pro Woche"
		5	=	"im Abstand von 1 Woche und mehr"
		6	=	"im Abstand von 1 Monat und mehr"
		7	=	"im Abstand von 1 Jahr und mehr"
		-999 = "Keine Angabe"
; value	idom_einnahmemodus	1	=	"regelm??ig"
		2	=	"nach Bedarf"
		-999 = "Keine Angabe"
; value	idom_expos_dauer	1	=	"3 Jahre und l?nger"
		2	=	"1 Jahr bis < 3 Jahre"
		3, 4, 5, 6, 7	=	"}< 1 Jahr"
		-999, .   =   "Unbekannt"
; value	idom_heute_eing	1	=	"}ja"
		2	=	"nein"
		-999   =   "Keine Angabe"
; value	jn	1	=	"ja"
		2	=	"nein"
; value source 1 = "Medikamentenpackung: Einscannen der PZN"
	2 =	"Medikamentenpackung: Tastatureingabe der PZN"
	3 =	"Medikamentenpackung: Arzneimittelname" 
	4 =	"Beipackzettel" 
	5 =	"Medikamentenplan" 
	6 =	"Rezept" 
	7 =	"Probandenangaben" 
	8 =	"NACHERHEBUNG (Telefon/Zettel): Tastatureingabe der PZN" 
	9 =	"NACHERHEBUNG (Telefon/Zettel): Arzneimittelname" 
	10 = "Medikamentenpackung: nachtr?gliche Tastatureingabe der PZN im aktuellen Formular"
;
run;

data idom;
set idom;

*Label vergeben;
LABEL 	
		idom_arz_bez   = "Arzneimittelbezeichnung"
		idom_einnahmemodus = "Einnahme regelm??ig oder nach Bedarf?"
		idom_ein_int   = "Einnahmeintervall"
		idom_exp_dauer  = "Dauer der aktuellen Einnahmeperiode"
		idom_heute_eing  = "Medikament heute eingenommen?"
		idom_atc_201805 = "ATC-Code nach GKV AI 2018/05"
		idom_tagesdosis = "Dosis pro Tag der Einnahme"
		idom_pha_zen_num = "Pharmazentralnummer"
		idom_quellen_am = "Quelle der Arzneimittelangabe"
		;


FORMAT
idom_ein_int	idom_ein_intv.
idom_einnahmemodus	idom_einnahmemodus.
idom_exp_dauer	idom_expos_dauer.
idom_heute_eing	idom_heute_eing.
idom_quellen_am source.
;
run;


*****************************************Datensatz ablegen;
PROC SORT data=idom; by ID; run;
Data library.idom;
set idom;
drop idom_am_exposition idom_dos_einheit unt194_status;
run;

/*** Stops writing on LOG and OUTPUT file ***/
proc printto;
run;
*** Deletes WORKSPACE ***;
proc datasets lib=work nolist;
	delete Nako_319_idom Nako_319_idom_1 idom;
quit;
