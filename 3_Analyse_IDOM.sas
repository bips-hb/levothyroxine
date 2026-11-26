*specify file to which log is written;
%global dat_log;
%let dat_log = %sysfunc(today(),date9.);
%let levothyroxine=%sysget(USERPROFILE)\levothyroxine;
%put &=levothyroxine;
filename newlog "&levothyroxine\logs\idomdataprocessing&dat_log..log";
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

/*** Declaration of Libnames ***/
libname library '&levothyroxine\dats';

/*** Including Macros ***/
%include '&levothyroxine\macros\duplicates_removing.sas';

/**************************************/
/*** Read study specific code lists ***/
/**************************************/

/*** Study specific ATC Codes ***/
data q_atc;
	attrib VAR_GRP length=$10 VAR_NAME length=$20 ATC length=$7 LABEL_NAME length=$22 TEXT length=$300 
		   SUBSTRING length=4;
	infile '&levothyroxine\dats\ATC.csv'
			dlm=';' missover DSD firstobs=2 lrecl=1000 termstr=CRLF;
	input VAR_GRP VAR_NAME ATC LABEL_NAME TEXT SUBSTRING;
run;


/***Wirkgehalt H03AA Pr?parate******/
PROC IMPORT DATAFILE= "&levothyroxine\dats\restore\H03AA_Wirkgehalt_250910.csv"
OUT=dosage
DBMS=csv
REPLACE;
delimiter=',';
GETNAMES=YES;
guessingrows=MAX; 
RUN; 

Proc format library=library;
value	pzn	0 = "Wirkgehalt ?ber PZN"
		1 = "Wirkgehalt ?ber idom_arz_bez"
		2 = "Wirkgehalt ?ber idom_arz_bez, da PZN nicht korrekt"
		3, 4, 5 = "keine Angabe m?glich"
		;
value combi 0 = "Monopr?parate der Gruppe H03AA01"
	  1 = "Mindestens ein Kombipr?parat aus der Gruppe H03AA51"
	  ;
run;

/*************************/
/*** Select ATCs ***/
/*************************/
%macro sel_codes();

proc sql;

create table idom_atc as

select a.*, b.atc as atc_list

from library.idom a inner join q_atc b

on (b.substring=2 and (substr(a.idom_atc_201805,1,7)=b.atc))
or (b.substring=0 and (substr(a.idom_atc_201805,1,4)=b.atc))
or (b.substring=1 and (substr(a.idom_atc_201805,1,3)=b.atc)) 
;

quit;

%mend sel_codes;



%sel_codes();


*Liste auf Personen begrenzen, die LT4 einnehmen;
PROC SQL;
    /* Schritt 1: Identifizieren der IDs, die die Bedingung erf?llen */
    /* Diese SELECT-Anweisung gibt alle IDs zur?ck, f?r die mindestens ein Eintrag die Bedingung 'idom_atc_201805 in ('H03AA01', 'H03AA51')' erf?llt */
    CREATE TABLE IDs_LTV AS
    SELECT DISTINCT ID
    FROM idom_atc
    WHERE idom_atc_201805 in ('H03AA01', 'H03AA51');
QUIT;

PROC SQL;
    /* Schritt 2: Nur LT4 Einnehmende behalten und in neuer Tabelle abspeichern*/
    CREATE TABLE idom_ltv AS
    SELECT *
    FROM idom_atc
    WHERE ID IN (SELECT ID FROM IDs_LTV);
QUIT;


********************************************************************************************************;
* Datensatz "medication" erstellen, reduziert auf eine Zeile pro ID, der angibt, ob zus?tzlich zum LT4 
noch Thyreoastatika oder Jodetten oder Amiodaron oder Lithium eingenommen werden;
*group 1;
	proc sql;
		create table group_1 as
		select distinct a.ID, 1 as Levothyroxine 
			from idom_ltv a
				where a.atc_list in ('H03AA01', 'H03AA51')
		order by ID;
	quit;

*group 2;
	proc sql;
		create table group_2 as
		select distinct a.ID, 1 as Thyreostatika 
			from idom_ltv a
				where a.atc_list in ('H03B')
		order by ID;
	quit;

*group 3;
	proc sql;
		create table group_3 as
		select distinct a.ID, 1 as Jodetten
			from idom_ltv a
				where a.atc_list in ('H03C')
		order by ID;
	quit;
*group 4;
	proc sql;
		create table group_4 as
		select distinct a.ID, 1 as Exclude_am
			from idom_ltv a
				where a.atc_list in ('C01BD01')
		order by ID;
	quit;
*group 5;
	proc sql;
		create table group_5 as
		select distinct a.ID, 1 as Exclude_cor
			from idom_ltv a
				where a.atc_list in ('H02')
		order by ID;
	quit;
*group 6;
	proc sql;
		create table group_6 as
		select distinct a.ID, 1 as Exclude_lith
			from idom_ltv a
				where a.atc_list in ('N05AN01')
		order by ID;
	quit;

* all groups together;
data medication;
	merge group_1 group_2 group_3 group_4 group_5 group_6;
	by ID;
run;
********************************************************************************************************;

* St?rke "strk_mikrog" aus Datei von Uwe anspielen;
proc sql;
create table test as
select a.*, b.strk_mikrog as strk_mikrog, b.strk_knz as strk_knz
from idom_ltv a inner join dosage b
on (a.idom_pha_zen_num=b.idom_pha_zen_num and a.idom_arz_bez=b.idom_arz_bez);
quit;

*Variable f?r Kombipr?parat dichotomisieren;
data test;
set test;
if idom_atc_201805 in ('H03AA01') then combi = 0; *Monopr?parat;
else if idom_atc_201805 in ('H03AA51') then combi = 1; *Kombipr?parat;
else combi = .;
run;

********************************************************************************************************;
* Hier Algorithmus zum Umgang mit invaliden Angaben der Variable idom_tagesdosis;
********************************************************************************************************;
proc sort data=test; by ID; run;
* Korrekturen der Variable Tagesdosis**********************************************************;
* Entspricht in der Datei copy die Tagesdosis der 1-fachen St?rke, dann wird die Tagesdosis auf 1 gesetzt.
* Entspricht in der Datei copy die Tagesdosis der 2-fachen St?rke, dann wird die Tagesdosis auf 2 gesetzt.
* Entspricht in der Datei copy die Tagesdosis der 0,5-fachen St?rke, dann wird die Tagesdosis auf 0.5 gesetzt.
* Bei allen anderen als invalide eingestuften Angaben wird die Tagesdosis auf missing (.) gesetzt.;
data test_corr; 
set test;
if 	idom_tagesdosis in (0.25, 0.5, 0.75, 1, 1.25, 1.5, 2, 2.5, 3, 4, 5) then tagesdosis = idom_tagesdosis;
else if strk_mikrog = . then tagesdosis = .;
else if idom_tagesdosis = strk_mikrog then tagesdosis = 1;
else if idom_tagesdosis = 2 * strk_mikrog then tagesdosis = 2;
else if idom_tagesdosis = 0.5 * strk_mikrog then tagesdosis = 0.5;
else tagesdosis = .;
drop atc_list idom_tagesdosis;
run;

********************************************************************************************************;
* Hier Algorithmus zum Umgang mit Duplicates;
********************************************************************************************************;

*Identifikation von IDs mit mind. einer Zeile, die als Nacherhebung gekennzeichnet ist;
PROC SQL;
    /* Schritt 1: Identifizieren der IDs, die die Bedingung erf?llen */
    /* Diese SELECT-Anweisung gibt alle IDs zur?ck, f?r die mindestens ein Eintrag die Bedingung 
	'idom_quellen_am in (8,9)' erf?llt */
    CREATE TABLE IDs_Nacherhebung AS
    SELECT DISTINCT ID
    FROM test_corr
    WHERE idom_quellen_am in (8,9);
QUIT;
*Identifikation von IDs mit mind. einer Zeile, die nicht als Nacherhebung gekennzeichnet ist;
PROC SQL;
    /* Schritt 2: Identifizieren der IDs, die die Bedingung erf?llen */
    /* Diese SELECT-Anweisung gibt alle IDs zur?ck, f?r die mindestens ein Eintrag die Bedingung 
	'idom_quellen_am in (1,2,3,4,5,6,7,10)' erf?llt */
    CREATE TABLE IDs_ohne_Nacherhebung AS
    SELECT DISTINCT ID
    FROM test_corr
    WHERE idom_quellen_am in (1,2,3,4,5,6,7,10);
QUIT;
PROC SQL;
    /* Schritt 3: Behalten aller Zeilen aus der IDs_ohne_Nacherhebung, deren ID ebenfalls in der erstellten Liste 
	mit Nacherhebung enthalten ist;*/
	CREATE TABLE IDs_Duplicates AS
    SELECT ID
	FROM IDs_ohne_Nacherhebung
    WHERE ID IN (SELECT ID FROM IDs_Nacherhebung);
QUIT;
*Datensatz, der alle Duplicates enth?lt, erstellen;
data library.all_duplicates;
merge test_corr IDs_duplicates (in=a);
by ID;
if a;
run;
*Macro aufrufen;
%duplicates_removing(input_ds=library.all_duplicates, output_ds=library.corr_duplicates)
PROC SQL;
    /* Schritt 4: L?schen aller Zeilen aus der Originaltabelle, deren ID eine Nacherhebung mit Duplikat hatte*/
    DELETE FROM test_corr 
    WHERE ID IN (SELECT ID FROM IDs_Duplicates);
QUIT;
*Add corrected entries;
proc sql;
    create table combined_data as
    select * from test_corr
    union all
    select * from library.corr_duplicates;
quit;


********************************************************************************************************;
*Algorithmus zur Bestimmung der t?glichen Dosis;
data daily;
set combined_data;
* Schritt 1: 1.	For each entry, first calculate from IDOM_EIN_INT the mean number of intakes per day (referred to as XXX);
if 		idom_ein_int = 1 then strk_mikrog_perday = strk_mikrog;
else if idom_ein_int = 2 then strk_mikrog_perday = strk_mikrog/2;
else if idom_ein_int = 3 then strk_mikrog_perday = round(strk_mikrog/3);
else if idom_ein_int = 4 then strk_mikrog_perday = round(strk_mikrog/4);
else if idom_ein_int = 5 then strk_mikrog_perday = round(strk_mikrog/7);
else if idom_ein_int = 6 then strk_mikrog_perday = round(strk_mikrog/30);
else if idom_ein_int in (-999, .) then strk_mikrog_perday = .; *Keine Angabe oder fehlend;
strk_mikrog_perday = strk_mikrog_perday * tagesdosis; * if tagesdosis is missing, strk_mikrog_perday is set to missing;
run;

*IDs mit mind. einer Zeile mit missing bei strk_mikrog_perday identifizieren und auf missing setzen;
proc sort data=daily; by ID; run;
PROC SQL;
    /* Schritt 1: Identifizieren der IDs, die die Bedingung erf?llen */
    /* Diese SELECT-Anweisung gibt alle IDs zur?ck, f?r die mindestens ein Eintrag die Bedingung 'strk_mikrog_perday = .' erf?llt */
    CREATE TABLE IDs_ohne_Dosis AS
    SELECT DISTINCT ID
    FROM daily
    WHERE strk_mikrog_perday = .;
QUIT;
PROC SQL;
	/* Schritt 2: Variable missing_value_flag setzen */
    CREATE TABLE daily_corr AS
    SELECT
		a.*,
        CASE
            WHEN daily.ID IN (SELECT ID FROM IDs_ohne_Dosis) THEN 1  /* IDs  bekommen '1' */
            ELSE 0                                 /* Alle anderen IDs bekommen '0' */
        END AS missing_value_flag
    FROM daily as a;
QUIT;

proc sort data=daily_corr; by ID; run;
proc sort data=combined_data; by ID; run;

*Datensatz pro ID mit aufsummierter Dosis;
PROC SQL;
  CREATE TABLE levo_dose AS
  SELECT ID,
         SUM(strk_mikrog_perday) AS complete_dose, MAX(strk_knz) AS valid_dose, MAX(missing_value_flag) AS miss
  FROM daily_corr
  GROUP BY ID;
QUIT;

*Probanden mit missing Information in einer Zeile werden insgesamt auf missing gesetzt;
data levo_dose;
set levo_dose;
if miss = 1 then complete_dose = .;
drop valid_dose miss;
run;

*Datensatz pro ID auf eine Zeile reduzieren mit Infos zur Einnahme;
PROC MEANS
	noprint
		data=combined_data;
			output out=levo_intake
			min(idom_exp_dauer) = idom_exp_dauer
			max(idom_heute_eing) = idom_heute_eing
			max(combi) = atc_combi;
			by ID;
run;

data levo_intake;
set levo_intake;
drop _type_ _freq_;
run;

data medication;
set medication;
if Thyreostatika = 1 then Thyreostatika = 1;
else Thyreostatika = 0;
if Jodetten = 1 then Jodetten = 1;
else Jodetten = 0;
run;


********************************************************************************************************;
*Finalen Datensatz f?r weitergehende Analyse ablegen;
data library.medication_neu;
merge medication (in=a)levo_dose levo_intake;
by ID;
if a;
label complete_dose="Levo: durchschnittliche Tagesdosis" idom_exp_dauer="Levo: Dauer der aktuellen Einnahmeperiode"
idom_heute_eing="Levo: Medikament heute eingenommen?" atc_combi ="Mindestens ein Kombipr?parat aus der Gruppe H03AA51";
format atc_combi combi.;
run;

/*** Stops writing on LOG and OUTPUT file ***/
proc printto;
run;
