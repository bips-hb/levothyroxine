*clear log and output;
dm 'clear log';
dm 'clear output';

/* Clean WORK library */
proc datasets lib=work kill nolist;
quit;

/*********************************************************************************/
*
** PROJECT:        NAKO_319
*
** PROGRAM NAME:   Duplicates_removing.sas
** AUTHOR:         Oliver Scholle
** VERSION/DATE:   V 1.0 / 2026-01-12
*
** SAS-VERSION     SAS 9.4
*
** PURPOSE:        Removing duplicates in NAKO IDOM data
*
*
/*********************************************************************************/;

*set date as macro variable;
data _null_;
	call symput('_date_today_',put(today(),yymmdd10.));
run;

/*********************************************************************************/
/*                                  BEGINNING                                    */
/*********************************************************************************/

/*--------------------------------------------------------------------
INPUT : input_ds (.sas7bdat)
OUTPUT: output_ds (.sas7bdat)

Participant id: ID

Keep only rows with IDOM_QUELLEN_AM containing "NACHERHEBUNG" (idom_quellen_am in (8,9) )
(case-insensitive). Keep ALL such rows.

For each NACHERHEBUNG row:
- If a crucial variable is missing/unknown (variable-specific rules below),
  backfill it from a non-NACHERHEBUNG row with the same ID AND the same
  STRK_MIKROG (in case it was not updated; if, then same ID is sufficient).
- If there is MORE THAN ONE non-NACHERHEBUNG row for the same ID+STRK_MIKROG,
  do NOT backfill automatically; instead:
    * flag the record via AMBIGUITY_FLAG = 1
    * stop the SAS run after dataset creation

Crucial variables and missing/unknown definitions:
- IDOM_EINNAHMEMODUS : "Keine Angabe"
- IDOM_EIN_INT       : "Keine Angabe" or empty
- IDOM_EXP_DAUER     : "Unbekannt"
- IDOM_HEUTE_EING    : "Keine Angabe"
- TAGESDOSIS         : empty (numeric missing)
- STRK_MIKROG        : empty (numeric missing)
--------------------------------------------------------------------*/
%MACRO duplicates_removing(input_ds, output_ds);

data _norm;
  set &input_ds;
run;

/* Split into NACHERHEBUNG vs others */
data _nach _other;
  set _norm;
  if idom_quellen_am in (8,9) then output _nach;
  else output _other;
run;

/* Count non-NACHERHEBUNG rows per ID + STRK_MIKROG */
proc sql;
  create table _other_cnt as
  select
      ID,
      STRK_MIKROG,
      count(*) as donor_n
  from _other
  group by
      ID,
      STRK_MIKROG
  ;
quit;

/* Count non-NACHERHEBUNG rows per ID (fallback when STRK_MIKROG differs in NACHERHEBUNG) */
proc sql;
  create table _other_id_cnt as
  select
      ID,
      count(*) as donor_n_id
  from _other
  group by ID
  ;
quit;

/* Keep only IDs where there is exactly ONE donor row (safe fallback donor)
Note: ID 271958 has three rows in "have_raw", as they were two NACHERHEBUNG for one original entry. */
proc sql;
  create table _other_id_one as
  select
      o.ID,
      /* donor values for fallback by ID */
      o.IDOM_EINNAHMEMODUS as IDOM_EINNAHMEMODUS_id,
      o.IDOM_EIN_INT       as IDOM_EIN_INT_id,
      o.IDOM_EXP_DAUER     as IDOM_EXP_DAUER_id,
      o.IDOM_HEUTE_EING    as IDOM_HEUTE_EING_id,
      o.TAGESDOSIS         as IDOM_TAGESDOSIS_id
  from _other as o
  inner join _other_id_cnt as c
    on o.ID = c.ID
  where c.donor_n_id = 1
  ;
quit;


/* Keep only unique donor rows (donor_n = 1) */
proc sort data=_other;
  by ID STRK_MIKROG;
run;

proc sort data=_other_cnt;
  by ID STRK_MIKROG;
run;

data _other_single;
  merge _other(in=a) _other_cnt(in=b);
  by ID STRK_MIKROG;
  if a and b and donor_n = 1;

  rename
    IDOM_EINNAHMEMODUS = IDOM_EINNAHMEMODUS_d
    IDOM_EIN_INT       = IDOM_EIN_INT_d
    IDOM_EXP_DAUER     = IDOM_EXP_DAUER_d
    IDOM_HEUTE_EING    = IDOM_HEUTE_EING_d
    TAGESDOSIS         = IDOM_TAGESDOSIS_d
  ;
  keep ID STRK_MIKROG donor_n
       IDOM_EINNAHMEMODUS IDOM_EIN_INT IDOM_EXP_DAUER IDOM_HEUTE_EING TAGESDOSIS;
run;

/* Sort NACHERHEBUNG rows */
proc sort data=_nach;
  by ID STRK_MIKROG;
run;

proc sort data=_other_id_cnt; by ID; run;
proc sort data=_other_id_one; by ID; run;

/* Merge STRK-based donor info and do STRK-based backfill */
data _want_step1;
  merge
    _nach(in=inN)
    _other_cnt(in=inC)
    _other_single(in=inO);
  by ID STRK_MIKROG;

  if not inN then delete;

  if not inC then donor_n = 0;

  /* Ambiguity flag (STRK-based) */
  length AMBIGUITY_REASON $160;
  AMBIGUITY_FLAG = (donor_n > 1);
  if AMBIGUITY_FLAG then
    AMBIGUITY_REASON = "Multiple non-NACHERHEBUNG rows for same ID+STRK_MIKROG";
  else
    AMBIGUITY_REASON = "";

  /* Backfill ONLY if exactly one STRK-based donor exists */
  if donor_n = 1 then do;

    if IDOM_EINNAHMEMODUS in (-999, .) then
      if inO and not missing(IDOM_EINNAHMEMODUS_d)
         and IDOM_EINNAHMEMODUS_d ne -999 then
        IDOM_EINNAHMEMODUS = IDOM_EINNAHMEMODUS_d;

    if IDOM_EIN_INT in (-999 , .) then
      if inO and not missing(IDOM_EIN_INT_d)
         and IDOM_EIN_INT_d ne -999 then
        IDOM_EIN_INT = IDOM_EIN_INT_d;

    if IDOM_EXP_DAUER in (-999, .) then
      if inO and not missing(IDOM_EXP_DAUER_d)
         and IDOM_EXP_DAUER_d ne -999 then
        IDOM_EXP_DAUER = IDOM_EXP_DAUER_d;

    if IDOM_HEUTE_EING in (-999, .) then
      if inO and not missing(IDOM_HEUTE_EING_d)
         and IDOM_HEUTE_EING_d ne -999 then
        IDOM_HEUTE_EING = IDOM_HEUTE_EING_d;

    if missing(TAGESDOSIS) then
      if inO and not missing(IDOM_TAGESDOSIS_d) then
        TAGESDOSIS = IDOM_TAGESDOSIS_d;

  end;

  drop IDOM_EINNAHMEMODUS_d IDOM_EIN_INT_d IDOM_EXP_DAUER_d
       IDOM_HEUTE_EING_d IDOM_TAGESDOSIS_d;
run;

/* Add ID-only donor counts and ID-only donor values, then do fallback backfill */
proc sort data=_want_step1; by ID; run;

data &output_ds;
  merge
    _want_step1(in=inW)
    _other_id_cnt(in=inCid)
    _other_id_one(in=inOid);
  by ID;

  if not inW then delete;

  if not inCid then donor_n_id = 0;

  /* Ambiguity: STRK-based match absent, but multiple ID-level donors exist */
  if donor_n = 0 and donor_n_id > 1 then do;
    AMBIGUITY_FLAG = 1;
    AMBIGUITY_REASON = "Multiple non-NACHERHEBUNG rows for same ID (STRK_MIKROG differs in NACHERHEBUNG)";
  end;

  /* Fallback backfill ONLY if no STRK-based donor exists and exactly one ID-level donor exists */
  if donor_n = 0 and donor_n_id = 1 then do;

    if IDOM_EINNAHMEMODUS in (-999, .) then
      if inOid and not missing(IDOM_EINNAHMEMODUS_id)
         and IDOM_EINNAHMEMODUS_id ne -999 then
        IDOM_EINNAHMEMODUS = IDOM_EINNAHMEMODUS_id;

    if IDOM_EIN_INT in (-999 , .) then
      if inOid and not missing(IDOM_EIN_INT_id)
         and IDOM_EIN_INT_id ne -999 then
        IDOM_EIN_INT = IDOM_EIN_INT_id;

    if IDOM_EXP_DAUER in (-999, .) then
      if inOid and not missing(IDOM_EXP_DAUER_id)
         and IDOM_EXP_DAUER_id ne -999 then
        IDOM_EXP_DAUER = IDOM_EXP_DAUER_id;

    if IDOM_HEUTE_EING in (-999, .) then
      if inOid and not missing(IDOM_HEUTE_EING_id)
         and IDOM_HEUTE_EING_id ne -999 then
        IDOM_HEUTE_EING = IDOM_HEUTE_EING_id;

    if missing(TAGESDOSIS) then
      if inOid and not missing(IDOM_TAGESDOSIS_id) then
        TAGESDOSIS = IDOM_TAGESDOSIS_id;

  end;

  drop donor_n_id
       IDOM_EINNAHMEMODUS_id IDOM_EIN_INT_id IDOM_EXP_DAUER_id
       IDOM_HEUTE_EING_id IDOM_TAGESDOSIS_id donor_n;
run;

/* Check durchf?hren */
%let abort_run = 0;

data _null_;
  set &output_ds end=last;
  if AMBIGUITY_FLAG = 1 then do;
    putlog "ERROR: Ambiguity detected. ID: " ID;
    call symputx('abort_run', 1);
  end;
run;

%if &abort_run. = 1 %then %do;
   %put ERROR: Aborting run due to ambiguity.;
   %abort cancel;
%end;


/*********************************************************************************/
/*                                  FINISHED                                     */
/*********************************************************************************/
%MEND duplicates_removing;

