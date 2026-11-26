# Levothyroxine

## Name
Levothyroxine overtreatment (NAKO-319)

## Description
Many levothyroxine users have thyrotropin (TSH) levels below the diagnostic reference range (“diagnostic” cut-offs) and are therefore overtreated. Evidence from predominantly untreated cohorts has led to TSH cut-offs based on cardiovascular and all-cause mortality risk (“prognostic” cut-offs). The prevalence of (potential) overtreatment according to prognostic cut-offs remains unknown. We estimated the prevalence of (potential) overtreatment among levothyroxine users and assessed subgroups with a higher prevalence of low TSH according to the prognostic cut-off.

## Usage

### SAS 9.4

Run the SAS programs in the following order:


1. `1_Einlesen CORE.sas`
2. `2_Einlesen IDOM.sas`
3. `3_Analyse_IDOM.sas`
   - Requires `ATC.csv`, `H03AA_Wirkgehalt_250910.csv`, and the macro `Duplicates_removing_revised.sas`.
4. `4_Analyse_CORE_IDOM.sas`

### R

Run the multiple-imputation scripts to generate the imputed data used in the main analysis:

- `Multiple-imputation.R`
- `Multiple-imputation-delta.R`

Then run `Main_Analysis_REV_Oct.R` to produce publication-ready tables and figures.


## Data availability

Access to and use of data and biospecimens from the German National Cohort (NAKO) can be requested through the [NAKO data transfer portal](https://transfer.nako.de/transfer/index).

The program `3_Analyse_IDOM.sas` requires `ATC.csv` and `H03AA_Wirkgehalt_250910.csv`. These files may be requested from the authors for appropriate purposes.

## Acknowledgements

We thank NAKO study participants and staff.  

## Reference

**Potential overtreatment among levothyroxine users defined by diagnostic and prognostic thyrotropin (TSH) cut-offs: a population-based study** (2026).  
Brandes B, ..., Scholle O.  
Submitted to *eClinicalMedicine*.

## Corresponding author

**Oliver Scholle**  
Leibniz Institute for Prevention Research and Epidemiology – BIPS  
Bremen, Germany  
[scholle@leibniz-bips.de](mailto:scholle@leibniz-bips.de)


