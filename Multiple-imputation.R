library(mice)
library(risks)
library(dplyr)
library(purrr)
library(forcats)

# load data ---------------------------------------------------------------
load(file = "outputs/main.rda")  # from Main_Analysis_REV_Oct.R


# Imputation --------------------------------------------------------------
## 1) remove dump variables (such as constants, id_no, etc.) --------------
# I drop region because I think center is more informative.
outlist <- c(
  "ageclass",
  "alcohol",
  "bmi_stet",
  "bmi_stet_q",
  "height_q",
  "weight_q"
)

## Never smokers have NA in packyears (smoking_stet). I set these NAs to 0 packyears
## Centers have many levels without data. Drop unnecessary levels in all variables
analysis <- main |>
  mutate(
    smoking_stet = if_else(smoking == "Never", 0, smoking_stet),
    across(where(is.factor), fct_drop)
  )


## 2) Initiliase ----------------------------------------------------------
analysis <- dplyr::select(analysis, -all_of(outlist))
meth <- make.method(analysis, defaultMethod = "rf")
pred <- make.predictorMatrix(analysis)
post <- make.post(analysis)
pred[, "ID"] <- 0
pred["ID", ] <- 0
# Diagnosis age must be smaller than age
post[
  which(names(post) == "diagnosis_age")
] <- "imp[[j]][, i] <- pmin(imp[[j]][, i], data$age[where[, j]])"

ini <- mice(
  analysis,
  method = meth,
  predictorMatrix = pred,
  post = post,
  maxit = 0
)
names(ini$nmis[ini$nmis == 0])
# which variable has more than 20% missing values
names(ini$nmis[ini$nmis > nrow(analysis) / 5])


## 3) Logged events ------------------------------------------------------
ini$loggedEvents

## 4)  Outflux (which variables should be used for imputation) --------------
## Variables with higher outflux are more powerful for imputation
## Varialbes with higher influx depend strongly on the imputation model
fx <- flux(analysis)
pdf("fluxplot.pdf")
mice::fluxplot(analysis)
dev.off()


# delete variables that are not good for the imputation process:
sort(fxout <- row.names(fx)[fx$outflux < 0.5])
fx3 <- flux(analysis)
pdf("fluxplot_2.pdf")
mice::fluxplot(analysis, eqscplot = FALSE)
dev.off()

# patterns
pdf("md-pattern.pdf")
md.pattern(analysis)
dev.off()

## 5) Imputation (ca. 6 min) ------------------------------
system.time(
  imp <- mice::futuremice(
    data = analysis,
    m = 20,
    method = meth,
    predictorMatrix = pred,
    post = post,
    maxit = 20,
    n.core = 10,
    parallelseed = 535185
  )
)


## 6) Check logged events
as.character(imp$loggedEvents[, "out"])


## 7) Datamanagement ----------------------------------
ana_long <- mice::complete(imp, action = "long", include = TRUE)
ana_long <- ana_long |>
  dplyr::mutate(
    bmi = weight / height^2 * 10000,
    bmi_cat = cut(
      bmi,
      breaks = c(-Inf, 18.5, 25, 30, Inf),
      right = FALSE,
      labels = c("Underweight", "Normal weight", "Overweight", "Obesity")
    ),
    bmi_cat = relevel(bmi_cat, ref = "Normal weight"),
    alcohol = factor(
      dplyr::case_when(
        sex == "Female" ~ dplyr::if_else(alcohol_stet < 4, 1L, 2L),
        sex == "Male" ~ dplyr::if_else(alcohol_stet < 5, 1L, 2L),
        TRUE ~ NA_integer_
      ),
      levels = c(1, 2),
      labels = c("low risk", "high risk")
    ),
    ageclass = cut(
      age,
      breaks = c(-Inf, 40, 50, 60, Inf),
      right = FALSE,
      labels = c("20&ndash;39", "40&ndash;49", "50&ndash;59", "&ge;60")
    ),
    # Zeit seit Diagnose
    tsd = age - diagnosis_age,
    tsd = if_else(tsd < 0, NA_real_, tsd),
    tsd_cat = cut(
      tsd,
      breaks = c(0, 1, 3, 10, Inf),
      right = FALSE,
      labels = c("<1", "1 to <3", "3 to <10", "\u226510")
    ),
    # Dosis pro kg Körpergewicht
    dosage_per_kg = complete_dose / weight,
    dosage_per_kg_cat = cut(
      dosage_per_kg,
      breaks = c(0, 0.67, 1, 1.5, Inf),
      right = TRUE,
      labels = c("\u22640.67", ">0.67 to 1.0", ">1.0 to 1.5", ">1.5")
    ),
    region = factor(
      case_when(
        as.character(center) %in%
          c(
            "Augsburg",
            "Regensburg",
            "Mannheim",
            "Freiburg",
            "Saarbr?cken",
            "Essen",
            "M?nster",
            "D?sseldorf",
            "Hannover",
            "Hamburg",
            "Bremen",
            "Kiel"
          ) ~ "Western Germany",
        as.character(center) %in%
          c(
            "Halle",
            "Leipzig",
            "Berlin Nord",
            "Berlin Mitte",
            "Berlin S?d",
            "Neubrandenburg",
            "Mecklenburgische Seenplatte"
          ) ~ "Eastern Germany"
      ),
      levels = c("Western Germany", "Eastern Germany")
    ),
    education = relevel(education, ref = "Medium"),

    ## a) diagnostischer Cut-off 0.45 - 3.32 (3 Kategorien)
    ref_cat = case_when(
      sa_tsh < 0.45 ~ 1L, # Overtreatment_diagnostic
      sa_tsh > 3.32 ~ 3L, # Undertreatment
      TRUE ~ 2L # Controlled
    ),
    ref_bin = case_when(
      sa_tsh < 0.45 ~ 1L, # Overtreatment_diagnostic
      sa_tsh > 3.32 ~ NA, # Undertreatment
      TRUE ~ 0L # Controlled
    ),

    ## c) alters- und geschlechtsspezifische Referenzintervalle
    spec_cutoff_lm = if_else(sex == "Male", 0.73 - 0.0054 * age, NA_real_),
    spec_cutoff_lf = if_else(
      sex == "Female",
      0.58 - 0.000045 * age^2,
      NA_real_
    ),
    spec_low = if_else(sex == "Male", spec_cutoff_lm, spec_cutoff_lf),

    ## Overtreatment (< spec_low vs. Controlled/Undertreatment)
    spec_cat = if_else(sa_tsh < spec_low, 1L, 0L),

    ## d) Quintile der TSH-Verteilung nach Xu et al.
    ohr_cat = as.integer(cut(
      sa_tsh,
      breaks = c(-Inf, 0.87, 1.16, 1.46, 1.92, Inf),
      labels = FALSE,
      right = TRUE
    )),

    ## beschriftete Faktoren
    ref_cat_f = factor(
      ref_cat,
      1:3,
      c("0&ndash;2.5th", "2.5&ndash;97.5th", "97.5&ndash;100th")
    ),
    spec_cat_f = factor(
      spec_cat,
      0:1,
      c("Within range or above", "Overtreated")
    ),
    ohr_cat_f = factor(
      ohr_cat,
      1:5,
      c(
        "0&ndash;20th",
        "20&ndash;40th",
        "40&ndash;60th",
        "60&ndash;80th",
        "80&ndash;100th"
      )
    ),
    ohr_sens = case_when(
      ohr_cat == 1L ~ 1L, # 0-20th
      ohr_cat %in% 2:4 ~ 0L, # 20-80th (Referenz)
      ohr_cat == 5L ~ NA_integer_ # 80-100th: excluded
    ),
    ohr_sens_f = factor(
      ohr_sens,
      levels = 0:1,
      labels = c("20&ndash;80th", "0&ndash;20th")
    ),
    ## binäre Kopien: 1 = overtreated, 0 = Rest
    ohr_bin = if_else(ohr_cat == 1L, 1L, 0L), # 0-20th vs. 20-100th)
    ohr_bin_f = factor(ohr_bin, 0:1, c("Within range or above", "Overtreated")),
    ref_bi = if_else(ref_cat == 1L, 1L, 0L), # diagnostischer Cut-off (TSH < 0.45)
  )

imp_dm <- mice::as.mids(ana_long)

save(
  imp,
  imp_dm,
  analysis,
  outlist,
  file = "outputs/imputation.rda"
)


# Function: Riskratio_missing ---------------------------------------------
riskratio_missing <- function(imp, form_rr) {
  # Make complete dataset list
  imp_complete <- mice::complete(imp, action = "all")

  # fit riskmodels
  risk_fits <- imp_complete |>
    purrr::map(\(d) {
      risks::riskratio(
        d,
        formula = form_rr,
        approach = "duplicate"
      )
    })

  # Auf der Log-RR-Skala poolen
  risk_pool <- mice::pool(risk_fits)

  # Danach Schätzer und Konfidenzgrenzen zurück auf die RR-Skala transformieren
  risk_results <- summary(risk_pool, conf.int = TRUE) |>
    as_tibble() |>
    dplyr::mutate(
      RR = exp(estimate),
      CI_low = exp(`2.5 %`),
      CI_high = exp(`97.5 %`)
    ) |>
    dplyr::select(term, RR, CI_low, CI_high, p.value)

  risk_results
}

# Run riskratio_missing for the list and all covariate adjustments ---------
myrisks <- function(imp, outcome, variables) {
  controls <- c("sex", "ageclass", "region")

  results <- vector(mode = "list", length = length(variables))
  names(results) <- variables

  # over all variables
  for (v in seq_along(variables)) {
    form_1 <- reformulate(
      variables[v],
      response = outcome
    )

    form_2 <- reformulate(
      unique(c(variables[v], controls)),
      response = outcome
    )

    form_5 <- reformulate(
      unique(c(variables[v], controls, variables[-v])),
      response = outcome
    )

    forms <- list(form_1, form_2, form_5)

    out <- lapply(forms, function(f) {
      riskratio_missing(imp = imp, form_rr = f)
    })

    names(out) <- paste(variables[v], 1:3, sep = "_")
    results[[v]] <- out

    # Sample size to table 1
    n_by_category <- mice::complete(imp, "long", include = FALSE) |>
      select(.imp, all_of(c(outcome, variables[v]))) |>
      group_by(.imp, .data[[variables[v]]]) |>
      summarise(
        overtreatment = sum(.data[[outcome]] == 1, na.rm = TRUE),
        total = sum(!is.na(.data[[outcome]])),
        prevalence = overtreatment / total,
        .groups = "drop"
      ) |>
      group_by(.data[[variables[v]]]) |>
      summarise(
        overtreatment = round(mean(overtreatment)),
        total = round(mean(total)),
        prevalence = scales::percent(mean(prevalence), accuracy = 0.1),
        .groups = "drop"
      ) |>
      mutate(
        term = paste0(
          rlang::as_string(variables[v]),
          levels(.data[[variables[v]]])
        ),
        row_id = row_number()
      )

    results[[v]][[1]] <- dplyr::full_join(
      results[[v]][[1]],
      n_by_category[, -1],
      by = "term"
    ) |>
      mutate(row_id = if_else(is.na(row_id), 0, row_id)) |>
      arrange(row_id) |>
      select(-c(row_id))
  }
  results
}



# Main Analysis -----------------------------------------------------------
rr_results <- myrisks(
  imp = imp_dm,
  outcome = "overtreated",
  variables = c(
    "sex",
    "ageclass",
    "region",
    "education",
    "migration",
    "bmi_cat",
    "smoking",
    "alcohol",
    "poly",
    "general_practitioner",
    "tsd_cat", 
    "radioiodine",
    "surgery",
    "atc_combi",
    "intake_duration",
    "dosage_per_kg_cat" 
  )
)

## 2 Decimal points
rr_results <- lapply(rr_results, function(x) {
  lapply(x, function(d) {
    d |>
      mutate(across(
        c(RR, CI_low, CI_high),
        ~ round(.x, 2)
      ))
  })
})

save(rr_results, file = "outputs/rr_results.rda")


# Sensitivity Analysis 1 -----------------------------------------------
rr_results_sens <- myrisks(
  imp = imp_dm,
  outcome = "ohr_sens",
  variables = c(
    "sex",
    "ageclass",
    "region",
    "education",
    "migration",
    "bmi_cat",
    "smoking",
    "alcohol",
    "poly",
    "general_practitioner",
    "tsd_cat", # delta, voll adjustiert
    "radioiodine",
    "surgery",
    "atc_combi",
    "intake_duration",
    "dosage_per_kg_cat" # delta, voll adjustiert
  )
)

## 2 Decimal points
rr_results_sens <- lapply(rr_results_sens, function(x) {
  lapply(x, function(d) {
    d |>
      mutate(across(
        c(RR, CI_low, CI_high),
        ~ round(.x, 2)
      ))
  })
})

save(rr_results_sens, file = "outputs/rr_results_sens.rda")




# Sensitivity Analysis 2 --------------------------------------------------
rr_results_ref <- myrisks(
  imp = imp_dm,
  outcome = "ref_bin",
  variables = c(
    "sex",
    "ageclass",
    "region",
    "education",
    "migration",
    "bmi_cat",
    "smoking",
    "alcohol",
    "poly",
    "general_practitioner",
    "tsd_cat", # delta, voll adjustiert
    "radioiodine",
    "surgery",
    "atc_combi",
    "intake_duration",
    "dosage_per_kg_cat" # delta, voll adjustiert
  )
)

## 2 Decimal points
rr_results_ref <- lapply(rr_results_ref, function(x) {
  lapply(x, function(d) {
    d |>
      mutate(across(
        c(RR, CI_low, CI_high),
        ~ round(.x, 2)
      ))
  })
})

save(rr_results_ref, file = "outputs/rr_results_ref.rda")



