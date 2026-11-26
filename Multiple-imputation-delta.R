library(mice)
library(risks)
library(dplyr)
library(purrr)
library(forcats)
library(future)
library(future.apply)
library(parallelly)


# load data ---------------------------------------------------------------
load("dats/imputation.rda")


# Function for Data Management after imputation ---------------------------
mice_data_management <- function(imp) {
    imp_long <- mice::complete(imp, action = "long", include = TRUE) |>
        dplyr::mutate(
            bmi = weight / height^2 * 10000,
            bmi_cat = cut(
                bmi,
                breaks = c(-Inf, 18.5, 25, 30, Inf),
                right = FALSE,
                labels = c(
                    "Underweight",
                    "Normal weight",
                    "Overweight",
                    "Obesity"
                )
            ),
            bmi_cat = relevel(bmi_cat, ref = "Normal weight"),
            alcohol = dplyr::case_when(
                sex == "Female" ~ dplyr::if_else(alcohol_stet < 4, 1L, 2L),
                sex == "Male" ~ dplyr::if_else(alcohol_stet < 5, 1L, 2L),
                TRUE ~ NA_integer_
            ),
            ageclass = cut(
                age,
                breaks = c(-Inf, 40, 50, 60, Inf),
                right = FALSE,
                labels = c(
                    "20&ndash;39",
                    "40&ndash;49",
                    "50&ndash;59",
                    "&ge;60"
                )
            ),
            tsd = age - diagnosis_age,
            tsd = if_else(tsd < 0, NA_real_, tsd),
            tsd_cat = cut(
                tsd,
                breaks = c(0, 1, 3, 10, Inf),
                right = FALSE,
                labels = c("<1", "1 to <3", "3 to <10", "\u226510")
            ),
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

            ## c) alters- und geschlechtsspezifische Referenzintervalle
            spec_cutoff_lm = if_else(
                sex == "Male",
                0.73 - 0.0054 * age,
                NA_real_
            ),
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

            ## Sensitivitätsanalyse: unterstes Quintil vs. 20.-80. Perzentil
            ## (oberstes Quintil wird ausgeschlossen -> NA)
            ohr_sens = case_when(
                ohr_cat == 1L ~ 1L, # 0-20th
                ohr_cat %in% 2:4 ~ 0L, # 20-80th (Referenz)
                ohr_cat == 5L ~ NA_integer_ # 80-100th: ausgeschlossen
            ),
            ohr_sens_f = factor(
                ohr_sens,
                levels = 0:1,
                labels = c("20&ndash;80th", "0&ndash;20th")
            ),
            ## binäre Kopien: 1 = overtreated, 0 = Rest
            ohr_bin = if_else(ohr_cat == 1L, 1L, 0L), # 0-20th vs. 20-100th)
            ohr_bin_f = factor(
                ohr_bin,
                0:1,
                c("Within range or above", "Overtreated")
            ),
            ref_bin = if_else(ref_cat == 1L, 1L, 0L), # diagnostischer Cut-off (TSH < 0.45)
            ref_bin_f = factor(
                ref_bin,
                0:1,
                c("Within range or above", "Overtreated")
            )
        )

    mice::as.mids(imp_long)
}

# Delta Method: diagnosis_age --------------------------------------------
## salloc --cpus-per-task=8 --mem=16G --time=08:00:00
delta_age <- c(-10, -5, -3, -1, 0, 2, 3, 5)

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

workers <- max(
    1L,
    min(length(delta_age), parallelly::availableCores(omit = 1))
)

# Bisherigen Future-Plan merken und parallel auf mehrere R-Prozesse umstellen
old_plan <- future::plan()
future::plan(future::multisession, workers = workers)

imp_delta_diagnosis <- tryCatch(
    future.apply::future_lapply(
        seq_along(delta_age),
        function(k) {
            d <- delta_age[k]

            # Eigene Kopie pro Task, damit Tasks nicht dieselbe post-Variable verändern
            post_k <- post
            post_k[which(names(post) == "diagnosis_age")] <- paste0(
                "imp[[j]][, i] <- imp[[j]][, i] + (",
                as.character(d),
                "); ",
                "imp[[j]][, i] <- pmin(imp[[j]][, i], data$age[where[, j]])"
            )

            mice::mice(
                data = analysis,
                post = post_k,
                m = 20,
                method = meth,
                predictorMatrix = pred,
                maxit = 20,
                seed = k,
                printFlag = FALSE
            )
        },
        future.seed = TRUE
    ),
    finally = future::plan(old_plan)
)

imp_delta_diagnosis_dm <- lapply(
    imp_delta_diagnosis,
    mice_data_management
)

save(
    imp_delta_diagnosis_dm,
    imp_delta_diagnosis,
    file = "outputs/imp_delta_diagnosis_neu.rda"
)


# Risk ratio ---------------------------------------------------------------
output_overtreated <- lapply(imp_delta_diagnosis_dm, function(x) {
    res <- mice::pool(with(
        x,
        risks::riskratio(
            data = data.frame(
                overtreated,
                tsd_cat,
                sex,
                ageclass,
                region,
                education,
                migration,
                bmi_cat,
                smoking,
                alcohol,
                poly,
                general_practitioner,
                radioiodine,
                surgery,
                atc_combi,
                intake_duration,
                dosage_per_kg_cat
            ),
            formula = overtreated ~ tsd_cat +
                sex +
                ageclass +
                region +
                education +
                migration +
                bmi_cat +
                smoking +
                alcohol +
                poly +
                general_practitioner +
                radioiodine +
                surgery +
                atc_combi +
                intake_duration +
                dosage_per_kg_cat,
            approach = "duplicate"
        )
    ))
    pool <- res$pooled
    pool[2:4, c("term", "estimate")]
})
output_overtreated_tsd <- lapply(output_overtreated, function(x) {
    x |>
        mutate(RR = exp(estimate)) |>
        select(-estimate)
}) |>
    purrr::map2(
        ifelse(
            delta_age < 0,
            paste0("m", abs(delta_age)),
            as.character(delta_age)
        ),
        ~ .x |>
            select(term, RR) |>
            rename(!!paste("RR", .y, sep = "_") := RR)
    ) |>
    purrr::reduce(full_join, by = "term")

save(output_overtreated_tsd, file = "outputs/rr_delta_tsd.rda")

# Sensitivity analysis --------------------------------------------
output_sens <- lapply(imp_delta_diagnosis_dm, function(x) {
    res <- mice::pool(with(
        x,
        risks::riskratio(
            data = data.frame(
                ohr_sens,
                tsd_cat,
                sex,
                ageclass,
                region,
                education,
                migration,
                bmi_cat,
                smoking,
                alcohol,
                poly,
                general_practitioner,
                radioiodine,
                surgery,
                atc_combi,
                intake_duration,
                dosage_per_kg_cat
            ),
            formula = ohr_sens ~ tsd_cat +
                sex +
                ageclass +
                region +
                education +
                migration +
                bmi_cat +
                smoking +
                alcohol +
                poly +
                general_practitioner +
                radioiodine +
                surgery +
                atc_combi +
                intake_duration +
                dosage_per_kg_cat,
            approach = "duplicate"
        )
    ))
    pool <- res$pooled
    pool[2:4, c("term", "estimate")]
})
output_sens_tsd <- lapply(output_sens, function(x) {
    x |>
        mutate(RR = exp(estimate)) |>
        select(-estimate)
}) |>
    purrr::map2(
        ifelse(
            delta_age < 0,
            paste0("m", abs(delta_age)),
            as.character(delta_age)
        ),
        ~ .x |>
            select(term, RR) |>
            rename(!!paste("RR", .y, sep = "_") := RR)
    ) |>
    purrr::reduce(full_join, by = "term")

save(output_sens_tsd, file = "outputs/rr_delta_tsd_sens.rda")

output_ref <- lapply(imp_delta_diagnosis_dm, function(x) {
    res <- mice::pool(with(
        x,
        risks::riskratio(
            data = data.frame(
                ref_bin,
                tsd_cat,
                sex,
                ageclass,
                region,
                education,
                migration,
                bmi_cat,
                smoking,
                alcohol,
                poly,
                general_practitioner,
                radioiodine,
                surgery,
                atc_combi,
                intake_duration,
                dosage_per_kg_cat
            ),
            formula = ref_bin ~ tsd_cat +
                sex +
                ageclass +
                region +
                education +
                migration +
                bmi_cat +
                smoking +
                alcohol +
                poly +
                general_practitioner +
                radioiodine +
                surgery +
                atc_combi +
                intake_duration +
                dosage_per_kg_cat,
            approach = "duplicate"
        )
    ))
    pool <- res$pooled
    pool[2:4, c("term", "estimate")]
})
output_ref_tsd <- lapply(output_ref, function(x) {
    x |>
        mutate(RR = exp(estimate)) |>
        select(-estimate)
}) |>
    purrr::map2(
        ifelse(
            delta_age < 0,
            paste0("m", abs(delta_age)),
            as.character(delta_age)
        ),
        ~ .x |>
            select(term, RR) |>
            rename(!!paste("RR", .y, sep = "_") := RR)
    ) |>
    purrr::reduce(full_join, by = "term")

save(output_ref_tsd, file = "outputs/rr_delta_tsd_ref_neu.rda")








# Delta Method: weight & complete dosage ---------------------------------
## salloc --cpus-per-task=10 --mem=16G --time=06:00:00
delta_weight <- c(-10, -5, 0, 10, 25)
delta_dose <- c(-5, 0, 10, 25, 50)
delta_dosage <- cbind(
    row_id = 1:(length(delta_weight) * length(delta_dose)),
    expand.grid(
        weight = delta_weight,
        dose = delta_dose
    )
)

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

workers <- max(
    1L,
    min(nrow(delta_dosage), parallelly::availableCores(omit = 1))
)


# Bisherigen Future-Plan merken und parallel auf mehrere R-Prozesse umstellen
old_plan <- future::plan()
future::plan(future::multisession, workers = workers)

imp_delta_dosage <- tryCatch(
    future.apply::future_lapply(
        seq_along(delta_dosage$row_id),
        function(k) {
            d1 <- delta_dosage[k, 2]
            d2 <- delta_dosage[k, 3]

            # Eigene Kopie pro Task, damit Tasks nicht dieselbe post-Variable verändern
            post_k <- post
            post_k[which(names(post) == "weight")] <- paste0(
                "imp[[j]][, i] <- imp[[j]][, i] + (",
                as.character(d1),
                ")"
            )
            post_k[which(names(post) == "complete_dose")] <- paste0(
                "imp[[j]][, i] <- imp[[j]][, i] + (",
                as.character(d2),
                ")"
            )

            mice::mice(
                data = analysis,
                post = post_k,
                m = 20,
                method = meth,
                predictorMatrix = pred,
                maxit = 20,
                seed = k,
                printFlag = FALSE
            )
        },
        future.seed = TRUE
    ),
    finally = future::plan(old_plan)
)

imp_delta_dosage_dm <- lapply(
    imp_delta_dosage,
    mice_data_management
)


save(
    imp_delta_dosage_dm,
    imp_delta_dosage,
    file = "outputs/imp_delta_dosage_neu.rda"
)


# Risk ratio --------------------------------------------------------
output_overtreated <- lapply(imp_delta_dosage_dm, function(x) {
    res <- mice::pool(with(
        x,
        risks::riskratio(
            data = data.frame(
                overtreated,
                tsd_cat,
                sex,
                ageclass,
                region,
                education,
                migration,
                bmi_cat,
                smoking,
                alcohol,
                poly,
                general_practitioner,
                radioiodine,
                surgery,
                atc_combi,
                intake_duration,
                dosage_per_kg_cat
            ),
            formula = overtreated ~ dosage_per_kg_cat +
                sex +
                ageclass +
                region +
                education +
                migration +
                bmi_cat +
                smoking +
                alcohol +
                poly +
                general_practitioner +
                tsd_cat +
                radioiodine +
                surgery +
                atc_combi +
                intake_duration +
                dosage_per_kg_cat,
            approach = "duplicate"
        )
    ))
    pool <- res$pooled
    pool[2:4, c("term", "estimate")]
})

weight_lab <- ifelse(
    delta_dosage$weight < 0,
    paste0("m", abs(delta_dosage$weight)),
    as.character(delta_dosage$weight)
)

dose_lab <- ifelse(
    delta_dosage$dose < 0,
    paste0("m", abs(delta_dosage$dose)),
    as.character(delta_dosage$dose)
)
name_lab <- paste("RR", weight_lab, dose_lab, sep = "_")


output_overtreated_dosage <- lapply(output_overtreated, function(x) {
    x |>
        mutate(RR = exp(estimate)) |>
        select(-estimate)
}) |>
    purrr::map2(
        name_lab,
        ~ .x |>
            select(term, RR) |>
            rename(!!.y := RR)
    ) |>
    reduce(full_join, by = "term")

save(output_overtreated_dosage, file = "outputs/rr_delta_dosage.rda")

# Median over all imputation ------------------------------
summary_over_m <- function(imp) {
    mice::complete(imp, action = "long", include = FALSE) |>
        group_by(.imp) |>
        summarise(
            mean_weight = mean(weight, na.rm = TRUE),
            mean_complete_dose = mean(complete_dose, na.rm = TRUE),
            mean_dosage = mean(dosage_per_kg),
            .groups = "drop"
        ) |>
        summarise(
            mean_weight_over_m = mean(mean_weight, na.rm = TRUE),
            mean_dose_over_m = mean(mean_complete_dose, na.rm = TRUE),
            mean_dosage_over_m = mean(mean_dosage, na.rm = TRUE),
        )
}

desc_dosage <- sapply(imp_delta_dosage_dm, summary_over_m)
colnames(desc_dosage) <- name_lab
desc_dosage

save(output_overtreated_dosage, desc_dosage, 
     file = "outputs/rr_delta_dosage.rda")


## Sensitivity --------------------------------------------------------
output_sens <- lapply(imp_delta_dosage_dm, function(x) {
    res <- mice::pool(with(
        x,
        risks::riskratio(
            data = data.frame(
                ohr_sens,
                dosage_per_kg_cat,
                sex,
                ageclass,
                region,
                bmi_cat,
                education,
                migration,
                smoking,
                alcohol,
                tsd_cat,
                atc_combi,
                intake_duration,
                poly,
                general_practitioner,
                radioiodine,
                surgery
            ),
            formula = ohr_sens ~ dosage_per_kg_cat +
                sex +
                ageclass +
                region +
                education +
                migration +
                bmi_cat +
                smoking +
                alcohol +
                poly +
                general_practitioner +
                tsd_cat +
                radioiodine +
                surgery +
                atc_combi +
                intake_duration +
                dosage_per_kg_cat,
            approach = "duplicate"
        )
    ))
    pool <- res$pooled
    pool[2:4, c("term", "estimate")]
})

weight_lab <- ifelse(
    delta_dosage$weight < 0,
    paste0("m", abs(delta_dosage$weight)),
    as.character(delta_dosage$weight)
)

dose_lab <- ifelse(
    delta_dosage$dose < 0,
    paste0("m", abs(delta_dosage$dose)),
    as.character(delta_dosage$dose)
)
name_lab <- paste("RR", weight_lab, dose_lab, sep = "_")


output_sens_dosage <- lapply(output_sens, function(x) {
    x |>
        mutate(RR = exp(estimate)) |>
        select(-estimate)
}) |>
    purrr::map2(
        name_lab,
        ~ .x |>
            select(term, RR) |>
            rename(!!.y := RR)
    ) |>
    reduce(full_join, by = "term")

save(output_sens_dosage, file = "outputs/rr_delta_dosage_sens.rda")


output_ref <- lapply(imp_delta_dosage_dm, function(x) {
    res <- mice::pool(with(
        x,
        risks::riskratio(
            data = data.frame(
                ref_bin,
                dosage_per_kg_cat,
                sex,
                ageclass,
                region,
                bmi_cat,
                education,
                migration,
                smoking,
                alcohol,
                tsd_cat,
                atc_combi,
                intake_duration,
                poly,
                general_practitioner,
                radioiodine,
                surgery
            ),
            formula = ref_bin ~ dosage_per_kg_cat +
                sex +
                ageclass +
                region +
                education +
                migration +
                bmi_cat +
                smoking +
                alcohol +
                poly +
                general_practitioner +
                tsd_cat +
                radioiodine +
                surgery +
                atc_combi +
                intake_duration +
                dosage_per_kg_cat,
            approach = "duplicate"
        )
    ))
    pool <- res$pooled
    pool[2:4, c("term", "estimate")]
})

weight_lab <- ifelse(
    delta_dosage$weight < 0,
    paste0("m", abs(delta_dosage$weight)),
    as.character(delta_dosage$weight)
)

dose_lab <- ifelse(
    delta_dosage$dose < 0,
    paste0("m", abs(delta_dosage$dose)),
    as.character(delta_dosage$dose)
)
name_lab <- paste("RR", weight_lab, dose_lab, sep = "_")


output_ref_dosage <- lapply(output_ref, function(x) {
    x |>
        mutate(RR = exp(estimate)) |>
        select(-estimate)
}) |>
    purrr::map2(
        name_lab,
        ~ .x |>
            select(term, RR) |>
            rename(!!.y := RR)
    ) |>
    reduce(full_join, by = "term")

save(output_ref_dosage, file = "outputs/rr_delta_dosage_ref_neu.rda")


