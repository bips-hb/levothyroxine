library (risks)
library(haven)
library(broom)
library(dplyr)
library(purrr)
library(rifttable)
library(knitr)
library(gt)
library(tidycat)
library(forcats)
library(ggplot2)
library(ggpattern)
library(ggbrace)
library(tidyr)
library(ggforce)
library(gtsummary)
library(flextable)
library(patchwork)
library(stringr)
library(ggtext)
library(svglite)
library(sjPlot)
library(scales)
library(mice)
library(car)
library(ranger)
library(labelled)

# Datum und Pfade definieren
current_date <- Sys.Date() # Format: YYYY-MM-DD
folder_path  <- file.path("outputs", current_date)

# Ordner erstellen, falls er noch nicht existiert
if (!dir.exists(folder_path)) {
  dir.create(folder_path, recursive = TRUE)
}


# Load dataset ------------------------------------------------------------
main <- read_sas("dats/r_final.sas7bdat")

# Define custom palette for plot
my_colors_plot <- c(
  "GroupA" = "#357BA2FF", 
  "Unknown"     = "grey70", 
  "Other"       = "#BB3754"
)
my_colors_histo <- c(
  "Within range or above" = "grey70",
  "Overtreated"           = "#BB3754"
)



# Data Management ---------------------------------------------------------
# Define levels and labels for each column
cols <- c("sex", "ageclass", "center", "education", "migration", "smoking", "alcohol", 
          "intake_duration", "atc_combi", "iodine_sf", "Jodetten", "Thyreostatika", "poly", 
          "radioiodine", "current_treatment", "general_practitioner", "radiation", "surgery", 
          "bmi_stet_q", "weight_q", "height_q")
lev_list <- list(c("1", "2"), c("20", "30", "40", "50", "60", "70", "80","90", "100"), 
                 c("0", "11", "12", "21", "22", "23", "31", "32", "33", "41", "42", "51", "52", "53", "61", "62", "63", "71", "81", "811", "812", "813"),
                 c("1", "2", "3", "4", "6", "7", "8", "-99", "-88", "-9"), 
                 c("0", "1", "-9"), c("1", "2", "3", "4"), c("0", "1", "-99"), 
                 c("3", "4", "5", "6", "7", "2", "1", "-999"), c("0", "1"), 
                 c("2", "1", "9999", "8888"), c("0", "1"), c("0", "1"), c("0", "1", "-9"),
                 c("2", "1", "8888", "9999"), c("2", "1", "8888", "9999"),
                 c("1","2","3","4","9","8"), c("2", "1", "8888", "9999"), c("2", "1", "8888", "9999"), 
                 c("0", "1", "2"), c("0", "1", "2"), c("0", "1", "2"))
                 
lab_list <- list(
  c("Male", "Female"), 
  c("20&ndash;39", "20&ndash;39", "40&ndash;49", "50&ndash;59","&ge;60","&ge;60","&ge;60","&ge;60","&ge;60"),
  c("nicht zugeordnet", "Augsburg", "Regensburg", "Mannheim", "Freiburg", "Saarbr\u00fccken", "Essen", "M\u00fcnster", "D\u00fcsseldorf", "Halle", 
    "Leipzig", "Berlin Nord", "Berlin Mitte", "Berlin S\u00fcd", "Hannover", "Hamburg", "Bremen", "Kiel", "Neubrandenburg", "Mecklenburgische Seenplatte", "Mecklenburgische Seenplatte", "Mecklenburgische Seenplatte"),
  c("Low", "Low", "Medium", "Medium", "High", "High", "High", "Unknown", "Unknown", "Unknown"),
  c("No", "Yes", "Unknown"),
  c("Never", "Former", "Current", "Unknown"),
  c("Low risk", "High risk", "Unknown"),
  c("&lt;1", "&lt;1", "&lt;1", "&lt;1", "&lt;1", "1 to &lt;3", "&ge;3", "Unknown"),
  c("No", "Yes"),
  c("No", "Yes", "Unknown", "Unknown"),
  c("No", "Yes"),
  c("No", "Yes"),
  c("No", "Yes", "Unknown"),
  c("No", "Yes", "Unknown", "Unknown"),
  c("No", "Yes", "Unknown", "Unknown"),
  c("&le;1", "2&ndash;3", "4&ndash;12", "&gt;12", "Unknown", "Unknown"),
  c("No", "Yes", "Unknown", "Unknown"),
  c("No", "Yes", "Unknown", "Unknown"),
  c("Measured", "Measured", "Self-reported"),
  c("Measured", "Measured", "Self-reported"),
  c("Measured", "Measured", "Self-reported")
)

main[cols] <- pmap(list(main[cols], lev_list, lab_list), 
                   function(x, lev, lab) factor(x, levels = lev, labels = lab))

## Save dataset for multiple imputation ------------------------------------
# save(main, file = file.path(folder_path, "main.rda")


# Extreme values and categorization of some variables needed for full model
# Function: SAS-konformes Runden (R's round() rundet nach IEEE: 0.125 -> 0.12)
round_sas <- function(x, digits = 2) {
  f <- 10^digits
  sign(x) * trunc(abs(x) * f + 0.5 + 1e-9) / f
}

west <- c("Augsburg", "Regensburg", "Mannheim", "Freiburg", "Saarbr\u00fccken",
          "Essen", "M\u00fcnster", "D\u00fcsseldorf", "Hannover", "Hamburg",
          "Bremen", "Kiel")
east <- c("Halle", "Leipzig", "Berlin Nord", "Berlin Mitte", "Berlin S\u00fcd",
          "Neubrandenburg", "Mecklenburgische Seenplatte")


# Function: data mangement 
prepare_thyroid <- function(dat, .name = NULL) {
  dat %>%
    mutate(
      # --- Plausibilitätsprüfung Laborwerte ---
      sa_tsh = if_else(sa_tsh > 130, NA_real_, sa_tsh),
      sa_ft3 = if_else(sa_ft3 >  50, NA_real_, sa_ft3),
      sa_ft4 = if_else(sa_ft4 > 100, NA_real_, sa_ft4),
      
      ## a) diagnostischer Cut-off 0.45 - 3.32 (3 Kategorien)
      ref_cat = case_when(
        sa_tsh < 0.45 ~ 1L,                       # Overtreatment_diagnostic
        sa_tsh > 3.32 ~ 3L,                       # Undertreatment
        TRUE          ~ 2L                        # Controlled
      ),
      
      ## b) konservativer Cut-off: Overtreatment (< 0.1) 
      cons_cat = if_else(sa_tsh < 0.10, 1L, 0L),
      
      ## c) alters- und geschlechtsspezifische Referenzintervalle
      spec_cutoff_lm = round_sas(if_else(sex == "Male", 0.73 - 0.0054 * age,     NA_real_)),
      spec_cutoff_lf = round_sas(if_else(sex == "Female", 0.58 - 0.000045 * age^2, NA_real_)),
      # spec_cutoff_hm = round_sas(if_else(sex == "Male", 2.75 + 23.32 / age,      NA_real_)),
      # spec_cutoff_hf = round_sas(if_else(sex == "Female", 3.68 - 0.006 * age,      NA_real_)),
      
      spec_low  = if_else(sex == "Male", spec_cutoff_lm, spec_cutoff_lf),
      #spec_high = if_else(sex == "Male", spec_cutoff_hm, spec_cutoff_hf),
      
      ## Overtreatment (< spec_low vs. Controlled/Undertreatment)
      spec_cat = if_else(sa_tsh < spec_low, 1L, 0L),
      
      ## d) Quintile der TSH-Verteilung nach Xu et al.
      ohr_cat = as.integer(cut(sa_tsh,
                               breaks = c(-Inf, 0.87, 1.16, 1.46, 1.92, Inf),
                               labels = FALSE, right = TRUE)),
      
      ## Beschriftung Faktoren
      ref_cat_f  = factor(ref_cat,  1:3, c("0&ndash;2.5th", "2.5&ndash;97.5th", "97.5&ndash;100th")),
      cons_cat_f = factor(cons_cat, 0:1, c("Within range or above", "Overtreated")),
      spec_cat_f = factor(spec_cat, 0:1, c("Within range or above", "Overtreated")),
      ohr_cat_f  = factor(ohr_cat,  1:5, c("0&ndash;20th", "20&ndash;40th", "40&ndash;60th", "60&ndash;80th", "80&ndash;100th")),
      
      ## Sensitivitätsanalyse: unterstes Quintil vs. 20.-80. Perzentil
      ## (oberstes Quintil wird ausgeschlossen -> NA)
      ohr_sens = case_when(
          ohr_cat == 1L              ~ 1L,          # 0-20th
          ohr_cat %in% 2:4           ~ 0L,          # 20-80th (Referenz)
          ohr_cat == 5L              ~ NA_integer_  # 80-100th: ausgeschlossen
      ),
      ohr_sens_f = factor(ohr_sens, levels = 0:1,
                            labels = c("20&ndash;80th", "0&ndash;20th"))
      ,
              ## binäre Kopien: 1 = overtreated, 0 = Rest
      ohr_bin  = if_else(ohr_cat == 1L, 1L, 0L),    # 0-20th vs. 20-100th)
      ref_bin  = if_else(ref_cat == 1L, 1L, 0L),    # diagnostischer Cut-off (TSH < 0.45)

      ref_bin_f  = factor(ref_bin,  0:1, c("Within range or above", "Overtreated")),
      ohr_bin_f  = factor(ohr_bin,  0:1, c("Within range or above", "Overtreated"))
      ,
      
      # --- Region ---
      region = factor(
        case_when(
          as.character(center) %in% west ~ "Western Germany",
          as.character(center) %in% east ~ "Eastern Germany"
        ),
        levels = c("Western Germany", "Eastern Germany")
      ),
      # --- Bildung: Referenz = Medium ---
      education = forcats::fct_relevel(as.factor(education), "Medium"),
      
      # --- Zeit seit Diagnose ---
      tsd = age - diagnosis_age,
      tsd = if_else(tsd < 0, NA_real_, tsd),   # negative Werte = Datenfehler
      
      tsd_cat = factor(
        case_when(
          tsd >=  0 & tsd <  1 ~ 1,
          tsd >=  1 & tsd <  3 ~ 2,
          tsd >=  3 & tsd < 10 ~ 3,
          tsd >= 10            ~ 4
        ),
        levels = 1:4,
        labels = c("<1", "1 to <3", "3 to <10", "\u226510")
      ),
      
      # --- Dosierung pro kg Körpergewicht ---
      dosage_per_kg = complete_dose / weight,
      
      dosage_per_kg_cat = factor(
        case_when(
          dosage_per_kg >= 0    & dosage_per_kg <= 0.67 ~ 1,
          dosage_per_kg >  0.67 & dosage_per_kg <= 1    ~ 2,
          dosage_per_kg >  1    & dosage_per_kg <= 1.5  ~ 3,
          dosage_per_kg >  1.5                          ~ 4
        ),
        levels = 1:4,
        labels = c("\u22640.67", "&gt;0.67 to 1.0", "&gt;1.0 to 1.5", "&gt;1.5")
      ),
      
      # --- BMI ---
      bmi = weight / height^2 * 10000,
      bmi_cat = cut(
        bmi,
        breaks = c(-Inf, 18.5, 25, 30, Inf),
        right = FALSE,
        labels = c("Underweight", "Normalweight", "Preadiposity", "Adiposity")
      ), 
      # Referenz = Normalweight, restliche Reihenfolge bleibt erhalten
      bmi_cat = forcats::fct_relevel(bmi_cat, "Normalweight")
    ) %>%
    labelled::set_variable_labels(
      sex           = "Sex/gender",
      ageclass      = "Age (years)",
      education     = "Education level",
      migration     = "Migration background",
      smoking       = "Smoking status",
      alcohol       = "Risky alcohol consumption",
      poly          = "Use of ≥5 different medications",
      general_practitioner = "Time since last GP visit (months)",
      atc_combi     = "Prescribed iodine (in combination with levothyroxine)",
      intake_duration = "Duration of levothyroxine use (years)",
      radioiodine   = "History of radioiodine therapy",
      surgery       = "History of thyroid surgery",
      sa_tsh        = "Thyrotropin (TSH) serum level (in mIU/L)",
      sa_ft3        = "Free triiodothyronine (FT3) serum level (in pmol/L)",
      sa_ft4        = "Free thyroxine (FT4) serum level (in pmol/L)",
      region        = "Region",
      tsd_cat       = "Time since first thyroid diagnosis (years)",
      complete_dose = "Levothyroxine dose (\u00b5g/day)",
      dosage_per_kg = "Levothyroxine dose (\u00b5g/kg/day)",
      dosage_per_kg_cat = "Levothyroxine dose (\u00b5g/kg/day)",
      weight        = "Weight (kg)",
      bmi           = "Body-mass-index (kg/m\u00b2)",
      bmi_cat       = "Body-mass-index (kg/m\u00b2)",
      tsd           = "Time since first thyroid diagnosis (years)",
      ref_cat_f     = "Diagnostic thyrotropin (TSH) cut-offs",
      ohr_cat_f     = "Prognostic thyrotropin (TSH) cut-offs",
      center        = "Study center",
      iodine_sf     = "Self-reported iodine intake (in thyroid disease-related interview)*",
      Jodetten      = "Self-reported use of iodine therapy preparation (ATC H03C)**",
      Thyreostatika = "Self-reported use of antithyroid preparation (ATC H03B)**",
      .strict       = FALSE          # Variablen, die es nicht gibt, werden ignoriert
    )
}
main_model    <- prepare_thyroid(main)
save(main_model, file = file.path(folder_path, "main_model.rda"))



#Load datasets from PR ------------------------------------------------------
load(file = file.path(folder_path, "main_model.rda"))
load(file = "outputs/rr_results.rda")
load(file = "outputs/rr_results_ref.rda")
load(file = "outputs/rr_results_sens.rda")
load(file = "outputs/rr_delta_tsd.rda")
load(file = "outputs/rr_delta_tsd_ref_neu.rda")
load(file = "outputs/rr_delta_tsd_sens.rda")
load(file = "outputs/rr_delta_dosage.rda")
load(file = "outputs/rr_delta_dosage_ref_neu.rda")
load(file = "outputs/rr_delta_dosage_sens.rda")

# Create Tableds ------------------------------------------------------------
## Table 1 Descriptive characteristics of the study population --------------
table1 <- main_model |>
  table1_design(
    sex, age, ageclass, region, education, migration, bmi, bmi_cat, smoking, alcohol,
    poly, general_practitioner, 
    tsd, tsd_cat,
    radioiodine, surgery, atc_combi,
    intake_duration, dosage_per_kg_cat, 
    ref_cat_f, ohr_cat_f
  ) |>
  mutate(label = if_else(!startsWith(label, " "), 
                         paste0("**", label, "**"), 
                         label),
         digits = if_else(
           outcome %in% c("age", "tsd"),
           true = 0,
           false = NA
         )
         ) |>
  rifttable(diff_digits = 1, risk_digits = 0, overall = TRUE) |>
  rt_gt() |>
  gt::text_transform(
    locations = cells_body(),
    fn = function(x) {
      # Search for number sequences with at least 4 digits and insert a comma
      gsub("(\\d)(?=(\\d{3})+(?!\\d))", "\\1,", x, perl = TRUE)
    }
  ) |>
  cols_label(Summary = "Characteristic") |>
  gt::tab_footnote(footnote = "Statistics shown are count (percent) or median (interquartile range).")
print(table1)
gtsave(data=table1, filename="Table 1 Descriptive characteristics of the study population.html", path=folder_path) 



# Table S1 ----------------------------------------------------------------
# Table S1: Further descriptive characteristics of the study population
tables1 <- main_model |>
  table1_design(
    center,
    sa_tsh, sa_ft3, sa_ft4, iodine_sf, Jodetten, Thyreostatika
  ) |>
  mutate(label = if_else(!startsWith(label, " "), 
                         paste0("**", label, "**"), 
                         label)) |>
  rifttable(diff_digits = 1, risk_digits = 0, overall = TRUE) |>
  rt_gt() |>
  gt::text_transform(
    locations = cells_body(),
    fn = function(x) {
      # Search for number sequences with at least 4 digits and insert a comma
      gsub("(\\d)(?=(\\d{3})+(?!\\d))", "\\1,", x, perl = TRUE)
    }
  ) |>
  cols_label(Summary = "Characteristic") |>
  gt::tab_footnote(
    footnote = gt::html("Statistics shown are count (percent) or median (interquartile range).<br>* Derived from core interview from participants who reported a physician's diagnosis of thyroid disease.<br>** Derived from interview on medication use.<br>ATC=Anatomic Therapeutic Chemical (code).")
  )
print(tables1)
gtsave(data=tables1, filename="Table S1 Further descriptive characteristics of the study population.html", path=folder_path) 




# Table S2 ------------------------------------------------------------------
# Table S2: Descriptive characteristics of levothyroxine users by TSH treatment status categorization 
tableS2 <- main_model |>
  table1_design(
    sex, age, tsd, dosage_per_kg, intake_duration, sa_ft3, sa_ft4,
    by=ohr_bin_f
  ) |>
  mutate(label = if_else(!startsWith(label, " "), 
                         paste0("**", label, "**"), 
                         label),
         digits = if_else(
           outcome %in% c("age", "tsd"),
           true = 0,
           false = NA
         )
  ) |>
  rifttable(diff_digits = 1, risk_digits = 0, overall = TRUE) |>
  rt_gt() |>
  gt::cols_label(Overtreated = "Below or at") |>
  gt::text_transform(
    locations = cells_body(),
    fn = function(x) {
      # Search for number sequences with at least 4 digits and insert a dot
      gsub("(\\d)(?=(\\d{3})+(?!\\d))", "\\1,", x, perl = TRUE)
    }
  ) |>
  cols_label_with(fn = ~ gt::md(.)) |>
  cols_label(Summary = "Characteristic") |>
  gt::tab_spanner(
    label = "Thyrotropin (TSH) value according to prognostic cut-off", 
    columns = 3:4
  ) |>
  gt::tab_footnote(footnote = "Statistics shown are count (percent) or median (interquartile range).")

print(tableS2)
gtsave(data=tableS2, filename="Table S2 Descriptive characteristics of levothyroxine users by TSH status categorization.html", path=folder_path) 



# Table S3 ----------------------------------------------------------------
tables3 <- main_model |>
  table1_design(
    weight, complete_dose, diagnosis_age
  ) |>
  rifttable(diff_digits = 1, risk_digits = 0, overall = TRUE) |>
  rt_gt() |>
  gt::text_transform(
    locations = cells_body(),
    fn = function(x) {
      # Search for number sequences with at least 4 digits and insert a comma
      gsub("(\\d)(?=(\\d{3})+(?!\\d))", "\\1,", x, perl = TRUE)
    }
  ) |>
  cols_label(Summary = "Characteristic") |>
  gt::tab_footnote(footnote = "Statistics shown are median (interquartile range).")
print(tables3)
gtsave(data=tables3, filename="Table S3.html", path=folder_path)



# Figure 1 Histogram --------------------------------------------------------
main_model <- main_model %>%
  mutate(kombi_filter = case_when(
    ref_bin_f == "Overtreated"           & ohr_bin_f == "Overtreated" ~ "Below diagnostic cut-off",
    ref_bin_f == "Within range or above" & ohr_bin_f == "Overtreated" ~ "Below or at prognostic cut-off",
    TRUE                                                              ~ "Within range or above"
  ))
main_histo <- main_model %>%
  mutate(sa_tsh_capped = ifelse(sa_tsh > 7.5, 7.5 + 0.05, sa_tsh))

histo2 <- ggplot(main_histo, aes(x = sa_tsh_capped, fill = kombi_filter,
                               pattern = kombi_filter, color = kombi_filter)) +
  # Pattern
  geom_histogram_pattern(
    binwidth        = 0.087,
    boundary        = 0.44,
    position        = "identity",
    pattern_color   = "#357BA2FF",   # Farbe der Streifen (Blau)
    pattern_fill    = "#357BA2FF",   # Farbe der Streifenfuellung
    pattern_angle   = 45,            # Winkel der Schraffur
    pattern_density = 0.1,           # Dichte der Streifen
    pattern_spacing = 0.02,          # Abstand der Streifen
    linewidth       = 0.5,
    color           = "#357BA2FF"    # Blauer Rand fuer alle mit Muster
  ) +
  scale_fill_manual(values = c(
    "Below diagnostic cut-off" = "#D56B82",
    "Below or at prognostic cut-off" = "#D56B82",
    "Within range or above"                        = "grey70"
  )) +
  scale_pattern_manual(values = c(
    "Below diagnostic cut-off" = "stripe",
    "Below or at prognostic cut-off" = "none",
    "Within range or above"                        = "none"
  )) +
  scale_color_manual(values = c(
    "Below diagnostic cut-off" = "#357BA2FF",
    "Below or at prognostic cut-off" = "#357BA2FF",
    "Within range or above"                        = "transparent"
  )) +
  # X-Achse: in 0.5er Schritten
  scale_x_continuous(breaks = seq(0, 7.5, by = 0.5)) +
  # Y-Achse: alle 100 Counts
  scale_y_continuous(breaks = seq(0, 1000, by = 100)) +
  geom_vline(xintercept = c(0.44, 0.87), linetype = "dashed",
             color = "black", linewidth = 0.7) +
  annotate("label",
           x             = 0.435,
           y             = 790,
           label         = "42%",
           fontface      = "bold",
           size          = 3.5,
           color         = "white",
           fill          = "#D56B82",   # Hintergrundfarbe des Feldes
           label.padding = unit(0.2, "lines")) +
  # Beschriftung des zusammengefassten obersten Balkens
  annotate("text",
           x     = 7.5,
           y     = 130,
           label = "\u22657.5",
           size  = 3,
           color = "grey40") +
  annotate("label",
           x             = 0.15,
           y             = 700,
           label         = "19%",
           fontface      = "bold",
           size          = 3.5,
           fill          = "white",
           label.padding = unit(0.2, "lines")) +
  coord_cartesian(xlim = c(0, 7.6)) +
  theme_minimal() +
  labs(
    x       = "Thyrotropin (TSH) serum level (in mIU/L)",
    y       = "Counts",
    fill    = "Thyrotropin (TSH)",
    pattern = "Thyrotropin (TSH)"
  ) +
  theme(
    legend.position      = c(0.98, 0.98),
    legend.justification = c("right", "top"),
    legend.direction     = "vertical",
    legend.background    = element_rect(fill = "white", color = NA),
    legend.box.background = element_blank(),
    legend.title         = element_text(face = "bold", size = 10),
    legend.text          = element_text(size = 9)
  )

# Legende: gemeinsame Reihenfolge, Fuellfarben nur fuer die Legende
histo2 <- histo2 +
  guides(
    fill = guide_legend(
      reverse      = TRUE,
      ncol         = 1,
      override.aes = list(fill = c("grey70", "#D56B82", "white"))
    ),
    pattern = guide_legend(reverse = TRUE, ncol = 1),
    color   = guide_legend(reverse = TRUE, ncol = 1)
  )

# Obere Klammer (0 bis 0.87)
histo2 <- histo2 +
  stat_brace(
    data         = data.frame(x = c(0, 0.87), y = c(730, 760)),
    aes(x = x, y = y),
    inherit.aes  = FALSE,
    rotate       = 0
  )

# Untere Klammer (0 bis 0.45)
histo2 <- histo2 +
  stat_brace(
    data         = data.frame(x = c(0, 0.45), y = c(640, 670)),
    aes(x = x, y = y),
    inherit.aes  = FALSE,
    rotate       = 0
  )

histo2
file_name <- "Figure 2 Histogram.svg"
full_path <- file.path(folder_path, file_name)
ggsave(filename = full_path, plot = histo2, width = 16.5, height = 14.5, units = "cm")




# Figure S1 Bar chart -------------------------------------------------------
lvl <- c("Diagnostic cut-off",
         "Conservative cut-off",
         "Age- and sex-specific cut-off",
         "Prognostic cut-off")

df_long <- main_model %>%
  select(
    `Diagnostic cut-off`             = ref_bin_f,
    `Conservative cut-off`           = cons_cat_f,
    `Age- and sex-specific cut-off`  = spec_cat_f,
    `Prognostic cut-off`             = ohr_bin_f
  ) %>%
  pivot_longer(
    cols             = everything(),
    names_to         = "Kategorie",
    values_to        = "Wert",
    values_transform = list(Wert = as.character)
  ) %>%
  mutate(
    Kategorie = factor(Kategorie, levels = lvl),
    Wert      = factor(Wert)   
  )

# 1. Daten vorbereiten
df_plot <- df_long %>%
  count(Kategorie, Wert) %>%
  group_by(Kategorie) %>%
  mutate(pct = n / sum(n))

# Reihenfolge festlegen (von oben nach unten im Plot)
df_plot$Kategorie <- factor(
  df_plot$Kategorie,
  levels = c("Diagnostic cut-off", "Prognostic cut-off", "Conservative cut-off", "Age- and sex-specific cut-off")
)

df_plot %>%
  ungroup() %>%
  arrange(Kategorie, Wert) %>%
  mutate(pct = scales::percent(pct, accuracy = 0.1)) %>%
  gt()

# 2. Plotten
figures2 <- ggplot(df_plot, aes(x = factor(Kategorie), y = pct, fill = fct_rev(Wert))) +
  geom_col(position = "fill") +
  scale_fill_manual(
    values = my_colors_histo,
    labels = c("Overtreated" = "Below (or at)")   #dieses Level wird umbenannt
  ) +
  geom_text(
    aes(label = scales::percent(pct, accuracy = 1, drop0trailing = TRUE)),
    position  = position_fill(vjust = 0.5),
    color     = "white",
    fontface  = "bold",
    size      = 2.5
  ) +
  facet_wrap(~Kategorie, scales = "free_y", ncol = 1) +
  coord_flip() +
  scale_y_continuous(labels = scales::percent) +
  labs(x = "", y = "Prevalence", fill = "") +
  theme_minimal(base_size = 11) +
  theme(
    text                 = element_text(),
    axis.text.y          = element_text(size = 11, hjust = 0),
    strip.text           = element_blank(),
    legend.text          = ggtext::element_markdown(),
    legend.position      = "top",
    legend.justification = "center",
    legend.direction     = "horizontal"
  ) +
  guides(fill = guide_legend(nrow = 1, reverse = TRUE))

figures2
# Speichern mit exakten Maßen
file_name_s2 <- "Figure S1 Prevalence of overtreatment.svg"
full_path_s2 <- file.path(folder_path, file_name_s2)

ggsave(filename = full_path_s2, plot = figures2,
       width = 16.5, height = 9.5, units = "cm")



# Figure S2 Scatter plot ------------------------------------------------------
figures3 <- ggplot(main_model, aes(x = sa_tsh, y = dosage_per_kg, color = ohr_bin_f)) +
  geom_point(size = 0.5, alpha = 0.6) +
  #coord_cartesian(xlim = c(0, 10)) +
  scale_x_continuous(limits = c(0, 10))+
  scale_color_discrete(
    labels = c("Overtreated" = "Below or at")   
  ) +
  labs(
    x = "TSH (mlU/L)",
    y = "Levothyroxine dose (\u00B5g/kg/day)",
    color = "Thyrotropin (TSH) value according to prognostic cut-off"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.text = element_markdown(),
    legend.position = c(0.97, 0.97),
    legend.justification = c("right", "top")
  )
print(figures3)
# Save scatter plot
file_name_s3    <- "Figure S2 Scatter plot.svg"
full_path_s3    <- file.path(folder_path, file_name_s3)
ggsave(full_path_s3, plot = figures3, width = 16.5, height = 16.5, units = "cm")




# Table 0 -----------------------------------------------------------------
# exclude radioiodine and/or surgery
main_model_filtered <- main_model %>%
  filter(!(radioiodine %in% "Yes") & !(surgery %in% "Yes"))
table(main_model_filtered$ohr_bin_f)
table(main_model_filtered$ref_bin_f)
nrow(main_model_filtered)
x_werte <- c(13779, 2525, 5562, 2401, 517, 3604, 1574)
n_werte <- c(109997, 13095, 13095, 13095, 13095, 9204, 9204)
labels <- c("Detail to results section: Levothyroxine users", "Detail to Figure 2: Diagnostic cut-off", 
            "Detail to Figure 2: Prognostic cut-off", "Detail to Figure S1: Age- and sex- specific cut-off", 
            "Detail to Figure S1: Conservative cut-off", "Detail to results section: Prognostic cut-off without
            radioiodine or surgery", "Detail to results section: Diagnostic cut-off without radioiodine 
            or surgery")
# Funktion zur Extraktion der prop.test Ergebnisse
get_stats <- function(x, n) {
  res <- prop.test(x, n)
  data.frame(
    Anteil = round(res$estimate * 100, 1),
    CI_Unten = round(res$conf.int[1] * 100, 1),
    CI_Oben = round(res$conf.int[2] * 100, 1)
  )
}
# Tabelle erstellen
ergebnisse <- do.call(rbind, mapply(get_stats, x_werte, n_werte, SIMPLIFY = FALSE))
final_table <- cbind(Kategorie = labels, x = x_werte, n = n_werte, ergebnisse)

# Details to Figure S3
no_dose <- sum(is.na(main_model$dosage_per_kg))  # Detail to Figure S2: Unknown levothyroxine dose
high_tsh <- sum(main_model$sa_tsh > 10) # Detail to Figure S2: TSH value above 10 mlU/L
miss  <- sum(is.na(main_model$dosage_per_kg) & main_model$sa_tsh <= 10, na.rm = TRUE) #Fehlende Werte im sichtbaren Bereich
all_rows <- nrow(main_model)
dots <- all_rows - miss - high_tsh # Detail to Figure S2: Numbers of dots (visible)
weight_sr <- sum(main_model$weight_q == "Self-reported", na.rm = TRUE) # self-reported weight
height_sr <- sum(main_model$height_q == "Self-reported", na.rm = TRUE) # self-reported height

# Neue Zeilen als Data Frame erstellen
details_s3 <- data.frame(
  Kategorie = c("Detail to Figure S2: Unknown levothyroxine dose", 
                "Detail to Figure S2: TSH value above 10 mlU/L", 
                "Detail to Figure S2: Numbers of dots (visible)",
                "Detail to results section: Self reported weight",
                "Detail to results section: Self reported height"),
  x = c(no_dose, high_tsh, dots, weight_sr, height_sr),
  n = NA,
  Anteil = NA,
  CI_Unten = NA, 
  CI_Oben = NA
)

# Mit der alten Tabelle (final_table) verbinden
table0 <- rbind(final_table, details_s3)
colnames(table0) <- c("Category", "n", "Total", "Prevalence (%)", "95% CI (lower)", "95% CI (upper)")

knitr::kable(table0, 
             col.names = c("Category", "n", "Total", "Prevalence (%)", "95% CI (lower) ", "95% CI (upper)"),
             caption = "Details to Figure 2, Figure S1 & Figure S2")

# Pfad
file_name_tab_html <- "Details to Figure 2, Figure S1 & Figure S2.html"
full_path_tab_html <- file.path(folder_path, file_name_tab_html)
# Save in html format
tab_df(table0, 
       title = "Details to Figure 2, Figure S1 & Figure S2", 
       file = full_path_tab_html,
       column.labels = c("Category", "n", "Total", "Prevalence (%)", "95% CI (lower)", "95% CI (upper)"),
       show.rownames = FALSE)





# Estimates after multiple imputation -------------------------------------
row_order = c(1:8, 10, 9, 11:13, 15, 14, 16:45)

build_PRs_wide <- function(rr_list) {
  
  table_rr_results <- rr_list |>
    map(~ bind_rows(.x, .id = "model")) |>
    bind_rows(.id = "variable") |>
    filter(term != "(Intercept)") |>
    select(variable, model, term, RR, CI_low, CI_high,
           overtreatment, total, prevalence)
  unadjustedPRs <- table_rr_results |>
    filter(grepl("_1$", model)) |>
    mutate(
      prevalence = sprintf(
        "%.0f/%.0f (%.0f%%)",
        overtreatment, total,
        as.numeric(str_remove_all(prevalence, "[^0-9.]"))
      ),
      RR_CI = if_else(
        is.na(RR) | is.na(CI_low) | is.na(CI_high),
        "1 (reference)",
        sprintf("%.2f (%.2f-%.2f)", RR, CI_low, CI_high)
      )
    ) |>
    select(variable, term, prevalence, RR_CI)
  
  adjustedPRs <- table_rr_results |>
    filter(grepl("_2$", model)) |>
    mutate(RR_CI = sprintf("%.2f (%.2f-%.2f)", RR, CI_low, CI_high)) |>
    select(variable, term, RR_CI)
  
  fullyadjustedPRs <- table_rr_results |>
    filter(grepl("_3$", model)) |>
    mutate(RR_CI = sprintf("%.2f (%.2f-%.2f)", RR, CI_low, CI_high)) |>
    select(variable, term, RR_CI, RR, CI_low, CI_high)
  
  list(
    unadjusted       = unadjustedPRs,
    adjusted         = adjustedPRs,
    `fully adjusted` = fullyadjustedPRs
  ) |>
    imap(~ rename(.x, !!.y := RR_CI)) |>
    reduce(full_join, by = c("variable", "term")) |>
    slice(row_order) |>
    mutate(
      across(c(adjusted, `fully adjusted`), ~ coalesce(.x, "1 (reference)")),
      across(c(RR, CI_low, CI_high), ~ replace_na(.x, 1))
    ) |>
    mutate(term = str_remove(term, paste0("^", variable))) |>
      mutate(
        term = if_else(
          term %in% c("low risk", "high risk"),
          str_to_sentence(term),
          term
      )
    )
}



# Table 2 / S3 / S4 -----------------------------------------------------------

save_PR_table <- function(PRs_wide, filename, path = folder_path) {
  
  PRs_wide |>
    select(term, prevalence, unadjusted, adjusted, `fully adjusted`) |>
    gt() |>
    cols_label(
      term             = "Characteristic",
      prevalence       = "Counts/total (prevalence)",
      unadjusted       = "Unadjusted prevalence ratio (95% CI)",
      adjusted         = "Adjusted prevalence ratio (95% CI)*",
      `fully adjusted` = "Fully adjusted prevalence ratio (95% CI)**"
    ) |>
    tab_options(table.font.size = px(13)) |>
    tab_source_note(source_note = html(
      "* Adjusted for sex/gender, age, and region.<br>
       ** Adjusted for all characteristics shown in this table.<br>
       CI = confidence interval. GP = general practitioner."
    )) |>
    gtsave(filename = filename, path = path)
  
  invisible(PRs_wide)
}

results_sets <- list(
  rr_results      = "Table 2 Determinants of low TSH according to the prognostic cut-off.html",
  rr_results_ref  = "Table S4 Determinants of low TSH according to diagnostic cut-off.html",
  rr_results_sens = "Table S3 Determinants of low TSH sensitivity analysis.html"
)

PRs_wide_all <- list(
  main = rr_results,
  ref  = rr_results_ref,
  sens = rr_results_sens
) |>
  map(build_PRs_wide)

walk2(PRs_wide_all, unlist(results_sets), save_PR_table)



create_plot_for_imp <- function(data, zoom) {
  
  d1 <- data %>%
    rename(level = term) %>%
    mutate(
      estimate  = RR,
      conf.low  = CI_low,
      conf.high = CI_high,
      is_ref    = grepl("reference", `fully adjusted`, ignore.case = TRUE),
      group     = ifelse(is_ref, "Reference", "GroupA")
    )
  
  d1_clean <- d1 %>%
    mutate(
      level = gsub("&ge;",    "\u2265", level),
      level = gsub("&le;",    "\u2264", level),
      level = gsub("&lt;",    "\u003C", level),
      level = gsub("&gt;",    "\u003E", level),
      level = gsub("&ndash;", "\u2013", level),
      level = fct_inorder(level)
    ) %>%
    mutate(variable = dplyr::recode(variable,
                                    "sex"                  = "Sex/gender\n(Reference: male)",
                                    "ageclass"             = "Age (years)\n(Reference: 20\u201339)",
                                    "region"               = "Region\n(Reference: Eastern Germany)",
                                    "education"            = "Education level\n(Reference: medium)",
                                    "alcohol"              = "Risky alcohol consumption\n(Reference: low risk)",
                                    "smoking"              = "Smoking status\n(Reference: never)",
                                    "migration"            = "Migration background\n(Reference: no)",
                                    "bmi_cat"              = "Body mass index\n(Reference: normalweight)",
                                    "tsd_cat"              = "Time since first thyroid diagnosis (years)\n(Reference: \u22641)",
                                    "intake_duration"      = "Duration of levothyroxine use (years)\n(Reference: \u003c1)",
                                    "poly"                 = "Use of \u22655 different medications\n(Reference: no)",
                                    "atc_combi"            = "Prescribed iodine (in combination with levothyroxine)\n(Reference: no)",
                                    "radioiodine"          = "History of radioiodine therapy\n(Reference: no)",
                                    "surgery"              = "History of thyroid surgery\n(Reference: no)",
                                    "internist"            = "Visit to an internist within the past 12 months\n(Reference: not even once)",
                                    "general_practitioner" = "Time since last general practitioner visit (months)\n(Reference: \u22641)",
                                    "dosage_per_kg_cat"    = "Levothyroxine dose (\u00B5g/kg/day)\n(Reference: \u22640.67)"
    ))
  
  target_order <- c(
    "Sex/gender\n(Reference: male)",
    "Age (years)\n(Reference: 20\u201339)",
    "Region\n(Reference: Eastern Germany)",
    "Education level\n(Reference: medium)",
    "Migration background\n(Reference: no)",
    "Body mass index\n(Reference: normalweight)",
    "Smoking status\n(Reference: never)",
    "Risky alcohol consumption\n(Reference: low risk)",
    "Use of \u22655 different medications\n(Reference: no)",
    "Time since last general practitioner visit (months)\n(Reference: \u22641)",
    "Visit to an internist within the past 12 months\n(Reference: not even once)",
    "Time since first thyroid diagnosis (years)\n(Reference: \u22641)",
    "History of radioiodine therapy\n(Reference: no)",
    "History of thyroid surgery\n(Reference: no)",
    "Prescribed iodine (in combination with levothyroxine)\n(Reference: no)",
    "Duration of levothyroxine use (years)\n(Reference: \u003c1)",
    "Levothyroxine dose (\u00B5g/kg/day)\n(Reference: \u22640.67)"
  )
  
  d1_clean <- d1_clean %>%
    mutate(variable = fct_relevel(
      variable, intersect(target_order, as.character(unique(variable)))
    )) %>%
    mutate(level_txt = as.character(level)) %>%
    mutate(sort_id = case_when(
      variable == "Time since last general practitioner visit (months)\n(Reference: \u22641)" & level_txt == "\u22641"      ~ 5,
      variable == "Time since last general practitioner visit (months)\n(Reference: \u22641)" & level_txt == "2\u20133"     ~ 4,
      variable == "Time since last general practitioner visit (months)\n(Reference: \u22641)" & level_txt == "4\u201312"    ~ 3,
      variable == "Time since last general practitioner visit (months)\n(Reference: \u22641)" & level_txt == "\u003E12"     ~ 2,
      
      variable == "Time since first thyroid diagnosis (years)\n(Reference: \u22641)" & level_txt == "\u22641"      ~ 5,
      variable == "Time since first thyroid diagnosis (years)\n(Reference: \u22641)" & level_txt == "1 to \u003C3"  ~ 4,
      variable == "Time since first thyroid diagnosis (years)\n(Reference: \u22641)" & level_txt == "3 to \u003C10" ~ 3,
      variable == "Time since first thyroid diagnosis (years)\n(Reference: \u22641)" & level_txt == "\u226510"      ~ 2,
      
      variable == "Duration of levothyroxine use (years)\n(Reference: \u003c1)" & level_txt == "\u003C1"      ~ 4,
      variable == "Duration of levothyroxine use (years)\n(Reference: \u003c1)" & level_txt == "1 to \u003C3"  ~ 3,
      variable == "Duration of levothyroxine use (years)\n(Reference: \u003c1)" & level_txt == "\u22653"       ~ 2,
      
      variable == "Sex/gender\n(Reference: male)" & level_txt == "Male"   ~ 2,
      variable == "Sex/gender\n(Reference: male)" & level_txt == "Female" ~ 1,
      
      variable == "Education level\n(Reference: medium)" & level_txt == "Low"    ~ 4,
      variable == "Education level\n(Reference: medium)" & level_txt == "Medium" ~ 3,
      variable == "Education level\n(Reference: medium)" & level_txt == "High"   ~ 2,
      
      variable == "Body mass index\n(Reference: normalweight)" & level_txt == "Underweight"  ~ 5,
      variable == "Body mass index\n(Reference: normalweight)" & level_txt == "Normalweight" ~ 4,
      variable == "Body mass index\n(Reference: normalweight)" & level_txt == "Preadiposity" ~ 3,
      variable == "Body mass index\n(Reference: normalweight)" & level_txt == "Adiposity"    ~ 2,
      
      TRUE ~ as.numeric(fct_rev(level))
    ))
  
  # Achsenbeschriftung und Limits
  if (zoom == 3) {
    y_breaks <- c(0.5, 0.8, 1, 1.2, 1.5, 2, 3)
    y_limits <- c(0.3, 3.2)
  } else {
    y_breaks <- c(0.2, 0.5, 1, 2, 5)
    y_limits <- c(0.15, 6)
  }
  y_max <- max(y_limits)
  
  # Abschneiden und Pfeile
  data_clipped <- d1_clean %>%
    mutate(
      has_arrow         = conf.high > y_max,
      conf.high_clipped = ifelse(has_arrow, y_max, conf.high)
    )
  
  ggplot(
    data = data_clipped,
    mapping = aes(x = reorder(level, sort_id), y = estimate, colour = group,
                  ymin = conf.low, ymax = conf.high)
  ) +
    facet_col(
      facets = vars(variable),
      scales = "free_y",
      space  = "free",
      strip.position = "left"
    ) +
    coord_flip(clip = "on") +
    geom_hline(yintercept = 1, linetype = "dashed") +
    geom_pointrange(aes(ymin = conf.low, ymax = conf.high_clipped),
                    linewidth = 0.8, size = 0.4) +
    geom_segment(
      data = filter(data_clipped, has_arrow == TRUE),
      aes(x = reorder(level, sort_id), xend = reorder(level, sort_id),
          y = y_max * 0.95, yend = y_max),
      arrow = arrow(length = unit(0.15, "cm"), type = "closed"),
      linewidth = 0.8, show.legend = FALSE
    ) +
    scale_color_manual(values = my_colors_plot) +
    scale_y_continuous(
      name   = "Prevalence ratios (95% confidence interval)",
      breaks = y_breaks,
      labels = as.character(y_breaks),
      trans  = "log",
      limits = y_limits,
      minor_breaks = NULL
    ) +
    xlab("") +
    theme_minimal() +
    theme(
      panel.spacing.y    = unit(0.6, "lines"),
      panel.grid.major.x = element_line(color = "gray90", linewidth = 0.5),
      panel.grid.minor.x = element_line(color = "gray90", linewidth = 0.25),
      panel.grid.major.y = element_blank(),
      panel.border       = element_rect(color = "gray85", fill = NA, linewidth = 0.8),
      strip.text.y.left  = element_text(angle = 0, hjust = 0),
      strip.placement    = "outside",
      axis.text.x        = element_text(face = "bold"),
      axis.text.y        = element_text(),
      axis.title.x       = element_text(size = 9),
      legend.position    = "none"
    )
}


# Create plot for imputed data
plots_imp <- map2(PRs_wide_all, c(3, 6, 3), create_plot_for_imp)
#Save plots
file_name_f2    <- "Figure 3.svg"
full_path_f2    <- file.path(folder_path, file_name_f2)
ggsave(full_path_f2, plot = plots_imp$main, width = 19.5, height = 26, units = "cm")
file_name_f2    <- "Figure S3.svg"
full_path_f2    <- file.path(folder_path, file_name_f2)
ggsave(full_path_f2, plot = plots_imp$sens, width = 19.5, height = 26, units = "cm")
file_name_f2    <- "Figure S4.svg"
full_path_f2    <- file.path(folder_path, file_name_f2)
ggsave(full_path_f2, plot = plots_imp$ref, width = 19.5, height = 26, units = "cm")






# Delta method approach ---------------------------------------------------
output_overtreated_tsd |> dplyr::as_tibble()
output_overtreated_dosage |> dplyr::as_tibble() 

tab <- output_overtreated_tsd |>
  dplyr::as_tibble() |>
  gt() |>
  tab_header(
    title = "Time since first thyroid diagnosis (years): Multiple imputation with delta adjustments",
    subtitle = "Risk ratios"
  ) |>
  fmt_number(columns = where(is.numeric), decimals = 2) |>
  cols_align(align = "right", columns = where(is.numeric)) |>
  tab_options(
    table.font.size = px(12),
    data_row.padding = px(4),
    column_labels.font.weight = "bold"
  )

gtsave(tab, "output_overtreated_tsd.docx", path=folder_path) 

tab2 <- output_overtreated_dosage |>
  dplyr::as_tibble() |>
  gt() |>
  tab_header(
    title = "Levothyroxine dose (µg/kg/day): Multiple imputation with delta adjustments",
    subtitle = "Risk ratios"
  ) |>
  fmt_number(columns = where(is.numeric), decimals = 2) |>
  cols_align(align = "right", columns = where(is.numeric)) |>
  tab_options(
    table.font.size = px(12),
    data_row.padding = px(4),
    column_labels.font.weight = "bold"
  )

tab2 <- output_overtreated_dosage |>
  as_tibble() |>
  # 1) Zahlen VOR dem Drehen formatieren (danach ist alles character)
  mutate(across(where(is.numeric), ~ format(round(.x, 2), nsmall = 2))) |>
  mutate(across(everything(), as.character)) |>
  # 2) drehen: erste Spalte wird zur Kopfzeile
  pivot_longer(-1, names_to = "Measure", values_to = "value") |>
  pivot_wider(names_from = 1, values_from = "value") |>
  # 3) wie gehabt setzen
  gt() |>
  tab_header(
    title = "Levothyroxine dose (µg/kg/day): Multiple imputation with delta adjustments",
    subtitle = "Risk ratios"
  ) |>
  cols_align(align = "right", columns = -Measure) |>
  cols_align(align = "left",  columns = Measure) |>
  cols_label(Measure = "") |>
  tab_options(
    table.font.size = px(12),
    data_row.padding = px(4),
    column_labels.font.weight = "bold"
  )

gtsave(tab2, "output_overtreated_dosage.docx", , path=folder_path) 



# Complete Case -------------------------------------------------------------
vars_mod <- c(
  "sex", "ageclass", "region", "education", "migration", "bmi_cat",
  "smoking", "alcohol", "poly", "general_practitioner", "tsd_cat",
  "radioiodine", "surgery", "atc_combi", "intake_duration",
  "dosage_per_kg_cat"
)
# Sample size complete case
sum(complete.cases(main_model[vars_mod]))
#Datensatz complete case erstellen
main_cc <- main_model[complete.cases(main_model[, vars_mod]), ]



# Table 2 (generisch) --------------------------------------------------------

# 1) Zeilenblöcke: Variable, Label, Filter "Unknown"
table2_exposures <- tibble::tribble(
  ~var,                   ~label,                                                  ~drop_unknown,
  "sex",                  "Sex/gender",                                            TRUE,
  "ageclass",             "Age (years)",                                           TRUE,
  "region",               "Region",                                                TRUE,
  "education",            "Education level",                                       TRUE,
  "migration",            "Migration background",                                  TRUE,
  "bmi_cat",              "Body mass index",                                       TRUE,
  "smoking",              "Smoking status",                                        TRUE,
  "alcohol",              "Risky alcohol consumption",                             TRUE,
  "poly",                 "Use of \u22655 different medications",                   TRUE,
  "general_practitioner", "Time since last GP visit (months)",                     TRUE,
  "tsd_cat",              "Time since first thyroid diagnosis (years)",             TRUE,
  "atc_combi",            "Prescribed iodine (in combination with levothyroxine)",  TRUE,
  "intake_duration",      "Duration of levothyroxine use (years)",                  TRUE,
  "dosage_per_kg_cat",    "Levothyroxine dose (\u00B5g/kg/day)",                     TRUE
)

# 2) Confounder-String bauen (Exposition wird immer herausgenommen)
conf_string <- function(vars, exposure) {
  vars <- setdiff(vars, exposure)
  if (length(vars) == 0) return(NA_character_)
  paste0("+ ", paste(vars, collapse = " + "))
}

# 3) Design für eine Exposition
make_design <- function(exposure,
                        adjusted,
                        fully_adjusted,
                        outcome = "overtreated",
                        ci = 0.95,
                        labels = c(
                          prevalence = "Counts/total (prevalence)",
                          unadjusted = "Unadjusted prevalence ratio (95% CI)",
                          adjusted   = "Adjusted prevalence ratio (95% CI)*",
                          fully      = "Fully adjusted prevalence ratio (95% CI)**"
                        )) {
  tibble::tibble(
    label = unname(labels[c("prevalence", "unadjusted", "adjusted", "fully")]),
    type  = c("outcomes/total (risk)", "rr", "rr", "rr"),
    confounders = c(
      NA_character_,
      NA_character_,
      conf_string(adjusted,       exposure),
      conf_string(fully_adjusted, exposure)
    ),
    arguments = list(NULL,
                     list(approach = "duplicate"),
                     list(approach = "duplicate"),
                     list(approach = "duplicate")),
    exposure = exposure,
    outcome  = outcome,
    ci       = ci
  )
}

# 4) Erste Spalte umbenennen
prepare_tab <- function(tab) {
  colnames(tab)[1] <- "Characteristic"
  tab
}

# 5) Ein Block
make_block <- function(data, exposure, label, drop_unknown,
                       adjusted, fully_adjusted, outcome = "ohr_bin", ci = 0.95) {
  tab <- rifttable(
    design = make_design(exposure, adjusted, fully_adjusted, outcome = outcome, ci = ci),
    data = data, to = "-", risk_percent = TRUE, layout = "cols"
  )
  tab <- dplyr::add_row(tab, !!!rlang::set_names(list(label), names(tab)[1]), .before = 1)
  if (isTRUE(drop_unknown)) {
    keep <- !is.na(tab[[1]]) & tab[[1]] != "Unknown"
    tab <- tab[keep, ]
  }
  prepare_tab(tab)
}

# 6) Ganze Tabelle
build_table2 <- function(data,
                         adjusted,
                         fully_adjusted,
                         exposures = NULL,
                         spec = table2_exposures,
                         outcome = "ohr_bin",
                         ci = 0.95) {
  # Standard: genau die Variablen des fully adjusted Modells als Zeilenblöcke,
  # in der Reihenfolge der Spezifikation
  if (is.null(exposures)) exposures <- intersect(spec$var, fully_adjusted)
  spec <- spec[match(exposures, spec$var), ]
  
  blocks <- purrr::pmap(
    list(spec$var, spec$label, spec$drop_unknown),
    function(v, l, u) make_block(data, v, l, u, adjusted, fully_adjusted, outcome, ci)
  )
  dplyr::bind_rows(blocks)
}

adjusted_vars <- c("sex", "ageclass", "region")

fully_adjusted_vars <- c(
  "sex", "ageclass", "region", "bmi_cat", "education", "migration",
  "dosage_per_kg_cat", "alcohol", "atc_combi", "intake_duration",
  "poly", "general_practitioner", "tsd_cat", "radioiodine", "surgery"
)

# Fit complete Case 
table2_main_cc    <- build_table2(main_cc,    adjusted_vars, fully_adjusted_vars)
print(table2_main_cc)

#export function
save_table2 <- function(tab, filename, path = folder_path) {
  out <- tab |>
    rt_gt() |>
    gt::text_transform(
      locations = cells_body(),
      fn = function(x) gsub("(\\d)(?=(\\d{3})+(?!\\d))", "\\1,", x, perl = TRUE)
    ) |>
    sub_missing(columns = where(~ !all(is.na(.))), missing_text = "") |>
    gt::tab_footnote(footnote = html(
      "* Adjusted for sex/gender, age, and region.<br>
      ** Adjusted for all characteristics shown in this table.<br>
      CI=confidence Interval. GP=general practitioner."))
  gtsave(data = out, filename = filename, path = path)
  invisible(out)
}
#export table
save_table2(table2_main_cc, "Table SX Determinants of levothyroxine overtreatment Complete Case.html")



# Figure 2 ----------------------------------------------------------------
#main analysis
fit_main_cc <- riskratio(formula= ohr_bin ~ ageclass + sex + region + bmi_cat + education + migration +
                           smoking + alcohol + dosage_per_kg_cat + atc_combi + intake_duration + poly + radioiodine +
                           general_practitioner + tsd_cat + surgery, approach="duplicate", data=main_model)


create_plot1 <- function(data, zoom){
  
  d0 <- data %>%
    tidy(exponentiate = TRUE, conf.int = TRUE, conf.level = 0.95) %>% 
    tidy_categorical(m = data, exponentiate = TRUE) %>%
    slice(-1) # Entfernt den Intercept
  
  d1 <- d0 %>%
    mutate(group=case_when(
      level == 'Unknown' ~ "Unknown", 
      level != 'Unknown' & reference == 'Baseline Category' ~ "Reference",
      TRUE ~ "GroupA"))
  
  d1 %>%
    select(-(3:5))
  
  d1_clean <- d1 %>%
    mutate(
      # HTML-Entities 
      level = gsub("&ge;", "\u2265", level),
      level = gsub("&le;", "\u2264", level),
      level = gsub("&lt;", "\u003C", level),
      level = gsub("&gt;", "\u003E", level),
      level = gsub("&ndash;", "\u2013", level),
      level = fct_inorder(level)
    ) %>%
    mutate(variable = dplyr::recode(variable, 
                                    "sex" = "Sex/gender\n(Reference: male)", 
                                    "ageclass" = "Age (years)\n(Reference: 20\u201339)",
                                    "region" = "Region\n(Reference: Eastern Germany)",
                                    "education" = "Education level\n(Reference: medium)",
                                    "alcohol" = "Risky alcohol consumption\n(Reference: low risk)",
                                    "smoking" = "Smoking status\n(Reference: never)",
                                    "migration" = "Migration background\n(Reference: no)",
                                    "bmi_cat" = "Body mass index\n(Reference: normalweight)",
                                    "tsd_cat" = "Time since first thyroid diagnosis (years)\n(Reference: \u22641)",
                                    "intake_duration" = "Duration of levothyroxine use (years)\n(Reference: \u003c1)",
                                    "poly" = "Use of \u22655 different medications\n(Reference: no)",
                                    "atc_combi" =  "Prescribed iodine (in combination with levothyroxine)\n(Reference: no)",
                                    "radioiodine" = "History of radioiodine therapy\n(Reference: no)",
                                    "surgery" = "History of thyroid surgery\n(Reference: no)",
                                    "internist" = "Visit to an internist within the past 12 months\n(Reference: not even once)",
                                    "general_practitioner" = "Time since last general practitioner visit (months)\n(Reference: \u22641)",
                                    "dosage_per_kg_cat" = "Levothyroxine dose (\u00B5g/kg/day)\n(Reference: \u22640.67)"))%>%
    
    mutate(
      estimate  = ifelse(group == "Reference", 1, estimate),
      conf.low  = ifelse(group == "Reference", 1, conf.low),
      conf.high = ifelse(group == "Reference", 1, conf.high)
    )
  
  target_order <- c(
    "Sex/gender\n(Reference: male)", 
    "Age (years)\n(Reference: 20\u201339)", 
    "Region\n(Reference: Eastern Germany)", 
    "Education level\n(Reference: medium)",
    "Migration background\n(Reference: no)",
    "Body mass index\n(Reference: normalweight)",
    "Smoking status\n(Reference: never)",
    "Risky alcohol consumption\n(Reference: low risk)",
    "Use of \u22655 different medications\n(Reference: no)",
    "Time since last general practitioner visit (months)\n(Reference: \u22641)",
    "Visit to an internist within the past 12 months\n(Reference: not even once)",
    "Time since first thyroid diagnosis (years)\n(Reference: \u22641)",
    "History of radioiodine therapy\n(Reference: no)",
    "History of thyroid surgery\n(Reference: no)",
    "Prescribed iodine (in combination with levothyroxine)\n(Reference: no)",
    "Duration of levothyroxine use (years)\n(Reference: \u003c1)",
    "Levothyroxine dose (\u00B5g/kg/day)\n(Reference: \u22640.67)"
  )
  
  d1_clean <- d1_clean %>%
    mutate(variable = fct_relevel(variable, intersect(target_order, as.character(unique(variable))))) %>%
    mutate(level_txt = as.character(level)) %>%
    
    # Eine eindeutige Sortier-ID pro Kategorie erstellen (Höhere Zahl = weiter OBEN im Plot)
    mutate(sort_id = case_when(
      # "Unknown" wird für ALLE Variablen global als niedrigster Wert definiert
      level_txt == "Unknown" ~ 1,
      
      
      # Variable 1: Last general practitioner visit
      variable == "Time since last general practitioner visit (months)\n(Reference: \u22641)" & level_txt == "\u22641"   ~ 5,
      variable == "Time since last general practitioner visit (months)\n(Reference: \u22641)" & level_txt == "2\u20133" ~ 4, 
      variable == "Time since last general practitioner visit (months)\n(Reference: \u22641)" & level_txt == "4\u201312" ~ 3,
      variable == "Time since last general practitioner visit (months)\n(Reference: \u22641)" & level_txt == "\u003E12"  ~ 2,
      
      # Variable 2: Time since first thyroid diagnosis
      variable == "Time since first thyroid diagnosis (years)\n(Reference: \u22641)" & level_txt == "\u22641"       ~ 5,
      variable == "Time since first thyroid diagnosis (years)\n(Reference: \u22641)" & level_txt == "1 to \u003C3"  ~ 4,
      variable == "Time since first thyroid diagnosis (years)\n(Reference: \u22641)" & level_txt == "3 to \u003C10" ~ 3,
      variable == "Time since first thyroid diagnosis (years)\n(Reference: \u22641)" & level_txt == "\u226510"      ~ 2,
      
      # Variable 3: Duration of levothyroxine use
      variable == "Duration of levothyroxine use (years)\n(Reference: \u003c1)" & level_txt == "\u003C1"      ~ 4, 
      variable == "Duration of levothyroxine use (years)\n(Reference: \u003c1)" & level_txt == "1 to \u003C3" ~ 3,
      variable == "Duration of levothyroxine use (years)\n(Reference: \u003c1)" & level_txt == "\u22653"     ~ 2,
      
      variable == "Sex/gender\n(Reference: male)" & level_txt == "Male" ~ 2,
      variable == "Sex/gender\n(Reference: male)" & level_txt == "Female" ~ 1,
      
      variable == "Education level\n(Reference: medium)" & level_txt == "Low" ~ 4,
      variable == "Education level\n(Reference: medium)" & level_txt == "Medium" ~ 3,
      variable == "Education level\n(Reference: medium)" & level_txt == "High" ~ 2,
      
      variable == "Body mass index\n(Reference: normalweight)" & level_txt == "Underweight" ~ 5,
      variable == "Body mass index\n(Reference: normalweight)" & level_txt == "Normalweight" ~ 4,
      variable == "Body mass index\n(Reference: normalweight)" & level_txt == "Preadiposity" ~ 3,
      variable == "Body mass index\n(Reference: normalweight)" & level_txt == "Adiposity" ~ 2,
      
      # CATCH-ALL für alle anderen Variablen (nutzt die ursprüngliche Reihenfolge, dreht sie aber um)
      TRUE ~ as.numeric(fct_rev(level)) 
    ))
  # 1. Dynamische Achsenbeschriftung und Limits definieren
  if (zoom == 3) {
    y_breaks <- c(0.5, 0.8, 1, 1.2, 1.5, 2, 3)
    y_limits <- c(0.3, 3.2)
  } else {
    y_breaks <- c(0.2, 0.5, 1, 2, 5, 7.5)
    y_limits <- c(0.15, 7.5)
  }
  # 2. Hilfsvariablen für das Abschneiden und die Pfeile berechnen
  data_clipped <- d1_clean %>%
    mutate(
      # Zeichne Pfeil, wenn das echte Intervall das Limit sprengt
      has_arrow = conf.high > zoom,
      # Intervall für die normale Pointrange
      conf.high_clipped = ifelse(has_arrow, zoom, conf.high)
    )
  p <- ggplot(data = data_clipped, 
              mapping = aes(x = reorder(level, sort_id), y = estimate, colour = group,
                            ymin = conf.low, ymax = conf.high)) +
    facet_col(
      facets = vars(variable), 
      scales = "free_y", 
      space = "free",
      strip.position = "left"
    )+
    coord_flip() +
    coord_flip(clip = "on") + 
    geom_hline(yintercept = 1, linetype = "dashed") +
    # Standard-Fehlerbalken (jetzt auf das Limit gedeckelt)
    geom_pointrange(aes(ymin = conf.low, ymax = conf.high_clipped), 
                    linewidth = 0.8, size = 0.4) +
    
    # Der Pfeil am Rand. Wird nur gezeichnet, wenn das CI das Limit sprengt.
    geom_segment(
      data = filter(data_clipped, has_arrow == TRUE),
      aes(x = reorder(level, sort_id), xend = reorder(level, sort_id), 
          y = zoom * 0.95, yend = zoom), # Startet kurz vor dem Rand und läuft exakt bis zur Kante
      arrow = arrow(length = unit(0.15, "cm"), type = "closed"),
      linewidth = 0.8, show.legend = FALSE
    ) +
    scale_color_manual(values = my_colors_plot) +
    
    scale_y_continuous(
      name = "Prevalence ratios(95% confidence interval)",
      breaks = y_breaks,
      labels = as.character(y_breaks), # Konvertiert die Breaks automatisch in Text-Labels
      trans = "log",
      limits = y_limits,
      minor_breaks = NULL 
    ) +
    xlab("") +
    theme_minimal()+
    theme(
      # panel.background = element_rect(fill = "white", color = NA),
      panel.spacing.y = unit(0.6, "lines"),
      panel.grid.major.x = element_line(color = "gray90", linewidth = 0.5), 
      panel.grid.minor.x = element_line(color = "gray90", linewidth = 0.25), 
      panel.grid.major.y = element_blank(), 
      panel.border = element_rect(color = "gray85", fill = NA, linewidth = 0.8),
      # strip.background = element_rect(color = "gray85", fill = NA, linewidth = 0.8),
      strip.text.y.left = element_text(angle = 0, hjust=0),
      strip.placement = "outside",
      axis.text.x = element_text(face="bold"), 
      axis.text.y = element_text(),
      axis.title.x = element_text(size = 9), 
      legend.position = "none"
    )
  return(p)
}


# Create plot for complete case 
plot_cc <- create_plot1(fit_main_cc, 3)
print(plot_cc)
# Save plot
file_name_f2    <- "Figure SX Complete case.svg"
full_path_f2    <- file.path(folder_path, file_name_f2)
ggsave(full_path_f2, plot = plot_cc, width = 19.5, height = 26, units = "cm")



