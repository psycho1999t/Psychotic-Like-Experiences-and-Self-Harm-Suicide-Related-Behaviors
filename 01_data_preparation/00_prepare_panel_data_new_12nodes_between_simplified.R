options(stringsAsFactors = FALSE)

suppressPackageStartupMessages({
  library(data.table)
  library(dplyr)
  library(tidyr)
  library(openxlsx)
})

set.seed(1234)

# =============================================================================
# 00_prepare_panel_data_new_12nodes.R
# Purpose:
#   Prepare the longitudinal panel-network dataset for Panel GVAR estimation.
#
# This 12-node new-three-wave version:
#   1) Defaults to the enlarged loose source+phone recovery matched long dataset.
#   2) Uses id_key as the longitudinal identifier.
#   3) Keeps pe1-pe8 and si1-si4 as separate analysis nodes.
#   4) Exports pooled-z and detrended datasets for downstream Panel GVAR models.
# =============================================================================

# -----------------------------
# Configuration
# -----------------------------

idvar <- "id_key"
timevar <- "wave"
groupvar <- "gender"
waves <- 1:3

input_item_vars <- c(paste0("pe", 1:8), paste0("si", 1:4))
vars <- input_item_vars
required_cols <- c(idvar, timevar, groupvar, input_item_vars)

input_path <- Sys.getenv("PANEL_INPUT_PATH", unset = "")
if (input_path == "") {
  stop("Set PANEL_INPUT_PATH to the already matched three-wave long-format input file.")
}

if (!file.exists(input_path)) {
  stop("Input long dataset does not exist: ", input_path)
}

default_output_base <- file.path(
  "final results",
  "pn12_between_simplified",
  "loose_source_phone_recovery"
)
output_base <- Sys.getenv("PANEL_OUTPUT_BASE", unset = default_output_base)
primary_analysis_dataset <- Sys.getenv("PANEL_PRIMARY_DATASET", unset = "detrended")
primary_analysis_dataset <- match.arg(primary_analysis_dataset, c("detrended", "pooled_z"))

run_stamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
output_root <- file.path(output_base, paste0("run_", run_stamp))
dir_data <- file.path(output_root, "data")
dir_tables <- file.path(output_root, "tables")
dir_notes <- file.path(output_root, "notes")

for (dir_path in c(output_base, output_root, dir_data, dir_tables, dir_notes)) {
  dir.create(dir_path, recursive = TRUE, showWarnings = FALSE)
}

# -----------------------------
# Metadata
# -----------------------------

node_meta <- data.frame(
  raw_name = vars,
  node_label = c(
    "Referential Thinking",
    "Persecutory Feelings",
    "Thought Withdrawal",
    "Alien Thoughts",
    "Thought Broadcasting",
    "External Control",
    "Auditory Hallucinations",
    "Visual Hallucinations",
    "Self-Harm Thoughts",
    "Self-Harm Behavior",
    "Suicidal Thoughts",
    "Life-Threatening Behavior"
  ),
  short_label = c(paste0("A", 1:8), paste0("B", 1:4)),
  community = c(
    rep("Psychotic Experiences", 8),
    rep("Self-Harm/Suicidality", 4)
  ),
  color = c(
    rep("#2F6C8F", 8),
    rep("#C65A5A", 4)
  ),
  stringsAsFactors = FALSE
)

community_palette <- stats::setNames(
  unique(node_meta$color),
  unique(node_meta$community)
)

node_dictionary <- node_meta %>%
  transmute(
    raw_variable = .data$raw_name,
    abbreviation = .data$short_label,
    english_label = .data$node_label,
    community_label = .data$community
  )

# -----------------------------
# Helper functions
# -----------------------------

write_csv <- function(df, filename) {
  if (is.null(df) || length(df) == 0 || (is.data.frame(df) && ncol(df) == 0)) {
    df <- data.frame(note = "No rows met the export criterion for this file.")
  }
  data.table::fwrite(as.data.frame(df), file = filename, na = "")
}

add_sheet <- function(wb, sheet_name, obj) {
  safe_name <- substr(sheet_name, 1, 31)
  if (is.matrix(obj)) {
    obj <- data.frame(node = rownames(obj), obj, check.names = FALSE)
  }
  if (!is.data.frame(obj)) {
    obj <- as.data.frame(obj, stringsAsFactors = FALSE)
  }
  if (ncol(obj) == 0) {
    obj <- data.frame(note = "No rows met the export criterion for this sheet.")
  }
  obj[] <- lapply(obj, function(col) {
    if (is.list(col)) {
      return(vapply(col, function(x) paste(x, collapse = "; "), character(1)))
    }
    col
  })
  obj[is.na(obj)] <- ""
  openxlsx::addWorksheet(wb, safe_name)
  openxlsx::writeData(wb, safe_name, obj)
}

normalize_id <- function(x) {
  x <- trimws(as.character(x))
  x[x %in% c("", "NA", "NaN", "NULL")] <- NA_character_
  numeric_like <- grepl("^[0-9eE+\\.-]+$", x)
  suppressWarnings(num_x <- as.numeric(x))
  convert_idx <- numeric_like & !is.na(num_x)
  if (any(convert_idx)) {
    x[convert_idx] <- format(num_x[convert_idx], scientific = FALSE, trim = TRUE)
  }
  x <- sub("\\.0+$", "", x)
  x
}

gender_to_label <- function(x) {
  dplyr::case_when(
    x == 1 ~ "Male",
    x == 2 ~ "Female",
    TRUE ~ paste0("Gender_", x)
  )
}

make_composite_sum <- function(...) {
  value_list <- lapply(list(...), function(x) suppressWarnings(as.numeric(x)))
  value_mat <- do.call(cbind, value_list)
  out <- rowSums(value_mat, na.rm = TRUE)
  all_missing <- rowSums(!is.na(value_mat)) == 0
  out[all_missing] <- NA_real_
  out
}

calc_shape_stats <- function(x) {
  x <- x[is.finite(x)]
  if (length(x) < 3) {
    return(c(skew = NA_real_, kurtosis = NA_real_))
  }
  s <- stats::sd(x)
  if (!is.finite(s) || s == 0) {
    return(c(skew = NA_real_, kurtosis = NA_real_))
  }
  z <- (x - mean(x)) / s
  c(
    skew = mean(z^3),
    kurtosis = mean(z^4) - 3
  )
}

summarise_variable <- function(x) {
  x_num <- suppressWarnings(as.numeric(x))
  n <- length(x_num)
  miss <- sum(is.na(x_num))
  nonmiss <- x_num[!is.na(x_num)]
  shape <- calc_shape_stats(nonmiss)
  data.frame(
    n = n,
    missing = miss,
    missing_rate = miss / n,
    mean = if (length(nonmiss)) mean(nonmiss) else NA_real_,
    sd = if (length(nonmiss) > 1) stats::sd(nonmiss) else NA_real_,
    variance = if (length(nonmiss) > 1) stats::var(nonmiss) else NA_real_,
    min = if (length(nonmiss)) min(nonmiss) else NA_real_,
    max = if (length(nonmiss)) max(nonmiss) else NA_real_,
    n_unique = length(unique(nonmiss)),
    zero_rate = if (length(nonmiss)) mean(nonmiss == 0) else NA_real_,
    skew = shape[["skew"]],
    kurtosis = shape[["kurtosis"]],
    stringsAsFactors = FALSE
  )
}

build_quality_by_wave <- function(dt, vars, node_meta) {
  out <- lapply(waves, function(w) {
    sub_dt <- dt[get(timevar) == w]
    rbindlist(lapply(vars, function(v) {
      as.data.table(cbind(
        wave = w,
        raw_name = v,
        summarise_variable(sub_dt[[v]])
      ))
    }), fill = TRUE)
  })
  as.data.frame(rbindlist(out, fill = TRUE)) %>%
    left_join(node_meta, by = "raw_name") %>%
    select(
      wave, raw_name, n, missing, missing_rate, mean, sd, variance,
      min, max, n_unique, zero_rate, skew, kurtosis,
      short_label, node_label, community
    )
}

build_quality_pooled <- function(dt, vars, node_meta) {
  as.data.frame(rbindlist(lapply(vars, function(v) {
    as.data.table(cbind(raw_name = v, summarise_variable(dt[[v]])))
  }), fill = TRUE)) %>%
    left_join(node_meta, by = "raw_name") %>%
    select(
      raw_name, n, missing, missing_rate, mean, sd, variance,
      min, max, n_unique, zero_rate, skew, kurtosis,
      short_label, node_label, community
    )
}

build_pooled_z <- function(dt, vars, node_meta) {
  z_dt <- data.table::copy(dt)
  stat_rows <- vector("list", length(vars))
  names(stat_rows) <- vars

  for (v in vars) {
    vec <- suppressWarnings(as.numeric(z_dt[[v]]))
    mu <- mean(vec, na.rm = TRUE)
    sigma <- stats::sd(vec, na.rm = TRUE)
    if (!is.finite(sigma) || sigma == 0) {
      z_dt[[v]] <- NA_real_
    } else {
      z_dt[[v]] <- (vec - mu) / sigma
    }
    stat_rows[[v]] <- data.frame(
      raw_name = v,
      pooled_mean = mu,
      pooled_sd = sigma,
      stringsAsFactors = FALSE
    )
  }

  standardization_table <- bind_rows(stat_rows) %>%
    left_join(node_meta, by = "raw_name") %>%
    select(raw_name, pooled_mean, pooled_sd, short_label, node_label, community)

  list(data_z = z_dt, standardization_table = standardization_table)
}

build_trend_tests <- function(z_dt, vars, node_meta) {
  bind_rows(lapply(vars, function(v) {
    idx <- is.finite(z_dt[[v]]) & is.finite(z_dt[[timevar]])
    if (sum(idx) < 3) {
      return(data.frame(
        raw_name = v,
        intercept = NA_real_,
        wave_slope = NA_real_,
        wave_se = NA_real_,
        wave_t = NA_real_,
        wave_p = NA_real_,
        r_squared = NA_real_,
        stringsAsFactors = FALSE
      ))
    }
    fit <- stats::lm(z_dt[[v]][idx] ~ z_dt[[timevar]][idx])
    sm <- summary(fit)
    coef_tab <- sm$coefficients
    data.frame(
      raw_name = v,
      intercept = unname(coef_tab[1, 1]),
      wave_slope = unname(coef_tab[2, 1]),
      wave_se = unname(coef_tab[2, 2]),
      wave_t = unname(coef_tab[2, 3]),
      wave_p = unname(coef_tab[2, 4]),
      r_squared = unname(sm$r.squared),
      stringsAsFactors = FALSE
    )
  })) %>%
    left_join(node_meta, by = "raw_name") %>%
    select(raw_name, intercept, wave_slope, wave_se, wave_t, wave_p, r_squared,
           short_label, node_label, community)
}

build_detrended <- function(z_dt, vars) {
  out <- data.table::copy(z_dt)
  for (v in vars) {
    idx <- is.finite(out[[v]]) & is.finite(out[[timevar]])
    res <- rep(NA_real_, nrow(out))
    if (sum(idx) >= 3) {
      fit <- stats::lm(out[[v]][idx] ~ out[[timevar]][idx])
      res[idx] <- stats::residuals(fit)
    }
    out[[v]] <- res
  }
  out
}

build_wide <- function(dt, vars) {
  tmp <- data.table::melt(
    dt[, c(idvar, groupvar, "gender_label", timevar, vars), with = FALSE],
    id.vars = c(idvar, groupvar, "gender_label", timevar),
    measure.vars = vars,
    variable.name = "raw_name",
    value.name = "value"
  )
  tmp[, node_wave := paste0(raw_name, "_t", get(timevar))]
  wide <- data.table::dcast(
    tmp,
    formula = as.formula(paste(idvar, "+", groupvar, "+ gender_label ~ node_wave")),
    value.var = "value"
  )
  ordered_cols <- unlist(lapply(waves, function(w) paste0(vars, "_t", w)), use.names = FALSE)
  missing_cols <- setdiff(ordered_cols, names(wide))
  if (length(missing_cols)) {
    wide[, (missing_cols) := NA_real_]
  }
  data.table::setcolorder(wide, c(idvar, groupvar, "gender_label", ordered_cols))
  list(wide = wide, ordered_cols = ordered_cols)
}

# -----------------------------
# Read and prepare long data
# -----------------------------

analysis_start <- Sys.time()

raw_long <- data.table::fread(
  input_path,
  colClasses = setNames("character", idvar),
  na.strings = c("", "NA", "NaN", "NULL")
)

missing_cols <- setdiff(required_cols, names(raw_long))
if (length(missing_cols)) {
  stop("Required columns missing from input: ", paste(missing_cols, collapse = ", "))
}

raw_long <- raw_long[, ..required_cols]
raw_long[, (idvar) := normalize_id(get(idvar))]
raw_long[, (timevar) := as.integer(get(timevar))]
raw_long[, (groupvar) := as.integer(get(groupvar))]

if (anyNA(raw_long[[idvar]])) {
  stop("Missing or invalid IDs detected after normalization.")
}

wave_check <- sort(unique(raw_long[[timevar]]))
if (!identical(wave_check, waves)) {
  stop("Input waves do not match the expected three-wave structure (1, 2, 3). Found: ",
       paste(wave_check, collapse = ", "))
}

raw_long[, gender_label := gender_to_label(get(groupvar))]
data.table::setcolorder(raw_long, c(idvar, timevar, groupvar, "gender_label", input_item_vars))
data.table::setorderv(raw_long, c(idvar, timevar))

id_counts <- raw_long[, .(
  n_rows = .N,
  n_unique_waves = data.table::uniqueN(get(timevar))
), by = idvar]

bad_ids <- id_counts[n_rows != length(waves) | n_unique_waves != length(waves), get(idvar)]
if (length(bad_ids)) {
  stop(
    "The input long dataset still contains IDs without exactly three rows / three waves. ",
    "Example IDs: ", paste(head(bad_ids, 10), collapse = ", ")
  )
}

analysis_raw_export <- data.table::copy(raw_long)

analysis_long <- analysis_raw_export[, c(idvar, timevar, groupvar, "gender_label", vars), with = FALSE]

pooled_z_info <- build_pooled_z(analysis_long, vars, node_meta)
data_z <- pooled_z_info$data_z
standardization_table <- pooled_z_info$standardization_table
trend_tests <- build_trend_tests(data_z, vars, node_meta)
data_detrended <- build_detrended(data_z, vars)

quality_by_wave <- build_quality_by_wave(analysis_long, vars, node_meta)
quality_pooled <- build_quality_pooled(analysis_long, vars, node_meta)

wide_z_info <- build_wide(data_z, vars)
wide_detrended_info <- build_wide(data_detrended, vars)
wide_z <- wide_z_info$wide
wide_detrended <- wide_detrended_info$wide
ordered_node_cols <- wide_detrended_info$ordered_cols

design <- sapply(waves, function(w) paste0(vars, "_t", w))
colnames(design) <- paste0("T", waves)
rownames(design) <- vars

sample_info <- analysis_long[, .(
  n_rows = .N,
  n_ids = data.table::uniqueN(get(idvar))
), by = c(groupvar, "gender_label")] %>%
  as.data.frame()

wave_group_counts <- analysis_long[, .(
  n_ids = data.table::uniqueN(get(idvar))
), by = c(groupvar, "gender_label", timevar)] %>%
  as.data.frame()

# -----------------------------
# Save outputs
# -----------------------------

prepared <- list(
  config = list(
    input_path = normalizePath(input_path, winslash = "/", mustWork = FALSE),
    output_root = normalizePath(output_root, winslash = "/", mustWork = FALSE),
    output_base = normalizePath(output_base, winslash = "/", mustWork = FALSE),
    idvar = idvar,
    timevar = timevar,
    groupvar = groupvar,
    primary_analysis_dataset = primary_analysis_dataset,
    created_at = format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  ),
  vars = vars,
  waves = waves,
  required_cols = required_cols,
  input_item_vars = input_item_vars,
  ordered_node_cols = ordered_node_cols,
  design = design,
  node_meta = node_meta,
  community_palette = community_palette,
  data_raw_original = raw_long,
  data_raw = analysis_long,
  data_z = data_z,
  data_detrended = data_detrended,
  wide_z = wide_z,
  wide_detrended = wide_detrended,
  node_dictionary = node_dictionary,
  sample_info = sample_info,
  wave_group_counts = wave_group_counts,
  quality_by_wave = quality_by_wave,
  quality_pooled = quality_pooled,
  standardization_table = standardization_table,
  trend_tests = trend_tests
)

prepared_rds <- file.path(dir_data, "prepared_panel_data.rds")
saveRDS(prepared, prepared_rds)

write_csv(
  analysis_raw_export,
  file.path(dir_data, "analysis_dataset_raw_long.csv")
)
write_csv(
  data_z[, c(idvar, timevar, groupvar, "gender_label", vars), with = FALSE],
  file.path(dir_data, "analysis_dataset_pooled_z_long.csv")
)
write_csv(
  data_detrended[, c(idvar, timevar, groupvar, "gender_label", vars), with = FALSE],
  file.path(dir_data, "analysis_dataset_detrended_long.csv")
)
write_csv(
  wide_z,
  file.path(dir_data, "analysis_dataset_pooled_z_wide.csv")
)
write_csv(
  wide_detrended,
  file.path(dir_data, "analysis_dataset_detrended_wide.csv")
)
write_csv(
  data.frame(node = rownames(design), design, check.names = FALSE),
  file.path(dir_data, "panel_design_matrix.csv")
)

write_csv(node_dictionary, file.path(dir_tables, "00_node_dictionary.csv"))
write_csv(sample_info, file.path(dir_tables, "01_sample_info_by_group.csv"))
write_csv(wave_group_counts, file.path(dir_tables, "02_sample_info_by_group_and_wave.csv"))
write_csv(quality_by_wave, file.path(dir_tables, "03_variable_quality_by_wave.csv"))
write_csv(quality_pooled, file.path(dir_tables, "04_variable_quality_pooled.csv"))
write_csv(standardization_table, file.path(dir_tables, "05_pooled_z_standardization_parameters.csv"))
write_csv(trend_tests, file.path(dir_tables, "06_linear_wave_trend_tests_on_pooled_z.csv"))

wb <- openxlsx::createWorkbook()
add_sheet(wb, "NodeDictionary", node_dictionary)
add_sheet(wb, "SampleByGroup", sample_info)
add_sheet(wb, "SampleByWave", wave_group_counts)
add_sheet(wb, "QualityByWave", quality_by_wave)
add_sheet(wb, "QualityPooled", quality_pooled)
add_sheet(wb, "ZStandardization", standardization_table)
add_sheet(wb, "TrendTests", trend_tests)
add_sheet(wb, "DesignMatrix", data.frame(node = rownames(design), design, check.names = FALSE))
openxlsx::saveWorkbook(
  wb,
  file.path(output_root, "00_preparation_summary.xlsx"),
  overwrite = TRUE
)

latest_pointer <- file.path(output_base, "latest_prepared_data_path.txt")
writeLines(normalizePath(prepared_rds, winslash = "/", mustWork = FALSE), latest_pointer)

analysis_end <- Sys.time()

notes <- c(
  "Panel data preparation summary",
  "",
  paste0("Input file: ", normalizePath(input_path, winslash = "/", mustWork = FALSE)),
  paste0("Output root: ", normalizePath(output_root, winslash = "/", mustWork = FALSE)),
  paste0("Prepared RDS: ", normalizePath(prepared_rds, winslash = "/", mustWork = FALSE)),
  paste0("Latest pointer: ", normalizePath(latest_pointer, winslash = "/", mustWork = FALSE)),
  paste0("Primary analysis dataset: ", primary_analysis_dataset),
  paste0("Started: ", format(analysis_start, "%Y-%m-%d %H:%M:%S")),
  paste0("Ended: ", format(analysis_end, "%Y-%m-%d %H:%M:%S")),
  "",
  "12-node analysis definition:",
  "- pe1-pe8 are retained as individual psychotic-experience nodes.",
  "- si1-si4 are retained as individual self-harm/suicidality nodes.",
  "",
  "Important interpretation note:",
  "- pooled_z = each analysis node z-scored across all participants and all waves.",
  "- detrended = pooled_z residualized on linear wave trend; this is the default primary dataset.",
  "- Downstream Panel GVAR estimation should use the 12 analysis nodes in prepared$vars.",
  "- This script prepares the enlarged loose source+phone recovery sample and does not run IVPP.",
  "- This prepared dataset is intended for the 12-node between-simplified Panel GVAR workflow, where between-person stable differences are controlled but not interpreted as a separate GGM network."
)
writeLines(notes, file.path(dir_notes, "00_preparation_notes.txt"))

cat("Panel data preparation completed.\n")
cat("Output root:", normalizePath(output_root, winslash = "/", mustWork = FALSE), "\n")
cat("Prepared RDS:", normalizePath(prepared_rds, winslash = "/", mustWork = FALSE), "\n")
