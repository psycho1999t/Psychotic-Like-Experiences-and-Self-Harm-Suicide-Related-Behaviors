options(stringsAsFactors = FALSE)

suppressPackageStartupMessages({
  library(data.table)
  library(dplyr)
  library(qgraph)
  library(IVPP)
})

set.seed(1234)

# =============================================================================
# 02_ivpp_gender_comparison_author_style_12nodes.R
#
# Purpose:
#   Author-style 12-node IVPP analysis following the public empirical demo:
#   1) use long-format raw data,
#   2) standardize inside IVPP with standardize = "z",
#   3) run global invariance tests for both/temporal/contemporaneous networks,
#   4) run partial pruning,
#   5) plot Male, Female, and Difference networks for temporal and
#      contemporaneous networks.
#
# This script deliberately bypasses the 01 baseline/pruned/stepup final-model
# selection system. It directly uses IVPP::IVPP_panelgvar().
# =============================================================================

# -----------------------------
# Configuration
# -----------------------------

prepared_rds <- Sys.getenv("PREPARED_DATA_RDS", unset = "")
if (prepared_rds == "") {
  stop("Set PREPARED_DATA_RDS to the prepared RDS created by the data-preparation step.")
}
if (!file.exists(prepared_rds)) {
  stop("Prepared RDS does not exist: ", prepared_rds)
}

prepared <- readRDS(prepared_rds)

vars <- prepared$vars
node_meta <- prepared$node_meta
idvar <- prepared$config$idvar
timevar <- prepared$config$timevar
groupvar <- prepared$config$groupvar

expected_vars <- c(paste0("pe", 1:8), paste0("si", 1:4))
if (!identical(vars, expected_vars)) {
  stop("This author-style script is locked to pe1-pe8 + si1-si4. Found: ",
       paste(vars, collapse = ", "))
}

if (!all(c("raw_name", "node_label", "short_label", "community", "color") %in% names(node_meta))) {
  stop("node_meta must contain raw_name, node_label, short_label, community, and color.")
}

ivpp_standardize <- "z"
ivpp_net_type <- match.arg(Sys.getenv("IVPP_NET_TYPE", unset = "sparse"), c("sparse", "saturated"))
ivpp_prune_alpha <- as.numeric(Sys.getenv("IVPP_PRUNE_ALPHA", unset = "0.01"))
ivpp_p_prune_alpha <- as.numeric(Sys.getenv("IVPP_P_PRUNE_ALPHA", unset = "0.01"))
ivpp_ncores <- as.integer(Sys.getenv("IVPP_NCORES", unset = "1"))
difference_tolerance <- as.numeric(Sys.getenv("IVPP_DIFFERENCE_TOLERANCE", unset = "1e-10"))

timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
output_tag <- Sys.getenv("IVPP_OUTPUT_TAG", unset = timestamp)
output_base <- Sys.getenv("IVPP_OUTPUT_BASE", unset = file.path("final results", "ivpp_analysis"))
output_root <- file.path(output_base, paste0("ivpp12_author_official_raw_std-z_", output_tag))

dir_tables <- file.path(output_root, "tables")
dir_figures <- file.path(output_root, "figures")
dir_models <- file.path(output_root, "models")
dir_notes <- file.path(output_root, "notes")

for (dir_path in c(output_root, dir_tables, dir_figures, dir_models, dir_notes)) {
  dir.create(dir_path, recursive = TRUE, showWarnings = FALSE)
}

data_ivpp <- data.table::copy(prepared$data_raw)
data_ivpp[, (idvar) := as.character(get(idvar))]
data_ivpp[, (timevar) := as.numeric(get(timevar))]
data_ivpp[, (groupvar) := as.integer(get(groupvar))]
for (v in vars) {
  data_ivpp[, (v) := suppressWarnings(as.numeric(get(v)))]
}

group_values <- sort(unique(data_ivpp[[groupvar]]))
if (!identical(group_values, c(1L, 2L))) {
  stop("This script requires gender groups coded 1 = Male and 2 = Female. Found: ",
       paste(group_values, collapse = ", "))
}

group_label <- function(x) {
  ifelse(as.integer(x) == 1L, "Male", ifelse(as.integer(x) == 2L, "Female", paste0("Group_", x)))
}

# -----------------------------
# Utility functions
# -----------------------------

write_csv <- function(x, path) {
  x <- as.data.frame(x, stringsAsFactors = FALSE)
  if (!ncol(x)) x <- data.frame(note = "No exportable columns.")
  data.table::fwrite(x, file = path, na = "")
}

write_note_lines <- function(path, lines) {
  lines <- as.character(lines)
  lines[is.na(lines)] <- ""
  writeLines(lines, con = path, useBytes = TRUE)
}

run_with_capture <- function(expr) {
  warnings <- character()
  value <- tryCatch(
    withCallingHandlers(
      expr,
      warning = function(w) {
        warnings <<- c(warnings, conditionMessage(w))
        invokeRestart("muffleWarning")
      }
    ),
    error = function(e) {
      structure(list(error = conditionMessage(e)), class = "captured_error")
    }
  )
  list(value = value, warnings = unique(warnings))
}

is_captured_error <- function(x) inherits(x, "captured_error")

parse_numeric_string <- function(x) {
  if (is.numeric(x)) return(x)
  x <- trimws(as.character(x))
  x[x %in% c("", "NA", "NaN", "Inf", "-Inf", "NULL")] <- NA_character_
  x <- gsub(",", "", x, fixed = TRUE)
  x[grepl("^<\\s*", x)] <- sub("^<\\s*", "", x[grepl("^<\\s*", x)])
  x[grepl("^\\.", x)] <- paste0("0", x[grepl("^\\.", x)])
  suppressWarnings(as.numeric(x))
}

normalize_numeric_columns <- function(df) {
  df <- as.data.frame(df, stringsAsFactors = FALSE)
  numeric_candidates <- c(
    "nvar", "nobs", "npar", "df", "chisq", "pvalue", "nfi", "tli", "cfi",
    "rmsea", "rmsea.ci.lower", "rmsea.ci.upper", "rmsea.pvalue",
    "AIC", "delta_AIC", "BIC", "delta_BIC", "chi_sq", "delta_chisq",
    "df_diff", "p_value", "sf"
  )
  for (nm in intersect(names(df), numeric_candidates)) {
    df[[nm]] <- parse_numeric_string(df[[nm]])
  }
  df
}

numeric_flag_rows <- function(df) {
  df <- normalize_numeric_columns(df)
  flag <- rep(FALSE, nrow(df))
  for (nm in intersect(names(df), c("chisq", "chi_sq"))) {
    x <- df[[nm]]
    flag <- flag | (!is.na(x) & (!is.finite(x) | x < -1e-3 | abs(x) > 1e8))
  }
  for (nm in intersect(names(df), c("cfi", "tli", "nfi"))) {
    x <- df[[nm]]
    flag <- flag | (!is.na(x) & (!is.finite(x) | x < -10 | x > 10))
  }
  for (nm in intersect(names(df), "rmsea")) {
    x <- df[[nm]]
    flag <- flag | (!is.na(x) & (!is.finite(x) | x < 0 | x > 10))
  }
  for (nm in intersect(names(df), c("AIC", "BIC", "delta_AIC", "delta_BIC", "delta_chisq"))) {
    x <- df[[nm]]
    flag <- flag | (!is.na(x) & (!is.finite(x) | abs(x) > 1e8))
  }
  flag
}

tag_fit_table <- function(tab, table_type) {
  if (is.null(tab)) return(data.frame(table_type = table_type, note = "Fit table not available."))
  out <- as.data.frame(tab, stringsAsFactors = FALSE)
  out$table_type <- table_type
  out <- out[, c("table_type", setdiff(names(out), "table_type")), drop = FALSE]
  out <- normalize_numeric_columns(out)
  out$numerical_flag <- numeric_flag_rows(out)
  out
}

tag_omnibus_table <- function(tab, network_type, table_type) {
  if (is.null(tab)) {
    return(data.frame(network_type = network_type, table_type = table_type, note = "Table not available."))
  }
  out <- as.data.frame(tab, stringsAsFactors = FALSE)
  out$model <- rownames(tab)
  rownames(out) <- NULL
  out$network_type <- network_type
  out$table_type <- table_type
  out <- out[, c("network_type", "table_type", "model", setdiff(names(out), c("network_type", "table_type", "model"))), drop = FALSE]
  out <- normalize_numeric_columns(out)
  out$numerical_flag <- numeric_flag_rows(out)
  out
}

tag_sf_table <- function(sf_obj) {
  if (is.null(sf_obj)) return(data.frame(network_type = NA_character_, note = "sF table not available."))
  if (!is.list(sf_obj)) sf_obj <- list(overall = sf_obj)
  out <- dplyr::bind_rows(lapply(names(sf_obj), function(nm) {
    data.frame(network_type = nm, as.data.frame(sf_obj[[nm]], stringsAsFactors = FALSE), check.names = FALSE)
  }))
  out <- normalize_numeric_columns(out)
  out$numerical_flag <- numeric_flag_rows(out)
  out
}

extract_group_matrix <- function(mat_obj, group_value, group_index, node_names) {
  if (is.list(mat_obj)) {
    if (!is.null(names(mat_obj)) && as.character(group_value) %in% names(mat_obj)) {
      mat <- as.matrix(mat_obj[[as.character(group_value)]])
    } else {
      mat <- as.matrix(mat_obj[[group_index]])
    }
  } else if (length(dim(mat_obj)) == 3) {
    mat <- as.matrix(mat_obj[, , group_index])
  } else {
    stop("Could not extract group matrix.")
  }
  rownames(mat) <- node_names
  colnames(mat) <- node_names
  mat
}

matrix_to_df <- function(mat) {
  data.frame(node = rownames(mat), mat, check.names = FALSE)
}

directed_edge_table <- function(mat_male, mat_female, node_meta, tolerance) {
  diff_mat <- mat_female - mat_male
  idx <- which(abs(diff_mat) > tolerance, arr.ind = TRUE)
  if (!nrow(idx)) return(data.frame())
  data.frame(
    source_raw = rownames(diff_mat)[idx[, 1]],
    target_raw = colnames(diff_mat)[idx[, 2]],
    male_weight = mat_male[idx],
    female_weight = mat_female[idx],
    diff_female_minus_male = diff_mat[idx],
    edge_kind = ifelse(idx[, 1] == idx[, 2], "autoregressive", "crosslag"),
    result_basis = "nonzero_difference_after_ivpp_partial_pruning_not_edgewise_p_value",
    stringsAsFactors = FALSE
  ) %>%
    mutate(abs_diff = abs(diff_female_minus_male)) %>%
    left_join(node_meta %>% select(raw_name, source_short_label = short_label, source_node_label = node_label, source_community = community),
              by = c("source_raw" = "raw_name")) %>%
    left_join(node_meta %>% select(raw_name, target_short_label = short_label, target_node_label = node_label, target_community = community),
              by = c("target_raw" = "raw_name")) %>%
    arrange(desc(abs_diff))
}

undirected_edge_table <- function(mat_male, mat_female, node_meta, tolerance) {
  diff_mat <- mat_female - mat_male
  idx <- which(upper.tri(diff_mat) & abs(diff_mat) > tolerance, arr.ind = TRUE)
  if (!nrow(idx)) return(data.frame())
  data.frame(
    node1_raw = rownames(diff_mat)[idx[, 1]],
    node2_raw = colnames(diff_mat)[idx[, 2]],
    male_weight = mat_male[idx],
    female_weight = mat_female[idx],
    diff_female_minus_male = diff_mat[idx],
    result_basis = "nonzero_difference_after_ivpp_partial_pruning_not_edgewise_p_value",
    stringsAsFactors = FALSE
  ) %>%
    mutate(abs_diff = abs(diff_female_minus_male)) %>%
    left_join(node_meta %>% select(raw_name, node1_short_label = short_label, node1_node_label = node_label, node1_community = community),
              by = c("node1_raw" = "raw_name")) %>%
    left_join(node_meta %>% select(raw_name, node2_short_label = short_label, node2_node_label = node_label, node2_community = community),
              by = c("node2_raw" = "raw_name")) %>%
    arrange(desc(abs_diff))
}

plot_three_panel <- function(mats, titles, file_base, directed) {
  max_abs <- max(abs(unlist(mats)), na.rm = TRUE)
  if (!is.finite(max_abs) || max_abs <= 0) max_abs <- 1
  layout_ref <- Reduce(`+`, lapply(mats, function(m) {
    x <- abs(as.matrix(m))
    diag(x) <- 0
    x[!is.finite(x)] <- 0
    x
  }))
  layout_coords <- tryCatch({
    qgraph::qgraph(
      layout_ref,
      layout = "spring",
      labels = node_meta$short_label,
      DoNotPlot = TRUE
    )$layout
  }, error = function(e) {
    qgraph::averageLayout(layout_ref)
  })
  rownames(layout_coords) <- node_meta$raw_name

  render <- function() {
    par(bg = "white")
    layout(matrix(1:3, nrow = 1))
    for (i in seq_along(mats)) {
      qgraph::qgraph(
        mats[[i]],
        layout = layout_coords,
        directed = directed,
        diag = directed,
        labels = node_meta$short_label,
        nodeNames = paste0(node_meta$short_label, ": ", node_meta$node_label),
        color = node_meta$color,
        border.color = "gray25",
        vsize = 6,
        label.cex = 1.0,
        asize = if (directed) 4 else 0,
        esize = if (directed) 12 else 0,
        maximum = max_abs,
        cut = 0.01,
        minimum = 0.01,
        edge.labels = FALSE,
        posCol = "#2B83BA",
        negCol = "#D7191C",
        fade = TRUE,
        mar = c(4.2, 4.2, 4.8, 4.2),
        title = titles[i],
        title.cex = 0.95,
        legend = FALSE,
        theme = "colorblind"
      )
      if (i == 1) {
        legend(
          "bottomleft",
          legend = c("Psychotic Experiences", "Self-Harm/Suicidality"),
          pt.bg = c("#2F6C8F", "#C65A5A"),
          pch = 21,
          pt.cex = 1.25,
          bty = "n",
          cex = 0.72
        )
      }
      box("figure", col = "gray85")
    }
  }

  grDevices::pdf(paste0(file_base, ".pdf"), width = 16.5, height = 5.6, compress = FALSE)
  render()
  grDevices::dev.off()

  grDevices::png(paste0(file_base, ".png"), width = 16.5, height = 5.6, units = "in", res = 360, bg = "white")
  render()
  grDevices::dev.off()
}

run_ivpp_or_stop <- function(label, expr) {
  message("Running author-style IVPP step: ", label)
  cap <- run_with_capture(expr)
  write_note_lines(file.path(dir_notes, paste0("warnings_", label, ".txt")), cap$warnings)
  if (is_captured_error(cap$value)) {
    write_note_lines(
      file.path(dir_notes, "author_style_ivpp_not_run_reason.txt"),
      c(
        paste0("Author-style IVPP step failed: ", label),
        paste0("Error: ", cap$value$error),
        "Warnings:",
        cap$warnings
      )
    )
    stop("Author-style IVPP failed at step: ", label)
  }
  cap$value
}

# -----------------------------
# Input checks
# -----------------------------

input_check <- data.frame(
  check = c(
    "prepared_rds_exists",
    "input_source_is_raw_long",
    "standardize_is_z",
    "vars_are_12_nodes",
    "id_missing",
    "wave_missing",
    "group_missing",
    "node_missing",
    "n_subjects_full",
    "n_subjects_male",
    "n_subjects_female",
    "n_rows",
    "n_waves",
    "uses_official_IVPP_panelgvar",
    "bypasses_01_final_model_selection",
    "between_network_exported"
  ),
  value = c(
    file.exists(prepared_rds),
    TRUE,
    ivpp_standardize,
    paste(vars, collapse = ","),
    sum(is.na(data_ivpp[[idvar]])),
    sum(is.na(data_ivpp[[timevar]])),
    sum(is.na(data_ivpp[[groupvar]])),
    sum(is.na(data_ivpp[, ..vars])),
    uniqueN(data_ivpp[[idvar]]),
    uniqueN(data_ivpp[get(groupvar) == 1L, get(idvar)]),
    uniqueN(data_ivpp[get(groupvar) == 2L, get(idvar)]),
    nrow(data_ivpp),
    paste(sort(unique(data_ivpp[[timevar]])), collapse = ","),
    TRUE,
    TRUE,
    FALSE
  ),
  pass = c(
    file.exists(prepared_rds),
    TRUE,
    ivpp_standardize == "z",
    identical(vars, expected_vars),
    sum(is.na(data_ivpp[[idvar]])) == 0,
    sum(is.na(data_ivpp[[timevar]])) == 0,
    sum(is.na(data_ivpp[[groupvar]])) == 0,
    sum(is.na(data_ivpp[, ..vars])) == 0,
    uniqueN(data_ivpp[[idvar]]) == 1265,
    uniqueN(data_ivpp[get(groupvar) == 1L, get(idvar)]) == 419,
    uniqueN(data_ivpp[get(groupvar) == 2L, get(idvar)]) == 846,
    nrow(data_ivpp) == 3795,
    identical(sort(unique(data_ivpp[[timevar]])), c(1, 2, 3)),
    TRUE,
    TRUE,
    TRUE
  ),
  stringsAsFactors = FALSE
)

write_csv(input_check, file.path(dir_tables, "00_author_ivpp_input_checks.csv"))

if (!all(input_check$pass)) {
  write_note_lines(
    file.path(dir_notes, "author_style_ivpp_not_run_reason.txt"),
    c("Input checks failed.", capture.output(print(input_check)))
  )
  stop("Input checks failed. See 00_author_ivpp_input_checks.csv")
}

analysis_start <- Sys.time()

# -----------------------------
# Author-style IVPP runs
# -----------------------------

ivpp_global_both <- run_ivpp_or_stop("global_both", IVPP::IVPP_panelgvar(
  as.data.frame(data_ivpp),
  vars = vars,
  idvar = idvar,
  beepvar = timevar,
  groups = groupvar,
  g_test_net = "both",
  partial_prune = FALSE,
  net_type = ivpp_net_type,
  prune_alpha = ivpp_prune_alpha,
  standardize = ivpp_standardize,
  ncores = ivpp_ncores
))
saveRDS(ivpp_global_both, file.path(dir_models, "author_ivpp_global_both.rds"))

ivpp_global_temporal <- run_ivpp_or_stop("global_temporal", IVPP::IVPP_panelgvar(
  as.data.frame(data_ivpp),
  vars = vars,
  idvar = idvar,
  beepvar = timevar,
  groups = groupvar,
  g_test_net = "temporal",
  partial_prune = FALSE,
  net_type = ivpp_net_type,
  prune_alpha = ivpp_prune_alpha,
  standardize = ivpp_standardize,
  ncores = ivpp_ncores
))
saveRDS(ivpp_global_temporal, file.path(dir_models, "author_ivpp_global_temporal.rds"))

ivpp_global_contemporaneous <- run_ivpp_or_stop("global_contemporaneous", IVPP::IVPP_panelgvar(
  as.data.frame(data_ivpp),
  vars = vars,
  idvar = idvar,
  beepvar = timevar,
  groups = groupvar,
  g_test_net = "contemporaneous",
  partial_prune = FALSE,
  net_type = ivpp_net_type,
  prune_alpha = ivpp_prune_alpha,
  standardize = ivpp_standardize,
  ncores = ivpp_ncores
))
saveRDS(ivpp_global_contemporaneous, file.path(dir_models, "author_ivpp_global_contemporaneous.rds"))

ivpp_partial_prune <- run_ivpp_or_stop("partial_prune_both", IVPP::IVPP_panelgvar(
  as.data.frame(data_ivpp),
  vars = vars,
  idvar = idvar,
  beepvar = timevar,
  groups = groupvar,
  global = FALSE,
  partial_prune = TRUE,
  prune_net = "both",
  p_prune_alpha = ivpp_p_prune_alpha,
  standardize = ivpp_standardize,
  ncores = ivpp_ncores
))
saveRDS(ivpp_partial_prune, file.path(dir_models, "author_ivpp_partial_prune_both.rds"))

if (is.null(ivpp_partial_prune$partial_prune)) {
  stop("Partial prune object does not contain $partial_prune.")
}

# -----------------------------
# Tables
# -----------------------------

global_fit <- bind_rows(
  tag_fit_table(ivpp_global_both$fit$free, "both_free"),
  tag_fit_table(ivpp_global_both$fit$bothEq, "both_bothEq"),
  tag_fit_table(ivpp_global_temporal$fit$free, "temporal_free"),
  tag_fit_table(ivpp_global_temporal$fit$bothEq, "temporal_bothEq"),
  tag_fit_table(ivpp_global_temporal$fit$tempEq, "temporal_tempEq"),
  tag_fit_table(ivpp_global_temporal$fit$contEq, "temporal_contEq"),
  tag_fit_table(ivpp_global_contemporaneous$fit$free, "contemporaneous_free"),
  tag_fit_table(ivpp_global_contemporaneous$fit$bothEq, "contemporaneous_bothEq"),
  tag_fit_table(ivpp_global_contemporaneous$fit$tempEq, "contemporaneous_tempEq"),
  tag_fit_table(ivpp_global_contemporaneous$fit$contEq, "contemporaneous_contEq")
)
write_csv(global_fit, file.path(dir_tables, "10_author_ivpp_global_fit_indices.csv"))

global_omnibus <- bind_rows(
  tag_omnibus_table(ivpp_global_both$omnibus$vs_free, "both", "vs_free"),
  tag_omnibus_table(ivpp_global_both$omnibus$vs_bothEq, "both", "vs_bothEq"),
  tag_omnibus_table(ivpp_global_temporal$omnibus$vs_free, "temporal", "vs_free"),
  tag_omnibus_table(ivpp_global_temporal$omnibus$vs_bothEq, "temporal", "vs_bothEq"),
  tag_omnibus_table(ivpp_global_contemporaneous$omnibus$vs_free, "contemporaneous", "vs_free"),
  tag_omnibus_table(ivpp_global_contemporaneous$omnibus$vs_bothEq, "contemporaneous", "vs_bothEq")
)
global_omnibus$official_vs_bothEq_bug_caveat <- global_omnibus$table_type == "vs_bothEq"
write_csv(global_omnibus, file.path(dir_tables, "11_author_ivpp_global_omnibus_tests.csv"))

partial_prune_fit <- tag_fit_table(ivpp_partial_prune$fit$partial_prune, "partial_prune")
similarity_f <- tag_sf_table(ivpp_partial_prune$sF)
write_csv(partial_prune_fit, file.path(dir_tables, "20_author_ivpp_partial_prune_fit_indices.csv"))
write_csv(similarity_f, file.path(dir_tables, "21_author_ivpp_similarity_f_indices.csv"))

male_beta_raw <- extract_group_matrix(ivpp_partial_prune$partial_prune$beta, 1, 1, vars)
female_beta_raw <- extract_group_matrix(ivpp_partial_prune$partial_prune$beta, 2, 2, vars)

# Follow the author's plotting convention for temporal networks.
male_temporal_beta <- t(male_beta_raw)
female_temporal_beta <- t(female_beta_raw)
diff_temporal_beta <- female_temporal_beta - male_temporal_beta

male_contemporaneous <- extract_group_matrix(ivpp_partial_prune$partial_prune$contemporaneous, 1, 1, vars)
female_contemporaneous <- extract_group_matrix(ivpp_partial_prune$partial_prune$contemporaneous, 2, 2, vars)
diff_contemporaneous <- female_contemporaneous - male_contemporaneous

write_csv(matrix_to_df(male_temporal_beta), file.path(dir_tables, "30_author_male_temporal_beta_matrix.csv"))
write_csv(matrix_to_df(female_temporal_beta), file.path(dir_tables, "31_author_female_temporal_beta_matrix.csv"))
write_csv(matrix_to_df(diff_temporal_beta), file.path(dir_tables, "32_author_temporal_beta_difference_matrix_female_minus_male.csv"))
write_csv(matrix_to_df(male_contemporaneous), file.path(dir_tables, "33_author_male_contemporaneous_matrix.csv"))
write_csv(matrix_to_df(female_contemporaneous), file.path(dir_tables, "34_author_female_contemporaneous_matrix.csv"))
write_csv(matrix_to_df(diff_contemporaneous), file.path(dir_tables, "35_author_contemporaneous_difference_matrix_female_minus_male.csv"))

temporal_edges <- directed_edge_table(male_temporal_beta, female_temporal_beta, node_meta, difference_tolerance)
contemporaneous_edges <- undirected_edge_table(male_contemporaneous, female_contemporaneous, node_meta, difference_tolerance)
write_csv(temporal_edges, file.path(dir_tables, "40_author_temporal_difference_edges.csv"))
write_csv(contemporaneous_edges, file.path(dir_tables, "41_author_contemporaneous_difference_edges.csv"))

# -----------------------------
# Figures
# -----------------------------

plot_three_panel(
  list(male_temporal_beta, female_temporal_beta, diff_temporal_beta),
  c("Male Temporal", "Female Temporal", "Difference (Female - Male)"),
  file.path(dir_figures, "author_ivpp_temporal_beta"),
  directed = TRUE
)

plot_three_panel(
  list(male_contemporaneous, female_contemporaneous, diff_contemporaneous),
  c("Male Contemporaneous", "Female Contemporaneous", "Difference (Female - Male)"),
  file.path(dir_figures, "author_ivpp_contemporaneous"),
  directed = FALSE
)

# -----------------------------
# Bundle and notes
# -----------------------------

results_bundle <- list(
  prepared_rds = normalizePath(prepared_rds, winslash = "/", mustWork = FALSE),
  output_root = normalizePath(output_root, winslash = "/", mustWork = FALSE),
  method = "author_style_official_IVPP_panelgvar_raw_standardize_z",
  between_handling = "official_IVPP_internal_default_ml_gvar_between_chol",
  vars = vars,
  node_meta = node_meta,
  input_check = input_check,
  global_both = ivpp_global_both,
  global_temporal = ivpp_global_temporal,
  global_contemporaneous = ivpp_global_contemporaneous,
  partial_prune = ivpp_partial_prune,
  tables = list(
    global_fit = global_fit,
    global_omnibus = global_omnibus,
    partial_prune_fit = partial_prune_fit,
    similarity_f = similarity_f,
    temporal_edges = temporal_edges,
    contemporaneous_edges = contemporaneous_edges
  ),
  matrices = list(
    male_temporal_beta = male_temporal_beta,
    female_temporal_beta = female_temporal_beta,
    diff_temporal_beta = diff_temporal_beta,
    male_contemporaneous = male_contemporaneous,
    female_contemporaneous = female_contemporaneous,
    diff_contemporaneous = diff_contemporaneous
  )
)
saveRDS(results_bundle, file.path(dir_models, "author_ivpp_results_bundle.rds"))

analysis_end <- Sys.time()

any_numeric_flag <- any(global_fit$numerical_flag, global_omnibus$numerical_flag,
                        partial_prune_fit$numerical_flag, similarity_f$numerical_flag, na.rm = TRUE)
status_text <- if (any_numeric_flag) {
  "作者口径 IVPP 已完成导出，但存在 numerical_flag，需要谨慎审查。"
} else {
  "作者口径 IVPP 已完成导出，核心拟合与比较表未发现灾难性数值异常。"
}

notes_cn <- c(
  "作者口径 12 节点 IVPP 主分析说明",
  "",
  "本轮目的：",
  "直接按照 IVPP 作者公开示例代码的思路进行分析，不再使用 01 脚本中的 baseline / pruned / stepup final model selection。",
  "",
  "本轮核心设置：",
  paste0("- 输入 prepared RDS: ", normalizePath(prepared_rds, winslash = "/", mustWork = FALSE)),
  "- 输入数据: prepared$data_raw，即 long-format 原始题项数据。",
  "- 标准化: standardize = 'z'，由 IVPP::IVPP_panelgvar() 内部完成。",
  "- 主函数: IVPP::IVPP_panelgvar()。",
  "- global tests: both, temporal, contemporaneous。",
  "- partial pruning: global = FALSE, partial_prune = TRUE, prune_net = 'both'。",
  "- between 处理: 使用官方函数内部默认设置，即 ml_gvar(..., between = 'chol')。",
  "- 输出解释范围: 只解释 temporal network 和 contemporaneous network，不输出、不解释 between network。",
  "",
  "与旧 01 流程的区别：",
  "- 本轮没有为 full / male / female 分别选择 baseline、pruned、stepup。",
  "- 本轮没有读取 10c_standard_panelgvar_final_model_selection.csv。",
  "- 本轮男女网络和差异网络来自 IVPP partial pruning 框架，而不是来自 01 的 final network。",
  "",
  "当前运行状态：",
  paste0("- ", status_text),
  paste0("- temporal difference edges: ", nrow(temporal_edges)),
  paste0("- contemporaneous difference edges: ", nrow(contemporaneous_edges)),
  "",
  "重要解释边界：",
  "- 差异边表示 IVPP partial pruning 后保留的组间差异边，不是普通逐边 p 值检验。",
  "- 本轮忠实作者口径，使用官方 between = 'chol' 路径；如果需要和项目稳定版 between = 'cov' 比较，应另作敏感性分析。",
  "- 本轮结果应优先用于复现作者方法逻辑和判断 12 节点官方 IVPP 是否可作为主分析。",
  "",
  paste0("Started: ", format(analysis_start, "%Y-%m-%d %H:%M:%S")),
  paste0("Ended: ", format(analysis_end, "%Y-%m-%d %H:%M:%S"))
)
write_note_lines(file.path(dir_notes, "author_style_ivpp_notes_cn.txt"), notes_cn)

gate_summary <- c(
  "# 作者口径 12 节点 IVPP gate summary",
  "",
  "## 结论",
  "",
  status_text,
  "",
  "本轮分析绕开了 01 的 baseline / pruned / stepup final selection，直接按照作者公开示例代码使用官方 IVPP_panelgvar() 完成 global tests 与 partial pruning。",
  "",
  "## 核心输出",
  "",
  paste0("- 输出目录: `", normalizePath(output_root, winslash = "/", mustWork = FALSE), "`"),
  "- Global fit: `tables/10_author_ivpp_global_fit_indices.csv`",
  "- Global omnibus: `tables/11_author_ivpp_global_omnibus_tests.csv`",
  "- Partial pruning fit: `tables/20_author_ivpp_partial_prune_fit_indices.csv`",
  "- Similarity sF: `tables/21_author_ivpp_similarity_f_indices.csv`",
  paste0("- Temporal difference edges: ", nrow(temporal_edges)),
  paste0("- Contemporaneous difference edges: ", nrow(contemporaneous_edges)),
  "- Figures: `figures/author_ivpp_temporal_beta.*` and `figures/author_ivpp_contemporaneous.*`",
  "",
  "## 方法边界",
  "",
  "- 本轮为官方作者口径，使用 `prepared$data_raw + standardize = 'z'`。",
  "- 本轮使用官方 IVPP 内部默认 `between = 'chol'`，不是项目稳定版 `between = 'cov'`。",
  "- 本轮不解释 between network。",
  "- `vs_bothEq` 的 chi-square 相关列在官方 IVPP 1.1.2 中存在已知实现问题，导出表已标记 caveat；判断时应优先结合 fit、AIC/BIC 和主要比较逻辑。"
)
write_note_lines(file.path(dir_notes, "author_style_ivpp_gate_summary_cn.md"), gate_summary)

write_csv(
  data.frame(
    package = c("IVPP", "psychonetrics", "qgraph", "dplyr", "data.table"),
    version = c(
      as.character(packageVersion("IVPP")),
      as.character(packageVersion("psychonetrics")),
      as.character(packageVersion("qgraph")),
      as.character(packageVersion("dplyr")),
      as.character(packageVersion("data.table"))
    )
  ),
  file.path(dir_notes, "package_versions.csv")
)

cat("Author-style 12-node IVPP completed.\n")
cat("Output root:", normalizePath(output_root, winslash = "/", mustWork = FALSE), "\n")
