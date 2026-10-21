options(stringsAsFactors = FALSE)

suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(patchwork)
  library(grid)
  library(scales)
  library(svglite)
  library(ragg)
  library(qgraph)
})


# =============================================================================
# 09_make_polished_split_network_figures_12nodes.R
#
# Purpose:
#   Rebuild the IVPP figure set with a visual language matched to the
#   manuscript logic diagram and the supplied network reference.
#   This script reads existing author-style IVPP outputs only. It does not rerun
#   models, change edge selection, or change the girls-minus-boys direction.
#
# Output:
#   A new split main/supplementary figure package. Existing figures and scripts
#   are never overwritten.
# =============================================================================

get_script_dir <- function() {
  args <- commandArgs(trailingOnly = FALSE)
  file_arg <- grep("^--file=", args, value = TRUE)
  if (length(file_arg)) {
    candidate <- tryCatch(
      normalizePath(sub("^--file=", "", file_arg[1]), winslash = "/", mustWork = TRUE),
      error = function(e) NA_character_
    )
    if (!is.na(candidate)) {
      candidate_dir <- tryCatch(dirname(candidate), error = function(e) NA_character_)
      if (!is.na(candidate_dir)) return(candidate_dir)
    }
  }
  this_file <- tryCatch(normalizePath(sys.frames()[[1]]$ofile, winslash = "/", mustWork = TRUE), error = function(e) NA_character_)
  if (!is.na(this_file) && nzchar(this_file)) {
    return(dirname(this_file))
  }
  normalizePath(getwd(), winslash = "/", mustWork = TRUE)
}

script_dir <- get_script_dir()
# The archive intentionally excludes raw/prepared data. Read the verified
# result tables from the complete project root and write only to the new archive.
project_root <- Sys.getenv("IVPP_RUNTIME_DIR", unset = getwd())
archive_root <- file.path(project_root, "output")
staged_inputs_dir <- file.path(project_root, "inputs")
tables_dir <- file.path(staged_inputs_dir, "results")

output_root <- file.path(archive_root, "split_mainfigs_network_polish_v10")
dir_main <- file.path(output_root, "main_figures")
dir_supp <- file.path(output_root, "supplementary_figures")
dir_src <- file.path(output_root, "source_data")
dir_notes <- file.path(output_root, "notes")
dir_qa <- file.path(output_root, "qa")
for (d in c(output_root, dir_main, dir_supp, dir_src, dir_notes, dir_qa)) {
  dir.create(d, recursive = TRUE, showWarnings = FALSE)
}

desc_file <- file.path(staged_inputs_dir, "descriptive_numeric.csv")
if (!file.exists(desc_file)) {
  stop("Could not find the staged descriptive numeric CSV: ", desc_file)
}
node_file <- file.path(tables_dir, "00_node_dictionary.csv")

required_files <- c(
  file.path(tables_dir, "10_author_ivpp_global_fit_indices.csv"),
  file.path(tables_dir, "11_author_ivpp_global_omnibus_tests.csv"),
  file.path(tables_dir, "20_author_ivpp_partial_prune_fit_indices.csv"),
  file.path(tables_dir, "30_author_male_temporal_beta_matrix.csv"),
  file.path(tables_dir, "31_author_female_temporal_beta_matrix.csv"),
  file.path(tables_dir, "32_author_temporal_beta_difference_matrix_female_minus_male.csv"),
  file.path(tables_dir, "33_author_male_contemporaneous_matrix.csv"),
  file.path(tables_dir, "34_author_female_contemporaneous_matrix.csv"),
  file.path(tables_dir, "35_author_contemporaneous_difference_matrix_female_minus_male.csv"),
  file.path(tables_dir, "40_author_temporal_difference_edges.csv"),
  file.path(tables_dir, "41_author_contemporaneous_difference_edges.csv"),
  desc_file,
  node_file
)
missing_files <- required_files[!file.exists(required_files)]
if (length(missing_files)) {
  stop("Missing required input files:\n", paste(missing_files, collapse = "\n"))
}

global_fit <- read_csv(file.path(tables_dir, "10_author_ivpp_global_fit_indices.csv"), show_col_types = FALSE)
global_omni <- read_csv(file.path(tables_dir, "11_author_ivpp_global_omnibus_tests.csv"), show_col_types = FALSE)
partial_fit <- read_csv(file.path(tables_dir, "20_author_ivpp_partial_prune_fit_indices.csv"), show_col_types = FALSE)
temporal_edges <- read_csv(file.path(tables_dir, "40_author_temporal_difference_edges.csv"), show_col_types = FALSE)
contemporaneous_edges <- read_csv(file.path(tables_dir, "41_author_contemporaneous_difference_edges.csv"), show_col_types = FALSE)
desc_stats <- read_csv(desc_file, show_col_types = FALSE)
node_dict <- read_csv(node_file, show_col_types = FALSE)

read_matrix_csv <- function(path) {
  x <- read_csv(path, show_col_types = FALSE)
  rn <- x$node
  x$node <- NULL
  mat <- as.matrix(x)
  storage.mode(mat) <- "numeric"
  rownames(mat) <- rn
  colnames(mat) <- colnames(x)
  mat
}

male_temp <- read_matrix_csv(file.path(tables_dir, "30_author_male_temporal_beta_matrix.csv"))
female_temp <- read_matrix_csv(file.path(tables_dir, "31_author_female_temporal_beta_matrix.csv"))
diff_temp <- read_matrix_csv(file.path(tables_dir, "32_author_temporal_beta_difference_matrix_female_minus_male.csv"))
male_cont <- read_matrix_csv(file.path(tables_dir, "33_author_male_contemporaneous_matrix.csv"))
female_cont <- read_matrix_csv(file.path(tables_dir, "34_author_female_contemporaneous_matrix.csv"))
diff_cont <- read_matrix_csv(file.path(tables_dir, "35_author_contemporaneous_difference_matrix_female_minus_male.csv"))

vars <- node_dict$raw_variable
abbr <- setNames(node_dict$abbreviation, node_dict$raw_variable)
labels_long <- setNames(node_dict$english_label, node_dict$raw_variable)
communities <- setNames(node_dict$community_label, node_dict$raw_variable)

palette <- c(
  navy = "#123A63",
  slate = "#34495E",
  grey_1 = "#F8FAFB",
  grey_2 = "#E7ECEF",
  grey_3 = "#B9C3CC",
  grey_4 = "#74808A",
  lavender_bg = "#F0ECFA",
  lavender = "#D0C0F0",
  purple = "#6B4BB3",
  ple = "#0F7C73",
  ple_soft = "#5ABCB2",
  si = "#F05A3C",
  si_soft = "#F08A78",
  girls = "#C85890",
  boys = "#5080C8",
  positive = "#0F7C73",
  negative = "#F05A3C",
  model = "#6B4BB3",
  fit = "#0F7C73",
  contribution = "#5C8C45",
  contribution_soft = "#B7C56A"
)
pal <- function(name) unname(palette[name])

theme_pub <- function(base_size = 7.2) {
  theme_classic(base_size = base_size, base_family = "Arial") +
    theme(
      axis.line = element_line(linewidth = 0.32, colour = pal("navy")),
      axis.ticks = element_line(linewidth = 0.28, colour = pal("navy")),
      axis.title = element_text(size = base_size, colour = pal("navy")),
      axis.text = element_text(size = base_size - 0.45, colour = pal("slate")),
      plot.title = element_text(size = base_size + 0.45, face = "bold", colour = pal("navy")),
      plot.subtitle = element_text(size = base_size - 0.3, colour = pal("slate")),
      legend.title = element_text(size = base_size - 0.35),
      legend.text = element_text(size = base_size - 0.7),
      legend.key.height = unit(3, "mm"),
      legend.key.width = unit(4.5, "mm"),
      strip.text = element_text(size = base_size - 0.25, face = "bold"),
      panel.grid.major.y = element_line(linewidth = 0.16, colour = pal("grey_2")),
      panel.grid.major.x = element_blank(),
      panel.grid.minor = element_blank()
    )
}
theme_set(theme_pub())

save_pub <- function(plot, name, width_mm = 183, height_mm = 120, dpi = 600) {
  target_dir <- if (grepl("^FigureS", name)) dir_supp else dir_main
  base <- file.path(target_dir, name)
  w <- width_mm / 25.4
  h <- height_mm / 25.4
  repair_short_extensions <- function() {
    ext_map <- list(svg = c("s", "sv"), png = c("p", "pn"), tiff = c("t", "ti"))
    for (target_ext in names(ext_map)) {
      target <- paste0(base, ".", target_ext)
      short <- paste0(base, ".", ext_map[[target_ext]])
      if (!file.exists(target) && file.exists(short)) {
        if (file.exists(target)) try(unlink(target), silent = TRUE)
        ok <- file.rename(short, target)
        if (!ok) stop("Failed to repair exported filename extension: ", short, call. = FALSE)
      }
    }
  }

  safe_device <- function(path, open_device) {
    tryCatch({
      if (file.exists(path)) try(unlink(path), silent = TRUE)
      open_device(path)
      print(plot)
      dev.off()
    }, error = function(e) {
      if (names(dev.cur()) != "null device") try(dev.off(), silent = TRUE)
      stop("Failed to write ", path, ": ", conditionMessage(e), call. = FALSE)
    })
  }

  safe_device(paste0(base, ".svg"), function(path) svglite::svglite(path, width = w, height = h))
  safe_device(paste0(base, ".pdf"), function(path) grDevices::cairo_pdf(path, width = w, height = h, family = "Arial"))
  safe_device(paste0(base, ".png"), function(path) ragg::agg_png(path, width = w, height = h, units = "in", res = dpi, background = "white"))
  safe_device(paste0(base, ".tiff"), function(path) ragg::agg_tiff(path, width = w, height = h, units = "in", res = dpi, background = "white", compression = "lzw"))
  repair_short_extensions()
}

write_source <- function(x, name) {
  write_csv(as.data.frame(x), file.path(dir_src, paste0(name, ".csv")))
}

fmt_num <- function(x, digits = 2) sprintf(paste0("%.", digits, "f"), x)

build_reference_matrix <- function(mats, vars, communities) {
  ref <- matrix(0, nrow = length(vars), ncol = length(vars), dimnames = list(vars, vars))
  for (m in mats) {
    mm <- abs(as.matrix(m))
    diag(mm) <- 0
    mm[!is.finite(mm)] <- 0
    ref <- ref + mm[vars, vars, drop = FALSE]
  }
  for (comm in unique(communities)) {
    idx <- which(communities[vars] == comm)
    if (length(idx) > 1) {
      for (i in seq_len(length(idx) - 1)) {
        ref[idx[i], idx[i + 1]] <- ref[idx[i], idx[i + 1]] + 0.01
        ref[idx[i + 1], idx[i]] <- ref[idx[i + 1], idx[i]] + 0.01
      }
    }
  }
  ref
}

make_qgraph_layout <- function(mats, vars, communities, labels) {
  ref <- build_reference_matrix(mats, vars, communities)
  groups_list <- list(
    "Psychotic Experiences" = which(communities[vars] == "Psychotic Experiences"),
    "Self-Harm/Suicidality" = which(communities[vars] == "Self-Harm/Suicidality")
  )
  lay <- tryCatch({
    qobj <- qgraph::qgraph(
      ref,
      layout = "spring",
      DoNotPlot = TRUE,
      labels = labels[vars],
      groups = groups_list,
      color = c("Psychotic Experiences" = pal("ple_soft"), "Self-Harm/Suicidality" = pal("si_soft")),
      borders = FALSE,
      edge.labels = FALSE,
      details = FALSE
    )
    as.matrix(qobj$layout)
  }, error = function(e) NULL)
  if (is.null(lay) || !is.matrix(lay) || nrow(lay) != length(vars) || ncol(lay) != 2 || any(!is.finite(lay))) {
    lay <- qgraph::averageLayout(lapply(mats, function(m) {
      mm <- abs(m[vars, vars, drop = FALSE])
      mm[!is.finite(mm)] <- 0
      diag(mm) <- 0
      mm
    }), layout = "spring")
  }
  lay <- as.matrix(lay)
  lay[, 1] <- as.numeric(scale(lay[, 1], center = TRUE, scale = max(abs(lay[, 1] - mean(lay[, 1])), na.rm = TRUE)))
  lay[, 2] <- as.numeric(scale(lay[, 2], center = TRUE, scale = max(abs(lay[, 2] - mean(lay[, 2])), na.rm = TRUE)))
  lay <- lay * 0.64
  lay[!is.finite(lay)] <- 0
  rownames(lay) <- vars
  lay
}

set.seed(20261009)
qgraph_layout_temporal <- make_qgraph_layout(
  list(male_temp, female_temp, diff_temp), vars, communities, abbr
)
set.seed(20261010)
qgraph_layout_contemporaneous <- make_qgraph_layout(
  list(male_cont, female_cont, diff_cont), vars, communities, abbr
)
qgraph_layout <- qgraph_layout_temporal
qgraph_groups <- list(
  "Psychotic Experiences" = which(communities[vars] == "Psychotic Experiences"),
  "Self-Harm/Suicidality" = which(communities[vars] == "Self-Harm/Suicidality")
)
qgraph_group_colors <- c(
  "Psychotic Experiences" = pal("ple_soft"),
  "Self-Harm/Suicidality" = pal("si_soft")
)
node_layout <- tibble::tibble(
  raw = vars,
  label = abbr[vars],
  full_label = labels_long[vars],
  community = communities[vars],
  x = qgraph_layout[, 1],
  y = qgraph_layout[, 2],
  node_fill = ifelse(communities[vars] == "Psychotic Experiences", pal("ple_soft"), pal("si_soft"))
)
write_source(node_layout, "nodes")
write_source(tibble::tibble(
  raw = vars,
  label = abbr[vars],
  full_label = labels_long[vars],
  community = communities[vars],
  x = qgraph_layout_contemporaneous[, 1],
  y = qgraph_layout_contemporaneous[, 2],
  node_fill = ifelse(communities[vars] == "Psychotic Experiences", pal("ple_soft"), pal("si_soft"))
), "nodes_contemporaneous")

matrix_to_edges <- function(mat, directed = TRUE, sign_mode = c("raw", "difference"), tolerance = 1e-12) {
  sign_mode <- match.arg(sign_mode)
  idx <- which(abs(mat) > tolerance, arr.ind = TRUE)
  if (!directed) idx <- idx[idx[, 1] < idx[, 2], , drop = FALSE]
  if (!nrow(idx)) return(tibble::tibble())
  x <- tibble::tibble(
    from = rownames(mat)[idx[, 1]],
    to = colnames(mat)[idx[, 2]],
    weight = mat[idx],
    abs_weight = abs(weight)
  )
  x %>%
    mutate(
      sign = if (sign_mode == "difference") {
        ifelse(weight >= 0, "Girls higher", "Boys higher")
      } else {
        ifelse(weight >= 0, "Positive", "Negative")
      }
    )
}

prepare_qgraph_matrix <- function(mat, directed = TRUE, tolerance = 1e-12) {
  m <- mat[vars, vars, drop = FALSE]
  m[!is.finite(m)] <- 0
  m[abs(m) <= tolerance] <- 0
  if (!directed) diag(m) <- 0
  m
}

edge_colour_matrix <- function(m) {
  ec <- matrix(NA_character_, nrow = nrow(m), ncol = ncol(m), dimnames = dimnames(m))
  ec[m > 0] <- pal("positive")
  ec[m < 0] <- pal("negative")
  ec[is.na(ec)] <- pal("grey_3")
  ec
}

draw_solid_arrowhead <- function(previous, tip, colour, size = NULL, lwd = 1) {
  direction <- tip - previous
  direction_norm <- sqrt(sum(direction^2))
  if (!is.finite(direction_norm) || direction_norm < 1e-8) return(invisible(NULL))
  if (is.null(size)) {
    size <- max(0.020, min(0.026, 0.018 + 0.0035 * min(lwd, 2.3)))
  }
  unit <- direction / direction_norm
  perpendicular <- c(-unit[2], unit[1])
  base <- tip - unit * size
  triangle <- rbind(
    tip,
    base + perpendicular * size * 0.42,
    base - perpendicular * size * 0.42
  )
  graphics::polygon(triangle[, 1], triangle[, 2], col = colour, border = colour)
  invisible(NULL)
}

draw_bezier_arrow <- function(path, colour, lwd, directed = TRUE,
                              avoid_nodes = NULL, excluded_nodes = integer(),
                              clearance = 0) {
  if (is.null(path) || nrow(path) < 2) return(invisible(NULL))
  if (!is.null(avoid_nodes) && nrow(path) > 1 && clearance > 0) {
    keep <- rep(TRUE, nrow(path))
    candidate_nodes <- setdiff(seq_len(nrow(avoid_nodes)), excluded_nodes)
    for (node_index in candidate_nodes) {
      distance <- sqrt(rowSums((path - matrix(avoid_nodes[node_index, ],
        nrow = nrow(path), ncol = 2, byrow = TRUE))^2))
      keep <- keep & distance >= clearance
    }
    kept <- which(keep)
    if (length(kept)) {
      runs <- split(kept, cumsum(c(TRUE, diff(kept) > 1)))
      for (run in runs) {
        if (length(run) >= 2) {
          graphics::lines(path[run, 1], path[run, 2], col = colour,
                          lwd = lwd, lend = "butt")
        }
      }
    }
  } else {
    graphics::lines(path[, 1], path[, 2], col = colour, lwd = lwd, lend = "butt")
  }
  if (directed) {
    n <- nrow(path)
    previous_index <- max(1, n - 3)
    draw_solid_arrowhead(path[previous_index, ], path[n, ], colour, lwd = lwd)
  }
}

make_edge_path <- function(from, to, node_radius, curve_offset = 0) {
  direct <- to - from
  direct_norm <- sqrt(sum(direct^2))
  if (!is.finite(direct_norm) || direct_norm < 1e-8) return(NULL)
  unit <- direct / direct_norm
  perp <- c(-unit[2], unit[1])
  start <- from + unit * node_radius
  end <- to - unit * (node_radius * 1.35)
  if (abs(curve_offset) < 1e-8) {
    # Sample straight contemporaneous edges densely so that the label-clearance
    # filter can remove only the segment crossing a non-endpoint node.
    tt <- seq(0, 1, length.out = 81)
    return(cbind(
      start[1] + (end[1] - start[1]) * tt,
      start[2] + (end[2] - start[2]) * tt
    ))
  }
  control <- (from + to) / 2 + perp * curve_offset
  tt <- seq(0, 1, length.out = 41)
  path <- t(vapply(tt, function(t) {
    (1 - t)^2 * from + 2 * (1 - t) * t * control + t^2 * to
  }, numeric(2)))
  path[1, ] <- from + (path[2, ] - from) / sqrt(sum((path[2, ] - from)^2)) * node_radius
  path[nrow(path), ] <- to - (to - path[nrow(path) - 1, ]) /
    sqrt(sum((to - path[nrow(path) - 1, ])^2)) * (node_radius * 1.35)
  path
}

draw_self_loop <- function(center, weight, max_abs, colour, node_radius) {
  radial_norm <- sqrt(sum(center^2))
  radial <- if (is.finite(radial_norm) && radial_norm > 1e-8) center / radial_norm else c(0, 1)
  tangent <- c(-radial[2], radial[1])
  loop_center <- center + radial * 0.078
  loop_radius <- min(0.026, max(0.020, node_radius * 0.38))
  theta <- seq(0.84 * pi, -0.84 * pi, length.out = 61)
  path <- t(vapply(theta, function(th) {
    loop_center + radial * cos(th) * loop_radius + tangent * sin(th) * loop_radius * 0.92
  }, numeric(2)))
  loop_width <- 0.52 + 0.92 * sqrt(abs(weight) / max_abs)
  draw_bezier_arrow(path, colour, loop_width, directed = TRUE)
}

plot_qgraph_panel <- function(mat, panel_letter, directed = TRUE, max_abs = NULL,
                              layout_coords = qgraph_layout) {
  m <- prepare_qgraph_matrix(mat, directed = directed)
  if (is.null(max_abs)) max_abs <- max(abs(m), na.rm = TRUE)
  if (!is.finite(max_abs) || max_abs <= 0) max_abs <- 1
  self_edges <- if (directed) which(abs(diag(m)) > 1e-12) else integer()
  self_weights <- diag(m)[self_edges]
  diag(m) <- 0
  coords <- as.matrix(layout_coords)[vars, , drop = FALSE]
  node_radius <- 0.068
  xlim <- c(-0.82, 0.82)
  ylim <- c(-0.82, 0.82)
  par(mar = c(0.05, 0.05, 0.15, 0.05))
  plot(NA, xlim = xlim, ylim = ylim, asp = 1, axes = FALSE,
       xlab = "", ylab = "", bty = "n", xaxt = "n", yaxt = "n")

  edge_idx <- which(abs(m) > 1e-12, arr.ind = TRUE)
  if (nrow(edge_idx)) {
    for (row_id in seq_len(nrow(edge_idx))) {
      i <- edge_idx[row_id, 1]
      j <- edge_idx[row_id, 2]
      weight <- m[i, j]
      reciprocal <- directed && i != j && abs(m[j, i]) > 1e-12
      curve_offset <- if (reciprocal) 0.026 * ifelse(i < j, 1, -1) else 0
      path <- make_edge_path(coords[i, ], coords[j, ], node_radius, curve_offset)
      if (is.null(path)) next
      edge_colour <- if (weight >= 0) pal("positive") else pal("negative")
      edge_width <- 0.68 + 1.55 * sqrt(abs(weight) / max_abs)
      draw_bezier_arrow(
        path, edge_colour, edge_width, directed = directed,
        avoid_nodes = if (!directed) coords else NULL,
        excluded_nodes = c(i, j),
        clearance = if (!directed) node_radius * 1.10 else 0
      )
    }
  }

  if (length(self_edges)) {
    for (k in seq_along(self_edges)) {
      i <- self_edges[k]
      loop_colour <- if (self_weights[k] >= 0) pal("positive") else pal("negative")
      draw_self_loop(coords[i, ], self_weights[k], max_abs, loop_colour, node_radius)
    }
  }

  node_fill <- ifelse(communities[vars] == "Psychotic Experiences", pal("ple_soft"), pal("si_soft"))
  points(coords[, 1], coords[, 2], pch = 16, cex = 2.52, col = node_fill)
  text(coords[, 1], coords[, 2], labels = unname(abbr[vars]),
       cex = 0.78, col = "white", family = "Helvetica")
  text(xlim[1] + 0.01, ylim[2] - 0.005, panel_letter, adj = c(0, 1),
       cex = 1.06, font = 2, col = pal("navy"), family = "Helvetica")
}

write_base_alignment_manifest <- function(file_base, width_mm, height_mm, panel_bboxes) {
  if (length(panel_bboxes) != 3L) {
    stop("Expected three comparable network panels for alignment QA")
  }
  width_pt <- width_mm / 25.4 * 72
  height_pt <- height_mm / 25.4 * 72
  panels <- lapply(seq_along(panel_bboxes), function(i) {
    b <- panel_bboxes[[i]]
    list(
      id = c("A", "B", "C")[i],
      bbox_pt = unname(b),
      panel_label_anchor_pt = c(b[1] + 8, b[4] - 8),
      grid_id = "base-layout-1",
      row_start = 0L,
      row_stop = 1L,
      col_start = as.integer(i - 1L),
      col_stop = as.integer(i)
    )
  })
  manifest <- list(
    schema_version = 1L,
    backend = "r-base-graphics",
    figure = list(width_pt = width_pt, height_pt = height_pt),
    panels = panels,
    row_groups = list(list(id = "network-triptych", panels = c("A", "B", "C"))),
    exemptions = list()
  )
  jsonlite::write_json(
    manifest,
    file.path(dir_qa, paste0(basename(file_base), ".alignment-layout.json")),
    auto_unbox = TRUE,
    pretty = TRUE,
    digits = NA
  )
}

plot_network_row <- function(mats, titles, directed, max_abs, layout_coords,
                             group_label, file_base, width_mm = 183,
                             height_mm = 82, include_node_key = FALSE) {
  panel_bboxes <- NULL
  draw_row <- function() {
    oldpar <- par(no.readonly = TRUE)
    on.exit(par(oldpar), add = TRUE)
    layout(matrix(c(1, 2, 3, 4, 4, 4), nrow = 2, byrow = TRUE),
           heights = c(0.78, 0.22))
    par(family = "Helvetica", bg = "white", oma = c(0.2, 0.2, 0.2, 0.2))
    for (i in seq_along(mats)) {
      plot_qgraph_panel(mats[[i]], titles[[i]], directed = directed,
                        max_abs = max_abs, layout_coords = layout_coords)
      if (length(panel_bboxes) < 3L) {
        fig <- par("fig")
        plt <- par("plt")
        left <- fig[1] + plt[1] * (fig[2] - fig[1])
        right <- fig[1] + plt[2] * (fig[2] - fig[1])
        bottom <- fig[3] + plt[3] * (fig[4] - fig[3])
        top <- fig[3] + plt[4] * (fig[4] - fig[3])
        panel_bboxes[[i]] <<- unname(c(
          left * width_mm / 25.4 * 72,
          bottom * height_mm / 25.4 * 72,
          right * width_mm / 25.4 * 72,
          top * height_mm / 25.4 * 72
        ))
      }
    }
    draw_network_legend_base(directed = directed, include_node_key = include_node_key)
  }
  save_base_figure(file_base, width_mm, height_mm, draw_row)
  write_base_alignment_manifest(file_base, width_mm, height_mm, panel_bboxes)
}

draw_legend_self_loop <- function(x = 0.72, y = 0.47, colour = pal("slate")) {
  center <- c(x, y)
  theta <- seq(0.84 * pi, -0.84 * pi, length.out = 41)
  loop_center <- center + c(0, 0.018)
  path <- cbind(
    loop_center[1] + cos(theta) * 0.028,
    loop_center[2] + sin(theta) * 0.028
  )
  graphics::lines(path[, 1], path[, 2], col = colour, lwd = 1.0, lend = "butt")
  draw_solid_arrowhead(path[nrow(path) - 3, ], path[nrow(path), ], colour, size = 0.010, lwd = 1)
}

draw_network_legend_base <- function(directed = TRUE, include_node_key = FALSE) {
  par(mar = c(0, 0, 0, 0))
  plot.new()
  plot.window(xlim = c(0, 1), ylim = c(0, 1), xaxs = "i", yaxs = "i")
  par(xpd = NA)
  title_cex <- 0.72
  body_cex <- 0.72
  text(0.03, 0.82, "Node domains", adj = 0, font = 2, cex = title_cex, col = pal("navy"))
  points(c(0.075, 0.075), c(0.55, 0.27), pch = 21,
         bg = c(pal("ple_soft"), pal("si_soft")), col = "white", lwd = 0.7, cex = 0.92)
  text(c(0.115, 0.115), c(0.55, 0.27),
       c("Psychotic-like experiences", "Self-harm / suicidality"),
       adj = 0, cex = body_cex, col = pal("navy"))

  text(0.35, 0.82, "Edge colour", adj = 0, font = 2, cex = title_cex, col = pal("navy"))
  segments(0.35, 0.55, 0.41, 0.55, lwd = 1.25, col = pal("positive"), lend = "butt")
  segments(0.35, 0.27, 0.41, 0.27, lwd = 1.25, col = pal("negative"), lend = "butt")
  text(0.44, 0.55, "Positive / girls higher", adj = 0, cex = body_cex, col = pal("navy"))
  text(0.44, 0.27, "Negative / boys higher", adj = 0, cex = body_cex, col = pal("navy"))

  text(0.68, 0.82, "Edge type", adj = 0, font = 2, cex = title_cex, col = pal("navy"))
  if (directed) {
    graphics::segments(0.68, 0.55, 0.75, 0.55, lwd = 1.0, col = pal("slate"), lend = "butt")
    draw_solid_arrowhead(c(0.73, 0.55), c(0.75, 0.55), pal("slate"), size = 0.010)
    text(0.78, 0.55, "Cross-wave directed", adj = 0, cex = body_cex, col = pal("navy"))
    draw_legend_self_loop(0.715, 0.24, pal("slate"))
    text(0.78, 0.27, "Autoregressive loop", adj = 0, cex = body_cex, col = pal("navy"))
  } else {
    graphics::segments(0.68, 0.41, 0.75, 0.41, lwd = 1.0, col = pal("slate"), lend = "butt")
    text(0.78, 0.41, "Within-wave undirected", adj = 0, cex = body_cex, col = pal("navy"))
  }
  if (include_node_key) {
    key <- paste0(unname(abbr[vars]), "=", unname(labels_long[vars]))
    text(0.03, 0.06, paste(key, collapse = "   "), adj = 0, cex = 0.52, col = pal("slate"))
  }
  par(xpd = FALSE)
}

save_base_figure <- function(name, width_mm, height_mm, plot_fun, dpi = 600) {
  target_dir <- if (grepl("^FigureS", name)) dir_supp else dir_main
  base <- file.path(target_dir, name)
  w <- width_mm / 25.4
  h <- height_mm / 25.4
  repair_short_extensions <- function() {
    ext_map <- list(svg = c("s", "sv"), png = c("p", "pn"), tiff = c("t", "ti"))
    for (target_ext in names(ext_map)) {
      target <- paste0(base, ".", target_ext)
      short <- paste0(base, ".", ext_map[[target_ext]])
      if (!file.exists(target) && file.exists(short)) {
        ok <- file.rename(short, target)
        if (!ok) stop("Failed to repair exported filename extension: ", short, call. = FALSE)
      }
    }
  }
  safe_device <- function(path, open_device) {
    tryCatch({
      if (file.exists(path)) try(unlink(path), silent = TRUE)
      open_device(path)
      plot_fun()
      dev.off()
    }, error = function(e) {
      if (names(dev.cur()) != "null device") try(dev.off(), silent = TRUE)
      stop("Failed to write ", path, ": ", conditionMessage(e), call. = FALSE)
    })
  }
  safe_device(paste0(base, ".svg"), function(path) svglite::svglite(path, width = w, height = h))
  safe_device(paste0(base, ".pdf"), function(path) grDevices::pdf(path, width = w, height = h, family = "Helvetica", useDingbats = FALSE))
  safe_device(paste0(base, ".png"), function(path) ragg::agg_png(path, width = w, height = h, units = "in", res = dpi, background = "white"))
  safe_device(paste0(base, ".tiff"), function(path) ragg::agg_tiff(path, width = w, height = h, units = "in", res = dpi, background = "white", compression = "lzw"))
  repair_short_extensions()
}

save_logic_figure <- function() {
  source_png <- file.path(staged_inputs_dir, "logic_diagram.png")
  source_pptx <- file.path(staged_inputs_dir, "logic_diagram_editable.pptx")
  if (!file.exists(source_png)) stop("Missing logic diagram PNG: ", source_png)
  img <- png::readPNG(source_png)
  h <- dim(img)[1]
  w <- dim(img)[2]
  width_mm <- 183
  height_mm <- width_mm * h / w
  base <- file.path(dir_main, "Figure1_logic_framework")
  draw <- function() {
    grid::grid.newpage()
    grid::grid.raster(img, interpolate = TRUE)
  }
  cairo_pdf <- function(path) grDevices::cairo_pdf(path, width = width_mm / 25.4,
                                                     height = height_mm / 25.4,
                                                     family = "Arial")
  for (ext in c("pdf", "png", "tiff")) {
    path <- paste0(base, ".", ext)
    if (file.exists(path)) unlink(path)
    if (ext == "pdf") {
      cairo_pdf(path); draw(); dev.off()
    } else if (ext == "png") {
      ragg::agg_png(path, width = width_mm / 25.4, height = height_mm / 25.4,
                    units = "in", res = 600, background = "white")
      draw(); dev.off()
    } else {
      ragg::agg_tiff(path, width = width_mm / 25.4, height = height_mm / 25.4,
                     units = "in", res = 600, background = "white", compression = "lzw")
      draw(); dev.off()
    }
  }
  if (file.exists(source_pptx)) {
    file.copy(source_pptx, file.path(dir_main, "Figure1_logic_framework_editable.pptx"), overwrite = TRUE)
  }
}

save_network_backup <- function(name = "FigureS4_complete_networks_backup", width_mm = 183, height_mm = 170) {
  save_base_figure(name, width_mm, height_mm, function() {
    oldpar <- par(no.readonly = TRUE)
    on.exit(par(oldpar), add = TRUE)
    layout(matrix(c(1, 2, 3, 4, 5, 6, 7, 7, 7), nrow = 3, byrow = TRUE), heights = c(1, 1, 0.32))
    par(family = "Helvetica", bg = "white", oma = c(0.2, 0.2, 0.2, 0.2))
    plot_qgraph_panel(male_temp, "A", directed = TRUE, max_abs = max_temp,
                      layout_coords = qgraph_layout_temporal)
    plot_qgraph_panel(female_temp, "B", directed = TRUE, max_abs = max_temp,
                      layout_coords = qgraph_layout_temporal)
    plot_qgraph_panel(diff_temp, "C", directed = TRUE, max_abs = max_temp,
                      layout_coords = qgraph_layout_temporal)
    plot_qgraph_panel(male_cont, "A", directed = FALSE, max_abs = max_cont,
                      layout_coords = qgraph_layout_contemporaneous)
    plot_qgraph_panel(female_cont, "B", directed = FALSE, max_abs = max_cont,
                      layout_coords = qgraph_layout_contemporaneous)
    plot_qgraph_panel(diff_cont, "C", directed = FALSE, max_abs = max_cont,
                      layout_coords = qgraph_layout_contemporaneous)
    draw_network_legend_base(include_node_key = TRUE)
  })
}

edge_label_data <- function(edge_df, directed = TRUE) {  if (directed) {
    edge_df %>%
      transmute(
        network = "Temporal",
        edge = paste0(source_short_label, " -> ", target_short_label),
        diff = diff_female_minus_male,
        abs_diff = abs_diff,
        direction = ifelse(diff > 0, "Girls higher", "Boys higher")
      )
  } else {
    edge_df %>%
      transmute(
        network = "Contemporaneous",
        edge = paste0(node1_short_label, " - ", node2_short_label),
        diff = diff_female_minus_male,
        abs_diff = abs_diff,
        direction = ifelse(diff > 0, "Girls higher", "Boys higher")
      )
  }
}

ranked_edges_panel <- function(df, title, xlim_abs = NULL) {
  if (is.null(xlim_abs)) xlim_abs <- max(abs(df$diff), na.rm = TRUE)
  df <- df %>%
    arrange(abs_diff) %>%
    mutate(edge = factor(edge, levels = edge))
  ggplot(df, aes(x = diff, y = edge, fill = direction)) +
    geom_vline(xintercept = 0, linewidth = 0.32, linetype = "dashed", colour = pal("grey_3")) +
    geom_col(width = 0.62, colour = "white", linewidth = 0.15) +
    geom_text(aes(label = sprintf("%.2f", diff), hjust = ifelse(diff > 0, -0.10, 1.10)), size = 2.0, colour = pal("navy")) +
    scale_fill_manual(values = c("Girls higher" = pal("positive"), "Boys higher" = pal("negative")), guide = "none") +
    scale_x_continuous(limits = c(-xlim_abs * 1.30, xlim_abs * 1.30), breaks = pretty_breaks(4)) +
    labs(title = title, x = "Edge-weight difference (girls - boys)", y = NULL) +
    theme_pub(base_size = 7.0) +
    theme(
      axis.text.y = element_text(size = 6.0),
      panel.grid.major.y = element_blank(),
      plot.margin = margin(2, 5, 2, 2)
    )
}

direction_legend_plot <- function() {
  df <- tibble::tibble(
    x = c(0.43, 0.57), xend = c(0.48, 0.62), y = c(0.5, 0.5),
    label = c("Boys higher", "Girls higher"),
    colour = c(pal("negative"), pal("positive"))
  )
  ggplot(df) +
    xlim(0, 1) + ylim(0, 1) +
    geom_segment(aes(x = x, xend = xend, y = y, yend = y), linewidth = 2.2, colour = df$colour, lineend = "butt") +
    geom_text(aes(x = xend + 0.018, y = y, label = label), hjust = 0, vjust = 0.5, size = 2.3, colour = pal("navy")) +
    theme_void(base_family = "Arial") +
    theme(plot.margin = margin(0, 2, 0, 2))
}
matrix_long <- function(mat, network_type) {
  as.data.frame(mat) %>%
    tibble::rownames_to_column("from") %>%
    pivot_longer(-from, names_to = "to", values_to = "diff") %>%
    mutate(from_label = abbr[from], to_label = abbr[to], network_type = network_type)
}

heatmap_panel <- function(mat, title, directed = TRUE, max_abs = NULL) {
  df <- matrix_long(mat, title)
  if (!directed) df <- df %>% filter(match(from, vars) < match(to, vars))
  if (is.null(max_abs)) max_abs <- max(abs(df$diff), na.rm = TRUE)
  ggplot(df, aes(x = to_label, y = from_label, fill = diff)) +
    geom_tile(colour = "white", linewidth = 0.18) +
    scale_fill_gradient2(low = pal("negative"), mid = "white", high = pal("positive"), midpoint = 0, limits = c(-max_abs, max_abs), name = "Girls - boys") +
    coord_equal() +
    labs(title = title, x = if (directed) "Predicted node at next wave" else "Node 2", y = if (directed) "Predictor node at previous wave" else "Node 1") +
    theme_void(base_size = 7.0, base_family = "Arial") +
    theme(
      axis.line = element_line(linewidth = 0.32, colour = pal("navy")),
      axis.ticks = element_line(linewidth = 0.28, colour = pal("navy")),
      axis.title = element_text(size = 7.0, colour = pal("navy")),
      axis.title.x = element_text(margin = margin(t = 4)),
      axis.title.y = element_text(angle = 90, vjust = 0.5, margin = margin(r = 5)),
      axis.text = element_text(size = 6.4, colour = pal("slate")),
      axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
      panel.grid = element_blank(),
      plot.title = element_text(size = 7.6, face = "bold", colour = pal("navy")),
      legend.title = element_text(size = 6.3),
      legend.text = element_text(size = 6.0),
      legend.position = "right",
      plot.margin = margin(2, 2, 2, 2)
    )
}

# -----------------------------
# Figure 2: global model evidence
# -----------------------------

model_compare <- global_omni %>%
  filter(table_type == "vs_free", model != "free") %>%
  mutate(
    comparison = case_when(
      network_type == "both" ~ "Both",
      network_type == "temporal" ~ "Temporal",
      network_type == "contemporaneous" ~ "Contemporaneous",
      TRUE ~ model
    ),
    comparison = factor(comparison, levels = c("Both", "Temporal", "Contemporaneous"))
  )

model_long <- model_compare %>%
  select(comparison, delta_AIC, delta_BIC) %>%
  pivot_longer(starts_with("delta_"), names_to = "metric", values_to = "delta") %>%
  mutate(metric = recode(metric, delta_AIC = "AIC", delta_BIC = "BIC"))

fit_summary <- bind_rows(
  global_fit %>% filter(table_type == "both_free") %>% mutate(model_label = "Free model"),
  partial_fit %>% mutate(model_label = "Partial pruning")
) %>%
  select(model_label, cfi, tli, rmsea) %>%
  pivot_longer(c(cfi, tli, rmsea), names_to = "index", values_to = "value") %>%
  mutate(
    index = recode(index, cfi = "CFI", tli = "TLI", rmsea = "RMSEA"),
    index = factor(index, levels = c("CFI", "TLI", "RMSEA")),
    model_label = factor(model_label, levels = c("Free model", "Partial pruning"))
  )

edge_counts <- tibble::tibble(
  network = factor(c("Temporal", "Contemporaneous"), levels = c("Temporal", "Contemporaneous")),
  edges = c(nrow(temporal_edges), nrow(contemporaneous_edges))
)

write_source(model_compare, "Figure2_model_evidence_source")
write_source(fit_summary, "Figure2_fit_indices_source")
write_source(edge_counts, "Figure2_retained_edge_counts_source")

p1a <- ggplot(model_long, aes(x = comparison, y = delta, colour = metric)) +
  geom_col(aes(fill = metric), width = 0.58, position = position_dodge(width = 0.70), colour = "white", linewidth = 0.18) +
  geom_text(aes(label = sprintf("%.1f", delta), group = metric), position = position_dodge(width = 0.70), vjust = -0.45, size = 2.0, colour = pal("navy")) +
  scale_fill_manual(values = c("AIC" = pal("model"), "BIC" = pal("navy")), name = NULL) +
  scale_colour_manual(values = c("AIC" = pal("model"), "BIC" = pal("navy")), guide = "none") +
  scale_y_continuous(expand = expansion(mult = c(0.08, 0.18))) +
  labs(title = "Equality constraints worsened model fit", x = NULL, y = "Change vs free model") +
  theme_pub() +
  theme(legend.position = "top", legend.justification = "left", panel.grid.major.x = element_blank())

p1b <- ggplot(fit_summary, aes(x = index, y = value, colour = model_label, group = model_label)) +
  geom_point(size = 2.55, position = position_dodge(width = 0.16)) +
  scale_colour_manual(values = c("Free model" = pal("model"), "Partial pruning" = pal("fit")), name = NULL) +
  labs(title = "Model fit indices", x = NULL, y = "Fit index") +
  theme_pub() +
  theme(legend.position = "top", legend.justification = "left", panel.grid.major.x = element_blank())

p1c <- ggplot(edge_counts, aes(x = network, y = edges, fill = network)) +
  geom_col(width = 0.55, colour = "white", linewidth = 0.2) +
  geom_text(aes(label = edges), vjust = -0.35, size = 2.55, fontface = "bold") +
  scale_fill_manual(values = c("Temporal" = pal("contribution"), "Contemporaneous" = pal("purple")), guide = "none") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.18))) +
  labs(title = "Retained sex-differentiated edges after pruning", x = NULL, y = "Number of edges") +
  theme_pub() +
  theme(panel.grid.major.x = element_blank())

fig1 <- p1a / (p1b | p1c) +
  plot_layout(heights = c(1.03, 1)) +
  plot_annotation(tag_levels = "a") &
  theme(plot.tag = element_text(size = 8.6, face = "bold"))
save_logic_figure()
save_pub(fig1, "Figure2_model_evidence", width_mm = 183, height_mm = 118)

# -----------------------------
# Figures 3-4: split network triptychs
# -----------------------------

max_temp <- max(abs(c(male_temp, female_temp, diff_temp)), na.rm = TRUE)
max_cont <- max(abs(c(male_cont, female_cont, diff_cont)), na.rm = TRUE)

complete_network_source <- bind_rows(
  matrix_to_edges(male_temp, directed = TRUE, sign_mode = "raw") %>% mutate(network = "Temporal", group = "Boys"),
  matrix_to_edges(female_temp, directed = TRUE, sign_mode = "raw") %>% mutate(network = "Temporal", group = "Girls"),
  matrix_to_edges(diff_temp, directed = TRUE, sign_mode = "difference") %>% mutate(network = "Temporal", group = "Girls - boys"),
  matrix_to_edges(male_cont, directed = FALSE, sign_mode = "raw") %>% mutate(network = "Contemporaneous", group = "Boys"),
  matrix_to_edges(female_cont, directed = FALSE, sign_mode = "raw") %>% mutate(network = "Contemporaneous", group = "Girls"),
  matrix_to_edges(diff_cont, directed = FALSE, sign_mode = "difference") %>% mutate(network = "Contemporaneous", group = "Girls - boys")
)
write_source(complete_network_source %>% filter(network == "Temporal"), "Figure3_temporal_network_edges")
write_source(complete_network_source %>% filter(network == "Contemporaneous"), "Figure4_contemporaneous_network_edges")
write_source(tibble::tibble(network = c("Temporal", "Contemporaneous"), row_scaling_max_abs = c(max_temp, max_cont)), "network_row_scaling")

plot_network_row(
  list(male_temp, female_temp, diff_temp),
  c("A", "B", "C"),
  directed = TRUE, max_abs = max_temp, layout_coords = qgraph_layout_temporal,
  group_label = "Temporal", file_base = "Figure3_temporal_network_triptych",
  width_mm = 183, height_mm = 86
)
plot_network_row(
  list(male_cont, female_cont, diff_cont),
  c("A", "B", "C"),
  directed = FALSE, max_abs = max_cont, layout_coords = qgraph_layout_contemporaneous,
  group_label = "Contemporaneous", file_base = "Figure4_contemporaneous_network_triptych",
  width_mm = 183, height_mm = 96
)

# -----------------------------
# Supplementary Figure S1: difference-edge strength summary
# -----------------------------

temporal_rank <- edge_label_data(temporal_edges, directed = TRUE)
contemporaneous_rank <- edge_label_data(contemporaneous_edges, directed = FALSE)
combined_rank <- bind_rows(temporal_rank, contemporaneous_rank)
write_source(combined_rank, "FigureS1_difference_edge_strength")

xlim_rank <- max(abs(combined_rank$diff), na.rm = TRUE)
p3a <- ranked_edges_panel(temporal_rank, "Temporal differentiated edges", xlim_abs = xlim_rank)
p3b <- ranked_edges_panel(contemporaneous_rank, "Contemporaneous differentiated edges", xlim_abs = xlim_rank)
fig3 <- (p3a | p3b) +
  plot_layout(guides = "collect") +
  plot_annotation(tag_levels = "a") &
  theme(plot.tag = element_text(size = 8.6, face = "bold"), legend.position = "top")
save_pub(fig3, "FigureS1_difference_edge_strength", width_mm = 183, height_mm = 112)

# -----------------------------
# Supplementary Figure S2: descriptive sex differences
# -----------------------------

desc_plot <- desc_stats %>%
  mutate(
    variable_group = ifelse(grepl("_total$", variable), "Total", "Nodes"),
    label_short = ifelse(variable %in% names(abbr), abbr[variable], variable),
    variable_group = factor(variable_group, levels = c("Nodes", "Total")),
    sig = q_fdr_bh < 0.05,
    direction = factor(ifelse(cohens_d_female_minus_male >= 0, "Girls higher", "Boys higher"),
                       levels = c("Girls higher", "Boys higher"))
  ) %>%
  arrange(variable_group, match(variable, c(vars, "PE_total", "SI_total"))) %>%
  mutate(label_short = factor(label_short, levels = rev(unique(label_short))))
write_source(desc_plot, "FigureS2_descriptive_sex_differences")

direction_legend_df <- tibble::tibble(
  cohens_d_female_minus_male = 0,
  label_short = factor("A1", levels = levels(desc_plot$label_short)),
  variable_group = factor("Nodes", levels = c("Nodes", "Total")),
  direction = factor(c("Girls higher", "Boys higher"), levels = c("Girls higher", "Boys higher"))
)

p_s1 <- ggplot(desc_plot, aes(x = cohens_d_female_minus_male, y = label_short)) +
  geom_vline(xintercept = 0, linewidth = 0.32, linetype = "dashed", colour = pal("grey_3")) +
  geom_segment(aes(x = 0, xend = cohens_d_female_minus_male, yend = label_short, colour = direction, alpha = sig), linewidth = 0.62) +
  geom_point(aes(fill = direction, alpha = sig), shape = 21, size = 2.45, colour = "white", stroke = 0.32) +
  geom_point(data = direction_legend_df, aes(x = cohens_d_female_minus_male, y = label_short, fill = direction), shape = 21, size = 2.45, alpha = 0, inherit.aes = FALSE, show.legend = TRUE) +
  facet_grid(variable_group ~ ., scales = "free_y", space = "free_y") +
  scale_fill_manual(values = c("Girls higher" = pal("girls"), "Boys higher" = pal("boys")), limits = c("Girls higher", "Boys higher"), drop = FALSE, name = NULL) +
  scale_colour_manual(values = c("Girls higher" = pal("girls"), "Boys higher" = pal("boys")), limits = c("Girls higher", "Boys higher"), drop = FALSE, guide = "none") +
  scale_alpha_manual(values = c(`TRUE` = 0.86, `FALSE` = 0.38), labels = c(`TRUE` = "FDR q < .05", `FALSE` = "FDR q >= .05"), name = NULL) +
  guides(fill = guide_legend(order = 1, override.aes = list(alpha = 1, size = 2.8)),
         alpha = guide_legend(order = 2)) +
  labs(title = "Descriptive sex differences in node and total scores", x = "Cohen's d (girls - boys)", y = NULL) +
  theme_pub() +
  theme(
    legend.position = "top",
    legend.justification = "left",
    panel.grid.major.y = element_blank(),
    strip.background = element_rect(fill = pal("lavender_bg"), colour = NA),
    strip.text.y = element_text(size = 6.7, face = "bold", colour = pal("navy"))
  )
save_pub(p_s1, "FigureS2_descriptive_sex_differences", width_mm = 118, height_mm = 110)

# -----------------------------
# Supplementary Figure S3: full difference matrices
# -----------------------------

matrix_source <- bind_rows(matrix_long(diff_temp, "Temporal"), matrix_long(diff_cont, "Contemporaneous"))
write_source(matrix_source, "FigureS3_difference_matrices")
matrix_max <- max(abs(matrix_source$diff), na.rm = TRUE)

fig_s2 <- (heatmap_panel(diff_temp, "Temporal network", directed = TRUE, max_abs = matrix_max) |
  heatmap_panel(diff_cont, "Contemporaneous network", directed = FALSE, max_abs = matrix_max)) +
  plot_layout(guides = "collect") +
  plot_annotation(tag_levels = "a", title = "Girls - boys difference matrices") &
  theme(plot.tag = element_text(size = 8.6, face = "bold"), legend.position = "right")
save_pub(fig_s2, "FigureS3_difference_matrices", width_mm = 183, height_mm = 95)

# -----------------------------
# Supplementary Figure S4: backup complete-network figure with node key
# -----------------------------

save_network_backup("FigureS4_complete_networks_backup", width_mm = 183, height_mm = 170)

# -----------------------------
# Figure notes, captions, and QA checks
# -----------------------------

qa_expected <- tibble::tibble(
  check = c(
    "temporal_boys_edges", "temporal_girls_edges", "temporal_difference_edges",
    "contemporaneous_boys_edges", "contemporaneous_girls_edges", "contemporaneous_difference_edges",
    "temporal_rank_edges", "contemporaneous_rank_edges"
  ),
  expected = c(5, 7, 6, 24, 23, 14, 6, 14),
  observed = c(
    nrow(matrix_to_edges(male_temp, TRUE, "raw")),
    nrow(matrix_to_edges(female_temp, TRUE, "raw")),
    nrow(matrix_to_edges(diff_temp, TRUE, "difference")),
    nrow(matrix_to_edges(male_cont, FALSE, "raw")),
    nrow(matrix_to_edges(female_cont, FALSE, "raw")),
    nrow(matrix_to_edges(diff_cont, FALSE, "difference")),
    nrow(temporal_edges),
    nrow(contemporaneous_edges)
  )
)
qa_expected <- qa_expected %>% mutate(pass = expected == observed)
write_source(qa_expected, "qa_edge_count_checks")
write_csv(as.data.frame(qa_expected), file.path(dir_qa, "qa_edge_count_checks.csv"))
if (!all(qa_expected$pass)) {
  stop("One or more edge-count checks failed. See qa_edge_count_checks.csv")
}

notes <- c(
  "Split main and supplementary IVPP figure set notes",
  "",
  "Analysis was not rerun. Figures are rendered from existing author-style IVPP matrices and result tables.",
  "All difference values are in the girls-minus-boys direction.",
  "Temporal networks are cross-wave predictive associations, not causal effects.",
  "Contemporaneous networks are within-wave conditional associations after temporal dependencies are modeled.",
  "",
  "Figure 1: study logic, design, and analysis framework; this is a workflow figure, not a results figure.",
  "Figure 2: global model evidence and retained difference-edge counts.",
  "Figure 3: Temporal network triptych for boys, girls, and girls-minus-boys.",
  "Figure 4: Contemporaneous network triptych for boys, girls, and girls-minus-boys.",
  "Supplementary Figure S1: ranked temporal and contemporaneous differentiated edges.",
  "Supplementary Figure S2: descriptive node and total-score sex differences.",
  "Supplementary Figure S3: full temporal and contemporaneous difference matrices.",
  "Supplementary Figure S4: backup complete-network figure with node key.",
  "",
  "Visual design reference checked: Miller et al., Journal of Psychopathology and Clinical Science (2024), DOI 10.1037/abn0000916. Its panel captions define temporal arrows as predictive paths, contemporaneous edges as undirected conditional associations, edge width as association strength, and stable positive/negative edge colours.",
  "This script implements a project-specific redraw of those conventions; it does not copy the published figure or infer new results.",
  "Network panels use a shared solid arrowhead geometry; autoregressive loops are drawn as compact node-adjacent arcs.",
  paste0("Temporal row max absolute edge for Figure 3 scaling: ", fmt_num(max_temp, 4)),
  paste0("Contemporaneous row max absolute edge for Figure 4 scaling: ", fmt_num(max_cont, 4)),
  paste0("Temporal differentiated edges: ", nrow(temporal_edges)),
  paste0("Contemporaneous differentiated edges: ", nrow(contemporaneous_edges)),
  paste0("Output root: ", normalizePath(output_root, winslash = "/", mustWork = FALSE))
)
writeLines(notes, file.path(dir_notes, "redesigned_figure_set_notes.txt"), useBytes = TRUE)

captions <- c(
  "Figure 1. Study logic, research design, and analysis framework. The diagram presents the three-wave panel design, the distinction between temporal and contemporaneous networks, the sex comparison, and the IVPP workflow. It describes the study framework and does not display empirical results.",
  "",
  "Figure 2. Model evidence for sex differences in panel GVAR networks. Panel a shows changes in information criteria when equality constraints are imposed relative to the freely estimated model. Panel b summarizes fit indices for the free and partial-pruned models. Panel c shows the number of temporal and contemporaneous sex-differentiated edges retained after partial pruning.",
  "",
  "Figure 3. Temporal network triptych for boys, girls, and girls-minus-boys. Directed edges represent cross-wave predictive associations from the previous wave to the next wave. Difference values are in the girls-minus-boys direction. Edge widths share one scale across the triptych; no causal interpretation is implied.",
  "",
  "Figure 4. Contemporaneous network triptych for boys, girls, and girls-minus-boys. Undirected edges represent within-wave conditional associations after temporal dependencies are modeled. Difference values are in the girls-minus-boys direction; no causal interpretation is implied.",
  "",
  "Supplementary Figure S1. Strength and direction of retained sex-differentiated edges. Bars show edge-weight differences in the girls-minus-boys direction for temporal and contemporaneous networks. Positive values indicate stronger edges in girls; negative values indicate stronger edges in boys.",
  "",
  "Supplementary Figure S2. Descriptive sex differences in node-level and total scores. Effect sizes are Cohen's d in the girls-minus-boys direction; highlighted points indicate FDR q < .05.",
  "",
  "Supplementary Figure S3. Full girls-minus-boys difference matrices for temporal and contemporaneous networks.",
  "",
  "Supplementary Figure S4. Backup complete-network display with node key and edge-colour legend."
)
writeLines(captions, file.path(dir_notes, "redesigned_figure_caption_drafts_en.txt"), useBytes = TRUE)

cat("Redesigned IVPP figure set completed.\n")
cat("Output root:", normalizePath(output_root, winslash = "/", mustWork = FALSE), "\n")
