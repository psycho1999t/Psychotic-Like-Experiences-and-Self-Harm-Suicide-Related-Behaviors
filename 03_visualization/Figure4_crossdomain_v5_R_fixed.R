# Figure 4 (revised): cross-domain candidate differential edges
# This script is written to match the author's existing figure series:
# - Arial base font
# - navy/slate/grey/teal/orange palette
# - 183 mm width exports
# It reproduces the same content as the checked v5 figure.

suppressPackageStartupMessages({
  library(ggplot2)
  library(dplyr)
  library(patchwork)
  library(scales)
  library(grid)
})

`%||%` <- function(x, y) if (is.null(x) || length(x) == 0 || is.na(x)) y else x

figure_data_path <- Sys.getenv("FIGURE4_DATA_PATH", unset = "figure_data.csv")
if (!file.exists(figure_data_path)) {
  stop("Set FIGURE4_DATA_PATH to the approved Figure 4 source-data CSV; data are not included in this code package.")
}
df <- read.csv(figure_data_path, stringsAsFactors = FALSE, check.names = FALSE)

palette <- c(
  navy = "#123A63",
  slate = "#34495E",
  grey_1 = "#F8FAFB",
  grey_2 = "#E7ECEF",
  grey_3 = "#B9C3CC",
  grey_4 = "#74808A",
  ple = "#0F7C73",
  ple_soft = "#5ABCB2",
  si = "#F05A3C",
  si_soft = "#F08A78"
)
pal <- function(x) unname(palette[[x]])

theme_pub <- function(base_size = 7.2) {
  theme_classic(base_size = base_size, base_family = "Arial") +
    theme(
      axis.line = element_line(linewidth = 0.32, colour = pal("grey_3")),
      axis.ticks = element_line(linewidth = 0.28, colour = pal("grey_3")),
      axis.title = element_text(size = base_size, colour = pal("navy")),
      axis.text  = element_text(size = base_size - 0.45, colour = pal("slate")),
      plot.title = element_text(size = base_size + 0.45, face = "bold", colour = pal("navy"), margin = margin(b = 6)),
      plot.tag   = element_text(size = base_size + 1.8, face = "bold", colour = pal("navy")),
      plot.tag.position = c(0, 1),
      legend.title = element_text(size = base_size - 0.35, colour = pal("slate")),
      legend.text  = element_text(size = base_size - 0.7, colour = pal("slate")),
      panel.grid.minor = element_blank()
    )
}

save_pub <- function(plot, name, width_mm = 183, height_mm = 136, dpi = 600) {
  width_in  <- width_mm / 25.4
  height_in <- height_mm / 25.4
  ggplot2::ggsave(paste0(name, ".pdf"), plot, width = width_in, height = height_in,
                  device = cairo_pdf, family = "Arial")
  ggplot2::ggsave(paste0(name, ".svg"), plot, width = width_in, height = height_in,
                  device = svglite::svglite)
  ggplot2::ggsave(paste0(name, ".png"), plot, width = width_in, height = height_in,
                  device = ragg::agg_png, dpi = dpi, units = "in", scaling = 1)
  ggplot2::ggsave(paste0(name, ".tiff"), plot, width = width_in, height = height_in,
                  device = ragg::agg_tiff, dpi = dpi, units = "in", scaling = 1, compression = "lzw")
}

common_xlim <- c(-0.275, 0.275)
xticks <- c(-0.2, -0.1, 0, 0.1, 0.2)

make_contrast_panel <- function(dat, title_text, tag_text, y_pad = 8) {
  dat <- dat %>%
    mutate(
      edge = factor(edge, levels = rev(edge)),
      y_id = seq_along(edge)
    )
  bg <- dat %>% transmute(edge, xmin = common_xlim[1] + 0.006, xmax = common_xlim[2] - 0.006)
  vline_dat <- data.frame(x = xticks)

  ggplot(dat, aes(x = diff_female_minus_male, y = edge)) +
    geom_rect(data = bg,
              aes(xmin = xmin, xmax = xmax,
                  ymin = as.numeric(edge) - 0.25, ymax = as.numeric(edge) + 0.25),
              inherit.aes = FALSE, fill = pal("grey_2"), colour = NA) +
    geom_vline(data = vline_dat, aes(xintercept = x),
               colour = ifelse(vline_dat$x == 0, pal("grey_3"), pal("grey_2")),
               linewidth = ifelse(vline_dat$x == 0, 0.32, 0.28)) +
    geom_segment(aes(x = 0, xend = diff_female_minus_male, yend = edge,
                     colour = diff_female_minus_male > 0),
                 linewidth = 1.25, lineend = "round", show.legend = FALSE) +
    geom_point(aes(colour = diff_female_minus_male > 0),
               size = 2.6, stroke = 0.35, show.legend = FALSE) +
    geom_text(aes(label = sprintf("%+.3f", diff_female_minus_male),
                  hjust = ifelse(diff_female_minus_male >= 0, -0.15, 1.15)),
              family = "Arial", fontface = "bold", size = 2.65, colour = pal("navy")) +
    scale_colour_manual(values = c(`TRUE` = pal("ple"), `FALSE` = pal("si"))) +
    scale_x_continuous(limits = common_xlim, breaks = xticks, labels = number_format(accuracy = 0.1)) +
    labs(x = "Difference coefficient (girls − boys)", y = NULL, title = title_text, tag = tag_text) +
    coord_cartesian(clip = "off") +
    theme_pub() +
    theme(
      axis.line.y = element_blank(),
      axis.ticks.y = element_blank(),
      axis.text.y = element_text(margin = margin(r = y_pad)),
      panel.grid.major.x = element_blank(),
      panel.grid.major.y = element_blank(),
      plot.margin = margin(6, 8, 4, 6)
    )
}

t_dat <- df %>% filter(network == "Temporal")
c_dat <- df %>% filter(network == "Contemporaneous")

p_a <- make_contrast_panel(t_dat, "Temporal cross-domain edges", "a", y_pad = 8)
p_b <- make_contrast_panel(c_dat, "Contemporaneous cross-domain edges", "b", y_pad = 14)

heat_dat <- df %>%
  mutate(edge = factor(edge, levels = edge)) %>%
  tidyr::pivot_longer(cols = c(male_weight, female_weight), names_to = "sex", values_to = "weight") %>%
  mutate(
    sex = factor(sex, levels = c("male_weight", "female_weight"), labels = c("Boys", "Girls")),
    label = ifelse(abs(weight) < 0.0005, "0", sprintf("%+.3f", weight))
  )

p_c <- ggplot(heat_dat, aes(x = sex, y = edge, fill = weight)) +
  geom_tile(colour = pal("grey_2"), linewidth = 0.32) +
  geom_text(aes(label = label,
                colour = abs(weight) >= 0.10),
            family = "Arial", size = 2.55, fontface = ifelse(abs(heat_dat$weight) >= 0.10, "bold", "plain"),
            show.legend = FALSE) +
  geom_hline(yintercept = 3.5, colour = pal("grey_3"), linewidth = 0.38) +
  scale_fill_gradient2(low = pal("si_soft"), mid = "#FFFDFC", high = pal("ple_soft"),
                       midpoint = 0, limits = c(-0.25, 0.25),
                       breaks = c(-0.2, -0.1, 0, 0.1, 0.2),
                       guide = guide_colourbar(direction = "vertical", title.position = "top",
                                               barheight = unit(24, "mm"), barwidth = unit(3.2, "mm"))) +
  scale_colour_manual(values = c(`TRUE` = pal("navy"), `FALSE` = pal("navy"))) +
  labs(x = NULL, y = NULL, fill = "Edge weight",
       title = "Archived edge estimates by sex", tag = "c") +
  theme_pub() +
  theme(
    axis.line = element_blank(),
    axis.ticks = element_blank(),
    legend.position = "right",
    legend.justification = c(0.5, 0.5),
    legend.title.align = 0.5,
    plot.margin = margin(6, 6, 4, 6)
  )

layout <- "
AA
BC
"

final_plot <- p_a + p_b + p_c +
  plot_layout(design = layout, widths = c(0.92, 1.08), heights = c(0.80, 1.00))

save_pub(final_plot, "Figure4_crossdomain_v5_Rstyle", width_mm = 183, height_mm = 136)


