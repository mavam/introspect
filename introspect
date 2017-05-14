#!/usr/bin/env Rscript --vanilla

suppressPackageStartupMessages(library(cowplot))
suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(ggplot2))
suppressPackageStartupMessages(library(optparse))
suppressPackageStartupMessages(library(readr))
suppressPackageStartupMessages(library(scales))
suppressPackageStartupMessages(library(tidyr))

# -- globals -------------------------------------------------------------------

# Reference interval describing normal values. The first two values represents
# minimum and maximum, and the third value the unit.
# (list names must correspond to column names in data files)
reference <- list(
  # CBC
  WBC = list(3, 10, "10^3/mu*l"),
  RBC = list(4.1, 5.9, "10^6/mu*l"),
  Hemoglobin = list(13, 18, "g/dl"),
  Hematocrit = list(37.5, 51, "\"%\""),
  MCV = list(79, 97, "fl"),
  MCH = list(26.6, 33.0, "fl"),
  MCHC = list(31.5, 35.7, "pg"),
  RDW = list(12.3, 15.4, "\"%\""),
  Platelets = list(150, 379, "10^3/mu*l"),
  # CMP
  Glucose = list(65, 99, "mg/dl"),
  BUN = list(6, 20, "mg/dl"),
  Creatinine = list(0.76, 1.27, "mg/dl"),
  eGFR = list(59, NA, "ml/min/1.73"),
  `BUN/Creatinine` = list(9, 20, ""),
  Sodium = list(134, 144, "mmol/l"),
  Potassium = list(3.5, 5.2, "mmol/l"),
  Chloride = list(96, 106, "mmol/l"),
  CO2 = list(18, 29, "mmol/l"),
  Calcium = list(8.7, 10.2, "mg/l"),
  Protein = list(6.0, 8.5, "g/l"),
  Albumin = list(3.5, 5.5, "g/l"),
  Globulin = list(1.5, 4.5, "g/l"),
  `A/G` = list(1.2, 2.2, ""),
  Bilirubin = list(0.0, 1.2, "mg/l"),
  ALP = list(39, 177, "IU/l"),
  AST = list(0, 40, "IU/l"),
  ALT = list(0, 44, "IU/l"),
  # Lipid
  Cholesterol = list(100, 199, "mg/dl"),
  HDL = list(39, NA, "mg/dl"),
  VLDL = list(5, 40, "mg/dl"),
  LDL = list(0, 99, "mg/dl"),
  Triglycerides = list(0, 149, "mg/dl")
)

# ------------------------------------------------------------------------------

# Make sure plots look fine when presented next to each other on a larger page.
theme_set(theme_minimal() +
  theme(
    plot.margin = margin(0, 0, 0, 0),
    plot.title = element_text(size = 10, hjust = 0.5,
                              margin = margin(t = 5, b = 0)),
    text = element_text(size = 8),
    legend.text = element_text(size = 6),
    axis.text = element_text(size = 6),
    axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1, debug = FALSE),
    axis.ticks.x = element_line(size = 0.25),
    axis.title.x = element_blank(), # always a date
    axis.title.y = element_text(margin = margin())
  )
)

# Override previously set defaults.
ggdraw_theme <- theme(
  axis.text.x = element_blank(),
  axis.text.y = element_blank(),
  axis.ticks.x = element_blank()
)

date_display_format <- "%b %y"

# ------------------------------------------------------------------------------

# Plots a specific lab value and highlights outliers.
plot.value <- function(data, aspect) {
  ref <- reference[[aspect]]
  if (is.null(ref))
    stop("missing reference data")
  # Figure out whether we have a one-sided or two-sided reference interval.
  filt_expr <- NULL
  ref_min <- min(data[[aspect]], na.rm = TRUE)
  ref_max <- max(data[[aspect]], na.rm = TRUE)
  #filt_expr <- paste(aspect, '<', ref[[1]], '|', aspect, '>', ref[[2]])
  quoted_aspect = paste0('`', aspect, '`')
  if (!is.na(ref[[1]])) {
    filt_expr <- c(paste(quoted_aspect, '<', ref[[1]]))
    ref_min <- ref[[1]]
  }
  if (!is.na(ref[[2]])) {
    filt_expr <- c(filt_expr, paste(quoted_aspect, '>', ref[[2]]))
    ref_max <- ref[[2]]
  }
  filt_expr <- do.call(paste, c(as.list(filt_expr), sep = " | "))
  outliers <- data %>% filter_(filt_expr)
  data %>%
    drop_na_(aspect) %>%
    ggplot(aes_string(x = "Date", y = quoted_aspect)) +
      geom_rect(aes(xmin = min(data$Date), xmax = max(data$Date),
                    ymin = ref_min, ymax = ref_max),
                alpha = 0.03, fill = "forestgreen") +
      geom_line() +
      geom_point() +
      geom_point(data = outliers, color = "red", size = 2) +
      scale_x_date(labels = date_format(date_display_format)) +
      theme(axis.title.y = element_text(margin = margin())) +
      ylab(parse(text = ref[[3]])) +
      ggtitle(aspect)
}

# Plots white blood cell composition.
plot.wbc.diff <- function(data) {
  data %>%
    select(Date, Neutrophils, Lymphs, Monocytes, Eos, Basos) %>%
    gather(key, value, -Date) %>%
    ggplot(aes(x = Date, y = value, fill = key)) +
      geom_area(position = "stack") +
      ylab("%") +
      scale_x_date(labels = date_format(date_display_format)) +
      theme(legend.title = element_blank()) +
      theme(axis.title.y = element_text(margin = margin())) +
      ggtitle("WBC Differential")
}

# Plots a Complete Blood Count (CBC) panel with differential.
plot.cbc <- function(filename) {
  cbc <- read_table2(filename, progress = FALSE)
  height <- 0.25
  ggdraw() +
    draw_plot(plot.value(cbc, "WBC"), 0, 0.75, 0.33, height) +
    draw_plot(plot.wbc.diff(cbc), 0.33, 0.75, 0.66, height) +
    draw_plot(plot.value(cbc, "RBC"), 0, 0.5, 0.33, height) +
    draw_plot(plot.value(cbc, "Hemoglobin"), 0.33, 0.5, 0.33, height) +
    draw_plot(plot.value(cbc, "Hematocrit"), 0.66, 0.5, 0.33, height) +
    draw_plot(plot.value(cbc, "MCV"), 0, 0.25, 0.25, height) +
    draw_plot(plot.value(cbc, "MCH"), 0.25, 0.25, 0.25, height) +
    draw_plot(plot.value(cbc, "MCHC"), 0.5, 0.25, 0.25, height) +
    draw_plot(plot.value(cbc, "RDW"), 0.75, 0.25, 0.25, height) +
    draw_plot(plot.value(cbc, "Platelets"), 0.1, 0, 0.8, height) +
    ggdraw_theme
}

# Plots a Complete Metabolic Panel (CMP).
plot.cmp <- function(filename) {
  cmp <- read_table2(filename, progress = FALSE)
  num_rows <- 5
  height <- 1 / num_rows
  y <- function(x) { (num_rows - x) / num_rows }
  ggdraw() +
    draw_plot(plot.value(cmp, "Glucose"), 0, y(1), 0.5, height) +
    draw_plot(plot.value(cmp, "Calcium"), 0.5, y(1), 0.5, height) +
    draw_plot(plot.value(cmp, "Sodium"), 0, y(2), 0.25, height) +
    draw_plot(plot.value(cmp, "Potassium"), 0.25, y(2), 0.25, height) +
    draw_plot(plot.value(cmp, "Chloride"), 0.5, y(2), 0.25, height) +
    draw_plot(plot.value(cmp, "CO2"), 0.75, y(2), 0.25, height) +
    draw_plot(plot.value(cmp, "BUN"), 0, y(3), 0.25, height) +
    draw_plot(plot.value(cmp, "Creatinine"), 0.25, y(3), 0.25, height) +
    draw_plot(plot.value(cmp, "BUN/Creatinine"), 0.5, y(3), 0.25, height) +
    draw_plot(plot.value(cmp, "eGFR"), 0.75, y(3), 0.25, height) +
    draw_plot(plot.value(cmp, "ALT"), 0, y(4), 0.25, height) +
    draw_plot(plot.value(cmp, "AST"), 0.25, y(4), 0.25, height) +
    draw_plot(plot.value(cmp, "ALP"), 0.5, y(4), 0.25, height) +
    draw_plot(plot.value(cmp, "Bilirubin"), 0.75, y(4), 0.25, height) +
    draw_plot(plot.value(cmp, "Protein"), 0, y(5), 0.25, height) +
    draw_plot(plot.value(cmp, "Albumin"), 0.25, y(5), 0.25, height) +
    draw_plot(plot.value(cmp, "Globulin"), 0.5, y(5), 0.25, height) +
    draw_plot(plot.value(cmp, "A/G"), 0.75, y(5), 0.25, height) +
    ggdraw_theme
}

# Plots cholesterol as stacked area.
plot.cholesterol <- function(data) {
  data %>%
    select(Date, HDL, VLDL, LDL) %>%
    gather(key, value, -Date) %>%
    ggplot(aes(x = Date, y = value,
               fill =factor(key, levels = c("HDL", "VLDL", "LDL")))) +
      geom_area(position = "stack") +
      #geom_hline(aes(yintercept = 199), color = "red", linetype = "dashed") +
      ylab("mg/dl") +
      scale_x_date(labels = date_format(date_display_format)) +
      theme(legend.title = element_blank()) +
      theme(axis.title.y = element_text(margin = margin())) +
      ggtitle("Cholesterol")
}

# Plots the entire lipid panel
plot.lipid <- function(filename) {
  lipid <- read_table2(filename, progress = FALSE)
  height <- 0.33
  ggdraw() +
    draw_plot(plot.value(lipid, "Cholesterol"), 0, 0.66, 0.33, height) +
    draw_plot(plot.cholesterol(lipid), 0.33, 0.66, 0.66, height) +
    draw_plot(plot.value(lipid, "HDL"), 0, 0.33, 0.33, height) +
    draw_plot(plot.value(lipid, "VLDL"), 0.33, 0.33, 0.33, height) +
    draw_plot(plot.value(lipid, "LDL"), 0.66, 0.33, 0.33, height) +
    draw_plot(plot.value(lipid, "Triglycerides"), 0.1, 0, 0.8, height) +
    ggdraw_theme
}

# -- main ----------------------------------------------------------------------

option_list <- list(
  make_option(c("-b", "--bw"), action = "store_true", default = TRUE,
              help = "generate black and white plots suitable for printing"),
  make_option(c("-H", "--height"), type = "double", default = 11,
              help = "height of the output plot [%default]"),
  make_option(c("-W", "--width"), type = "double", default = 8.5,
              help = "width of the output plot [%default]"),
  make_option(c("-u", "--unit"), default = "in",
              help = "the unit for width and height [%default]"),
  make_option(c("-f", "--format"), default = "pdf",
              help = "the plot output format [%default]"),
  make_option(c("-i", "--input-dir"), default = file.path(getwd(), "input"),
              help = "the path to input data [%default]"),
  make_option(c("-o", "--output-dir"), default = file.path(getwd(), "output"),
              help = "the path to input data [%default]"),
  make_option(c("-h", "--help"), action = "store_true", default = F,
              help = "display this help and exit")
)

opts <- parse_args(OptionParser(option_list = option_list,
                                add_help_option = FALSE))

input <- function(x) file.path(opts$`input-dir`, paste0(x, ".txt"))
output <- function(x) file.path(opts$`output-dir`, paste0(x, ".", opts$format))
generate <- function(name, plot_fun) {
  if (file.exists(input(name))) {
    message("generating ", opts$format, " for ", input(name))
    if (!dir.exists(opts$`output-dir`))
      dir.create(opts$`output-dir`)
    ggsave(output(name), plot_fun(input(name)),
           height = opts$height, width = opts$width, units = opts$unit)
  }
}
generate("cbc", plot.cbc)
generate("cmp", plot.cmp)
generate("lipid", plot.lipid)
