#!/usr/bin/env Rscript

# library -------

library(plyr, quietly = TRUE, warn.conflicts = FALSE)
library(dplyr, quietly = TRUE, warn.conflicts = FALSE)
library(tidyr, quietly = TRUE, warn.conflicts = FALSE)
library(tibble, quietly = TRUE, warn.conflicts = FALSE)
library(readxl, quietly = TRUE, warn.conflicts = FALSE)
library(ggplot2, quietly = TRUE, warn.conflicts = FALSE)
library(janitor, quietly = TRUE, warn.conflicts = FALSE)
library(tibble, quietly = TRUE, warn.conflicts = FALSE)

### Excel ----

dir_excel <- list.files(path = "Data", pattern = "xlsx", full.names = TRUE, recursive = TRUE, include.dirs = FALSE)

### datos general ----

survey_general <- read_excel(dir_excel[1], sheet = 2)

### datos parsed general ----

general_data <- data.frame(
    id = as.character(survey_general$`Relecov-ID`),
    comunidad = as.character(survey_general$`Autonomous community`),
    centro = as.character(survey_general$`Center name`),
    fecha = excel_numeric_to_date(as.numeric(survey_general$fecha))
)

#### plot tiempo de ejecucion ----

ggplot(fechas_data, aes(
    x = recepcion, xend = secuenciacion,
    y = id, yend = id, color = "#4a8abe"
)) +
    geom_segment(size = 3, show.legend = F) +
    geom_text(aes(label = fechas_data$ejecucion), position = position_dodge(width = 1), hjust = -1, color = "black", size = 2.5) +
    labs(x = "Execution time (days)", y = "")
ggsave("Graficos/qc_tiempo_ejecucion.png")