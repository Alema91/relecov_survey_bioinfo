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

level_comunidades <- c("CATALUÑA", "MADRID", "GALICIA", "VALENCIA", "BALEARES", "CANARIAS", "CANTABRIA", "CASTILLA LA MANCHA", "LA RIOJA")
level_centros <- c(
    "Hospital Joan XXIII",
    "Hospital Universitari Vall d’Hebron",
    "Hospital Dr. Josep Trueta",
    "Hospital Clínic de Barcelona",
    "Banc de Sang i Teixits Catalunya",
    "Hospital Universitario La Paz",
    "Hospital Universitario Gregorio Marañón",
    "Hospital Universitario Ramón y Cajal",
    "Hospital Universitario 12 de Octubre",
    "Hospital de A Coruña",
    "Complejo Hospitalario de Santiago de Compostela",
    "Hospital General Universitario de Alicante",
    "FISABIO CSIC",
    "Hospital Universitario Son Espases",
    "Instituto Tecnológico y de Energías Renovables",
    "Hospital Universitario Marqués de Valdecilla",
    "Hospital Universitario de Ciudad Real",
    "Fundación Rioja Salud"
)

general_data <- data.frame(
    id = as.character(sort(survey_general$`Relecov-ID`)),
    comunidad = factor(survey_general$`Autonomous community`, levels = level_comunidades),
    centro = factor(survey_general$`Center name`, levels = level_centros),
    fecha = excel_numeric_to_date(as.numeric(survey_general$fecha))
)

#### plot general ----

ggplot(general_data, aes(x = comunidad, fill = centro)) +
    geom_bar(aes(fill = centro)) +
    labs(x = "", y = "", title = "") +
    theme(
        axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 6),
        axis.text.y = element_text(size = 7)
    )
ggsave("Graficos/survey_general.png")