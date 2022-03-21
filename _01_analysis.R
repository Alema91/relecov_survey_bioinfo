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
library(scales, quietly = TRUE, warn.conflicts = FALSE)
library(RColorBrewer, quietly = TRUE, warn.conflicts = FALSE)
library(forcats, quietly = TRUE, warn.conflicts = FALSE)

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
    geom_bar() +
    labs(x = "", y = "", title = "") +
    scale_fill_manual("Centros", values = c(
        "Hospital Joan XXIII" = "#E41A1C",
        "Hospital Universitari Vall d’Hebron" = "#E41A1C",
        "Hospital Dr. Josep Trueta" = "#E41A1C",
        "Hospital Clínic de Barcelona" = "#E41A1C",
        "Banc de Sang i Teixits Catalunya" = "#E41A1C",
        "Hospital Universitario La Paz" = "#377EB8",
        "Hospital Universitario Gregorio Marañón" = "#377EB8",
        "Hospital Universitario Ramón y Cajal" = "#377EB8",
        "Hospital Universitario 12 de Octubre" = "#377EB8",
        "Hospital de A Coruña" = "#4DAF4A",
        "Complejo Hospitalario de Santiago de Compostela" = "#4DAF4A",
        "Hospital General Universitario de Alicante" = "#984EA3",
        "FISABIO CSIC" = "#984EA3",
        "Hospital Universitario Son Espases" = "#FF7F00",
        "Instituto Tecnológico y de Energías Renovables" = "#FFFF33",
        "Hospital Universitario Marqués de Valdecilla" = "#A65628",
        "Hospital Universitario de Ciudad Real" = "#F781BF",
        "Fundación Rioja Salud" = "#999999"
    )) +
    theme(
        axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 6),
        axis.text.y = element_text(size = 7)
    )
ggsave("Graficos/survey_general.png")

### datos parsed bioinfo info ----

clases_expertise <- c(
    "Microbiology", "Genetics",
    "Biostatistics", "Microbiology", "Genetics", "Mathematics", "Biochemistry",
    "Microbiology", "Genetics", "Biochemistry",
    "System administration", "Biostatistics", "Data analysis/science", "Microbiology", "Genetics", "Informatics",
    "Microbiology", "Genetics",
    "Data analysis/science", "Microbiology", "Genetics",
    "Microbiology", "Genetics",
    "Biostatistics", "Microbiology", "Genetics", "Biochemistry",
    "Data analysis/science", "Genetics", "Informatics",
    "Biostatistics", "Data analysis/science", "Microbiology", "Genetics", "Informatics", "Evolutionary biology", "Cancer genomics", "Anatomic pathology", "Immunology",
    "Data analysis/science", "Microbiology", "Genetics",
    "Biostatistics", "Data analysis/science", "Microbiology", "Genetics",
    "Statistics", "Biostatistics", "Data analysis/science", "Microbiology",
    "Microbiology",
    "Biostatistics", "Data analysis/science", "Microbiology",
    "System administration", "Statistics", "Biostatistics", "Data analysis/science", "Microbiology", "Genetics", "Mathematics", "Informatics", "Human genetics",
    "Statistics", "Biostatistics", "Data analysis/science", "Microbiology", "Genetics", "Informatics",
    "System administration", "Statistics", "Biostatistics", "Data analysis/science", "Microbiology", "Genetics", "Informatics", "Biochemistry"
)

data_expertise <- data.frame(expertise = clases_expertise)

data_expertise_perc <- data_expertise %>%
    group_by(expertise) %>%
    summarise(count = n()) %>%
    mutate(perc = round((count / sum(count)) * 100, 3))

data_expertise_perc$expertise <- factor(data_expertise_perc$expertise, levels = c("Microbiology", "Genetics", "Data analysis/science", "Biostatistics", "Informatics", "Statistics", "Biochemistry", "System administration", "Mathematics", "Immunology", "Human genetics", "Evolutionary biology", "Cancer genomics", "Anatomic pathology"))

ggplot(data_expertise_perc, aes(expertise, perc)) +
    geom_bar(stat = "identity") +
    labs(x = "", y = "", title = "") +
    theme(
        axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 6),
        axis.text.y = element_text(size = 7)
    )
ggsave("Graficos/survey_expertises_percentage.png")

ggplot(data_expertise, aes(fct_infreq(expertise))) +
    geom_bar() +
    labs(x = "", y = "", title = "") +
    theme(
        axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 6),
        axis.text.y = element_text(size = 7)
    )
ggsave("Graficos/survey_expertises_count.png")