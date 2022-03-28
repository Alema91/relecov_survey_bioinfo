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
library(scales, quietly = TRUE, warn.conflicts = FALSE)

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
    "Hospital Universitari Germans Trias i Pujol",
    "Hospital Universitario La Paz",
    "Hospital Universitario Gregorio Marañón",
    "Hospital Universitario Ramón y Cajal",
    "Hospital Universitario 12 de Octubre",
    "Hospital de A Coruña",
    "Complejo Hospitalario de Santiago de Compostela",
    "Hospital de Vigo",
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
        "Hospital Universitari Germans Trias i Pujol" = "#E41A1C",
        "Hospital Universitario La Paz" = "#377EB8",
        "Hospital Universitario Gregorio Marañón" = "#377EB8",
        "Hospital Universitario Ramón y Cajal" = "#377EB8",
        "Hospital Universitario 12 de Octubre" = "#377EB8",
        "Hospital de A Coruña" = "#4DAF4A",
        "Complejo Hospitalario de Santiago de Compostela" = "#4DAF4A",
        "Hospital de Vigo" = "#4DAF4A",
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

str(general_data)
str(df)

df <- data.frame(
    group = c("Male", "Female", "Child"),
    value = c(25, 25, 50)
)


ggplot(df, aes(x = "", y = value, fill = group)) +
    geom_bar(width = 1, stat = "identity") +
    coord_polar("y", start = 0)

# Bioinfo info

survey_bioinfo <- read_excel(dir_excel[1], sheet = 3)

bioinfo_data <- data.frame(
    id = as.character(sort(survey_bioinfo$`Relecov-ID`)),
    q_1 = as.character(survey_bioinfo$`In which role are you replying to this questionnaire?`),
    q_2 = as.character(survey_bioinfo$`Does your organization have experience in the use of HTS/NGS technologies?`),
    q_3 = as.character(survey_bioinfo$`How much experience does your organization have in the use of HTS/NGS technologies?`),
    q_4 = as.character(survey_bioinfo$`Where do you work?`),
    q_5 = as.character(survey_bioinfo$`If you do not work in a bioinformatics unit, does your organization have one?`)
)

# Q1

bioinfo_data_q1 <- bioinfo_data %>%
    group_by(q_1) %>%
    summarise(count = n())

colnames(bioinfo_data_q1) <- c("question_1_1", "value")

ggplot(bioinfo_data_q1, aes(x = "", y = value, fill = question_1_1)) +
    geom_col(color = "black") +
    guides(color = guide_legend(title = "Samples"), fill = guide_legend(title = "Answers")) +
    geom_text(aes(label = value),
        position = position_stack(vjust = 0.6)
    ) +
    coord_polar(theta = "y") +
    labs(y = "", x = "", title = "In which role are you replying to this questionnaire?") +
    scale_fill_brewer(palette = "Spectral")
ggsave("Graficos/qc_survey_bioinfo_1.png")

# Q2

bioinfo_data_q2 <- bioinfo_data %>%
    group_by(q_2) %>%
    summarise(count = n())

colnames(bioinfo_data_q2) <- c("question_1_2", "value")

ggplot(bioinfo_data_q2, aes(x = "", y = value, fill = question_1_2)) +
    geom_col(color = "black") +
    guides(fill = guide_legend(title = "Answers")) +
    geom_text(aes(label = value),
        position = position_stack(vjust = 0.6)
    ) +
    coord_polar(theta = "y") +
    labs(y = "", x = "", title = "Does your organization have experience in the use of HTS/NGS technologies?") +
    scale_fill_brewer(palette = "Spectral")
ggsave("Graficos/qc_survey_bioinfo_2.jpeg")

# Q3

bioinfo_data_q3 <- bioinfo_data %>%
    group_by(q_3) %>%
    summarise(count = n())

colnames(bioinfo_data_q2) <- c("question_1_3", "value")

ggplot(bioinfo_data_q3, aes(x = "", y = value, fill = question_1_3)) +
    geom_col(color = "black") +
    guides(fill = guide_legend(title = "Answers")) +
    geom_text(aes(label = value),
        position = position_stack(vjust = 0.6)
    ) +
    coord_polar(theta = "y") +
    labs(y = "", x = "", title = "How much experience does your organization have in the use of HTS/NGS technologies?") +
    scale_fill_brewer(palette = "Spectral")
ggsave("Graficos/qc_survey_bioinfo_3.jpeg")

# Q4

bioinfo_data_q4 <- bioinfo_data %>%
    group_by(q_4) %>%
    summarise(count = n())

colnames(bioinfo_data_q4) <- c("question_1_4", "value")

ggplot(bioinfo_data_q4, aes(x = "", y = value, fill = question_1_4)) +
    geom_col(color = "black") +
    guides(fill = guide_legend(title = "Answers")) +
    geom_text(aes(label = value),
        position = position_stack(vjust = 0.6)
    ) +
    coord_polar(theta = "y") +
    labs(y = "", x = "", title = "Where do you work?") +
    scale_fill_brewer(palette = "Spectral")
ggsave("Graficos/qc_survey_bioinfo_4.jpeg")

# Q4

bioinfo_data_q5 <- bioinfo_data %>%
    group_by(q_5) %>%
    summarise(count = n())

colnames(bioinfo_data_q5) <- c("question_1_5", "value")

ggplot(bioinfo_data_q5, aes(x = "", y = value, fill = question_1_5)) +
    geom_col(color = "black") +
    guides(fill = guide_legend(title = "Answers")) +
    geom_text(aes(label = value),
        position = position_stack(vjust = 0.6)
    ) +
    coord_polar(theta = "y") +
    labs(y = "", x = "", title = "Where do you work?") +
    scale_fill_brewer(palette = "Spectral")
ggsave("Graficos/qc_survey_bioinfo_5.jpeg")


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