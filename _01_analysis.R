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

survey_general <- read_excel(dir_excel[2], sheet = 2)

### datos parsed general ----

level_comunidades <- c("CATALUÑA", "MADRID", "GALICIA", "CASTILLA LA MANCHA", "VALENCIA", "ARAGÓN", "CANARIAS", "BALEARES", "CANTABRIA", "ANDALUCÍA", "ASTURIAS", "LA RIOJA", "MURCIA", "CASTILLA Y LEON", "PAÍS VASCO", "EXTREMADURA")
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


ggplot(general_data, aes(x = comunidad, fill = centro)) +
    geom_bar() +
    labs(x = "", y = "", title = "") +
    theme(
        axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 6),
        axis.text.y = element_text(size = 7)
    )
ggsave("Graficos/survey_general.png")


# Bioinfo info

survey_bioinfo <- read_excel(dir_excel[2], sheet = 3)

bioinfo_data <- data.frame(
    id = as.character(sort(survey_bioinfo$ID)),
    q_1 = as.character(survey_bioinfo$`In which role are you replying to this questionnaire?`),
    q_2 = as.character(survey_bioinfo$`Does your organization have experience in the use of HTS/NGS technologies?`),
    q_3 = as.character(survey_bioinfo$`How much experience does your organization have in the use of HTS/NGS technologies?`),
    q_4 = as.character(survey_bioinfo$`Where do you work?`),
    q_5 = as.character(survey_bioinfo$`If you do not work in a bioinformatics unit, does your organization have one?`),
    q_6 = as.character(survey_bioinfo$`How many bioinformaticians work in your environment?`),
    q_7 = as.character(survey_bioinfo$`Which is your academic background?`),
    q_8 = as.character(survey_bioinfo$`Do you have a post graduate training in Bioinformatics? More than one option can be chosen.`),
    q_9 = as.character(survey_bioinfo$`Choose the area of expertise that you or other people in your team have`),
    q_10 = as.character(survey_bioinfo$`Do you use HTS in other organisims besides SARS-CoV-2? If so, in which organisim are you applying HTS technology?`)
)


# Q1

ggplot(bioinfo_data, aes(q_1)) +
    geom_bar(fill = "#1F77B4") +
    guides(fill = guide_legend(title = "Plataforma")) +
    labs(y = "Respuestas", x = "", title = "") +
    geom_text(stat = "count", position = position_stack(), aes(label = after_stat(count)), size = 10, vjust = 1) +
    theme(
        text = element_text(size = 45),
        axis.text.x = element_text(angle = 90, vjust = 1, hjust = 1)
    )
ggsave("Graficos/survey_bio_q1.png", width = 50, height = 40, dpi = 500, units = c("cm"))


# Q2
ggplot(bioinfo_data, aes(q_2)) +
    geom_bar(fill = "#1F77B4") +
    guides(fill = guide_legend(title = "Plataforma")) +
    labs(y = "Respuestas", x = "", title = "") +
    geom_text(stat = "count", position = position_stack(), aes(label = after_stat(count)), size = 10, vjust = 1) +
    theme(
        text = element_text(size = 45),
        axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)
    )
ggsave("Graficos/survey_bio_q2.png", width = 50, height = 30, dpi = 500, units = c("cm"))



# Q3
ggplot(bioinfo_data, aes(q_3)) +
    geom_bar(fill = "#1F77B4") +
    guides(fill = guide_legend(title = "Plataforma")) +
    labs(y = "Respuestas", x = "", title = "") +
    geom_text(stat = "count", position = position_stack(), aes(label = after_stat(count)), size = 10, vjust = 1) +
    theme(
        text = element_text(size = 45),
        axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)
    )
ggsave("Graficos/survey_bio_q3.png", width = 50, height = 30, dpi = 500, units = c("cm"))

# Q4
ggplot(bioinfo_data, aes(q_4)) +
    geom_bar(fill = "#1F77B4") +
    guides(fill = guide_legend(title = "Plataforma")) +
    labs(y = "Answers", x = "", title = "") +
    geom_text(stat = "count", position = position_stack(), aes(label = after_stat(count)), size = 10, vjust = 0.5) +
    theme(
        text = element_text(size = 35),
        axis.text.x = element_text(angle = 90, vjust = 1, hjust = 1)
    )
ggsave("Graficos/survey_bio_q4.png", width = 45, height = 35, dpi = 500, units = c("cm"))

# Q5
ggplot(bioinfo_data, aes(q_5)) +
    geom_bar(fill = "#1F77B4") +
    guides(fill = guide_legend(title = "Plataforma")) +
    labs(y = "Respuestas", x = "", title = "") +
    geom_text(stat = "count", position = position_stack(), aes(label = after_stat(count)), size = 10, vjust = 1) +
    theme(
        text = element_text(size = 22),
        axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)
    )
ggsave("Graficos/survey_bio_q5.png", width = 20, height = 30, dpi = 500, units = c("cm"))

# Q6
ggplot(bioinfo_data, aes(q_6)) +
    geom_bar(fill = "#1F77B4") +
    guides(fill = guide_legend(title = "Plataforma")) +
    labs(y = "Answers", x = "", title = "") +
    geom_text(stat = "count", position = position_stack(), aes(label = after_stat(count)), size = 5, vjust = -0.5, hjust = 0.5) +
    theme(
        text = element_text(size = 22),
        axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)
    )
ggsave("Graficos/survey_bio_q6.png")

# Q7
ggplot(bioinfo_data, aes(q_7)) +
    geom_bar(fill = "#1F77B4") +
    guides(fill = guide_legend(title = "Plataforma")) +
    labs(y = "Answers", x = "", title = "") +
    geom_text(stat = "count", position = position_stack(), aes(label = after_stat(count)), vjust = -0.5, hjust = 0.5) +
    theme(
        text = element_text(size = 18),
        axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)
    )
ggsave("Graficos/survey_bio_q7.png")

# Q8
ggplot(bioinfo_data, aes(q_8)) +
    geom_bar(fill = "#1F77B4") +
    guides(fill = guide_legend(title = "Plataforma")) +
    labs(y = "Respuestas", x = "", title = "") +
    geom_text(stat = "count", position = position_stack(), aes(label = after_stat(count)), vjust = -0.5, hjust = 0.5) +
    theme(
        text = element_text(size = 13),
        axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)
    )
ggsave("Graficos/survey_bio_q8.png")

# Q9

survey_bioinfo_2 <- read_excel(dir_excel[1], sheet = 4)

bioinfo_data_2 <- data.frame(
    q_9 = as.character(survey_bioinfo_2$`Choose the area of expertise that you or other people in your team have`),
    q_10 = as.character(survey_bioinfo_2$`Do you use HTS in other organisims besides SARS-CoV-2? If so, in which organisim are you applying HTS technology?`),
    q_11 = as.character(survey_bioinfo_2$`Which of the following NGS applications are you currently using?`)
)


# Q9 plot
ggplot(bioinfo_data_2, aes(q_9)) +
    geom_bar(fill = "#1F77B4") +
    guides(fill = guide_legend(title = "Plataforma")) +
    labs(y = "Respuestas", x = "", title = "") +
    geom_text(stat = "count", position = position_stack(), aes(label = after_stat(count)), vjust = -0.5, hjust = 0.5) +
    theme(
        text = element_text(size = 13),
        axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)
    )
ggsave("Graficos/survey_bio_q9.png")

# Q10 plot
ggplot(subset(bioinfo_data_2, q_10 != "NA"), aes(q_10)) +
    geom_bar(fill = "#1F77B4") +
    guides(fill = guide_legend(title = "Plataforma")) +
    labs(y = "Respuestas", x = "", title = "") +
    geom_text(stat = "count", position = position_stack(), aes(label = after_stat(count)), vjust = -0.5, hjust = 0.5) +
    theme(
        text = element_text(size = 18),
        axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)
    )
ggsave("Graficos/survey_bio_q10.png")

# Q11 plot
ggplot(subset(bioinfo_data_2, q_11 != "NA"), aes(q_11)) +
    coord_flip() +
    geom_bar(fill = "#1F77B4") +
    guides(fill = guide_legend(title = "Plataforma")) +
    labs(y = "Answers", x = "", title = "") +
    geom_text(stat = "count", position = position_stack(), aes(label = after_stat(count)), vjust = 0.5, hjust = -0.3) +
    theme(
        text = element_text(size = 18),
        axis.text.x = element_text()
    )
ggsave("Graficos/survey_bio_q11.png")

######### Sequencing info

survey_bioinfo_sequencing <- read_excel(dir_excel[1], sheet = 5)

sequencing_data <- data.frame(
    id = as.character(sort(survey_bioinfo_sequencing$ID)),
    q_11 = as.character(survey_bioinfo_sequencing$`Does your organization have a genomics unit?`),
    q_12 = as.character(survey_bioinfo_sequencing$`Does your lab have its own sequencing machines?`),
    q_13 = as.character(survey_bioinfo_sequencing$`Do you externalise sequencing?`),
    q_14 = as.character(survey_bioinfo_sequencing$`Which sequencing platforms does your lab/unit use?`)
)

# Q11

ggplot(sequencing_data, aes(q_11)) +
    geom_bar(fill = "#1F77B4") +
    guides(fill = guide_legend(title = "Plataforma")) +
    labs(y = "Respuestas", x = "", title = "") +
    geom_text(stat = "count", position = position_stack(), aes(label = after_stat(count)), vjust = -0.5, hjust = 0.5) +
    theme(
        text = element_text(size = 18),
        axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)
    )
ggsave("Graficos/survey_bio_q10.png")