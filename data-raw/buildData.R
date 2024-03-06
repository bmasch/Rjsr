library(pdftools)
library(dplyr)
library(stringr)
library(jsonlite)

#set the data-raw folder as working directory, then run code below


spring.filename <- "2024-or-wa-spring-joint-staff-report.pdf"
fall.filename <- "2023-or-wa-fall-joint-staff-report-final-230710.pdf"

spring <- pdf_text(spring.filename) %>% strsplit(split="\n")
fall <- pdf_text(fall.filename) %>% strsplit(split="\n")

#spring.Table1 <- Rjsr::getTable(spring)

#usethis::use_data(DATA, overwrite = TRUE)

Current.Report <- list()
Current.Report$Spring <- list(URL=spring.filename,text=spring)
Current.Report$Fall <- list(URL=fall.filename,text=fall)

usethis::use_data(Current.Report, overwrite = TRUE)

Table.Config <- fromJSON("table.config.json")

usethis::use_data(Table.Config, overwrite = TRUE)

