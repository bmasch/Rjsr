library(pdftools)
library(dplyr)
library(stringr)
library(jsonlite)
library(rvest)

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



html <- read_html("https://wdfw.wa.gov/fishing/management/columbia-river/compact/other-information")

p.elements <- html %>% html_elements("p")

Reports <- data.frame()

for(i in 1:length(p.elements)){
  p.text <- (html_text(p.elements[i]))
  if(grepl("Joint Staff Report:",p.text)){
    if(grepl("pring Chinook",p.text)){
      a.element <- html_elements(p.elements[i],"a")
      href <- html_attr(a.element,"href")
      if(substring(href,1,1)=="/")href <- paste0("https://wdfw.wa.gov",href)
      textlines <- p.text %>% strsplit(split="\n")
      Reports <- rbind(Reports,data.frame(report="Spring",title=textlines[[1]][2],href=href))
    }
    if(grepl("all Chinook Salmon",p.text)){
      a.element <- html_elements(p.elements[i],"a")
      href <- html_attr(a.element,"href")
      if(substring(href,1,1)=="/")href <- paste0("https://wdfw.wa.gov",href)
      textlines <- p.text %>% strsplit(split="\n")
      Reports <- rbind(Reports,data.frame(report="Fall",title=textlines[[1]][2],href=href))
    }
  }
}

Reports <- Reports %>%
  mutate(year= as.numeric(substring(title,1,4))) %>%
  select(year,report,title,href)

usethis::use_data(Reports, overwrite = TRUE)



