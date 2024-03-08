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

Current.Report <- list()
Current.Report$Spring <- list(URL=spring.filename,text=spring,year=2024)
Current.Report$Fall <- list(URL=fall.filename,text=fall,year=2023)

usethis::use_data(Current.Report, overwrite = TRUE)

Table.Config <- fromJSON("table.config.json")

usethis::use_data(Table.Config, overwrite = TRUE)


#########   get from WDFW website

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

######### get from ODFW website

#ODFW URL: https://www.dfw.state.or.us/fish/oscrp/crm/joint_staff_reports_archive.asp

html <- read_html("https://www.dfw.state.or.us/fish/oscrp/crm/joint_staff_reports_archive.asp")

a.elements <- html %>% html_elements("a")

Reports <- data.frame()

for(i in 1:length(a.elements)){
  a.text <- (html_text(a.elements[i]))
  if(grepl("Stock Status and Fisheries",a.text)){
    if(grepl("Spring Chinook",a.text)){
      href <- html_attr(a.elements[i],"href")
      Reports <- rbind(Reports,data.frame(report="Spring",title=a.text,href=href))
    }
    if(grepl("Fall Chinook",a.text)){
      href <- html_attr(a.elements[i],"href")
      Reports <- rbind(Reports,data.frame(report="Fall",title=a.text,href=href))
    }
  }
}

Reports <- Reports %>%
  mutate(year= as.numeric(substring(title,1,4))) %>%
  select(year,report,title,href)

usethis::use_data(Reports, overwrite = TRUE)



