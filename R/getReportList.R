library(rvest)


#' Fetches a page from the WDFW website and scrapes available report links
#'
#' @param url String. The url for the page to be scraped
#' @return dataframe
#' @export
#' @examples getReportList())
#' 
getReportList <- function(url="https://wdfw.wa.gov/fishing/management/columbia-river/compact/other-information"){
  
  #ODFW URL: https://www.dfw.state.or.us/fish/oscrp/crm/joint_staff_reports_archive.asp
  
  html <- read_html(url)
  
  p.elements <- html %>% html_elements("p")
  
  reports <- data.frame()
  
  for(i in 1:length(p.elements)){
    p.text <- (html_text(p.elements[i]))
    if(grepl("Joint Staff Report:",p.text)){
      if(grepl("pring Chinook",p.text)){
        a.element <- html_elements(p.elements[i],"a")
        href <- html_attr(a.element,"href")
        if(substring(href,1,1)=="/")href <- paste0("https://wdfw.wa.gov",href)
        textlines <- p.text %>% strsplit(split="\n")
        reports <- rbind(reports,data.frame(report="Spring",title=textlines[[1]][2],href=href))
      }
      if(grepl("all Chinook Salmon",p.text)){
        a.element <- html_elements(p.elements[i],"a")
        href <- html_attr(a.element,"href")
        if(substring(href,1,1)=="/")href <- paste0("https://wdfw.wa.gov",href)
        textlines <- p.text %>% strsplit(split="\n")
        reports <- rbind(reports,data.frame(report="Fall",title=textlines[[1]][2],href=href))
      }
    }
  }
  
  reports <- reports %>%
    mutate(year= as.numeric(substring(title,1,4))) %>%
    select(year,report,title,href)
  
  return(reports)
}