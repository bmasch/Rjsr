library(rvest)


#' Fetches a page from the WDFW website and scrapes available report links
#'
#' @param url String. The url for the page to be scraped
#' @return dataframe
#' @export
#' @examples getReportList())
#' 
getReportList <- function(url="https://wdfw.wa.gov/fishing/management/columbia-river/compact/other-information"){
  
  html <- read_html(url)
  
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
  
  return(Reports)
}