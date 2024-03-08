library(pdftools)

#' Fetches the report from the WDFW website and parses it. If url is NA, the current is used
#'
#' @param report String, e.g. "Spring" or "Fall"
#' @param year Number. If NA
#' @param url String. The url for the page to be scraped
#' @return list
#' @export
#' @examples getReport())
#' 
getReport <- function(requested.report=NA,requested.year=NA,requested.url=NA){
  if(is.na(requested.report))requested.report <- "Spring"
  if(is.na(requested.year)){
    rpt <- Current.Report[[requested.report]]
    return(list(URL=rpt$URL,text=rpt$text,year=rpt$year))
  }
  else{
    #check whether requested report year matches the current one
    if(Current.Report[[requested.report]]$year == requested.year){
      rpt <- Current.Report[[requested.report]]
      return(list(URL=rpt$URL,text=rpt$text,year=rpt$year))
    }
    else{
      #if url has been passed, get report there
      if(!is.na(requested.url)){
        requested <= list(URL=requested.url,year=requested.year)
      }
      else{
        #otherwise, get list of available reports
        reports.df <- getReportList()
        requested <- reports.df %>% filter(report==requested.report,year==requested.year)
        if(nrow(requested) == 0)return(NULL)
        else{
          requested <- list(URL=requested$href,year=requested.year)
        }
      }
      #actually get and parse the report
      #might need to check if exists, or actually works
      requested$text <- pdf_text(requested$URL) %>% strsplit(split="\n")
      return(requested)
    }
  }
}