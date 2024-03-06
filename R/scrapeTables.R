library(dplyr)
library(pdftools)
library(stringr)


#' Remove commas from a string representation of a number
#'
#' @param string_with_comma A string
#' @return A number
#' @examples unComma("24,000")
#' 
unComma <- function(valueStrings) {
  # Remove the commas
  # Convert to numeric, handling negative values represented by parentheses
  suppressWarnings({
    numericValues <- sapply(valueStrings, function(x) {
      x <- gsub(",", "", x)
      if (grepl("\\(", x)) {
        # Remove parentheses and convert to negative numeric value
        return(-as.numeric(gsub("[()]", "", x)))
      }
      else if(is.na(x)){
        return(0)
      }
      else {
        # Convert positive values directly to numeric
        return(as.numeric(x))
      }
    })
  })
  
  return(unname(numericValues))
}

#' Remove percent sign from a string representation of a number
#'
#' @param string_with_percent A string
#' @return A number
#' @examples unPct("24%")
#' 
unPct <- function(string_with_percent){
  suppressWarnings({
    value=(as.numeric(gsub('\\%','',string_with_percent)))
    value <- ifelse(is.na(value),0,value)
    #value <- unComma(value)
  })
  return(value)  
}

#jc <- pdf_text("2024-or-wa-spring-joint-staff-report.pdf")
#jc <- jc %>% strsplit(split="\n")

#' Finds a table in the extracted text from a pdf. Internal use
#'
#' @param p A character vector representation of a pdf document output from pdf_text()
#' @param skip A number, the number of lines to skip before assigning the first row of data
#' @param ncols The number of columns in the table
#' @param minchar The minimum number of spaces needed for a break between columns
#' @return A list
#' @examples getTable_(pdf_text("MyPDF.pdf") %>% strsplit(split="\n"),title.text="Table 2.",ncols=13)
#' 
getTable_ <- function(p,title.text="Table 1.",skip=3,ncols=9,minchar=2){
  tnum <- 0
  for(i in 1:length(p)){
    if(substring(p[[i]][1],1,8) == title.text){
      tnum <- i
    }
  }
  table <- trimws(p[[tnum]])
  startnum <- -1
  endnum <- 0 
  startyear <- 3000
  suppressWarnings({
    for(j in skip:length(table)){
      line <- table[j]
      year <- substring(line,1,4)
      year <- as.numeric(year)
      if(!is.na(year)){
        if(startnum == -1){
          startnum <- j
          startyear <- year
        }
        else{
          if(year > startyear)endnum <- j
        }
      }
    }
  })
  
  table.title <- table[1]
  table.rows <- table[startnum:endnum]
  sep <- paste0(" {",minchar,",}")
  table.rows <- str_split_fixed(table.rows,sep, ncols) %>% data.frame()
  table.notes <- table[endnum+1:length(table)]
  return(list(title=table.title,body=table.rows,notes=table.notes))
}

#' Finds a table from the specified report and returns a list
#'
#' @param report A string. Currently either Spring or Fall
#' @param tablenum An integer identifier for the table (e.g. 1 for Table 1.)
#' @param year The year the report was generated. If NA, then current report is used
#' @param name.columns Boolean. 
#' @param format.columns Boolean
#' @return A list of: title: report title ; body: main table with header ; notes: notes on columns 
#' @export
#' @examples getTable("Spring",titlenum=2)
#' 
getTable <- function(report="Spring",tablenum=1,year=NA,name.columns=TRUE,format.columns=TRUE){
  if(is.na(year)){
    table.text <- paste("Table ",tablenum, ".",sep="")
    table.config <- Table.Config[[report]]$tables[[table.text]]
    ncolumns <- table.config$ncols
    columntypes <- table.config$columntypes
    table <- getTable_(Current.Report[[report]]$text,table.text,ncols=ncolumns)
    if(name.columns==TRUE)names(table$body) <- table.config$columnnames
    if(format.columns==TRUE){
      table$body <- table$body %>% mutate(across(which(table.config$columntypes=="numeric"),unComma))
      table$body <- table$body %>% mutate(across(which(table.config$columntypes=="pct"),unPct))
    }
    
    return(table)
  }
}