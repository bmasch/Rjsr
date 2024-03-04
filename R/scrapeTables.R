library(tidyverse)
library(pdftools)
library(stringr)

unComma <- function(commaValue) {
  # Remove commas
  cleanedStrings <- gsub(",", "", commaValue)
  return(cleanedStrings)
}

unPct <- function(amt){
  #removes % sign
  #return(gsub('\u002c|\u0024','',amt))
  value=(as.numeric(gsub('\\%','',amt)))
  value <- ifelse(is.na(value),0,value)
  value <- unComma(value)
  return(value)  
}

#jc <- pdf_text("2024-or-wa-spring-joint-staff-report.pdf")
#jc <- jc %>% strsplit(split="\n")

getTable2 <- function(p){
  tnum <- 0
  for(i in 1:length(p)){
    if(substring(p[[i]][1],1,8) == "Table 2."){
      tnum <- i
    }
  }
  table <- trimws(p[[tnum]])
  startnum <- -1
  endnum <- 0 
  startyear <- 3000
  for(j in 3:length(table)){
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
  table <- table[startnum:endnum]
  print(trimws(table[startnum]))
  print(trimws(table[endnum]))
  table <- str_split_fixed(table, " {2,}", 13)
  table <- data.frame(table) %>% select(1,11,12)
  names(table) <- c("year","forecast","actual")
  table <- table %>% mutate(year=as.numeric(year),forecast=1000*as.numeric(forecast),actual=1000*as.numeric(actual))
  return(table)
}

getTable5 <- function(p){
  tnum <- 0
  for(i in 1:length(p)){
    if(substring(p[[i]][1],1,8) == "Table 5."){
      tnum <- i
    }
  }
  table <- trimws(p[[tnum]])
  startnum <- -1
  endnum <- 0 
  startyear <- 3000
  for(j in 3:length(table)){
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
  table <- table[startnum:endnum]
  table <- str_split_fixed(table, " {2,}", 15)
  #table <- data.frame(table) %>% select(1,11,12)
  #names(table) <- c("year","forecast","actual")
  #table <- table %>% mutate(year=as.numeric(year),forecast=1000*as.numeric(forecast),actual=1000*as.numeric(actual))
  return(table)
}

getTable <- function(p,skip=3,title.text="Table 1.",ncols=NA,minchar=2){
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
  table <- table[startnum:endnum]
  print(trimws(table[startnum]))
  print(trimws(table[endnum]))
  sep <- paste0(" {",minchar,",}")
  table <- str_split_fixed(table,sep, ncols)
  return(table)
}