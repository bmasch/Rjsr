% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/scrapeTables.R
\name{getTable}
\alias{getTable}
\title{Finds a table from the specified report and returns a list}
\usage{
getTable(
  report = NA,
  year = NA,
  tablenum = NA,
  name.columns = TRUE,
  format.columns = TRUE
)
}
\arguments{
\item{report}{A string. Currently either Spring or Fall}

\item{year}{The year the report was generated. If NA, then current report is used}

\item{tablenum}{An integer identifier for the table (e.g. 1 for Table 1.)}

\item{name.columns}{Boolean.}

\item{format.columns}{Boolean}
}
\value{
A list of: title: report title ; body: main table with header ; notes: notes on columns
}
\description{
Finds a table from the specified report and returns a list
}
\examples{
getTable("Spring",titlenum=2)

}
