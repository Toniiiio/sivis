% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sivis.R
\name{get_multicol_candidates}
\alias{get_multicol_candidates}
\title{Create candidates for additional columns / variables}
\usage{
get_multicol_candidates(response_string, xp1, extract_pathes)
}
\arguments{
\item{response_string}{An string with html content}

\item{xp1}{}

\item{extract_pathes}{The target data can be nested in multiple data formats. For example a json containg html code.
To build and dynamically update the scraper all extraction steps have to be available and in the correct order.
Therefore, this extraction steps will be stored in this variable: extract_pathes and updated in the corresponding
extraction functions.}
}
\value{
Candidates for additional columns
}
\description{
Relevant data might be spread across multiple columns. Selecting all data at once by the user in the browser might yield difficulties for the extraction. Manually label each column / variable might get cubersome for the user.
Another possibility is to suggest additional variables / columns to the user.
Goal: Find neighbour columns but avoid displaying other data, that is not a neighbour column. \cr \cr
}
\details{
This function generates candidates that will be filtered in \code{\link{add_multi_cols}}.
}
\examples{
data("doc_html")
doc <- xml2::read_html(doc_html)
xpath <- "/html/body/table/tr/td[1]"
tags <- rvest::html_nodes(doc, xpath = xpath)
get_multicol_candidates(doc_html, xpath, list())
}
