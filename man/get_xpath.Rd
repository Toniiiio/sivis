% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sivis.R
\name{get_xpath}
\alias{get_xpath}
\title{Get xpath given the target values.}
\usage{
get_xpath(
  text = NULL,
  strict_xpath = FALSE,
  doc = NULL,
  attr = NULL,
  by_index = FALSE,
  all_text
)
}
\arguments{
\item{text}{the target value to find the xpath for}

\item{strict_xpath}{As written in the description the user has the possibility to}

\item{doc}{document containing the server response or extracted sub parts}

\item{attr}{Should xpath be generated by classes. Currently an open issue.}

\item{by_index}{Should xpath be generated by index.}

\item{all_text}{All target values. They are required to avoid over and underfitting the xpath.}

\item{url}{page url - todo: seems to be used for usage outside of sivis. adapt that.}
}
\description{
Wrapper function for \code{\link{get_xpath_by_tag}}. Before an xpath can be derived from tags,
this tags have to be identified. Tags are found by searching through the document for the target
values. It might be confusing, but it will also be done via xpath.\cr
A simplified form of that xpath would be: //text()[contains(., '", target_values,"')]"). It will search for nodes
with texts that contain the target values. The xpath search is case sensitive. But the rendered browser
texts selected by the user can be upper case while the server reponse contains lower case and vice versa.
Therefore, translate(., 'ABCDEFGHIJKLMNOPQRSTUVWXYZ ', 'abcdefghijklmnopqrstuvwxyz') is used to make
the search for nodes case insensitive. Also spaces are removed, so that additional spaces wil not prohibit
a match. This case rarely occurs, but false positive cases are not known. So we can savely use it so far. \cr \cr
To summarize: This search is approximate as it uses contains(.), no spaces and case insensitivity. The upside of using it, is that in most
cases small derivations are no problem for sivis. In rare cases it might yield false positive results.
Therefore, a config parameter is introduced to disable this behaviour and allow an exact search.
}
