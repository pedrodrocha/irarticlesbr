% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/download_moncoes.R
\name{download_moncoes}
\alias{download_moncoes}
\title{Download articles from Estudos Internacionais}
\usage{
download_moncoes(year, volume, number, dir, info_data = FALSE)
}
\arguments{
\item{year}{Issue Year}

\item{volume}{Issue volume}

\item{number}{Issue Number}

\item{dir}{Directory for storing files}

\item{info_data}{Whether to return or not a tibble with info on collected pdfs}
}
\value{
Nothing. It just download files based on user input
}
\description{
Download articles from Estudos Internacionais
}
\examples{
download_moncoes(year = 2020, volume = 9, number = 17, dir = "/home/std_pedrorocha/Documentos/irarticlesbr/teste")
}
