% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ruml.R
\name{make_plantuml_relations}
\alias{make_plantuml_relations}
\alias{make_plantuml_class_description}
\alias{make_plantuml_code}
\title{Generate plantuml data.}
\usage{
make_plantuml_relations(x)

make_plantuml_class_description(x, generics)

make_plantuml_code(x, pkg, generics = pkg_get_all_methods(pkg))
}
\arguments{
\item{x}{The name of a class stated as a (length-one) \code{character} vector.}

\item{generics}{The generic methods to consider.}

\item{pkg}{An (unquoted) package name.}
}
\description{
Generate plantuml data.
}
\details{
\code{make_plantuml_relations} will generate a \code{character}
  vector with all relations between super- and subclasses.

\code{make_plantuml_class_description} will
}
\examples{
make_plantuml_relations("count")
library(polmineR)
plantuml_code <- make_plantuml_code("corpus", pkg = polmineR)
}