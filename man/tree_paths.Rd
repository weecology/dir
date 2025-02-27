% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/directree.R
\name{tree_paths}
\alias{tree_paths}
\title{Create the Paths for a Directory Tree}
\usage{
tree_paths(..., fsep = .Platform$file.sep, winslash = "\\\\", mustWork = NA)
}
\arguments{
\item{fsep}{Path separator to use (assumed to be ASCII).
See \code{\link[base]{file.path}}.}

\item{winslash}{Separator to be used on Windows - ignored elsewhere. 
See \code{\link[base]{normalizePath}}.}

\item{mustWork}{\code{logical}: if \code{TRUE} then an error is given if 
the result cannot be determined; if \code{NA}, then a warning.
See \code{\link[base]{normalizePath}}.}

\item{tree}{\code{list} object describing a directory tree.}
}
\value{
\code{character} vector of results of the \code{...} entries
 expanded and run through \code{\link{normalized_file_path}}.
}
\description{
Creates a vector of paths for a directory tree through
 dot-based list expansion
}
