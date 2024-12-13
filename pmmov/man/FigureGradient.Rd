% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/FigureGradient.R
\name{FigureGradient}
\alias{FigureGradient}
\title{FigureGradient - Create and save gradient map figure as PDF.}
\usage{
FigureGradient(scan_fb, power = NA, use_last = TRUE, out_path = "manu/figures")
}
\arguments{
\item{scan_fb}{a data frame; cleaned SCAN data}

\item{power}{a positive scalar; power to use for IDW}

\item{use_last}{a boolean flag; if TRUE, use the last saved interpolation}

\item{out_path}{a string; path to save figure}
}
\value{
None
}
\description{
Figure is a map of the contiguous USA with a color gradient made with
cubic regression spline generalized additive model.
}
