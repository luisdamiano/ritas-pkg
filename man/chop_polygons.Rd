% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/polygons.R
\name{chop_polygons}
\alias{chop_polygons}
\title{Crops polygons using a grid.}
\usage{
chop_polygons(spdf, gridSpdf, colIdentity, colWeight, tol = 1e-08)
}
\arguments{
\item{spdf}{A `\code{\link[sp]{SpatialPolygonsDataFrame}}` object with the
polygons to be cropped.}

\item{gridSpdf}{A `\code{\link[sp]{SpatialPolygonsDataFrame}}` object with
the grid as created by \code{\link{make_grid}}.}

\item{colIdentity}{A character vector with the name of the columns of `spdf`
that should be passed through without modification.}

\item{colWeight}{A character vector with the name of the columns of `spdf`
that should be multiplied by the areal weight (apportioning).}

\item{tol}{The tolerance value for simplifying the polygons before cropping
(see \code{\link[sp]{gSimplify}}).}
}
\value{
A `\code{\link[sp]{SpatialPolygons}}` list with the resulting
  polygons.
}
\description{
Chops many polygons into many finer pieces by ovelaying a grid.
}
