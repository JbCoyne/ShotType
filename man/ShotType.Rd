\name{ShotType}
\alias{ShotType}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
ShotType
}
\description{
ShotType takes a paired coordinate (x,y) and outputs the area on an NBA court which an NBA shot came from.
}
\usage{
ShotType(x, y)
}
Used to map out shot areas.
\arguments{
  \item{x}{
Numeric value that represents the horizontal distance, in inches, from the top left corner of the NBA court when viewed from above.
}
  \item{y}{
Numeric value that represents the vertical distance, in inches, from the top left corner of the NBA court when viewed from above.
}
}
\details{
X has a range from 0 to 1128 inclusive, and Y has a range from 0 to 600 inclusive.
}
\value{
Returns a text string describing shot location.
}
\references{
Can reference SportRadar for examples of shot location data that work in this function.
}
\author{
Jake Coyne
}
