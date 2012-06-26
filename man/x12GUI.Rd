\name{x12GUI}
\alias{x12GUI}
\title{Graphical User Interface for the S4 implementation of the X12-Arima wrapper in package x12}
\description{
  GUI
}
\usage{
x12GUI(x12orig,...)
}
\arguments{
  \item{x12orig}{object of class x12Batch or x12Single}
  \item{\dots}{further arguments (currently ignored).}
}
\author{Daniel Schopfhauser}
\seealso{
\code{\link{X12}},
\code{\linkS4class{x12Single}},
\code{\linkS4class{x12Batch}},
\code{\linkS4class{x12Parameter}},
\code{\linkS4class{x12List}},
\code{\linkS4class{x12Output}},
\code{\linkS4class{x12BaseInfo}},
%\code{\link{summary}},
\code{\link{summary.x12}},
\code{\link{x12}}
}
\examples{
\dontrun{
  data(AirPassengersX12Batch)
  xbn <- x12GUI(AirPassengersX12Batch)
}
}


