\name{sem}
\alias{sem}
\title{Standard error of measurement}
\description{Calculates the standard error of measurement}
\usage{sem(data = NULL, conf.level = 0.95)}
\arguments{
  \item{data}{A matrix with n subjects and m observations (n*m matrix).}
  \item{conf.level}{Confidence level of the interval.}
}
\details{
The standard error of measurement, calculated as the mean within-subject standard deviation across observations (Nunnally and Bernstein 1994). The confidence interval is based on a t distribution. Incomplete cases are omitted listwise.
}
\value{
\item{method}{Analysis name}
\item{obs}{Number of observations}
\item{sample}{Sample size}
\item{est}{Point estimate}
\item{lb}{Lower confidence boundary}
\item{ub}{Upper confidence boundary}
\item{data}{analyzed data}
}
\references{
Nunnally, J. C., Bernstein, I. H. (1994). Psychometric theory. New York, NY: McGraw-Hill.\cr\cr
}
\author{Riccardo Lo Martire}
\examples{
#Sample data: 200 subjects rated their weight twice.
data <- cbind(sample(50:100,200,replace=TRUE), sample(50:100,200,replace=TRUE))

#Standard error of measurement
sem(data=data, conf.level=0.95)
}
\keyword{univar}