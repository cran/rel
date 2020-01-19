\name{bags}
\alias{bags}
\title{Bennett, Alpert, and Goldstein's S}
\description{Calculates S as an index of agreement for two observations of nominal scale data.}
\usage{bags(data = NULL, kat = NULL, conf.level = 0.95)}
\arguments{
  \item{data}{A matrix with n subjects and two observations (n*2 matrix)}
  \item{kat}{Number of possible categories}
  \item{conf.level}{Confidence level of the interval.}
}
\value{
\item{method}{Analysis name}
\item{obs}{Number of observations}
\item{sample}{Sample size}
\item{est}{Point estimate}
\item{se}{Standard error}
\item{lb}{Lower confidence boundary}
\item{ub}{Upper confidence boundary}
\item{cont.table}{contingency table}
\item{data}{analyzed data}
}
\references{Bennett, E. M., Alpert, R., Goldstein, A. C. (1954). Communications through limited-response questioning. Public Opinion Quarterly, 18(3), 303-308.}
\author{Riccardo Lo Martire}
\examples{
#Sample data: 200 subjects and one 5-category item.
data <- cbind(sample(1:5,200, replace=TRUE),sample(1:5,200, replace=TRUE))

#Analysis
bags(data=data, kat=5, conf.level=0.95)
}
\keyword{univar}