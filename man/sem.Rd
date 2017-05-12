\name{sem}
\alias{sem}
\title{Standard error of measurement}
\description{Calculates the standard error of measurement}
\usage{sem(data = NULL, type = c("mse", "sd", "cpd"), conf.level = 0.95)}
\arguments{
  \item{data}{A matrix with n subjects and m observations (n*m matrix).}
  \item{type}{The method used to compute sem with a character string specifying "sd" for the within-subject standard deviation, "mse" for the square root of the ANOVA error variance, or "cpd" for the consecutive pairwise difference.}
  \item{conf.level}{Confidence level of the interval.}
}
\details{
"sd" and "mse" includes complete cases only and have a confidence interval based on a t distribution. "cpd" includes all cases, derives sem from the difference between adjacent trials, and has a confidence interval based on a chi squared distribution (Hopkins 2015). "cpd" is computed both overall and separately for consecutive trials, the latter allowing one to assess whether habituation decreases sem (Hopkins 2015).
}
\value{
\item{method}{Analysis name}
\item{obs}{Number of observations}
\item{sample}{Sample size}
\item{na}{missing values}
\item{est}{Point estimate}
\item{lb}{Lower confidence boundary}
\item{ub}{Upper confidence boundary}
\item{est.cpd}{sem for adjacent columns}
\item{data}{analyzed data}
}
\references{
Nunnally, J. C., Bernstein, I. H. (1994). Psychometric theory. New York, NY: McGraw-Hill.\cr\cr
Hopkins, W. G. (2015). Spreadsheets for Analysis of Validity and Reliability. Sportscience 19, 36-42.\cr\cr
}
\author{Riccardo Lo Martire}
\examples{
#Sample data: 200 subjects rated their weight twice.
data <- cbind(sample(50:100,200,replace=TRUE), sample(50:100,200,replace=TRUE))

#Standard error of measurement
sem(data=data, type="mse", conf.level=0.95)
}
\keyword{univar}