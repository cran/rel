\name{bS}
\alias{bS}
\title{Bennett, Alpert, and Goldstein's S}
\description{Calculates S (Bennett et al., 1954) as an index of nominal scale inter-rater agreement.}
\usage{bS(data = NULL, kat = NULL, conf.level = 0.95)}
\arguments{
  \item{data}{A numeric matrix with n subjects and two measurements (n*2 matrix)}
  \item{kat}{Number of categories}
  \item{conf.level}{Confidence level of the interval.}
}
\value{
\item{method}{Analysis name}
\item{sample}{Sample size}
\item{est}{Point estimate}
\item{std.err}{Standard error}
\item{ci.lower}{Lower confidence boundary}
\item{ci.upper}{Upper confidence boundary}
}
\references{Bennett, E. M., Alpert, R., Goldstein, A. C. (1954). Communications through limited-response questioning. Public Opinion Quarterly, 18(3), 303-308.}
\author{Riccardo Lo Martire}
\examples{
#Sample data: 200 subjects and one 5-category item.
data <- cbind(sample(1:5,200, replace=TRUE),sample(1:5,200, replace=TRUE))

#Analysis
bS(data=data, kat=5, conf.level=0.95)
}
\keyword{univar}