\name{sPi}
\alias{sPi}
\title{Scott's pi}
\description{Calculates Scott's pi (Scott, 1955) as an index of nominal or ordinal scale inter-rater agreement.}
\usage{sPi(data = NULL, weight = c("unweighted","linear","quadratic"), conf.level = 0.95)}
\arguments{
  \item{data}{A numeric matrix with n subjects and two raters (n*2 matrix)}
  \item{weight}{A character string specifying "unweighted", "linear", or "quadratic", or a numeric categories*categories matrix with custom weights (see details).}
  \item{conf.level}{Confidence level of the interval.}
}
\details{
Weights based on weighted dissimilarities (diagonal = 1, off-diagonal < 1). Linear weights decrease equally with distance from the diagonal and quadratic weights decrease exponentially with distance from the diagonal. Custom weights should be specified as a categories*categories matrix with values <= 1.
}
\value{
\item{method}{Analysis name}
\item{sample}{Sample size}
\item{est}{Point estimate}
\item{std.err}{Standard error}
\item{ci.lower}{Lower confidence boundary}
\item{ci.upper}{Upper confidence boundary}
}
\references{
Scott W. A. (1955). Reliability of content analysis: The case of nominal scale coding. Public Opinion Quarterly 19, 321-325.
}
\author{Riccardo Lo Martire}
\examples{
#Sample data: 200 subjects and one 5-category item.
data <- cbind(sample(1:5,200, replace=TRUE),sample(1:5,200, replace=TRUE))

#A numeric categories*categories matrix with custom weights 
cw <- diag(ncol(matrix(0,5,5)))
cw[cw!=diag(cw)] <- runif(20,0,1)

#Unweighted pi
sPi(data=data, weight="unweighted", conf.level = 0.95)

#Weighted pi with custom weights
sPi(data=data, weight=cw, conf.level = 0.95)
}
\keyword{univar}