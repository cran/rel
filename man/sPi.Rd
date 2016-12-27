\name{sPi}
\alias{sPi}
\title{Scott's pi and Fleiss' kappa}
\description{Calculates Scott's pi or Fleiss' kappa as an index of agreement between two raters or more than two raters, respectively.}
\usage{sPi(data = NULL, weight = c("unweighted","linear","quadratic"), conf.level = 0.95)}
\arguments{
  \item{data}{A matrix with n subjects and m raters (n*m matrix)}
  \item{weight}{A character string specifying "unweighted", "linear", or "quadratic", or a numeric categories*categories matrix with custom weights (see details).}
  \item{conf.level}{Confidence level of the interval.}
}
\details{
Scott's pi measures the chance-corrected agreement between two raters (Scott, 1955), and Fleiss' kappa is a generalisation of Scott's pi for m raters (Fleiss, 1971).\cr\cr The standard error for Fleiss' kappa is based on the formula from Fleiss et al., 1979. Weights are based on weighted dissimilarities (diagonal = 1, off-diagonal < 1). Linear weights decrease equally with distance from the diagonal and quadratic weights decrease exponentially with distance from the diagonal. Custom weights should be specified as a categories*categories matrix with values <= 1. Incomplete cases are omitted listwise and the confidence interval is based on a t distribution.
}
\value{
\item{method}{Analysis name}
\item{raters}{Number of raters}
\item{sample}{Sample size}
\item{est}{Point estimate}
\item{std.err}{Standard error}
\item{ci.lower}{Lower confidence boundary}
\item{ci.upper}{Upper confidence boundary}
}
\references{
Scott W. A. (1955). Reliability of content analysis: The case of nominal scale coding. Public Opinion Quarterly, 19, 321-325.\cr\cr
Fleiss J. L. (1971). Measuring nominal scale agreement among many raters. Psychological Bulletin, 76, 378-382.\cr\cr
Fleiss J. L., Nee J. C. M., Landis J. R. (1979). Large sample variance of kappa in the case of different sets of raters. Psychological Bulletin, 86, 974-977.
}
\author{Riccardo Lo Martire}
\examples{
#Sample data: 200 subjects and one 5-category item.
data <- cbind(sample(1:5,200, replace=TRUE),sample(1:5,200, replace=TRUE))

#A numeric categories*categories matrix with custom weights 
cw <- diag(ncol(matrix(0,5,5)))
cw[cw!=diag(cw)] <- runif(20,0,1)

#Scott's pi
sPi(data=data, weight="unweighted", conf.level = 0.95)

#Weighted pi with custom weights
sPi(data=data, weight=cw, conf.level = 0.95)
}
\keyword{univar}