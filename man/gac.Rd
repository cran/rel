\name{gac}
\alias{gac}
\title{Gwet's AC1 and AC2}
\description{Calculates Gwet's AC as index of agreement for two observations of nominal, ordinal, or ratio scale data.}
\usage{gac(data = NULL, kat = NULL, weight = c("unweighted","linear","quadratic","ratio"),
    conf.level = 0.95)}
\arguments{
  \item{data}{A matrix with n subjects and two observations (n*2 matrix)}
  \item{kat}{Number of possible categories}
  \item{weight}{A character string specifying "unweighted", "linear", "quadratic" or "ratio", or a numeric kat*kat matrix with custom weights (see details).}
  \item{conf.level}{Confidence level of the interval.}
}
\details{
Gwet's AC has the advantage of not relying on independence between observations (Gwet, 2008), making it suitable for data with dependent measurements. Weights are based on weighted dissimilarities (diagonal = 1, off-diagonal < 1). Linear weights decrease equally with distance from the diagonal and quadratic weights decrease exponentially with distance from the diagonal. Custom weights should be specified as a kat*kat matrix with values <= 1. Incomplete cases are omitted listwise and the confidence interval is based on a t distribution.
}
\value{
\item{method}{Analysis name}
\item{obs}{Number of observations}
\item{sample}{Sample size}
\item{est}{Point estimate}
\item{se}{Standard error}
\item{lb}{Lower confidence boundary}
\item{ub}{Upper confidence boundary}
\item{data}{analyzed data}
}
\references{
Gwet, K. L. (2008). Computing inter-rater reliability and its variance in the presence of high agreement. British Journal of Mathematical and Statistical Psychology 61, 29-48.
}
\author{Riccardo Lo Martire}
\examples{
#Sample data: 200 subjects and one 5-category item.
data <- cbind(sample(1:5,200, replace=TRUE),sample(1:5,200, replace=TRUE))

#A numeric kat*kat matrix with custom weights 
cw <- diag(ncol(matrix(0,5,5)))
cw[cw!=diag(cw)] <- runif(20,0,1)

#AC1
gac(data=data, kat=5, weight="unweighted", conf.level = 0.95)

#AC2 with custom weights
gac(data=data, kat=5, weight=cw, conf.level = 0.95)
}
\keyword{univar}