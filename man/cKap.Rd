\name{cKap}
\alias{cKap}
\title{Cohen's kappa, weighted kappa, and Conger's kappa}
\description{Calculates Cohen's kappa or weighted kappa as indices of nominal or ordinal scale agreement between two raters, or Conger's kappa as an index of nominal scale agreement for more than two raters.}
\usage{cKap(data = NULL, weight = c("unweighted", "linear", "quadratic"), 
    std.err = c("Fleiss", "Cohen"), conf.level = 0.95, R = 0)}
\arguments{
  \item{data}{A matrix with n subjects and m raters (n*m matrix).}
  \item{weight}{A character string specifying "unweighted", "linear", or "quadratic", or a numeric categories*categories matrix with custom weights (see details).}
  \item{std.err}{Standard error calculation formula specified as "Fleiss" or "Cohen" (see details).}
  \item{conf.level}{Confidence level of the interval.}
  \item{R}{Number of bootstrap replicates used to estimate the confidence interval for Conger's kappa.}
}
\details{
Cohen's kappa measures the chance-corrected agreement between two raters (Cohen, 1960 and 1968), and Conger's kappa is a generalisation of Cohen's kappa for m raters (Conger, 1980). Because the maximum value for kappa commonly is restricted below 1.00 by the marginal distributions (Cohen 1968), it can also be beneficial to consider kmax when interpreting results.\cr\cr By default, the standard error of kappa for two raters is derived via Fleiss et als., corrected formula from 1969, with Cohen's original formula from 1960 optional, and the confidence interval is based on a t distribution. The confidence interval of Conger's kappa is derived via bootstrapping. Weighted kappa is based on weighted dissimilarities (diagonal = 1, off-diagonal < 1). Linear weights decrease equally with distance from the diagonal and quadratic weights decrease exponentially with distance from the diagonal. Custom weights should be specified as a categories*categories matrix with values <= 1. Incomplete cases are omitted listwise. 
}
\value{
\item{method}{Analysis name}
\item{raters}{Number of raters}
\item{sample}{Sample size}
\item{est}{Point estimate}
\item{std.err}{Standard error}
\item{ci.lower}{Lower confidence boundary}
\item{ci.upper}{Upper confidence boundary}
\item{kmax}{The maximum value of kappa permitted by the marginal distributions}
\item{kmax.prop}{The proportion of the kappa point estimate to the maximum possible kappa value}
}
\references{
Cohen, J. (1960). A coefficient of agreement for nominal scales. Educational and Psychological Measurement, 20, 213-220.\cr\cr
Cohen, J. (1968). Weighted kappa: Nominal scale agreement provision for scaled disagreement or partial credit. Psychological Bulletin, 70, 213-220.\cr\cr
Conger, A. J. (1980). Integration and generalization of kappas for multiple raters. Psychological Bulletin, 88, 322-328.\cr\cr
Fleiss, J. L., Cohen, J. and Everitt, B.S. (1969). Large sample standard errors of kappa and weighted kappa. Psychological Bulletin, 72, 332-327.
}
\author{Riccardo Lo Martire}
\examples{
#Sample data: 200 subjects and 5 reponse categories.
data <- cbind(sample(1:5,200, replace=TRUE),sample(1:5,200, replace=TRUE))

#A numeric categories*categories matrix with custom weights 
cw <- diag(ncol(matrix(0,5,5)))
cw[cw!=diag(cw)] <- runif(20,0,1)

#Cohen's kappa with Fleiss corrected standard error formula
cKap(data=data, weight="unweighted", std.err="Fleiss", conf.level = 0.95)

#Weighted kappa with linear weight
cKap(data=data, weight="linear", conf.level = 0.95)

#Weighted kappa with custom weights
cKap(data=data, weight=cw, conf.level = 0.95)
}
\keyword{univar}