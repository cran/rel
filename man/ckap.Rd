\name{ckap}
\alias{ckap}
\title{Cohen's kappa, weighted kappa, and Conger's kappa}
\description{Calculates Cohen's kappa or weighted kappa as indices of agreement for two observations of nominal or ordinal scale data, respectively, or Conger's kappa as an index of agreement for more than two observations of nominal scale data.}
\usage{ckap(data = NULL, weight = c("unweighted", "linear", "quadratic"), 
    std.err = c("Fleiss", "Cohen"), conf.level = 0.95, R = 0)}
\arguments{
  \item{data}{A matrix with n subjects and m observations (n*m matrix).}
  \item{weight}{A character string specifying "unweighted", "linear", or "quadratic", or a numeric categories*categories matrix with custom weights (see details).}
  \item{std.err}{Standard error calculation formula specified as "Fleiss" or "Cohen" (see details).}
  \item{conf.level}{Confidence level of the interval.}
  \item{R}{Number of bootstrap replicates used to estimate the confidence interval for Conger's kappa.}
}
\details{
Cohen's kappa measures the chance-corrected agreement for two observations (Cohen, 1960 and 1968), and Conger's kappa is a generalization of Cohen's kappa for m observations (Conger, 1980). Because the maximum value for kappa commonly is restricted below 1.00 by the marginal distributions (Cohen 1960), it can also be beneficial to consider kmax when interpreting results.\cr\cr By default, the standard error of Cohen's kappa is derived via Fleiss et als., corrected formula from 1969, with Cohen's original formula from 1960 optional, and the confidence interval is based on a t distribution. The confidence interval of Conger's kappa is derived via bootstrapping. Weighted kappa is based on weighted dissimilarities (diagonal = 1, off-diagonal < 1). Linear weights decrease equally with distance from the diagonal and quadratic weights decrease exponentially with distance from the diagonal. Custom weights should be specified as a categories*categories matrix with values <= 1. Incomplete cases are omitted listwise. 
}
\value{
\item{method}{Analysis name}
\item{obs}{Number of observations}
\item{sample}{Sample size}
\item{est}{Point estimate}
\item{se}{Standard error}
\item{lb}{Lower confidence boundary}
\item{ub}{Upper confidence boundary}
\item{kmax}{The maximum value of kappa permitted by the marginal distributions}
\item{kmax.prop}{The proportion of the kappa point estimate to the maximum possible kappa value}
\item{cont.table}{contingency table}
\item{data}{analyzed data}
}
\references{
Cohen, J. (1960). A coefficient of agreement for nominal scales. Educational and Psychological Measurement, 20(1), 37-46.\cr\cr
Cohen, J. (1968). Weighted kappa: Nominal scale agreement provision for scaled disagreement or partial credit. Psychological Bulletin, 70(4), 213-220.\cr\cr
Conger, A. J. (1980). Integration and generalization of kappas for multiple raters. Psychological Bulletin, 88(2), 322-328.\cr\cr
Fleiss, J. L., Cohen, J., Everitt, B.S. (1969). Large sample standard errors of kappa and weighted kappa. Psychological Bulletin, 72(5), 323-327.
}
\author{Riccardo Lo Martire}
\examples{
#Sample data: 200 subjects and 5 reponse categories.
data <- cbind(sample(1:5,200, replace=TRUE),sample(1:5,200, replace=TRUE))

#A numeric categories*categories matrix with custom weights 
cw <- diag(ncol(matrix(0,5,5)))
cw[cw!=diag(cw)] <- runif(20,0,1)

#Cohen's kappa with Fleiss corrected standard error formula
ckap(data=data, weight="unweighted", std.err="Fleiss", conf.level = 0.95)

#Weighted kappa with linear weight
ckap(data=data, weight="linear", conf.level = 0.95)

#Weighted kappa with custom weights
ckap(data=data, weight=cw, conf.level = 0.95)
}
\keyword{univar}