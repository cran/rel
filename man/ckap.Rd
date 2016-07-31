\name{ckap}
\alias{ckap}
\title{Cohen's kappa and weighted kappa}
\description{Calculates Cohen's kappa (Cohen, 1960) or weighted kappa (Cohen, 1968) as indices of nominal or ordinal scale inter-rater agreement, respectively.}
\usage{ckap(data = NULL, weight = c("unweighted", "linear", "quadratic"), 
    std.err = c("Cohen", "Fleiss"), conf.level = 0.95)}
\arguments{
  \item{data}{A numeric matrix with n subjects and two raters (n*2 matrix).}
  \item{weight}{A character string specifying "unweighted", "linear", or "quadratic", or a numeric categories*categories matrix with custom weights (see details).}
  \item{std.err}{Standard error calculation formula specified as "Cohen" or "Fleiss" (see details).}
  \item{conf.level}{Confidence level of the interval.}
}
\details{
Cohen's kappa by default derives the standard error from Fleiss et als., corrected formula from 1969, with Cohen's original formula from 1960 optional. Weighted kappa is based on weighted dissimilarities (diagonal = 1, off-diagonal < 1). Linear weights decrease equally with distance from the diagonal and quadratic weights decrease exponentially with distance from the diagonal. Custom weights should be specified as a categories*categories matrix with values <= 1.
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
Cohen, J. (1960). A coefficient of agreement for nominal scales. Educational and Psychological Measurement, 20, 213-220.\cr\cr
Cohen, J. (1968). Weighted kappa: Nominal scale agreement provision for scaled disagreement or partial credit. Psychological Bulletin, 70, 213-220.\cr\cr
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
ckap(data=data, weight="unweighted", std.err="Fleiss", conf.level = 0.95)

#Weighted kappa with linear weight
ckap(data=data, weight="linear", conf.level = 0.95)

#Weighted kappa with custom weights
ckap(data=data, weight=cw, conf.level = 0.95)
}
\keyword{univar}