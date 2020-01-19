\name{kra}
\alias{kra}
\title{Krippendorff's alpha}
\description{Calculates Krippendorff's alpha as an index of agreement for nominal, ordinal, interval, or ratio scale data.}
\usage{kra(data = NULL, weight = c("nominal","ordinal","interval","ratio"), 
    conf.level = 0.95, R = 0)}
\arguments{
  \item{data}{A matrix with n subjects and m observations (n*m matrix).}
  \item{weight}{Data scale of ratings specified as "nominal, "ordinal", "interval", or "ratio".}
  \item{conf.level}{Confidence level of the interval.}
  \item{R}{Number of bootstrap replicates used to estimate the confidence interval.}
}
\details{
Krippendorff's alpha is a measure of observed disagreement relative to disagreement expected by chance which has the advantages of being applicable to multiple raters, different scale metrics, and incomplete data sets (Krippendorff, 2004 p.221-243). Noteworthy is that alpha by definition is zero when the observed disagreement equals the expected disagreement (i.e., when variance is absent which is the case for perfect agreement). The confidence interval of alpha is derived via bootstrapping because its distribution is unknown.
}
\value{
\item{method}{Analysis name}
\item{raters}{Number of raters}
\item{sample}{Sample size}
\item{na}{Percent of missing values}
\item{est}{Point estimate}
\item{lb}{Lower confidence boundary}
\item{ub}{Upper confidence boundary}
\item{cont.table}{contingency table}
\item{data}{analyzed data}
}
\references{
Krippendorff, K. (2004). Content Analysis: An Introduction to Its Methodology. Thousand Oaks, CA: Sage. 
}
\author{Riccardo Lo Martire}
\examples{
#Sample data: 4 raters, 12 subjects, and 5 ordinal scale response categories.
data <- cbind(rbind(1,2,3,3,2,1,4,1,2,NA,NA,NA),
              rbind(1,2,3,3,2,2,4,1,2,5,NA,3),
              rbind(NA,3,3,3,2,3,4,2,2,5,1,NA),
              rbind(1,2,3,3,2,4,4,1,2,5,1,NA))

#Krippendorff's alpha for ordinal scale data
kra(data = data, weight = "ordinal", conf.level = 0.95, R = 0)
}
\keyword{univar}