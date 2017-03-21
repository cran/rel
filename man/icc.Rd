\name{icc}
\alias{icc}
\title{Intraclass correlation coefficients}
\description{Calculates intraclass correlation coefficients as a reliability measure for interval scale data.}
\usage{icc(data = NULL, model = c("one", "two"), type = c("agreement", "consistency"), 
           measure = c("single", "average"), conf.level = 0.95)}
\arguments{
  \item{data}{A matrix with n subjects and m observations (n*m matrix).}
  \item{model}{A character string specifying "one" for one-way models or "two" for two-way models (See details).}
  \item{type}{A character string specifying "agreement" or "consistency" (See details).}
  \item{measure}{A character string specifying "singel" or "average" (See details).}
  \item{conf.level}{Confidence level of the interval.}
}
\details{
ICC measures the proportion of variance that is attributable to the objects of measurement (McGraw and Wong 1996). In the one-way model rows are random (i.e., columns are nested within rows), and in the two-way model both rows and columns are random (i.e., rows and columns are crossed).\cr\cr Consistency considers observations relative to each other while absolute agreement considers the absolute difference of the observations (McGraw and Wong 1996). For example, ICC equals 1.00 for the paired scores (2,4), (4,6) and (6,8) for consistency, but only 0.67 for absolute agreement. In the one-way model, only absolute agreement is possible (McGraw and Wong 1996). The measure chosen should reflect the application of the tested item. The single measure is appropriate if the intention is to use the score from a single observation, while the average measure is suitable if the intention is to use an average score across a number of observations. The confidence interval is based on a F distribution. Incomplete cases are omitted listwise. 
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
McGraw, K. O., Wong, S. P. (1996), Forming inferences about some intraclass correlation coefficients. Psychological Methods, 1(1), 30-46.\cr\cr
Shrout, P. E., Fleiss, J. L. (1979), Intraclass correlation: uses in assessing rater reliability. Psychological Bulletin, 86(2), 420-428.
}
\author{Riccardo Lo Martire}
\examples{
#Sample data: 200 subjects rated their weight twice.
data <- cbind(sample(50:100,200,replace=TRUE), sample(50:100,200,replace=TRUE))

#ICC based on a two-way random effects model of absolute agreement for a single observation
icc(data=data, model = "two", type = "agreement", measure ="single", conf.level=0.95)
}
\keyword{univar}