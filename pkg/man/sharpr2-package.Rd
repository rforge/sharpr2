\name{sharpr2-package}
\alias{sharpr2-package}
\docType{package}
\title{
Estimating regularoty scores and identifying high resolution driver elements for HiDRA data
}
\description{
The package develops an algorithm for handling HiDRA datasets by extending MPRA-SHARPR. 
}
\details{
\tabular{ll}{
Package: \tab sharpr2\cr
Type: \tab Package\cr
Version: \tab 0.1.1.0000 \cr
Date: \tab 2017-09-26\cr
License: \tab GPL \cr
}
}
\author{
Liang He

Maintainer: Liang He <lianghe@mit.edu>
}

\references{
Xinchen Wang, Liang He, Sarah Goggin, Alham Saadat, Li Wang, Melina Claussnitzer, Manolis Kellis. 
High-resolution genome-wide functional dissection of transcriptional regulatory regions in human.
}
\keyword{ package }

\examples{
data(hidra_ex)
re <- sharpr2(hidra_ex, l_min = 150, l_max = 600, f_dna = 5, f_rna = 0, sig=FALSE)

}
