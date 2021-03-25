\name{getCPCHT}
\alias{getCPCHT}
\title{get formatted CPCHT}
\usage{
getCPCHT(filepath, askfile = FALSE)
}
\description{
  This function prepared formatted data-frame from raw CONOP generated file 'cpcht.txt'.
}
\examples{

# ask choose a file interactively
cpcht<-getCPCHT(askfile = TRUE)

# define a path to file
cpcht<-getCPCHT(filepath="path_to_cpcht_file/cpcht.txt")

}
