% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/linreg.R
\docType{class}
\name{linreg-class}
\alias{linreg-class}
\alias{linreg}
\title{Linreg}
\arguments{
\item{formula, }{a suggested linear regression model}

\item{data, }{a data source used to train the model}
}
\value{
a list of key results from regression, including Coefficients,FittedValues,Residuals,FreedomDegree,VarianceResiduals,StdErrorResiduals,VarianceCoefficients,StdError,Tcoefficients,Possibility,X,Y,Formula,Data,VariableNames
}
\description{
Given \code{formula} and data source \code{data}, find the best fitted linear regression model.
}

\examples{
data(iris)
a<-linreg$new(formula=Sepal.Length~Sepal.Width,data=iris)
a$print()          
a$plot()
}
