\name{toon_data_in_excel}
\alias{toon_data_in_excel}
\title{Toont data in Excel}
\usage{
toon_data_in_excel(df)
}
\description{
opent Excel en toont de data uit de dataframe.
Handig voor grote dataframe's (100MB+).
}
\examples{
## Hieronder een voorbeeld
library(datasets)

data(mtcars)
head(mtcars)

toon_data_in_excel(df=mtcars)

}
