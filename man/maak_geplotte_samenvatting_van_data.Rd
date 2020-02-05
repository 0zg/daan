\name{maak_geplotte_samenvatting_van_data}
\alias{maak_geplotte_samenvatting_van_data}
\title{Samenvatting van plots maken vanuit een dataframe}
\usage{
maak_geplotte_samenvatting_van_data(df)
}
\description{
maakt een scattermatrix vanuit alle kolommen in de dataframe.
}
\examples{
## Hieronder een voorbeeld

library(datasets)

data(mtcars)
head(mtcars)

plot <- maak_geplotte_samenvatting_van_data(df=mtcars)

#print(plot)
}
