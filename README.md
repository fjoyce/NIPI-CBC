# Paso del Istmo Biological Corridor, Rivas, Nicaragua Christmas Bird Count Shiny app

[Shiny app](https://fhjoyce.shinyapps.io/NIPI-CBC) for Paso del Istmo Biological Corridor Christmas Bird Count. 

Adapted from [Sharleen W's Shiny app for Hamilton, Ontario](https://sharleenw.shinyapps.io/hamilton_cbc_shiny/), as described on [this blog]( https://sharleenw.rbind.io/2019/03/24/hamilton-cbc-part-3/)

# data

~"HistoricalResultsByCount [NIPI-2015-2020].csv" is the raw csv download (taxonomic sort) from https://netapp.audubon.org/cbcobservation/historical/resultsbycount.aspx# for count code NIPI.~

~Start year: XX (2014) and End year: 120 (2019)~

"HistoricalResultsByCount [NIPI-2015-2021].csv" is the raw csv download (taxonomic sort) from https://netapp.audubon.org/cbcobservation/historical/resultsbycount.aspx# for count code NIPI.

Start year: XX (2014) and End year: 121 (2020)

I did do a bit of manual cleanup as described in the Rmd.

~cleaning.Rmd takes NIPI-2015-2020_pre-cleaned.csv and outputs NIPI-CBC-2020-cleaned.csv~
cleaning.Rmd takes NIPI-2015-2021_pre-cleaned.csv and outputs NIPI-CBC-2021-cleaned.csv


# root
~NIPI-CBC-2020-cleaned.csv is the data file used by the app.~

NIPI-CBC-2021-cleaned.csv is the data file used by the app.


app.R is the code for the Shiny app.
