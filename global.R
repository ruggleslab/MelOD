library(shiny)
library(bslib)
library(bsicons)
library(magrittr)
library(shinyWidgets)
library(shinydashboard)
library(shinyjs)
library(plotly)
library(DESeq2)
library(dplyr)
library(tidyverse)
library(heatmaply)
library(reshape2)
library(S4Vectors)
library(jsonlite)
library(pheatmap)
library(RColorBrewer)
library(shinydlplot)
library(survival)
library(readxl)
library(ggpubr)
library(shinyalert)
library(Glimma)
library(DT)
library(googledrive)
library(shinybusy)

## Setting up the auth token location and email
options(
  gargle_oauth_email = "ruggleslab.shinyseq.backend@gmail.com",
  gargle_oauth_cache = "authentication" ## location within the shiny structure to store the auth token
)

## Getting the auth token (only needs to be done once, so if you have the file you don't need to do this)
#drive_auth()

## List all of the files that you have available to download
drive_find()
options(shiny.plot.res=96)


