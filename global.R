library(dplyr)
library(ggplot2)
library(maps)
library(plotly)
library(readxl)
library(ggrepel)
library(lubridate)
library(shiny)
library(stringr)
library(tidyverse)
library(scales)

excel_data <- readxl::read_excel("gun_violence_curated.xlsx") %>% 
  filter(incident_date > '2014-12-31')


