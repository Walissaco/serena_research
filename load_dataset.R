# Install/load required packages
install.packages("readxl")
install.packages("dplyr")
install.packages("ggplot2")

library(readxl)
library(dplyr)
library(ggplot2)

# Load the Excel file
data <- read_excel("JorgeNewdata3feb.xlsx")