require(ggplot2)
require(dplyr)

# Load Files
N.bias.raw <- read.csv("Model Bias Comparison/SampleSize/Master N Bias.csv")
HR.bias.raw <- read.csv("Model Bias Comparison/SampleSize/Master HR Bias.csv")
WSR.bias.raw <- read.csv("Model Bias Comparison/SampleSize/Master WSR Bias.csv")
R.bias.raw <- read.csv("Model Bias Comparison/SampleSize/Master R Bias.csv")
SimInfo.raw <- read.csv("Model Bias Comparison/SampleSize/MasterSimInfo.csv")