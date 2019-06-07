# Author: Shrikant Pawar, Forecasting program with dimension reductionality.

setwd("C:/Users/Bio-user/Desktop/ITISE_2019")
MyData <- read.csv(file="Year_Data.csv", header=TRUE, sep=",")
MyData[is.na(MyData)] <- 0
Data1 <- as.numeric(unlist(MyData[,2:48]))

# Plot raw data
dotchart(Data1,cex=.7,
   main="Dot plot of M3-COMPETITION - 3003 series data", 
   xlab="Observations for 5 categories", ylab="Years (1811-1975)")

# Make 24 dimensions
yy <- window(sunspot.month, 1901, 2000)
XX <- embed(yy, 24)
XX <- ts(XX, end = end(yy), freq = 12)
dim(XX)

# Apply ForeCA
library(ForeCA)
# this can take several seconds
mod.foreca <- foreca(XX, n.comp = 4, 
                     spectrum.control = list(method = "pgram"))

plot(mod.foreca)
mod.foreca$scores <- ts(mod.foreca$scores, start = start(XX), 
                        freq = frequency(XX))
plot(mod.foreca$scores)

round(cor(mod.foreca$scores), 3)
spec <- mvspectrum(mod.foreca$scores, "pgram")
plot(spec)