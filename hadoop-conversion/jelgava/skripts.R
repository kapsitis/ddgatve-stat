setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

dfMeteo <- read.table("meteodati2.csv", header=TRUE,sep=",", encoding="UTF-8")
dfKrustojums <- read.table("lielas-ielas-cakstes-bulvara-krustojums.csv", header=TRUE,sep=",", encoding="UTF-8")

dfKrustojums$Datums <- as.POSIXlt(dfKrustojums$Hour)
murr <- as.numeric((as.vector(dfKrustojums$Datums))$hour)

newDf = data.frame(hr = murr, dfKrustojums$Total, dfKrustojums$N_L1D1)

aa <- aggregate(dfKrustojums.Total ~ hr, data = newDf, FUN = "sum")

library(ggplot2)

p <- ggplot(newDf, aes(fill='gray', y=dfKrustojums.Total, x=hr)) + 
  geom_bar(position="stack", stat="identity")

p


