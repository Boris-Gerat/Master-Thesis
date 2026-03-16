library(quantmod)
library(readxl)

getSymbols("^RUT", src= "yahoo", from= "1980-10-10")

rut_price <- RUT$RUT.Adjusted
rut_quartelry <- apply.quarterly(rut_price, last)
rut_quartelry
write.csv(rut_quartelry, "Rusell_data_Q.csv")



