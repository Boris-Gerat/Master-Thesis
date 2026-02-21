library("DBI")
library("RPostgres")
library("zoo")



###### SENTIMENT  PLOTS RAW DATA
con <- dbConnect(
		 RPostgres::Postgres(),
		 dbname = "fed_speeches",
		 host = "localhost",
		 port = "5432",
		 user = "borisgerat"
)

dbIsValid(con)
dbListTables(con)

data <- dbReadTable(con, "sentiment_base")
colnames(data)

data$Quarter <- as.yearqtr(data$Quarter, format = "%Y Q%q")



