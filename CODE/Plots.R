library("DBI")
library("RPostgres")
library("zoo")
library("xts")


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
data <- subset(data,
	       Quarter >= as.yearqtr("1995 Q1"),
	       Quarter <=("2025 Q4")
		)
data_subset_vader <- data[, c("gdelt_avgTone", "fed_minutes_vader", "gov_vader", "fed_speech_vader", "sec_vader")]
data_subset_vader_scaled <- scale(data_subset_vader)

# Convert to xts
data_xts <- xts(
  data_subset_vader_scaled,
  order.by = as.Date(data$Quarter)
)

# Professional display names
legend_labels <- c(
  "GDELT Avg Tone",
  "Fed Minutes Sentiment",
  "Government Sentiment",
  "Fed Speech Sentiment",
  "SEC Sentiment"
)

# Quant palette (mapped to data order)
cols <- c(
  "#00AEEF",  # Electric Quant Blue
  "#1F3B73",  # Deep Navy
  "#17BECF",  # Cyan
  "#6C8EBF",  # Steel Blue
  "#9C6ADE"   # Muted Purple
)

par(
  bg = "#0F1C2B",
  col.axis = "white",
  col.lab = "white",
  col.main = "white",
  col = "white"
)

plot(
  data_xts,
  main = "Scaled Sentiment Indicators (VADER)",
  col = adjustcolor(cols, alpha.f = 0.9),
  lwd = 2
)

legend(
  "topright",
  legend = legend_labels,
  col = cols,
  lwd = 3,
  cex = 0.85,
  bty = "n",
  text.col = "white",
  inset = 0.02,
  xpd = NA
)
