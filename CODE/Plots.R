library(DBI)
library(RPostgres)
library(zoo)
library(dplyr)
library(tidyr)
library(ggplot2)
library(scales)
library(readxl)
library(lubridate)
library(xts)
###### LOAD DATA
con <- dbConnect(
  RPostgres::Postgres(),
  dbname = "fed_speeches",
  host   = "localhost",
  port   = "5432",
  user   = "borisgerat"
)

stopifnot(dbIsValid(con))

data <- dbReadTable(con, "sentiment_base")


###### CLEAN + FILTER
data$Quarter <- as.yearqtr(data$Quarter, format = "%Y Q%q")

data <- data %>%
  filter(
    Quarter >= as.yearqtr("1995 Q1"),
    Quarter <= as.yearqtr("2025 Q4")
  ) %>%
  mutate(Date = as.Date(Quarter)) %>%
  arrange(Date)

###### SELECT + SCALE (z-score)
vars <- c("gdelt_avgTone", "fed_minutes_vader", "gov_vader", "fed_speech_vader", "sec_vader")

scaled_mat <- scale(as.matrix(data[, vars]))
scaled_df  <- as.data.frame(scaled_mat)
scaled_df$Date <- data$Date

###### LONG FORMAT FOR GGPLOT
plot_df <- scaled_df %>%
  pivot_longer(-Date, names_to = "series", values_to = "value") %>%
  mutate(
    series = recode(
      series,
      gdelt_avgTone       = "GDELT Avg Tone",
      fed_minutes_vader   = "FED Minutes Sentiment",
      gov_vader           = "Government Sentiment",
      fed_speech_vader    = "FED Speech Sentiment",
      sec_vader           = "SEC Sentiment"
    )
  )

###### COLORS (match your palette to the recoded names)
cols <- c(
  "GDELT Avg Tone"         = "#00AEEF",
  "FED Minutes Sentiment"  = "#1F3B73",
  "Government Sentiment"   = "#17BECF",
  "FED Speech Sentiment"   = "#6C8EBF",
  "SEC Sentiment"          = "#9C6ADE"
)

p <- ggplot(plot_df, aes(x = Date, y = value, color = series)) +
  geom_line(linewidth = 1.1) +

  scale_x_date(
    date_breaks = "1 year",
    date_labels = "%Y",
    expand = expansion(mult = c(0.01, 0.01))
  ) +

  scale_color_manual(values = cols) +

  labs(
    title = "Scaled Sentiment Indicators (VADER)",
    x = NULL,
    y = "Standardized value (z-score)",
    color = NULL
  ) +

  theme_minimal(base_size = 12) +
  theme(
    # White background
    plot.background  = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA),

    # Subtle grid
    panel.grid.major.x = element_line(color = "grey85"),
    panel.grid.major.y = element_line(color = "grey88"),
    panel.grid.minor   = element_blank(),

    # Axis styling
    axis.text  = element_text(color = "black"),
    axis.title = element_text(color = "black"),

    # CENTER + enlarge title
    plot.title = element_text(
      color = "black",
      face = "bold",
      size = 18,
      hjust = 0.5   # center
    ),

    # Legend inside top-right
    legend.position = c(0.98, 0.98),
    legend.justification = c(1, 1),
    legend.background = element_blank(),
    legend.key        = element_blank(),
    legend.text       = element_text(color = "black", size = 11)
  ) +
  guides(color = guide_legend(override.aes = list(linewidth = 2)))

print(p)


########### FED only VADER


# Filter only FED sentiment series
fed_df <- plot_df %>%
  filter(series %in% c("FED Minutes Sentiment",
                       "FED Speech Sentiment"))

# FED-specific color mapping
fed_cols <- c(
  "FED Minutes Sentiment" = "#1F3B73",  # Deep Navy
  "FED Speech Sentiment"  = "#6C8EBF"   # Steel Blue
)

p_fed <- ggplot(fed_df, aes(x = Date, y = value, color = series)) +
  geom_line(linewidth = 1.3) +

  scale_x_date(
    date_breaks = "1 year",
    date_labels = "%Y",
    expand = expansion(mult = c(0.01, 0.01))
  ) +

  scale_color_manual(values = fed_cols) +

  labs(
    title = "FED Sentiment Indicators (Scaled)",
    x = NULL,
    y = "Standardized value (z-score)",
    color = NULL
  ) +

  theme_minimal(base_size = 12) +
  theme(
    plot.background  = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA),

    panel.grid.major.x = element_line(color = "grey85"),
    panel.grid.major.y = element_line(color = "grey88"),
    panel.grid.minor   = element_blank(),

    axis.text  = element_text(color = "black"),
    axis.title = element_text(color = "black"),

    plot.title = element_text(
      color = "black",
      face = "bold",
      size = 18,
      hjust = 0.5
    ),

    legend.position = c(0.98, 0.98),
    legend.justification = c(1, 1),
    legend.background = element_blank(),
    legend.key        = element_blank(),
    legend.text       = element_text(color = "black", size = 11)
  ) +
  guides(color = guide_legend(override.aes = list(linewidth = 2)))

print(p_fed) 




######## OTHER SERIES
#######

# Filter remaining variables
other_df <- plot_df %>%
  filter(series %in% c("GDELT Avg Tone",
                       "Government Sentiment",
                       "SEC Sentiment")) %>%
  mutate(
    value = ifelse(
      series == "Government Sentiment" & Date < as.Date("1997-01-01"),
      NA,
      value
    )
  )

other_cols <- c(
  "GDELT Avg Tone"       = "#00AEEF",
  "Government Sentiment" = "#17BECF",
  "SEC Sentiment"        = "#9C6ADE"
)

p_other <- ggplot(other_df, aes(x = Date, y = value, color = series)) +
  geom_line(linewidth = 1.3, na.rm = TRUE) +

  scale_x_date(
    date_breaks = "1 year",
    date_labels = "%Y",
    expand = expansion(mult = c(0.01, 0.01))
  ) +

  scale_color_manual(values = other_cols) +

  labs(
    title = "Non-FED Sentiment Indicators (Scaled)",
    x = NULL,
    y = "Standardized value (z-score)",
    color = NULL
  ) +

  theme_minimal(base_size = 12) +
  theme(
    plot.background  = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA),

    panel.grid.major.x = element_line(color = "grey85"),
    panel.grid.major.y = element_line(color = "grey88"),
    panel.grid.minor   = element_blank(),

    axis.text  = element_text(color = "black"),
    axis.title = element_text(color = "black"),

    plot.title = element_text(
      color = "black",
      face = "bold",
      size = 18,
      hjust = 0.5
    ),

    legend.position = c(0.98, 0.98),
    legend.justification = c(1, 1),
    legend.background = element_blank(),
    legend.key        = element_blank(),
    legend.text       = element_text(color = "black", size = 11)
  ) +
  guides(color = guide_legend(override.aes = list(linewidth = 2)))

print(p_other)



########## FINBERT STUFFF
#########
########### FINBERT VERSION ###########

# --- Select + scale (z-score)
vars_finbert <- c(
  "gdelt_avgTone",
  "fed_minutes_finbert",
  "gov_finbert",
  "fed_speech_finbert",
  "sec_finbert"
)

scaled_mat_finbert <- scale(as.matrix(data[, vars_finbert]))
scaled_df_finbert  <- as.data.frame(scaled_mat_finbert)
scaled_df_finbert$Date <- data$Date

# --- Long format
plot_df_finbert <- scaled_df_finbert %>%
  pivot_longer(-Date, names_to = "series", values_to = "value") %>%
  mutate(
    series = recode(
      series,
      gdelt_avgTone         = "GDELT Avg Tone",
      fed_minutes_finbert   = "FED Minutes Sentiment (FinBERT)",
      gov_finbert           = "Government Sentiment (FinBERT)",
      fed_speech_finbert    = "FED Speech Sentiment (FinBERT)",
      sec_finbert           = "SEC Sentiment (FinBERT)"
    )
  )

# --- Colors (keep your style)
cols_finbert <- c(
  "GDELT Avg Tone"                    = "#00AEEF",
  "FED Minutes Sentiment (FinBERT)"   = "#1F3B73",
  "Government Sentiment (FinBERT)"    = "#17BECF",
  "FED Speech Sentiment (FinBERT)"    = "#6C8EBF",
  "SEC Sentiment (FinBERT)"           = "#9C6ADE"
)

# --- 1) All series plot
p_finbert <- ggplot(plot_df_finbert, aes(x = Date, y = value, color = series)) +
  geom_line(linewidth = 1.1) +
  scale_x_date(
    date_breaks = "1 year",
    date_labels = "%Y",
    expand = expansion(mult = c(0.01, 0.01))
  ) +
  scale_color_manual(values = cols_finbert) +
  labs(
    title = "Scaled Sentiment Indicators (FinBERT)",
    x = NULL,
    y = "Standardized value (z-score)",
    color = NULL
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.background  = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA),
    panel.grid.major.x = element_line(color = "grey85"),
    panel.grid.major.y = element_line(color = "grey88"),
    panel.grid.minor   = element_blank(),
    axis.text  = element_text(color = "black"),
    axis.title = element_text(color = "black"),
    plot.title = element_text(color = "black", face = "bold", size = 18, hjust = 0.5),
    legend.position = c(0.98, 0.98),
    legend.justification = c(1, 1),
    legend.background = element_blank(),
    legend.key        = element_blank(),
    legend.text       = element_text(color = "black", size = 11)
  ) +
  guides(color = guide_legend(override.aes = list(linewidth = 2)))

print(p_finbert)


########### FED-only FINBERT ###########

fed_finbert_df <- plot_df_finbert %>%
  filter(series %in% c("FED Minutes Sentiment (FinBERT)",
                       "FED Speech Sentiment (FinBERT)"))

fed_finbert_cols <- c(
  "FED Minutes Sentiment (FinBERT)" = "#1F3B73",
  "FED Speech Sentiment (FinBERT)"  = "#6C8EBF"
)

p_finbert_fed <- ggplot(fed_finbert_df, aes(x = Date, y = value, color = series)) +
  geom_line(linewidth = 1.3) +
  scale_x_date(
    date_breaks = "1 year",
    date_labels = "%Y",
    expand = expansion(mult = c(0.01, 0.01))
  ) +
  scale_color_manual(values = fed_finbert_cols) +
  labs(
    title = "FED Sentiment Indicators (FinBERT, Scaled)",
    x = NULL,
    y = "Standardized value (z-score)",
    color = NULL
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.background  = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA),
    panel.grid.major.x = element_line(color = "grey85"),
    panel.grid.major.y = element_line(color = "grey88"),
    panel.grid.minor   = element_blank(),
    axis.text  = element_text(color = "black"),
    axis.title = element_text(color = "black"),
    plot.title = element_text(color = "black", face = "bold", size = 18, hjust = 0.5),
    legend.position = c(0.98, 0.98),
    legend.justification = c(1, 1),
    legend.background = element_blank(),
    legend.key        = element_blank(),
    legend.text       = element_text(color = "black", size = 11)
  ) +
  guides(color = guide_legend(override.aes = list(linewidth = 2)))

print(p_finbert_fed)


########### OTHER (Non-FED) FINBERT ###########
# GOV starts from 1997

other_finbert_df <- plot_df_finbert %>%
  filter(series %in% c("GDELT Avg Tone",
                       "Government Sentiment (FinBERT)",
                       "SEC Sentiment (FinBERT)")) %>%
  mutate(
    value = ifelse(
      series == "Government Sentiment (FinBERT)" & Date < as.Date("1997-01-01"),
      NA, value
    )
  )

other_finbert_cols <- c(
  "GDELT Avg Tone"                 = "#00AEEF",
  "Government Sentiment (FinBERT)" = "#17BECF",
  "SEC Sentiment (FinBERT)"        = "#9C6ADE"
)

p_finbert_other <- ggplot(other_finbert_df, aes(x = Date, y = value, color = series)) +
  geom_line(linewidth = 1.3, na.rm = TRUE) +
  scale_x_date(
    date_breaks = "1 year",
    date_labels = "%Y",
    expand = expansion(mult = c(0.01, 0.01))
  ) +
  scale_color_manual(values = other_finbert_cols) +
  labs(
    title = "Non-FED Sentiment Indicators (FinBERT, Scaled)",
    x = NULL,
    y = "Standardized value (z-score)",
    color = NULL
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.background  = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA),
    panel.grid.major.x = element_line(color = "grey85"),
    panel.grid.major.y = element_line(color = "grey88"),
    panel.grid.minor   = element_blank(),
    axis.text  = element_text(color = "black"),
    axis.title = element_text(color = "black"),
    plot.title = element_text(color = "black", face = "bold", size = 18, hjust = 0.5),
    legend.position = c(0.98, 0.98),
    legend.justification = c(1, 1),
    legend.background = element_blank(),
    legend.key        = element_blank(),
    legend.text       = element_text(color = "black", size = 11)
  ) +
  guides(color = guide_legend(override.aes = list(linewidth = 2)))

print(p_finbert_other)



######### FED RISK INDEXES PLOT
########

data_risk <- read_excel(
  "/Users/borisgerat/Documents/Projects/MA_Thesis/DATA_MAIN_MA.xlsx",
  sheet = "FED_RISK"
)

# Force Time to be Date (handles Excel serial dates too)
if (inherits(data_risk$Time, "numeric")) {
  data_risk <- data_risk %>% mutate(Time = as.Date(Time, origin = "1899-12-30"))
} else {
  data_risk <- data_risk %>% mutate(Time = as.Date(Time))
}

data_risk <- data_risk %>%
  filter(Time >= as.Date("1995-01-01")) %>%
  arrange(Time)

risk_long <- data_risk %>%
  pivot_longer(
    cols = c(STLFSI, NFCI, KCFSI, VIX),
    names_to = "Index",
    values_to = "Value"
  )

risk_cols <- c(
  "STLFSI" = "#1F77B4",   # Quant Blue
  "NFCI"   = "#C9A227",   # Muted Gold
  "KCFSI"  = "#58508D",   # Indigo
  "VIX"    = "#A60628"    # Deep Professional Red
)

risk_long <- risk_long %>%
  group_by(Index) %>%
  mutate(Value = scale(Value)) %>%
  ungroup()

p_risk <- ggplot(risk_long, aes(x = Time, y = Value, color = Index)) +
  geom_line(linewidth = 1.2, na.rm = TRUE) +
  scale_color_manual(values = risk_cols) +
  scale_x_date(
    date_breaks = "2 years",
    date_labels = "%Y",
    expand = expansion(mult = c(0.01, 0.01))
  ) +
 labs(
  title = "Financial Stress Indicators and Market Volatility (Standardized)",
  x = NULL,
  y = "Standardized Index (Z-Score)",
  color = NULL
  )+
  theme_minimal(base_size = 13) +
  theme(
    plot.background  = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA),
    panel.grid.major.x = element_line(color = "grey85"),
    panel.grid.major.y = element_line(color = "grey88"),
    panel.grid.minor   = element_blank(),
    axis.text  = element_text(color = "black"),
    axis.title = element_text(color = "black"),
    plot.title = element_text(face = "bold", size = 20, hjust = 0.5),
    legend.position = c(0.98, 0.98),
    legend.justification = c(1, 1),
    legend.background = element_blank(),
    legend.key        = element_blank(),
    legend.text       = element_text(size = 12)
  ) +
  guides(color = guide_legend(override.aes = list(linewidth = 2)))

print(p_risk)



######## SENTIMENT BENCHMARK
#######

# -----------------------------
# 1) Load + clean dates
# -----------------------------
shapiro_raw <- read_excel(
  "/Users/borisgerat/Documents/Projects/MA_Thesis/DATA_MAIN_MA.xlsx",
  sheet = "Shapiro"
)

# ensure Date class
shapiro_raw$date <- as.Date(shapiro_raw$date)

shapiro_xts <- xts(shapiro_raw$NSI_Shapiro, order.by = shapiro_raw$date)

# quarterly mean
shapiro_q <- apply.quarterly(shapiro_xts, function(z) mean(z, na.rm = TRUE))
colnames(shapiro_q) <- "Shapiro_NSI"


epui <- read_excel("/Users/borisgerat/Documents/Projects/MA_Thesis/DATA_MAIN_MA.xlsx",
                   sheet = "Sentiment_Benchmark")

# parse Time safely (handles Excel serial too)
if (inherits(epui$Time, "numeric")) {
  epui$Time <- as.Date(epui$Time, origin = "1899-12-30")
} else {
  epui$Time <- as.Date(epui$Time)
}

epui <- epui %>% filter(!is.na(Time))

epu_q <- xts(epui$EPUI, order.by = epui$Time)
colnames(epu_q) <- "EPUI"

# -----------------------------
# 2) Merge + start from 1995
# -----------------------------

# move index to quarter end consistently
index(shapiro_q) <- as.Date(as.yearqtr(index(shapiro_q)), frac = 1)
index(epu_q)     <- as.Date(as.yearqtr(index(epu_q)), frac = 1)

sent_q <- merge(shapiro_q, epu_q, join = "inner")
sent_q <- sent_q[index(sent_q) >= as.Date("1995-01-01")]
# (optional but often smart for comparing levels)
# sent_q <- scale(sent_q)  # uncomment if you want z-scores

# Standardize (column-wise z-score)
sent_q_std <- scale(sent_q)

# Convert back to xts (scale() drops xts class)
sent_q_std <- xts(
  sent_q_std,
  order.by = index(sent_q)
)

colnames(sent_q_std) <- colnames(sent_q)


sent_q_std_adj <- sent_q_std
sent_q_std_adj[,"EPUI"] <- -1 * sent_q_std_adj[,"EPUI"]   # <--- flip EPUI
# If you prefer flipping Shapiro instead, do:
# sent_q_std_adj[,"Shapiro_NSI"] <- -1 * sent_q_std_adj[,"Shapiro_NSI"]

sent_df <- data.frame(
  Time = index(sent_q_std_adj),
  Shapiro_NSI = as.numeric(sent_q_std_adj$Shapiro_NSI),
  EPUI = as.numeric(sent_q_std_adj$EPUI)
)

sent_long <- sent_df %>%
  pivot_longer(
    cols = c(Shapiro_NSI, EPUI),
    names_to = "Series",
    values_to = "Value"
  ) %>%
  mutate(
    Series = factor(Series, levels = c("Shapiro_NSI", "EPUI")),
    Series = recode(Series,
      "Shapiro_NSI" = "Shapiro NSI (z)",
      "EPUI"        = "EPUI (z, flipped)"
    )
  )

# Your risk palette family
risk_cols <- c(
  "STLFSI" = "#1F77B4",
  "NFCI"   = "#C9A227",
  "KCFSI"  = "#58508D",
  "VIX"    = "#A60628"
)

# Colors that match the *final legend labels* above
sent_cols <- c(
  "Shapiro NSI (z)"      = unname(risk_cols["KCFSI"]),  # Indigo
  "EPUI (z, flipped)"    = unname(risk_cols["NFCI"])    # Gold
)

p_sent <- ggplot(sent_long, aes(x = Time, y = Value, color = Series)) +
  geom_line(linewidth = 1.25, na.rm = TRUE) +
  scale_color_manual(values = sent_cols) +
  scale_x_date(
    date_breaks = "2 years",
    date_labels = "%Y",
    expand = expansion(mult = c(0.01, 0.01))
  ) +
  labs(
    title = "Sentiment Benchmarks (Standardized, Quarterly)",
    x = NULL,
    y = "Z-Score",
    color = NULL
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.background  = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA),
    panel.grid.major.x = element_line(color = "grey85"),
    panel.grid.major.y = element_line(color = "grey88"),
    panel.grid.minor   = element_blank(),
    axis.text  = element_text(color = "black"),
    plot.title = element_text(face = "bold", size = 18, hjust = 0.5),

    # Legend visible + clean
    legend.position = "top",
    legend.justification = "left",
    legend.background = element_blank(),
    legend.key = element_blank(),
    legend.text = element_text(size = 12)
  ) +
  guides(color = guide_legend(override.aes = list(linewidth = 2.2)))

print(p_sent)
