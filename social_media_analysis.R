args <- commandArgs(trailingOnly = TRUE)
script_arg <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
script_dir <- if (length(script_arg)) {
  dirname(normalizePath(sub("^--file=", "", script_arg[[1]])))
} else {
  getwd()
}

input_file <- if (length(args)) args[[1]] else file.path(script_dir, "social_media_usage.csv")
output_dir <- file.path(script_dir, "output")

if (!requireNamespace("ggplot2", quietly = TRUE)) {
  stop("Package 'ggplot2' is required. Install it with install.packages('ggplot2').")
}
if (!file.exists(input_file)) {
  stop("Input CSV not found: ", input_file)
}

usage <- read.csv(input_file, stringsAsFactors = FALSE)
required_columns <- c(
  "user_id", "age", "gender", "platform", "daily_usage_hours",
  "posts_per_day", "likes_received_per_day", "dominant_emotion"
)
missing_columns <- setdiff(required_columns, names(usage))
if (length(missing_columns)) {
  stop("Missing required columns: ", paste(missing_columns, collapse = ", "))
}
if (!nrow(usage)) {
  stop("The input CSV has no data rows.")
}

numeric_columns <- c("age", "daily_usage_hours", "posts_per_day", "likes_received_per_day")
for (column in numeric_columns) {
  original_values <- usage[[column]]
  converted_values <- suppressWarnings(as.numeric(original_values))
  if (any(!is.na(original_values) & is.na(converted_values))) {
    stop("Column '" , column, "' contains values that are not numeric.")
  }
  usage[[column]] <- converted_values
}

if (any(usage$daily_usage_hours < 0, na.rm = TRUE) ||
    any(usage$daily_usage_hours > 24, na.rm = TRUE)) {
  stop("'daily_usage_hours' must be between 0 and 24.")
}
if (any(usage$age < 0, na.rm = TRUE) ||
    any(usage$posts_per_day < 0, na.rm = TRUE) ||
    any(usage$likes_received_per_day < 0, na.rm = TRUE)) {
  stop("Age, posts, and likes cannot be negative.")
}

usage <- usage[!is.na(usage$platform) & nzchar(usage$platform), ]
if (!nrow(usage)) {
  stop("No rows have a valid platform value.")
}
usage$age_group <- cut(
  usage$age,
  breaks = c(-Inf, 17, 24, 34, 44, 54, Inf),
  labels = c("13-17", "18-24", "25-34", "35-44", "45-54", "55+"),
  right = TRUE
)

dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)
mean_or_na <- function(values) {
  if (all(is.na(values))) NA_real_ else mean(values, na.rm = TRUE)
}

overall_summary <- data.frame(
  measure = c("respondents", "mean_daily_usage_hours", "median_daily_usage_hours",
              "mean_posts_per_day", "mean_likes_received_per_day"),
  value = c(
    nrow(usage),
    mean_or_na(usage$daily_usage_hours),
    if (all(is.na(usage$daily_usage_hours))) NA_real_ else median(usage$daily_usage_hours, na.rm = TRUE),
    mean_or_na(usage$posts_per_day),
    mean_or_na(usage$likes_received_per_day)
  )
)
platform_summary <- aggregate(
  cbind(daily_usage_hours, posts_per_day, likes_received_per_day) ~ platform,
  data = usage,
  FUN = mean_or_na
)
platform_summary$respondents <- as.integer(table(usage$platform)[platform_summary$platform])
gender_summary <- aggregate(
  daily_usage_hours ~ gender,
  data = usage[!is.na(usage$gender) & nzchar(usage$gender), ],
  FUN = mean_or_na
)
gender_summary$respondents <- as.integer(table(usage$gender)[gender_summary$gender])
age_summary <- aggregate(
  daily_usage_hours ~ age_group,
  data = usage[!is.na(usage$age_group), ],
  FUN = mean_or_na
)
age_summary$respondents <- as.integer(table(usage$age_group)[as.character(age_summary$age_group)])
emotion_summary <- as.data.frame(table(usage$dominant_emotion, useNA = "no"), stringsAsFactors = FALSE)
names(emotion_summary) <- c("dominant_emotion", "respondents")
emotion_summary <- emotion_summary[order(-emotion_summary$respondents), ]

correlation_data <- usage[c("daily_usage_hours", "age", "posts_per_day", "likes_received_per_day")]
correlation_matrix <- cor(correlation_data, use = "pairwise.complete.obs")

write.csv(overall_summary, file.path(output_dir, "overall_summary.csv"), row.names = FALSE)
write.csv(platform_summary, file.path(output_dir, "platform_summary.csv"), row.names = FALSE)
write.csv(gender_summary, file.path(output_dir, "gender_summary.csv"), row.names = FALSE)
write.csv(age_summary, file.path(output_dir, "age_summary.csv"), row.names = FALSE)
write.csv(emotion_summary, file.path(output_dir, "emotion_summary.csv"), row.names = FALSE)
write.csv(correlation_matrix, file.path(output_dir, "correlations.csv"))

plots <- list(
  usage_by_platform = ggplot2::ggplot(platform_summary, ggplot2::aes(x = reorder(platform, daily_usage_hours), y = daily_usage_hours)) +
    ggplot2::geom_col(fill = "#287A78") +
    ggplot2::coord_flip() +
    ggplot2::labs(title = "Average daily use by platform", x = NULL, y = "Hours per day") +
    ggplot2::theme_minimal(base_size = 12),
  usage_distribution = ggplot2::ggplot(usage, ggplot2::aes(x = daily_usage_hours)) +
    ggplot2::geom_histogram(binwidth = 0.5, boundary = 0, fill = "#D98A45", color = "white") +
    ggplot2::labs(title = "Daily social media usage", x = "Hours per day", y = "Respondents") +
    ggplot2::theme_minimal(base_size = 12),
  usage_by_age = ggplot2::ggplot(usage, ggplot2::aes(x = age, y = daily_usage_hours)) +
    ggplot2::geom_point(color = "#287A78", alpha = 0.75) +
    ggplot2::geom_smooth(method = "lm", se = TRUE, color = "#B34E42") +
    ggplot2::labs(title = "Daily usage by age", x = "Age", y = "Hours per day") +
    ggplot2::theme_minimal(base_size = 12),
  emotions = ggplot2::ggplot(emotion_summary, ggplot2::aes(x = reorder(dominant_emotion, respondents), y = respondents)) +
    ggplot2::geom_col(fill = "#6573A6") +
    ggplot2::coord_flip() +
    ggplot2::labs(title = "Reported dominant emotion", x = NULL, y = "Respondents") +
    ggplot2::theme_minimal(base_size = 12)
)

for (plot_name in names(plots)) {
  ggplot2::ggsave(
    filename = file.path(output_dir, paste0(plot_name, ".png")),
    plot = plots[[plot_name]],
    width = 8,
    height = 5,
    dpi = 150
  )
}

cat("Analysis complete. Results saved to ", output_dir, "\n", sep = "")