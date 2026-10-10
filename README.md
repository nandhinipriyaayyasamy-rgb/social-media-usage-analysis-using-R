# Social Media Usage Analysis (R)

This project summarizes a respondent-level CSV and creates summary tables and plots. The included `social_media_usage.csv` contains synthetic example data for demonstrating the workflow; it is not a real survey.

## Requirements

- R
- The `ggplot2` package

Install the package once in R:

```r
install.packages("ggplot2")
```

## Run

From this folder, run:

```sh
Rscript social_media_analysis.R
```

To analyze another CSV, pass its path:

```sh
Rscript social_media_analysis.R path/to/your_data.csv
```

The script writes CSV summaries and PNG charts to `output/`.

## CSV columns

Provide one row per respondent with these column names:

| Column | Meaning |
| --- | --- |
| `user_id` | Anonymous respondent identifier |
| `age` | Respondent age in years |
| `gender` | Self-reported gender category |
| `platform` | Primary social media platform |
| `daily_usage_hours` | Average hours spent per day, from 0 to 24 |
| `posts_per_day` | Average posts per day |
| `likes_received_per_day` | Average likes received per day |
| `dominant_emotion` | Respondent's reported dominant emotion while using social media |

The script reports overall metrics, average usage and activity by platform, average usage by gender and age group, emotion counts, and pairwise correlations. Interpret comparisons as descriptive, not causal; results depend on how the survey was sampled and measured.