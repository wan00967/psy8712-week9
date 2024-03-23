# Script Settings and Resources
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(tidyverse)
library(RedditExtractoR)

# Data Import and Cleaning
# Use RedditExtractoR to fetch data from the r/rstats subreddit
# find_thread_urls() fetches URLs of posts from the last month in r/rstats
# The curly braces {} allow us to directly access the URLs from the previous step's result
# get_thread_content() retrieves content for each thread based on their URLs
# Selects and renames relevant columns for analysis and converts the result into a tibble
rstats_tbl <- find_thread_urls(subreddit = "rstats", period = "month") %>%
  {get_thread_content(.$url)} %>%
  .$threads %>%
  select(post = title, upvotes = score, comments) %>%
  as_tibble()

# Visualization
# Create a scatter plot to visualize the relationship between upvotes and comments
ggplot(rstats_tbl, aes(x = upvotes, y = comments)) +
  geom_point() +
  geom_smooth(method = "lm")

# Analysis
# Calculate the correlation between upvotes and comments and perform a significance test
cor_test_result <- cor.test(rstats_tbl$upvotes, rstats_tbl$comments)
cor_test_result

# Publication
# "The correlation between upvotes and comments was r(118) = .53, p = .00. This test was statistically significant."
# constructing and displaying the summary of correlation test results
summary_text <- sprintf("The correlation between upvotes and comments was r(%d) = %.2f, p = %.2f. This test %s statistically significant.",
                        cor_test_result$parameter,
                        cor_test_result$estimate,
                        cor_test_result$p.value,
                        ifelse(cor_test_result$p.value < 0.05, "was", "was not"))
summary_text <- gsub(" = 0\\.", " = .", summary_text)
summary_text



