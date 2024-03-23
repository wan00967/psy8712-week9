## Script Settings and Resources
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(tidyverse)
library(rvest)

# Data Import and Cleaning
# Define the URLs for the four sections
urls <- c(
  Business = "https://www.cnbc.com/business/",
  Investing = "https://www.cnbc.com/investing/",
  Tech = "https://www.cnbc.com/technology/",
  Politics = "https://www.cnbc.com/politics/"
)

# empty tibble to store results
cnbc_tbl <- tibble(headline = character(), length = integer(), source = character())

# Loop through each URL to scrape data
for (source in names(urls)) {
  page_content <- read_html(urls[source]) %>%
    html_nodes(".Card-title, .RiverHeadline-headline") %>%
    html_text(trim = TRUE) %>%
    tibble(headline = ., source = source) %>%
    mutate(length = str_count(headline, boundary("word"))) # Calculate headline length
  
  # Append the data to the main tibble
  cnbc_tbl <- bind_rows(cnbc_tbl, page_content)
}

# Visualization
ggplot(cnbc_tbl, aes(x = source, y = length, fill = source)) +
  geom_boxplot()

# Analysis
# Running an ANOVA to compare headline lengths across the four sources
anova_result <- aov(length ~ source, data = cnbc_tbl)
summary_anova <- summary(anova_result)

# Publication
# "The results of an ANOVA comparing lengths across sources was F(3, 130) = 4.13, p = .01. This test was statistically significant."
# Extract the values from the ANOVA summary
F_value <- summary_anova[[1]]$`F value`[1]
p_value <- summary_anova[[1]]$`Pr(>F)`[1]
dfn <- summary_anova[[1]]$Df[1] # degrees of freedom for the numerator (model)
dfd <- summary_anova[[1]]$Df[2] # degrees of freedom for the denominator (residuals)

# summary text with placeholders for dynamic content
summary_text <- sprintf("The results of an ANOVA comparing lengths across sources was F(%d, %d) = %.2f, p = %.2f. This test %s statistically significant.",
                        dfn, dfd, F_value, p_value,
                        ifelse(p_value < 0.05, "was", "was not"))

# remove leading zeros before the decimal point for the p-value
summary_text <- gsub(" p = 0\\.", " p = .", summary_text)

summary_text
