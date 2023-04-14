library(httr)
library(tidyverse)
library(glue)
library(zen4R)

# Get all paper metadata from Zenodo via a get request to their API
status <- "published"
size <- "100"
communities <- "css"
endpoint_url <- glue("https://zenodo.org/api/records/?status={status}&size={size}&communities={communities}")

request <- GET(endpoint_url)
records <- content(request, as = "parsed")

# Create empty dataframe with the columns we want
publications_df <- data.frame(title = character(), authors = character(), date = character(), description = character(), keywords = character())

for (record in records) {
    title <- record$metadata$title
    authors <- record$metadata$creators

    author_names <- c()
    for (author in authors) {
        author_names <- c(author_names, c(author$name))
    }

    author_names <- paste(author_names, collapse = ', ')
    date <- record$metadata$publication_date
    keywords <- paste(record$metadata$keywords, collapse = ', ')

    publications_df <- publications_df %>% 
        add_row(title = title, authors = author_names, date = date, keywords = keywords)
}
