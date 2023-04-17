library(httr)
library(tidyverse)
library(glue)
library(zen4R)
library("jsonlite")

# Get all paper metadata from Zenodo via a get request to their API
status <- "published"
size <- "1000"

# Create empty dataframe with the columns we want
publications_df <- data.frame(title = character(), journal = character(), date = character(), description = character(), keywords = character(), community = character())

for(community_id in filtered_communities$id) {
  endpoint_url <- glue("https://zenodo.org/api/records/?status={status}&size={size}&communities={community_id}")
  request <- GET(endpoint_url)
  records <- content(request, as = "parsed")
    
  for (record in records) {
    titles <- record$metadata$title
    journal_titles <- record$metadata$journal_title

    publication_dates <- record$metadata$publication_date
    keywords <- paste(record$metadata$keywords, collapse = ', ')
    
    publications_df <- publications_df %>% 
      add_row(title = title, journal = journal_titles, date = publication_dates, keywords = keywords, community = rep(community_id, length(records)))
  }
  
}

# Remove duplicate rows
publications_df <- publications_df[!duplicated(publications_df), ]


