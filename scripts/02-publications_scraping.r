library("httr")
library("tidyverse")
library("glue")
library("progress")

# Get all paper metadata from Zenodo via a get request to their API

status <- "published"
page_size <- "200"

# Create empty dataframe with the columns we want

publications_df <- data.frame(title = character(), journal = character(), date = character(), description = character(), keywords = character(), community = character())

# Progress bar as the scraping can take several minutes

pb <- progress_bar$new(total = length(filtered_communities$id))

# Get all publications for each community

for(community_id in filtered_communities$id) {
  pb$tick()
  page_num <- 1
  
  while (TRUE) {
    endpoint_url <- glue("https://zenodo.org/api/records/?status={status}&page={page_num}&size={page_size}&communities={community_id}")
    request <- GET(endpoint_url)
    records <- content(request, as = "parsed")
    for (record in records) {
      pub_title <- record$metadata$title
      pub_journal <- record$metadata$journal_title
      pub_date <- record$metadata$publication_date
      pub_description <- record$metadata$description
      pub_keywords <- paste(record$metadata$keywords, collapse = ", ")
      publications_df <- publications_df %>%
        add_row(title = pub_title, journal = pub_journal, date = pub_date, description = pub_description, keywords = pub_keywords, community = rep(community_id, length(records)))
    }
    
    # If the retrieved data is not a full page then go to next community
    # Otherwise go to next page
    
    if (length(records) < page_size) {
      break
    } 
    else {
      page_num <- page_num + 1
    }
    
    # Wait to not get blocked
    
    Sys.sleep(10)
  }
}

# Remove duplicate entries

publications_df <- publications_df[!duplicated(publications_df), ]
