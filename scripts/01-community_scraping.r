library(httr)
library(tidyverse)
library(glue)
library(zen4R)

# Instance of Zenodo manager
zenodo <- ZenodoManager$new()

# Get all communities on Zenodo
#communities <- zenodo$getCommunities()

# write.csv(communities, "all_communities.csv")

# Uncomment lines above after finished coding and delete line below
communities <- read.csv("all_communities.csv")

# Return logical array with entry TRUE if any keyword present
keyword_detect <- function(column, keywords) {
    include <- rep(FALSE, length(column))
    for (keyword in keywords) {
       include <- include | str_detect(column, fixed(keyword, ignore_case=TRUE))
    }
    return(include)
}

# Filter communities based on keywords
first_keywords <- c("social", "psych", "socio")
soc_communities <- communities %>% filter(keyword_detect(title, first_keywords) | keyword_detect(description, first_keywords))

# Secondary filter 
second_keywords <- c("computational")
css_communities <- soc_communities %>% filter(keyword_detect(title, second_keywords) | keyword_detect(description, second_keywords))
