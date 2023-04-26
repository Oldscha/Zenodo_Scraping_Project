library("tidyverse")
library("zen4R")

# Create Zenodo manager

zenodo <- ZenodoManager$new()

# Get all communities on Zenodo

communities <- zenodo$getCommunities()

# Return logical vector with entry TRUE if any keyword found

keyword_detect <- function(column, keywords) {
    include <- rep(FALSE, length(column))
    for (keyword in keywords) {
      
       # Set entry in vector to TRUE as soon as at least one keyword is found
      
       include <- include | str_detect(column, fixed(keyword, ignore_case=TRUE))
    }
    return(include)
}

# Filter communities based on keywords

first_keywords <- c("social", "psych", "socio")
filtered_communities <- communities %>% filter(keyword_detect(title, first_keywords) | keyword_detect(description, first_keywords))

# Secondary filter

second_keywords <- c("sociology", "psychology")
filtered_communities <- filtered_communities %>% filter(keyword_detect(title, second_keywords) | keyword_detect(description, second_keywords))

