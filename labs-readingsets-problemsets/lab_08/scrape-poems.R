poem_list_url <- "https://en.wikipedia.org/wiki/List_of_Emily_Dickinson_poems"
paths_allowed(poem_list_url)

poem_table <- poem_list_url %>%
  read_html() %>%
  html_elements("table") %>%
  pluck(1) %>%
  html_table() %>%
  janitor::clean_names() %>%
  select(title = first_line_often_used_as_title)

url_href <- poem_list_url %>% 
  read_html() %>% 
  html_elements("#mw-content-text > div.mw-parser-output > table > tbody > tr > td > a") %>% 
  html_attr("href")

# Grab display text of each URL (i.e., poem title)
url_table <- poem_list_url %>% 
  read_html() %>% 
  html_elements("#mw-content-text > div.mw-parser-output > table > tbody > tr > td > a") %>% 
  html_text()

url_table <- tibble(title = url_text, href = url_href) %>%
  filter(grepl("https:", url_href))

poem_table <- poem_table %>%
  left_join(url_table)

# Identify number of iterations (start with 1, 5, 20, 50, etc.)
n_links <- nrow(poem_table)

# Pre-allocate space in dataframe for poem text
poem_tibble <- poem_table[1:n_links, ] %>% 
  mutate(text = "") 

# Iterate through links to grab text
for(i in seq_len(n_links)){
  
  # Identify URL
  link <- poem_tibble$href[i]
  
  # Scrape poem title and text 
  poem_tibble$text[i] <- tryCatch(
    
    # Return "Missing" instead of poem text when error is thrown
    error = function(cnd) {
      return("Missing")
    },
    
    # Scrape text otherwise
    text <- link %>%               
      read_html() %>%
      # a. Get list of "div p" elements on the page 
      html_elements("div p") %>% 
      # b. `Pluck` poem from list and grab text 
      pluck(1) %>% 
      html_text()
  )
}

write_csv(poem_tibble, "dickinson-poems.txt")

test <- read_csv("dickinson-poems.txt")
