# Clean up data and anonymize sensitive fields

library(digest)
library(readr)
library(dplyr)

amazon_header <- read_csv("data/Frame Study - Amazon_February 9, 2018_13.18.csv", n_max = 1, col_names = F) %>% as.character()
amazon <- read_csv("data/Frame Study - Amazon_February 9, 2018_13.18.csv", skip = 3, col_names = amazon_header)
reddit_header <- read_csv("data/Frame Study - reddit-final.csv", n_max = 1, col_names = F) %>% as.character()
reddit <- read_csv("data/Frame Study - reddit-final.csv", skip = 3, col_names = reddit_header)

clean_data <- function(df) {
  df %>%
    select(-matches("Click|Page")) %>%
    mutate_at(.vars = vars(matches("MTurkCode|IP|Recipient|IPAddress")), .funs = funs(digest)) %>%
    # Fuzz IP address-based location
    mutate_at(.vars = vars(matches("Location")), ~. + rnorm(length(.), sd = 0.1)) %>%
    magrittr::set_names(stringr::str_replace(names(.), "Location", "Approx"))
}

amazon_clean <- clean_data(amazon)
reddit_clean <- clean_data(reddit)

write_csv(amazon_clean, "Amazon_Data_Anon.csv")
write_csv(reddit_clean, "Reddit_Data_Anon.csv")

