library(digest)
library(readr)
library(dplyr)
library(tidyr)


# ---- Code to anonymize identifiable data ----
# # Read in original data
# amazon_header <- read_csv("../data/Frame Study - Amazon_February 9, 2018_13.18.csv", n_max = 1, col_names = F) %>% as.character()
# amazon <- read_csv("../data/Frame Study - Amazon_February 9, 2018_13.18.csv", skip = 3, col_names = amazon_header)
# amazon$source <- "amazon"
# reddit_header <- read_csv("../data/Frame Study - reddit-final.csv", n_max = 1, col_names = F) %>% as.character()
# reddit <- read_csv("../data/Frame Study - reddit-final.csv", skip = 3, col_names = reddit_header)
# reddit$source <- "reddit"
#
# # Function to hash sensitive information
# digestvec <- function(x) {
#   lapply(x, function(y) digest(as.character(y))) %>% unlist()
# }
#
# # Function to anonymize all sensitive fields and filter out unnecessary columns
# anon_data <- function(df) {
#   df %>%
#     select(-matches("Click|Page")) %>%
#     mutate_at(.vars = vars(matches("MTurkCode|IP|Recipient|IPAddress")), .funs = funs(digestvec)) %>%
#     # Fuzz IP address-based location
#     mutate_at(.vars = vars(matches("Location")), ~. + rnorm(length(.), sd = 0.1)) %>%
#     magrittr::set_names(stringr::str_replace(names(.), "Location", "Approx"))
# }
#
# # Anonymize data and write to csv
# amazon_anon <- anon_data(amazon)
# reddit_anon <- anon_data(reddit)
#
# write_csv(amazon_anon, "../data/Amazon_Data_Anon.csv")
# write_csv(reddit_anon, "../data/Reddit_Data_Anon.csv")


# ---- Reproducible Pipeline ----

coltypes <- paste0("TTccidlTc____dd__c", paste0(rep("c", 56), collapse = ""), "_", "c")
amazon <- read_csv("../data/Amazon_Data_Anon.csv", col_types = coltypes)
reddit <- read_csv("../data/Reddit_Data_Anon.csv", col_types = coltypes)

user_responses <- bind_rows(amazon, reddit)

responses <- user_responses %>% rename(
  Age = Q56,
  Gender = Q520,
  Education = Q524
)
responses <- filter(responses, Finished)


percentages <- responses %>%
  dplyr::select(IPAddress, ResponseId,  starts_with("Q")) %>%
  gather(key = question, value = howmuch, -IPAddress, -ResponseId)
percentages$howmuch <- str_replace(percentages$howmuch, "\\.{2}", "") %>%
  str_replace("~", "")

percentages$howmuch <- as.numeric(percentages$howmuch)
percentages <- percentages %>% filter(howmuch > 0)
percentages <- percentages %>% filter(howmuch < 100)
idx <- which(percentages$howmuch < 1)
percentages$howmuch[idx] <- percentages$howmuch[idx]*100


# percentages %>% group_by(ResponseId) %>% summarize(n=length(!is.na(howmuch)))

pl <- read.csv("../data/PlotLabels.csv")
percentages <- percentages %>% left_join(pl %>% select(Question, perc, Type, Frame, isFrame), by=c("question"="Question"))


percentages <- percentages %>%
  # I can't understand what this filter is doing (or if it just wasn't reported???)
  # filter(howmuch < 75) %>%
  na.omit() %>%
  mutate(
    rel.error = (howmuch-perc)/perc,
    diff.error = howmuch-perc,
    log.error = abs(log(howmuch) - log(perc)),
    includeObs = log.error < log(5)
  )

percentages$frameframe <- 0
percentages <- percentages %>% mutate(
  frameframe = replace(frameframe, Frame & !isFrame, 1),
  frameframe = replace(frameframe, Frame & isFrame, 2)
)
percentages$frameframe <- c("Unframed", "Framed-inside", "Framed-frame")[percentages$frameframe+1]
percentages$frameframe <- factor(percentages$frameframe, levels=c("Unframed", "Framed-inside", "Framed-frame"))
percentages


