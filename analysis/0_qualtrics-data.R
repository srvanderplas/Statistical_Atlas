read_and_clean <- function(file) {
  responses <- read.csv(file)
  responses <- responses[-(1:2),]
  library(tidyverse)
  responses <- responses %>% rename(
    Age=Q56,
    Gender = Q520,
    Education=Q524
  )
  responses <- filter(responses, Finished=="True")
  
  percentages <- responses %>% select(IPAddress, ResponseId,  grep("Q...$", names(responses), value=TRUE), grep("Q..$", names(responses), value=TRUE)) %>% gather(question, howmuch, matches("Q..."), matches("Q.."))
  percentages$howmuch <- as.numeric(percentages$howmuch)
  percentages <- percentages %>% filter(howmuch > 0)
#  percentages <- percentages %>% filter(howmuch < 100)
  idx <- which(percentages$howmuch < 1)
  percentages$howmuch[idx] <- percentages$howmuch[idx]*100
  
  
  percentages %>% group_by(ResponseId) %>% summarize(n=length(!is.na(howmuch)))
  
  pl <- read.csv(file.path(path, "PlotLabels.csv"))
  percentages <- percentages %>% left_join(pl %>% select(Question, perc, Type, Frame, isFrame), by=c("question"="Question"))
  
  #####################
  percentages <- percentages %>% filter(howmuch < 75) %>% na.omit() %>% mutate(
    rel.error = (howmuch-perc)/perc,
    diff.error = howmuch-perc
  )
  
  percentages$frameframe <- 0
  percentages <- percentages %>% mutate(
    frameframe = replace(frameframe, Frame & !isFrame, 1),
    frameframe = replace(frameframe, Frame & isFrame, 2)
  )
  percentages$frameframe <- c("Unframed", "Framed-inside", "Framed-frame")[percentages$frameframe+1]
  percentages$frameframe <- factor(percentages$frameframe, levels=c("Unframed", "Framed-inside", "Framed-frame"))
  percentages
}
path <- "../data"
amazon <- read_and_clean("../analysis/Frame Study - Amazon_February 9, 2018_13.18.csv")
reddit <- read_and_clean("../analysis/Frame Study - reddit-final.csv")
amazon$source <- "amazon"
reddit$source <- "reddit"
percentages <- rbind(amazon, reddit)

write.csv(percentages, "study-results.csv", row.names=FALSE)
