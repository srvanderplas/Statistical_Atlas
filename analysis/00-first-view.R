#responses <- read.csv("/Users/heike/Downloads/Frame Study - Pics_November 20, 2017_14.35.csv")

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
  percentages <- percentages %>% filter(howmuch < 100)
  idx <- which(percentages$howmuch < 1)
  percentages$howmuch[idx] <- percentages$howmuch[idx]*100


  percentages %>% group_by(ResponseId) %>% summarize(n=length(!is.na(howmuch)))

  pl <- read.csv("../data/PlotLabels.csv")
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

amazon <- read_and_clean("../analysis/Frame Study - Amazon_February 9, 2018_13.18.csv")
reddit <- read_and_clean("../analysis/Frame Study - reddit-final.csv")
amazon$source <- "amazon"
reddit$source <- "reddit"
percentages <- rbind(amazon, reddit)

percentages %>% ggplot(aes(x=IPAddress, y=howmuch)) +
  geom_point() + coord_flip() +
  facet_grid(.~source)


percentages %>% na.omit() %>% ggplot(aes(x = perc, y = howmuch)) +
  geom_abline() +
  geom_point(aes(shape=isFrame, colour=isFrame)) +
  facet_grid(Frame~Type, labeller="label_both") +
  xlab("Actual Value") +
  ylab("Estimated Value") +
  theme_bw() +
  scale_colour_brewer("Frame piece estimate", palette="Set1") +
  scale_shape_discrete("Frame piece estimate") +
  theme(legend.position="bottom") +
  facet_grid(.~source)



percentages %>%
  ggplot(aes(x = perc, y = diff.error)) + geom_point() +
  facet_grid(.~Frame, labeller="label_both") + geom_smooth(method="lm")

error0 <- lm(diff.error ~ perc*Frame, data = percentages)

percentages %>%
  ggplot(aes(x = perc, y = diff.error, colour=frameframe)) + geom_point() +
  facet_grid(.~Frame, labeller="label_both") + geom_smooth(method="lm")

error1 <- lm(diff.error ~ perc*frameframe, data = percentages)
anova(error0, error1)
summary(error1)

percentages %>%
  ggplot(aes(x = perc, y = diff.error, colour=frameframe)) + geom_point() +
  facet_grid(Type~Frame, labeller="label_both") + geom_smooth(method="lm")

error2 <- lm(diff.error ~ perc*frameframe*Type, data = percentages)
anova(error1, error2)
summary(error2)

library(lme4)
m0 <- lmer(howmuch~-1+perc+isFrame*Type+(1|ResponseId), data=percentages)


#####################

firstclicks <- responses %>% select(ResponseId, contains("First.Click")) %>% gather(clicks, when, contains("First.Click"))
firstclicks <- firstclicks %>% mutate(when = as.numeric(when))

firstclicks %>% ggplot(aes(x=ResponseId, y=when)) + geom_point() + coord_flip()

percs <- percentages %>% select(-IPAddress) %>% spread(ResponseId, howmuch)

cor(percs[,3:10], use = "pairwise.complete")
percs %>% ggplot(aes(x = R_2abkkkeQ2FPqoXD, y=R_2YfEpIfXw8WPYn3)) + geom_point()
percs %>% ggplot(aes(x = R_2WVDySUTNc67XPB, y=R_3KJ8VQluGLaNCT0)) + geom_point()
