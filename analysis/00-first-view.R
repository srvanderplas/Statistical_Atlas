responses <- read.csv("/Users/heike/Downloads/Frame Study - Pics_November 16, 2017_23.19.csv")

firstclicks <- responses %>% select(ResponseId, contains("First.Click")) %>% gather(clicks, when, contains("First.Click"))
firstclicks <- firstclicks %>% mutate(when = as.numeric(when))

firstclicks %>% ggplot(aes(x=ResponseId, y=when)) + geom_point() + coord_flip()

percentages <- responses %>% select(IPAddress, ResponseId, grep("Q...$", names(responses), value=TRUE)) %>% gather(question, howmuch, matches("Q..."))
percentages$howmuch <- as.numeric(percentages$howmuch)

percentages %>% ggplot(aes(x=IPAddress, y=howmuch)) + geom_point() + coord_flip()
percentages %>% group_by(ResponseId) %>% summarize(n=length(!is.na(howmuch)))
percs <- percentages %>% select(-IPAddress) %>% spread(ResponseId, howmuch)

cor(percs[,3:10], use = "pairwise.complete")
percs %>% ggplot(aes(x = R_2abkkkeQ2FPqoXD, y=R_2YfEpIfXw8WPYn3)) + geom_point()
percs %>% ggplot(aes(x = R_2WVDySUTNc67XPB, y=R_3KJ8VQluGLaNCT0)) + geom_point()

pl <- read.csv("/Users/heike/papers/2018-Atlas-study/data/PlotLabels.csv")
percentages <- percentages %>% left_join(pl %>% select(Question, perc, Type, Frame, isFrame), by=c("question"="Question"))

percentages %>% na.omit() %>% ggplot(aes(x = perc, y = howmuch)) +
  geom_abline() +
  geom_point(aes(shape=isFrame, colour=isFrame)) +
  facet_grid(Frame~Type, labeller="label_both") +
  xlab("Actual Value") +
  ylab("Estimated Value") 
