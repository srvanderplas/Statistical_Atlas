library(RColorBrewer)
library(munsell)
cols = c(brewer.pal(n = 12, name = "Paired")[c(1,3,5,7,9,11)], "grey80")

#cols <- c("#ffcc80","#ff9900")
#cols <- "#ccf2ff"

theme_fix <-   list(
  xlab(NULL), ylab(NULL),
  theme_void(),
  scale_y_continuous(expand = c(0,0), limits = c(0,1)),
  theme(plot.margin = grid::unit(c(0,0,-.2,-.2), unit = "lines"),
        legend.position = "none",
        panel.background = element_blank(),
        panel.border = element_blank(),
        panel.grid = element_blank(),
        axis.line = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank(),
        aspect.ratio = 1,
        # probably overkill
        line = element_blank(),
        rect = element_blank(),
        text = element_blank(),
        title = element_blank())
)

colRGB <- t(col2rgb(cols))/256
colMNSL <- rgb2mnsl(colRGB)
colMNSL <- c(colMNSL,
             darker(colMNSL, steps = 2),
             darker(colMNSL, steps = 4))

colHEX <- mnsl(colMNSL)
colHEX <- colHEX[rep(0:6, each = 3) + c(1, 8, 15)]
colHEX <- c(colHEX, "grey60")

church <- read.csv("data/atlas-data-clean/denominations-1874.csv")
cl <- church %>% gather(key = "Denomination",
                        value = "Number",
                        c(4:22, 26))
cl <- cl %>% mutate(Denomination =
                      reorder(Denomination, Number, na.rm = TRUE))

cl$Denomination <- factor(cl$Denomination,
                          c("Unaccommodated", levels(cl$Denomination)[-20]))
levels(cl$Denomination) <- gsub("\\.", " ", levels(cl$Denomination))

cl_data <- cl %>% filter(as.character(STATEICP)==as.character(State)) %>%
  group_by(STATEICP) %>% nest()

cl_data$data <- cl_data$data %>% purrr::map(.f = function(x) {
  x %>%  arrange(desc(Number)) %>% mutate(
    group = c(as.character(Denomination[1:5]), rep("Other", n() - 5))
  ) %>% group_by(group) %>% summarize(Number = sum(Number, na.rm=TRUE))
})

cl_data <- cl_data %>% unnest()

get_plots <- function(sub_data) {
  sub_data <- sub_data %>% mutate(
    group = factor(group),
    group = reorder(group, -Number)
  )
  lvls <- levels(sub_data$group)
  sub_data$group <- factor(sub_data$group,
                           levels=c(c("Unaccommodated", "Other"),
                                    rev(setdiff(lvls, c("Other", "Unaccommodated")))))

  sub_data %>% arrange(desc(group)) %>%
    ggplot() +
    geom_bar(aes(weight = Number,  x = STATEICP, fill = group),
             position = "fill", colour="white", size=0.1,
             width = 1) +
    scale_fill_manual("Denomination", values = rev(colHEX)[-c(2:4)]) + coord_flip() +
    geomnet::theme_net() +
    theme(legend.position = "none") + ylab("") +
    # ggtitle(state_name) +
    theme_fix
}

createSpine <- function(data, state_name) {
  spine_df <- data %>% filter(STATEICP == state_name)
  if (nrow(spine_df) == 0) return()

  plot2 <- spine_df %>% get_plots()
  scalars <- spine_df %>% mutate(
    type = group=="Unaccommodated"
  ) %>% group_by(type) %>% summarize(
    Number = sum(Number, na.rm = TRUE)
  )
  if (nrow(scalars)==1) {
    plot1 <- plot2
  } else {
  scalars$weight <- scalars$Number/sum(scalars$Number)
  scalars$weight[1] <- sqrt(scalars$weight[1])
  scalars$weight[2] <- 1 - scalars$weight[1]
  spine <- spine_df %>% mutate(Number = replace(Number, group=="Unaccommodated", 0)) %>%
    get_plots()

  spine_wo_df <- ggplot_build(spine)$data[[1]] %>% mutate(
    xmin = scalars$weight[2]/2,
    xmax = scalars$weight[1] + scalars$weight[2]/2,
    ymin = ymin*scalars$weight[1] + scalars$weight[2]/2,
    ymax = ymax*scalars$weight[1] + scalars$weight[2]/2
  )
  plot1 <- spine_wo_df %>%
    ggplot(aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax)) +
    geom_rect(aes(xmin=0, xmax=1, ymin=0, ymax=1), fill="grey60") +
    geom_rect(aes(fill=fill), colour="white", size=0.1) +
    scale_fill_identity() +
    coord_flip() +
    geomnet::theme_net() +
    theme_fix +
    scale_x_continuous(expand = c(0,0), limits = c(0,1))
    # ggtitle(state_name)
  }
  list(plot1=plot1, plot2=plot2)
}


#########

states <- createSpine(data = cl_data, state_name = "Florida")
states$plot1
states$plot2

#########
# Read in plot annotation info

plotlabs <- read.csv("data/study-setup/PlotLabels.csv", stringsAsFactors = F) %>%
  filter(Type == "spine") %>%
  group_by(State, Frame) %>%
  mutate(num = 1:n()) %>%
  ungroup() %>%
  filter(Frame)


add_spineplot_label <- function(plot, fill, label = "A", frame = F) {
  pb <- ggplot_build(plot)

  ldf <- data_frame(fill = fill, label = label) %>%
    left_join(bind_rows(pb$data)) %>%
    unique() %>%
    filter(ymin != ymax) %>%
    mutate(
      ylab = (ymin + ymax)/2,
      xlab = (xmin + xmax)/2
    )

  if (frame) {
    ldf$ylab <- 1
    ldf$xlab <- 1
    plot +
      annotate("text", x = ldf$xlab, y = ldf$ylab, label = ldf$label, color = "#FFFC00", size = 8, fontface = "bold", hjust = 1, vjust = 1)
  } else {
    plot +
      annotate("text", x = ldf$xlab, y = ldf$ylab, label = ldf$label, color = "#FFFC00", size = 8, fontface = "bold")
  }

}
# save plots in folder
lvls <- unique(cl_data$STATEICP)

# Save all plots
purrr::map(1:length(lvls), function(k) {
  states <- createSpine(data = cl_data, state_name = lvls[k])
  if (is.null(states[1])) return()

  states[1]$plot1
  ggsave( filename = paste0("images/all-images/", lvls[k],
                            "-spine_with_frame", ".png"),
          width = 5, height = 5)
  states[2]$plot2
  ggsave( filename = paste0("images/all-images/", lvls[k],
                            "-spine_without_frame", ".png"),
          width = 5, height = 5)
})


if (!dir.exists("images/all-test-images")) {
  dir.create("images/all-test-images")
}

# Save labeled test plots
plotlabs$states <- purrr::map(1:nrow(plotlabs), function(k) {
  states <- createSpine(data = cl_data, state_name = plotlabs$State[k])
  if (is.null(states[1])) return()

  p <- states[1]$plot1
  add_spineplot_label(p, fill = plotlabs$fill[k], label = "A", frame = plotlabs$isFrame[k])
  ggsave( filename = paste0("images/all-test-images/", plotlabs$State[k],
                            "-spine_with_frame", plotlabs$num[k], ".png"),
          width = 5, height = 5)
  p <- states[2]$plot2
  add_spineplot_label(p, fill = plotlabs$fill[k], label = "A", frame = F)
  ggsave( filename = paste0("images/all-test-images/", plotlabs$State[k],
                            "-spine_without_frame", plotlabs$num[k], ".png"),
          width = 5, height = 5)
  states
})

# get percentages out of plots
plotlabs$perc <- purrr::map_dbl(1:nrow(plotlabs), function(k) {
  if (is.null(plotlabs$states[[k]])) return()
#browser()
  d <- ggplot_build(plotlabs$states[[k]][2]$plot2)[[1]][[1]]
  d$perc <- d$count/sum(d$count)
  filter(d,fill==plotlabs$fill[k])$perc*100
})
write.csv(plotlabs %>% select(-states), file="spines.csv", row.names=FALSE)



#######################
# summarized test images for the paper


add_s_label <- function(plot, fill, label = "A", frame = F) {
  pb <- ggplot_build(plot)

  ldf <- data_frame(fill = fill, label = label) %>%
    left_join(bind_rows(pb$data)) %>%
    unique() %>%
    filter(ymin != ymax) %>%
    mutate(
      ylab = (ymin + ymax)/2,
      xlab = (xmin + xmax)/2
    )

  if (frame) {
    ldf$ylab <- 1
    ldf$xlab <- 1
      annotate("text", x = ldf$xlab, y = ldf$ylab, label = ldf$label, color = "#FFFC00", size = 8, fontface = "bold", hjust = 1, vjust = 1)
  } else {
      annotate("text", x = ldf$xlab, y = ldf$ylab, label = ldf$label, color = "#FFFC00", size = 8, fontface = "bold")
  }
}

plotlabs$states <- purrr::map(1:nrow(plotlabs), function(k) {
  k <- 3*(k %/% 3)

  states <- createSpine(data = cl_data, state_name = plotlabs$State[k+1])
  if (is.null(states[1])) return()

  p <- states$plot1
  labelA <- add_s_label(p, fill = plotlabs$fill[k+1], label = "A", frame = plotlabs$isFrame[k+1])
  labelB <- add_s_label(p, fill = plotlabs$fill[k+2], label = "B", frame = plotlabs$isFrame[k+2])
  labelC <- add_s_label(p, fill = plotlabs$fill[k+3], label = "C", frame = plotlabs$isFrame[k+3])
  ggsave(p+labelA+labelB+labelC, filename = paste0("images/paper-images/", plotlabs$State[k+1],
                            "-spine_with_frame", plotlabs$num[k+1], ".png"),
          width = 5, height = 5)
  p <- states$plot2
  labelA <- add_s_label(p, fill = plotlabs$fill[k+1], label = "A", frame = F)
  labelB <- add_s_label(p, fill = plotlabs$fill[k+2], label = "B", frame = F)
  labelC <- add_s_label(p, fill = plotlabs$fill[k+3], label = "C", frame = F)
  ggsave(p+labelA+labelB+labelC, filename = paste0("images/paper-images/", plotlabs$State[k+1],
                            "-spine_without_frame", plotlabs$num[k+1], ".png"),
          width = 5, height = 5)
  k <- k+3
  states
})


