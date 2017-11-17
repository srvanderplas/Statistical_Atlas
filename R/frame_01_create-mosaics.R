cols <- c("#333300", "#000066", "#cc9900", "#202060", "#86742d", "grey50")

# alternate color scheme
library(RColorBrewer)
cols <- c(brewer.pal(5, "Dark2"), "grey50")
library(ggmosaic)
library(tidyverse)

theme_fix <-   list(
  xlab(NULL), ylab(NULL),
  theme_void(),
  scale_x_continuous(expand = c(0,0), limits = c(0,1)),
  scale_y_continuous(expand = c(0,0), limits = c(0,1)),
  theme(plot.margin = grid::unit(c(0,0,-.2,-.2), unit = "lines"),
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

occ3 <- read.csv("data/occ3.csv")
occ3 <- occ3 %>% mutate(
  Occupation = factor(Occupation, levels =
                        c("Agriculture", "Manufacturing",
                          "Trade", "Service", "School", "Unaccounted")),
  Sex = factor(Sex, levels = c("Male", "Female")),
  State = as.character(State),
  Area.name = as.character(Area.name)
)

getMosaic <- function(data) {
  data %>%
    ggplot() +
    geom_mosaic(aes(x = product(Sex, Occupation),
                    fill=Occupation, alpha = Sex, weight = Number),
                offset = 0.00, colour="grey85", size=0.1) +
    scale_fill_manual(values=cols) + theme_bw() +
    scale_alpha_manual(values=c(0.8,1)) +
    coord_equal() +
    theme(axis.line=element_blank(), axis.text=element_blank(),
          axis.title.y = element_blank(), axis.ticks = element_blank()) +
    # ggtitle(state_name) +
    geomnet::theme_net() +
    theme(legend.position = "none")
}

createPlots <- function(data = occ3, state_name = "Iowa") {
  occ4 <- occ3 %>% filter(State == Area.name, State == state_name)


  # Force unaccounted to be female to match frame
  occ4.unacc <- filter(occ4, Occupation == "Unaccounted")
  occ4.other <- filter(occ4, Occupation != "Unaccounted")

  occ4 <- bind_rows(
    occ4.other,
    occ4.unacc %>%
      mutate(Number = sum(Number)) %>%
      filter(Sex == "Female")
  )

  if (nrow(occ4) == 0) return(NULL)

  scalars <- occ4 %>%
    mutate(frame = Occupation == "Unaccounted") %>%
    group_by(frame) %>%
    summarize(
      Number = sum(Number)
    ) %>% mutate(
      weight = Number/sum(Number)
    )
  scalars$weight[1] <- sqrt(scalars$weight[1])
  scalars$weight[2] <- 1-scalars$weight[1]

  # make inside plot, then scale
  ggp <- getMosaic(occ4.other %>% filter(Occupation != "Unaccounted"))
  ggp_df <- ggplot_build(ggp)$data[[1]] %>% mutate(
    xmin = xmin*scalars$weight[1] + scalars$weight[2]/2,
    xmax = xmax*scalars$weight[1] + scalars$weight[2]/2,
    ymin = ymin*scalars$weight[1] + scalars$weight[2]/2,
    ymax = ymax*scalars$weight[1] + scalars$weight[2]/2
  )

  plot1 <- ggp_df %>% ggplot() +
    geom_rect(xmin=0, xmax=1, ymin=0, ymax=1, fill="grey50") +
    geom_rect(xmin=scalars$weight[2]/2, xmax=1-scalars$weight[2]/2,
              ymin=scalars$weight[2]/2, ymax=1-scalars$weight[2]/2, fill="white") +
    geom_rect(aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax, fill=fill, alpha=alpha),
              colour="grey85", size=0.1) +
    scale_fill_identity() +
    scale_alpha_identity() +
    theme_fix

  ggp2 <- getMosaic(occ4)
  ggp2_df <- ggplot_build(ggp2)$data[[1]]

  # plot2 <- getMosaic(occ4)

  # This is ugly, but scale_x_product is getting in the way
  # because it doesn't take expand=c(0,0)
  plot2 <- ggp2_df %>% ggplot() +
#    geom_rect(xmin=0, xmax=1, ymin=0, ymax=1, fill="grey50") +
    geom_rect(xmin=0, xmax=1, ymin=0, ymax=1, fill="white") +
    geom_rect(aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax, fill=fill, alpha=alpha),
              colour="grey85", size=0.1) +
    scale_fill_identity() +
    scale_alpha_identity() +
    theme_fix

  list(plot1 = plot1, plot2 = plot2)
}

states <- occ3 %>% group_by(Area.name) %>% nest()
# get all mosaics into a variable
states$mosaics <- purrr::map2(
  states$data, states$Area.name,
  .f=function(x,y) {
    createPlots(data=x, state_name = y)})

# get both plots for Alabama
states$mosaics[[1]][1]
states$mosaics[[1]][2]


#########
# Read in plot annotation info

plotlabs <- read.csv("data/PlotLabels.csv", stringsAsFactors = F) %>%
  filter(Type == "mosaic") %>%
  group_by(State, Frame) %>%
  mutate(num = 1:n()) %>%
  ungroup() %>%
  filter(Frame) %>%
  left_join(states, by = c("State" = "Area.name"))


add_mosaicplot_label <- function(plot, fill, label = "A", alpha = 1, frame = F) {
  pb <- ggplot_build(plot)

  ldf <- if(grepl(fill, "grey")) {
    data_frame(fill = fill, label = label, alpha = NA)
  } else {
    data_frame(fill = fill, label = label, alpha = alpha)
  }

  ldf <- ldf %>%
    left_join(bind_rows(pb$data)) %>%
    unique() %>%
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
purrr::map(1:nrow(states), function(k) {
  if (is.null(states$mosaics[[k]])) return()
  print(states$mosaics[[k]][1])
  ggsave( filename = paste0("inst/all-images/", states$Area.name[k],"-mosaic_with_frame.png"),
          width = 5, height = 5)
  print(states$mosaics[[k]][2])
  ggsave( filename = paste0("inst/all-images/", states$Area.name[k],"-mosaic_without_frame.png"),
          width = 5, height = 5)
})

# save annotated plots

purrr::map(1:nrow(plotlabs), function(k) {
  if (is.null(plotlabs$mosaics[[k]])) return()

  p <- plotlabs$mosaics[[k]][1]$plot1
  add_mosaicplot_label(p, fill = plotlabs$fill[k], label = "A",
                       alpha = plotlabs$alpha[k],
                       frame = plotlabs$isFrame[k])
  ggsave( filename = paste0("inst/test-images/", plotlabs$State[k],
                            "-mosaic_with_frame", plotlabs$num[k], ".png"),
          width = 5, height = 5)
  p <- plotlabs$mosaics[[k]][2]$plot2
  add_mosaicplot_label(p, fill = plotlabs$fill[k], label = "A",
                       alpha = plotlabs$alpha[k],
                       frame = F)
  ggsave( filename = paste0("inst/test-images/", plotlabs$State[k],
                            "-mosaic_without_frame", plotlabs$num[k], ".png"),
          width = 5, height = 5)
})



# get percentages out of plots
plotlabs$perc <- purrr::map_dbl(1:nrow(plotlabs), function(k) {
  if (is.null(plotlabs$mosaics[[k]])) return()

  d <- plotlabs$mosaics[[k]][2]$plot2[[1]]
  d$perc <- d$.wt/sum(d$.wt)
  filter(d,fill==plotlabs$fill[k], alpha==plotlabs$alpha[k])$perc*100
})
write.csv(plotlabs %>% select(-data, -mosaics), file="mosaics.csv", row.names=FALSE)
