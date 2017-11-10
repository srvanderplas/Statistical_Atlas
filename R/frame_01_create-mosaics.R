cols <- c("#333300", "#000066", "#cc9900", "#202060", "#86742d", "grey50")

# alternate color scheme
library(RColorBrewer)
cols <- c(brewer.pal(5, "Dark2"), "grey50")
library(ggmosaic)
library(tidyverse)

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
    labs(x=NULL,y=NULL) +
    geomnet::theme_net() +
    theme(legend.position = "none",
          plot.title = element_blank(),
          plot.margin = grid::unit(c(0,0,-.2,-.2), unit = "lines"))
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
    scale_x_continuous(expand=c(0,0), limits = c(0, 1)) +
    scale_y_continuous(expand=c(0,0), limits = c(0, 1)) +
    labs(x = NULL,y = NULL) +
    geomnet::theme_net() +
    theme(plot.title = element_blank(),
          plot.margin = grid::unit(c(0,0,-.2,-.2), unit = "lines"))

  ggp2 <- getMosaic(occ4)
  ggp2_df <- ggplot_build(ggp2)$data[[1]]

  # plot2 <- getMosaic(occ4)

  # This is ugly, but scale_x_product is getting in the way
  # because it doesn't take expand=c(0,0)
  plot2 <- ggp2_df %>% ggplot() +
    geom_rect(xmin=0, xmax=1, ymin=0, ymax=1, fill="grey50") +
    geom_rect(aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax, fill=fill, alpha=alpha),
              colour="grey85", size=0.1) +
    scale_fill_identity() +
    scale_alpha_identity() +
    scale_x_continuous(expand=c(0,0), limits = c(0, 1)) +
    scale_y_continuous(expand=c(0,0), limits = c(0, 1)) +
    labs(x = NULL,y = NULL) +
    geomnet::theme_net() +
    theme(plot.title = element_blank(),
          plot.margin = grid::unit(c(0,0,-.2,-.2), unit = "lines"))

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

# save plots in folder
purrr::map(1:nrow(states), function(k) {
  if (is.null(states$mosaics[[k]])) return()
  print(states$mosaics[[k]][1])
  ggsave( filename = paste0("inst/test-images/", states$Area.name[k],"-mosaic_with_frame.png"),
          width = 5, height = 5)
  print(states$mosaics[[k]][2])
  ggsave( filename = paste0("inst/test-images/", states$Area.name[k],"-mosaic_without_frame.png"),
          width = 5, height = 5)
})
