#------------------------------------------------------------------
# title: Stat133 Homework 2: GSW shots data preparation
# description: The goal of this is to create a csv data file that contains
#              the right variables that will be used for the visualization phase.
# input(s): "shots-data.csv", "nba-court.jpg"
# output(s): "shots-data.csv", "andre-iguodala-shot-chart.pdf" "draymond-green-shot-chart.pdf"
#              "kevin-durant-shot-chart.pdf" "stephen-curry-shot-chart.pdf" 
#              "klay-thompson-shot-chart.pdf"
#------------------------------------------------------------------ 

library(jpeg)
library(grid)
library(dplyr)
library(ggplot2)

shots <- read.csv("../data/shots-data.csv")

court_file <- "../images/nba-court.jpg"
court_image <- rasterGrob(
     readJPEG(court_file),
     width = unit(1, "npc"),
     height = unit(1, "npc")
)

plot_shots <- function(player_name){
     ggplot(data = shots %>% filter(name == player_name)) +
          annotation_custom(court_image, -250, 250, -50, 420) + 
          geom_point(aes(x=x, y=y, color = shot_made_flag)) +
          ylim(-50, 420) +
          ggtitle(paste("Shot Chart:", player_name, "(2016 season)")) +
          theme_minimal()
}

andre_shot_chart <- plot_shots("Andre Iguodala")
draymond_shot_chart <- plot_shots("Draymond Green")
kd_shot_chart <- plot_shots("Kevin Durant")
steph_shot_chart <- plot_shots("Stephen Curry")
klay_shot_chart <- plot_shots("Klay Thompson")

ggsave(filename = "../images/andre-iguodala-shot-chart.pdf", plot = andre_shot_chart, width = 8, height = 5)
ggsave(filename = "../images/draymond-green-shot-chart.pdf", plot = draymond_shot_chart, width = 8, height = 5)
ggsave(filename = "../images/kevin-durant-shot-chart.pdf", plot = kd_shot_chart, width = 8, height = 5)
ggsave(filename = "../images/stephen-curry-shot-chart.pdf", plot = steph_shot_chart, width = 8, height = 5)
ggsave(filename = "../images/klay-thompson-shot-chart.pdf", plot = klay_shot_chart, width = 8, height = 5)

gg_ht_weight_positions <- ggplot(data) + 
     geom_point(aes(x = data$height, y = data$weight)) + xlab("Height") + ylab("Weight") + facet_grid(. ~ position)

gsw_shot_chart <- ggplot(data = shots) + 
     annotation_custom(court_image, -250, 250, -50, 420) + 
     geom_point(aes(x=x, y=y, color = shot_made_flag)) + 
     ylim(-50, 420) +
     facet_grid(. ~ name)

ggsave(filename = "../images/gsw_shot_charts.pdf", plot = gsw_shot_chart, width = 8, height = 7)

