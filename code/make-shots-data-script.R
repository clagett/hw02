#------------------------------------------------------------------
# title: Stat133 Homework 2: GSW shots data preparation
# description: The goal of this is to create a csv data file that contains
#              the right variables that will be used for the visualization phase
# input(s): csv files: "andre-iguodala.csv", "draymond-green.csv"
#         "kevin-durant.csv", "stephen-curry.csv", "klay-thompson.csv"
# output(s): "shots-data.csv
#------------------------------------------------------------------     

andre0 <- read.csv("../data/andre-iguodala.csv", stringsAsFactors = FALSE)
draymond0 <- read.csv("../data/draymond-green.csv", stringsAsFactors = FALSE)
kd0 <- read.csv("../data/kevin-durant.csv", stringsAsFactors = FALSE)
steph0 <- read.csv("../data/stephen-curry.csv", stringsAsFactors = FALSE)
klay0 <- read.csv("../data/klay-thompson.csv", stringsAsFactors = FALSE)


add_col_name <- function(table, playername){
     names <- as.matrix(rep(playername, nrow(table)))
     colnames(names) <- "name"
     return(cbind(names, table))
}

andre <- add_col_name(andre0, "Andre Iguodala")
draymond <- add_col_name(draymond0, "Draymond Green")
kd <- add_col_name(kd0, "Kevin Durant")
steph <- add_col_name(steph0, "Stephen Curry")
klay <- add_col_name(klay0, "Klay Thompson")

editdata <- function(player_table){
     player_table$shot_made_flag[player_table$shot_made_flag == "n"] <- "missed shot"
     player_table$shot_made_flag[player_table$shot_made_flag == "y"] <- "made shot"
     player_table <- player_table %>% mutate(minute = period*12-minutes_remaining)
     player_table
}

andre1 <- editdata(andre)
draymond1 <- editdata(draymond)
kd1 <- editdata(kd)
steph1 <- editdata(steph)
klay1 <- editdata(klay)


sink("../output/andre-iguodala-summary.txt")
summary(andre1)
sink()

sink("../output/draymond-green-summary.txt")
summary(draymond1)
sink()

sink("../output/kevin-durant-summary.txt")
summary(kd1)
sink()

sink("../output/stephen-curry-summary.txt")
summary(steph1)
sink()

sink("../output/klay-thompson-summary.txt")
summary(klay1)
sink()

list.files("../output")

stacked <- rbind(andre1, draymond1, kd1, steph1, klay1)

write.csv(stacked, file = "../data/shots-data.csv")

sink("../output/shots-data-summary.txt")
summary(stacked)
sink()


