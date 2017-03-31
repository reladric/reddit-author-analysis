library(ggplot2)
library(data.table)

score_data <-
  read.csv2(
    file = "./userMonthlyScore.csv",
    sep = ",",
    header = TRUE,
    stringsAsFactors = FALSE
  )

score_data$month <- as.numeric(score_data$month)
score_data$user <- as.character(score_data$user)

score_data$numPostsMR <- as.numeric(score_data$numPostsMR)
score_data$cumPostsMR <- as.numeric(score_data$cumPostsMR)
score_data$karmaMR <- as.numeric(score_data$karmaMR)
score_data$cumKarmaMR <- as.numeric(score_data$cumKarmaMR)


score_data$numPostsFem <- as.numeric(score_data$numPostsFem)
score_data$cumPostsFem <- as.numeric(score_data$cumPostsFem)
score_data$karmaFem <- as.numeric(score_data$karmaFem)
score_data$cumKarmaFem <- as.numeric(score_data$cumKarmaFem)


score_data_table <- data.table(score_data)

total_scores <-
  score_data_table[, list(
    fem_score = sum(karmaFem),
    mr_score = sum(karmaMR),
    fem_posts = sum(numPostsFem),
    mr_posts = sum(numPostsMR),
    duration = max(month) - min(month)
  ), by = user]

plot_for_month <- function(x, complete_data) {
  total_scores <- complete_data[complete_data$month == x, ]
  total_scores$fem_avg <-
    total_scores$cumKarmaFem / total_scores$cumPostsFem
  total_scores$mr_avg <-
    total_scores$cumKarmaMR / total_scores$cumPostsMR
  total_scores$fem_z_score <- scale(total_scores$fem_avg)
  total_scores$mr_z_score <- scale(total_scores$mr_avg)

  total_scores$fem_type <- "+"
  total_scores$fem_type[total_scores$fem_z_score < 0] <- "-"
  total_scores$mr_type <- "+"
  total_scores$mr_type[total_scores$mr_z_score < 0] <- "-"
  
  heat_data <-
    as.data.frame(table(total_scores$fem_type, total_scores$mr_type))
  heat_data$`Month Number`<-x
  colnames(heat_data) <-
    c("Feminism Score", "Mens Right Score", "# of users","Month Number")
  return (heat_data)
}
all_month_results <-
  lapply(seq(min(score_data$month), max(score_data$month)),  plot_for_month, score_data)

quadrant_frame<-do.call(rbind, all_month_results)

write.table(quadrant_frame,
            file = "quadrant_values.csv",
            sep = ",",
            quote = TRUE, row.names = FALSE)