library(ggplot2)
library(data.table)





plot_for_month <- function(x, complete_data) {
    selected_score_data <- complete_data[complete_data$month <= x,]
    if (is.null(dim(selected_score_data)) || dim(selected_score_data)[1] == 0) {
        heat_data = data.frame(matrix(c(
            "+","+","-","-","+","-","+","-",0,0,0,0
        ), ncol = 3))
    }else{
        selected_score_table = data.table(selected_score_data)
        total_scores <-
            selected_score_table[, list(
                fem_score = sum(karmaFem),
                mr_score = sum(karmaMR),
                fem_posts = sum(numPostsFem),
                mr_posts = sum(numPostsMR),
                duration = max(month) - min(month)
            ), by = user]
        total_scores$fem_avg <-
            total_scores$fem_score / total_scores$fem_posts
        total_scores$mr_avg <-
            total_scores$mr_score / total_scores$mr_posts
        total_scores$fem_z_score <- scale(total_scores$fem_avg)
        total_scores$mr_z_score <- scale(total_scores$mr_avg)
        
        total_scores$fem_type <- "+"
        total_scores$fem_type[total_scores$fem_z_score < 0] <- "-"
        total_scores$mr_type <- "+"
        total_scores$mr_type[total_scores$mr_z_score < 0] <- "-"
        
        heat_data <-
            as.data.frame(table(total_scores$fem_type, total_scores$mr_type))
    }
    heat_data$`Month Number` <- x
    colnames(heat_data) <-
        c("Feminism Score", "Mens Right Score", "# of users","Month Number")
    return (heat_data)
}
all_month_results <-
    lapply(seq(min(score_data$month), max(score_data$month)),  plot_for_month, score_data)

quadrant_frame <- do.call(rbind, all_month_results)

write.table(
    quadrant_frame,
    file = "quadrant_values.csv",
    sep = ",",
    quote = TRUE, row.names = FALSE
)