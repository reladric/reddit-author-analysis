library(shiny)
library(ggplot2)
library(scales)
library(data.table)
plotdata_for_month <- function(x, complete_data) {
    selected_score_data <-
        complete_data[complete_data$month <= x ,]
    if (is.null(dim(selected_score_data)) ||
        dim(selected_score_data)[1] == 0) {
        plot_data = data.frame(x = numeric(0), y = numeric(0))
    } else{
        selected_score_table = data.table(selected_score_data)
        plot_data <-
            selected_score_table[, list(
                fem_score = sum(karmaFem),
                mr_score = sum(karmaMR),
                fem_posts = sum(numPostsFem),
                mr_posts = sum(numPostsMR),
                duration = max(month) - min(month)
            ), by = user]
        
        plot_data <-
            plot_data[(plot_data$mr_posts != 0 &
                           plot_data$fem_posts != 0),]
        plot_data$fem_avg <-
            plot_data$fem_score / plot_data$fem_posts
        plot_data$fem_avg[is.nan(plot_data$fem_avg)] <- 0
        
        plot_data$mr_avg <-
            plot_data$mr_score / plot_data$mr_posts
        plot_data$mr_avg[is.nan(plot_data$mr_avg)] <- 0
        mu <- mean(plot_data$fem_avg)
        sigma <- sd(plot_data$fem_avg)
        plot_data$fem_z_score <- (plot_data$fem_avg - mu) / sigma
        mu <- mean(plot_data$mr_avg)
        sigma <- sd(plot_data$mr_avg)
        plot_data$mr_z_score <- (plot_data$mr_avg - mu) / sigma
        plot_data$month_number <- x
        
        
    }
    return (as.data.frame(plot_data))
}

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

all_month_data <-
    lapply(seq(1, 133) , plotdata_for_month,score_data)
all_month_matrix <- do.call(rbind, all_month_data)
all_month_frame <- as.data.frame(all_month_matrix)
all_month_table <- data.table(all_month_frame)
monthwise_counts <-
    all_month_table[, list(users = length(user)), by = month_number]

all_month_reduced_frame <-
    all_month_frame[(all_month_frame$fem_posts > 5 &
                         all_month_frame$mr_posts > 5),]

all_month_reduced_table <- data.table(all_month_reduced_frame)
reduced_monthwise_counts <-
    all_month_reduced_table[, list(users = length(user)), by = month_number]

library(RColorBrewer)
k <- 11
my.cols <- rev(brewer.pal(k, "RdYlBu"))
get_quantile_counts <- function(x,y) {
    x_quantile_values <- quantile(x)
    y_quantile_values <- quantile(y)
    count_frame <- as.data.frame(matrix(c(x,y), ncol = 2))
    colnames(count_frame) <- c("x_value", "y_value")
    count_frame$x_qval <- 1
    count_frame$y_qval <- 1
    count_frame[(
        count_frame$x_value > x_quantile_values[2] &
            count_frame$x_value < x_quantile_values[3]
    ),]$x_qval <-
        2
    count_frame[(
        count_frame$x_value > x_quantile_values[3] &
            count_frame$x_value < x_quantile_values[4]
    ),]$x_qval <-
        3
    count_frame[(count_frame$x_value > x_quantile_values[4]),]$x_qval <-
        4
    
    count_frame[(
        count_frame$y_value > y_quantile_values[2] &
            count_frame$y_value < y_quantile_values[3]
    ),]$y_qval <-
        2
    count_frame[(
        count_frame$y_value > y_quantile_values[3] &
            count_frame$y_value < y_quantile_values[4]
    ),]$y_qval <-
        3
    count_frame[(count_frame$y_value > y_quantile_values[4]),]$y_qval <-
        4
    
    return (cbind(count_frame$x_qval, count_frame$y_qval))
}
test_data <-
    all_month_frame[(all_month_frame$month_number == 100),]
#median(test_data$fem_score) + (2*sd(test_data$fem_score))
ggplot(test_data,aes(x = fem_avg,y = mr_avg)) +
    stat_density2d(aes(fill = ..level..), geom = "polygon") +
    scale_alpha_continuous(limits = c(0,.5),breaks = seq(0,.5,by = 0.1)) +
    geom_vline(xintercept = median(test_data$fem_avg)) + geom_hline(yintercept = median(test_data$mr_avg)) 
+
    geom_point(alpha = 0.05, col = "red")


#scale_fill_gradientn(colours = c("#1d3349","#50565b","#6a7582","#7c98b5","#8eb8e5" ))    