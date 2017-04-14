library(shiny)
library(ggplot2)
library(scales)
library(data.table)
source("global.R")

score_data<-compute_data()[[4]]

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


#scale_fill_gradientn(colours = c("#1d3349","#50565b","#6a7582","#7c98b5","#8eb8e5" ))    