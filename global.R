library(shiny)
library(ggplot2)
library(scales)
library(data.table)
library(gridExtra)
file_loaded <- 0
#### Load data from csvs ----
compute_data <-
    function(updateProgress = NULL,
             user_statistics,
             activity_data,
             user_comments) {
        # Create 0-row data frame which will be used to store data
        user_statistics <-
            read.csv2(file = "./user_statistics.csv",
                      sep = ",",
                      header = TRUE)
        if (is.function(updateProgress)) {
            text <- "1/6"
            updateProgress(detail = text)
        }
        colnames(user_statistics) <-
            c(
                "Author",
                "Majority Subreddit",
                "First Subreddit",
                "Account Created Date",
                "Total Post Karma (Score)",
                "Total Comment Karma (Score"
            )
        user_statistics$`Account Created Date`[user_statistics$`Account Created Date` ==
                                                   "NaT"] <- NA
        user_statistics$`Total Post Karma (Score)`[user_statistics$`Total Post Karma (Score)` ==
                                                       ""] <- NA
        user_statistics$`Total Comment Karma (Score`[user_statistics$`Total Comment Karma (Score` ==
                                                         ""] <- NA
        
        user_statistics$`Total Post Karma (Score)` <-
            as.numeric(levels(user_statistics$`Total Post Karma (Score)`))[user_statistics$`Total Post Karma (Score)`]
        user_statistics$`Total Comment Karma (Score` <-
            as.numeric(levels(user_statistics$`Total Comment Karma (Score`))[user_statistics$`Total Comment Karma (Score`]
        if (is.function(updateProgress)) {
            text <- "2/6"
            updateProgress(detail = text)
        }
        activity_data <-
            read.csv2(file = "./author_posting_activity.csv",
                      sep = ",",
                      header = TRUE)
        if (is.function(updateProgress)) {
            text <- "3/8"
            updateProgress(detail = text)
        }
        activity_data$ratio <-
            round(as.numeric(levels(activity_data$ratio))[activity_data$ratio], 2)
        if (is.function(updateProgress)) {
            text <- "4/6"
            updateProgress(detail = text)
        }
        user_comments <-
            read.csv2(
                file = "./comment_only_set.csv",
                sep = ",",
                header = TRUE,
                stringsAsFactors = FALSE,
                quote = "\"",
                fileEncoding = "UTF-8"
            )
        if (is.function(updateProgress)) {
            text <- "5/6"
            updateProgress(detail = text)
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
        if (is.function(updateProgress)) {
            text <- "6/6"
            updateProgress(detail = text)
        }
        
        
        return (list(user_statistics,
                     activity_data,
                     user_comments,
                     score_data))
        
        
    }
#### Home category to vector mapping - N/A ----
# getAuthorCategories <- function() {
#   category_vector <- c(1, -1, -2)
#   names(category_vector) <-
#     c("Crossposters", "Mensrights", "Feminism")
#   return (category_vector)
# }
#### Get author from quadrant ----
getAuthors <- function(quadrant = 1, user_statistics) {
    if (quadrant == 1) {
        current_data <- user_statistics$Author[(
            user_statistics$"Majority Subreddit" == "Feminism" &
                user_statistics$"First Subreddit" == "Feminism"
        )]
        names(current_data) <-
            1:length(current_data)
        selected_author = current_data[sample(length(current_data), 1)]
        return (list(length(current_data), selected_author))
    }
    if (quadrant == 2) {
        current_data <- user_statistics$Author[(
            user_statistics$"Majority Subreddit" == "MensRights" &
                user_statistics$"First Subreddit" == "Feminism"
        )]
        names(current_data) <-
            1:length(current_data)
        selected_author = current_data[sample(length(current_data), 1)]
        return (list(length(current_data), selected_author))
    }
    if (quadrant == 3) {
        current_data <- user_statistics$Author[(
            user_statistics$"Majority Subreddit" == "Feminism" &
                user_statistics$"First Subreddit" == "MensRights"
        )]
        names(current_data) <-
            1:length(current_data)
        selected_author = current_data[sample(length(current_data), 1)]
        return (list(length(current_data), selected_author))
    }
    if (quadrant == 4) {
        current_data <- user_statistics$Author[(
            user_statistics$"Majority Subreddit" == "MensRights" &
                user_statistics$"First Subreddit" == "MensRights"
        )]
        names(current_data) <-
            1:length(current_data)
        selected_author = current_data[sample(length(current_data), 1)]
        return (list(length(current_data), selected_author))
    }
}
#### Generate Quadrant Heatmap data given month ----
heatdata_for_month <- function(x, complete_data) {
    selected_score_data <- complete_data[complete_data$month <= x,]
    if (is.null(dim(selected_score_data)) ||
        dim(selected_score_data)[1] == 0) {
        heat_data = data.frame(matrix(c(
            "+", "+", "-", "-", "+", "-", "+", "-", 0, 0, 0, 0
        ), ncol = 3))
    } else{
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
        c("Feminism Score",
          "Mens Right Score",
          "# of users",
          "Month Number")
    heat_data$labelField = ""
    heat_data$labelField[(heat_data$`Feminism Score` == "+" &
                              heat_data$`Mens Right Score` == "+")] <-
        paste("Saints", heat_data$`# of users`[(heat_data$`Feminism Score` == "+" &
                                                    heat_data$`Mens Right Score` == "+")], sep = ": ")
    
    heat_data$labelField[(heat_data$`Feminism Score` == "+" &
                              heat_data$`Mens Right Score` == "-")] <-
        paste("Fem Partisan", heat_data$`# of users`[(heat_data$`Feminism Score` == "+" &
                                                          heat_data$`Mens Right Score` == "-")], sep = ": ")
    
    heat_data$labelField[(heat_data$`Feminism Score` == "-" &
                              heat_data$`Mens Right Score` == "+")] <-
        paste("MR Partisan", heat_data$`# of users`[(heat_data$`Feminism Score` == "-" &
                                                         heat_data$`Mens Right Score` == "+")], sep = ": ")
    
    heat_data$labelField[(heat_data$`Feminism Score` == "-" &
                              heat_data$`Mens Right Score` == "-")] <-
        paste("Trolls", heat_data$`# of users`[(heat_data$`Feminism Score` == "-" &
                                                    heat_data$`Mens Right Score` == "-")], sep = ": ")
    return (heat_data)
}
#### Obtain aggregated window data ----
get_window_data <-
    function(x,  complete_data,feature, window_size, threshold) {
        if (x >= window_size) {
            w_start <-  x - window_size
            w_end <-  x
            selected_score_data <-
                complete_data[(complete_data$month >= w_start &
                                   complete_data$month <= w_end),]
            
            if (is.null(dim(selected_score_data)) ||
                dim(selected_score_data)[1] == 0) {
                plot_data = data.frame(
                    x = numeric(0),
                    y = numeric(0),
                    month_number = numeric(0)
                )
                print("Empty selection")
            } else{
                selected_score_table = data.table(selected_score_data)
                plot_data <-
                    selected_score_table[, list(
                        fem_score = sum(karmaFem),
                        mr_score = sum(karmaMR),
                        fem_posts = sum(numPostsFem),
                        mr_posts = sum(numPostsMR),
                        active_duration = length(month)
                    ), by = user]
                plot_data <- as.data.frame(plot_data)
                plot_data <-
                    plot_data[(plot_data$fem_posts >= threshold &
                                   plot_data$fem_posts >= threshold),]
                plot_data <-
                    plot_data[(plot_data$mr_posts != 0 &
                                   plot_data$fem_posts != 0),]
                
                plot_data$fem_avg <-
                    plot_data$fem_score / plot_data$fem_posts
                plot_data$fem_avg[is.nan(plot_data$fem_avg)] <- 0
                
                plot_data$mr_avg <-
                    plot_data$mr_score / plot_data$mr_posts
                plot_data$mr_avg[is.nan(plot_data$mr_avg)] <- 0
                x_field_name <- paste("fem_", feature, sep = "")
                y_field_name <- paste("mr_", feature, sep = "")
                
                plot_data$x <-
                    plot_data[[x_field_name]]
                plot_data$y <- plot_data[[y_field_name]]
            }
            negative_x_data <-
                plot_data[plot_data[,x_field_name] < 0,]
            if (is.null(dim(negative_x_data)) ||
                dim(negative_x_data)[1] == 0) {
                plot_data$x_negative_point <-
                    plot_data$vline
            }else{
                negative_x_max_data <-
                    negative_x_data$x[negative_x_data[,x_field_name] == max(negative_x_data[,x_field_name])]
                plot_data$x_negative_point <-
                    unique(negative_x_max_data)
            }
            negative_y_data <-
                plot_data[plot_data[,y_field_name] < 0,]
            if (is.null(dim(negative_y_data)) ||
                dim(negative_y_data)[1] == 0) {
                plot_data$y_negative_point <-
                    plot_data$hline
            }else{
                negative_y_max_data <-
                    negative_y_data$y[negative_y_data[,y_field_name] == max(negative_y_data[,y_field_name])]
                plot_data$y_negative_point <-
                    unique(negative_y_max_data)
            }
        } else{
            print("Too low")
            plot_data = data.frame(x = numeric(0),
                                   y = numeric(0),
                                   month_number = numeric(0))
        }
        
        return(plot_data)
        
    }
#### Generate Scatter Plot data given month ----
plotdata_for_month <-
    function(x,
             complete_data,
             feature = "avg",
             scale = TRUE,
             center = "median",
             window_size = 6,
             threshold) {
        # Window start and end
        plot_data <-
            get_window_data(x,  complete_data, feature, window_size, threshold)
        if (center == "mean") {
            x_center <- mean(plot_data$x)
            y_center <- mean(plot_data$y)
        }
        else{
            x_center <- median(plot_data$x)
            y_center <- median(plot_data$y)
        }
        
        if (scale) {
            x_sigma <- sd(plot_data$x)
            plot_data$x <-
                (plot_data$x - x_center) / x_sigma
            y_center <- mean(plot_data$y)
            y_sigma <- sd(plot_data$y)
            plot_data$y <-
                (plot_data$y - y_center) / y_sigma
        }
        
        if (center == "mean") {
            plot_data$hline <- mean(plot_data$y)
            plot_data$vline <- mean(plot_data$x)
        }
        else{
            plot_data$hline <- median(plot_data$y)
            plot_data$vline <- median(plot_data$x)
        }
        
        
        x_quantile_values <- quantile(plot_data$x)
        y_quantile_values <- quantile(plot_data$y)
        plot_data$month_number <- x
        plot_data$x_qval <- 4
        plot_data$y_qval <- 4
        if (dim(plot_data[(plot_data$x < x_quantile_values[4]),])[1] >
            0) {
            plot_data[(plot_data$x < x_quantile_values[4]),]$x_qval <- 3
        }
        if (dim(plot_data[(plot_data$x < x_quantile_values[3]),])[1] >
            0) {
            plot_data[(plot_data$x < x_quantile_values[3]),]$x_qval <- 2
        }
        if (dim(plot_data[(plot_data$x < x_quantile_values[2]),])[1] >
            0) {
            plot_data[(plot_data$x < x_quantile_values[2]),]$x_qval <- 1
        }
        
        if (dim(plot_data[(plot_data$y <= y_quantile_values[4]),])[1] >
            0) {
            plot_data[(plot_data$y <= y_quantile_values[4]),]$y_qval <- 3
        }
        if (dim(plot_data[(plot_data$y <= y_quantile_values[3]),])[1] >
            0) {
            plot_data[(plot_data$y <= y_quantile_values[3]),]$y_qval <- 2
        }
        if (dim(plot_data[(plot_data$y <= y_quantile_values[2]),])[1] >
            0) {
            plot_data[(plot_data$y <= y_quantile_values[2]),]$y_qval <- 1
        }
        
        
        
        if (dim(plot_data[(plot_data$x < x_quantile_values[3]),])[1] >
            0) {
            plot_data[(plot_data$x < x_quantile_values[3]),]$x_qval <- 2
        }
        if (dim(plot_data[(plot_data$x < x_quantile_values[2]),])[1] >
            0) {
            plot_data[(plot_data$x < x_quantile_values[2]),]$x_qval <- 1
        }
        
        if (dim(plot_data[(plot_data$y <= y_quantile_values[4]),])[1] >
            0) {
            plot_data[(plot_data$y <= y_quantile_values[4]),]$y_qval <- 3
        }
        if (dim(plot_data[(plot_data$y <= y_quantile_values[3]),])[1] >
            0) {
            plot_data[(plot_data$y <= y_quantile_values[3]),]$y_qval <- 2
        }
        if (dim(plot_data[(plot_data$y <= y_quantile_values[2]),])[1] >
            0) {
            plot_data[(plot_data$y <= y_quantile_values[2]),]$y_qval <- 1
        }
        plot_data$x_pval <-
            sapply(plot_data$x, function(x, all) {
                length(all[all <= x]) / length(all)
            }, plot_data$x)
        plot_data$y_pval <-
            sapply(plot_data$y, function(x, all) {
                length(all[all <= x]) / length(all)
            }, plot_data$y)
        
        plot_data$x_tval <- 1
        plot_data$x_tval[plot_data$x_pval > 0.1] <- 2
        plot_data$x_tval[plot_data$x_pval > 0.2] <- 3
        plot_data$x_tval[plot_data$x_pval > 0.3] <- 4
        plot_data$x_tval[plot_data$x_pval > 0.4] <- 5
        plot_data$x_tval[plot_data$x_pval > 0.5] <- 6
        plot_data$x_tval[plot_data$x_pval > 0.6] <- 7
        plot_data$x_tval[plot_data$x_pval > 0.7] <- 8
        plot_data$x_tval[plot_data$x_pval > 0.8] <- 9
        plot_data$x_tval[plot_data$x_pval > 0.9] <- 10
        
        
        plot_data$y_tval <- 1
        plot_data$y_tval[plot_data$y_pval > 0.1] <- 2
        plot_data$y_tval[plot_data$y_pval > 0.2] <- 3
        plot_data$y_tval[plot_data$y_pval > 0.3] <- 4
        plot_data$y_tval[plot_data$y_pval > 0.4] <- 5
        plot_data$y_tval[plot_data$y_pval > 0.5] <- 6
        plot_data$y_tval[plot_data$y_pval > 0.6] <- 7
        plot_data$y_tval[plot_data$y_pval > 0.7] <- 8
        plot_data$y_tval[plot_data$y_pval > 0.8] <- 9
        plot_data$y_tval[plot_data$y_pval > 0.9] <- 10
        
        xpercentile <- ecdf(plot_data$x)
        plot_data$neg_x_qval <- (xpercentile(unique(plot_data$x_negative_point)) * 4)
        ypercentile <- ecdf(plot_data$y)
        plot_data$neg_y_qval <- (ypercentile(unique(plot_data$y_negative_point)) * 4)
        
        return (plot_data)
    }
#### Get group average of selected feature ----
group_average <-
    function(x,
             complete_data,
             feature = "avg",
             window_size = 6,
             threshold) {
        window_data <-
            get_window_data(
                x = x, complete_data = complete_data , feature = feature,window_size = window_size,threshold = threshold
            )
        return (c(x,mean(window_data$x), mean(window_data$y)))
    }