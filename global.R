library(shiny)
library(ggplot2)
library(scales)
library(data.table)
file_loaded <- 0

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
            text <- "1/5"
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
            text <- "2/5"
            updateProgress(detail = text)
        }
        activity_data <-
            read.csv2(file = "./author_posting_activity.csv",
                      sep = ",",
                      header = TRUE)
        if (is.function(updateProgress)) {
            text <- "3/5"
            updateProgress(detail = text)
        }
        activity_data$ratio <-
            round(as.numeric(levels(activity_data$ratio))[activity_data$ratio], 2)
        if (is.function(updateProgress)) {
            text <- "4/5"
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
            text <- "5/5"
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
        
        
        
        return (list(user_statistics,
                     activity_data,
                     user_comments,
                     score_data))
        
        
    }

getAuthorCategories <- function() {
    category_vector <- c(1,-1,-2)
    names(category_vector) <-
        c("Crossposters", "Mensrights", "Feminism")
    return (category_vector)
}
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
heatdata_for_month <- function(x, complete_data) {
    selected_score_data <- complete_data[complete_data$month <= x,]
    if (is.null(dim(selected_score_data)) ||
        dim(selected_score_data)[1] == 0) {
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
    heat_data$labelField = ""
    heat_data$labelField[(heat_data$`Feminism Score` == "+" &
                              heat_data$`Mens Right Score` == "+")] <-
        paste("Saints",heat_data$`# of users`[(heat_data$`Feminism Score` == "+" &
                                                   heat_data$`Mens Right Score` == "+")], sep = ": ")
    
    heat_data$labelField[(heat_data$`Feminism Score` == "+" &
                              heat_data$`Mens Right Score` == "-")] <-
        paste("Fem Partisan",heat_data$`# of users`[(heat_data$`Feminism Score` == "+" &
                                                         heat_data$`Mens Right Score` == "-")], sep = ": ")
    
    heat_data$labelField[(heat_data$`Feminism Score` == "-" &
                              heat_data$`Mens Right Score` == "+")] <-
        paste("MR Partisan",heat_data$`# of users`[(heat_data$`Feminism Score` == "-" &
                                                        heat_data$`Mens Right Score` == "+")], sep = ": ")
    
    heat_data$labelField[(heat_data$`Feminism Score` == "-" &
                              heat_data$`Mens Right Score` == "-")] <-
        paste("Trolls",heat_data$`# of users`[(heat_data$`Feminism Score` == "-" &
                                                   heat_data$`Mens Right Score` == "-")], sep = ": ")
    return (heat_data)
}
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