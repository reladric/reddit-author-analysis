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
                                                   heat_data$`Mens Right Score` == "+")], sep = ": ")
    return (heat_data)
}
plotdata_for_month <- function(x, complete_data) {
    selected_score_data <- complete_data[complete_data$month <= x,]
    if (is.null(dim(selected_score_data)) ||
        dim(selected_score_data)[1] == 0) {
        plot_data = data.frame(x = numeric(0), y = numeric(0))
    }else{
        selected_score_table = data.table(selected_score_data)
        plot_data <-
            selected_score_table[, list(
                fem_score = sum(karmaFem),
                mr_score = sum(karmaMR),
                fem_posts = sum(numPostsFem),
                mr_posts = sum(numPostsMR),
                duration = max(month) - min(month)
            ), by = user]
        plot_data$fem_avg <-
            plot_data$fem_score / plot_data$fem_posts
        plot_data$mr_avg <-
            plot_data$mr_score / plot_data$mr_posts
        plot_data$fem_z_score <- scale(plot_data$fem_avg)
        plot_data$mr_z_score <- scale(plot_data$mr_avg)
        
    }
    return (plot_data)
}

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
    progress <- shiny::Progress$new()
    progress$set(message = "Computing data", value = 0)
    # Close the progress when this reactive exits (even if there's an error)
    on.exit(progress$close())
    
    
    updateProgress <- function(value = NULL, detail = NULL) {
        if (is.null(value)) {
            value <- progress$getValue()
            value <- value + (progress$getMax() - value) / 5
        }
        progress$set(value = value, detail = detail)
    }
    user_statistics <- data.frame(x = numeric(0), y = numeric(0))
    activity_data <- data.frame(x = numeric(0), y = numeric(0))
    user_comments <- data.frame(x = numeric(0), y = numeric(0))
    score_data <-
        data.frame(x = numeric(0), y = numeric(0))
    
    list_of_values <-
        compute_data(updateProgress,
                     user_statistics,
                     activity_data,
                     user_comments)
    print(class(list_of_values[1]))
    print(class(list_of_values[2]))
    print(class(list_of_values[3]))
    user_statistics <- list_of_values[[1]]
    activity_data <- list_of_values[[2]]
    user_comments <- list_of_values[[3]]
    score_data <- list_of_values[[4]]
    
    output$q1_count <- renderText({
        paste("Quadrant Count:", getAuthors(1, user_statistics)[[1]])
    })
    output$q2_count <- renderText({
        paste("Quadrant Count:", getAuthors(2, user_statistics)[[1]])
    })
    output$q3_count <- renderText({
        paste("Quadrant Count:", getAuthors(3, user_statistics)[[1]])
    })
    output$q4_count <- renderText({
        paste("Quadrant Count:", getAuthors(4, user_statistics)[[1]])
    })
    values <-
        reactiveValues(lastUpdated = "Aerik", selected_month = 100)
    inputs_to_monitor <-
        c("q1_authors",
          "q2_authors",
          "q3_authors",
          "q4_authors")
    observeEvent(input$q1_authors, {
        author <- getAuthors(1, user_statistics)[[2]]
        values$lastUpdated <- author
    })
    observeEvent(input$q2_authors, {
        author <- getAuthors(2, user_statistics)[[2]]
        values$lastUpdated <- author
    })
    observeEvent(input$q3_authors, {
        author <- getAuthors(3, user_statistics)[[2]]
        values$lastUpdated <- author
    })
    observeEvent(input$q4_authors, {
        author <- getAuthors(4, user_statistics)[[2]]
        values$lastUpdated <- author
    })
    observeEvent(input$monthSelector, {
        values$selected_month <- input[["monthSelector"]]
    })
    
    observe({
        lapply(names(inputs_to_monitor), function(x) {
            observe({
                input[[x]]
                values$lastUpdated <- input[[x]]
            })
        })
    })
    output$stat_table <-
        renderTable({
            user_statistics[user_statistics$Author == values$lastUpdated,]
        })
    observeEvent(input$resample_fem, {
        output$fem_comment <-
            renderText({
                current_user_fem_comment <-
                    user_comments[(
                        user_comments$author == values$lastUpdated &
                            user_comments$subreddit == "Feminism"
                    ),]
                if (is.null(nrow(current_user_fem_comment)) ||
                    nrow(current_user_fem_comment) == 0) {
                    res = "No Comments"
                } else{
                    rownames(current_user_fem_comment) <-
                        1:nrow(current_user_fem_comment)
                    selected_comment = current_user_fem_comment[sample(nrow(current_user_fem_comment), 1),]
                    selected_comment_stat = paste("<br/> <b>Score:</b> ", selected_comment$score)
                    res = markdown::markdownToHTML(text = selected_comment$body, fragment.only = TRUE)
                    res = paste(res, selected_comment_stat)
                }
                res
            })
    })
    observeEvent(input$resample_mr, {
        output$mr_comment <-
            renderText({
                current_user_mr_comment <-
                    user_comments[(
                        user_comments$author == values$lastUpdated &
                            user_comments$subreddit == "MensRights"
                    ),]
                if (is.null(nrow(current_user_mr_comment)) ||
                    nrow(current_user_mr_comment) == 0) {
                    res = "No Comments"
                } else{
                    rownames(current_user_mr_comment) <-
                        1:nrow(current_user_mr_comment)
                    selected_comment = current_user_mr_comment[sample(nrow(current_user_mr_comment), 1),]
                    selected_comment_stat = paste("<br/> <b>Score:</b> ", selected_comment$score)
                    res = markdown::markdownToHTML(text = selected_comment$body, fragment.only = TRUE)
                    res = paste(res, selected_comment_stat)
                }
                res
            })
    })
    output$fem_comment <-
        renderText({
            current_user_fem_comment <-
                user_comments[(
                    user_comments$author == values$lastUpdated &
                        user_comments$subreddit == "Feminism"
                ),]
            if (is.null(nrow(current_user_fem_comment)) ||
                nrow(current_user_fem_comment) == 0) {
                res = "No Comments"
            } else{
                rownames(current_user_fem_comment) <-
                    1:nrow(current_user_fem_comment)
                selected_comment = current_user_fem_comment[sample(nrow(current_user_fem_comment), 1),]
                selected_comment_stat = paste("<br/> <b>Score:</b> ", selected_comment$score)
                res = markdown::markdownToHTML(text = selected_comment$body, fragment.only = TRUE)
                res = paste(res, selected_comment_stat)
            }
            res
        })
    
    output$mr_comment <-
        renderText({
            current_user_mr_comment <-
                user_comments[(
                    user_comments$author == values$lastUpdated &
                        user_comments$subreddit == "MensRights"
                ),]
            if (is.null(nrow(current_user_mr_comment)) ||
                nrow(current_user_mr_comment) == 0) {
                res = "No Comments"
            } else{
                rownames(current_user_mr_comment) <-
                    1:nrow(current_user_mr_comment)
                selected_comment = current_user_mr_comment[sample(nrow(current_user_mr_comment), 1),]
                selected_comment_stat = paste("<br/> <b>Score:</b> ", selected_comment$score)
                res = markdown::markdownToHTML(text = selected_comment$body, fragment.only = TRUE)
                res = paste(res, selected_comment_stat)
            }
            res
        })
    output$activity_graph <- renderPlot({
        plot_data <-
            activity_data[activity_data$author == values$lastUpdated,]
        x_end <- 0
        if (is.null(dim(plot_data))  || dim(plot_data)[1] == 0) {
            x_end <- 133
        } else{
            x_end <- max(plot_data$month_num)
        }
        
        plot <-
            ggplot(plot_data[order(plot_data$subreddit),],
                   aes(
                       x = month_num, y = post_count, fill = subreddit
                   )) + geom_bar(stat = 'identity', width =
                                     0.8)  + xlab("Months since origin") +
            ylab("Ratio of posts") +
            ggtitle("Monthwise posting composition") +      theme(
                aspect.ratio = 4 / 16,
                legend.position = "left",
                plot.title = element_text(
                    size = 15,
                    face = "bold",
                    hjust = 0.5
                )
            )
        plot + scale_x_continuous(breaks = seq(0, x_end , by = 10))
    })
    output$monthSelector <- renderUI({
        sliderInput(
            "monthSelector",
            label =  "Month",
            min = min(score_data$month),
            max = max(score_data$month),
            value = min(score_data$month)
        )
    })
    output$currentMonthHeatMap <- renderPlot({
        karma_quadrant_counts = heatdata_for_month(values$selected_month,score_data)
        plot <-
            ggplot(data = karma_quadrant_counts, aes(x = `Feminism Score`, y = `Mens Right Score`)) +
            geom_tile(aes(fill =  `# of users`))  +
            scale_fill_gradient2(low = "blue",
                                 high = "darkgreen",
                                 guide = "colorbar") +
            geom_text(aes(label =  `labelField`)) +
            ggtitle(
                paste(
                    "Mensrights v Feminism - Zscore of average Karma per comment (Month : ",
                    values$selected_month,
                    ")",
                    sep = " "
                )
            ) +      theme(plot.title = element_text(
                size = 15,
                face = "bold",
                hjust = 0.5
            ))
        plot
    })
    output$currentMonthPlot <- renderPlot({
        plot_data <-
            plotdata_for_month(values$selected_month,score_data)
        plot <-
            ggplot(data = plot_data, aes(x = `fem_z_score`, y = `mr_z_score`)) +
            geom_point(alpha = 0.3) +
            ggtitle(
                paste(
                    "Mensrights v Feminism - Zscore of average Karma per comment (Month : ",
                    values$selected_month,
                    ")",
                    sep = " "
                )
            ) +      theme(plot.title = element_text(
                size = 15,
                face = "bold",
                hjust = 0.5
            )) + geom_vline(xintercept = 0) + geom_hline(yintercept = 0) + coord_trans(y =
                                                                                           "log10", x = "log10")
        plot
    })
})
