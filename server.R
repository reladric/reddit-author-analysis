#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

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
    quadrant_data <-
      read.csv2(
        file = "./quadrant_values.csv",
        sep = ",",
        header = TRUE,
        stringsAsFactors = FALSE
      )
    quadrant_data$`X..of.users` = as.numeric(quadrant_data$`X..of.users`)
    quadrant_data$`Month.Number` = as.numeric(quadrant_data$`Month.Number`)
    return (list(
      user_statistics,
      activity_data,
      user_comments,
      quadrant_data
    ))
    
    
  }
#
# filepath = "./dev.csv"
# con = file(filepath, "r")
# authors=c()
# while (length(oneLine <- readLines(con, n = 1)) > 0) {
#   myLine <- unlist((strsplit(oneLine, ",")))
#   current_author<-c(myLine[1])
#   authors<-c( authors , current_author[ ! current_author %chin% authors ] )
# }
# close(con)

# ptm <- proc.time()
# crosstalking_data<-read.csv2(file = "./final_file.csv", sep=",", header=TRUE)
# crosstalking_data_table<-data.table(crosstalking_data)
# user_cp_data<-crosstalking_data_table[,list(crossposter=max(isCrossposterYet)),by=list(author, HomeSubreddit_x)]
# user_cp_data$crossposter[user_cp_data$crossposter!=1]<--1
# user_cp_data$crossposter[(user_cp_data$crossposter!=1&user_cp_data$HomeSubreddit_x =="Feminism" )]<--2
# print("Load complete")
# proc.time() - ptm
#
# user_statistics <-
#   read.csv2(file = "./user_statistics.csv", sep = ",", header = TRUE)
# colnames(user_statistics) <-
#   c(
#     "Author",
#     "Majority Subreddit",
#     "First Subreddit",
#     "Account Created Date",
#     "Total Post Karma (Score)",
#     "Total Comment Karma (Score"
#   )
# user_statistics$`Account Created Date`[user_statistics$`Account Created Date` ==
#                                          "NaT"] <- NA
# user_statistics$`Total Post Karma (Score)`[user_statistics$`Total Post Karma (Score)` ==
#                                              ""] <- NA
# user_statistics$`Total Comment Karma (Score`[user_statistics$`Total Comment Karma (Score` ==
#                                                ""] <- NA
#
# user_statistics$`Total Post Karma (Score)` <-
#   as.numeric(levels(user_statistics$`Total Post Karma (Score)`))[user_statistics$`Total Post Karma (Score)`]
# user_statistics$`Total Comment Karma (Score` <-
#   as.numeric(levels(user_statistics$`Total Comment Karma (Score`))[user_statistics$`Total Comment Karma (Score`]
#
# activity_data <-
#   read.csv2(file = "./author_posting_activity.csv", sep = ",", header = TRUE)
# activity_data$ratio <-
#   round(as.numeric(levels(activity_data$ratio))[activity_data$ratio], 2)
#
# user_comments <-
#   read.csv2(
#     file = "./comment_only_set.csv",
#     sep = ",",
#     header = TRUE,
#     stringsAsFactors = FALSE,
#     quote = "\"",
#     fileEncoding = "UTF-8"
#   )
#
#
# print("Load Complete")


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

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  # output$distPlot <- renderPlot({
  #   # generate bins based on input$bins from ui.R
  #   x    <- faithful[, 2]
  #   bins <- seq(min(x), max(x), length.out = input$bins + 1)
  #
  #   # draw the histogram with the specified number of bins
  #   hist(x, breaks = bins, col = 'darkgray', border = 'white')
  #
  # })
  # Create a Progress object
  progress <- shiny::Progress$new()
  progress$set(message = "Computing data", value = 0)
  # Close the progress when this reactive exits (even if there's an error)
  on.exit(progress$close())
  
  # Create a callback function to update progress.
  # Each time this is called:
  # - If `value` is NULL, it will move the progress bar 1/5 of the remaining
  #   distance. If non-NULL, it will set the progress to that value.
  # - It also accepts optional detail text.
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
  karma_quadrant_counts <-
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
  karma_quadrant_counts <- list_of_values[[4]]
  # output$q1_authorDropDown <- renderUI({
  #   selectInput(
  #     "q1_authors",
  #     "Majority Feminism",
  #     choices = getAuthors(1)[[2]],
  #     size = 4,
  #     selectize = FALSE
  #   )
  # })
  # output$q2_authorDropDown <- renderUI({
  #   selectInput(
  #     "q2_authors",
  #     "Majority Mensrights",
  #     choices = getAuthors(2)[[2]],
  #     size = 4,
  #     selectize = FALSE
  #   )
  # })
  # output$q3_authorDropDown <- renderUI({
  #   selectInput(
  #     "q3_authors",
  #     "Majority Feminism",
  #     choices = getAuthors(3)[[2]],
  #     size = 4,
  #     selectize = FALSE
  #   )
  # })
  # output$q4_authorDropDown <- renderUI({
  #   selectInput(
  #     "q4_authors",
  #     "Majority Mensrights",
  #     choices = getAuthors(4)[[2]],
  #     size = 4,
  #     selectize = FALSE
  #   )
  # })
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
          ), ]
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
          ), ]
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
        ), ]
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
        ), ]
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
      activity_data[activity_data$author == values$lastUpdated, ]
    x_end <- 0
    if (is.null(dim(plot_data))  || dim(plot_data)[1] == 0) {
      x_end <- 133
    } else{
      x_end <- max(plot_data$month_num)
    }
    
    plot <-
      ggplot(plot_data[order(plot_data$subreddit),],
             aes(x = month_num, y = post_count, fill = subreddit)) + geom_bar(stat = 'identity', width =
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
      min = min(karma_quadrant_counts$`Month.Number`),
      max = max(karma_quadrant_counts$`Month.Number`),
      value = min(karma_quadrant_counts$`Month.Number`)
    )
  })
  output$currentMonthPlot <- renderPlot({
    current_month_data<-data.table(karma_quadrant_counts[karma_quadrant_counts$Month.Number ==
                                                values$selected_month,])
    total_scores <-
      current_month_data[, list(X..of.users = sum(X..of.users)), by = list(Feminism.Score, Mens.Right.Score)]
    plot <-
      ggplot(data = total_scores, aes(x = `Feminism.Score`, y = `Mens.Right.Score`)) +
      geom_tile(aes(fill =  X..of.users))  +
      scale_fill_gradient2(low = "blue",
                           high = "darkgreen",
                           guide = "colorbar") +
      geom_text(aes(label = `X..of.users`)) +
      ggtitle(
        paste(
          "Mensrights v Feminism - Zscore of average Karma per comment (Month : ",
          values$selected_month,
          ")",
          sep = " "
        )
      ) +      theme(
        aspect.ratio = 1,
        plot.title = element_text(
          size = 15,
          face = "bold",
          hjust = 0.5
        )
      )
    plot
  })
})
