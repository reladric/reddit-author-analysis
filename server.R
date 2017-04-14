#### Server Start ----
shinyServer(function(input, output) {
    ## Test
    progress <- shiny::Progress$new()
    progress$set(message = "Computing data", value = 0)
    # Close the progress when this reactive exits (even if there's an error)
    on.exit(progress$close())
    
    #### Progress bar ----
    updateProgress <- function(value = NULL, detail = NULL) {
        if (is.null(value)) {
            value <- progress$getValue()
            value <- value + (progress$getMax() - value) / 5
        }
        progress$set(value = value, detail = detail)
    }
    #### Inits ----
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
    #### Reacive init ----
    values <-
        reactiveValues(
            lastUpdated = "Aerik",
            selected_month = 100,
            selected_window = 1,
            selected_feature = "score",
            scale = TRUE,
            center = "mean"
        )
    #### Tab 1 - User Analysis ----
    #### Tab 1 -  Quadrant selection for users  ----
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
    #### Tab 1 -  Monitors for quadrant selectors ----
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
    
    observe({
        lapply(names(inputs_to_monitor), function(x) {
            observe({
                input[[x]]
                values$lastUpdated <- input[[x]]
            })
        })
    })
    #### Tab 1 - User statistics ----
    output$stat_table <-
        renderTable({
            user_statistics[user_statistics$Author == values$lastUpdated,]
        })
    #### Tab 1 - Comment resampling - Feminism ----
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
    #### Tab 1 - Comment Resampling - Mensrights ----
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
    #### Tab 1 - Comment Intial text - Feminism ----
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
    #### Tab 1 - Comment Intial text - Mensrights ----
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
    #### Tab 1 - Crossposter Acitivity graph ----
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
    #### Tab 2 ----
    #### Tab 2 - Month slider input  ----
    output$monthSelector <- renderUI({
        sliderInput(
            "monthSelector",
            label =  "Up to Month",
            min = min(score_data$month) + values$selected_window,
            max = max(score_data$month),
            value = min(score_data$month)
        )
    })
    #### Tab 2 - Month slider monitor ----
    observeEvent(input$monthSelector, {
        values$selected_month <- input[["monthSelector"]]
    })
    #### Tab 2 - Quadrant Heatmap ----
    output$currentMonthHeatMap <- renderPlot({
        karma_quadrant_counts = heatdata_for_month(values$selected_month, score_data)
        plot <-
            ggplot(data = karma_quadrant_counts, aes(x = `Feminism Score`, y = `Mens Right Score`)) +
            geom_tile(aes(fill =  `# of users`))  +
            scale_fill_gradient2(low = "blue",
                                 high = "darkgreen",
                                 guide = "colorbar") +
            geom_text(aes(label =  `labelField`)) +
            ggtitle(
                paste(
                    "Mensrights v Feminism - Zscore of average cumulative karma per comment (Month : ",
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
    #### Tab 2 - Gradient scatter plot ----
    output$currentMonthPlot <- renderPlot({
        plot_data <-
            plotdata_for_month(
                as.numeric(values$selected_month),
                score_data,
                values$selected_feature,
                values$scale,
                values$center,
                as.numeric(values$selected_window)
                
            )
        
        plot <-
            ggplot(plot_data, aes(x = x, y = y)) +
            stat_density2d(aes(fill = ..level..), geom = "polygon") +
            scale_alpha_continuous(limits = c(0, .5), breaks = seq(0, .5, by = 0.1)) +
            ggtitle(
                paste(
                    "Mensrights v Feminism -",
                    values$feature,
                    "score for",
                    values$selected_month,
                    "(sliding window:",
                    values$selected_window,
                    ")",
                    sep = " "
                )
            ) +      theme(plot.title = element_text(
                size = 15,
                face = "bold",
                hjust = 0.5
            )) +
            geom_vline(xintercept = unique(plot_data$vline)) +
            geom_hline(yintercept = unique(plot_data$hline)) +
            geom_vline(xintercept = unique(plot_data$y_negative_point), col =
                           "red") +
            geom_hline(yintercept = unique(plot_data$x_negative_point), col =
                           "red") +
            xlab(paste("Feminism", values$feature, sep = " ")) +
            ylab(paste("MensRights", values$feature, sep = " "))
        plot
    })
    #### Tab 2 -User details ----
    # output$selected_user_details <-
    #   renderText({
    #     if (is.null(input$plot_click$x)) {
    #       "Click on the plot"
    #     } else{
    #       y_reduced <- plotdata_for_month(values$selected_month, score_data)
    #       previous_row_count <- nrow(y_reduced)
    #       granularity <- 0.5
    #       loopcount <- 0
    #       while (TRUE) {
    #         loopcount <- loopcount + 1
    #         x_reduced <-
    #           y_reduced[(
    #             y_reduced$fem_z_score < input$plot_click$x + granularity &
    #               y_reduced$fem_z_score > input$plot_click$x - granularity
    #           ), ]
    #         y_reduced <-
    #           x_reduced[(
    #             x_reduced$mr_z_score < input$plot_click$y + granularity &
    #               x_reduced$mr_z_score > input$plot_click$y - granularity
    #           ), ]
    #         current_row_count <- nrow(y_reduced)
    #         if (current_row_count < 1) {
    #           granularity <- granularity + (granularity / 10)
    #           #print("Increasing granularity")
    #         }
    #         else if (current_row_count > 1) {
    #           granularity <- granularity - (granularity / 10)
    #           #print("Reducing granularity")
    #         } else{
    #           print("Zeroed in")
    #           break
    #         }
    #         if (loopcount > 100) {
    #           y_reduced$user = "Unable to resolve user. Try again"
    #           break
    #         }
    #         print(loopcount)
    #         previous_row_count <- current_row_count
    #       }
    #       y_reduced$user
    #     }
    #   })
    
})
