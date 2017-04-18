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
            value <- value + (progress$getMax() - value) / 6
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
    
    centering_values <- c("mean", "median")
    names(centering_values) <- c("Mean", "Median")
    
    feature_values <- c("avg", "score")
    names(feature_values) <-
        c("Score per Post in the window", "Raw score in the window")
    
    window_values <- seq(1, (max(score_data$month) / 2))
    names(window_values) <- window_values
    
    scale_values <- c(TRUE, FALSE)
    names(scale_values) <- c("Yes", "No")
    
    threshold_values <- c(0,1,2,5,10)
    names(threshold_values) <- threshold_values
    #### Reacive init ----
    values <-
        reactiveValues(
            lastUpdated = "Aerik",
            selected_month = 100,
            selected_window = 10,
            selected_feature = "avg",
            scale = TRUE,
            center = "median",
            firstVector_x_quantile = 1,
            firstVector_y_quantile = 1,
            secondVector_x_quantile = 1,
            secondVector_y_quantile = 1,
            firstForward_x_quantile = 1,
            firstForward_y_quantile = 1,
            secondForward_x_quantile = 1,
            secondForward_y_quantile = 1,
            threshold = 2
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
            label =  "Month (Window End)",
            min = min(score_data$month) + as.numeric(values$selected_window),
            max = max(score_data$month),
            value = values$selected_month
        )
    })
    #### Tab 2 - Plot Controls input  ----
    output$centering <- renderUI({
        selectInput(
            "centering",
            choices = centering_values,
            label = "Center",
            selected = centering_values[2]
        )
    })
    
    output$feature <- renderUI({
        selectInput(
            "feature",
            choices = feature_values,
            label = "Feature",
            selected = feature_values[1]
        )
    })
    
    output$window_size <- renderUI({
        selectInput(
            "window_size",
            choices = window_values,
            label = "Window Size",
            selected =
                window_values[length(window_values) / 2]
        )
    })
    output$threshold <- renderUI({
        selectInput(
            "threshold",
            choices = threshold_values,
            label = "Threshold on number of posts in window",
            selected = 2
            
        )
    })
    output$scale <- renderUI({
        selectInput(
            "scale",
            choices = scale_values,
            label = "Scale (Z-score)?",
            selected = scale_values[1]
        )
    })
    #### Tab 2 - Month slider monitor ----
    observeEvent(input$monthSelector, {
        values$selected_month <- input[["monthSelector"]]
    })
    #### Tab 2 - Plot contrfols monitor ----
    observeEvent(input$centering, {
        values$center <- input[["centering"]]
    })
    observeEvent(input$feature, {
        values$selected_feature <- input[["feature"]]
    })
    observeEvent(input$window_size, {
        values$selected_window <- as.numeric(input[["window_size"]])
    })
    observeEvent(input$scale, {
        values$scale <- input[["scale"]]
    })
    observeEvent(input$threshold, {
        values$threshold <- input[["threshold"]]
    })
    #### Tab 2 Group Average of metric ----
    output$groupAverage <- renderPlot({
        monthly_averages <-
            as.data.frame(t(
                sapply(
                    unique(score_data$month),
                    group_average,
                    complete_data = score_data,
                    feature = values$selected_feature,
                    window_size = values$selected_window,
                    threshold=values$threshold
                )
            ))
        colnames(monthly_averages) <- c("month", "fem", "mr")
        plot <-
            ggplot(data = monthly_averages, aes(month)) + geom_line(aes(y = fem, colour =
                                                                            "feminism")) + geom_line(aes(y = mr, colour = "mensrights")) +
            ggtitle(
                paste(
                    "Group Average of Feminism & Mensrights",
                    values$selected_feature,
                    "for window:",
                    values$selected_window,
                    sep = " "
                )
            ) +      theme(plot.title = element_text(
                size = 15,
                face = "bold",
                hjust = 0.5
            )) +
            xlab("Month (Window end)") +
            ylab("Group Average")
        plot
    })
    ### Tab 2 - Data extraction based on month selection
    current_month_data <- reactive({
        plotdata_for_month(
            as.numeric(values$selected_month),
            score_data,
            values$selected_feature,
            values$scale,
            values$center,
            as.numeric(values$selected_window),
            threshold = values$threshold
        )
    })
    #### Tab 2 - Quadrant Heatmap ----
    output$currentMonthHeatMap <- renderPlot({
        plot_data <-
            current_month_data()
        hline <-
            data.frame(yint = unique(plot_data$neg_y_qval) ,lt = 'Negative Score Axis')
        vline <-
            data.frame(xint = unique(plot_data$neg_x_qval),lt = 'Negative Score Axis')
        plot_data$x_qval<- plot_data$x_qval-0.5
        plot_data$y_qval<- plot_data$y_qval-0.5
        plot_data_table <- data.table(plot_data)
        quadrant_counts <-
            as.data.frame(plot_data_table[order(x_qval, y_qval), list(count = length(user)), by = list(x_qval, y_qval)])
        quadrant_counts$ratio <-
            quadrant_counts$count / sum(quadrant_counts$count)
        plot <-
            ggplot(data = quadrant_counts, aes(x = `x_qval`, y = `y_qval`)) +
            geom_tile(aes(fill =  `ratio`))  +
            geom_hline(yintercept = 2) +
            geom_vline(xintercept = 2) +
            scale_fill_gradient2(low = "blue",
                                 high = "darkgreen",
                                 guide = "colorbar") +
            geom_hline(
                data = hline,aes(yintercept = yint,linetype = lt),color = "red",size = 1
            ) +
            geom_vline(
                data = vline,aes(xintercept = xint,linetype = lt),color = "red",size = 1
            ) +
            scale_colour_discrete(guide = "none") +
            scale_linetype_manual(name = 'Legend',values = 1,guide = "legend") +
            geom_text(aes(label =  round(`ratio`, 3))) +
            ggtitle(
                paste(
                    "Mensrights v Feminism - Quantiles of",
                    values$selected_feature,
                    "density heat tiles for ",
                    values$selected_month,
                    "(Window size : ",
                    values$selected_window,
                    ")",
                    sep = " "
                )
            ) +      theme(plot.title = element_text(
                size = 15,
                face = "bold",
                hjust = 0.5
            )) +
            xlab("Feminism Quantile") +
            ylab("MensRights Quantile")
        plot
    })
    #### Tab 2 - Quadrant Heatmap Duplicate 1----
    output$currentMonthHeatMap_1 <- renderPlot({
        plot_data <-
            current_month_data()
        plot_data_table <- data.table(plot_data)
        quadrant_counts <-
            as.data.frame(plot_data_table[order(x_qval, y_qval), list(count = length(user)), by = list(x_qval, y_qval)])
        quadrant_counts$ratio <-
            quadrant_counts$count / sum(quadrant_counts$count)
        plot <-
            ggplot(data = quadrant_counts, aes(x = `x_qval`, y = `y_qval`)) +
            geom_tile(aes(fill =  `ratio`))  +
            geom_hline(yintercept = 2.5) +
            geom_vline(xintercept = 2.5) +
            scale_fill_gradient2(low = "blue",
                                 high = "darkgreen",
                                 guide = "colorbar") +
            geom_text(aes(label =  round(`ratio`, 3))) +
            ggtitle(
                paste(
                    "Mensrights v Feminism - Density heat tiles of",
                    values$selected_feature,
                    "for ",
                    values$selected_month,
                    "(Window : ",
                    values$selected_window,
                    ")",
                    sep = " "
                )
            ) +      theme(plot.title = element_text(
                size = 12,
                face = "bold",
                hjust = 0.5
            )) +
            xlab("Feminism Quantile") +
            ylab("MensRights Quantile")
        plot
    })
    #### Tab 2 - Quadrant Heatmap Duplicate 2----
    output$currentMonthHeatMap_2 <- renderPlot({
        plot_data <-
            current_month_data()
        plot_data_table <- data.table(plot_data)
        quadrant_counts <-
            as.data.frame(plot_data_table[order(x_qval, y_qval), list(count = length(user)), by = list(x_qval, y_qval)])
        quadrant_counts$ratio <-
            quadrant_counts$count / sum(quadrant_counts$count)
        plot <-
            ggplot(data = quadrant_counts, aes(x = `x_qval`, y = `y_qval`)) +
            geom_tile(aes(fill =  `ratio`))  +
            geom_hline(yintercept = 2.5) +
            geom_vline(xintercept = 2.5) +
            scale_fill_gradient2(low = "blue",
                                 high = "darkgreen",
                                 guide = "colorbar") +
            geom_text(aes(label =  round(`ratio`, 3))) +
            ggtitle(
                paste(
                    "Mensrights v Feminism - Density heat tiles of",
                    values$selected_feature,
                    "for ",
                    values$selected_month,
                    "(Window : ",
                    values$selected_window,
                    ")",
                    sep = " "
                )
            ) +      theme(plot.title = element_text(
                size = 12,
                face = "bold",
                hjust = 0.5
            )) +
            xlab("Feminism Quantile") +
            ylab("MensRights Quantile")
        plot
    })
    #### Tab 2 - Gradient scatter plot ----
    output$currentMonthPlot <- renderPlot({
        plot_data <-
            current_month_data()
        hlinedf <-
            data.frame(yint = unique(plot_data$y_negative_point),lt = 'Negative Score Axis')
        vlinedf <-
            data.frame(xint = unique(plot_data$x_negative_point),lt = 'Negative Score Axis')
        
        plot <-
            ggplot(plot_data, aes(x = x, y = y)) +
            stat_density2d(aes(alpha = ..level..,fill = ..level..), geom = "polygon") +
            scale_fill_gradient(low = "#3ea87a", high = "#181f1c") +
            scale_alpha(range = c(0.00, 0.75), guide = FALSE) +
            ggtitle(
                paste(
                    "Mensrights v Feminism - Density contour map",
                    values$selected_feature,
                    "for ",
                    values$selected_month,
                    "(Window : ",
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
            geom_hline(
                data = hlinedf,aes(yintercept = yint,linetype = lt),color = "red",size = 1
            ) +
            geom_vline(
                data = vlinedf,aes(xintercept = xint,linetype = lt),color = "red",size = 1
            ) +
            scale_colour_discrete(guide = "none") +
            scale_linetype_manual(name = 'Legend',values = 1,guide = "legend") +
            
            xlab(paste("Feminism", values$selected_feature, sep = " ")) +
            ylab(paste("MensRights", values$selected_feature, sep = " "))
        plot
    })
    #### Tab 2 - Quantile Movements ----
    #### Tab 2 - Quantile Click Capture ----
    observe({
        input$firstVectorclick
        isolate({
            if (!is.null(input$firstVectorclick$x))
            {
                values$firstVector_x_quantile = round(as.numeric(input$firstVectorclick$x))
                values$firstVector_y_quantile = round(as.numeric(input$firstVectorclick$y))
            }
        })
    })
    
    observe({
        input$secondVectorclick
        isolate({
            if (!is.null(input$secondVectorclick$x))
            {
                values$secondVector_x_quantile = round(as.numeric(input$secondVectorclick$x))
                values$secondVector_y_quantile = round(as.numeric(input$secondVectorclick$y))
            }
        })
    })
    observe({
        input$firstForwardclick
        isolate({
            if (!is.null(input$firstForwardclick$x))
            {
                values$firstForward_x_quantile = round(as.numeric(input$firstForwardclick$x))
                values$firstForward_y_quantile = round(as.numeric(input$firstForwardclick$y))
            }
        })
    })
    observe({
        input$secondForwardclick
        isolate({
            if (!is.null(input$secondForwardclick$x))
            {
                values$secondForward_x_quantile = round(as.numeric(input$secondForwardclick$x))
                values$secondForward_y_quantile = round(as.numeric(input$secondForwardclick$y))
            }
        })
    })
    #### Tab 2 Vector Field - One Month ago ----
    output$firstVectorField <-
        renderPlot({
            second_month_data <- plotdata_for_month(
                x = values$selected_month - 1,
                complete_data = score_data,
                feature = values$selected_feature,
                scale = values$scale,
                window_size = values$selected_window,
                center = values$center,
                threshold = values$threshold
            )
            
            base_month_data <- current_month_data()[, c(1, 16, 17)]
            colnames(base_month_data) <-
                c("user", "current_xqval", "current_yqval")
            
            previous_month_data <- second_month_data[, c(1, 16, 17)]
            colnames(previous_month_data) <-
                c("user", "previous_xqval", "previous_yqval")
            
            common_rows <-
                merge(base_month_data, previous_month_data, by =
                          "user")
            common_rows_table <- data.table(common_rows)
            
            common_rows_agg <-
                as.data.frame(common_rows_table[, list(count  = length(user)), by = list(current_xqval,
                                                                                         current_yqval,
                                                                                         previous_xqval,
                                                                                         previous_yqval)])
            quiver_plot_data <-
                common_rows_agg[(
                    common_rows_agg$current_xqval == values$firstVector_x_quantile &
                        common_rows_agg$current_yqval == values$firstVector_y_quantile
                ),]
            ggplot(data = quiver_plot_data,
                   aes(x = previous_xqval , y = previous_yqval + 0.25)) +
                geom_segment(aes(xend = current_xqval , yend = current_yqval - 0.25),
                             arrow = arrow(length = unit(0.3, "cm"))) +
                geom_text(data = quiver_plot_data,
                          aes(
                              x = previous_xqval - 0.15  ,
                              y = (previous_yqval + 0.3),
                              label = count
                          )) +
                geom_hline(yintercept = 2.5, col = "red") +
                geom_vline(xintercept = 2.5, col = "red") +
                geom_hline(yintercept = 1.5, col = "blue") +
                geom_vline(xintercept = 1.5, col = "blue") +
                geom_hline(yintercept = 3.5, col = "blue") +
                geom_vline(xintercept = 3.5, col = "blue") +
                geom_hline(yintercept = 0.5, col = "blue") +
                geom_vline(xintercept = 0.5, col = "blue") +
                geom_hline(yintercept = 4.5, col = "blue") +
                geom_vline(xintercept = 4.5, col = "blue") +
                xlim(c(0.5, 4.5)) +
                ylim(c(0.5, 4.5)) +
                theme(aspect.ratio = 1) + xlab("Feminism Quantile") +
                ylab("MensRights Quantile") +
                ggtitle("Quantile Movement from 1 Month ago to current month (right)") +
                theme(
                    plot.title = element_text(
                        size = 12,
                        face = "bold",
                        hjust = 0.5
                    ),
                    panel.grid.major = element_blank(),
                    panel.grid.minor = element_blank()
                )
        })
    #### Tab 2 Vector Field - Two Month ago ----
    output$secondVectorField <-
        renderPlot({
            second_month_data <- plotdata_for_month(
                x = values$selected_month - 2,
                complete_data = score_data,
                feature = values$selected_feature,
                scale = values$scale,
                window_size = values$selected_window,
                center = values$center,
                threshold = values$threshold
            )
            
            base_month_data <- current_month_data()[, c(1, 16, 17)]
            colnames(base_month_data) <-
                c("user", "current_xqval", "current_yqval")
            
            
            previous_month_data <- second_month_data[, c(1, 16, 17)]
            colnames(previous_month_data) <-
                c("user", "past_xqval", "past_yqval")
            
            
            common_rows <-
                merge(base_month_data, previous_month_data, by =
                          "user")
            common_rows_table <- data.table(common_rows)
            
            common_rows_agg <-
                as.data.frame(common_rows_table[, list(count  = length(user)), by = list(current_xqval, current_yqval, past_xqval, past_yqval)])
            
            quiver_plot_data <-
                common_rows_agg[(
                    common_rows_agg$current_xqval == values$secondVector_x_quantile &
                        common_rows_agg$current_yqval == values$secondVector_y_quantile
                ),]
            ratio <- .25 / (256 / 5)
            ggplot(data = quiver_plot_data,
                   aes(x = past_xqval , y = past_yqval + 0.25)) +
                geom_segment(aes(xend = current_xqval , yend = current_yqval - 0.25),
                             arrow = arrow(length = unit(0.3, "cm"))) +
                geom_text(data = quiver_plot_data,
                          aes(
                              x = past_xqval - 0.15  ,
                              y = (past_yqval + 0.3),
                              label = count
                          )) +
                geom_hline(yintercept = 2.5, col = "red") +
                geom_vline(xintercept = 2.5, col = "red") +
                geom_hline(yintercept = 1.5, col = "blue") +
                geom_vline(xintercept = 1.5, col = "blue") +
                geom_hline(yintercept = 3.5, col = "blue") +
                geom_vline(xintercept = 3.5, col = "blue") +
                geom_hline(yintercept = 0.5, col = "blue") +
                geom_vline(xintercept = 0.5, col = "blue") +
                geom_hline(yintercept = 4.5, col = "blue") +
                geom_vline(xintercept = 4.5, col = "blue") +
                xlim(c(0.5, 4.5)) +
                ylim(c(0.5, 4.5)) +
                theme(aspect.ratio = 1) + xlab("Feminism Quantile") +
                ylab("MensRights Quantile") +
                ggtitle("Quantile Movement from 2 Month ago to current month (right)") +      theme(
                    plot.title = element_text(
                        size = 12,
                        face = "bold",
                        hjust = 0.5
                    ),
                    panel.grid.major = element_blank(),
                    panel.grid.minor = element_blank()
                )
        })
    #### Tab 2 Vector Field - One Month forward ----
    output$firstForwardField <-
        renderPlot({
            second_month_data <- plotdata_for_month(
                x = values$selected_month + 1,
                complete_data = score_data,
                feature = values$selected_feature,
                scale = values$scale,
                window_size = values$selected_window,
                center = values$center,
                threshold = values$threshold
            )
            base_month_data <- current_month_data()[, c(1, 16, 17)]
            colnames(base_month_data) <-
                c("user", "current_xqval", "current_yqval")
            
            previous_month_data <- second_month_data[, c(1, 16, 17)]
            colnames(previous_month_data) <-
                c("user", "next_xqval", "next_yqval")
            
            common_rows <-
                merge(base_month_data, previous_month_data, by =
                          "user")
            common_rows_table <- data.table(common_rows)
            
            common_rows_agg <-
                as.data.frame(common_rows_table[, list(count  = length(user)), by = list(current_xqval, current_yqval, next_xqval, next_yqval)])
            
            quiver_plot_data <-
                common_rows_agg[(
                    common_rows_agg$current_xqval == values$firstForward_x_quantile &
                        common_rows_agg$current_yqval == values$firstForward_y_quantile
                ),]
            ggplot(data = quiver_plot_data,
                   aes(x = current_xqval , y = current_yqval + 0.25)) +
                geom_segment(aes(xend = next_xqval , yend = next_yqval - 0.25),
                             arrow = arrow(length = unit(0.3, "cm"))) +
                geom_text(data = quiver_plot_data,
                          aes(
                              x = next_xqval - 0.15  ,
                              y = (next_yqval - 0.3),
                              label = count
                          )) +
                geom_hline(yintercept = 2.5, col = "red") +
                geom_vline(xintercept = 2.5, col = "red") +
                geom_hline(yintercept = 1.5, col = "blue") +
                geom_vline(xintercept = 1.5, col = "blue") +
                geom_hline(yintercept = 3.5, col = "blue") +
                geom_vline(xintercept = 3.5, col = "blue") +
                geom_hline(yintercept = 0.5, col = "blue") +
                geom_vline(xintercept = 0.5, col = "blue") +
                geom_hline(yintercept = 4.5, col = "blue") +
                geom_vline(xintercept = 4.5, col = "blue") +
                xlim(c(0.5, 4.5)) +
                ylim(c(0.5, 4.5)) +
                theme(aspect.ratio = 1) + xlab("Feminism Quantile") +
                ylab("MensRights Quantile") +
                ggtitle("Quantile Movement from current month(left) to next month") +
                theme(
                    plot.title = element_text(
                        size = 12,
                        face = "bold",
                        hjust = 0.5
                    ),
                    panel.grid.major = element_blank(),
                    panel.grid.minor = element_blank()
                )
        })
    #### Tab 2 Vector Field - Two Month forward ----
    output$secondForwardField <-
        renderPlot({
            second_month_data <- plotdata_for_month(
                x = values$selected_month + 2,
                complete_data = score_data,
                feature = values$selected_feature,
                scale = values$scale,
                window_size = values$selected_window,
                center = values$center,
                threshold = values$threshold
            )
            
            base_month_data <- current_month_data()[, c(1, 16, 17)]
            colnames(base_month_data) <-
                c("user", "current_xqval", "current_yqval")
            
            previous_month_data <- second_month_data[, c(1, 16, 17)]
            colnames(previous_month_data) <-
                c("user", "future_xqval", "future_yqval")
            
            common_rows <-
                merge(base_month_data, previous_month_data, by =
                          "user")
            common_rows_table <- data.table(common_rows)
            
            common_rows_agg <-
                as.data.frame(common_rows_table[, list(count  = length(user)),
                                                by = list(current_xqval, current_yqval, future_xqval, future_yqval)])
            
            quiver_plot_data <-
                common_rows_agg[(
                    common_rows_agg$current_xqval == values$secondForward_x_quantile &
                        common_rows_agg$current_yqval == values$secondForward_y_quantile
                ),]
            
            ggplot(data = quiver_plot_data,
                   aes(x = current_xqval , y = current_yqval + 0.25)) +
                geom_segment(aes(xend = future_xqval   , yend = future_yqval - 0.25),
                             arrow = arrow(length = unit(0.3, "cm"))) +
                geom_text(data = quiver_plot_data,
                          aes(
                              x = future_xqval - 0.15  ,
                              y = (future_yqval - 0.3),
                              label = count
                          )) +
                geom_hline(yintercept = 2.5, col = "red") +
                geom_vline(xintercept = 2.5, col = "red") +
                geom_hline(yintercept = 1.5, col = "blue") +
                geom_vline(xintercept = 1.5, col = "blue") +
                geom_hline(yintercept = 3.5, col = "blue") +
                geom_vline(xintercept = 3.5, col = "blue") +
                geom_hline(yintercept = 0.5, col = "blue") +
                geom_vline(xintercept = 0.5, col = "blue") +
                geom_hline(yintercept = 4.5, col = "blue") +
                geom_vline(xintercept = 4.5, col = "blue") +
                xlim(c(0.5, 4.5)) +
                ylim(c(0.5, 4.5)) +
                theme(aspect.ratio = 1) + xlab("Feminism Quantile") +
                ylab("MensRights Quantile") +
                ggtitle("Quantile Movement from current month(left) to 2 months forward") +
                theme(
                    plot.title = element_text(
                        size = 12,
                        face = "bold",
                        hjust = 0.5
                    ),
                    panel.grid.major = element_blank(),
                    panel.grid.minor = element_blank()
                )
        })
})
