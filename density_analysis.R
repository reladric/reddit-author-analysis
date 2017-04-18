library(shiny)
library(ggplot2)
library(scales)
library(data.table)
source("global.R")
plot_data <-
    plotdata_for_month(
        x = 100, complete_data = score_data, feature = "avg",scale = TRUE, window_size = 5
    )


plot <-
    ggplot(plot_data, aes(x = x, y = y)) +
    stat_density2d(aes(alpha = ..level..,fill=..level..), geom = "polygon") +
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
    ))+
    geom_vline(xintercept = unique(plot_data$vline)) +
    geom_hline(yintercept = unique(plot_data$hline)) +
    geom_hline(
        data = hline,aes(yintercept = yint,linetype = lt),color = "red",size = 1
    ) +
    geom_vline(
        data = vline,aes(xintercept = xint,linetype = lt),color = "red",size = 1
    ) +
    scale_linetype_manual(name = 'Legend',values = 1,guide = "legend") +
    xlab(paste("Feminism", values$selected_feature, sep = " ")) +
    ylab(paste("MensRights", values$selected_feature, sep = " ")) 
plot