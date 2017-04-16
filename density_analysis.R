library(shiny)
library(ggplot2)
library(scales)
library(data.table)
source("global.R")
values = list(as.numeric(100),"avg",TRUE,"median",10)
names(values) <-
    c("selected_month","selected_feature","scale","center","selected_window")
score_data <- compute_data()[[4]]
plot_data <-
    plotdata_for_month(
        as.numeric(values$selected_month),
        score_data,
        values$selected_feature,
        values$scale,
        values$center,
        as.numeric(values$selected_window)
        
    )
# stat_density2d(aes(fill = ..density..), geom = "polygon") +
# scale_alpha_continuous(limits = c(0, .5), breaks = seq(0, .5, by = 0.1)) +
plot <-
    ggplot(plot_data, aes(x = x, y = y)) +
    stat_density2d(aes(color = ..level..)) +
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
    ) +      theme(plot.title = element_text(size = 15,
                                             face = "bold",
                                             hjust = 0.5)) +
    geom_vline(xintercept = unique(plot_data$vline)) +
    geom_hline(yintercept = unique(plot_data$hline)) +
    geom_vline(xintercept = unique(plot_data$y_negative_point), col =
                   "red") +
    geom_hline(yintercept = unique(plot_data$x_negative_point), col =
                   "red") +
    xlab(paste("Feminism", values$feature, sep = " ")) +
    ylab(paste("MensRights", values$feature, sep = " ")) +

plot