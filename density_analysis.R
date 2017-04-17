library(shiny)
library(ggplot2)
library(scales)
library(data.table)
source("global.R")
plot_data <-
    plotdata_for_month(
        x = 100, complete_data = score_data, feature = "avg",scale = TRUE, window_size = 5
    )

hline <- data.frame(yint = unique(plot_data$y_negative_point),lt = 'Negative Score Axis') 
vline <- data.frame(xint = unique(plot_data$x_negative_point),lt = 'Negative Score Axis') 
# stat_density2d(aes(fill = ..density..), geom = "polygon") +
# scale_alpha_continuous(limits = c(0, .5), breaks = seq(0, .5, by = 0.1)) +
plot <-
    ggplot(plot_data, aes(x = x, y = y)) +
    stat_density2d(aes(alpha = ..level..), geom = "polygon") +
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
    scale_fill_gradient(low="blue", high="green")+
    geom_hline(data = hline,aes(yintercept=yint,linetype = lt),color = "red",size=1) +
    geom_vline(data = vline,aes(xintercept=xint,linetype = lt),color = "red",size=1) +
    scale_colour_discrete(guide = "none") + 
    scale_linetype_manual(name = 'Legend',values = 1,guide = "legend")+
    #             geom_vline(
    #                 xintercept = unique(plot_data$y_negative_point),
    #                 col =
    #                     "red", show_guide=T
    #             ) +
    #             geom_hline(
    #                 yintercept = unique(plot_data$x_negative_point),
    #                 col =
    #                     "red", show_guide=T
    #             ) +
    xlab(paste("Feminism", values$selected_feature, sep = " ")) +
    ylab(paste("MensRights", values$selected_feature, sep = " "))
plot