#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(# Application title
  fluidRow(column(
    12,
    style = "border:0px;",
    h4("Reddit User Analysis - Feminism and MensRights")
  ))
  ,
  
  fluidRow(column(
    12, tabsetPanel(
      tabPanel(
        "User Analysis",
        fluidRow(
          column(
            style = 'padding:0px;',
            1,
            h6("First Subreddit", style = "margin-top:70px;font-weight:bold"),
            h6("Feminism", style = "margin-top:5px;font-weight:bold"),
            h6("First Subreddit", style = "margin-top:90px;font-weight:bold"),
            h6("MensRights", style = "margin-top:5px;font-weight:bold")
          ),
          column(
            3,
            style = "border:0px;",
            fluidRow(
              column(
                6,
                style = 'padding:0px;',
                h6("Majority Feminism"),
                wellPanel(actionButton("q1_authors", "Resample"),
                          uiOutput("q1_count"))
              ),
              column(
                6,
                style = 'padding:0px;',
                h6("Majority Mensrights"),
                wellPanel(actionButton("q2_authors", "Resample"),
                          uiOutput("q2_count"))
              ),
              style = 'padding:0px;'
            ),
            fluidRow(
              column(6, style = 'padding:0px;',
                     wellPanel(
                       actionButton("q3_authors", "Resample "),
                       uiOutput("q3_count")
                     )),
              column(6, style = 'padding:0px;',
                     wellPanel(
                       actionButton("q4_authors", "Resample"),
                       uiOutput("q4_count")
                     )),
              style = 'padding:0px;'
            )
          )
          ,
          column(8, tabsetPanel(
            tabPanel(
              "Overview",
              fluidRow(column(12, tableOutput("stat_table"))),
              fluidRow(
                column(
                  6,
                  strong(h4("Sample Feminism Comment")),
                  htmlOutput("fem_comment"),
                  htmlOutput("fem_comment_stat")
                  
                ),
                column(
                  6,
                  h4("Sample MensRights Comment"),
                  htmlOutput("mr_comment"),
                  htmlOutput("mr_comment_stat")
                )
              ),
              fluidRow(column(
                6, actionButton("resample_fem", "Resample")
              ), column(
                6, actionButton("resample_mr", "Resample")
              ))
            )
            # ,tabPanel("Language Model",wellPanel())
          ))
        ),
        
        fluidRow(style = "border:0px;border-top:2px;border-style:solid", column(
          12, plotOutput("activity_graph", width = "100%")
        ))
      ),
      tabPanel("Karma Analysis", fluidRow(column(
        12, wellPanel(uiOutput("monthSelector"))
      )), fluidRow(column(
        6, wellPanel(plotOutput("currentMonthHeatMap", width = "100%"))
      ),column(
          6, wellPanel(plotOutput("currentMonthPlot", width = "100%"))
      )))
    )
  ))))
