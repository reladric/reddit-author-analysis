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
  introjsUI(),
  # Application title
  fluidRow(column(
    12,
    style = "border:0px;",
    h4("Reddit User Analysis - Feminism and MensRights")
  ))
  ,
  
  fluidRow(column(
    12,
    tabsetPanel(
      selected = "Karma Analysis",
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
      tabPanel(
        "Karma Analysis",
        fluidRow(column(12, wellPanel(
          uiOutput("tutorial"), width = "20%"
        ))),
        fluidRow(fluidRow(
          column(4,
                 wellPanel(
                   introBox(
                     uiOutput("window_size") ,
                     data.step = 1,
                     data.intro = "This drop down selects the window size. All operations such as data selection, mean, median, post count etc is computed in windows of selected size. This value controls all of the plots and data below"
                   )
                 )),
          column(4, wellPanel(
            introBox(
              uiOutput("feature"),
              data.step = 2,
              data.intro = "You can choose to visualize either raw score or score per comment in the window"
            )
          )),
          column(4, wellPanel(
            introBox(
              uiOutput("threshold"),
              data.step = 3,
              data.intro = "You can set minimums on number of posts that a user has to make to be included in below analysis"
            )
          ))
        )),
        fluidRow(column(6, wellPanel(
          introBox(
            plotOutput("groupAverage"),
            data.step = 4,
            data.intro = "This graph plots for each possible window of selected size, the average karma scored by the selected users in both subreddits in current window"
          )
        )), column(6, wellPanel(
          introBox(
            plotOutput("groupCounts"),
            data.step = 5,
            data.intro = "This graph plots for each possible window of selected size, the number of posts made by selected users in both subreddits in current window"
          )
        ))),
        fluidRow(column(6, wellPanel(
          introBox(
            plotOutput("mean_movement"),
            data.step = 6,
            data.intro = "This graph tracks the movement of mean of selected variable in both subreddit. Green dot is the starting point, red the ending and blue for everything in between"
          )
        )), column(6, wellPanel(
          introBox(
            plotOutput("median_movement"),
            data.step = 7,
            data.intro = "This graph tracks the median of mean of selected variable in both subreddit. Green dot is the starting point, red the ending and blue for everything in between"
          )
        ))),
        fluidRow(column(6,  wellPanel(
          introBox(
            uiOutput("centering"),
            data.step = 8,
            data.intro = "For the graphs below, select if you want to use mean or median as the center. The selected center will be the axis in the density plot below"
          )
        )), column(6, wellPanel(
          introBox(
            uiOutput("scale"),
            data.step = 9,
            data.intro = "For the graphs below, select if you want to Scale the selected variable. i.e subtract the center and divide by standard deviation"
          )
        ))),
        fluidRow(column(12, wellPanel(
          introBox(
            uiOutput("monthSelector"),
            data.step = 10,
            data.intro = "Select the month to be used as window end in the graphs below. Changing this slides the window across time scale"
          )
        ))),
        fluidRow(style = "border-top:0px;padding-top:0px", column(6, wellPanel(
          introBox(
            plotOutput("currentMonthHeatMap", width = "100%"),
            data.step = 11,
            data.intro = "With median of selected variable in  Feminism(x-axis) and  Mensright(y-axis) as axis , this plot shows the ratio of users who are in each quantiles combination. Dependant on scaling, independant of center variable"
          )
        )), column(6, wellPanel(
          introBox(
            plotOutput("currentMonthPlot", width = "100%"),
            data.step = 12,
            data.intro = "With the selected center of selected variable in Feminism(x-axis) and  Mensright(y-axis) as axis , this plot shows the denisty of across continous axis. Dependant on scaling and center variable"
          )
        ))),
        introBox(
          fluidRow(
            style = "border:0px;padding:0px",
            column(4, wellPanel(
              plotOutput("firstVectorField", click = "firstVectorclick")
            )),
            column(4, wellPanel(plotOutput(
              "currentMonthHeatMap_1"
            ))),
            column(4, wellPanel(
              plotOutput("firstForwardField", click = "firstForwardclick")
            ))
          ),
          data.step = 13,
          data.intro = "You can click on quantile box in left plot to see the source of users from previous window to current window. For the plots on right, you can see destination quantile box from current window to next window "
        ),
        introBox(
          fluidRow(
            style = "border:0px;padding:0px",
            column(4, wellPanel(
              plotOutput("secondVectorField", click = "secondVectorclick")
            )),
            column(4, wellPanel(plotOutput(
              "currentMonthHeatMap_2"
            ))),
            column(4, wellPanel(
              plotOutput("secondForwardField", click = "secondForwardclick")
            ))
          ),
          data.step = 14,
          data.intro = "Similar to above but looks 2 windows before current window for the past and 2 windows in to the future"
        )
      )
    )
  ))))
