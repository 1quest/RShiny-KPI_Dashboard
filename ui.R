library(shiny)
library(shinydashboard)
library(r2d3)
library(dplyr)
library(lubridate)
library(ggplot2)
library(data.table)
library(R.utils)


# PCC Dashboard ----
#TODO ADD MORE INFO fillPage -> DashboardPage for colors on ValueBox
ui <- dashboardPage(
  #theme = "black",
  #padding = 0,
  # Header definition, disabled----
  dashboardHeader(disable = TRUE),

  #1. Sidebar content, collapsed----
  #TODO: enter comment on what could/should be in the side-bar in terms of functionality / user-story
  #TODO: current Power BI filtering is done by clicking in the dashboard, suggestion is putting that in sidebar
  dashboardSidebar(
    collapsed = TRUE,

    sidebarMenu(),
    #Adjust the height oft he sidebar button as well
    tags$style(".left-side, .main-sidebar {padding-top: 20px}")

  ),
  #2. Body UI row definitions----
  dashboardBody(
    #Set background of entire page as well as themes
    tags$head(tags$style(
      HTML(".small-box {
                                height: 30vh;
                                display: flex;
                                align-items: center;
                                justify-content: center;
                                margin:auto;
                                }
           .skin-blue .main-sidebar {
                                background-color: #AFAFAF;
                                vertical-align: middle;
           }
           .shiny-plot-output . shiny-bound-output {
                                background-color: #AFAFAF;
                                vertical-align: middle;
                                }
           .content-wrapper, .right-side {
                                background-color: #AFAFAF;
           }
           .inner {
                                text-align: center;
                                vertical-align: middle;
           }
           .fa{
                                font-size: 120px;
           }
           ")
    )),
    tags$style(".content-wrapper {padding-top:0px}"),
    fillPage(
      title = "PCC Dashboard",
      ##value = "page1",
      #TODO: comment on the different KPI-row

      #TODO: comment on each KPI and what it shows
      #3. Header row ----
      fluidRow(
        column(
          2,
          style = "height:10vh;background-color:#AFAFAF;margin-top:-15px",
          align = "center",
          plotOutput("tf_icon", width = '100%', height = "100%")
        ),
        column(
          3,
          align="center",
          div(style = "height:10vh;background-color:#AFAFAF;margin-top:-15px;text-align:center;
                                 padding:25px", htmlOutput("pcc_monitoring", width='100%'),)
        ),
        column(
          4,
          style = "height:10vh;background-color:#AFAFAF;margin-top:-15px;padding:20px",
          align = "left",
          htmlOutput("page_descr", width = '100%')
        ),
        column(
          3,
          style = "height:10vh;background-color:#AFAFAF;margin-top:-15px;padding:20px;text-align:center",
          align = "right",
          htmlOutput("last_update", width = '100%')
        )
      ),
      #4. Pie chart row and KPIs----
      fluidRow(
        fluidRow(
          column(9, style = "background-color:#cccccc; font-weight:bold; border-radius: 20px; border: 10px solid #AFAFAF; font-size: 20px",  align = "center" ,"To be picked Today"),
          column(3, style = "background-color:#cccccc; font-weight:bold; border-radius: 20px; border: 10px solid #AFAFAF; font-size: 20px",  align = "center" ,"Picked today")
      ),
      fluidRow(
        column(3 ,style='background-color:#AFAFAF;border: 13px solid #AFAFAF; padding:-15px; margin=-15px', plotOutput("Pie_country", height='30vh',width = '100%')),
        column(3, style='padding:-15px; margin=-15px', valueBoxOutput("order_lines_total", width = '100%')),
        column(3, style='padding:-15px; margin=-15px', valueBoxOutput("order_lines_today", width = '100%')),
        column(3, style='padding:-15px; margin=-15px', valueBoxOutput("orders_total", width = '100%'))
      )
      ),
      #5. Bar chart row----
      fluidRow(
        column(12, style = "background-color:#cccccc; margin-top:-25px; font-weight:bold ; font-size: 20px",  align = "center" ,"Released for picking")
      ),
      fluidRow(column(width = 12, style='margin-top:-0px;', plotOutput("Chart_picking", height= '52vh')))
    )
  )
)
