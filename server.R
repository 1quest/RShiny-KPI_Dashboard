library(R.utils)
library(ggplot2)
library(ggtext)
library(RColorBrewer)


server <- function(input, output, session) {

  #Server: uncomment /srv/shiny-server/pcc
  mainpath =  "./data"

  #Data setup----
  data_hispo = read.csv(file.path(mainpath,"pcc_hispo.csv"), sep = ";")

  # Transform characters into actual dates in the r-lang
  data_odh = read.csv(file.path(mainpath,"pcc_odh.csv"), sep = ",")

  #Remove the time-piece of the DELIVERYTIME
  data_odh$DELIVERYTIME <- substr(data_odh$DELIVERYTIME, 1,10)
  data_odh <- filter(data_odh, STATUS <= 20)

  #Populate the column for Country_EU from the RegionTranslate csv
  #based on the odh.TOURNO <-> RegionTranslate.Route column
  regionTranslate <- read.csv(file.path(mainpath,"RegionTranslate.csv"), sep = ";")

  #Translate the TourNos into countries
  #Server: remove the "ï.." from Route
  index <- match(data_odh$TOURNO, regionTranslate$Route)
  #Get a list of the translated countries
  countries <- regionTranslate$Country[index]

  #Replace all NAs with Internal,because it means match couldn't find the Country
  countries[is.na(countries)] = "Internal"
  #Set all countries with Region EU to DD
  EU_region_index <- regionTranslate$Region[index] == "EU"
  countries[EU_region_index] = "DD"

  data_odh$Country_EU <- countries

  #Top bar setup----
  icon_file <- file.path(mainpath,"icon.jpg")

  output$tf_icon <- renderImage({
    list(src = icon_file,
         width =200,
         height=100)
  },
  deleteFile=FALSE)

  output$pcc_monitoring <- renderText({
    HTML(paste( "<b><font face='Arial' size='5ex' color=#000000 >", "PCC Monitoring", "</font>", "</b><br/>"))
  })

  output$page_descr <- renderText({
    HTML(paste( "<b><font face='Arial' size='2ex' color=#000000 >", "Page description:", "</font>", "</b><br/>",
                "<font face='Arial' text-align:center size='2ex' color=#000000>", "- Data Source: PCC-HISPO (real-time delivery operations)", "</font>"), "<br/>",
         "<font face='Arial' text-align:center size='2ex' color=#000000>", "- First data refresh 08:00, update every 10 min.", "</font>", "<br/>",
         "<font face='Arial' text-align:center size='2ex' color=#000000>", "- Screen refresh: every X minutes", "</font>")
  })

  output$last_update <- renderUI({
    HTML(paste( "<b><font face='Arial' style=text-align:center size='4ex' color=#000000 >", "Data refreshed:", "</font>", "</b><br/>",
                "<font face='Arial' style=text-align:center size='4ex' color=#000000>", lastModified(file.path(mainpath,"pcc_odh.csv")), "</font>")
    )
  })

  #KPI setups & Pie chart----
  #STATUS: OK
  # This KPI COUNTS the number of orders included after filtered
  # Filter is HISPOTYPE = 35
  output$orders_total <- renderValueBox({
    order_today <- filter(data_hispo, HISPOTYPE == 35)
    valueBox(
      value = tags$p(nrow(order_today),style="font-size:50px"),
      subtitle = tags$p("Total # Order Lines", style="font-size:30px"),
      color = "green",
      icon = icon("calculator")
    )
  })

  # ASKQ: Ask what the point of this KPI is
  order_today <- filter(data_odh, as.Date(data_odh$DELIVERYTIME) == as.Date(today()), STATUS <= 20)
  output$order_lines_total <- renderValueBox({
    valueBox(
      value = tags$p(sum(order_today[, 'NOOFLINES']), style="font-size:50px"),
      subtitle = tags$p("Order Lines", style="font-s
ize:30px"),
      color = "blue",
      icon = icon("calculator")
    )
  })

  #TODO: This needs to be filtered on status 20 and is today(done)
  #This KPI shows the ORDERNO of the Order in the last 1 day
  # AND where status is less than or equal to 20
  # First find the correct order:

  order_latest <- filter(data_odh, as.Date(data_odh$DELIVERYTIME) == as.Date(today()), STATUS <= 20)
  output$order_lines_today <- renderValueBox({
    valueBox(
      value = tags$p(nrow(order_latest),style="font-size:50px"),
      subtitle = tags$p("# Order(s)", style="font-size:30px"),
      color = "green"
    )
  })

  #Create color pallette with tons of entries:
  nb.cols <- 18
  mycolors <- colorRampPalette(brewer.pal(8, "Set2"))(nb.cols)

  #PIE CHART showing the country distribution
  output$Pie_country <- renderPlot({
    order_today <- filter(data_odh, as.Date(data_odh$DELIVERYTIME) == as.Date(as.Date("24feb2022","%d%b%Y")), STATUS <= 20)

    #Avoid plotting error if there aren't any rows in the data
    if (nrow(order_today) > 0) {
    odh_aggregate <- aggregate(order_today$NOOFLINES, by=list(order_today$Country_EU), FUN=sum)
    odh_aggregate <- odh_aggregate[order(-odh_aggregate$x),]
    #Add the total quantities next to the names of the countries
    odh_aggregate$Group.1 <- paste(odh_aggregate$Group.1,"(",odh_aggregate$x,")")
    df <- data.frame(
      Country = odh_aggregate$Group.1,
      value = odh_aggregate$x
    )
    #Set the sorting to be based on the list already sorted on number of Orders
    df$Country <- factor(df$Country, levels = as.character(df$Country))

    #Calculate percentage share per country, remove those lower than 10%
    share <- round(100*df$value/sum(df$value))
    #share[share < 10] <- ""
    #ACTIVATE THIS LATER: This yields a warning
    #share[share != ""] <- paste(as.character(share), "%")

    #Set the aesthetics of the pie chart
    # TODO: make the text nicer and bold for example for Country
    bp<- ggplot(df, aes(x="", y=value, fill=Country))+geom_bar(width = 1, stat = "identity")
    pie <- bp + coord_polar("y", start=0) +
      geom_bar(width = 1, size = 1, color = "white", stat = "identity") +
      geom_text(aes(label = value),
                position = position_stack(vjust = 0.5)) +
      labs(x="",y="") +
      theme_classic() +
      theme(axis.line = element_blank(),
            axis.text = element_blank(),
            axis.ticks = element_blank(),
            panel.border = element_blank(),
            legend.title = element_text(face = "bold", color = "#666666"),
            panel.background = element_rect(fill = "#AFAFAF", color = "#AFAFAF", size=0),
            plot.background = element_rect(fill = "#AFAFAF", color = "#AFAFAF", size=0),
            plot.margin=grid::unit(c(0,0,0,0),"mm"),
            plot.title =  element_text(hjust = -0.05, face="bold",color = "#d9d9d9",vjust = -0.95)) +
      scale_fill_manual(values = mycolors)  +
      labs(title="Order lines by Country")

    execOnResize=T
    }
    else{
      #plot a blank chart that says no data
      df <- data.frame(
        x = c(1.5),
        y = c(1.5),
        text = c("No data"))
      pie <- ggplot(df, aes(x, y)) +
        geom_text(aes(label = text)) +
        theme_void() +
        theme(
          panel.background = element_rect(fill = "#AFAFAF",
                                          colour = "#AFAFAF",
                                          size = 0.5, linetype = "solid"))
    }
    pie


  },height=297)

  #Bar chart----
  #BAR CHART showing picked number of orders released per week
  #NOTE:the date is based on DELIVERYTIME
  output$Chart_picking <- renderPlot({
    bg = "transparent"
    odh_week_aggr <- filter(data_odh, as.Date(data_odh$DELIVERYTIME) > as.Date("21feb2022","%d%b%Y"), STATUS <= 20)
    odh_week_aggr$DELIVERYTIME <- substr(odh_week_aggr$DELIVERYTIME,1,10)
    odh_week_aggr$weekno <-  week(odh_week_aggr$DELIVERYTIME)
    odh_week_aggr_test <- aggregate(odh_week_aggr$NOOFLINES, by=list(odh_week_aggr$DELIVERYTIME, odh_week_aggr$Country_EU), FUN=sum)

    odh_week_aggr_date <- aggregate(odh_week_aggr$NOOFLINES, by=list(odh_week_aggr$DELIVERYTIME), FUN=sum)

    df <- data.frame(
      Week = as.character(odh_week_aggr_test$Group.1),
      value = odh_week_aggr_test$x,
      country = odh_week_aggr_test$Group.2
    )

    country_label = odh_week_aggr_test$Group.2
    country_label = country_label[!duplicated(country_label)]
    #Sort country labels based on largest quantity
    y_country = order(-odh_week_aggr_test$x)
    week_label = df$Week
    week_label = week_label[y_country]
    country_label = country_label[y_country]

    df_sum<-aggregate(df$value,by=list(df$Week),FUN=sum)
    df_sum <- data.frame(
      Week_sum = df_sum$Group.1,
      value_sum = df_sum$x
    )

    #Set the aesthetics of the bar chart

    label_test <- paste(df$value, df$country, sep="-")

    bp <- ggplot() +
          geom_bar(data=df, aes(fill=country,
                                y=value,
                                x=Week),
                                position="dodge",
                                stat="identity") +

          geom_richtext(aes(x = df$Week,
                        y=df$value + pmin(0.5*df$value, 50 ),
                        label = label_test
                        ),
                        angle=60,
                        group = df$country,
                        position = position_dodge2(width = 0.9),
                        label.size = 0) +

          scale_fill_manual(values = mycolors)  +
          xlab("Date")+
          ylab("# Order lines")+
      theme(plot.background = element_rect(fill = "#AFAFAF", color = "#AFAFAF"),
            axis.title.x = element_text(colour = "#d9d9d9",size=10),
            axis.text = element_text(colour = "#d9d9d9",size=20),
            axis.title.y = element_text(colour = "#d9d9d9",size=40),
            legend.direction = "horizontal",
            legend.position = "top",
            legend.box = "horizontal" ) +
          labs(fill = "Countries") +
          guides(fill = "none")#guide_legend(nrow = 1, byrow = TRUE))

    bp2 <-  bp + geom_line(data = df_sum, aes(group=1, y=value_sum, x=Week_sum))  + geom_label(aes(x = df_sum$Week_sum, y=df_sum$value_sum,label = df_sum$value_sum))


    bp2
  })

}
