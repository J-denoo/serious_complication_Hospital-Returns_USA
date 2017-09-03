library(shiny)
library(dplyr)
library(DT)

data_url = ('https://data.medicare.gov/api/views/632h-zaca/rows.csv?accessType=DOWNLOAD')

df <- read.csv(data_url)

df = df[df$Score != 'Not Available',]

#df= tbl_df(df)

df = select(df, c( 'Hospital.Name','City', 'State','Measure.Name','Measure.ID', 'Score'))

df = filter(df, Score != 'Not Available')


# Replace the data in a field based on equal to some value
df$Score = as.numeric(as.character(df$Score))
df$Measure.ID = as.character(df$Measure.ID)

summary(df$Measure.ID)
#EDAC_30_AMI
# EDAC_30_HF
# READM_30_AMI
#  READM_30_CABG
#   READM_30_COPD
#   READM_30_HF
#   READM_30_HIP_KNEE
#READM_30_HOSP_WIDE
# READM_30_PN
# READM_30_STK

EDAC_30_AMI = mean(df[df$Measure.ID == 'EDAC_30_AMI' ,]$Score)
EDAC_30_HF = mean(df[df$Measure.ID == 'EDAC_30_HF' ,]$Score)
READM_30_AMI = mean(df[df$Measure.ID == 'READM_30_AMI' ,]$Score)
READM_30_CABG = mean(df[df$Measure.ID == 'READM_30_CABG' ,]$Score)
READM_30_COPD = mean(df[df$Measure.ID == 'READM_30_COPD' ,]$Score)
READM_30_HF = mean(df[df$Measure.ID == 'READM_30_HF' ,]$Score)
READM_30_HIP_KNEE = mean(df[df$Measure.ID == 'READM_30_HIP_KNEE' ,]$Score)
READM_30_HOSP_WIDE = mean(df[df$Measure.ID == 'READM_30_HOSP_WIDE' ,]$Score)
READM_30_PN  = mean(df[df$Measure.ID == 'READM_30_PN' ,]$Score)
READM_30_STK = mean(df[df$Measure.ID == 'READM_30_STK' ,]$Score)
#PSI_7_CVCBI = mean(df[df$Measure.ID == 'PSI_7_CVCBI',]$Score)

df$National.av.Score[df$Measure.ID == 'EDAC_30_AMI'] <- EDAC_30_AMI
df$National.av.Score[df$Measure.ID == 'EDAC_30_HF'] <- EDAC_30_HF
df$National.av.Score[df$Measure.ID == 'READM_30_AMI'] <- READM_30_AMI
df$National.av.Score[df$Measure.ID == 'READM_30_CABG'] <- READM_30_CABG
df$National.av.Score[df$Measure.ID == 'READM_30_COPD'] <- READM_30_COPD
df$National.av.Score[df$Measure.ID == 'READM_30_HF'] <- READM_30_HF
df$National.av.Score[df$Measure.ID == 'READM_30_HIP_KNEE'] <- READM_30_HIP_KNEE
df$National.av.Score[df$Measure.ID == 'READM_30_HOSP_WIDE'] <- READM_30_HOSP_WIDE
df$National.av.Score[df$Measure.ID == 'READM_30_PN'] <- READM_30_PN
df$National.av.Score[df$Measure.ID == 'READM_30_STK'] <- READM_30_STK
df$National.av.Score[df$Measure.ID == 'PSI_6_IAT_PTX'] <- PSI_6_IAT_PTX

df$National.av.Score = round(df$National.av.Score,2)


#df = sort(df, 'State')



ui = fluidPage(
    titlePanel(tags$h4("Healthcare Research and Quality (AHRQ) measures of serious complications in U.S. Hospitals")),
    tags$a(href = 'https://www.medicare.gov/hospitalcompare/Data/Serious-Complications.html', 'Measures of serious complications are drawn from AHRQ Patient Safety Indicators (PSIs)'),
    tags$p(),
    tags$a('Source of data : medicare.gov, data collected between July 2017 to June 2016', href= 'https://data.medicare.gov/api/views/632h-zaca/rows.csv?accessType=DOWNLOAD'),
    tags$p(),tags$p(),  tags$p(),
 #   "app created by: ", tags$em('Justice Denoo ;'),  "email: jdenoo@gmail.com",
 #   tags$p(),  tags$p(),
    tags$p(), tags$p(),
    fluidRow(
        column(2,
               selectInput("State", "Select a State:",  c('All', sort(unique(as.character(df$State))) )) ),
        column(4,
               selectInput("hosp", "Select a Hospitals :", c('All', sort(unique(as.character(df$Hospital.Name)) )                          )) ),
        column(4,
               selectInput("Cond", "Select a Condition:", c('All',sort( unique(as.character(df$'Measure.Name')))
               ) ) ) ),

    fluidRow(column(2 ),
        DT::dataTableOutput("table") ,  style = "font-size : 80%; width : 80%" ), tags$p(),
    fluidRow(column(1 ),

             "app created by :  ", tags$em(' Justice Denoo ,  '),  "  email: jdenoo@gmail.com"

 ))




server = function(input, output, session) {

    observe({
        sel_State <- input$State

        updateSelectInput(session, "hosp", choices = c("All",sort(as.character(df$Hospital.Name[df$State == sel_State ] ) )  )     )    }   )


    output$table <- DT::renderDataTable(DT::datatable ({
        df$Measure.Name = as.character(df$Measure.Name)
        # Filter data based on selections
        data <- df

        if (input$State != "All") {
            data <- data[data$State == input$State,]
            data$State.av.Score = mean(data[data$Measure.Name == input$Cond ,]$Score)   }
        if (input$State != "All" & input$hosp != "All") {
            data <- data[data$Hospital.Name == input$hosp,] }
        if (input$Cond != "All") { data <- data[data$Measure.Name == input$Cond,]   }

        #  data$State.av.Score = round(data$State.av.Score,2)
        data$State.av.Score = NULL
        row.names(data) = NULL
        data$Measure.ID = NULL
        data

    } ) )


}

shinyApp(ui = ui, server = server)

