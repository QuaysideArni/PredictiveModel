library(shiny)
library(rsconnect)
load("model.Rdata")
ui <- fluidPage(
  titlePanel(
    h1("please input information to predict the probability of brain metastasis")),
  fluidRow(
    column(3,
           h4("risk probability:"),
           selectInput(inputId = "age",label = "age",choices = c("No", "Yes")),
           selectInput(inputId = "sex",label = "gender",choices = c("No", "Yes")),
           selectInput(inputId = "marital",label = "marital status",choices = c("No", "Yes")),
           selectInput(inputId = "surgery",label = "surgery history",choices = c("No", "Yes")),
           selectInput(inputId = "Radiation",label = "Radiation history",choices = c("No", "Yes")),
           selectInput(inputId = "Chemotherapy",label = "Chemotherapy history",choices = c("No", "Yes")),
           br(),
           numericInput(inputId = "Months.from.diagnosis.to.treatment",label = "Months.from.diagnosis.to.treatment",value = 0),
           selectInput(inputId = "Histological.type.adenocarcinoma", label = "Histological.type.adenocarcinoma",choices = c("No", "Yes")),
           selectInput(inputId = "Histological.type.Squamous_cell_carcinoma", label = "Histological.type.Squamous cell carcinoma",choices = c("No", "Yes")),
           selectInput(inputId = "Histological.type.other", label = "Histological.type.other",choices = c("No", "Yes")),
           selectInput(inputId = "Tumor.laterality.left", label = "Tumor.laterality.left",choices = c("No", "Yes")),
           selectInput(inputId = "Tumor.laterality.right", label = "Tumor.laterality.right",choices = c("No", "Yes")),
           selectInput(inputId = "Tumor.laterality.Bilateral", label = "Tumor.laterality.Bilateral",choices = c("No", "Yes"))
    ),
    column(4, offset = 1,
           selectInput(inputId = "site.Main_bronchus", label = "site.Main.bronchus",choices = c("No", "Yes")),
           selectInput(inputId = "site.Upper_lobe", label = "site.Upper lobe",choices = c("No", "Yes")),
           selectInput(inputId = "site.Middle_lobe", label = "site.Middle lobe",choices = c("No", "Yes")),
           selectInput(inputId = "site.Lower_lobe", label = "site.Lower lobe",choices = c("No", "Yes")),
           selectInput(inputId = "site.Overlapping_lesion", label = "site.Overlapping lesion",choices = c("No", "Yes")),
           selectInput(inputId = "T1a", label = "T1 stage",choices = c("No", "Yes")),
           selectInput(inputId = "T1b", label = "T1 stage",choices = c("No", "Yes")),
           selectInput(inputId = "T1c", label = "T1 stage",choices = c("No", "Yes")),
           selectInput(inputId = "T2a", label = "T2 stage",choices = c("No", "Yes")),
           selectInput(inputId = "T2b", label = "T2 stage",choices = c("No", "Yes")),
           selectInput(inputId = "T3", label = "T3 stage",choices = c("No", "Yes")),
           selectInput(inputId = "T4", label = "T4 stage",choices = c("No", "Yes")),
           numericInput(inputId = "Tumor.Size",label = "Tumor.Size",value = 0)
    ),
    column(4,
           selectInput(inputId = "N0", label = "N0 stage",choices = c("No", "Yes")),
           selectInput(inputId = "N1", label = "N1 stage",choices = c("No", "Yes")),
           selectInput(inputId = "N2", label = "N2 stage",choices = c("No", "Yes")),
           selectInput(inputId = "N3", label = "N3 stage",choices = c("No", "Yes")),
           selectInput(inputId = "Reg.LN.Sur",label = "Reg.LN.Surgery",choices = c("No", "Yes")),
           selectInput(inputId = "Regional.nodes.negative",label = "Regional.nodes.negative",choices = c("No", "Yes")),
           selectInput(inputId = "Regional.nodes.positive",label = "Regional.nodes.positive",choices = c("No", "Yes")),
           selectInput(inputId = "Regional.nodes.unknown",label = "Regional.nodes.unknown",choices = c("No", "Yes")),
           selectInput(inputId = "Mets.at.Distant.LN",label = "Mets.at.Distant.LN",choices = c("No", "Yes")),
           selectInput(inputId = "Mets.at.bone",label = "Mets.at.bone",choices = c("No", "Yes")),
           selectInput(inputId = "Mets.at.liver",label = "Mets.at.liver",choices = c("No", "Yes")),
           selectInput(inputId = "Mets.at.lung",label = "Mets.at.lung",choices = c("No", "Yes")),
           selectInput(inputId = "Mets.at.Other",label = "Mets.at.Other",choices = c("No", "Yes"))
           ),
           mainPanel(
             br(),
             h1('risk prediction:'),
             br(),
             h2(strong(textOutput('answer'), style = "color:blue")),
             br(),
             br()
      )
   )
)


server <- function(input, output) {
  output$answer <- renderPrint({
    data.test <- data.frame(age=input$age,sex=input$sex,marital=input$marital,
                            site.Main_bronchus=input$site.Main_bronchus,site.Upper_lobe=input$site.Upper_lobe,site.Middle_lobe=input$site.Middle_lobe,site.Lower_lobe=input$site.Lower_lobe,site.Overlapping_lesion=input$site.Overlapping_lesion,
                            Histological.type.adenocarcinoma=input$Histological.type.adenocarcinoma,Histological.type.Squamous_cell_carcinoma=input$Histological.type.Squamous_cell_carcinoma,Histological.type.other=input$Histological.type.other,
                            Tumor.laterality.left=input$Tumor.laterality.left,Tumor.laterality.right=input$Tumor.laterality.right,Tumor.laterality.Bilateral=input$Tumor.laterality.Bilateral,
                            T1a=input$T1a,T1b=input$T1b,T1c=input$T1c,T2a=input$T2a,T2b=input$T2b,T3=input$T3,T4=input$T4,
                            N0=input$N0,N1=input$N1,N2=input$N2,N3=input$N3,
                            surgery=input$surgery,Radiation=input$Radiation,Chemotherapy=input$Chemotherapy,
                            Reg.LN.Sur=input$Reg.LN.Sur,Regional.nodes.negative=input$Regional.nodes.negative,Regional.nodes.positive=input$Regional.nodes.positive,Regional.nodes.unknown=input$Regional.nodes.unknown,
                            Mets.at.Distant.LN=input$Mets.at.Distant.LN,Mets.at.bone=input$Mets.at.bone,Mets.at.liver=input$Mets.at.liver,Mets.at.lung=input$Mets.at.lung,Mets.at.Other=input$Mets.at.Other,
                            Months.from.diagnosis.to.treatment=input$Months.from.diagnosis.to.treatment,
                            Tumor.Size=input$Tumor.Size)
    probability <- predict(GBM_model,newdata = data.test, type = 'prob')
    prob <- probability$Yes
    temp <- as.character()
    ifelse(prob>=0.5,
           temp <- sprintf('high risk!'),
           temp <- sprintf('low risk,'))
    temp1 <-paste0(sprintf('probability:%s',round(prob*100,2)),'%')  
    return(cat(temp,'\n',temp1))
  })
}
shinyApp(ui = ui, server = server)


