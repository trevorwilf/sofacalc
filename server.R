source('dependencies.R')
source('global.R')

`%then%` <- shiny:::`%OR%`

shinyServer(function(input, output) {
  
  ########################################
  ## Calculator 
  ########################################
  
  output$onetable <- renderTable({
    scoreresult <- as.data.frame(sofascore(FiO2 = input$FiO2, PaO2 = input$PaO2, Urineoutputscore = input$Urineoutput,
                             Mechanicalventilation = input$Mechanicalventilation, Platelets = input$Platelets, 
                             Bilirubin = input$Bilirubin, Glasgowcomascore = input$Glasgowcomascore, 
                             MAP = input$MAP, Vasopressors = input$Vasopressors, Dopamine = input$Dopamine,
                             Dobutamine = input$Dobutamine, Epinephrine = input$Epinephrine,
                             Norepinephrine = input$Norepinephrine, Creatinine = input$Creatinine))
    
    scoreresult
  })
  
  output$barplot1 <- renderPlot({
    score <- seq(from= 0, to=16, by = 1)
    mortality <- c(3.3, 5.8, 3.8, 3.3,
                   7, 10, 4.5, 15.3,
                   22.5, 22.5, 45.8, 40,
                   45.8, 60, 51.5, 82, 
                   87.3)

    scoreresult <- as.data.frame(sofascore(FiO2 = input$FiO2, PaO2 = input$PaO2, Urineoutputscore = input$Urineoutput,
                                           Mechanicalventilation = input$Mechanicalventilation, Platelets = input$Platelets, 
                                           Bilirubin = input$Bilirubin, Glasgowcomascore = input$Glasgowcomascore, 
                                           MAP = input$MAP, Vasopressors = input$Vasopressors, Dopamine = input$Dopamine,
                                           Dobutamine = input$Dobutamine, Epinephrine = input$Epinephrine,
                                           Norepinephrine = input$Norepinephrine, Creatinine = input$Creatinine))
    
    totalscore <- as.numeric(scoreresult[7,2])
    result <- data.frame(mortality, score)
    barcolors <- c()
    
    
    if (totalscore <= 15) {
      for (i in seq_along(score)) {
        if (totalscore == result$score[i]) {
          barcolors <- c(barcolors, "red") 
        }else{
          barcolors <- c(barcolors, "blue")
        }
      }
    } else {
      barcolors <- c("blue", "blue", "blue", "blue", 
                     "blue", "blue", "blue", "blue", 
                     "blue", "blue", "blue", "blue", 
                     "blue", "blue", "blue", "blue", 
                     "red")
    }
    
    #tmp <- as.data.frame(t(result[, -2]))
    #colnames(tmp) <- result$score
    
    par(mar=c(6, 6, 5, 4) + 0.1, bg=NA) 
    barplot(result$mortality, col=barcolors, width=1, 
            axes=FALSE, xlab="", ylab="", las=2,
            names.arg = score)
    axis(2, ylim=c(0,100),col="midnightblue",las=2, line = 1)  ## las=1 makes horizontal labels
    mtext("Hospital Mortality (%)", side=2, line=3.5, col="midnightblue", cex=1.5)
    mtext("SOFA Score", side=1, line=2.5, col="midnightblue", cex=1.5)

  })
  
  ########################################
  ## regression 
  ########################################
  
  output$SofaFile <- renderUI({
    fileInput('SofaFile1', 'Upload Sofa Data:',
              accept=c('sheetName', 'header'), multiple=FALSE)
    
  })
  
  
  sofadata <- reactive({
    
    #     validate(
    #       need(output$SofaFile != "", "Please select or upload a data set")  %then%
    #       need(is.null(output$SofaFile), "Please select or upload a data set")  %then% 
    #       need(is.null(output$SofaFile), "Please select or upload a data set")  %then% 
    #     )
    
    infile <- input$SofaFile1
    sofadata <- NULL
    
    if (!(is.null(infile))) {
      babydata <- as.data.frame(read.xlsx2(infile$datapath, sheetIndex = 1, header = TRUE, stringsAsFactors = FALSE))
    }

    if (input$loadsample == 0) {
      sofadata <- NULL
    }
    
    if (input$loadsample == 1) {
      sofadata <- as.data.frame(read.xlsx2("sofanumbers.xlsx", sheetName = "improving", header = TRUE, stringsAsFactors = FALSE))
    }
    
    if (input$loadsample == 2) {
      sofadata <- as.data.frame(read.xlsx2("sofanumbers.xlsx", sheetName = "worse", header = TRUE, stringsAsFactors = FALSE))
    }
    
    if (input$loadsample == 3) {
      sofadata <- as.data.frame(read.xlsx2("sofanumbers.xlsx", sheetName = "static", header = TRUE, stringsAsFactors = FALSE))
    }
    
    validate(
         need(!is.null(sofadata), "Please select or upload a data set")
    )
    
#     sofadata$Mechanicalventilation <- sapply(as.data.frame(sofadata$Mechanicalventilation), as.logical)
#     sofadata$Vasopressors <- sapply(as.data.frame(sofadata$Vasopressors), as.logical)
#     sofadata$FiO2 <- sapply(as.data.frame(sofadata$FiO2), as.numeric)
#     sofadata$PaO2 <- sapply(as.data.frame(sofadata$PaO2), as.numeric)
#     sofadata$Urineoutputscore <- sapply(as.data.frame(sofadata$Urineoutputscore), as.numeric)
#     sofadata$Platelets <- sapply(as.data.frame(sofadata$Platelets), as.numeric)
#     sofadata$Bilirubin <- sapply(as.data.frame(sofadata$Bilirubin), as.numeric)
#     sofadata$Glasgowcomascore <- sapply(as.data.frame(sofadata$Glasgowcomascore), as.numeric)
#     sofadata$MAP <- sapply(as.data.frame(sofadata$MAP), as.numeric)
#     sofadata$Creatinine <- sapply(as.data.frame(sofadata$Creatinine), as.numeric)
    
    sofadata$Mechanicalventilation <- as.logical(sofadata$Mechanicalventilation)
    sofadata$Vasopressors <- as.logical(sofadata$Vasopressors)
    sofadata$FiO2 <- as.numeric(sofadata$FiO2)
    sofadata$PaO2 <- as.numeric(sofadata$PaO2)
    sofadata$Urineoutputscore <- as.numeric(sofadata$Urineoutputscore)
    sofadata$Platelets <- as.numeric(sofadata$Platelets)
    sofadata$Bilirubin <- as.numeric(sofadata$Bilirubin)
    sofadata$Glasgowcomascore <- as.numeric(sofadata$Glasgowcomascore)
    sofadata$MAP <- as.numeric(sofadata$MAP)
    sofadata$Creatinine <- as.numeric(sofadata$Creatinine)
    
    sofadata <- sofadata[!is.na(sofadata$date),]
    sofadata$TIMESTAMP <- as.POSIXct(as.Date(as.numeric(sofadata$date), origin="1899-12-30"))
    #sofadata <- subset(sofadata, subset = -c("date","time"))
    
    
    if (is.null(sofadata))
      return(NULL)
    
    return(sofadata)
    
  })
  
  output$sofaplot <- renderPlot({
    
#     validate(
#       need(output$SofaFile != "", "Please select or upload a data set")  %then%
#       need(is.null(output$SofaFile), "Please select or upload a data set")  %then% 
#       need(is.null(output$SofaFile), "Please select or upload a data set")  %then% 
#     )
    
    data1 <- as.data.frame(sofadata())
    
    ##################################
    ## for testing
    #################################
#     data1 <- as.data.frame(read.xlsx2("sofanumbers.xlsx", sheetName = "worse", header = TRUE, stringsAsFactors = FALSE)) 
#     data1$Mechanicalventilation <- sapply(as.data.frame(data1$Mechanicalventilation), as.logical)
#     data1$Vasopressors <- sapply(as.data.frame(data1$Vasopressors), as.logical)
#     data1$FiO2 <- sapply(as.data.frame(data1$FiO2), as.numeric)
#     data1$PaO2 <- sapply(as.data.frame(data1$PaO2), as.numeric)
#     data1$Urineoutputscore <- sapply(as.data.frame(data1$Urineoutputscore), as.numeric)
#     data1$Platelets <- sapply(as.data.frame(data1$Platelets), as.numeric)
#     data1$Bilirubin <- sapply(as.data.frame(data1$Bilirubin), as.numeric)
#     data1$Glasgowcomascore <- sapply(as.data.frame(data1$Glasgowcomascore), as.numeric)
#     data1$MAP <- sapply(as.data.frame(data1$MAP), as.numeric)
#     data1$Creatinine <- sapply(as.data.frame(data1$Creatinine), as.numeric)
#     #data1 <- filter(data1, !grepl('^[[:space:]]*$', date)) 
#     data1 <- data1[!is.na(data1$date),]
#     data1$TIMESTAMP <- as.POSIXct(as.Date(as.numeric(data1$date), origin="1899-12-30"))
#     source("scoring.R")
    #################################
    #print(data1)
    
    for (i in 1:nrow(data1)) {
      tmp <- data1[i,]
      scoreresult <- as.data.frame(sofascore(FiO2 = tmp$FiO2, PaO2 = tmp$PaO2, Urineoutputscore = tmp$Urineoutput,
                                             Mechanicalventilation = tmp$Mechanicalventilation, Platelets = tmp$Platelets, 
                                             Bilirubin = tmp$Bilirubin, Glasgowcomascore = tmp$Glasgowcomascore, 
                                             MAP = tmp$MAP, Vasopressors = tmp$Vasopressors, Dopamine = tmp$Dopamine,
                                             Dobutamine = tmp$Dobutamine, Epinephrine = tmp$Epinephrine,
                                             Norepinephrine = tmp$Norepinephrine, Creatinine = tmp$Creatinine))
      
      data1$score[i] <- as.numeric(scoreresult$score[7])
    }
    
    
    plotdata <- subset(data1, select = c("TIMESTAMP", "score"))
    
    ## plot section
    par(mar=c(10, 6, 5, 12) + 0.1, bg=NA) 
    plot(plotdata, axes=FALSE, xlab="", ylab="", 
         type="b",lty=1, col="darkred", pch=23,
         ylim=c(0,24))
    scorefit <- lm(score~TIMESTAMP, data = plotdata)
    abline(scorefit)
    axis(2, ylim=c(0,24),col="darkred",las=1, line = 1)  ## las=1 makes horizontal labels
    axis(1, col="black", las=2, line = 1, labels =  format(plotdata$TIMESTAMP, "%Y-%m-%d %H:%M"), at= plotdata$TIMESTAMP, cex = .75)  ## las=1 makes horizontal labels
    mtext("Sofa Score", side=2, line=4, col="darkred", cex=2)
    
  })
  
  output$sofadataset <- DT::renderDataTable(
    DT::datatable(
      subset(sofadata(), select = c("TIMESTAMP", "FiO2", "PaO2", "Urineoutputscore",
                                    "Mechanicalventilation", "Platelets", 
                                    "Bilirubin", "Glasgowcomascore", 
                                    "MAP", "Vasopressors", "Dopamine",
                                    "Dobutamine", "Epinephrine",
                                    "Norepinephrine", "Creatinine"))
      , 
      options = list(pageLength = 25)
      
      )
  )
  
})

