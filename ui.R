source('dependencies.R')
source('global.R')

shinyUI(
  fluidPage(
      tags$head(
        tags$style(HTML(".shiny-output-error-validation {color: green;}")),
        tags$style(type="text/css", "select { max-width: 140px; }"),
        tags$style(type="text/css", "input { max-width: 80px; }"),
        tags$style(type="text/css", ".col-fixed-280 {
          max-width:280;
          background:red;
          position:fixed;
          height:100%;
          }")),    
    
     titlePanel("Sequential Organ Failure Assessment (SOFA"),
     
     fluidRow( 
       #position = "left",
       column(width = 2,
              wellPanel(
      
        conditionalPanel(condition="input.conditionedPanels==1",
                       h3("Respiration"),
                       div(style="display:inline-block", numericInput("FiO2", 
                                    label = h5("FiO2"), 
                                    value = 21),
                           HTML(" %"),
                           br(),
                       numericInput("PaO2", 
                                    label = h5("PaO2"), 
                                    value = 85),
                          HTML(" mmHg"),
                       class="form-inline"),
                       checkboxInput("Mechanicalventilation", label = "Mechanical ventilation", value = FALSE),
                       #br(),
                       h3("Coagulation"),
                       div(style="display:inline-block", numericInput("Platelets", 
                                    label = h5("Platelets"), 
                                    value = 180),
                           HTML(" (x10^3)/(mm^3)"), 
                           class="form-inline"),
                       #br(),
                       h3("Liver"),
                       div(style="display:inline-block", numericInput("Bilirubin", 
                                    label = h5("Bilirubin"), 
                                    value = 1), 
                           HTML(" mg/dL"),
                           class="form-inline"),
                       #br(),
                       h3("Neurological"),
                       div(style="display:inline-block", numericInput("Glasgowcomascore", 
                                    label = h5("Glasgow coma score"), 
                                    value = 15)),
                       #br(),
                       h3("Cardiovascular"),
                       div(style="display:inline-block", numericInput("MAP", 
                                    label = h5("Mean Arterial Pressure"), 
                                    value = 70), 
                           HTML(" mmHg"),
                           class="form-inline"),
                       checkboxInput("Vasopressors", label = "Vasopressors", value = FALSE),
                       conditionalPanel(
                         condition="input.Vasopressors",
                         div(style="display:inline-block", numericInput("Dopamine", 
                                                                        label = h5("Dopamine"), 
                                                                        value = 0), 
                             HTML("mcg/kg/min"), 
                             class="form-inline"),
                         div(style="display:inline-block", numericInput("Dobutamine", 
                                                                        label = h5("Dobutamine"), 
                                                                        value = 0), 
                             HTML("mcg/kg/min"), 
                             class="form-inline"),
                         div(style="display:inline-block", numericInput("Epinephrine", 
                                                                        label = h5("Epinephrine"), 
                                                                        value = 0), 
                             HTML("mcg/min"),
                             class="form-inline"),
                         div(style="display:inline-block", numericInput("Norepinephrine", 
                                                                        label = h5("Norepinephrine"), 
                                                                        value = 0), 
                             HTML("mcg/min"),
                             class="form-inline")
                         ),
                       #br(),
                       h3("Renal"),
                       div(style="display:inline-block", numericInput("Creatinine", 
                                    label = h5("Creatinine"), 
                                    value = 1), 
                           HTML(" mg/dL"),
                           class="form-inline"),
                       selectInput("Urineoutput", label = h5("Urine output"), 
                                   choices = list("500 mL/day or more" = 0, "200 - 500 mL/day" = 3, "200 mL/day or less" = 4), 
                                   selected = 2, width = '200px'),
                       br()
      ),
      
      conditionalPanel(condition="input.conditionedPanels==2",
                       helpText("Sofa Data"),
                       uiOutput("SofaFile"),
                       br(),
                       selectInput("loadsample", label = h5("Load Sample Data:"), 
                                   choices = list("Dont Load Sample Data" = 0,
                                                  "Load Improving Sample Data" = 1, 
                                                  "Load Worsening Sample Data" = 2, 
                                                  "Load Static Sample Data" = 3), 
                                   selected = 0, width = '200px')

      )
    )
    ),

    column(width = 10,
    tabsetPanel(
      tabPanel("SOFA Calculator", 
               value=1,
               br(),
               h3("Results"),
               br(),
               tableOutput("onetable"),
               br(),
               plotOutput("barplot1")
               ), 
      
      tabPanel("SOFA Spreadsheet", 
               value=2,
               plotOutput("sofaplot"),
               br(),
               hr(),
               HTML("This is a copy of the raw data set used to create that table."),
               DT::dataTableOutput('sofadataset')
               ),
      
      tabPanel("About SOFA", 
               value=3,
               HTML("<p><strong><u>About</u></strong></p>
                    <p>The<strong>&nbsp;Sequential Organ Failure Assessment</strong> (SOFA) is used to track the a patients status while in the <strong>intensive care unit</strong> (ICU). &nbsp;The score is the conglomeration six different scores relating to: respiratory, cardiovascular, hepatic, coagulation, renal and neurological systems.
                        Eache system is given a rank of 0 - 4, 4 being the worse.&nbsp;</p>
                    <p>SOFA does not predict mortality, but provides a mortality risk assessment. &nbsp;It was based on a study of 1449 patients from 40 intensive care units from around the world. &nbsp;The study followed patients over the age of 13 and who were in the ICU for longer than 48 hours. &nbsp;&nbsp;</p>"),
               
               HTML("<p><strong><u>How To Use</u></strong></p>
                    <p>Generally speaking a doctor would input an initial screening score in the calculator tab when the patient is first admitted. &nbsp;Then worst individual test results for all six areas in every 24 hours. &nbsp;</p>
                    <p>This shiny app also allows you to input simulated data taken approximately every hour to look for trends. &nbsp;But does not take the worst point in a 24 hour period. &nbsp;</p>
                    <p>to use the spreadsheet the following headings must be used from an excel spreadsheet:</p>
                    <table border='1' cellpadding='1' cellspacing='1' width='1188'>
                    	<tbody>
                    <tr height='20' width='120'>
                    <td>Name</td>
                    <td>Description</td>
                    </tr>
                    <tr height='20' width='120'>
                    <td>date</td>
                    <td>The date must come from using the date functions within excel (ie. now())</td>
                    </tr>
                    <tr height='20' width='120'>
                    <td width='64'>FiO2</td>
                    <td>Fraction of inspired oxygen.  Normal air at sea level has 20.9% oxygen.  Enriched air can go up to 100% although it is typically capped at 50% to avoid oxygen toxicity.</td>
                    </tr>
                    <tr height='20' width='120'>
                    <td width='64'>PaO2</td>
                    <td>Partial pressure arterial oxygen.  PaO2 describes the amount of oxygen dissolved in arterial blood plasma. The measurement is given as a pressure value (mmHg). 
                    <br>If PaO2 is > 80 mmHg, the patient has a normal value. 
                    <br>If PaO2 is < 80 mmHg, the patient has arterial hypoxemia. 
                    <br>79 - 70 mmHg = mild hypoxemia 
                    <br>69 - 60 = moderate hypoxemia 
                    <br>59 - 50 = severe hypoxemia 
                    <br>< 50 = extreme hypoxemia
                    </td>
                    </tr>
                    <tr height='20' width='120'>
                    <td width='64'>Mechanicalventilation</td>
                    <td>Mechanical ventilation is a method to mechanically assist or replace spontaneous breathing.  
                    <br>This is a True or False value.</td>
                    </tr>
                    <tr height='20' width='120'>
                    <td width='125'>Urineoutputscore</td>
                    <td>This is the amount urine excreted during a 24 hour period.
                    <br>500 mL/day or more = 0
                    <br>200 - 500 mL/day = 3
                    <br>200 mL/day or less = 4</td>
                    </tr>
                    <tr height='20' width='120'>
                    <td width='64'>Platelets</td>
                    <td>The platelet count is a lab test to measure how many platelets you have in your blood. Platelets are parts of the blood that help the blood clot. 
                    <br>< 150 = 1 
                    <br>< 100 = 2 
                    <br>< 50 = 3 
                    <br>< 20 = 4 
                    </td>
                    </tr>
                    <tr height='20' width='120'>
                    <td width='64'>Bilirubin</td>
                    <td>Bilirubin us an orange-yellow pigment formed in the liver by the breakdown of hemoglobin and excreted in bile.
                    <br>1.2–1.9 [> 20-32] = 1 
                    <br>2.0–5.9 [33-101] = 2 
                    <br>6.0–11.9 [102-204] = 3 
                    <br>> 12.0 [> 204] = 4 
                    </td>
                    </tr>
                    <tr height='20' width='120'>
                    <td width='127'>Glasgowcomascore</td>
                    <td>The Glasgow Coma Scale is a neurological scale which gives a way of assessing the conscious state of a person. A patient is assessed and given a score between 3 (indicating deep unconsciousness) and 15 (Alert and awake)
                    </td>
                    </tr>
                    <tr height='20' width='120'>
                    <td width='64'>MAP</td>
                    <td>MAP is an average blood pressure in an individual.  An normal range is between 65 and 110 mmHg</td>
                    </tr>
                    <tr height='20' width='120'>
                    <td width='64'>Vasopressors</td>
                    <td>Drugs that causing the constriction of blood vessels.
                        <br>This is a True False Value
                    </td>
                    </tr>
                    <tr height='20' width='120'>
                    <td width='64'>Dopamine</td>
                    <td>A drug that causes the constriction of blood vessels.</td>
                    </tr>
                    <tr height='20' width='120'>
                    <td width='64'>Dobutamine</td>
                    <td>A drug that causes the constriction of blood vessels.</td>
                    </tr>
                    <tr height='20' width='120'>
                    <td width='64'>Epinephrine</td>
                    <td>A drug that causes the constriction of blood vessels.</td>
                    </tr>
                    <tr height='20' width='120'>
                    <td width='64'>Norepinephrine</td>
                    <td>A drug that causes the constriction of blood vessels.</td>
                    </tr>
                    <tr height='20' width='120'>
                    <td width='64'>Creatinine</td>
                    <td>Creatine is a compound formed in protein metabolism and present in much living tissue. It is involved in the supply of energy for muscular contraction.
                    <br>1.2–1.9 [110-170] = 1 
                    <br>2.0–3.4 [171-299] = 2 
                    <br>3.5–4.9 [300-440] (or < 500 ml/d) = 3 
                    <br>> 5.0 [> 440] (or < 200 ml/d) = 4 
                    </td>
                    </tr>
                    </tbody>
                    </table>
                    <p></p>"),
               
               HTML("<p><strong><u>For Further Reading:</u></strong></p>
                    <p><a href='http://www.ncbi.nlm.nih.gov/pubmed/9824069'>Use of the SOFA score to assess the incidence of organ dysfunction/failure in intensive care units</a></p>")
               
      )
      , id = "conditionedPanels"
    )
  ) 
  )

)
)


