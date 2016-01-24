library(shinythemes)
fluidPage(theme = shinytheme('united'),
        navbarPage("Blood Donation Prediction Interface",
                   tabPanel('Predict Donation',
                            fluidRow(
                                column(4, wellPanel(
                                        dateRangeInput('dateRange',
                                                       label='Select Donor Activity Period',
                                                       start = '2005-01-01',
                                                       end = '2007-01-01',
                                                       separator = ' to ',
                                                       min = '2001-01-01',
                                                       max = '2007-02-28',
                                                       format = 'MM-dd-yyyy',
                                                       startview = 'month'
                                                       ),
                                        numericInput("volumeDonated",
                                                     "Select Total Volume Donated (c.c.):",
                                                     value = 1500,
                                                     min = 250,
                                                     max = 10000,
                                                     step = 250),
                                        actionButton('submitButton','Predict Donation Likelihood!')
                                )),
                                column(6, h2('Predictor Inputs'),
                                       textOutput("start"),
                                       textOutput("recent"),
                                       textOutput("volDonated"),
                                       hr(),
                                       h2('March 07 Donation Likelihood'),
                                       tableOutput("prediction")
                                       )
                                )
                   ),
                   tabPanel('About',
                            fluidRow(
                                    column(6, h1('DrivenData.org Blood Donation Prediction'),
                                           hr(),
                                           h2('Instructions'),
                                           em('**PLEASE ALLOW approx. 5 SECONDS FOR TRAINING MODELS ON INITIAL LOAD**'), br(),
                                           p("In the Interface provided, the user
                                             may simply enter the date range that indicates the Donor's activity period
                                             (first donation to last donation). Additionally, select the total volume donated
                                             , then click 'Predict Donation Likelihood!' to predict whether or not the donor
                                             will return for a donation in March 2007.", style = "color:blue"),
                                           hr(),
                                           h2('About'),
                                           p("This RStudio Shiny powered interface serves as a quick way to
                                             test a parametric model on the dataset for the 'Warmup: Predict
                                             Blood Donations' competition (link on the right.)"),
                                           br(),
                                           p("The aim is to predict whether or not a Donor will donate blood in 
                                             March 2007. 
                                             There are 3 predictors considered: "),
                                           p("* First Donated: Time in months since donor's first donation,"),
                                           p("* Last Donated: Time in months since donor's most recent donation,"),
                                           p("* Volume Donated: Total donation volume as measured in cubic centimeters."),
                                           hr(),
                                           h2('Outputs'),
                                           p("The predictors have been trained on the entire dataset provided at the competition
                                             website from the ", a("UCI Machine Learning Repository.", 
                                                                                                   href= "https://archive.ics.uci.edu/ml/datasets/Blood+Transfusion+Service+Center")),
                                           br(),
                                           p('Two classification, and two tree-based algorithms are trained on the dataset on application load
                                             (allow about 3-4 seconds for initial load). Once trained, the user may enter different
                                             inputs to see the predictions made by each type of algorithm. The
                                             class prediction probabilities generated by each algorithm on the training set are 
                                             also displayed alongside the predicted class, along with the in-sample evaluation metric: 
                                             the LogLoss score (the lower it is, the more it fits the training data).')
                                           
                                           
                                           ),
                                    column(6, h1('References and Links'),
                                                    hr(),
                                           p("* ", a("Pitch Deck", 
                                                                 href= "http://rpubs.com/ashirwad/PredictBloodDonations")),
                                           p("* ", a("Application Code", 
                                                                 href= "https://github.com/ashirwad08/DD-Blood-Donations")),
                                           p("* Competition Site: ", a("Driven Data", 
                                                                 href= "http://www.drivendata.org/competitions/2/")),
                                           p("* Creator: ", a("Ash Chakraborty", 
                                                             href="https://www.linkedin.com/in/ashirwadchakraborty")),
                                           hr()
                                           )
                            )
                   )

))