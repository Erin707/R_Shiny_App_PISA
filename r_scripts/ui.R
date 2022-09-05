

ui <- navbarPage("PISA World",
                 tabPanel("PISA World Map",
                          sidebarLayout(
                            sidebarPanel(
                              radioButtons("sel_year", "Choose a PISA Cycle",
                                           years),
                              radioButtons("sel_subj", "Choose a PISA Subject",
                                           c("maths" = "math", "reading" = "read", "science"="science"))
                            ),
                            mainPanel(
                              leafletOutput("world_map", width = "100%", 
                                            height = 300),
                              DT::dataTableOutput("pisa_rank", width = "100%", 
                                                  height = "auto")
                            )
                          )),
                 tabPanel("Zoom in Country",
                          sidebarLayout(
                            sidebarPanel(
                              selectInput("sel_ctr", 
                                          "Select a country to see trend",
                                          choices = all_ctr_name,
                                          multiple = TRUE)
                            ),
                            mainPanel(
                              plotOutput("ctr_trend", width = "100%")
                            )
                          )),
                 tabPanel("Socio-economic Status")
)