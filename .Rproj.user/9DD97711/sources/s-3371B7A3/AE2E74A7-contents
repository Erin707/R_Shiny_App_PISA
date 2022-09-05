
server <- function(input, output, session){
  
  data_input <- reactive( {
    tmp_pisa <- data_pisa_result_long %>% 
      filter(year == input$sel_year,
             subject == input$sel_subj)
    

  })
  
  output$world_map <- renderLeaflet({
    
    m <- get_pisa_plot(data_input())
    m
  }) 
 
  output$pisa_rank <- renderDataTable({
    data_pisa_result_long %>% 
      filter(year == input$sel_year) %>% 
      group_by(subject) %>% 
      mutate(rank = rank(-score, ties.method = "min")) %>% 
      mutate(score = round(score, 2)) %>% 
      pivot_wider(names_from = "subject",
                  values_from = c("score", "rank"))
    
  })
  
  
  output$ctr_trend <- renderPlot({
    
    n_ctr <- length(input$sel_ctr)
    
    plot_df <- data_pisa_result_long %>% 
      left_join(concord_ctr_name_iso3, by = "iso3") %>% 
      filter(name %in% input$sel_ctr)
    
    trend_plot <- get_trend_plot(plot_df, n_ctr)
    trend_plot
    
  })
   
}

