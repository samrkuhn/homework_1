
#Create named character vector of variables
vars <- training_set |> 
  #select(-index) |> 
  names() |> 
  set_names()

#Use map function to create a sequence of plots
scatter_plots <-  map(vars, ~ggplot(data = training_set) +
              geom_point(aes(x = target_wins, y = .data[[.x]]) ) +
              theme_minimal() +
              labs(y = .x)
)



#Correlation matrix
cor_matrix <- training_set |> 
  dplyr::select(-index) |>
  cor() |> 
  as.matrix()

corrplot(cor_matrix)

#Get correlation values as a table, sorted highest to lowest
purrr::map_df(vars, ~cor(training_set$target_wins, training_set[[.x]])) |> 
  pivot_longer(cols = !c("target_wins"), names_to = "correlation") |> 
  arrange(desc(value)) |> 
  gt::gt()


