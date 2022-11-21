##First model: Only use variables with positive correlation coefficient
lm_fit_1 <- lm(target_wins ~ team_batting_h + team_batting_2b + team_batting_bb + team_pitching_hr + team_batting_3b +
                 team_pitching_bb + team_baserun_sb + team_batting_hbp + team_baserun_cs, data = training_set)
tidy(lm_fit_1)


##Diagnostics
plot(lm_fit_1)
#check for spefication bias
lm_df <- broom::augment(lm_fit_1)

##histogram of residuals
#slight right tail
lm_df |> 
  ggplot(aes(.resid)) +
  geom_histogram(binwidth = 10) +
  labs(
    title = "Histogram of residuals"
  ) +
  xlab("Residuals") +
  theme_minimal()

#Distribution of residuals, small deviations
qqnorm(lm_fit_1$residuals)

qqnorm(lm_fit_1$fitted.values)
#Fitted vs. residuals
#Heavy concentration of values
lm_df |> 
  ggplot(aes(x = .fitted, y = .resid)) +
  geom_point() +
  geom_smooth(method = "loess", se = FALSE, linetype = "dashed", color = "red") +
  labs(
    title = "Fitted vs. residual values"
  ) +
  xlab("Fitted Value") +
  ylab("Residuals") +
  theme_minimal()

#Residuals against estimated Y
lm_df |> 
  ggplot(aes(x = .resid, y = .fitted)) +
  geom_point() +
  geom_smooth(method = "loess", se = FALSE, linetype = "dashed", color = "red") +
  labs(
    title = "Fitted vs. residual values"
  ) +
  xlab("Fitted Value") +
  ylab("Residuals") +
  theme_minimal()

#Estimated Y vs actual y
#Major outlier
lm_df |> 
  ggplot(aes(x = .hat, y = target_wins)) +
  geom_point() +
  geom_smooth(method = "loess", se = FALSE, linetype = "dashed", color = "red") +
  labs(
    title = "Fitted vs. residual values"
  ) +
  xlab("Fitted Value") +
  ylab("Residuals") +
  theme_minimal()

lm_fit_2 <- lm(target_wins ~ ., data = training_set)
lm_fit_2_stepwise <- MASS::stepAIC(lm_fit_2, direction = "both", trace = FALSE)
tidy(lm_fit_2_stepwise)
tidy(lm_fit_2)
