# load essential packages
install.packages("readr") 
install.packages("haven")
library(readr)
library(haven)

# import data 
ins_df <- read_tsv("C:/Users/janel/Desktop/ASDS/Applied Stats/replication/rawdata/insta_posts_coded.tsv")   #instagrame post data
survey_df<- read_dta("C:/Users/janel/Desktop/ASDS/Applied Stats/replication/rawdata/btw2021_influencers_survey.dta")  # respondent survey data

# check data
head(ins_df)
head(survey_df)

str(ins_df)
str(survey_df)
summary(survey_df)

# Part 1: Instagram Post Visualization (Fig 2and 3 Replication)
library(tidyverse) 
library(lubridate)
library(scales) 

# data cleaning for instagram post analysis
clean_df <- ins_df |> 
  mutate(
    pos_neg = ifelse(pos_neg_mode == "party_no", "no", "yes"),
    date = ymd(post_date),
    wk = week(post_date)
  ) |>
  group_by(user_md5) |> 
  mutate(obs = n()) |>
  filter(obs >= 5) |> 
  ungroup()

# calculate the proportion of post types per influencer (for Fig 2)
agg_long <- clean_df |>
  group_by(user_md5) |> 
  summarize(
    pct_ad = mean(ads_mode == "yes"),     
    pct_pol = mean(politics_mode == "yes"), 
    pct_pty = mean(pos_neg == "yes")       
  ) |>
  pivot_longer(cols = c("pct_ad", "pct_pol", "pct_pty")) |>
  mutate(name = factor(name, levels = c("pct_pty", "pct_pol", "pct_ad")))

# replicate the Violin plot (Fig 2)
pdf("fig2_rep.pdf", width = 10, height = 7)
fig2_rep <- agg_long |>
  ggplot(aes(x = value, y = name)) +
  geom_violin(aes(fill = name, alpha = 0.8), linewidth = 1) +
  geom_boxplot(width = 0.15, color = "black", fill = "white") +
  scale_y_discrete(
    labels = c(
      "pct_pty" = "Posts including support\nor disapproval of parties,\npoliticians, or political events",
      "pct_pol" = "Posts including\npolitical content",
      "pct_ad" =  "Posts including\nadvertisement"
    )
  ) +
  scale_fill_brewer(palette = "Set2") + 
  scale_x_continuous(labels = percent_format(), breaks = pretty_breaks(n = 9)) +
  labs(
    title = "Distribution of Post Types per Influencer",
    y = NULL, 
    x = "\nContent Type Percentage (by Influencer)",
    caption = paste0(
      "Source: Posts scraped through Instagram API from June 2021 to September 2021"
    )
  ) +
  theme_light(base_size = 14) +
  theme(
    plot.title = element_text(color = "black", face = "bold", size = 16, hjust = 0.5, margin = margin(b = 15)),
    legend.position = "none",
    axis.text.y = element_text(size = 12, lineheight = 1.2),
    plot.margin = margin(t = 20, r = 20, b = 20, l = 10),
    plot.caption = element_text(face = "italic", color = "gray40", size = 10, margin = margin(t = 15))
  )
print(fig2_rep)
dev.off()

# aggregate post trends by WEEK (for Fig 3)
trend_wk <- clean_df |> 
  filter(date < ymd("2021-09-28")) |>  
  group_by(wk) |>         
  summarise(
    pct_ad = mean(ads_mode == "yes"),     
    pct_pol = mean(politics_mode == "yes"), 
    pct_pty = mean(pos_neg == "yes")  
  ) |>
  pivot_longer(cols = c("pct_ad", "pct_pol", "pct_pty")) |>
  mutate(name = factor(name, levels = c("pct_pty", "pct_pol", "pct_ad")))

# build the LOESS plot (Fig 3)
pdf("fig3_rep.pdf", width = 10, height = 7)
fig3_rep <- trend_wk |>
  ggplot(aes(x = wk, y = value, group = name, color = name)) + 
  geom_point(size = 2) +
  geom_smooth(aes(fill = name), method = "loess", alpha = 0.25, level = 0.95) +
  geom_vline(xintercept = 38.5, linetype = "dashed", color = "darkgray", linewidth = 1) + 
  annotate("text", x = 37.8, y = 0.50, label = "German Federal Election (BTW) 2021", size = 5, hjust = 1, lineheight = 1.1) + 
  scale_x_continuous(breaks = pretty_breaks(n = 7)) + 
  scale_y_continuous(labels = percent_format(), breaks = pretty_breaks(n = 7)) +
  scale_color_manual(
    values = c("pct_pty" = "#440154", 
               "pct_pol" = "#21908C",  
               "pct_ad"  = "#FDE725"), 
    labels = c("pct_pty" = "support / disproval of political\nentities or events",
               "pct_pol" = "including political content",
               "pct_ad"  = "including advertisement")
  ) +
  scale_fill_manual(
    values = c("pct_pty" = "#440154",  
               "pct_pol" = "#21908C",  
               "pct_ad"  = "#FDE725"), 
    labels = c("pct_pty" = "support / disproval of political\nentities or events",
               "pct_pol" = "including political content",
               "pct_ad"  = "including advertisement")
  ) +
  
  labs(
    title = "Weekly Trends of Influencer Post Content (2021)", 
    x = "\nWeek of Year 2021", 
    y = "Percentage of Posts\n", 
    fill = NULL, 
    color = NULL,
    caption = "Source: Posts scraped through Instagram API from June 2021 to September 2021\nPoints denote average weekly values, and bands denote loessnsmoothed uncertainty estimates. "
  ) +
  theme_light(base_size = 14) +
  theme(
    plot.title = element_text(color = "black", face = "bold", size = 16, hjust = 0.5, margin = margin(b = 15)),
    legend.position = c(0.25, 0.85),
    legend.background = element_rect(fill = "white", color = "gray80"),
    legend.text = element_text(size = 11, lineheight = 1.2),
    plot.margin = margin(t = 20, r = 20, b = 20, l = 20),
    plot.caption = element_text(face = "italic", color = "gray40", size = 11, margin = margin(t = 15), hjust = 1)
  )

print(fig3_rep)
dev.off()


# Part 2: Survey Data Logit Models (Fig 4 Replication)
library(tidyverse)
library(broom)
library(stargazer)

# data cleaning for survey 
survey_rep <- survey_df |>  
  filter(!is.na(duration) & duration >= 120) |>
  mutate(across(everything(), ~ ifelse(. %in% c(99, "99"), NA, .))) |> 
  mutate(
    res_id = str_c("r_", as.character(respondent_id)),
    edu_cat = case_when(
      edu %in% 1:3 ~ 1,
      edu == 4 ~ 2,
      edu %in% 5:6 ~ 3,
      str_detect(edu_other, "Berufsschule") ~ 1,
      str_detect(edu_other, "Studi|Promotion|Meister") ~ 3
    ) |> as_factor(),
    fem_dum = ifelse(sex == 2, 1, 0),        
    aw_dum = ifelse(influ_aware == 1, 1, 0), 
    fw_dum = ifelse(influ_follow == 1, 1, 0),
    polint = as.numeric(polint),
    sm04_num = as.numeric(why_use_sm04),
    sm05_num = as.numeric(why_use_sm05),
    sm07_num = as.numeric(why_use_sm07)
  )

# calculate count of social media platforms used weekly
survey_rep <- survey_rep |>
  mutate(
    sm_oth = ifelse(str_length(sm_other) > 1, "Y", "N"),
    sm_cnt = rowSums(across(c(sm_whatsapp:sm_twitch, sm_oth), ~ .x == "Y"), na.rm = TRUE)
  )

# fit the logit models
mod_aw <- glm(
  aw_dum ~ age + edu_cat + fem_dum + sm_cnt +
    sm04_num + sm05_num + sm07_num + polint,
  data = survey_rep,
  family = binomial(link = "logit") 
)

mod_fw <- glm(
  fw_dum ~ age + edu_cat + fem_dum + sm_cnt +
    sm04_num + sm05_num + sm07_num + polint,
  data = survey_rep,
  family = binomial(link = "logit")
)

nobs(mod_aw)
nobs(mod_fw)

# fit the null models 
mod_aw_null <- glm(
  aw_dum ~ 1, 
  data = model.frame(mod_aw), 
  family = binomial(link = "logit")
)

mod_fw_null <- glm(
  fw_dum ~ 1, 
  data = model.frame(mod_fw), 
  family = binomial(link = "logit")
)

# perform likelihood ratio tests to assess overall model fit
print(anova(mod_aw_null, mod_aw, test = "LRT"))
print(anova(mod_fw_null, mod_fw, test = "LRT"))

# output logit regression results as a LaTeX table (Converting Log-odds to Odds Ratios)
stargazer(
  mod_aw, mod_fw, 
  title = "Logit Regression on Influencer Awareness and Following",
  label = "tab:logit_models",
  column.labels = c("Awareness of Influencers (OR)", "Following Influencers (OR)"),
  covariate.labels = c("Age", "Education: mid", "Education: high", "Sex: Female",
                       "Number of SM used", "Using SM: Public figures", 
                       "Using SM: Brands", "Using SM: Entertainment", "Interest in politics"),
  dep.var.labels.include = FALSE,
  coef = list(exp(coef(mod_aw)), exp(coef(mod_fw))), 
  type = "latex",
  out = "logit_models.tex"
)

# extract and combine model coefficients (Using exponentiate = TRUE for Odds Ratios)
tidy_aw <- tidy(mod_aw, exponentiate = TRUE, conf.int = TRUE) |> 
  mutate(mod_type = "Awareness of influencers\n(n = 925)") |> 
  filter(term != "(Intercept)")

tidy_fw <- tidy(mod_fw, exponentiate = TRUE, conf.int = TRUE) |> 
  mutate(mod_type = "Following any influencers\n(n = 708)") |>
  filter(term != "(Intercept)")

plot_dat <- bind_rows(tidy_aw, tidy_fw) |>
  mutate(
    mod_type = factor(mod_type, levels = c("Following any influencers\n(n = 708)", 
                                           "Awareness of influencers\n(n = 925)")),
    term_cln = case_when(
      term == "age" ~ "Age",
      term == "edu_cat2" ~ "Education: mid (ref. = low)",
      term == "edu_cat3" ~ "Education: high (ref. = low)",
      term == "fem_dum" ~ "Sex: Female",
      term == "sm_cnt" ~ "Number of SM used\n>= once per week",
      term == "sm04_num" ~ "Using SM to follow persons of public interest",
      term == "sm05_num" ~ "Using SM to follow companies or brands",
      term == "sm07_num" ~ "Using SM for entertainment",
      term == "polint" ~ "Interest in politics",
      TRUE ~ term
    )
  )

# set proper top-to-bottom order for the y-axis
plot_dat$term_cln <- factor(plot_dat$term_cln, levels = rev(c(
  "Age", "Education: mid (ref. = low)", "Education: high (ref. = low)",
  "Sex: Female", "Number of SM used\n>= once per week",
  "Using SM to follow persons of public interest",
  "Using SM to follow companies or brands",
  "Using SM for entertainment", "Interest in politics"
)))

# build the coefficient plot (Fig 4)
pdf("fig4_rep.pdf", width = 11, height = 7) # slightly increased width to accommodate legend on right
fig4_rep <- ggplot(plot_dat, aes(x = estimate, y = term_cln, color = mod_type)) +
  geom_vline(xintercept = 1, color = "black", linetype = "dashed") +
  geom_point(position = position_dodge(width = 0.5), size = 3) +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), 
                 position = position_dodge(width = 0.5), height = 0, linewidth = 1) +
  scale_color_manual(
    values = c("Awareness of influencers\n(n = 925)" = "#E69F00", 
               "Following any influencers\n(n = 708)" = "#56B4E9"),
    breaks = c("Awareness of influencers\n(n = 925)", 
               "Following any influencers\n(n = 708)")
  ) +
  labs(
    title = "Logit Model Estimates: Influencer Awareness and Following",
    x = "\nOdds Ratios (OR)", 
    y = "Covariates\n", 
    color = "Dependent Variables",
    # updated source text and added detail about OR/CI
    caption = "Source: German Federal Election (BTW) 2021 Influencer Survey\nPoints denote coefficients (OR) from two logistic regression models. Bars denote 95% confidence intervals."
  ) +
  theme_light(base_size = 14) +
  theme(
    plot.title = element_text(color = "black", face = "bold", size = 16, hjust = 0.5, margin = margin(b = 15)),
    legend.position = "top",
    legend.background = element_blank(),
    legend.text = element_text(size = 11, lineheight = 1.2),
    legend.title = element_text(size = 12, face = "bold"),
    plot.margin = margin(t = 20, r = 20, b = 20, l = 20),
    plot.caption = element_text(face = "italic", color = "gray40", size = 11, margin = margin(t = 15), hjust = 1)
  )
print(fig4_rep)
dev.off()

# Part 3:model extention:recoding and ordered logit
library(MASS)
install.packages("brant")
library(brant)  
library(tidyverse)

# check value distribution of covariates
dist_plot_data <- survey_rep |>
  dplyr::select(polint, sm04_num, sm05_num, sm07_num) |>
  pivot_longer(cols = everything(), names_to = "variable", values_to = "score") |>
  filter(!is.na(score)) |>
  mutate(variable = case_when(
    variable == "polint" ~ "Political Interest",
    variable == "sm04_num" ~ "Motivation: Follow Celebs (sm04)",
    variable == "sm05_num" ~ "Motivation: Follow Brands (sm05)",
    variable == "sm07_num" ~ "Motivation: Entertainment (sm07)"
  ))

# visualize distribution for checking non-uniformity
pdf("covariate_distribution.pdf", width = 12, height = 8)
dist_plot <- ggplot(dist_plot_data, aes(x = factor(score), fill = variable)) +
  geom_bar(show.legend = FALSE, alpha = 0.8) +
  facet_wrap(~variable, scales = "free_y") +
  scale_fill_manual(values = c("#C1CDC1", "#A2B5CD", "#009E73", "#CC79A7")) +
  labs(
    title = "Distribution of Likert-scale Covariates (1-5)",
    subtitle = "Checking for non-uniformity",
    x = "\nResponse Categories (1 = Low, 5 = High)",
    y = "Count of Respondents\n",
    caption = "Source:German Federal Election (BTW) 2021 Influencer Survey. Note: Non-uniform distributions suggest that \ntreating these as continuous intervals may violate linearity assumptions."
  ) +
  theme_light(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5, size = 13, margin = margin(b = 15)), 
    strip.background = element_rect(fill = "gray90"),
    strip.text = element_text(color = "black", face = "bold"),
    plot.margin = margin(20, 20, 20, 20),
    plot.caption = element_text(face = "italic", color = "gray40", size = 11, margin = margin(t = 15), hjust = 1) 
  )
print(dist_plot)
dev.off()

# recode covariates and build new dependent variable
survey_ext <- survey_rep |>
  mutate(
    # dependent variable: influencer engagement
    influ_eng = case_when(
      aw_dum == 0 ~ 0,                                          # level 0: don't know influencer
      aw_dum == 1 & fw_dum == 0 ~ 1,                            # level 1: know but not follow
      aw_dum == 1 & fw_dum == 1 ~ 2                             # level 2: know and follow
    ),
    influ_eng_fact = factor(influ_eng, 
                            levels = c(0, 1, 2), 
                            labels = c("None", "Aware", "Engaged"), 
                            ordered = TRUE),
    
    # political interest: 1-2(low), 3(moderate), 4-5(high)
    polint_cat = case_when(
      polint %in% 1:2 ~ "Low",
      polint == 3     ~ "Moderate",
      polint %in% 4:5 ~ "High"
    ) |> as_factor() |> fct_relevel("Low"),   # set "low" as reference baseline
    
    # calculate continuous index for entertainment and commercial motivation
    ent_mot_index = rowMeans(across(c(sm04_num, sm05_num, sm07_num)), na.rm = TRUE),
    
    # transform the continuous index into a 3-level categorical factor variable
    ent_mot_factor = case_when(
      ent_mot_index < 3 ~ "Low",
      ent_mot_index >= 3 & ent_mot_index < 4 ~ "Moderate",
      ent_mot_index >= 4 ~ "High"
    ) |> as_factor() |> fct_relevel("Low")    # set "low" as reference baseline
  )

# fit the ordered logit model using the new extension dataset
ext_ord <- polr(
  influ_eng_fact ~ age + edu_cat + fem_dum + sm_cnt + 
    polint_cat + ent_mot_factor,
  data = survey_ext,
  Hess = TRUE
)

# test the parallel regression assumption
brant_test <- brant(ext_ord)
print(brant_test)

# transform to multinomial logistic regression
library(nnet)
# recode dependent variable as an UNORDERED factor 
survey_multi <- survey_ext |>
  mutate(
    # remove the 'ordered = TRUE' constraint for multinomial logit
    influ_eng_fact = factor(influ_eng, 
                            levels = c(0, 1, 2), 
                            labels = c("None", "Aware", "Engaged"), 
                            ordered = FALSE),
    # explicitly set "None" as the reference group (baseline)
    influ_eng_fact = relevel(influ_eng_fact, ref = "None")
  ) |>
  drop_na(influ_eng_fact, age, edu_cat, fem_dum, sm_cnt, 
          polint_cat, ent_mot_factor)

# fit the multinomial logit model
ext_multi <- multinom(
  influ_eng_fact ~ age + edu_cat + fem_dum + sm_cnt + 
    polint_cat + ent_mot_factor,
  data = survey_multi
)

# summary the model
summary(ext_multi)

# output LaTeX table 
stargazer(
  ext_multi, 
  type = "latex",
  title = "Multinomial Logistic Regression Predicting Influencer Engagement(OR)",
  label = "tab:multi_model",
  column.labels = c("Aware (vs. None)", "Engaged (vs. None)"), 
  covariate.labels = c("Age", "Education: mid", "Education: high", "Sex: Female",
                       "Number of SM used", 
                       "Pol. Interest: Moderate (ref: Low)", "Pol. Interest: High (ref: Low)",
                       "Ent/Com. Motivation: Moderate (ref: Low)", "Ent. Motivation: High (ref: Low)", 
                       "Constant"),
  dep.var.labels.include = FALSE,
  apply.coef = exp, 
  p.auto = FALSE,   
  notes = "Note: The comparison category is 'None'. Coefficients represent relative Odds Ratios.", 
  notes.append = TRUE,
  out = "multinomial_model_OR.tex"
)

# evaluate model fit with confusion matrix
conf_matrix <- addmargins(table(Actual = survey_multi$influ_eng_fact, Predicted = predict(ext_multi, type = "class")))
print(conf_matrix)

# calculate overall accuracy
acc_rate <- sum(diag(conf_matrix)[1:3]) / conf_matrix[4, 4]
cat(sprintf("Overall Model Accuracy: %.2f%%\n", acc_rate * 100))

# predict probabilities for four specific audience profiles
pre_data <- data.frame(
  polint_cat = factor(c("Low", "Low", "High", "High"), levels = c("Low", "Moderate", "High")),
  ent_mot_factor = factor(c("Low", "High", "Low", "High"), levels = c("Low", "Moderate", "High")),
  age = mean(survey_multi$age, na.rm = TRUE),              
  edu_cat = factor("2", levels = levels(survey_multi$edu_cat)),
  fem_dum = 1,                                                 
  sm_cnt = mean(survey_multi$sm_cnt, na.rm = TRUE)           
)

# extract predicted probabilities
predicted_probs <- predict(ext_multi, newdata = pre_data, type = "probs")

# print the predicted probabilities 
predicted_values <- cbind(
  Profile = c("Low pol. interest, low ent/com motivation", 
              "Low pol. interest, high ent/com motivation", 
              "High pol. interest, low ent/com motivation", 
              "High pol. interest, high ent/com motivation"),
  round(predicted_probs * 100, 2) 
)
print(as.data.frame(predicted_values))

# Visualize probability variation
library(tidyr)
library(stringr)

# Convert the predicted values table into a data frame
plot_data <- as.data.frame(predicted_values)

# Make sure probability columns are numeric
plot_data$None <- as.numeric(plot_data$None)
plot_data$Aware <- as.numeric(plot_data$Aware)
plot_data$Engaged <- as.numeric(plot_data$Engaged)

plot_data_long <- plot_data |>
  mutate(
    # rename profiles to be more descriptive and concise
    label_name = case_when(
      str_detect(Profile, "Low pol.*low ent") ~ "Low Pol. Int.\nLow Ent. Mot.",
      str_detect(Profile, "Low pol.*high ent") ~ "Low Pol. Int.\nHigh Ent. Mot.",
      str_detect(Profile, "High pol.*low ent") ~ "High Pol. Int.\nLow Ent. Mot.",
      str_detect(Profile, "High pol.*high ent") ~ "High Pol. Int.\nHigh Ent. Mot."
    ),
    label_name = factor(label_name, levels = unique(label_name))
  ) |>
  pivot_longer(cols = c("None", "Aware", "Engaged"), 
               names_to = "Engagement", 
               values_to = "Probability") |>
  mutate(
    # set the stack order: Engaged on top, None at bottom
    Engagement = factor(Engagement, levels = c("Engaged", "Aware", "None"))
  )

# build and save the optimized stacked bar chart
pdf("fig5_predicted_probs.pdf", width = 10, height = 7)
fig5_probs <- ggplot(plot_data_long, aes(x = label_name, y = Probability, fill = Engagement)) +
  geom_bar(stat = "identity", width = 0.6, color = "black", linewidth = 0.5) +
  # add percentage labels inside the bars
  geom_text(aes(label = sprintf("%.1f%%", Probability)), 
            position = position_stack(vjust = 0.5), 
            size = 4, color = "white", fontface = "bold") +
  # academic color palette (orange for the core finding 'Engaged')
  scale_fill_manual(values = c("Engaged" = "#D55E00", "Aware" = "#56B4E9", "None" = "#999999")) +
  labs(
    title = "Predicted Probabilities of Influencer Engagement",
    subtitle = "By Audience Motivation Profiles (Multinomial Logit Model)",
    x = "\nAudience Type",
    y = "Predicted Probability (%)\n",
    fill = "Engagement Level:"
  ) +
  theme_light(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5, size = 16),
    plot.subtitle = element_text(hjust = 0.5, size = 13, margin = margin(b = 15)),
    legend.position = "top",
    legend.title = element_text(face = "bold"),
    axis.text.x = element_text(size = 11, lineheight = 1.1, face = "bold"),
    axis.text.y = element_text(size = 12),
    plot.margin = margin(20, 20, 20, 20)
  )
print(fig5_probs)
dev.off()