## Setup ----
# Load libraries
library(dplyr)
library(ggplot2)
# Load data
individual <- readr::read_csv(
  here::here("data", "individual.csv")
) %>%
  select(stem_diameter, height, growth_form)

## Subset analysis data ----
analysis_df <- individual %>%
  filter(complete.cases(.), growth_form != "liana")

## Order growth form levels
gf_levels <- table(analysis_df$growth_form) %>%
  sort() %>%
  names()
analysis_df <- analysis_df %>%
  mutate(growth_form = factor(growth_form,
                              levels = gf_levels
  ))

## Plots ----
## Figure 1: Bar plot of growth forms
analysis_df %>%
  ggplot(aes(
    y = growth_form, colour = growth_form,
    fill = growth_form
  )) +
  geom_bar(alpha = 0.5, show.legend = FALSE)

## Figure 2: Violin plots of stem diameter and height across growth forms
analysis_df %>%
  tidyr::pivot_longer(
    cols = c(stem_diameter, height),
    names_to = "var",
    values_to = "value"
  ) %>%
  ggplot(aes(
    x = log(value), y = growth_form,
    colour = growth_form, fill = growth_form
  )) +
  geom_violin(alpha = 0.5, trim = TRUE, show.legend = FALSE) +
  geom_boxplot(alpha = 0.7, show.legend = FALSE) +
  facet_grid(~var) +
  theme_linedraw()

## Fit overall linear model ----
lm_overall <- lm(
  log(stem_diameter) ~ log(height),
  analysis_df
)
lm_overall %>%
  broom::glance()
lm_overall %>%
  broom::tidy()

## Plot overall linear model
analysis_df %>%
  ggplot(aes(x = log(height), y = log(stem_diameter))) +
  geom_point(alpha = 0.2) +
  geom_smooth(method = "lm") +
  xlab("Log of height (m)") +
  ylab("Log of stem diameter (cm)") +
  theme_linedraw()

## Fit linear model with growth form interaction ----
lm_growth <- lm(
  log(stem_diameter) ~ log(height) * growth_form,
  analysis_df
)
lm_growth %>%
  broom::glance()
lm_growth %>%
  broom::tidy()

## Plot linear model with growth form interaction
analysis_df %>%
  ggplot(aes(x = log(height), y = log(stem_diameter), colour = growth_form)) +
  geom_point(alpha = 0.1) +
  geom_smooth(method = "lm") +
  labs(
    x = "Log of height (m)",
    y = "Log of stem diameter (cm)",
    colour = "Growth forms"
  ) +
  theme_linedraw()
