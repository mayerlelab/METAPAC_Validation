library(tidyverse)
library(patchwork)

source("https://raw.githubusercontent.com/umahajanatlmu/useful_commands/main/auxillary/basicFunctions.R")

######################
## age
######################
population_median_getting_PDAC <- 74 
cohort_median_age <-  66
cohort_median_getting_PDAC <- 69

age <- c("age", 
         NA, 
         population_median_getting_PDAC, 
         NA,
         cohort_median_getting_PDAC,
         "risk_factors")

######################
## Gender (M/F ratio)
######################
population_gender_ratio_getting_PDAC <- 15.4 / 12.0 #https://seer.cancer.gov/statfacts/html/pancreas.html
cohort_gender_ratio_age <-  681/670
cohort_gender_ratio_getting_PDAC <- 269/219

gender <- c("gender", 
         NA, 
         population_gender_ratio_getting_PDAC, 
         NA,
         cohort_gender_ratio_getting_PDAC,
         "risk_factors")

######################
## PDAC
######################
population_prevalence_PDAC <- (12.5/100000)*100 #31648972
cohort_approx_prevalence_PDAC <- 20 
cohort_prevalence_PDAC <- (489/1129)*100

pdac <- c("PDAC", 
            NA, 
            population_prevalence_PDAC, 
            NA,
            cohort_prevalence_PDAC,
            "disease")

######################
## Smoking
######################
population_prevalence_smoking <-  32.6 #10.1136/tobaccocontrol-2021-056535
odd_ratio_smokers_getting_PDAC <- 1.74 #10.1038/s41575-021-00457-x
# convert odds ratio to relative risk
# Formula: RR = OR/((1-P0) + (P0 × OR))
# where P0 is the prevalence in the unexposed group

# calculate P0 (baseline risk in non-smokers)
# p for total prevalence, p0 for non-smokers, p1 for smokers
# p = p0(1-s) + p1(s) where s is smoking prevalence
# p1 = RR × p0
# Therefore: p = p0(1-s) + (RR × p0)(s)
# p = p0(1-s + RR×s)
# Calculate relative risk from odds ratio
relative_risk <- odd_ratio_smokers_getting_PDAC/
  ((1-population_prevalence_smoking) + 
     (population_prevalence_smoking * odd_ratio_smokers_getting_PDAC))
# population prevalence
population_prevalence_smokers_getting_PDAC <- population_prevalence_smoking * relative_risk
cohorts_prevalence_smokers <- sum(c(2,63,61,22,1,103,6,30,76,76,4,139))/1353*100
cohorts_prevalence_smokers_getting_PDAC <- (sum(c(103,139))/sum(c(2,63,61,22,1,103,6,30,76,76,4,139)))*100

smoking <- c("smoking",
            population_prevalence_smoking, 
            population_prevalence_smokers_getting_PDAC, 
            cohorts_prevalence_smokers,
            cohorts_prevalence_smokers_getting_PDAC,
          "risk_factors")
######################
## DM
######################
population_prevalence_DM <-  10.5  #Magliano DJ, Boyko EJ; IDF Diabetes Atlas 10th edition scientific committee .
population_prevalence_DM_getting_PDAC <- 0.85 ## aproximation
cohorts_prevalence_DM <-(sum(c(1,36,56,51,2,154))/1353)*100
cohorts_prevalence_DM_getting_PDAC <-(154/sum(c(1,36,56,51,2,154)))*100

DM <- c("DM",
             population_prevalence_DM, 
             NA, 
             cohorts_prevalence_DM,
             NA,
             "risk_factors")

######################
## BMI
######################
population_prevalence_BMI <-  50.6 #https://ourworldindata.org/obesity
odd_ratio_BMI_getting_PDAC <- 1.19 #10.1038/s41575-021-00457-x
relative_risk <- odd_ratio_BMI_getting_PDAC/
  ((1-population_prevalence_BMI) + 
     (population_prevalence_BMI * odd_ratio_BMI_getting_PDAC))
# population prevalence
population_prevalence_BMI_getting_PDAC <- population_prevalence_smoking * relative_risk
cohorts_prevalence_BMI <- (sum(c(4,40,113,10,299,172))/1353)*100
cohorts_prevalence_BMI_getting_PDAC <- (299/sum(c(4,40,113,10,299,172)))*100

BMI <- c("BMI",
        population_prevalence_BMI, 
        NA, 
        cohorts_prevalence_BMI,
        NA,
        "risk_factors")

######################
## CP
######################
population_prevalence_CP <-  mean(c((30/100000)*100,(50/100000)*100)) #https://www.researchgate.net/profile/Joerg-Kleeff/publication/319600224_Chronic_pancreatitis/links/59b7d027aca2722453a6744d/Chronic-pancreatitis.pdf
population_prevalence_CP_getting_PDAC <- mean(c(2.0,2.6)) #https://www.nature.com/articles/s41598-022-26411-8
cohorts_prevalence_CP <- (113/1129)*100

CP <- c("CP",
         population_prevalence_CP, 
         population_prevalence_CP_getting_PDAC, 
         cohorts_prevalence_CP,
         NA,
         "risk_groups")

######################
## Cysts
######################
population_prevalence_Cysts <-  26#mean(c(13,18)) #https://www.sciencedirect.com/science/article/abs/pii/S1542356524002222
population_prevalence_Cysts_getting_PDAC <- (17/755)*100 #https://www.giejournal.org/article/S0016-5107(15)03046-1/fulltext
cohorts_prevalence_Cysts <- ((271+232)/1129)*100

Cysts <- c("Cysts",
        population_prevalence_Cysts, 
        NA, 
        cohorts_prevalence_Cysts,
        NA,
        "risk_groups")

######################
## Relatives_with_PDAC
######################
## Prevalence in exposed group=RR×Prevalence in unexposed group
population_prevalence_relatives_getting_PDAC <- 6.4*population_prevalence_PDAC #https://pmc.ncbi.nlm.nih.gov/articles/PMC7073774/#:~:text=FPC%20is%20defined%20by%20consensus,%25%20to%2018%25%2C%20respectively.
cohorts_prevalence_relatives <- (9/239)*100

relatives <- c("relatives",
           NA, 
           population_prevalence_relatives_getting_PDAC, 
           cohorts_prevalence_relatives,
           NA,
           "risk_groups")

# Create the dataframe by binding the rows
df <- rbind(age, gender, pdac, smoking, DM, BMI, CP, Cysts, relatives)

# Add column names
colnames(df) <- c("variable", 
                  "population_prevalence",
                  "population_prevalence_PDAC",
                  "cohort_prevalence",
                  "cohort_prevalence_PDAC",
                  "category")

# Convert to a tibble for better printing
df <- as_tibble(df)

# Convert numeric columns from character to numeric
df <- df %>%
  mutate(across(c(population_prevalence, 
                  population_prevalence_PDAC, 
                  cohort_prevalence, 
                  cohort_prevalence_PDAC), 
                ~as.numeric(.)))


library(tidyverse)
library(ggrepel)

# Combine prevalence columns and track sources
df_combined <- df %>%
  mutate(
    # Track source of population prevalence
    population_source = case_when(
      !is.na(population_prevalence) ~ "prev",  # Simplified
      !is.na(population_prevalence_PDAC) ~ "PDAC",  # Simplified
      TRUE ~ "missing"
    ),
    # Track source of cohort prevalence
    cohort_source = case_when(
      !is.na(cohort_prevalence) ~ "prev",  # Simplified
      !is.na(cohort_prevalence_PDAC) ~ "PDAC",  # Simplified
      TRUE ~ "missing"
    ),
    # Combine prevalences
    population_prev_combined = coalesce(population_prevalence, population_prevalence_PDAC),
    cohort_prev_combined = coalesce(cohort_prevalence, cohort_prevalence_PDAC),
    # Calculate standard errors
    population_se = sqrt((population_prev_combined * (100 - population_prev_combined)) / 100),
    cohort_se = sqrt((cohort_prev_combined * (100 - cohort_prev_combined)) / 100),
    # Create simplified labels with units
    label = case_when(
      variable == "age" ~ paste0(variable, "\n(Median Age)"),
      variable == "gender" ~ paste0(variable, "\n(M/F Ratio)"),
      TRUE ~ paste0(variable, "\n(", 
                    population_source, "/",
                    cohort_source, ")")
    )
  )

# Calculate correlation
correlation <- cor(
  df_combined$population_prev_combined,
  df_combined$cohort_prev_combined,
  use = "complete.obs"
)

# Fit linear model through origin and get summary
lm_fit <- lm(cohort_prev_combined ~ 0 + population_prev_combined, data = df_combined)
lm_summary <- summary(lm_fit)

# Extract p-value
p_value <- lm_summary$coefficients[1, 4]  # p-value for the slope

col <- c("#BC3C29FF","#E18727FF","#20854EFF")

# Create base plot
p <- ggplot(df_combined, aes(x = population_prev_combined, y = cohort_prev_combined)) +
  # Add smooth trend line through origin
  geom_smooth(method = "lm", formula = y ~ 0 + x, color = "#0072B5FF", alpha = 0.2) +
  # Add error bars
  geom_errorbar(aes(ymin = cohort_prev_combined - cohort_se,
                    ymax = cohort_prev_combined + cohort_se),
                width = 0.5, alpha = 0.5) +
  geom_errorbarh(aes(xmin = population_prev_combined - population_se,
                     xmax = population_prev_combined + population_se),
                 height = 0.5, alpha = 0.5)

# Add outer circles for PDAC population source
p <- p + geom_point(data = filter(df_combined, population_source == "PDAC"),
                    aes(color = category),
                    size = 5,  # Larger size for outer circle
                    shape = 1) # Empty circle

# Add regular points
p <- p + geom_point(aes(color = category), size = 3)

# Add repelled labels
p <- p + geom_text_repel(
  aes(label = label),
  size = 3
)

# Get slope coefficient
slope <- coef(lm_fit)[1]

# Add formatting with p-value and slope in the subtitle
p <- p +
  labs(
    title = "Population vs Cohort Prevalence",
    subtitle = paste("Correlation:", round(correlation, 3),
                     " P-value:", round(p_value, 4),
                     "\nLarge circles indicate PDAC population source"),
    x = "Population Prevalence (%)",
    y = "Cohort Prevalence (%)"
  ) +
  theme_publication(base_family = "sans") +
  scale_color_manual(values =  col) +
  # Force plot to include (0,0)
  expand_limits(x = 0, y = 0)

# Display plot
print(p)

ggsave(filename = "results/cohort_cmparison.pdf",
       plot=p,
       width = 6.5, height = 5,
       dpi= 300)


# Print summary of data sources
cat("\nData Source Summary:\n")
df_combined %>%
  select(variable, population_source, cohort_source, 
         population_prev_combined, cohort_prev_combined) %>%
  arrange(variable) %>%
  mutate(
    source_combination = paste(population_source, "/",
                               cohort_source)
  ) %>%
  print(n = nrow(.))

# Print detailed results
df_combined %>%
  select(variable, 
         population_prev_combined, population_se, population_source,
         cohort_prev_combined, cohort_se, cohort_source,
         category) %>%
  filter(!is.na(population_prev_combined) & !is.na(cohort_prev_combined)) %>%
  mutate(
    diff = cohort_prev_combined - population_prev_combined,
    diff_se = sqrt(population_se^2 + cohort_se^2),
    z_score = diff / diff_se,
    p_value = 2 * (1 - pnorm(abs(z_score)))
  ) %>%
  arrange(p_value)
