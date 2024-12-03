library(here)
library(tidyverse)
library(ggbreak) 
library(stats)

library(tidyverse)
library(stats)

# Function to calculate p-value from mean and CI
.calculate_pvalue <- function(mean1, lower1, upper1, mean2, lower2, upper2) {
  # Calculate SE
  se1 <- (upper1 - lower1)/(2*1.96)
  se2 <- (lower2 - upper2)/(2*1.96)
  
  # Calculate t-statistic
  t_stat <- (mean1 - mean2)/sqrt(se1^2 + se2^2)
  
  # Calculate degrees of freedom (using Welch-Satterthwaite approximation)
  df <- (se1^2 + se2^2)^2/(se1^4/(2-1) + se2^4/(2-1))
  
  # Calculate two-sided p-value
  p_value <- 2 * pt(-abs(t_stat), df)
  
  return(p_value)
}

dat <- readxl::read_excel("FINAL_RESULTS/TASA_Lancet_gastro/results/Table_onlySpecificity.xlsx")

dat$Group <- factor(dat$Group, levels = c("All stages", "Resectable tumors, stages IA-II", "Detectable CA19.9", "CA19.9 (<37 U/ml)" , "CA19.9 (<10 U/ml)","CA19.9 (>2 U/ml)"))

dat <- dat[!dat$Group %in% c("CA19.9 (<10 U/ml)","CA19.9 (>2 U/ml)"),]

# Modify your data to add p-values
dat <- dat %>%
  group_by(Group) %>%
  mutate(
    p_value = case_when(
      Model == "i_met" ~ .calculate_pvalue(
        mean, lower, upper,
        mean[Model == "CA alone"], 
        lower[Model == "CA alone"], 
        upper[Model == "CA alone"]
      ),
      Model == "m_met" ~ .calculate_pvalue(
        mean, lower, upper,
        mean[Model == "CA alone"], 
        lower[Model == "CA alone"], 
        upper[Model == "CA alone"]
      ),
      Model == "CA alone" ~ NA_real_
    )
  ) %>%
  ungroup()

# Format p-values for display
dat <- dat %>%
  mutate(
    p_value_formatted = case_when(
      is.na(p_value) ~ NA_character_,
      p_value < 0.001 ~ "<0.001",
      TRUE ~ sprintf("%.3f", p_value)
    )
  )

# p <- ggplot(dat, aes(x = Model, y = mean, fill = Model)) + 
#   geom_bar(stat = "identity", color = "black", position = position_dodge(width = 0.9)) +
#   geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.2, position = position_dodge(0.9)) +
#   theme_bw() +
#   facet_wrap(~ Group, nrow = 1) +  # Removed scales = "free_x"
#   #scale_y_break(c(0.6, 0.78), scales = 2) +
#   scale_fill_manual(values = c("#E18727FF", "#0072B5FF", "#BC3C29FF")) +
#   theme(legend.position = "bottom")

# Assuming 'dat' is your data frame
p <- ggplot(dat, aes(x = Model, y = mean, fill = Model)) + 
  geom_bar(stat = "identity", color = "black", position = position_dodge(width = 0.9)) +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.2, position = position_dodge(0.9)) +
  geom_text(aes(label = p_value_formatted, y = upper + 0.02),  # Adjust y position as needed
            position = position_dodge(width = 0.9), vjust = 0) +
  facet_wrap(~ Group, nrow = 1) +
  scale_fill_manual(values = c("#E18727FF", "#0072B5FF", "#BC3C29FF")) +
  theme_publication(base_family = "sans") +
  theme(axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        axis.ticks.x = element_blank())

print(p)


ggsave(filename = "results/specificity_plot.pdf",
       plot=p,
       width = 9, height =4,
       dpi= 300)

