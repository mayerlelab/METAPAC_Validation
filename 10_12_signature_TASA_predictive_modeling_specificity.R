library(here)
library(tidyverse)
library(ggbreak) 

dat <- readxl::read_excel("FINAL_RESULTS/TASA/results/Table_onlySpecificity.xlsx")

dat$Group <- factor(dat$Group, levels = c("All stages", "Resectable tumors, stages IA-II", "Detectable CA19.9", "CA19.9 (<37 U/ml)" , "CA19.9 (<10 U/ml)"))

  p <- ggplot(dat,
            aes(x=Model,
                y= mean,
                fill = Model)) + 
  geom_bar(stat="identity", color="black", 
                       position=position_dodge()) +
  geom_errorbar(aes(ymin=lower, ymax=upper), width=.2,
                            position=position_dodge(.9)) +
  theme_bw() +
  facet_wrap(~Group, scales = "free_x", ncol = 5) +
  scale_y_break(c(.6, 0.78), scales=2) +
    scale_fill_manual(values = c("#E18727FF","#0072B5FF","#BC3C29FF")) +
  theme(legend.position = "bottom")

print(p)


ggsave(filename = "results/specificity_plot.pdf",
       plot=p,
       width = 9, height =4,
       dpi= 300)
  