library(tidyverse)
library(kableExtra)
df <- read.csv("performances.csv")


df2 <- df %>%
  filter(bt == "pb_str_orb_no",
         k == 15,
         delta_00 == 0,
         sigma2_u == 0.01,
         sigma2_v == 0.01,
         psss == "small") %>%
  select(avg_num_study_selected,
         avg_num_out_selected,
         avg_perc_out_excluded) %>%
  transmute(numberStudies = round(avg_num_study_selected),
         numberOutcomes = round(avg_num_out_selected),
         percentExcluded = round(avg_perc_out_excluded))

table_out <- knitr::kable(df2, "html", align = "c") %>%
    kable_paper(full_width = F) %>%
    column_spec(1, bold = T) #%>%
    # collapse_rows(columns = 1:2, valign = "top") %>%
    # scroll_box()
table_out

df3 <- df %>%
  filter(sigma2_u == sigma2_v) %>%
  select(bt,
         k,
         delta_00,
         sigma2_u,
         sigma2_v,
         psss,
         avg_num_study_selected,
         avg_num_out_selected,
         avg_perc_out_excluded) %>%
  transmute(biasType = bt,
            k = k,
            betweenStudyVar = sigma2_u,
            withinStudyVar = sigma2_v,
            sampleSize = psss,
            numberStudies = round(avg_num_study_selected),
            numberOutcomes = round(avg_num_out_selected),
            percentExcluded = round(avg_perc_out_excluded))


table_out3 <- knitr::kable(df3, "html", align = "c") %>%
  kable_paper(full_width = F) %>%
  column_spec(1, bold = T) %>%
  collapse_rows(columns = 1:6, valign = "top") %>%
  scroll_box()
table_out3
