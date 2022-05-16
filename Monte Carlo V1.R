
# Load Packages -----------------------------------------------------------

library(tidyverse)
library(readxl)
library(janitor)
library(skimr)
library(writexl)

# Read in Assumptions Data ------------------------------------------------

df_assumptions <- read_excel("Raw_Data/assumptions.xlsx") %>%
  clean_names() %>%
  mutate(
    min_plan = round(0.95*plan_out,0), 
    max_plan = round(1.05*plan_out,0),
    sd_plan = round((max_plan - min_plan)/3.29,0), 
    
    max_waste_1 = wastage_1_rt + 0.05,
    min_waste_1 = wastage_1_rt - 0.03, 
    sd_waste_1 = (max_waste_1 - min_waste_1)/3.29,
    
    max_waste_2 = wastage_2_rt + 0, 
    min_waste_2 = wastage_2_rt - 0.1,
    sd_waste_2 = (max_waste_2 - min_waste_2)/3.29,
    
    mth_index = abs(mth_no -13))


# Define Constants --------------------------------------------------------

n <- 10000
goal <- 2000

# Define Monte Carlo Function ---------------------------------------------

MC_Sim <- function(df_source, x, s) {
  output <- NULL
  for (i in 1:12) {
    output <- cbind(
      output, 
      qnorm(
        runif(n), 
        mean = as.numeric(df_source %>% select(all_of(x), all_of(s)) %>% pull(1) %>% nth(i)),
        sd = as.numeric(df_source %>% select(all_of(x), all_of(s)) %>% pull(2) %>% nth(i))
      )
    )
  }
  colnames(output) = c("jan", "feb", "mar", "apr", "may", "jun", "jul", "aug", "sep", "oct", "nov", "dec")
  output <- output %>% data.frame()
}


# Define Some Cleaning Functions ------------------------------------------

# Replace negatives with 0's 
replace_neg <- function(df_source) {
  df_source[df_source <0] <- 0
  df_source
}

# replace x >1 with x=1

replace_upper <- function(df_source) { 
  df_source[df_source > 1] <- 1
  df_source
  }

# Run the Monte Carlo Simulations -----------------------------------------

df_mc_out <- df_assumptions %>%
  MC_Sim("plan_out", "sd_plan") %>%
  round(0) %>% 
  replace_neg()

df_mc_waste1 <- df_assumptions %>% 
  MC_Sim("wastage_1_rt", "sd_waste_1") %>%
  round(digits = 2) %>% 
  replace_neg() %>%
  replace_upper()

df_mc_waste2 <- df_assumptions %>% 
  MC_Sim("wastage_2_rt", "sd_waste_2") %>%
  round(digits = 2) %>% 
  replace_neg() %>%
  replace_upper()


df_expected <- round(df_mc_out * (1 - df_mc_waste1) * df_mc_waste2, 0) %>% 
  mutate(
    total = jan + feb + mar + apr + may + jun + jul + aug + sep + oct + nov + dec,
    success_ind = if_else(
      total >= goal, "success", "failure"))


save(df_expected, file = 'Clean_Data/mc_sim_v1.Rdata')


skim(df_expected)
