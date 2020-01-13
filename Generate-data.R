library(sn)
library(truncnorm)

set.seed(1000)


# Generate reparation dataset  ------------------------------------------

terminals_df <- tibble(
  terminal_model = c(rep("a", length(parts_a)), rep("b", length(parts_b))), 
  nr_terminals = c(rep(nr_a, length(parts_a)), rep(nr_b, length(parts_b))),
  part_code = c(parts_a, parts_b)
) %>%
  mutate(
    part = ifelse(str_detect(part_code,"K"), "Keyboard", "Screen")
  )


simulate_parts_failures <- function(part_code, nr_terminals){
  #Draw failures for each part from their respective distribution
  if (part_code == "K01"){
    df = data.frame(
      failure_description = rep("Key not responding", nr_terminals*.3),
      lifetime_at_failure = c( rsn(n = nr_terminals*.2, omega = 60, alpha = 10, tau = 0),  #Skewed normal
                               rsn(n = nr_terminals*.1, omega = -100, alpha = 5, tau = 10) + 300))  #Skewed normal
  }
  if (part_code == "S01"){
    df = data.frame(
      failure_description = c(rep("Screen flickering", nr_terminals*.2),
                              rep("Screen shutdown", nr_terminals*.2)),
      lifetime_at_failure = c(rsn(n = nr_terminals*.05, omega = -150, alpha = 10000, tau = .2) + 800, #Skewed normal
                              rtruncnorm(n = nr_terminals*.15, mean = 800, sd = 600, a = 800), #Truncated normal (only variates above average are kept)
                              runif(n = nr_terminals*.2, min = 1, max = 2500))) # Uniform
  }
  if (part_code == "K02"){
    df = data.frame(
      failure_description = rep("Key not responding", nr_terminals*.15),
      lifetime_at_failure = rsn(n = nr_terminals*.15, omega = -100, alpha = 5, tau = 10) + 300)  #Skewed normal
  }
  # eliminate failures in life <=0 and resample to keep the same number of failures 
  x <- df$lifetime_at_failure
  df$lifetime_at_failure <- c(x[x>0], sample(x[x>0], size = length(x[x<=0])))
  
  return(df)
}

(failures_df <- terminals_df %>%
  mutate(failures = map2(part_code, nr_terminals, simulate_parts_failures)))

failures_df <- unnest(failures_df)

# Generate sales dataset  ----------------------------------------------

library(lubridate)
library(tidyverse)

### Sales dataset generation ---------


probability_vector<-
  rep(c(rep(1,304),cumprod(rep(1.02, 61))),3) * seq(from = 1, to = 5, length.out = 1095)

plot.ts(probability_vector)

sales_df<-data.frame(
  device_model = c(rep("A", sales_A), rep("A", sales_B)),
  date_sold = sample(seq(from = startdate, to = enddate, by = "day"), size = 3000, prob = probability_vector, replace = TRUE)
) %>%
  group_by(device_model, date_sold) %>%
  summarize(sales = n()) 

