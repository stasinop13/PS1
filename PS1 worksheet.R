library(haven); library(dplyr); library(knitr)

# Read the SPSS file
data <- read_sav("SAF_R9.data_.final_.wtd_release.30May23.sav")


#Question 2
#Age is Q1
#Q2 is what is the primary language you speak at home
#Q84A is Ethnic community, cultural group or tribe 
#Gender of respondent is Q100, 1=Man, 2=Woman 
data %>%
  select(RESPNO, Q1, Q2, Q84A, Q100) %>%
  summary() %>%
  kable(caption = "Summary of Selected Variables")


#question 3
#prop.table takes a table and turns it into frequencies (percentage of total for each response type)
china_support <- prop.table(table(data$Q78A))

# Convert the table to a data frame and rename columns, this is necessary because it seems like there is no way to rename within kable
china_support_df <- as.data.frame(china_support) %>%
  setNames(c("Outlook on Chinese Influence (Q78A)", "Relative Frequency"))

#output table (which is actually a data frame now)
kable(china_support_df)

#question 4
#prop.table takes a table and turns it into frequencies (percentage of total for each response type)
usa_support <- prop.table(table(data$Q78B))

# Convert the table to a data frame and rename columns, this is necessary because it seems like there is no way to rename within kable
usa_support_df <- as.data.frame(usa_support) %>%
  setNames(c("Outlook on Chinese Influence (Q78A)", "Relative Frequency"))

#output table (which is actually a data frame now)
kable(usa_support_df)

#question 5
data %>%
  mutate(
    across(Q78A:Q78B, # vars to work on
           ~ if_else(.x %in% 1:5, .x, NA) # function applied to vars
    )
  )

t.test(data$Q78A, data$Q78B, paired = TRUE)

