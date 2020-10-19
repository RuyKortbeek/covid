library(tidyverse)

##############################
# Read data from github repo #
##############################

df <- read_csv2(file = "https://data.rivm.nl/covid-19/COVID-19_aantallen_gemeente_per_dag.csv",
         col_names = TRUE) %>%
  mutate(cumulative_Total_reported = cumsum(Total_reported),
         cumulative_Hospital_admission = cumsum(Hospital_admission),
         cumulative_Deceased = cumsum(Deceased)) %>%
  pivot_longer(cols = 10:15,
               names_to = "case",
               values_to = "number")

df.small <- df[1:20,1:ncol(df)]

###########
# Plot NL #
###########

df %>% 
  filter(!case %in% c("cumulative_Total_reported", "cumulative_Hospital_admission", "cumulative_Deceased")) %>%
  dplyr::group_by(Date_of_publication, Province, case) %>% 
  dplyr::summarise(sum_nl = sum(number)) %>%
  ggplot(aes(x = Date_of_publication,
             y = sum_nl,
             colour = case,
             fill = case))+
  geom_bar(stat = "identity")+
  facet_wrap(~case, ncol = 1, scale = "free")+
  ylab("Number of cases")+
  xlab("Date")

##############
# Cumulative #
##############
df %>% 
  filter(case %in% c("cumulative_Total_reported", "cumulative_Hospital_admission", "cumulative_Deceased")) %>%
  dplyr::group_by(Date_of_publication, Province, case) %>% 
  dplyr::summarise(sum_nl = sum(number)) %>%
  ggplot(aes(x = Date_of_publication,
             y = sum_nl,
             colour = case,
             fill = case))+
  geom_bar(stat = "identity")+
  facet_wrap(~case, ncol = 1, scale = "free")+
  ylab("Number of cases")+
  xlab("Date")
