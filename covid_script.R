library(tidyverse)
library(ggbiplot)

##############################
# Read data from github repo #
##############################

# Load latest dasta from the Dutch Natuional Institute of Health (RIVM)
df <- read_csv2(file = "https://data.rivm.nl/covid-19/COVID-19_aantallen_gemeente_per_dag.csv",
         col_names = TRUE) %>%
  dplyr::group_by(Municipality_code, Date_of_publication) %>%
  mutate(cumulative_Total_reported = cumsum(Total_reported),
         cumulative_Hospital_admission = cumsum(Hospital_admission),
         cumulative_Deceased = cumsum(Deceased)) %>%
  dplyr::ungroup() %>%
  pivot_longer(cols = 10:15,
               names_to = "case",
               values_to = "number")

df.small <- df[nrow(df)-500:nrow(df),1:ncol(df)]

###########
# Plot NL #
###########

df %>% 
  filter(!case %in% c("cumulative_Total_reported", "cumulative_Hospital_admission", "cumulative_Deceased")) %>%
  dplyr::group_by(Date_of_publication, case) %>% 
  dplyr::summarise(sum_nl = sum(number)) %>%
  ggplot(aes(x = Date_of_publication,
             y = sum_nl,
             colour = case,
             fill = case))+
  geom_bar(stat = "identity")+
  facet_wrap(~case, ncol = 1, scale = "free")+
  ylab("Number of cases per day")+
  xlab("Date")+
  theme_bw()

##############
# Cumulative #
##############
df %>% 
  filter(case %in% c("cumulative_Total_reported", "cumulative_Hospital_admission", "cumulative_Deceased")) %>%
  dplyr::group_by(Date_of_publication, case) %>% 
  dplyr::summarise(sum_nl = sum(number)) %>%
  ggplot(aes(x = Date_of_publication,
             y = sum_nl,
             colour = case,
             fill = case))+
  geom_bar(stat = "identity")+
  facet_wrap(~case, ncol = 1, scale = "free")+
  ylab("Number of cases")+
  xlab("Date")

###########################
# Filter per Minicipality #
###########################

m = "Zaanstad"

df %>% 
  filter(!case %in% c("cumulative_Total_reported", "cumulative_Hospital_admission", "cumulative_Deceased"),
         Municipality_name == m) %>%
  dplyr::group_by(Date_of_publication, case) %>% 
  dplyr::summarise(sum_nl = sum(number)) %>%
  ggplot(aes(x = Date_of_publication,
             y = sum_nl,
             colour = case,
             fill = case))+
  geom_bar(stat = "identity")+
  facet_wrap(~case, ncol = 1, scale = "free")+
  ylab("Number of cases")+
  xlab("Date")

####################
# Plot by Province #
####################

p = "Noord-Holland"

df %>% 
  filter(!case %in% c("cumulative_Total_reported", "cumulative_Hospital_admission", "cumulative_Deceased"),
         Province == p) %>%
  dplyr::group_by(Date_of_publication, case) %>% 
  dplyr::summarise(sum_nl = sum(number)) %>%
  ggplot(aes(x = Date_of_publication,
             y = sum_nl,
             colour = case,
             fill = case))+
  geom_bar(stat = "identity")+
  facet_wrap(~case, ncol = 1, scale = "free")+
  ylab("Number of cases")+
  xlab("Date")

#####################
# Compare Provinces #
#####################

df %>% 
  filter(case == "Total_reported") %>%
  dplyr::group_by(Date_of_publication, Province, case) %>% 
  dplyr::summarise(sum_nl = sum(number)) %>%
  ggplot(aes(x = Date_of_publication,
             y = sum_nl,
             fill = Province))+
  geom_bar(stat = "identity")+
  facet_wrap(~Province, scale = "free")+
  ylab("Number of cases")+
  xlab("Date")+
  theme_bw()



######################
# PCA and regression #
######################

df.pca <- df %>%
  select(Date_of_publication, Municipality_name, ROAZ_region, Province, case, number) %>%
  filter(case == "Total_reported") %>%
  dplyr::group_by(Date_of_publication, Province, Municipality_name) %>%
  dplyr::summarise(sum_number = sum(number)) %>%
  drop_na() 


matrix.pca <- df.pca %>% select(Date_of_publication, Municipality_name, sum_number) %>%
  pivot_wider( names_from = "Date_of_publication",
         values_from = "sum_number") %>%
  drop_na(Municipality_name) %>%
  column_to_rownames("Municipality_name")


pca <- prcomp(log(matrix.pca[,2:ncol(matrix.pca)]+1))

ggbiplot(pca,
         var.axes = FALSE,
         groups = matrix.pca$Province,
         ellipse = TRUE,
         labels = rownames(matrix.pca))+
  theme_minimal()
