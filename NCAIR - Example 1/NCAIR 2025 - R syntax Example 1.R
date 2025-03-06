#import libraries
library(tidyverse)
library(skimr)
library(formattable)

#### Example Syntax ####
?read_csv() #pulling up the documentation for this function

file <- read_csv("CurrentStudents_2022.csv") #reading the CSV file in my local directory

file %>% view() #viewing the file, like you would in excel

skimr::skim(file) #a neat function for viewing descriptive stats of the file



###Read and combine all term files into one dataframe####
# Read in the column mapping file
col_mapping <- read_csv("Switchboard.csv", col_types = cols())

# Extract the mapping: Use most recent file column as the default, the older column names will be mapped to that
col_mapping_long <- col_mapping %>%
  pivot_longer(-1, names_to = "source", values_to = "old_name") %>% #easily pivot the file from wide to long
  select(old_name, new_name = 1) %>%  # Rename first column as new_name
  drop_na()  # Remove any NA values from the data

# Convert to a vector for easy lookup
col_mapping_vector <- deframe(col_mapping_long)

# Function to rename columns using mapping
rename_columns <- function(df) {
  new_colnames <- ifelse(colnames(df) %in% names(col_mapping_vector), #look for old columns names, else use the first column name 
                         col_mapping_vector[colnames(df)], 
                         colnames(df))  # Keep unmatched columns
  colnames(df) <- new_colnames
  return(df)
}

# Read and rename all CurrentStudents CSV files
df_list <- list.files(pattern = "^CurrentStudents.*\\.csv$", full.names = TRUE) %>% #pull all CSV files that start with CurrentStudents
  map(~ read_csv(.x, col_types = cols(.default = "c"))) %>%  # Force all columns as character because they're stored differently
  map(rename_columns) #add the rename function to have consistent column names

# Combine all cleaned data frames into one dataset, and do some data cleaning
FINAL <- bind_rows(df_list) %>% #bind_rows is a union that combines dataframes with matching columns
  mutate(Sex = case_when(Sex == "F" ~ "Female", #recode values to be consistent across years
                         Sex == "M" ~ "Male",
                         TRUE ~ Sex),
         Paid_Amount = str_remove(Paid_Amount, "\\$"), #remove any $ symbols from Paid_Amount
         Paid_Amount = str_remove(Paid_Amount, "\\,"), #remove any commas from Paid_Amount
         Paid_Amount = as.numeric(Paid_Amount), #treat Paid_Amount as a numeric variable
         PELL = if_else(!is.na(Paid_Amount) & Paid_Amount != "0", 
                        "Pell Grant Recipient", "Non-Pell")) %>% #an if_else function to code Pell Grant recipients
  select(-c(Paid_Amount))

#check for any duplicates within a single term
FINAL %>%
  count(`Term Code`, `Banner ID`) %>%
  filter(n>1) %>% 
  print()

####Create the retention/graduation rate to track cohorts #####
#Based on students entering term, determine the term that would coount for retention and grad rates
Retention <- FINAL %>% 
  filter(`Type` == "Entering First Year Student") %>% 
  rename(ID = `Banner ID`) %>% #an example of how to rename an existing column
  mutate(Cohort_Term = as.numeric(`Term Code`), #converting a string data type to numeric to add numeric values 
         TERM_RETENTION_0_2 = Cohort_Term + 1,
         TERM_RETENTION_1 = Cohort_Term + 100,
         TERM_RETENTION_1_2 = Cohort_Term + 101,
         TERM_RETENTION_2 = Cohort_Term + 200,
         TERM_RETENTION_2_2 = Cohort_Term + 201,
         TERM_RETENTION_3 = Cohort_Term + 300,
         TERM_RETENTION_3_2 = Cohort_Term + 301,
         TERM_RETENTION_4 = Cohort_Term + 400,
         TERM_RETENTION_4_2 = Cohort_Term + 401,
         TERM_GRADUATION_1 = Cohort_Term + 2,
         TERM_GRADUATION_2 = Cohort_Term + 102,
         TERM_GRADUATION_3 = Cohort_Term + 202,
         TERM_GRADUATION_4 = Cohort_Term + 302,
         TERM_GRADUATION_5 = Cohort_Term + 402,
         TERM_GRADUATION_6 = Cohort_Term + 502)

#make a dataframe to enable 0/1 coding of retention
Enrolled <- FINAL %>% 
  select(ID = `Banner ID`, `Term Code`) %>%
  mutate(Enrolled = 1,
         `Term Code` = as.numeric(`Term Code`))


#read the graduation file, default all column types to character
Graduation <- read_csv("Graduates.csv", col_types = "c") 

#create the final retention dataset that has 0/1 values in all retention/graduation columns
#using left_join to determine if the student is retained for the specific term or graduated before/at the term
Retention_FINAL <- left_join(Retention, Enrolled, by=c("ID", "TERM_RETENTION_0_2"="Term Code")) %>%
  rename(RETENTION_0_2 = Enrolled) %>% 
  relocate(RETENTION_0_2, .after="TERM_RETENTION_0_2") %>%
  left_join(., Enrolled, by=c("ID", "TERM_RETENTION_1"="Term Code")) %>%
  rename(RETENTION_1 = Enrolled) %>% 
  relocate(RETENTION_1, .after="TERM_RETENTION_1") %>% 
  left_join(., Enrolled, by=c("ID", "TERM_RETENTION_1_2"="Term Code")) %>%
  rename(RETENTION_1_2 = Enrolled) %>% 
  relocate(RETENTION_1_2, .after="TERM_RETENTION_1_2") %>%
  left_join(., Enrolled, by=c("ID", "TERM_RETENTION_2"="Term Code")) %>%
  rename(RETENTION_2 = Enrolled) %>% 
  relocate(RETENTION_2, .after="TERM_RETENTION_2") %>%
  left_join(., Enrolled, by=c("ID", "TERM_RETENTION_2_2"="Term Code")) %>%
  rename(RETENTION_2_2 = Enrolled) %>% 
  relocate(RETENTION_2_2, .after="TERM_RETENTION_2_2") %>%
  left_join(., Enrolled, by=c("ID", "TERM_RETENTION_3"="Term Code")) %>%
  rename(RETENTION_3 = Enrolled) %>% 
  relocate(RETENTION_3, .after="TERM_RETENTION_3") %>%
  left_join(., Enrolled, by=c("ID", "TERM_RETENTION_3_2"="Term Code")) %>%
  rename(RETENTION_3_2 = Enrolled) %>% 
  relocate(RETENTION_3_2, .after="TERM_RETENTION_3_2") %>%
  left_join(., Enrolled, by=c("ID", "TERM_RETENTION_4"="Term Code")) %>%
  rename(RETENTION_4 = Enrolled) %>% 
  relocate(RETENTION_4, .after="TERM_RETENTION_4") %>%
  left_join(., Enrolled, by=c("ID", "TERM_RETENTION_4_2"="Term Code")) %>%
  rename(RETENTION_4_2 = Enrolled) %>% 
  relocate(RETENTION_4_2, .after="TERM_RETENTION_4_2") %>%
  mutate_at(vars(starts_with("RETENTION")), 
            ~replace(., is.na(.), 0)) %>% #for all the retention columns, recode blank values to NA
  left_join(., Graduation, by=c("ID")) %>%
  mutate(GRADUATION_1 = if_else((Graduation_Term_Code <= TERM_GRADUATION_1), 1,0), #recode 1/0 to indicate if a student has graduated in Year 1
         GRADUATION_2 = if_else((Graduation_Term_Code <= TERM_GRADUATION_2), 1,0), #recode 1/0 to indicate if a student has graduated in Year 2
         GRADUATION_3 = if_else((Graduation_Term_Code <= TERM_GRADUATION_3), 1,0), #recode 1/0 to indicate if a student has graduated in Year 3
         GRADUATION_4 = if_else((Graduation_Term_Code <= TERM_GRADUATION_4), 1,0), #recode 1/0 to indicate if a student has graduated in Year 4
         GRADUATION_5 = if_else((Graduation_Term_Code <= TERM_GRADUATION_5), 1,0), #recode 1/0 to indicate if a student has graduated in Year 5
         GRADUATION_6 = if_else((Graduation_Term_Code <= TERM_GRADUATION_6), 1,0)) %>%
  relocate(GRADUATION_1, .after=TERM_GRADUATION_1) %>%
  relocate(GRADUATION_2, .after=TERM_GRADUATION_2) %>%
  relocate(GRADUATION_3, .after=TERM_GRADUATION_3) %>%
  relocate(GRADUATION_4, .after=TERM_GRADUATION_4) %>% 
  relocate(GRADUATION_5, .after=TERM_GRADUATION_5) %>%
  relocate(GRADUATION_6, .after=TERM_GRADUATION_6) %>%
  mutate_at(vars(starts_with("GRADUATION")), 
            ~replace(., is.na(.), 0)) #for all the graduation columns, recode blank values to NA

#now look how easy it is to pull retention and graduation rates for multiple cohorts! 
Retention_FINAL %>%
  group_by(Cohort_Term) %>%
  summarize(Cohort_Size=n(), #total number of students in the cohort
            Retained = sum(RETENTION_1), #count the number of students that retained to seccond year
            RET1 = mean(RETENTION_1), #averages of 0/1 to get the retention rate
            GRAD4 = mean(GRADUATION_4)) %>% #averages of 0/1 to get the graduation rate
  mutate(RET1_PERC = formattable::percent(RET1, digits=1), #convert the decimals to nicely formatted %s
         GRAD4_PERC = formattable::percent(GRAD4, digits=1)) %>%
  relocate(GRAD4, .before="GRAD4_PERC") %>% #example of how to easily relocate a column
  arrange(Cohort_Term) #sort the results by Cohort Term
