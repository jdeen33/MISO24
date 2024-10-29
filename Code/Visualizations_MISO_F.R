library(data.table)
library(dplyr)
library(tidyverse)
library(ggplot2)
library(hrbrthemes)
library(glue)
library(scales)


## more cleaning ####

files_ <- list.files("/Users/jdeen@middlebury.edu/Documents/Lib_Data/MISO24/Clean_Data/Proportion_tables/CA_Students", 
                    pattern = ".csv$", recursive = TRUE, full.names = TRUE)
files <- files_[-99]
CA_Students <-rbind(read_csv(files[1:length(files)]))
medians<- read_csv("/Users/jdeen@middlebury.edu/Documents/Lib_Data/MISO24/Clean_Data/Medians/CA_Students.csv")
Q_key<- read_csv("/Users/jdeen@middlebury.edu/Documents/Lib_Data/MISO24/Clean_Data/Proportion_tables/CA_Students/Q_Keys.csv")
names(Q_key)[names(Q_key) == '...1'] <- 'service'
names(Q_key)[names(Q_key) == 'V1'] <- 'service_long'
names(medians)[names(medians)=="...1"] <- 'service'
names(medians)[names(medians)=="V1"]<- 'median'
CA_Students_F <-  merge(CA_Students,Q_key,by="service") 
CA_Students_Medians <- merge(medians,Q_key, by= "service")
write_csv(CA_Students_Medians,"/Users/jdeen@middlebury.edu/Documents/Lib_Data/MISO24/Clean_Data/Final_clean/CA_Students/CA_Students_Medians.csv")


VT_Staff <- read_csv("/Users/jdeen@middlebury.edu/Documents/Lib_Data/MISO24/Clean_Data/Final_clean/VT_Staff/VT_Staff_F.csv")


Importance <- VT_Staff[VT_Staff$service %like% "IMP",]
Frequency<- CA_Students[CA_Students$service %like% "USE",]
Satisfaction<- CA_Students[CA_Students$service %like% "DS",]
Informed<- CA_Students[CA_Students$service %like% "INF",]
Learn<- CA_Students[CA_Students$service %like% "LRN",]

write_csv(Frequency,"/Users/jdeen@middlebury.edu/Documents/Lib_Data/MISO24/Clean_Data/Final_clean/CA_Students/CA_Students_freq.csv")
write_csv(Satisfaction,"/Users/jdeen@middlebury.edu/Documents/Lib_Data/MISO24/Clean_Data/Final_clean/CA_Students/CA_Students_sat.csv")
write_csv(Informed,"/Users/jdeen@middlebury.edu/Documents/Lib_Data/MISO24/Clean_Data/Final_clean/CA_Students/CA_Students_Informed.csv")
write_csv(Learn,"/Users/jdeen@middlebury.edu/Documents/Lib_Data/MISO24/Clean_Data/Final_clean/CA_Students/CA_Students_lrn.csv")
write_csv(Importance ,"/Users/jdeen@middlebury.edu/Documents/Lib_Data/MISO24/Clean_Data/Final_clean/VT_Staff/VT_Staff_imp.csv")

help_desk_imp <- imp[imp['service']=="IMP_FPC",]
canvas_imp <- imp[imp['service']=="IMP_CMS",]

tot<-  imp[1:16,1:4]
tot_<- tot%>% 
  mutate_at(vars(Percentage), ~ round(., 1))  

ggplot(tot_, aes(fill=Value, y= Percentag),reorder( x=service_long,)) + 
  geom_bar(position= position_stack(), stat="identity",color="black")+ scale_fill_brewer(palette = "BuPu")+ coord_flip()+ ggtitle('Middlebury College Faculty') + geom_text(aes(label=Percentage,fontface = 'bold'), position=position_stack(vjust=0.5), size=3)






library(ggplot2)

ggplot(tot_, aes(fill = reorder(Value), y = Percentage, x = reorder(service_long, Percentage))) + 
  geom_bar(position = position_stack(), stat = "identity", color = "black") + 
  scale_fill_brewer(palette = "BuPu") + 
  coord_flip() + 
  ggtitle('Middlebury College Faculty') + 
  geom_text(aes(label = scales::percent(Percentage / 100, accuracy = 1), fontface = 'bold'), 
            position = position_stack(vjust = 0.5), size = 3) + 
  theme_minimal() + 
  theme(plot.title = element_text(hjust = 0.5))







##### combining categories 
CA_Faculty_imp <- read_csv("/Users/jdeen@middlebury.edu/Documents/Lib_Data/MISO24/Clean_Data/Final_clean/CA_Faculty/CA_Faculty_imp.csv")

#condition<- test$Value < 3 
#condition_row_sums <- test %>%
 # filter(condition) %>%
 # mutate(Unsatisfied = sum(select(., Percentage )))
## ^ one method that didn't work well

by_service <- split( CA_Faculty_imp , f = CA_Faculty_imp$service ) 
## ^ splitting dataframe into groups with the same index (index is the service 
##that the data is concerning)

## Importance Scores 
for(i in seq_along(by_service)){
  df <- as.data.frame(by_service[i])
  colnames(df) <- c("service","Value","Percentage", "service_long") 
  cat(names(colnames(df)))
  total_percentage_p <- df %>%
    filter(Value %in% c("Important", "Very Important")) %>% #filters rows in column for categories to combine 
    group_by(Value,service,service_long) %>%# groupby to we can do summary stats to get sum of percentages 
    summarise(
      total_percentage = sum(Percentage, na.rm = TRUE),#gives us value only
      count = n(), #not aure what this does tbh need to check
      .groups = 'drop') %>% mutate(total_percentage = sum(total_percentage, na.rm = TRUE)) #drops our grouping and adds a total_percentage column 
  
  total_percentage_n <- df%>%
    filter(Value %in% c("Somewhat Important", "Not Important")) %>% #filters rows in column for categories to combine 
    group_by(Value,service,service_long) %>%# groupby to we can do summary stats to get sum of percentages 
    summarise(
      total_percentage = sum(Percentage, na.rm = TRUE),#gives us value only
      count = n(), #not aure what this does tbh need to check
      .groups = 'drop') %>% mutate(total_percentage = sum(total_percentage, na.rm = TRUE))
  
  final_percentage_p <- total_percentage_p[!duplicated(total_percentage_p$total_percentage), ]
  
  final_percentage_n <- total_percentage_n[!duplicated(total_percentage_n$total_percentage),]
  
  final_percentage_t <- rbind(final_percentage_p, final_percentage_n  )
  
  nom <- final_percentage_t$service[1]
  
  write.csv(final_percentage_t, glue("/Users/jdeen@middlebury.edu/Documents/Lib_Data/MISO24/Clean_Data/Final_clean/Proportion_tables_combined/CA_Faculty/Importance/{nom}_imp.csv"))
  
}




#subset(test1, IMP_AWAC.Value == "Very Important" | IMP_AWAC.Value == "Important")
## ^ other subsetting method 
#sum(test[which(test$Value > 3), 3])

###https://www.marsja.se/how-to-sum-rows-in-r-master-summing-specific-rows-with-dplyr/


#### Satisfaction #####
CA_students_DS <- read_csv("/Users/jdeen@middlebury.edu/Documents/Lib_Data/MISO24/Clean_Data/Final_clean/CA_Students/CA_Students_sat.csv")

by_service <- split( CA_students_DS , f = CA_students_DS$service ) 

for(i in seq_along(by_service)){
  df <- as.data.frame(by_service[i])
  colnames(df) <- c("service","Value","Percentage", "service_long") 
  cat(names(colnames(df)))
  total_percentage_p <- df %>%
    filter(Value %in% c("Satisfied", "Somewhat Satisfied")) %>% #filters rows in column for categories to combine 
    group_by(Value,service,service_long) %>%# groupby to we can do summary stats to get sum of percentages 
    summarise(
      total_percentage = sum(Percentage, na.rm = TRUE),#gives us value only
      count = n(), #not aure what this does tbh need to check
      .groups = 'drop') %>% mutate(total_percentage = sum(total_percentage, na.rm = TRUE)) #drops our grouping and adds a total_percentage column 
  
  total_percentage_n <- df%>%
    filter(Value %in% c("Somewhat Dissatisfied", "Dissatisfied")) %>% #filters rows in column for categories to combine 
    group_by(Value,service,service_long) %>%# groupby to we can do summary stats to get sum of percentages 
    summarise(
      total_percentage = sum(Percentage, na.rm = TRUE),#gives us value only
      count = n(), #not aure what this does tbh need to check
      .groups = 'drop') %>% mutate(total_percentage = sum(total_percentage, na.rm = TRUE))
  
  final_percentage_p <- total_percentage_p[!duplicated(total_percentage_p$total_percentage), ]
  
  final_percentage_n <- total_percentage_n[!duplicated(total_percentage_n$total_percentage),]
  
  final_percentage_t <- rbind(final_percentage_p, final_percentage_n  )
  
  nom <- final_percentage_t$service[1]
  
  write.csv(final_percentage_t, glue("/Users/jdeen@middlebury.edu/Documents/Lib_Data/MISO24/Clean_Data/Final_clean/Proportion_tables_combined/CA_Students/Satisfaction/{nom}_imp.csv"))
  
}


#### Informed #####

CA_Students_Informed <- read_csv("/Users/jdeen@middlebury.edu/Documents/Lib_Data/MISO24/Clean_Data/Final_clean/CA_Students/CA_Students_informed.csv")

by_service <- split(CA_Students_Informed , f = CA_Students_Informed$service )

for(i in seq_along(by_service)){
  df <- as.data.frame(by_service[i])
  colnames(df) <- c("service","Value","Percentage", "service_long") 
  cat(names(colnames(df)))
  total_percentage_p <- df %>%
    filter(Value %in% c("Informed", "Very Informed")) %>% #filters rows in column for categories to combine 
    group_by(Value,service,service_long) %>%# groupby to we can do summary stats to get sum of percentages 
    summarise(
      total_percentage = sum(Percentage, na.rm = TRUE),#gives us value only
      count = n(), #not aure what this does tbh need to check
      .groups = 'drop') %>% mutate(total_percentage = sum(total_percentage, na.rm = TRUE)) #drops our grouping and adds a total_percentage column 
  
  total_percentage_n <- df%>%
    filter(Value %in% c("Not Informed", "Somewhat Informed")) %>% #filters rows in column for categories to combine 
    group_by(Value,service,service_long) %>%# groupby to we can do summary stats to get sum of percentages 
    summarise(
      total_percentage = sum(Percentage, na.rm = TRUE),#gives us value only
      count = n(), #not aure what this does tbh need to check
      .groups = 'drop') %>% mutate(total_percentage = sum(total_percentage, na.rm = TRUE))
  
  final_percentage_p <- total_percentage_p[!duplicated(total_percentage_p$total_percentage), ]
  
  final_percentage_n <- total_percentage_n[!duplicated(total_percentage_n$total_percentage),]
  
  final_percentage_t <- rbind(final_percentage_p, final_percentage_n  )
  
  nom <- final_percentage_t$service[1]
  
  write.csv(final_percentage_t, glue("/Users/jdeen@middlebury.edu/Documents/Lib_Data/MISO24/Clean_Data/Final_clean/Proportion_tables_combined/CA_Students/Informed/{nom}_inf.csv"))
  
}





####Learn #####

CA_Faculty_Lrn <- read_csv("/Users/jdeen@middlebury.edu/Documents/Lib_Data/MISO24/Clean_Data/Final_clean/CA_Faculty/CA_Faculty_lrn.csv")

by_service <- split(CA_Faculty_Lrn, f = CA_Faculty_Lrn$service )

for(i in seq_along(by_service)){
  df <- as.data.frame(by_service[i])
  colnames(df) <- c("service","Value","Percentage", "service_long") 
  cat(names(colnames(df)))
  total_percentage_p <- df %>%
    filter(Value %in% c("Very Interested", "Interested")) %>% #filters rows in column for categories to combine 
    group_by(Value,service,service_long) %>%# groupby to we can do summary stats to get sum of percentages 
    summarise(
      total_percentage = sum(Percentage, na.rm = TRUE),#gives us value only
      count = n(), #not aure what this does tbh need to check
      .groups = 'drop') %>% mutate(total_percentage = sum(total_percentage, na.rm = TRUE)) #drops our grouping and adds a total_percentage column 
  
  total_percentage_n <- df%>%
    filter(Value %in% c("Not Interested", "Somewhat Interested")) %>% #filters rows in column for categories to combine 
    group_by(Value,service,service_long) %>%# groupby to we can do summary stats to get sum of percentages 
    summarise(
      total_percentage = sum(Percentage, na.rm = TRUE),#gives us value only
      count = n(), #not aure what this does tbh need to check
      .groups = 'drop') %>% mutate(total_percentage = sum(total_percentage, na.rm = TRUE))
  
  final_percentage_p <- total_percentage_p[!duplicated(total_percentage_p$total_percentage), ]
  
  final_percentage_n <- total_percentage_n[!duplicated(total_percentage_n$total_percentage),]
  
  final_percentage_t <- rbind(final_percentage_p, final_percentage_n  )
  
  nom <- final_percentage_t$service[1]
  
  write.csv(final_percentage_t, glue("/Users/jdeen@middlebury.edu/Documents/Lib_Data/MISO24/Clean_Data/Final_clean/Proportion_tables_combined/CA_Faculty/Learn/{nom}_inf.csv"))
  
}


