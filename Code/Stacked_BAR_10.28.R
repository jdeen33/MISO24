library(data.table)
library(dplyr)
library(tidyverse)
library(ggplot2)
library(hrbrthemes)
library(glue)




files <- list.files("/Users/jdeen@middlebury.edu/Documents/Lib_Data/MISO24/Clean_Data/Final_clean/Proportion_tables_combined/VT_Students/Satisfaction", 
                    pattern = ".csv$", recursive = TRUE, full.names = TRUE)

##more data re arranging 

for (i in seq(1, length(files), by = 4)){ #GPT
  current_files <- files[i:min(i + 3, length(files))]
  
  # Read and concatenate the current batch
  batch_data <- do.call(rbind, lapply(current_files, read.csv))
  
  # Combine with the overall dataframe
  write_csv(batch_data,glue("/Users/jdeen@middlebury.edu/Documents/Lib_Data/MISO24/Clean_Data/Final_clean/Proportion_tables_combined/VT_Students/Satisfaction/{i}.csv"))
}



#Importance####

files <- list.files("/Users/jdeen@middlebury.edu/Documents/Lib_Data/MISO24/Clean_Data/Final_clean/Proportion_tables_combined/CA_Faculty/Importance", 
                    pattern = "[1-9].csv$", recursive = TRUE, full.names = TRUE)

for (i in 1:length(files)){
  df <- read_csv(files[i])
  df$Value <- factor(df$Value, levels=c("Important", "Not Important"))
  plot1<- ggplot(df, aes(fill=Value, x=total_percentage ,y= service_long)) + 
    geom_bar(position= position_stack(), stat="identity",color="black")+ scale_fill_brewer(palette = "YlGnBu")+ geom_text(aes(label=total_percentage,fontface = 'bold'), position=position_stack(vjust=0.5), size=3)+ggtitle('MIIS Faculty Importance')
  pdf(glue("/Users/jdeen@middlebury.edu/Documents/Lib_Data/MISO24/Viz_outputs/CA_Faculty/StackedBar/Importance/{i}.pdf"),width = 10, height = 5)
  print(plot1)
 dev.off()
}

#Satisfaction####

files <- list.files("/Users/jdeen@middlebury.edu/Documents/Lib_Data/MISO24/Clean_Data/Final_clean/Proportion_tables_combined/VT_Staff/Satisfaction", 
                    pattern = "[1-9].csv$", recursive = TRUE, full.names = TRUE)

for (i in 1:length(files)){
  df <- read_csv(files[i])
  df$Value <- factor(df$Value, levels=c("Satisfied", "Dissatisfied"))
  plot1<- ggplot(df, aes(fill=Value, x=total_percentage ,y= service_long)) + 
    geom_bar(position= position_stack(), stat="identity",color="black")+ scale_fill_brewer(palette = "PuBuGn")+ geom_text(aes(label=total_percentage,fontface = 'bold'), position=position_stack(vjust=0.5), size=3)+ggtitle('Vermont Staff Satisfaction')
  pdf(glue("/Users/jdeen@middlebury.edu/Documents/Lib_Data/MISO24/Viz_outputs/VT_Staff/StackedBar/Satisfaction/{i}.pdf"),width = 10, height = 5)
  print(plot1)
  dev.off()
}

#Learn####

path1<- "/Users/jdeen@middlebury.edu/Documents/Lib_Data/MISO24/Clean_Data/Final_clean/Proportion_tables_combined/VT_Students/Learn"
path2<- "/Users/jdeen@middlebury.edu/Documents/Lib_Data/MISO24/Viz_outputs/VT_Students/StackedBar/Learn"

files <- list.files(path1,pattern = "[1-9].csv$", recursive = TRUE, full.names = TRUE)

for (i in 1:length(files)){
  df <- read_csv(files[i])
  df$Value <- factor(df$Value, levels=c("Interested", "Not Interested"))
  plot1<- ggplot(df, aes(fill=Value, x=total_percentage ,y= service_long)) + 
    geom_bar(position= position_stack(), stat="identity",color="black")+ scale_fill_brewer(palette = "Greens")+ geom_text(aes(label=total_percentage,fontface = 'bold'), position=position_stack(vjust=0.5), size=3)+ggtitle("Vermont Students Learn")
  pdf(glue("{path2}/{i}.pdf"),width = 10, height = 5)
  print(plot1)
  dev.off()
}

#Informed ####
path1<- "/Users/jdeen@middlebury.edu/Documents/Lib_Data/MISO24/Clean_Data/Final_clean/Proportion_tables_combined/VT_Students/Informed "
path2<- "/Users/jdeen@middlebury.edu/Documents/Lib_Data/MISO24/Viz_outputs/VT_Students/StackedBar/Informed"

files <- list.files(path1,pattern = "[1-9].csv$", recursive = TRUE, full.names = TRUE)

for (i in 1:length(files)){
  df <- read_csv(files[i])
  df$Value <- factor(df$Value, levels=c("Informed", "Not Informed"))
  plot1<- ggplot(df, aes(fill=Value, x=total_percentage ,y= service_long)) + 
    geom_bar(position= position_stack(), stat="identity",color="black")+ scale_fill_brewer(palette = "Paired")+ geom_text(aes(label=total_percentage,fontface = 'bold'), position=position_stack(vjust=0.5), size=3)+ggtitle("VT Students Informed")
  pdf(glue("{path2}/{i}.pdf"),width = 10, height = 5)
  print(plot1)
  dev.off()
}



#Frequency ####

path1<- "/Users/jdeen@middlebury.edu/Documents/Lib_Data/MISO24/Clean_Data/Final_clean/VT_Students/VT_Students_bahr_freq.csv"
path2<- "/Users/jdeen@middlebury.edu/Documents/Lib_Data/MISO24/Viz_outputs/VT_Students/StackedBar/Frequency"
  
df<- read_csv(path1)
by_service <- split( df ,f = df$service) 

for (i in seq_along(by_service)){
  df_ <- as.data.frame(by_service[i])
  colnames(df_) <- c("service","Value","Percentage", "service_long")
  cat(names(colnames(df_)))
  plot1 <- ggplot(df_, aes(fill=Value, x=Percentage ,y= service_long)) + 
    geom_bar(position= position_stack(), stat="identity",color="black")+ scale_fill_brewer(palette = "PRGn")+ geom_text(aes(label= Percentage,fontface = 'bold'), position=position_stack(vjust=0.5), size=3)+ggtitle("VT Students Frequency")
  pdf(glue("{path2}/{i}.pdf"),width = 12, height = 5)
  print(plot1)
  dev.off()
}

test <- as.data.frame(by_service[1:4])
colnames(test) <- c("service","Value","Percentage", "service_long") 
cat(names(colnames(test)))

#test$Value <- factor(test$Value, levels=c("Never", "Once or twice per semester","Once or twice a week", "More than three times a week"))
ggplot(test, aes(fill=Value, x=Percentage ,y= service_long)) + 
  geom_bar(position= position_stack(), stat="identity",color="black")+ scale_fill_brewer(palette = "PRGn")+ geom_text(aes(label= Percentage,fontface = 'bold'), position=position_stack(vjust=0.5), size=3)+ggtitle("VT Students Informed")




