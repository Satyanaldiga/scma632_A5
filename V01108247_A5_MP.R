# Set the working directory and verify it
setwd('C:\\Users\\SPURGE\\Desktop\\SCMA\\data')
getwd()
# Function to install and load libraries
install_and_load <- function(package) {
  if (!require(package, character.only = TRUE)) {
    install.packages(package, dependencies = TRUE)
    library(package, character.only = TRUE)
  }
}

libraries <- c("dplyr", "readr", "readxl", "tidyr", "ggplot2", "BSDA","glue")
lapply(libraries, install_and_load)

# Reading the file into R
data <- read.csv("NSSO68.csv")

# Filtering for Madhya pradesh data
df <- data %>%
  filter(state_1 == "MP")

# Display dataset info
cat("Dataset Information:\n")
print(dim(df))

# Finding missing values
missing_info <- colSums(is.na(df))

# Sub-setting the data
MPData <- df %>%
  select(state_1, District, Region, Sector, State_Region, Meals_At_Home, ricepds_v, Wheatpds_q, chicken_q, pulsep_q, wheatos_q, No_of_Meals_per_day)

# Check for missing values in the subset
cat("Missing Values in Subset:\n")
print(colSums(is.na(MPData)))


# HANDLING MISSING VALUES
# Impute missing values with mean for specific columns
impute_with_mean <- function(column) {
  if (any(is.na(column))) {
    column[is.na(column)] <- mean(column, na.rm = TRUE)
  }
  return(column)
}

MPData$Meals_At_Home <- impute_with_mean(MPData$Meals_At_Home)
MPData$No_of_Meals_per_day <- impute_with_mean(MPData$No_of_Meals_per_day)


# Check for missing values after imputation
cat("Missing Values After Imputation:\n")
print(colSums(is.na(MPData)))


# CHECK FOR OUTLIERS
remove_outliers <- function(df, column_name) {
  Q1 <- quantile(df[[column_name]], 0.25)
  Q3 <- quantile(df[[column_name]], 0.75)
  IQR <- Q3 - Q1
  lower_threshold <- Q1 - (1.5 * IQR)
  upper_threshold <- Q3 + (1.5 * IQR)
  df <- subset(df, df[[column_name]] >= lower_threshold & df[[column_name]] <= upper_threshold)
  return(df)
}


outlier_columns <- c("ricepds_v", "chicken_q")
for (col in outlier_columns) {
  MPData <- remove_outliers(MPData, col)
}

# RENAME DISTRICTS AND SECTORS USING CODES FROM APPENDIX OF NSSA 68TH ROUND DATA
sector_mapping <- c("2" = "URBAN", "1" = "RURAL")

# Create a data frame for district codes and names
district_mapping_df <- data.frame(
  District_Code = c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13", "14", 
                    "15", "16", "17", "18", "19", "20", "21", "22", "23", "24", "25", "26", "27", "28", 
                    "29", "30", "31", "32", "33", "34", "35", "36", "37", "38","39","40","41","42","43","44","45","46
                    ","47","48","49","50"),
  District_Name = c("Sheopur", "Morena","Bhind", "Gwalior", "Datia", "Shivpuri", "Guna", "Tikamgarh", "Chhatarpur", "Panna",
                    "Sagar", "Damoh", "Satna", "Rewa", "Umaria", "Shahdol", "Sidhi", "Neemuch", "Mandsaur",
                    "Ratlam", "Ujjain", "Shajapur", "Dewas", "Jhabua", "Dhar", "Indore", "West Nimar", "Barwani",
                    "East Nimar", "Rajgarh", "Vidisha", "Bhopal", "Sehore", "Raisen", "Betul", "Harda",
                    "Hoshangabad", "Katni", "Jabalpur", "Narsimhapur", "Dindori", "Mandla", "Chhindwara",
                    "Seoni", "Balaghat", "Ashoknagar", "Anuppur", "Burhanpur", "Alirajpur", "Singrauli")

)

MPData$District <- as.character(MPData$District)
MPData$Sector <- as.character(MPData$Sector)
MPData$District <- ifelse(MPData$District %in% names(district_mapping_df), district_mapping_df[MPData$District], MPData$District)
MPData$Sector <- ifelse(MPData$Sector %in% names(sector_mapping), sector_mapping[MPData$Sector], MPData$Sector)


# Merge MPData with the district mapping data frame
MPData <- MPData %>%
  left_join(district_mapping_df, by = c("District" = "District_Code"))

# Replace the old District column with the new District_Name column
MPData <- MPData %>%
  select(-District) %>%
  rename(District = District_Name)

# Check the mapping
print(unique(MPData$District))

#SUMMARIZING VARIABLES
MPData$total_consumption <- rowSums(MPData[, c("ricepds_v", "Wheatpds_q", "chicken_q", "pulsep_q", "wheatos_q")], na.rm = TRUE)


# Summarize and display top and bottom consuming districts and regions
summarize_consumption <- function(group_col) {
  summary <- MPData %>%
    group_by(across(all_of(group_col))) %>%
    summarise(total = sum(total_consumption)) %>%
    arrange(desc(total))
  return(summary)
}

district_summary <- summarize_consumption("District")
region_summary <- summarize_consumption("Region")


# DISPLAYING TOP AND BOTTOM 3 DISTRICTS OF CONSUMPTION
cat("Top 3 Consuming Districts:\n")
print(head(district_summary, 3))
cat("Bottom 3 Consuming Districts:\n")
print(tail(district_summary, 3))



region_summary$Region <- ifelse(region_summary$Region == 1, "RURAL", "URBAN")

cat("Region Consumption Summary:\n")
print(region_summary)

View(MPData)


#A5)Visualization 
#A)histogram to show the distribution of total consumption across different districts in Bihar 
hist(MPData$total_consumption, breaks = 10, col = 'red', border = 'black', 
     xlab = "Consumption", ylab = "Frequency", main = "Consumption Distribution in MADHYA PRADESH State")

# Create a ggplot histogram with enhancements
ggplot(MPData, aes(x = total_consumption)) +
  geom_histogram(aes(y = ..density..), bins = 10, fill = 'blue', color = 'black', alpha = 0.7) +
  geom_density(alpha = 0.2, fill = "#FF6666") +
  labs(
    title = "Distribution of Total Consumption Across Districts in MADHYA PRADESH",
    x = "Total Consumption",
    y = "Density"
  ) +
  theme_minimal(base_size = 15) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 18),
    axis.title = element_text(face = "bold"),
    axis.text = element_text(color = "black"),
    panel.grid.major = element_line(color = "grey80"),
    panel.grid.minor = element_blank()
  ) +
  scale_x_continuous(breaks = seq(0, max(MPData$total_consumption, na.rm = TRUE), by = 500)) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
  geom_vline(aes(xintercept = mean(total_consumption, na.rm = TRUE)), color = "red", linetype = "dashed", size = 1) +
  annotate("text", x = mean(MPData$total_consumption, na.rm = TRUE), y = 0.0007, label = "Mean", color = "red", size = 4, hjust = -0.1)


mp_consumption <- aggregate(total_consumption ~ District, data = MPData, sum) 
View(mp_consumption)


#barplot To visualize consumption per district with district names in madhya pradesh

# Sorting the Bhr_consumption data frame by total_consumption
mp_consumption <- mp_consumption[order(mp_consumption$total_consumption, decreasing = TRUE), ]

barplot(mp_consumption$total_consumption, 
        names.arg = mp_consumption$District, 
        las = 2, # Makes the district names vertical
        col = 'blue', 
        border = 'black', 
        xlab = "District", 
        ylab = "Total Consumption", 
        main = "Total Consumption per District",
        cex.names = 0.7)


# Create a ggplot bar plot 
ggplot(mp_consumption, aes(x = reorder(District, total_consumption), y = total_consumption)) +
  geom_bar(stat = "identity", aes(fill = total_consumption), color = 'black', alpha = 0.8) +
  scale_fill_gradient(low = "skyblue", high = "steelblue") +
  coord_flip() +
  labs(
    title = "Total Consumption per District in madhya pradesh",
    x = "District",
    y = "Total Consumption",
    caption = "Data Source: NSSO68"
  ) +
  theme_classic(base_size = 15) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 18),
    axis.title = element_text(face = "bold"),
    axis.text = element_text(color = "black", size = 12),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    plot.caption = element_text(hjust = 0, size = 10, face = "italic")
  ) +
  geom_text(aes(label = round(total_consumption, 1)), hjust = -0.2, size = 4, color = "black", fontface = "bold") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1)))



#B)Plot Total_consumption and Meals_at_home on the madhya pradesh state map

#install.packages("sf")
library(ggplot2) 
library(sf) 
library(dplyr) 
Sys.setenv("SHAPE_RESTORE_SHX" = "YES") 


data_map <- st_read("C:\\Users\\SPURGE\\Desktop\\SCMA\\MADHYA PRADESH_DISTRICTS.geojson") 
View(data_map)


data_map <- data_map %>% 
  rename(District = dtname) 
colnames(data_map) 

data_map_data <- merge(mp_consumption,data_map,by = "District") 
View(data_map_data)


ggplot(data_map_data) + 
  geom_sf(aes(fill = total_consumption, geometry = geometry), color = "white", lwd = 0.2) + 
  scale_fill_gradient(low = "lightblue", high = "red", name = "Total Consumption") + 
  labs(
    title = "Total Consumption by District in madhya pradesh",
    subtitle = "Data from NSSO68",
    caption = "Source: NSSO68"
  ) +
  geom_sf_text(aes(label = District, geometry = geometry), size = 3, color = "black", fontface = "bold") +
  theme_minimal(base_size = 15) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 18, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, size = 12, face = "italic"),
    plot.caption = element_text(hjust = 0, size = 10, face = "italic"),
    legend.position = "right",
    legend.title = element_text(size = 12, face = "bold"),
    legend.text = element_text(size = 10)
  )



# Aggregate the Meals_At_Home data by district
mp_meals <- aggregate(Meals_At_Home ~ District, data = MPData, mean) 

# Merge the aggregated data with the data_map data frame
data_map_meals <- merge(mp_meals, data_map, by = "District") 
View(data_map_meals)

# Plot the map with the Meals_At_Home variable
ggplot(data_map_meals) + 
  geom_sf(aes(fill = Meals_At_Home, geometry = geometry)) + 
  scale_fill_gradient(low = "yellow", high = "red") + 
  ggtitle("Average Meals at Home by District in Madhya pradesh") +
  geom_sf_text(aes(label = District, geometry = geometry), size = 3, color = "black")
