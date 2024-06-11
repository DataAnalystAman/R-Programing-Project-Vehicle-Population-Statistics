# Load necessary libraries
library(tidyverse)
library(ggplot2)
library(dplyr)
library(tidyr)

# Set Directory
setwd('C:/Users/Jithin Jayachandran/Downloads/R')

# Load Data
df <- read.csv('Vehicle Population Statistics.csv')
View(df)

# Display the structure of the dataset
str(df)

# Display the first few rows of the dataset
head(df)

# Display summary statistics of the dataset
summary(df)

# Handling

# Display the column names of the DataFrame
column_names <- names(df)
print(column_names)

# List of columns to check where values are zero
columns_to_check <- c("Motor.Cycles", "Scooters", "Moped", "Cars", "Jeeps", "Stn..Wagons", 
                      "Taxis.meter.fited", "Luxury..Turist.Cabs.", "Auto.rikshaws", 
                      "Stage.carriages", "Contract.carriages..Mini.Bus", "School.Buses", 
                      "Private.Service.Vehicles", "Ambulances", "Articulated.Multi.", 
                      "Trucks...Lorries", "Tanker", "Delivery.Van..4.wheelers.", 
                      "Delivery.Van..3.wheelers.", "Tractors", "Trailors", "Others")

# Delete rows where all specified columns are zero
df <- df %>%
  filter(!(rowSums(select(., all_of(columns_to_check)) == 0) == length(columns_to_check)))

# Display the updated DataFrame
print("Updated DataFrame:")
print(df)

View(df)

# List of columns containing vehicle types
columns_to_check <- c("Motor.Cycles", "Scooters", "Moped", "Cars", "Jeeps", "Stn..Wagons", 
                      "Taxis.meter.fited", "Luxury..Turist.Cabs.", "Auto.rikshaws", 
                      "Stage.carriages", "Contract.carriages..Mini.Bus", "School.Buses", 
                      "Private.Service.Vehicles", "Ambulances", "Articulated.Multi.", 
                      "Trucks...Lorries", "Tanker", "Delivery.Van..4.wheelers.", 
                      "Delivery.Van..3.wheelers.", "Tractors", "Trailors", "Others")

# Ensure column names match exactly with the names in the data frame
columns_to_check <- intersect(columns_to_check, colnames(df))



# Reshape data to long format for plotting
df_long <- df %>%
  pivot_longer(cols = all_of(columns_to_check), names_to = "Vehicle_Type", values_to = "Count") %>%
  filter(Count > 0)  # Remove rows with zero count

# Data Analysis

#1. Plot the distribution of vehicle types
ggplot(df_long, aes(x=Vehicle_Type, y=Count)) +
  geom_bar(stat="identity") +
  labs(title="Distribution of Vehicle Types", x="Vehicle Type", y="Count") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


#2. What is the trend of vehicle registration over the years?
  ggplot(df, aes(x=Year)) +
  geom_bar() +
  labs(title="Trend of Vehicle Registration Over the Years", x="Year", y="Count") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


#3. How does the vehicle population vary across different regions?
  ggplot(df, aes(x=Region, fill=Region)) +
    geom_bar() +
    labs(title="Distribution of Vehicle Population by Region", x="Region", y="Count") +
    theme(axis.text.x = element_text(angle = 90, hjust = 1))
  

#4. What is the distribution of vehicle population by category?
  ggplot(df, aes(x=Category, fill=Category)) +
    geom_bar() +
    labs(title="Vehicle Population by Category", x="Category", y="Count") +
    theme(axis.text.x = element_text(angle = 90, hjust = 1))
  

#5. Distrubtuion of Motorcycles, Scooters and Mopeds
    df %>%
    select(Motor.Cycles, Scooters, Moped) %>%
    pivot_longer(cols = everything(), names_to = "Vehicle_Type", values_to = "Count") %>%
    ggplot(aes(x = Vehicle_Type, y = Count)) +
    geom_bar(stat = "identity", fill = "skyblue") +
    labs(title = "Distribution of Motorcycles, Scooters, and Mopeds", x = "Vehicle Type", y = "Count") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))


#6. What is the total vehicle population per year?
    # Total vehicle population per year
    df %>%
      group_by(Year) %>%
      summarise(Total_Vehicles = sum(across(starts_with("Motor") | starts_with("Scooters") | starts_with("Moped") | 
                                              starts_with("Cars") | starts_with("Jeeps") | starts_with("Stn.") | 
                                              starts_with("Taxis") | starts_with("Luxury") | starts_with("Auto") | 
                                              starts_with("Stage") | starts_with("Contract") | starts_with("School") | 
                                              starts_with("Private") | starts_with("Ambulances") | starts_with("Articulated") | 
                                              starts_with("Trucks") | starts_with("Tanker") | starts_with("Delivery") | 
                                              starts_with("Tractors") | starts_with("Trailors") | starts_with("Others")))) %>%
      ggplot(aes(x=Year, y=Total_Vehicles, group=1)) +
      geom_line(color="blue") +
      geom_point(color="red") +
      labs(title="Total Vehicle Population Per Year", x="Year", y="Total Vehicles") +
      theme_minimal()
    

#7. What is the trend of a specific vehicle type (e.g., Cars) over the years?
# Trend of Cars over the years
    ggplot(df, aes(x=Year, y=Cars, group=1)) +
      geom_line(color="green") +
      geom_point(color="blue") +
      labs(title="Trend of Cars Over the Years", x="Year", y="Number of Cars") +
      theme_minimal()
    

#8 Compare the vehicle population of two regions (Greater Mumbai and Pune Region) over the years.
    # This covers entire cities inside Mumbai and Pune Districts in dataset
    
    # Ensure only numeric columns are selected for calculating the total vehicle population
    numeric_columns <- c("Motor.Cycles", "Scooters", "Moped", "Cars", "Jeeps", "Stn..Wagons", 
                         "Taxis.meter.fited", "Luxury..Turist.Cabs.", "Auto.rikshaws", 
                         "Stage.carriages", "Contract.carriages..Mini.Bus", "School.Buses", 
                         "Private.Service.Vehicles", "Ambulances", "Articulated.Multi.", 
                         "Trucks...Lorries", "Tanker", "Delivery.Van..4.wheelers.", 
                         "Delivery.Van..3.wheelers.", "Tractors", "Trailors", "Others")
    
    # Define the regions to compare
    regions_to_compare <- c("Greater Mumbai", "Pune Region")
    
    # Filter and summarize the data for the specified regions
    df %>%
      filter(Region %in% regions_to_compare) %>%
      group_by(Year, Region) %>%
      summarise(Total_Vehicles = sum(across(all_of(numeric_columns)), na.rm = TRUE)) %>%
      ggplot(aes(x = Year, y = Total_Vehicles, color = Region, group = Region)) +
      geom_line(size = 1.2) +
      geom_point(size = 3) +
      scale_color_manual(values = c("Greater Mumbai" = "blue", "Pune Region" = "red")) +
      labs(title = "Vehicle Population of Greater Mumbai and Pune Region Over the Years", x = "Year", y = "Total Vehicles") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    

#9. Which city has the highest vehicle population?
      
    # Summarize total vehicle population by category
    category_summary <- df %>%
      group_by(Category) %>%
      summarise(Total_Vehicles = sum(across(c("Motor.Cycles", "Scooters", "Moped", "Cars", "Jeeps", "Stn..Wagons", 
                                              "Taxis.meter.fited", "Luxury..Turist.Cabs.", "Auto.rikshaws", 
                                              "Stage.carriages", "Contract.carriages..Mini.Bus", "School.Buses", 
                                              "Private.Service.Vehicles", "Ambulances", "Articulated.Multi.", 
                                              "Trucks...Lorries", "Tanker", "Delivery.Van..4.wheelers.", 
                                              "Delivery.Van..3.wheelers.", "Tractors", "Trailors", "Others")), na.rm = TRUE)) %>%
      arrange(desc(Total_Vehicles))
    
    # Plot the data
    ggplot(category_summary, aes(x = reorder(Category, -Total_Vehicles), y = Total_Vehicles)) +
      geom_bar(stat = "identity", fill = "skyblue") +
      labs(title = "Total Vehicle Population by City", x = "Cities", y = "Total Vehicles") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    

#10. What is the trend of each vehicle type over the years?
  
    # Reshape data to long format for plotting
    df_long <- df %>%
      pivot_longer(cols = all_of(numeric_columns), names_to = "Vehicle_Type", values_to = "Count") %>%
      filter(Count > 0)  # Remove rows with zero count
    
    # Plot the trend of each vehicle type over the years
    ggplot(df_long, aes(x = Year, y = Count, color = Vehicle_Type)) +
      geom_line() +
      facet_wrap(~ Vehicle_Type, scales = "free_y") +
      labs(title = "Trend of Each Vehicle Type Over the Years", x = "Year", y = "Count") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    

    
#11. # Summarize the total number of vehicles in the dataset based on vehicle type
    df_long <- df %>%
      pivot_longer(cols = c("Motor.Cycles", "Scooters", "Moped", "Cars", "Jeeps", "Stn..Wagons",
                            "Taxis.meter.fited", "Luxury..Turist.Cabs.", "Auto.rikshaws",
                            "Stage.carriages", "Contract.carriages..Mini.Bus", "School.Buses",
                            "Private.Service.Vehicles", "Ambulances", "Articulated.Multi.",
                            "Trucks...Lorries", "Tanker", "Delivery.Van..4.wheelers.",
                            "Delivery.Van..3.wheelers.", "Tractors", "Trailors", "Others"),
                   names_to = "Vehicle_Type", values_to = "Count") %>%
      filter(!is.na(Count))  # Remove NA values
    
    # Summarize the total number of vehicles based on vehicle type
    total_vehicles_by_type <- df_long %>%
      group_by(Vehicle_Type) %>%
      summarise(Total_Vehicles = sum(Count))
    
    # Print the summary of total vehicles by vehicle type
    print(total_vehicles_by_type)
    
    # Plot the total number of vehicles by vehicle type
    ggplot(total_vehicles_by_type, aes(x = reorder(Vehicle_Type, Total_Vehicles), y = Total_Vehicles)) +
      geom_bar(stat = "identity", fill = "skyblue") +
      labs(title = "Total Number of Vehicles by Vehicle Type", x = "Vehicle Type", y = "Total Vehicles") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
    

#12. What are the top 5 regions with the highest vehicle registration counts, and what conclusions can be drawn from this analysis?    
    
    category_registrations <- df %>%
      group_by(Category) %>%
      summarise(Total_Registrations = sum(Motor.Cycles, Scooters, Moped, Cars, Jeeps, Stn..Wagons,
                                          Taxis.meter.fited, Luxury..Turist.Cabs., Auto.rikshaws,
                                          Stage.carriages, Contract.carriages..Mini.Bus, School.Buses,
                                          Private.Service.Vehicles, Ambulances, Articulated.Multi.,
                                          Trucks...Lorries, Tanker, Delivery.Van..4.wheelers.,
                                          Delivery.Van..3.wheelers., Tractors, Trailors, Others, na.rm = TRUE)) %>%
      arrange(desc(Total_Registrations)) %>%
      top_n(5)
    
    # Print the top 5 categories with the highest vehicle registration counts
    print(category_registrations)
    
    # Plot the top 5 categories with the highest vehicle registration counts
    ggplot(category_registrations, aes(x = reorder(Category, Total_Registrations), y = Total_Registrations, fill = Category)) +
      geom_bar(stat = "identity") +
      labs(title = "Top 5 Categories with Highest Vehicle Registration Counts",
           x = "Category", y = "Total Registrations") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
    

#Thanks