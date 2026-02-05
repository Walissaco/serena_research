
#plot 1

library(ggplot2)
library(dplyr)
library(tidyr)

# Prepare data with both PAO and THA counts, remove NA
plot_data <- data %>%
  filter(!is.na(`Amount of hips that underwent PAO`),
         !is.na(`Amount of hips that underwent THA of all the hips that underwent PAO`)) %>%
  select(`First author`, Year,
         `Amount of hips that underwent PAO`,
         `Amount of hips that underwent THA of all the hips that underwent PAO`) %>%
  mutate(Study = paste(`First author`, "(", Year, ")")) %>%
  pivot_longer(cols = c(`Amount of hips that underwent PAO`,
                        `Amount of hips that underwent THA of all the hips that underwent PAO`),
               names_to = "Type",
               values_to = "Count")

# Plot grouped bars
ggplot(plot_data, aes(x = Study, y = Count, fill = Type)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(values = c(
    "Amount of hips that underwent PAO" = "lightblue",
    "Amount of hips that underwent THA of all the hips that underwent PAO" = "orange"
  ),
  labels = c(
    "Amount of hips that underwent PAO" = "Total PAO hips",
    "Amount of hips that underwent THA of all the hips that underwent PAO" = "Converted to THA"
  )) +
  labs(
    title = "Conversion Rates per study",
    x = "Study",
    y = "Number of hips",
    fill = "",
    caption = "Light blue bars show the total number of hips that underwent PAO in each study.
Orange bars show the number of hips that were later converted to THA.
Comparing the two bars per study illustrates the conversion rate."
  ) +
  theme_minimal(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(face = "bold", hjust = 0.5),
    plot.caption = element_text(hjust = 0, size = 10, color = "gray40")
  )

#_________________________________________________________________________________


#plot 2
library(ggplot2)
library(dplyr)

# Recalculate conversion rate and remove NA in Mean Age
clean_data <- data %>%
  mutate(ConversionRate = (`Amount of hips that underwent THA of all the hips that underwent PAO` /
                             `Amount of hips that underwent PAO`) * 100) %>%
  filter(!is.na(`Mean (age)`),
         !is.na(ConversionRate),
         !is.na(`Amount of hips that underwent PAO`),
         !is.na(`Amount of hips that underwent THA of all the hips that underwent PAO`))

# Bubble chart
ggplot(clean_data, aes(x = ConversionRate,
                       y = `Mean (age)`,
                       size = `Amount of hips that underwent PAO`)) +
  geom_point(alpha = 0.7, color = "orange") +
  labs(
    title = "Mean Age vs THA Conversion Rate",
    x = "THA Conversion Rate (%)",
    y = "Mean Age (years)",
    size = "Sample Size (PAO hips)",
    caption = "Each bubble represents a study. 
Orange bubbles show the relationship between mean patient age at surgery and the percentage of hips converted to THA. 
Bubble size corresponds to the number of hips included in the study."
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    plot.caption = element_text(hjust = 0, size = 10, color = "gray40")
  )

#_______________________________________________________________________________

#plot3
library(ggplot2)
library(dplyr)

# Clean dataset: remove NA in Mean Age and Mean Time to THA
clean_data3 <- data %>%
  filter(!is.na(`Mean (age)`),
         !is.na(`Time (years) conversion THA (mean)`),
         !is.na(`Amount of hips that underwent PAO`))

# Bubble chart: Mean Age vs Mean Time to THA
ggplot(clean_data3, aes(x = `Time (years) conversion THA (mean)`,
                        y = `Mean (age)`,
                        size = `Amount of hips that underwent PAO`)) +
  geom_point(alpha = 0.7, color = "orange") +
  labs(
    title = "Mean Age vs Mean Time Conversion to THA",
    x = "Mean Time to THA (years)",
    y = "Mean Age (years)",
    size = "Sample Size (PAO hips)",
    caption = "Each bubble represents a study. 
Orange bubbles show the relationship between mean patient age at surgery and the average time until THA conversion. 
Bubble size corresponds to the number of hips included in the study."
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    plot.caption = element_text(hjust = 0, size = 10, color = "gray40")
  )

#_________________________________________________________________________

#plot 4

library(ggplot2)
library(dplyr)

# Clean dataset: calculate conversion rate and remove NA
clean_data4 <- data %>%
  mutate(ConversionRate = (`Amount of hips that underwent THA of all the hips that underwent PAO` /
                             `Amount of hips that underwent PAO`) * 100) %>%
  filter(!is.na(`Mean (years) follow-up`),
         !is.na(ConversionRate),
         !is.na(`Amount of hips that underwent PAO`),
         !is.na(`Amount of hips that underwent THA of all the hips that underwent PAO`))

# Bubble chart: Mean Follow-up vs THA Conversion Rate
ggplot(clean_data4, aes(x = ConversionRate,
                        y = `Mean (years) follow-up`,
                        size = `Amount of hips that underwent PAO`)) +
  geom_point(alpha = 0.7, color = "orange") +
  labs(
    title = "Mean Follow-up Duration vs THA Conversion Rate",
    x = "THA Conversion Rate (%)",
    y = "Mean Follow-up Period (years)",
    size = "Sample Size (PAO hips)",
    caption = "Each bubble represents a study. 
Orange bubbles show the relationship between mean follow-up duration and the percentage of hips converted to THA. 
Bubble size corresponds to the number of hips included in the study."
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    plot.caption = element_text(hjust = 0, size = 10, color = "gray40")
  )

#______________________________________________________________________________

#plot: 5
library(ggplot2)
library(dplyr)
library(tidyr)

# Reshape dataset: THA counts per Tönnis grade
tonnis_data <- data %>%
  select(`First author`, Year, `Mean (age)`,
         `Amount of hips that underwent PAO`,
         `Amount of hips that underwent THA of all the hips that underwent PAO`,
         `Tonnis grade 0 (amount) under THA group`,
         `Tonnis grade 1 (amount) under THA group`,
         `Tonnis grade 2 (amount) under THA group`,
         `Tonnis grade 3 (amount) under THA group`) %>%
  pivot_longer(cols = starts_with("Tonnis grade"),
               names_to = "TonnisGrade",
               values_to = "THA_Count") %>%
  filter(!is.na(`Mean (age)`), !is.na(THA_Count), THA_Count > 0) %>%
  mutate(
    Study = paste(`First author`, "(", Year, ")"),
    ConversionRate = (`Amount of hips that underwent THA of all the hips that underwent PAO` /
                        `Amount of hips that underwent PAO`) * 100,   # overall conversion rate per study
    Grade = case_when(
      grepl("grade 0", TonnisGrade) ~ "0",
      grepl("grade 1", TonnisGrade) ~ "1",
      grepl("grade 2", TonnisGrade) ~ "2",
      grepl("grade 3", TonnisGrade) ~ "3"
    )
  )

# Bubble chart: Mean Age vs Conversion Rate, stratified by grade
ggplot(tonnis_data, aes(x = ConversionRate,
                        y = `Mean (age)`,
                        size = THA_Count,
                        color = Grade)) +
  geom_point(alpha = 0.7) +
  scale_color_manual(values = c("0" = "lightblue",
                                "1" = "orange",
                                "2" = "darkgreen",
                                "3" = "purple")) +
  labs(
    title = "Mean Age vs Conversion Rate per Tönnis Grade",
    x = "Overall THA Conversion Rate (%)",
    y = "Mean Age (years)",
    size = "THA Conversions (per grade)",
    color = "Tönnis Grade",
    caption = "Each bubble represents THA conversions stratified by Tönnis grade. 
Bubble size corresponds to the number of hips converted in that grade. 
Colors distinguish Tönnis grades (0–3). X-axis shows the overall study-level conversion rate."
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    plot.caption = element_text(hjust = 0, size = 10, color = "gray40")
  )

#_____________________________________________________ 

#plot:6
library(ggplot2)
library(dplyr)
library(tidyr)

# Reshape dataset: THA counts per Tönnis grade
tonnis_data6 <- data %>%
  select(`First author`, Year, `Mean (age)`,
         `Amount of hips that underwent PAO`,
         `Amount of hips that underwent THA of all the hips that underwent PAO`,
         `Tonnis grade 0 (amount) under THA group`,
         `Tonnis grade 1 (amount) under THA group`,
         `Tonnis grade 2 (amount) under THA group`,
         `Tonnis grade 3 (amount) under THA group`) %>%
  pivot_longer(cols = starts_with("Tonnis grade"),
               names_to = "TonnisGrade",
               values_to = "THA_Count") %>%
  filter(!is.na(`Mean (age)`), !is.na(THA_Count), THA_Count > 0) %>%
  mutate(
    Study = paste(`First author`, "(", Year, ")"),
    ConversionRate = (`Amount of hips that underwent THA of all the hips that underwent PAO` /
                        `Amount of hips that underwent PAO`) * 100,   # overall conversion rate per study
    Grade = case_when(
      grepl("grade 0", TonnisGrade) ~ "0",
      grepl("grade 1", TonnisGrade) ~ "1",
      grepl("grade 2", TonnisGrade) ~ "2",
      grepl("grade 3", TonnisGrade) ~ "3"
    )
  )

# Scatter plot: Mean Age vs Conversion Rate, stratified by grade
ggplot(tonnis_data6, aes(x = `Mean (age)`, y = ConversionRate, color = Grade)) +
  geom_point(size = 3, alpha = 0.8) +
  scale_color_manual(values = c("0" = "lightblue",
                                "1" = "orange",
                                "2" = "darkgreen",
                                "3" = "purple")) +
  labs(
    title = "Study-level Association between Mean Age and THA Conversion Rate after PAO, Stratified by Preoperative Tönnis Grade",
    x = "Mean Age at Surgery (years)",
    y = "THA Conversion Rate (%)",
    color = "Tönnis Grade",
    caption = "Each point represents a study stratified by Tönnis grade. 
Colors distinguish Tönnis grades (0–3). X-axis shows mean age at surgery, Y-axis shows overall study-level conversion rate."
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    plot.caption = element_text(hjust = 0, size = 10, color = "gray40")
  )

#_____________________________________________________________________________________________

#plot 7

library(ggplot2)
library(dplyr)
library(tidyr)

#Create age categories from mean age
data7 <- data %>%
  filter(!is.na(`Mean (age)`),
         !is.na(`Amount of hips that underwent PAO`),
         !is.na(`Amount of hips that underwent THA of all the hips that underwent PAO`)) %>%
  mutate(
    AgeCategory = case_when(
      `Mean (age)` < 30 ~ "25–30",
      `Mean (age)` >= 30 & `Mean (age)` < 34 ~ "30–34",
      `Mean (age)` >= 34 & `Mean (age)` < 39 ~ "34–39",
      `Mean (age)` >= 39 & `Mean (age)` <= 43 ~ "39–43",
      TRUE ~ ">43"
    ),
    ConversionRate = (`Amount of hips that underwent THA of all the hips that underwent PAO` /
                        `Amount of hips that underwent PAO`) * 100   # calculate conversion rate per study
  ) %>%
  # Order age categories from lower to higher
  mutate(AgeCategory = factor(AgeCategory,
                              levels = c("25–30", "30–34", "34–39", "39–43", ">43")))

#Reshape Tönnis grade columns (THA counts per grade)
tonnis_data7 <- data7 %>%
  select(`First author`, Year, AgeCategory,
         `Amount of hips that underwent PAO`,
         `Amount of hips that underwent THA of all the hips that underwent PAO`,
         ConversionRate,
         `Tonnis grade 0 (amount) under THA group`,
         `Tonnis grade 1 (amount) under THA group`,
         `Tonnis grade 2 (amount) under THA group`,
         `Tonnis grade 3 (amount) under THA group`) %>%
  pivot_longer(cols = starts_with("Tonnis grade"),
               names_to = "TonnisGrade",
               values_to = "THA_Count") %>%
  filter(!is.na(THA_Count)) %>%
  mutate(
    Grade = case_when(
      grepl("grade 0", TonnisGrade) ~ "0",
      grepl("grade 1", TonnisGrade) ~ "1",
      grepl("grade 2", TonnisGrade) ~ "2",
      grepl("grade 3", TonnisGrade) ~ "3"
    )
  )

#Heatmap 
ggplot(tonnis_data7, aes(x = Grade, y = AgeCategory, fill = ConversionRate)) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = "lightblue", high = "darkred", name = "Conversion Rate (%)") +
  labs(
    title = "Heatmap of THA Conversion Rates by Mean Study Age and Preoperative Tönnis Grade",
    x = "Tönnis Grade (0–3)",
    y = "Mean Age Category (years)",
    caption = "Heatmap shows THA conversion rates across age categories and Tönnis grades. 
Darker colors indicate higher conversion rates."
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    plot.caption = element_text(hjust = 0, size = 10, color = "gray40"),
    axis.text.x = element_text(face = "bold"),
    axis.text.y = element_text(face = "bold")
  )