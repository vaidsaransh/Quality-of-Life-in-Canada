#installing and loading packages
install.packages("dplyr")
install.packages("lubridate")
install.packages("stringr")
install.packages("tidyr")
install.packages("plotly")
install.packages("rnaturalearth")
install.packages("rnaturalearthdata")
install.packages("ggmap")
install.packages("devtools")
install.packages("sf")
devtools::install_github("ropensci/rnaturalearthhires")

remove.packages("tidyverse")

library(plotly)
library(readr)
library(dplyr)
library(lubridate)
library(stringr)
library(ggplot2)
library(tidyr)
library(reshape2)
library(rnaturalearth)
library(sf)
library(rnaturalearthdata)
library(ggmap)


#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------

#importing data files into DFs
Prosperity_Poverty <- read_csv("/Users/saranshvaid/Desktop/Big Data Group Project/ProsPov.csv")
Prossperty_Unemployment <- read_csv("/Users/saranshvaid/Desktop/Big Data Group Project/ProsUnem.csv")
Health_Life <- read_csv("/Users/saranshvaid/Desktop/Big Data Group Project/HealLif.csv")
Health_Mental <- read_csv("/Users/saranshvaid/Desktop/Big Data Group Project/HealMen.csv")
Social_Belonging <- read_csv("/Users/saranshvaid/Desktop/Big Data Group Project/SocBel.csv")
Environment_Gas <- read_csv("/Users/saranshvaid/Desktop/Big Data Group Project/EnvGas.csv")
Governance_Victim <- read_csv("/Users/saranshvaid/Desktop/Big Data Group Project/GoodVic.csv")

#Selecting relevant cols for DFs and renaming
Prosperity_Poverty <- Prosperity_Poverty[, c("REF_DATE", "GEO", "Individual MBM poverty status (7)", "VALUE")]
names(Prosperity_Poverty)[names(Prosperity_Poverty) == 'Individual MBM poverty status (7)'] <- 'CATEGORY'
Prossperty_Unemployment <- Prossperty_Unemployment[, c("REF_DATE", "GEO", "Labour force characteristics", "VALUE")]
names(Prossperty_Unemployment)[names(Prossperty_Unemployment) == 'Labour force characteristics'] <- 'CATEGORY'
Health_Life <- Health_Life[, c("REF_DATE", "GEO", "Age group", "VALUE")]
names(Health_Life)[names(Health_Life) == 'Age group'] <- 'CATEGORY'
Health_Mental <- Health_Mental[, c("REF_DATE", "GEO", "Indicators", "VALUE")]
Social_Belonging <- Social_Belonging[, c("REF_DATE", "GEO", "Indicators", "VALUE")]
names(Environment_Gas)[names(Environment_Gas) == '1990 greenhouse gas emissions (megatonnes of carbon dioxide equivalent)'] <- 'Year_1990'
names(Environment_Gas)[names(Environment_Gas) == '2005 greenhouse gas emissions (megatonnes of carbon dioxide equivalent)'] <- 'Year_2005'
names(Environment_Gas)[names(Environment_Gas) == '2020 greenhouse gas emissions (megatonnes of carbon dioxide equivalent)'] <- 'Year_2020'
names(Environment_Gas)[names(Environment_Gas) == 'Province or territory'] <- 'Province'
names(Governance_Victim)[names(Governance_Victim) == 'Percentage of persons reporting victimization (%)'] <- 'Victimization'
names(Governance_Victim)[names(Governance_Victim) == 'Percentage of persons reporting violent victimization (%)'] <- 'Violent_Victimization'

#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------

#importing data files into DFs
Prosperity_Poverty <- read_csv("/Users/saranshvaid/Desktop/Big Data Group Project/ProsPov.csv")
Prossperty_Unemployment <- read_csv("/Users/saranshvaid/Desktop/Big Data Group Project/ProsUnem.csv")
Health_Life <- read_csv("/Users/saranshvaid/Desktop/Big Data Group Project/HealLif.csv")
Health_Mental <- read_csv("/Users/saranshvaid/Desktop/Big Data Group Project/HealMen.csv")
Social_Belonging <- read_csv("/Users/saranshvaid/Desktop/Big Data Group Project/SocBel.csv")
Environment_Gas <- read_csv("/Users/saranshvaid/Desktop/Big Data Group Project/EnvGas.csv")
Governance_Victim <- read_csv("/Users/saranshvaid/Desktop/Big Data Group Project/GoodVic.csv")

#Selecting relevant cols for DFs and renaming
Prosperity_Poverty <- Prosperity_Poverty[, c("REF_DATE", "GEO", "Individual MBM poverty status (7)", "VALUE")]
names(Prosperity_Poverty)[names(Prosperity_Poverty) == 'Individual MBM poverty status (7)'] <- 'CATEGORY'
Prossperty_Unemployment <- Prossperty_Unemployment[, c("REF_DATE", "GEO", "Labour force characteristics", "VALUE")]
names(Prossperty_Unemployment)[names(Prossperty_Unemployment) == 'Labour force characteristics'] <- 'CATEGORY'
Health_Life <- Health_Life[, c("REF_DATE", "GEO", "Age group", "VALUE")]
names(Health_Life)[names(Health_Life) == 'Age group'] <- 'CATEGORY'
Health_Mental <- Health_Mental[, c("REF_DATE", "GEO", "Indicators", "VALUE")]
Social_Belonging <- Social_Belonging[, c("REF_DATE", "GEO", "Indicators", "VALUE")]
names(Environment_Gas)[names(Environment_Gas) == '1990 greenhouse gas emissions (megatonnes of carbon dioxide equivalent)'] <- 'Year_1990'
names(Environment_Gas)[names(Environment_Gas) == '2005 greenhouse gas emissions (megatonnes of carbon dioxide equivalent)'] <- 'Year_2005'
names(Environment_Gas)[names(Environment_Gas) == '2020 greenhouse gas emissions (megatonnes of carbon dioxide equivalent)'] <- 'Year_2020'
names(Environment_Gas)[names(Environment_Gas) == 'Province or territory'] <- 'Province'
names(Governance_Victim)[names(Governance_Victim) == 'Percentage of persons reporting victimization (%)'] <- 'Victimization'
names(Governance_Victim)[names(Governance_Victim) == 'Percentage of persons reporting violent victimization (%)'] <- 'Violent_Victimization'

#splitting DF into 3 DFs
Prosperity_Poverty1 <- Prosperity_Poverty[Prosperity_Poverty$CATEGORY %in% c("Total - Individual MBM poverty status"), ]
Prosperity_Poverty2 <- Prosperity_Poverty[Prosperity_Poverty$CATEGORY %in% c("In poverty"), ]
Prosperity_Poverty3 <- Prosperity_Poverty[Prosperity_Poverty$CATEGORY %in% c("Not in poverty"), ]

#assigning common ID so the DFs can now be merged
Prosperity_Poverty1 <- cbind(ID = 1:nrow(Prosperity_Poverty1), Prosperity_Poverty1)
Prosperity_Poverty2 <- cbind(ID = 1:nrow(Prosperity_Poverty2), Prosperity_Poverty2)
Prosperity_Poverty3 <- cbind(ID = 1:nrow(Prosperity_Poverty3), Prosperity_Poverty3)

#merging the 3 DFs by ID's
Prosperity_Poverty4 <- merge(Prosperity_Poverty1, Prosperity_Poverty2, by = c("ID"))
Prosperity_Poverty5 <- merge(Prosperity_Poverty3, Prosperity_Poverty4, by = c("ID"))

#Rename
names(Prosperity_Poverty5)[names(Prosperity_Poverty5) == 'VALUE'] <- 'NOT_IN_POVERTY'
names(Prosperity_Poverty5)[names(Prosperity_Poverty5) == 'VALUE.x'] <- 'TOTAL'
names(Prosperity_Poverty5)[names(Prosperity_Poverty5) == 'VALUE.y'] <- 'IN_POVERTY'

#Selecting relevant cols for DFs and renaming
Prosperity_Poverty6 <- Prosperity_Poverty5[, c("REF_DATE", "GEO", "TOTAL", "NOT_IN_POVERTY", "IN_POVERTY")]

#splitting DF into 2 DFs
Prossperty_Unemployment1 <- Prossperty_Unemployment[Prossperty_Unemployment$CATEGORY %in% c("Population"), ]
Prossperty_Unemployment2 <- Prossperty_Unemployment[Prossperty_Unemployment$CATEGORY %in% c("Unemployment rate"), ]

#assigning common ID so the DFs can now be merged
Prossperty_Unemployment1 <- cbind(ID = 1:nrow(Prossperty_Unemployment1), Prossperty_Unemployment1)
Prossperty_Unemployment2 <- cbind(ID = 1:nrow(Prossperty_Unemployment2), Prossperty_Unemployment2)

#merging the 3 DFs by ID's
Prossperty_Unemployment3 <- merge(Prossperty_Unemployment1, Prossperty_Unemployment2, by = c("ID"))

#Rename
names(Prossperty_Unemployment3)[names(Prossperty_Unemployment3) == 'REF_DATE.x'] <- 'REF_DATE'
names(Prossperty_Unemployment3)[names(Prossperty_Unemployment3) == 'GEO.x'] <- 'GEO'
names(Prossperty_Unemployment3)[names(Prossperty_Unemployment3) == 'VALUE.x'] <- 'POPULATION'
names(Prossperty_Unemployment3)[names(Prossperty_Unemployment3) == 'VALUE.y'] <- 'UNEMPLOYMENT_RATE'

#Selecting relevant cols for DFs and renaming
Prossperty_Unemployment4 <- Prossperty_Unemployment3[, c("REF_DATE", "GEO", "POPULATION", "UNEMPLOYMENT_RATE")]

#splitting DF into 3 DFs
Social_Belonging1 <- Social_Belonging[Social_Belonging$Indicators %in% c("Very strong or somewhat strong sense of belonging to local community"), ]
Social_Belonging2 <- Social_Belonging[Social_Belonging$Indicators %in% c("Somewhat weak or very weak sense of belonging to local community"), ]
Social_Belonging3 <- Social_Belonging[Social_Belonging$Indicators %in% c("No opinion on sense of belonging to local community"), ]

#assigning common ID so the DFs can now be merged
Social_Belonging1 <- cbind(ID = 1:nrow(Social_Belonging1), Social_Belonging1)
Social_Belonging2 <- cbind(ID = 1:nrow(Social_Belonging2), Social_Belonging2)
Social_Belonging3 <- cbind(ID = 1:nrow(Social_Belonging3), Social_Belonging3)

#merging the 3 DFs by ID's
Social_Belonging4 <- merge(Social_Belonging1, Social_Belonging2, by = c("ID"))
Social_Belonging5 <- merge(Social_Belonging3, Social_Belonging4, by = c("ID"))

#Rename
names(Social_Belonging5)[names(Social_Belonging5) == 'VALUE'] <- 'NO_OPINION'
names(Social_Belonging5)[names(Social_Belonging5) == 'VALUE.x'] <- 'VERYSTRONG_SOMEWHATSTRONG'
names(Social_Belonging5)[names(Social_Belonging5) == 'VALUE.y'] <- 'SOMEWHATWEAK_VERYWEAK'

#Selecting relevant cols for DFs and renaming
Social_Belonging6 <- Social_Belonging5[, c("REF_DATE", "GEO", "VERYSTRONG_SOMEWHATSTRONG", "SOMEWHATWEAK_VERYWEAK", "NO_OPINION")]

#splitting DF into 3 DFs
Health_Mental1 <- Health_Mental[Health_Mental$Indicators %in% c("Excellent or very good perceived mental health"), ]
Health_Mental2 <- Health_Mental[Health_Mental$Indicators %in% c("Good perceived mental health"), ]
Health_Mental3 <- Health_Mental[Health_Mental$Indicators %in% c("Fair or poor perceived mental health"), ]

#assigning common ID so the DFs can now be merged
Health_Mental1 <- cbind(ID = 1:nrow(Health_Mental1), Health_Mental1)
Health_Mental2 <- cbind(ID = 1:nrow(Health_Mental2), Health_Mental2)
Health_Mental3 <- cbind(ID = 1:nrow(Health_Mental3), Health_Mental3)

#merging the 3 DFs by ID's
Health_Mental4 <- merge(Health_Mental1, Health_Mental2, by = c("ID"))
Health_Mental5 <- merge(Health_Mental3, Health_Mental4, by = c("ID"))

#Rename
names(Health_Mental5)[names(Health_Mental5) == 'VALUE'] <- 'FAIR_OR_POOR'
names(Health_Mental5)[names(Health_Mental5) == 'VALUE.x'] <- 'EXCELLENT_OR_VERYGOOD'
names(Health_Mental5)[names(Health_Mental5) == 'VALUE.y'] <- 'GOOD'

#Selecting relevant cols for DFs and renaming
Health_Mental5 <- Health_Mental5[, c("REF_DATE", "GEO", "EXCELLENT_OR_VERYGOOD", "GOOD", "FAIR_OR_POOR")]

#splitting DF into 2 DFs
Health_Life1 <- Health_Life[Health_Life$CATEGORY %in% c("At birth"), ]
Health_Life2 <- Health_Life[Health_Life$CATEGORY %in% c("At age 65"), ]

#assigning common ID so the DFs can now be merged
Health_Life1 <- cbind(ID = 1:nrow(Health_Life1), Health_Life1)
Health_Life2 <- cbind(ID = 1:nrow(Health_Life2), Health_Life2)

#merging the 3 DFs by ID's
Health_Life3 <- merge(Health_Life1, Health_Life2, by = c("ID"))

#Rename
names(Health_Life3)[names(Health_Life3) == 'REF_DATE.x'] <- 'REF_DATE'
names(Health_Life3)[names(Health_Life3) == 'GEO.x'] <- 'GEO'
names(Health_Life3)[names(Health_Life3) == 'VALUE.x'] <- 'AT_BIRTH'
names(Health_Life3)[names(Health_Life3) == 'VALUE.y'] <- 'AT_AGE65'

#Selecting relevant cols for DFs and renaming
Health_Life4 <- Health_Life3[, c("REF_DATE", "GEO", "AT_BIRTH", "AT_AGE65")]

#---------------------------------------------------------------
#---------------------------------------------------------------
#---------------------------------------------------------------

#Prosperity Poverty --------------------------------------------

df1 <- Prosperity_Poverty6[, c("GEO", "TOTAL", "IN_POVERTY", "NOT_IN_POVERTY")]
df1$POVERTY_RATE <- (df1$IN_POVERTY / df1$TOTAL) * 100

#Creating another dummy for sorting GEO by Alphabetical order
df1_S <- df1

#Sorting GEO by Alphabetical order
df1_Sorted <- df1_S[order(df1_S$GEO), ]

#rounding off average rate columns to 2 DP
df1_Sorted <- df1_Sorted %>%
  mutate_if(is.numeric,
            round, digits = 2)

# Create named vector of province abbreviations
PROVINCE_drop <- c(
  "Newfoundland and Labrador" = "NL",
  "Prince Edward Island" = "PE",
  "Nova Scotia" = "NS",
  "New Brunswick" = "NB",
  "Quebec" = "QC",
  "Ontario" = "ON",
  "Manitoba" = "MB",
  "Saskatchewan" = "SK",
  "Alberta" = "AB",
  "British Columbia" = "BC"
)

df1_Sorted <- df1_Sorted %>% 
  mutate(PROVINCE_drop = PROVINCE_drop[GEO])

df1B <- df1_Sorted[, c("PROVINCE_drop", "TOTAL", "IN_POVERTY", "NOT_IN_POVERTY", "POVERTY_RATE")]

df1C <- df1B
#--------------------------------------------------------
#--------------------------------------------------------
#--------------------------------------------------------

#Renaming the PROVINCE_drop to postal for merging the dataframe
names(df1C)[names(df1C) == 'PROVINCE_drop'] <- 'postal'

# load the data
canada_provinces <- ne_states(country = "canada", returnclass = "sf")
#1mturkData <- read.csv("alldata2.csv", header = TRUE)

#Creating dummy
canada_provincesA <- canada_provinces

#Merging the data to have same number of rows
#merged_data <- merge(canada_provincesA, df1BSorted, by = "postal", all.x = TRUE)
merged_data <- merge(canada_provincesA, df1C, by = c("postal"))

#binding the data since it has same number of rows now
df1D <- cbind(df1C, merged_data)

#dropping one of the postal code column and creating a new dataframe
df1E <- df1D[, -6]

#--------------------------------------------------------
#--------------------------------------------------------
#--------------------------------------------------------

# convert df1E to sf object
df1E_sf <- st_as_sf(df1E, coords = c("longitude", "latitude"), crs = 4326)

# plot choropleth map
ggplot() +
  geom_sf(data = df1E_sf, aes(fill = POVERTY_RATE)) +
  scale_fill_gradient(low = "white", high = "red", na.value = "grey") +
  theme_void()

#--------------------------------------------------------
#--------------------------------------------------------
#--------------------------------------------------------

library(plotly)
library(sf)

# Load the data
df1E <- read.csv("https://raw.githubusercontent.com/kho777/data-visualization/master/data/df1E.csv")

# Convert to sf object
df1E_sf <- st_as_sf(df1E, coords = c("longitude", "latitude"), crs = 4326)

# Filter for Canada
df1E_canada <- df1E_sf[df1E_sf$COUNTRY == "Canada", ]

# Create a plotly object
map_plot <- plot_ly(df1E_canada) %>%
  add_sf(z = ~POVERTY_RATE, color = I("red"), colors = "YlOrRd") %>%
  layout(
    title = "Poverty rate in Canada",
    geo = list(
      scope = "north america",
      projection = list(type = "albers usa"),
      showlakes = TRUE,
      lakecolor = toRGB("white")
    )
  )

# Show the plot
map_plot





#Prosperity Unemployment Rate --------------------------------------------

df2 <- Prossperty_Unemployment4[, c("GEO", "POPULATION", "UNEMPLOYMENT_RATE")]

# Add new column with province abbreviations to existing data frame
df2 <- df2 %>% 
  mutate(PROVINCE = PROVINCE[GEO])

Prossperty_Unem <- read_csv("ProsUnem1.csv")
df2A <- Prossperty_Unem[, c("GEO", "VALUE")]
names(df2A)[names(df2A) == 'VALUE'] <- 'UNEMPLOYMENT'

df2B <- cbind(df2, df2A$UNEMPLOYMENT)
names(df2B)[names(df2B) == 'df2A$UNEMPLOYMENT'] <- 'UNEMPLOYMENT'
#df2B$POVERTY_RATE <- (df1$IN_POVERTY / df1$TOTAL) * 100

df2C <- df2B[, c("PROVINCE", "POPULATION", "UNEMPLOYMENT_RATE")]

# Create the plot
ggplot(df2C, aes(x = PROVINCE, y = POPULATION)) +
  geom_bar(stat = "identity", fill = "#077b8a") +
  geom_text(aes(label = paste0(UNEMPLOYMENT_RATE, "%")), vjust = -0.5, size = 4) +
  labs(x = "Province", y = "Population", title = "Population vs Unemployment Rate by Province") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#Health-adjusted life expectancy  --------------------------------------------

df3 <- Health_Life4

# Add new column with province abbreviations to existing data frame
df3 <- df3 %>% 
  mutate(PROVINCE = PROVINCE[GEO])

df3A <- df3[, c("PROVINCE", "AT_BIRTH", "AT_AGE65")]

ggplot(df3A, aes(x = PROVINCE)) +
  geom_col(aes(y = AT_BIRTH, fill = "At Birth"), position = "dodge", width = 0.5) +
  geom_col(aes(y = AT_AGE65, fill = "At Age 65"), position = "dodge", width = 0.5) +
  scale_fill_manual(values = c("#0d1137","#e52165"), name = "Group") +
  labs(x = "Province", y = "Years", title = "Health-adjusted life expectancy") +
  theme(legend.position = "bottom", 
        plot.title = element_text(hjust = 0.5, margin = margin(b = 20), size = 18))


df3B <- df3A %>%
  select(PROVINCE, AT_BIRTH, AT_AGE65) %>%
  pivot_longer(cols = c(AT_BIRTH, AT_AGE65),
               names_to = "Variable", values_to = "Value") %>%
  mutate(Variable = factor(Variable, levels = c("AT_BIRTH", "AT_AGE65")))


# create grouped bar chart
ggplot(df3B, aes(x = PROVINCE, y = Value, fill = Variable)) +
  geom_bar(position = "dodge", stat = "identity") +
  xlab("Province") +
  ylab("Years") +  # Change y-axis label
  ggtitle("Health-adjusted life expectancy") +
  theme_classic() +
  scale_fill_discrete(name = "Year") +
  labs(fill = "Year") + 
  scale_fill_manual(values = c("#26495c", "#c4a35a"), 
                    guide = guide_legend(title = "Year")) +  
  theme(plot.title = element_text(hjust = 0.5, margin = margin(b = 20), size = 18))

#Health-Perceived mental health  --------------------------------------------

df4 <- Health_Mental5

# Add new column with province abbreviations to existing data frame
df4 <- df4 %>% 
  mutate(PROVINCE = PROVINCE[GEO])

df4A <- df4[, c("PROVINCE", "EXCELLENT_OR_VERYGOOD", "GOOD", "FAIR_OR_POOR")]

df4B <- df4A %>%
  select(PROVINCE, EXCELLENT_OR_VERYGOOD, GOOD, FAIR_OR_POOR) %>%
  pivot_longer(cols = c(EXCELLENT_OR_VERYGOOD, GOOD, FAIR_OR_POOR),
               names_to = "Variable", values_to = "Value") %>%
  mutate(Variable = factor(Variable, levels = c("EXCELLENT_OR_VERYGOOD", "GOOD", "FAIR_OR_POOR")))

ggplot(df4B, aes(x = Value, y = reorder(PROVINCE, Value), fill = Variable)) +
  geom_col(position = "stack") +
  geom_text(aes(label = paste0(round(Value), "%")), position = position_stack(vjust = 0.5)) +
  labs(x = "Percentage", y = "Province", 
       title = "Perceived Mental Health") +
  scale_fill_manual(values = c("#0072B2", "#E69F00", "#009E73"),
                    name = "Health Perception") +
  theme(plot.title = element_text(hjust = 0.5, margin = margin(b = 20), size = 18))

#Society - Sense of belonging to local community ---------------------------
df5 <- Social_Belonging6

df5 <- df5 %>% 
  mutate(PROVINCE = PROVINCE[GEO])

df5A <- df5[, c("PROVINCE", "VERYSTRONG_SOMEWHATSTRONG", "SOMEWHATWEAK_VERYWEAK", "NO_OPINION")]

df5B <- df5A %>%
  select(PROVINCE, VERYSTRONG_SOMEWHATSTRONG, SOMEWHATWEAK_VERYWEAK, NO_OPINION) %>%
  pivot_longer(cols = c(VERYSTRONG_SOMEWHATSTRONG, SOMEWHATWEAK_VERYWEAK, NO_OPINION),
               names_to = "Variable", values_to = "Value") %>%
  mutate(Variable = factor(Variable, levels = c("VERYSTRONG_SOMEWHATSTRONG", "SOMEWHATWEAK_VERYWEAK", "NO_OPINION")))

ggplot(df5B, aes(x = Value, y = reorder(PROVINCE, Value), fill = Variable)) +
  geom_col(position = "stack") +
  geom_text(aes(label = paste0(round(Value), "%")), position = position_stack(vjust = 0.5)) +
  labs(x = "Percentage", y = "Province", 
       title = "Sense of Belonging to Society") +
  scale_fill_manual(values = c("#1e3d59", "#ff6e40", "#ffc13b"),
                    name = "Belonging Perception") +
  theme(plot.title = element_text(hjust = 0.5, margin = margin(b = 20), size = 18))

#Environment - Greenhouse Gas Emissions ------------------------------------
df6 <- Environment_Gas

# drop last 3 rows
df6 <- head(df6, -3)

#add province abbr
df6$Abbr <- c("NL", "PE", "NS", "NB", "QC", "ON", "MB", "SK", "AB", "BC")

df6A <- df6[, c("Abbr", "Year_1990", "Year_2005", "Year_2020")]
names(df6A)[names(df6A) == 'Abbr'] <- 'PROVINCE'

df6A$Year_1990 <- as.numeric(df6A$Year_1990)

df6B <- df6A %>%
  select(PROVINCE, Year_1990, Year_2005, Year_2020) %>%
  pivot_longer(cols = c(Year_1990, Year_2005, Year_2020),
               names_to = "Variable", values_to = "Value") %>%
  mutate(Variable = factor(Variable, levels = c("Year_1990", "Year_2005", "Year_2020")))

# create grouped bar chart
ggplot(df6B, aes(x = PROVINCE, y = Value, fill = Variable)) +
  geom_bar(position = "dodge", stat = "identity") +
  xlab("Province") +
  ylab("Emissions (CO2 Mt)") +  # Change y-axis label
  ggtitle("Greenhouse Gas Emission") +
  theme_classic() +
  scale_fill_discrete(name = "Year") +
  labs(fill = "Year") +  # Change legend title (alternative method)
  scale_fill_manual(values = c("#26495c", "#c4a35a", "#c66b3d"), 
                    guide = guide_legend(title = "Year")) +  
  theme(plot.title = element_text(hjust = 0.5, margin = margin(b = 20), size = 18))


#Good Governance - Personal Safety ------------------------------------
df7 <- Governance_Victim

# drop last 3 rows and top row
df7 <- head(df7, -3)
df7 <- slice(df7, -1)

#add province abbr
df7$Abbr <- c("NL", "PE", "NS", "NB", "QC", "ON", "MB", "SK", "AB", "BC")

df7A <- df7[, c("Abbr", "Victimization", "Violent_Victimization")]
names(df7A)[names(df7A) == 'Abbr'] <- 'PROVINCE'

df7A

my_colors <- c("#E69F00", "#ff6e40", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7","#26495c", "#c4a35a", "#c66b3d") 

ggplot(df7A, aes(x = Victimization, y = Violent_Victimization, color = PROVINCE)) +
  geom_point(size = 5, aes(color = PROVINCE)) +
  geom_text(aes(label = PROVINCE), size = 3, vjust = 2, color = "black") +
  labs(title = "Reports Made of Violent and Non-Violent Victimization", x = "Non-Violent", y = "Violent") +
  theme(plot.title = element_text(hjust = 0.5, margin = margin(b = 20), size = 15)) +
  scale_color_manual(values = my_colors) +
  guides(color = "none")

############################################################################
############################################################################
############################################################################

