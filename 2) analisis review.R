# Load the libraries
library(tidyverse)
library(wordcloud)
library(cowplot)
library(reshape2)
library(RColorBrewer)
library(viridis)
library(ggsankey)
library(reshape2)
library(forcats)
library(googlesheets4)
library(rworldmap)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)


# Read the dataframe
df<- read.delim("Database/Data_extraction.csv", sep = ";")

# Eliminate rows with NA values in the ID column
df <- df[!is.na(df$ID),]

# Count of the papers analyzed
print(length(unique(df$ID)))


###################################
######### 1) Supply/Demand ########
###################################

# Select CES type by paper
sd <- df %>% group_by(ID, Supply.demand.CES) %>% 
  select(ID, Supply.demand.CES) %>% unique()

# CES type count
sd_count <- sd %>% 
  group_by(Supply.demand.CES) %>% 
  summarise(n = n(), .groups = 'drop') %>%
  mutate(prop = n / sum(n) * 100)

# Calculate positions for labels
sd_count <- sd_count %>% 
  arrange(desc(Supply.demand.CES)) %>%
  mutate(lab.ypos = cumsum(prop) - 0.5*prop)

# Customize the order of labels
custom_order <- c("Supply", "Demand", "Both", "Undefined")

# Convert Supply.demand.CES to a factor with custom order
sd_count$Supply.demand.CES <- factor(sd_count$Supply.demand.CES, levels = custom_order)


# Create the pie chart
sup <- ggplot(sd_count, aes(x = "", y = prop, fill = Supply.demand.CES)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar(theta = "y") +
  labs(fill = "", title = "") +
  theme_void() +
  scale_fill_manual(values = c("gray20", "gray40", "gray60", "gray80"))+
  # geom_text(aes(y = lab.ypos, label =  round(prop, 0)), color = "white", size = 5)
  geom_text(aes(y = lab.ypos, label =  paste0(round(prop, 0), "%")), color = "white", size = 3)

sup

###################################
######### 2) CES ##################
###################################

# Read the CES dataframe 
df2 <- read.delim("Database/Validacion_CES and variables groups.csv", sep = ";")

# CES Count
ces <- df2 %>%
  group_by(IPBES.categories, CES ) %>%
  summarise(CES_count = n()) %>%
  arrange(desc(CES_count))

# Exclude rows with NAs
ces <- ces[!is.na(ces$CES) & ces$CES != ""  & ces$CES != "?",]

# Exclude rows with NAs
ces <- ces[!is.na(ces$IPBES.categories) & ces$IPBES.categories != ""  & ces$IPBES.categories != "?",]

# Rename Non-material NCP like "General CES"
ces <- ces %>%
  mutate(IPBES.categories = ifelse(IPBES.categories == "Non-material NCP", "General CES", IPBES.categories))

ces <- ces %>%
  mutate(CES = ifelse(CES == "Non-material NCP", "General CES", CES))

# Customize the order of labels
# unique(ces$IPBES.categories)
ces <- ces %>%
  mutate(IPBES.categories = factor(IPBES.categories, levels = c( "Physical and psychological experiences", "Supporting identities" ,"Learning and inspiration","Maintenance of options", "General CES")))


# Convert CES to a factor with custom order 
ces <- ces %>% 
  mutate(CES.services.type.homogene = factor(CES, levels = CES))


# CES Chart
ces_fig <- ggplot(ces, aes(x = CES.services.type.homogene, y= CES_count, fill = IPBES.categories)) +
  geom_col() +              
  theme_bw() +              
  labs(x = "CES in the literature review", y = "number of articles")+
  scale_fill_viridis(discrete = TRUE)+
  # scale_fill_brewer(15,palette = "BrBG") + 
  # scale_fill_discrete()
  # scale_fill_manual(values = c("Abundance" = "darkred", "Both"= "darkgreen", "Presence"= "darkorange"))+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  guides(fill = guide_legend(title = "IPBES categories", nrow = 2))+
  theme(legend.position = "bottom")

ces_fig

## Values of  CES type 
categories <- c("Instrumental", "Relational", "Intrinsic")
val <- c(64, 28, 8)
lab.ypos <- c(68, 14,32)

table <- data.frame(categories, val, lab.ypos)


# Create the pie chart
CES_t <- ggplot(table, aes(x = "", y = val, fill = categories)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar(theta = "y") +
  labs(fill = "", title = "") +
  theme_void() +
  scale_fill_manual(values = c("gray20", "gray40", "gray60", "gray80"))+
  # geom_text(aes(y = lab.ypos, label =  round(prop, 0)), color = "white", size = 5)
  geom_text(aes(y = lab.ypos, label =  paste0(round(prop, 0), "%")), color = "white", size = 3)

CES_t

# Combine both graphs using cowplot
combined_plot <- ggdraw() +
  draw_plot(ces_fig, 0, 0, 1, 1) +
  draw_plot(CES_t, 0.60, 0.69, 0.35, 0.37) 
# Display plot
print(combined_plot)

# Save plot
ggsave("Figures/1) CES.png", width = 8, height = 5, dpi = 600)

###############################
##CES definition percentage#### 
###############################

# CES count
sum(ces$CES_count)

# CES count based on definition
CES_def <- df2 %>% 
  group_by(well.defined) %>% 
  summarise(count=n(),
            porcentaje = (n()/224)*100)

# CES count
ces_count <- ces %>%
  group_by(IPBES.categories) %>%
  summarise(CES_count_total = sum(CES_count)) %>%
  arrange(desc(CES_count_total))

# Calculate the percentages
ces_count <- ces_count %>%
  mutate(prop = CES_count_total / sum(CES_count_total) * 100)

###############################
##########CES SANKEY########### 
###############################

# Select the columns of interest
ces_sankey <- df %>% 
  select(ID, CES, IPBES, Variables.Standardized, Level3, Level2, Level1) 

# Exclude rows with NAs
ces_sankey <- ces_sankey[!is.na(ces_sankey$CES) & ces_sankey$CES != ""  & ces_sankey$CES != 0,]

# split columns (since there are cases where there is more than one CES per row separated by ";")
ces_sankey <- ces_sankey %>%
  separate_rows(IPBES, CES, sep = ";")

# Exclude rows with NAs
ces_sankey <- ces_sankey[!is.na(ces_sankey$IPBES) & ces_sankey$IPBES != ""  & ces_sankey$IPBES != 0,]
ces_sankey <- ces_sankey[!is.na(ces_sankey$Level3) & ces_sankey$Level3 != ""  & ces_sankey$Level3 != 0,]

# Modify specific values in level2 column
ces_sankey <- ces_sankey %>%
  mutate(CES = case_when(
    CES == "Maintenance of options" ~ "Maintenance of options ",
    TRUE ~ CES  # To keep the other values unchanged
  ))

# Convert table to long format
data_sankey_CES <- ces_sankey %>%
  make_long(IPBES, CES, Level3)

# unique values
unique(data_sankey_CES$node)


orden_variables <- c(
 
  #LEVEL3
  "Accommodation",
  "Air Quality", 
  "Buildings", 
  "Climate and Weather", 
  "Coastal and Marine Activities", 
  "Coastal and Marine Places",
  "Cultural and Historical Heritage", 
  "Cultural and Recreational Zones", 
  "Socio-Demographic data", 
  "Diversity and Heterogeneity Indices", 
  "Fauna", 
  "Flora", 
  "Geology", 
  "Green and blue spaces", 
  "Habitat Quality Index", 
  "Historical Buildings and Structures", 
  "Human perceptions", 
  "Land Use and Land Cover",
  "Land ownership", 
  "Landscape metrics",
  "Management regime", 
  "Maritime and Air Transport Infrastructure", 
  "Naturalness indicators",
  "Parking Infrastructure", 
  "Protected Areas", 
  "Public Transport", 
  "Public infrastructures", 
  "Recreational or tourism infrastructures",
  "Remote sensing data", 
  "Risk and Hazard Factors", 
  "Roads and Paths", 
  "Ski and Snow Sports", 
  "Soil characteristics", 
  "Topographical Variables",
  "Urban settlement", 
  "Viewshed analysis",
  "Visitor information", 
  "Water",
  "Water Quality", 

  #NCP
  "Artistic inspiration",
  "Environmental education", 
  "Research", 
  "Maintenance of options ",
  "Enjoy the fauna and flora", 
  "Enjoy the landscape", 
  "Psychological experiences",
  "Psychological experiences ",
  "Recreation",
  "Recreation (e.g. hunting, fishing)",
  "Cultural heritage",
  "Intrinsic", 
  "Rural tourism",
  "Sense of belonging", 
  "Social relations", 
  "Spiritual significance",
  "Demographic data",
  
  #CES
  "Learning and inspiration", 
  "Maintenance of options",
  "Physical and psychological experiences",
  "Supporting identities"  )



# Convert variables to a factor with custom order 
data_sankey_CES2 <- data_sankey_CES %>%
  mutate(node = factor(node, levels = orden_variables))

  
# Create Sankey plot using ggplot2 and ggsankey
ggplot(data_sankey_CES2, aes(x = x, next_x = next_x, node = node, next_node = next_node, fill = factor(node), label = node)) +
  geom_sankey(flow.alpha = 0.5, node.color = "black") +
  geom_sankey_label(size = 5, color = "white", fill = "black", ) +
  theme_sankey(base_size = 16) +
  theme(legend.position = "none",
        axis.text.x = element_text(size = 22))+
  labs(x= "")+
  scale_x_discrete(label = c("IPBES Categories", "CES NCP", "Variables grouped Level 3"))

# Save plot
ggsave("Figures/2) CES_sankey_level3.png", width = 14, height = 10, dpi = 600)


# Convert table to long format
data_sankey_CES3 <- ces_sankey %>%
  make_long(IPBES, CES, Level2)

# unique values
unique(data_sankey_CES3$node)


orden_variables <- c(
  
  #LEVEL2
  "Accessibility",
  "Cultural and Historical Heritage",
  "Environmental Quality",
  "Green and blue spaces",  
  "Human perceptions",
  "Infrastructure",
  "Land Use and Land Cover",
  "Management ",
  "Natural Environment",
  "Protected Areas",
  "Recreational Opportunities",
  "Remote sensing data",  
  "Statistical data",
  
  #NCP
  "Artistic inspiration",
  "Environmental education", 
  "Research", 
  "Maintenance of options ",
  "Enjoy the fauna and flora", 
  "Enjoy the landscape", 
  "Psychological experiences",
  "Psychological experiences ",
  "Recreation",
  "Recreation (e.g. hunting, fishing)",
  "Cultural heritage",
  "Intrinsic", 
  "Rural tourism",
  "Sense of belonging", 
  "Social relations", 
  "Spiritual significance",
  
  #CES
  "Learning and inspiration", 
  "Maintenance of options",
  "Physical and psychological experiences",
  "Supporting identities"  
)



# Convert variables to a factor with custom order 
data_sankey_CES4 <- data_sankey_CES3 %>%
  mutate(node = factor(node, levels = orden_variables))


# Crear el gráfico de Sankey usando ggplot2 y ggsankey
ggplot(data_sankey_CES4, aes(x = x, next_x = next_x, node = node, next_node = next_node, fill = factor(node), label = node)) +
  geom_sankey(flow.alpha = 0.5, node.color = "black") +
  geom_sankey_label(size = 5, color = "white", fill = "black", ) +
  theme_sankey(base_size = 16) +
  theme(legend.position = "none",
        axis.text.x = element_text(size = 22))+
  labs(x= "")+
  scale_x_discrete(label = c("IPBES Categories", "CES NCP", "Variables grouped Level 2"))

# Save plot
ggsave("Figures/2) CES_sankey_level2.png", width = 14, height = 10, dpi = 600)
##################################
######### 3) Variables ###########
##################################

# original variables count
length(unique(df$Variables.Originales))

# standardized variable count
length(unique(df$Variables.Standardized))

# standardized variable count
v <- df %>% 
  group_by(ID, Variables.Standardized) %>% 
  summarise(var_count = n()) %>% 
  arrange(desc(var_count))

v2 <- v %>% 
  group_by(Variables.Standardized) %>% 
  summarise(sum = sum(var_count)) %>% 
  arrange(desc(sum))

# variables list  
unique(df$Variables.Standardized)


# Variable count per level
variables <- df %>% 
  group_by(Level1, Level2) %>% 
  summarise(var_count = n()) %>% 
  arrange(desc(var_count))

# Exclude rows with NAs
variables <- variables[!is.na(variables$Level2) & variables$Level2 != "", ]
variables <- variables[!is.na(variables$Level1) & variables$Level1 != "", ]


# Change "Anthropic elements" for "Anthropogenic elements"
variables <- variables %>%
  mutate(Level1 = ifelse(Level1 == "Anthropic elements", "Anthropogenic elements", Level1))

# Customize the order of labels
variables <- variables %>%
  mutate(Level1 = factor(Level1, levels = c( "Natural elements", "Anthropogenic elements" ,"Land management","Remote sensing data")))

# Convert variables to a factor with custom order 
variables <- variables %>% 
  mutate(Level2 = factor(Level2, levels = Level2))


# plot variables 
ggplot(variables, aes(x = Level2, y = var_count, fill = Level1)) +
  geom_col() +              
  theme_bw() +              
  labs(x = "Grouped explanatory variables (level 2)", y = "Number of variables \nused in the articles (level 3)", fill = "Grouped explanatory\n variables (level 1)")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Save plot
ggsave("Figures/3) Variables.png", width = 8, height = 4, dpi = 600)


# Count variables by level
variables_level <- variables %>% 
  group_by(Level1) %>% 
  summarise(var_count_total = sum(var_count)) %>% 
  arrange(desc(var_count_total))

# Calculate the percentages
variables_level <- variables_level %>% 
  mutate(prop = var_count_total / sum(var_count_total) * 100)

#########################################
##########Variables by metric ########### 
#########################################

# Select the columns of interest
vari <- df %>% select(c(ID,Type, Variables.Standardized, Level2))

# Exclude rows with NAs
vari <- vari[!is.na(vari$Type) & vari$Type != "",] 


# Summary of variables by type of metric used
vari <- vari %>% 
  group_by(ID, Type, Level2) %>% 
  summarise(Variables.Standardized = unique(Variables.Standardized))

# Counting variables by metric type
vari3 <- vari %>% 
  group_by(Level2, Type) %>% 
  summarise(count = length(Type))

# Heat map dataframe 
heatmap_data <- vari3 %>%
  group_by(Level2, Type) %>%
  summarise(count = sum(count)) %>%
  ungroup() %>%
  complete(Level2, Type, fill = list(count = 0))

# To improve the visualization I make some modifications to the original values
heatmap_data <- heatmap_data %>% 
  mutate(
    count2 = log(count + 1e-6),
    count3 = (count - 0) / (86 - 0),
    count4 = (count - mean(count)) / sd(count),
    count5 = sqrt(count)
  )

# plot the heatmap
ggplot(heatmap_data, aes(x = Type, y = Level2, fill = count5)) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = "white", high = "red") +
  # scale_fill_gradient(low = "white", high = "blue") +
  # theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "",
       x = "Aggregation methods used",
       y = "variables",
       fill = "Count (sqrt)")

ggsave("Figures/4) Type of aggregation methods.png", width = 8, height = 4, dpi = 600)

# Count by metric type
vari4 <- vari3 %>% 
  group_by(Type) %>% 
  summarise(count2 = sum(count))

# Calculate the percentages
vari4 <- vari4 %>% 
  mutate(prop = count2 / sum(count2) * 100)

#####################################
##########variables SANKEY########### 
#####################################

unique(df$Level2)

# Modify specific values in level3 column
data <- df %>%
  mutate(Level3 = case_when(
    Level3 == "Human perceptions" ~ "Human perception",
    Level3 == "Green and blue spaces" ~ "Green/blue spaces",
    Level3 == "Cultural and Historical Heritage" ~ "Cultural/historical heritage",
    Level3 == "Land Use and Land Cover"~ "Land Use/Land Cover",
    Level3 == "Protected Areas"~ "Protected Area",
    TRUE ~ Level3  # To keep the other values unchanged
  ))

# Convert table to long format
data_sankey_long2 <- data %>%
  make_long(Level1, Level2 , Level3)

# Exclude rows with NAs
data_sankey_long2 <- data_sankey_long2[!is.na(data_sankey_long2$node) & data_sankey_long2$node != 0 & data_sankey_long2$node != "",] 

# unique values
unique(data_sankey_long2$node)

# Customize the order of labels
orden_variables <- c(
  
  #LEVEL3
  "Maritime and Air Transport Infrastructure",
  "Parking Infrastructure", 
  "Public Transport",
  "Roads and Paths", 
  "Cultural and Recreational Zones",
  "Cultural/historical heritage", 
  "Historical Buildings and Structures", 
  "Habitat Quality Index",
  "Water Quality", 
  "Air Quality", 
  "Green/blue spaces",
  "Human perception",
  "Accommodation",
  "Buildings", 
  "Public infrastructures",
  "Urban settlement", 
  "Coastal and Marine Activities", 
  "Recreational or tourism infrastructures", 
  "Ski and Snow Sports", 
  "Demographic data", 
  "Visitor information", 
  "Risk and Hazard Factors",
  "Land ownership",
  "Management regime",
  "Protected Area", 
  "Land Use/Land Cover",
  "Landscape metrics", 
  "Climate and Weather", 
  "Coastal and Marine Places", 
  "Diversity and Heterogeneity Indices",
  "Soil characteristics", 
  "Naturalness indicators", 
  "Fauna", 
  "Flora", 
  "Geology", 
  "Topographical Variables",
  "Viewshed analysis", 
  "Water",
  
  #LEVEL2
  "Accessibility",
  "Cultural and Historical Heritage", 
  "Environmental Quality", 
  "Green and blue spaces", 
  "Human perceptions", 
  "Infrastructure",
  "Recreational Opportunities", 
  "Statistical data",
  "Management ",
  "Protected Areas", 
  "Land Use and Land Cover",
  "Natural Environment",
  "Remote sensing",
  
  #LEVEL1
  "Anthropic elements",
  "Land management", 
  "Natural elements", 
  "Remote sensing data"
  
  
  
)


# Convert variables to a factor with custom order 
data_sankey_long3 <- data_sankey_long2 %>%
  mutate(node = factor(node, levels = orden_variables))

# Create Sankey plot using ggplot2 and ggsankey
ggplot(data_sankey_long3, aes(x = x, next_x = next_x, node = node, next_node = next_node, fill = factor(node), label = node)) +
  geom_sankey(flow.alpha = 0.5, node.color = "black") +
  geom_sankey_label(size = 5, color = "white", fill = "black") +
  theme_sankey(base_size = 16) +
  theme(legend.position = "none",
        axis.text.x = element_text(size = 22))+
  labs(x="")+
  scale_x_discrete(label = c("Variables grouped\n Level 1", "Variables grouped\n Level 2", "Variables grouped\n Level 3"))

ggsave("Figures/5) Sankey Variables.png", width = 14, height = 10, dpi = 600)


#########################################
##########variables most used ########### 
#########################################

# Select the columns of interest
vari.x <- df %>% 
  select(ID, IPBES, Variables.Standardized) 

# Sum of variables related with IPBES
var.y <- vari.x %>% 
  group_by(IPBES, Variables.Standardized) %>% 
  summarise(suma = length(Variables.Standardized))

# Exclude rows with NAs
var.y <- var.y[!is.na(var.y$IPBES) & var.y$IPBES != "" & var.y$IPBES != 0,]

# split columns (since there are cases where there is more than one CES and variables per row separated by ";")
var.y <- separate_rows(var.y, IPBES, sep = ";")


# Select the 5 variables most used by CES
result <- var.y %>%
  group_by(IPBES) %>%
  slice_max(order_by = suma, n = 6) 
# %>% arrange(desc(suma))
  
# write the results
write.csv(result, "top_variables.csv")

# Sumar roads los valores de suma por Variables.Standardized
var.y %>%
  group_by(Variables.Standardized) %>%
  summarise(suma = sum(suma)) %>%
  arrange(desc(suma))

result %>%
  group_by(Variables.Standardized) %>%
  summarise(suma = sum(suma)) %>%
  arrange(desc(suma))

# show result
print(result)

##################################
######### 4) Models ##############
##################################

# Data frame of literature review
d<- read.delim("Database/csv files/savedrecs.csv", sep = ";", stringsAsFactors = FALSE, encoding = "latin1")

library(dplyr)
library(stringr)

# Load the base dataset
d <- read.delim("Database/csv files/savedrecs.csv", sep = ";",
                stringsAsFactors = FALSE, encoding = "latin1")

# Select only relevant columns and remove entries with missing or empty Authors
a <- d %>% select(ID, Authors, Publication.Year) %>%
  filter(!is.na(Authors), Authors != "")

# Function to extract only last names from each author string
extract_lastname <- function(author_string) {
  
  # Split authors by ';' (each entry corresponds to a person)
  persons <- unlist(strsplit(author_string, ";"))
  persons <- trimws(persons)
  
  # Possible surname prefixes commonly found in multi-word last names
  prefixes <- c("van", "von", "de", "del", "de la", "da", "di")
  
  # Apply surname extraction to each person
  lastnames <- sapply(persons, function(p) {
    
    # Split the person string by comma to detect different formats
    parts <- unlist(strsplit(p, ","))
    parts <- trimws(parts)
    
    if (length(parts) == 1) {
      # Format: "Kim Y."
      # Extract the first word (assumed to be the surname)
      return(strsplit(parts, " ")[[1]][1])
    } else {
      # Format: "Tieskens, Koen F." OR "K.F., Tieskens"
      
      # Case 1: surname comes first (no periods in the first element)
      if (!grepl("\\.", parts[1])) {
        ln <- parts[1]
      } else {
        # Case 2: initials first → surname is in parts[2]
        ln <- parts[2]
      }
      
      # Handle multi-word surnames with prefixes
      ln_words <- strsplit(ln, " ")[[1]]
      
      # If the first word is a known prefix, join it with the next word
      if (tolower(ln_words[1]) %in% prefixes && length(ln_words) > 1)
        ln <- paste(ln_words[1], ln_words[2])
      
      return(ln)
    }
  })
  
  return(lastnames)
}

# Create the 'author' column with formatted surnames (et al., and, etc.)
a$author <- sapply(1:nrow(a), function(i) {
  
  # Extract all surnames from the Authors field
  ln <- extract_lastname(a$Authors[i])
  
  # Formatting rules:
  if (length(ln) == 1) {
    # Single author → use surname only
    ln
  } else if (length(ln) == 2) {
    # Two authors → "Surname1 and Surname2"
    paste(ln[1], "and", ln[2])
  } else {
    # More than two authors → "Surname1 et al."
    paste(ln[1], "et al.")
  }
})

# Append the publication year to the author string
a$author <- paste(a$author, a$Publication.Year)

# improve some wrong assignments
a$author[a$Authors == "Van Berkel et al. 2018"] <- "van Berkel et al. 2018"



# Select the columns of interest
sel <- df %>% select(ID,Authors, Models.used, Presence.Abundance.model)

# join with the table of interest
sel2 <- merge(sel, a, by = c("ID"))


# Model Summary
models <- sel2 %>% group_by(ID, Models.used) %>% 
  select(ID,Models.used, author, Presence.Abundance.model) %>% unique()


# split columns (since there are cases where there is more than one Models per row separated by ";")
models <- separate_rows(models, Models.used, sep = ",")
models <- separate_rows(models, Models.used, sep = ";")

# Exclude rows with NAs
models <- models[!is.na(models$Models.used) & models$Models.used != "",]

# Identify duplicate IDs
models <- models %>%
  group_by(ID) %>%
  mutate(duplicated = n() > 1) %>%
  ungroup()

# Labeling models
models$du<- ifelse(models$duplicated, "Multiple models", "Single model")

# Manually set the best model based on the review
models[15, "du"] <- "Best performing model"#Hale
models[24, "du"] <- "Best performing model"#Zhao
models[33, "du"] <- "Best performing model"#Fox
models[68, "du"] <- "Best performing model"#Hamstead
models[74, "du"] <- "Best performing model"#Zhang




# Convert data into factors
models$ID <- as.factor(models$ID) 

unique(models$Models.used)

# Create a vector to replace the models with initials
reemplazo <- c(
  "MaxEnt" = "MaxEnt",
  "Random Forest" = "RF",
  "Global regression" = "GR",
  "GEM (Global Ecological Model)" = "GEM",
  "Logistic geographic weighted regression (GWR)" = "LGWR",
  "InVEST" = "INV",
  "Geographic Weighted Poisson Regression" = "GWPR",
  "Geographic Weighted Regression" = "GWR",
  "Simple Linear Regression" = "SLR",
  "Structural Equations Models" = "SEM",
  "Negative binomial regression" = "NBR",
  "Negative Binomial Regression" = "NBR",
  "Multivariate linear regression" = "MLR",
  "Multivariate regression" = "MLR",
  "Multiple Linear Regression" = "MLR",
  "Multiple linear regression" = "MLR",
  "Logarithmic model" = "LM",
  "Logistic regression" = "LR",
  "Logistic Regression" = "LR",
  "Spatial error model (SEM)" = "SEM",
  
  "Logistic geographic weighted regression" = "LGWR",
  "Dual Logarithmic model" = "DLM",
  "Generalized linear model" = "GLM", 
  "Generalized Linear Model" = "GLM",
  "Generalized Linear Models" = "GLM",
  "Generalized Additive Model" = "GAM", 
  "Generalized Boosting Model" = "GBM",
  "Classification Tree Analysis"  = "CTA" ,
  "Surface Range Envelop" = "SRE", 
  "Flexible Discriminant Analysis" = "FDA",
  "Artificial Neural Network" = "ANN",
  "Surface range envelop" = "SRE",                                      
  "Flexible discriminant analysis" = "FDA" ,                               
  "Multiple Adaptive Regression Splines" = "MARS",
  "Random effect meta-regression models" = "MRG",
  "Ordinary Least Squares (OLS)" = "OLS",
  "Geographical Random Forest" = "GRF",
  "Generalized Mixed-Effects Model" = "GLMM",
  "XGBoost" = "XGBoost"
                                    
  
)


# Create a new column with the initials
models$Iniciales <- reemplazo[as.character(models$Models.used)]

# como hay dos Zhao et al. 2023 reemplazar el primer por Zhao et al. 2023a y Zhao et al. 2023b
# Modificar valores específicos según la condición
models$author[models$ID == 6] <- "Zhao et al. 2023a"
models$author[models$ID == 30] <- "Zhao et al. 2023b"
models$author[models$ID == 145] <- "Zhang et al. 2022a"
models$author[models$ID == 477] <- "Zhao et al. 2022b"


# Sort alphabetically by author
models$author <- factor(models$author, levels = rev(sort(unique(models$author))))


## Plot models
ggplot(models, aes(y=author, x=Iniciales, color = du))+
  geom_point(size=2)+
  scale_color_manual(values = c("Best performing model" = "darkred", "Single model"= "darkgreen", "Multiple models"= "darkorange"))+
  theme_bw() + 
  labs(x= "Models", y= "Articles", color = "")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  guides(size = "none")  # Oculta la leyenda de tamaño

ggsave("Figures/6) Models_SM.png", width = 8, height = 7, dpi = 600)


##############################################
######### 5) Evaluation metrics ##############
##############################################


# Spreadsheet URL
# url <- "https://docs.google.com/spreadsheets/d/15jqksor_23ZRpqhGwfxFBika0phFHUcRLBwKH8dCzqQ/edit?usp=sharing"

# Read the specific 'Evaluation Metrics' sheet
data2 <- read.delim("Database/EvaluationMetrics.csv", sep = ";")#read_sheet(url, sheet = "EvaluationMetrics")

# Data frame of literature 
# data2<- read.delim("Database/Data_extraction.csv", sep = ";", stringsAsFactors = FALSE, encoding = "latin1")

# Exclude REVISAR, SOLVES and Undefined in Metrics column
eval_metric <- data2[data2$Metrics != "REVISAR" & data2$Metrics != "SOLVES" & data2$Metrics != "Undefined",]


# Create a vector to replace the metrics with initials
reemplazo2 <- c(
  "Sensitivity and Specificity" = "Sensitivity",  
  "Accuracy"= "Acc",
  "RMSE (Root Mean Squared Error)" = "RMSE",
  "McFadden and Nagelkerke pseudo R²" = "R²",# Join with R2
  "MSE (Mean Square Error)"= "MSE",
  "Gini Impurity"= "GI",
  "Pseudo R2", "pseudo R²" = "R²",# Join with R2
  "Adjusted R²" = "R²",# JOin with R2
  "Adjusted Log Likelihood" = "LL",
  "Log Likelihood" = "LogLik",
  "Likelihood Ratio" = "LRT",
  "Gain" = "Gain",
  "TSS (True Skill Statistic)" = "TSS",
  "R² (Coefficient of Determination)" ="R²" ,
  "AIC (Akaike Information Criterion)" = "AIC",
  "AIC (Corrected Akaike Information Criterion)" = "AICc",
  "AUC-ROC (Area Under the Receiver Operating Characteristic Curve)" = "AUC-ROC",
  "MAPE(meanabsolutepercentageerror)" = "MAPE",
  "MSE(Mean Square Error)" = "MSE",
  "Percentage of deviance" = "Deviance", #Join deviance
  "Percentage of explained deviance" = "Deviance",  #Join deviance
  "deviance explained (D2)"= "Deviance",  #Join deviance
  "Moran's I" = "Moran's I",
  "Kappa" = "Kappa",
  "Weight" = "Weight"
  
  
)

# Create a new column with the initials of metrics
eval_metric$Metrics2 <- reemplazo2[as.character(eval_metric$Metrics)]

# Create a new column with the initials of models (row 684 of the script)
eval_metric$Iniciales <- reemplazo[as.character(eval_metric$Models.used)]

# Exclude rows with NAs
eval_metric <- eval_metric[!is.na(eval_metric$Metrics) & eval_metric$Metrics != "",]

# Select unique values based on ID and Metrics
# eval_metric <- eval_metric %>% distinct(ID, Models.used, .keep_all = TRUE)


# Calculates the percentage of each metric using the  Metrics2 column
eval_metric_count <- eval_metric %>%
  group_by(Metrics2) %>%
  summarise(n = n(), .groups = 'drop') %>%
  mutate(prop = n / sum(n) * 100)


# Convert table to long format
data_sankey_eval <- eval_metric %>%
  make_long(Iniciales, Metrics2 )

# Exclude rows with NAs
data_sankey_eval <- data_sankey_eval[!is.na(data_sankey_eval$node) & data_sankey_eval$node != 0 & data_sankey_eval$node != "",] 

# unique values
unique(data_sankey_eval$node)

# Customize the order of labels
orden_variables <- c(
  # Evaluation metrics
  "Sensitivity and Specificity",  "Accuracy",
  "RMSE (Root Mean Squared Error)",
  "McFadden and Nagelkerke pseudo R²",
  "MSE (Mean Square Error)", "Gini Impurity",
  "Pseudo R2", "pseudo R²",
  "Adjusted R²",
  "Log Likelihood",
  "Gain (goodness of fit)",
  "TSS (True Skill Statistic)",
  "R² (Coefficient of Determination)" ,
  "AIC (Akaike Information Criterion)",
  "AUC-ROC (Area Under the Receiver Operating Characteristic Curve)",
  # Models
  "GBM", 
  "CTA", 
  "ANN",  
  "SRE", 
  "FDA", 
  "MARS", 
  "SEM", 
  "NBR", 
  "GR", 
  "LGWR",
  "LR",  
  "MRG", 
  "LM", 
  "DLM",
  "GLM",  
  "GAM",   
  "GWR",
  "MLR",
  "SLR",
  "RF",
  "MaxEnt",
  "XGBoost"
  
)




# Convert variables to a factor with custom order
# data_sankey_eval <- data_sankey_eval %>%
#   mutate(node = factor(node, levels = orden_variables))

# Create Sankey plot using ggplot2 and ggsankey
sankey_metrics<-ggplot(data_sankey_eval, aes(x = x, next_x = next_x, node = node, next_node = next_node, fill = factor(node), label = node)) +
  geom_sankey(flow.alpha = 0.5, node.color = "black",  width = 0.19, space = 14) +
  geom_sankey_label(size = 4, color = "white", fill = "black", space = 14) +
  theme_sankey(base_size = 16) +
  theme(legend.position = "none",
        axis.text.x = element_text(size = 22))+
  labs(x="")+
  scale_x_discrete(label = c("Models", "Evaluation metrics"))

sankey_metrics


####################################################
# Value of metrics 
####################################################


# Read the specific 'Evaluation Metrics' sheet
eval_metric_values <- read.delim("Database/EvaluationMetrics.csv", sep = ";", stringsAsFactors = FALSE, encoding = "latin1")

# Convert Metrics values column to numeric
eval_metric_values$Metrics.values <- as.numeric(eval_metric_values$Metrics.values)


# Convert to long format using pivot_longer
eval_metric_values_long <- eval_metric_values %>%
  pivot_longer(cols = c("Metrics.values", "Metrics.values2"), 
               names_to = "Metric Type", 
               values_to = "Metric Value")

# Exclude rows with NAs
eval_metric_values_long <- eval_metric_values_long[!is.na(eval_metric_values_long$`Metric Value`),]

# Select AUC values
auc_values <- eval_metric_values_long %>% filter(`Metrics` == "AUC-ROC (Area Under the Receiver Operating Characteristic Curve)")


# Histogram for AUC
auc_g<- ggplot(auc_values, aes(x = `Metric Value`)) +
  geom_histogram(binwidth = 0.05, fill = "skyblue", color = "black") +
  labs(title = "",
       x = "AUC value",
       y = "Frequency") +
  theme_minimal()+
  scale_y_continuous(position = "right")

auc_g

# Histogram for R2
r2_values <- eval_metric_values_long %>% filter(`Metrics` == "R² (Coefficient of Determination)")

# Since there were some r2 values used to evaluate random forest and with negative values, we excluded these values because they were not consistent with what is typically reported for these models.
r2_values <- r2_values %>% filter(`Metric Value` >= 0)


r2_g<- ggplot(r2_values, aes(x = `Metric Value`)) +
  geom_histogram(binwidth = 0.05, fill = "skyblue", color = "black") +
  labs(title = "",
       x = "R² (Coefficient of Determination)",
       y = "Frequency") +
  theme_minimal()+
  scale_x_continuous(limits = c(0, 1))+
  scale_y_continuous(position = "right")


# Calculate the mean and standard deviation of the AUC and R2 values
mean(auc_values$`Metric Value`, na.rm = TRUE)
sd(auc_values$`Metric Value`, na.rm = TRUE)

mean(r2_values$`Metric Value`, na.rm = TRUE)
sd(r2_values$`Metric Value`, na.rm = TRUE)


# Combine three graphs using cowplot
combined_plot <- ggdraw() +
  draw_plot(sankey_metrics, 0, 0, 0.9, 0.9) +
  draw_plot(auc_g, 0.77, 0.26, 0.14, 0.22) +
  draw_plot(r2_g, 0.77, 0.57, 0.14, 0.22)

# print combined plot 
print(combined_plot)
ggsave( "Figures/7) Sankey_Evaluation metrics_hist.png", width = 15, height = 9, dpi = 600)


# Another option with values
combined_plot <- ggdraw() +
  draw_plot(sankey_metrics, 0, 0, 1, 1) +
  draw_text("mean = 0.84\n sd=0.13", x = 0.80, y = 0.40, size = 12, color = "black")+
  draw_text("mean = 0.42\n sd=0.22", x = 0.80, y = 0.72, size = 12, color = "black")


# print combined plot 
print(combined_plot)

ggsave( "Figures/7) Sankey_Evaluation metrics.png", width = 16, height = 8, dpi = 600)


##########################################
############ 6) Presence/Abundance #######
##########################################

# Select the columns of interest
pres <- df %>% group_by(ID, Presence.Abundance.model) %>% 
  select(ID, Presence.Abundance.model) %>% unique()

# Exclude rows with NAs
pres <- pres[!is.na(pres$Presence.Abundance.model) & pres$Presence.Abundance.model != "",] 

# split columns (since there are cases where there is more than one presence per row separated by ";")
pres <- separate_rows(pres, Presence.Abundance.model, sep = ";")

# Customize the order of labels
custom_order <- c("Presence", "Abundance")

# Convert variables to a factor with custom order 
pres$Presence.Abundance.model <- factor(pres$Presence.Abundance.model, levels = custom_order)

# Calculate the percentages
pres_count <- pres %>%
  group_by(Presence.Abundance.model) %>%
  summarise(n = n(), .groups = 'drop') %>%
  mutate(prop = n / sum(n) * 100)

# Calculate positions for labels
pres_count <- pres_count %>% 
  arrange(desc(Presence.Abundance.model)) %>%
  mutate(lab.ypos = cumsum(prop) - 0.5*prop)

# Create the pie chart
ab <- ggplot(pres_count, aes(x = "", y = prop, fill = Presence.Abundance.model)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar(theta = "y") +
  labs(fill = "", title = "Presence/Abundance") +
  theme_void() +
  scale_fill_manual(values = c("Abundance" = "#E69F00", "Both" = "#56B4E9", "Presence" = "#009E73")) +
  geom_label(aes(y = lab.ypos, label = paste0(round(prop, 0), "%")), 
             fill = "white", color = "black", size = 5, alpha = 0.7)


ab

##########################################
##### Join with models ###################
##########################################

# Sort and summarize data
models_summary <- models %>% 
  group_by(Models.used, Iniciales, Presence.Abundance.model) %>% 
  summarise(mod = n()) %>% 
  arrange(desc(mod))

# Convert Models.used to factor with the desired order
models_summary$Models.used <- factor(models_summary$Models.used, levels = unique(models_summary$Models.used))
models_summary$Iniciales <- factor(models_summary$Iniciales, levels = unique(models_summary$Iniciales))

# split columns (since there are cases where there is more than one presence per row separated by ";")
models_summary <- separate_rows(models_summary, Presence.Abundance.model, sep = ";")


# Plot 
m<- ggplot(models_summary, aes(x = Iniciales, y = mod, fill = Presence.Abundance.model)) +
  geom_col() +              
  theme_bw() +              
  labs(x = "Models", y = "Number of articles")+
  scale_fill_manual(values = c("Abundance" = "#E69F00", "Both" = "#56B4E9", "Presence" = "#009E73")) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  theme(legend.position = "none")  

m

# Combine both graphs using cowplot
combined_plot <- ggdraw() +
  draw_plot(m, 0, 0, 1, 1) +
  draw_plot(ab, 0.7, 0.65, 0.28, 0.33) # adjust coordinates and size as needed

# print plot
print(combined_plot)

# Save figure
ggsave("Figures/8) Models_summarise.png", width = 8, height = 4, dpi = 600)


sum(models_summary$mod)

## Porcentajes de Modelos para el paper
models_summary <- models_summary %>% 
  group_by(Iniciales) %>% 
  mutate(prop = mod / 90 * 100) %>% 
  ungroup()





## Alternative plot 
# Sort and summarize data
models_summary2 <- models %>% 
  group_by(Iniciales) %>% 
  summarise(mod = n()) %>% 
  arrange(desc(mod))

# Convert Models.used to factor with the desired order
# models_summary2$Models.used <- factor(models_summary2$Models.used, levels = unique(models_summary2$Models.used))
models_summary2$Iniciales <- factor(models_summary2$Iniciales, levels = unique(models_summary2$Iniciales))

# split columns (since there are cases where there is more than one presence per row separated by ";")
models_summary2 <- separate_rows(models_summary2, sep = ";")

# Create a new column for identified the multples models
models_summary2 <- models_summary2 %>%
  mutate(multiple = ifelse(Iniciales %in% c("RF", "DLM", "MLR"), "Multiple", "Single model"))

models_summary2 <- models_summary2 %>%
  mutate(mod2 = ifelse(multiple == "Multiple", mod - 1, mod)) %>% # Crear la columna mod2 inicial
  bind_rows(
    filter(models_summary2, multiple == "Multiple") %>% # Filtrar filas donde mod es "Multiple"
      mutate(mod2 = 1) # Cambiar mod2 a 1 para las filas duplicadas
  ) %>%
  arrange(desc(mod == "Multiple"), row_number())

models_summary2 <- models_summary2 %>%
  mutate(multiple2 = ifelse(mod2 == 1 & multiple == "Multiple", "Best model", "Single model"))



# Plot 
m2<- ggplot(models_summary2, aes(x = Iniciales, y = mod2, fill = as.factor(multiple2))) +
  geom_col() +              
  theme_bw() +              
  labs(x = "Models", y = "Number of articles", fill = "")+
  scale_fill_manual(values = c("Best model" = "#E69F00",  "Single model" = "#009E73")) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
  # theme(legend.position = "none")  

m2

ggsave("Figures/9) Models_Principal_Figure.png", width = 8, height = 4, dpi = 600)


###########################################
############# 7) Source ###################
###########################################


# Group by ID and Social.media.source.data, select specific columns and remove duplicates
source <- df %>%
  select(ID, Social.media.source.data, Type.of.social.media.data) %>%
  distinct()

# Exclude rows with NAs
source <- source %>%
  filter(!is.na(Social.media.source.data) & Social.media.source.data != "")

# split columns (since there are cases where there is more than one source per row separated by ";")
source <- source %>%
  separate_rows(Social.media.source.data, sep = ";") %>%
  separate_rows(Type.of.social.media.data, sep = ";")

# Group and count souces
source <- source %>%
  group_by(Social.media.source.data, Type.of.social.media.data) %>%
  summarise(sourc = n()) %>%
  ungroup() %>%
  arrange(desc(sourc))


# Plot
source_g <-ggplot(source, aes(x = reorder(Social.media.source.data, -sourc), y = sourc, fill = Type.of.social.media.data)) +
  geom_col() +
  theme_bw() +
  labs(x = "Social media data source", y = "Number of articles") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme(legend.position = "none")+
  scale_fill_manual(values = c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00"))


source_g


# Group only by source
source_b <- source %>%
  group_by(Social.media.source.data) %>%
  summarise(sourc = sum(sourc)) %>%
  arrange(desc(sourc))

# Calculate the percentages
source_b <- source_b %>%
  mutate(prop = sourc / sum(sourc) * 100)

##########################
######SOURCE VS PAPERS###
##########################

# join with the table of interest
sel <- df %>% select(ID,Authors, Social.media.source.data)
sel2 <- merge(sel, a, by = c("ID"))

source2 <- sel2 %>% group_by(ID, Social.media.source.data) %>% 
  select(ID,Social.media.source.data, author) %>% unique()

# Exclude rows with NAs
source2 <- source2[!is.na(source2$Social.media.source.data) & source2$Social.media.source.data != "",]

# split columns (since there are cases where there is more than one source and social media per row separated by ";")
source2 <- separate_rows(source2, Social.media.source.data, sep = ";")

# Identify duplicate IDs
source2 <- source2 %>%
  group_by(ID) %>%
  mutate(duplicated = n() > 1) %>%
  ungroup()

# Rename the column
source2$du<- ifelse(source2$duplicated, "Multiple source", "Single source")


# Sort alphabetically by author
source2$author <- factor(source2$author, levels = rev(sort(unique(source2$author))))


# Plot by ID
ggplot(source2, aes(y=author, x=Social.media.source.data, color=du))+
  geom_point(size=2)+
  theme_bw() + 
  scale_color_manual(values = c("Single source"= "darkgreen", "Multiple source"= "darkorange"))+
  labs(x= "Social media data source", y= "Articles", color = "")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  guides(size = "none")  

# save plot 
ggsave("Figures/10) Source by paper.png", width = 8, height = 7, dpi = 600)

###########################################
############# 8) Type Social media ########
###########################################

type <- df %>% group_by(ID, Type.of.social.media.data) %>% 
  select(ID,Type.of.social.media.data) %>% unique()

# split columns (since there are cases where there is more than one type of social media per row separated by ";")
type <- separate_rows(type, Type.of.social.media.data, sep = ";")


# change the values text for Text in the column Type.of.social.media.data
type$Type.of.social.media.data <- ifelse(type$Type.of.social.media.data == "text", "Text", type$Type.of.social.media.data)

# Count the number of occurrences for each category
type_count <- type %>%
  group_by(Type.of.social.media.data) %>%
  summarise(n = n(), .groups = 'drop') %>%
  mutate(prop = n / sum(n) * 100)

# Calculate positions for labels
type_count <- type_count %>%
  arrange(desc(Type.of.social.media.data)) %>%
  mutate(ypos = cumsum(n) - 0.5 * n)

# Create the pie chart
type_social_media<- ggplot(type_count, aes(x = "", y = n, fill = Type.of.social.media.data)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar(theta = "y") +
  
  geom_label(aes(y = ypos, label = paste0(round(prop, 0), "%")), 
             fill = "white", color = "black", size = 3, alpha = 0.7)+

  labs(fill = "", title = "Type of social media data", title.x = 0.5, title.y = 1.2 ,size=10) +
  theme_void() +
  theme(plot.title = element_text(size = 8))+
  scale_fill_manual(values = c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00"))

type_social_media

# Combine both graphs using cowplot
combined_plot <- ggdraw() +
  draw_plot(source_g, 0, 0, 1, 1) +
  draw_plot(type_social_media, 0.54, 0.56, 0.45, 0.45) # adjust coordinates and size as needed5

# print plot
print(combined_plot)

# Save plot 
ggsave("Figures/11) Source.png", width = 7, height = 4, dpi = 600)



###########################################
############# 9) Spatial cover ###########
###########################################


spatial_cover <- df %>% group_by(ID, Spatial.cover) %>% 
  select(ID,Spatial.cover) %>% unique()

# Exclude rows with NAs
spatial_cover <- spatial_cover[!is.na(spatial_cover$Spatial.cover) & spatial_cover$Spatial.cover != "",]

# see the options
unique(spatial_cover$Spatial.cover)

# split columns (since there are cases where there is more than one spatial cover per row separated by ";")
spatial_cover <- separate_rows(spatial_cover, Spatial.cover, sep = ";")


# Plot Spatial cover
s<- ggplot(spatial_cover, aes(x = "", fill = Spatial.cover)) +
  geom_bar(width = 1) +
  coord_polar(theta = "y") +
  labs(fill = "", title = "Spatial cover") +
  theme_void()+
  scale_fill_manual(values = c("gray20", "gray40", "gray60", "gray80","gray100" , "gray90"))


s

###########################################
############# 10) Protected Area ###########
###########################################

protected <- df %>% group_by(ID, Protect.area) %>% 
  select(ID,Protect.area) %>% unique()

# Exclude rows with NAs
protected <- protected[!is.na(protected$Protect.area) & protected$Protect.area != "",]


# Count the number of occurrences for each category
protected_count <- protected %>%
  group_by(Protect.area) %>%
  summarise(n = n(), .groups = 'drop') %>%
  mutate(prop = n / sum(n) * 100)

# Customize the order of labels
custom_order <- c("yes", "no", "partially", "unmentioned")

# Convert Supply.demand.CES to a factor with custom order
protected_count$Protect.area <- factor(protected_count$Protect.area, levels = custom_order)

# Calculate positions for labels
protected_count <- protected_count %>%
  arrange(desc(Protect.area)) %>%
  mutate(ypos = cumsum(prop) - 0.5 * prop)

# plot 
p <- ggplot(protected_count, aes(x = "", y = prop, fill = Protect.area)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar(theta = "y") +
  geom_text(aes(y = ypos, label = paste0(round(prop, 0), "%")),
            color = "white", size = 3) +
  labs(fill = "", title = "Protected area") +
  theme_void() +
  scale_fill_manual(values = c("gray20", "gray40", "gray60", "gray80"))

p

#######################################################
##### 11) Year of publication #########################
#######################################################


# read the dataframe 
d<- read.delim("Database/csv files/savedrecs.csv", sep = ";", stringsAsFactors = FALSE, encoding = "latin1")

# First step: filter only the papers that were analyzed in depth
d<- d %>% filter(Final.extraction=="yes")

# select rows of interest
publication_year <- d %>% select(Publication.Year)

# count papers by year 
publication_year <- publication_year %>%
  group_by(Publication.Year) %>% 
  summarise(n=length(Publication.Year))

# drop na
publication_year <- drop_na(publication_year)

# plot 
ggplot(publication_year, aes(Publication.Year, n, group = 1))+
  geom_line()+
  geom_point()+
  theme_bw()+
  labs(y= "count", x= "year")


publication_year$Publication.Year<-as.numeric(publication_year$Publication.Year)



y<- ggplot(publication_year, aes(Publication.Year, n, group = 1))+
  geom_line()+
  geom_point()+
  theme_bw()+
  labs(y= "Number of articles", x= "year")+
  theme(
    panel.background = element_rect(fill = "transparent", colour = NA), # Fondo del panel transparente
    plot.background = element_rect(fill = "transparent", colour = NA),  # Fondo de la trama transparente
    legend.background = element_rect(fill = "transparent", colour = NA),# Fondo de la leyenda transparente
    legend.box.background = element_rect(fill = "transparent", colour = NA),
    panel.grid.major = element_blank(), # Elimina la grilla principal
    panel.grid.minor = element_blank()  # Elimina la grilla secundaria
  ) +
  scale_x_continuous(breaks = c(2018,2022), limits = c(2015, 2025))
# scale_x_continuous(breaks = scales::pretty_breaks(n = 4))

print(y)

ggsave("Figures/12) Publication year.png", width = 8, height = 4, dpi = 600)


###########################################
############# 12) Countries ################
###########################################

# Select the columns of interest
countries <- df %>% group_by(ID, Country) %>% 
  select(ID, Country) %>% unique()

# Exclude rows with NAs
countries <- countries[!is.na(countries$Country) & countries$Country != "",] 


# split columns (since there are cases where there is more than one country per row separated by ";")
countries <- separate_rows(countries, Country, sep = ";")

# unique(countries$Country)

# Countries count
countries <- countries %>% group_by(Country) %>% 
  summarise(count= n()) %>% 
  arrange(desc(count))

paises <- countries
paises <- paises %>% rename(country= Country)

# Reemplace United States by United States of America and Korea, Republic of by South Korea
paises$country <- ifelse(paises$country == "United States", "United States of America", paises$country)
paises$country <- ifelse(paises$country == "Korea, Republic of", "South Korea", paises$country)


# Load Countries 
world <- map_data("world")

countries <- ne_countries(scale = "medium", returnclass = "sf")#st_as_sf(wrld_simpl)

# Join df
df_plot <- left_join(countries, paises %>% group_by(country),
                     by = c("name" = "country"))


# Create the chart using the defined palette
# Define custom color palette
palette <- c('#fef0d9','#fdcc8a','#fc8d59','#d7301f')

# Plot 
map_literature_review <- ggplot(df_plot) + 
  geom_sf(aes(fill = cut(count, breaks = c(0,3,6,9,19), 
                         labels = c("1-3","4-7", "7-10", "10-19")))) +
  scale_fill_manual(values = palette, na.value = "gray95") +  # Gris claro para países sin datos
  labs(fill = "Number of articles") +  # Cambia la etiqueta de la leyenda aquí
  theme_bw() +  # Fondo blanco
  theme(legend.position = "right", 
        panel.grid = element_blank(), 
        axis.text = element_text(size = 8),
        legend.title = element_text(size = 10)) +  # Ajustes de leyenda
  coord_sf(expand = FALSE)

map_literature_review


# Combine three graphs using cowplot
combined_plot <- ggdraw() +
  draw_plot(map_literature_review, 0, 0, 1, 1) +
  draw_plot(y, 0.05, 0.28, 0.20, 0.23)+
  draw_text("Spatial cover: regional 60%, local 28%, national 12%", x = 0.58, y = 0.25, size = 12, color = "black")

# print combined plot 
print(combined_plot)

# save plot
ggsave("Figures/13) Maps.png", width = 9, height = 6, dpi = 600)
  
####################################################
############ 13) Changes over time #################
####################################################

changes <- df %>% group_by(ID, Evaluate.changes.over.time) %>% 
  select(ID, Evaluate.changes.over.time) %>% unique()

# Exclude rows with NAs
changes <- changes[!is.na(changes$Evaluate.changes.over.time) & changes$Evaluate.changes.over.time != "",] 

# plot changes over time 
ggplot(changes, aes(x = "", fill = Evaluate.changes.over.time)) +
  geom_bar(width = 1) +
  coord_polar(theta = "y") +
  labs(fill = "", title = "Evaluate changes over time?") +
  theme_void()+
  scale_fill_manual(values = c("gray20", "gray80"))



#######################################################
############## 14) Ecosystem type ######################
#######################################################

# select columns of interest
ecosystem <- df %>% group_by(ID, Ecosystem.type.1, Ecosystem.type.2, Ecosystem.type.3) %>% 
  select(ID, Ecosystem.type.1, Ecosystem.type.2, Ecosystem.type.3, Protect.area) %>% unique()

# melt the table
ecosystem2 <- melt(ecosystem, id.vars = c("ID", "Protect.area") )

# Exclude rows with NAs
ecosystem2 <- ecosystem2[!is.na(ecosystem2$value) & ecosystem2$value != "",]

# count of ecosystem
eco <- ecosystem2 %>%
  group_by(value, Protect.area) %>%
  summarise(ecosy = n(), .groups = 'drop') %>%
  arrange(desc(ecosy)) %>% 
  mutate(orden = row_number()) 

eco <- eco %>%
  arrange(desc(ecosy))

# Convert 'Protect.area' to factor with custom order
eco$Protect.area <- factor(eco$Protect.area, levels = c("yes", "no", "partially", "unmentioned"))  # Cambia el orden según tus datos


# Plot
eco_g <- eco %>% 
  group_by(value) %>% 
  mutate(sum = sum(ecosy)) %>%
  ungroup() %>% 
  mutate(value = forcats::fct_reorder(value, sum)) %>%
  ggplot() + 
  geom_bar(aes(y= value, x = ecosy, fill = Protect.area), stat = "identity")+
  theme_minimal()+
  scale_fill_manual(values = c( "yes" = "#009E73" , "no" = "#E69F00" , "partially" = "#56B4E9", "unmentioned" = "#F0E442"
                                 ))+
  scale_x_continuous(labels = scales::number_format(accuracy = 1))+ 
  # theme(
  #   axis.text = element_text(size = 12),
  #   axis.title = element_text(size = 14),
  #   legend.title = element_text(size = 12),
  #   legend.text = element_text(size = 10),
  #   legend.position = c(0.85, 0.15), # Coordenadas x, y (relativo al gráfico)
  #   # legend.background = element_rect(fill = "white", color = "black"),
  #   legend.key.size = unit(0.6, "cm") # Ajusta el tamaño de los cuadros de la leyenda
  # )+
  labs(title = "",
       x = "Count of articles by ecosystem type",
       y = "Ecosystem type", fill= "Protected area")+
  guides(fill = guide_legend(title = "Protected area:", nrow = 1))+
  theme(legend.position = "bottom")
  

eco_g


# save the plot 
ggsave("Figures/14) Ecosystem.png", width = 8, height = 5, dpi = 600)


# Calculate the percentages 
eco_percentage <- eco %>% 
  group_by(value) %>%
  summarise(ecosy = sum(ecosy)) %>%
  mutate(prop = ecosy / sum(ecosy) * 100)

###################################
##### B)ANALYSIS OF THE PAPERS#####
###################################


# read the dataframe 
d<- read.delim("Database/csv files/savedrecs.csv", sep = ";", stringsAsFactors = FALSE, encoding = "latin1")

# First step: filter only the papers that were analyzed in depth

d<- d %>% filter(Final.extraction=="yes")


####################################################
############### 15) Autores ########################
####################################################

# Use the Authors data create below call 'a'

# Group by first author and count the number of papers
aut_res <- a %>%
  group_by(author) %>%
  summarise(n = n()) %>%
  arrange(desc(n)) %>% # Sort by number of jobs in descending order
  slice_head(n = 10)   # Select the first 10 authors

# Plot with ggplot2
ggplot(aut_res, aes(x = reorder(author, n), y = n)) +
  geom_col() +
  coord_flip() +  
  labs(title = "",
       x = "Author",
       y = "articles numbers") +
  theme_minimal()

ggsave("Figures/15) Authors.png", width = 8, height = 4, dpi = 600)

####################################################
################ 16) Title #########################
####################################################
library(tm)
library(wordcloud)
library(RColorBrewer)
library(textstem)

# select rows of interest
title<- d %>% select(Article.Title)

# Extract the titles
titles <- title$Article.Title

# Create a Corpus from the titles
corpus <- Corpus(VectorSource(titles))

# Text preprocessing
corpus <- tm_map(corpus, content_transformer(tolower)) # Convert to lowercase
corpus <- tm_map(corpus, removePunctuation)            # Remove punctuation
corpus <- tm_map(corpus, removeNumbers)                # Remove numbers
corpus <- tm_map(corpus, removeWords, stopwords("english")) # Remove English stopwords
corpus <- tm_map(corpus, content_transformer(lemmatize_strings)) # Lemmatize the text

# Add your own words to delete
# custom_stopwords <- c("word1", "word2") # Replace 'word1', 'word2' with the words you want to exclude
# corpus <- tm_map(corpus, removeWords, custom_stopwords)

# Create a matrix of document terms
dtm <- TermDocumentMatrix(corpus)
matrix <- as.matrix(dtm)
word_freqs <- sort(rowSums(matrix), decreasing = TRUE)
title_words <- data.frame(word = names(word_freqs), freq = word_freqs)

# Save the word cloud as a PNG file
png("Figures/16) Title.png", width = 800, height = 600, res=100)

# Generate the word cloud
set.seed(1234) 
wordcloud(words = title_words$word, freq = title_words$freq, min.freq = 2,
          max.words = 200, random.order = FALSE, rot.per = 0.35,
          colors = brewer.pal(8, "Dark2"))

# Close the graphic device
dev.off()

#######################################################
################ 17) Abstract #########################
#######################################################

# select rows of interest
abst<- d %>% select(Abstract)

abst <- abst$Abstract

# Create a Corpus from the abstract
corpus <- Corpus(VectorSource(abst))


# Text preprocessing
corpus <- tm_map(corpus, content_transformer(tolower)) # Convert to lowercase
corpus <- tm_map(corpus, removePunctuation)            # Remove punctuation
corpus <- tm_map(corpus, removeNumbers)                # Remove numbers
corpus <- tm_map(corpus, removeWords, stopwords("english")) # Remove English stopwords
corpus <- tm_map(corpus, content_transformer(lemmatize_strings)) # Lemmatize the text

# Add your own words to delete
# custom_stopwords <- c("word1", "word2") # Replace 'word1', 'word2' with the words you want to exclude
# corpus <- tm_map(corpus, removeWords, custom_stopwords)

# Create a matrix of document terms
dtm <- TermDocumentMatrix(corpus)
matrix <- as.matrix(dtm)
word_freqs <- sort(rowSums(matrix), decreasing = TRUE)
abstract_words <- data.frame(word = names(word_freqs), freq = word_freqs)

# Save the word cloud as a PNG file
png("Figures/17) Abstract.png", width = 800, height = 600)

# Generate the word cloud
set.seed(1234)
wordcloud(words = abstract_words$word, freq = abstract_words$freq, min.freq = 1,
          max.words = 200, random.order = FALSE, rot.per = 0.35,
          colors = brewer.pal(8, "Dark2"))

# Close the graphic device
dev.off()


#################################################
##### 18) KEY WORDS #############################
#################################################

key<- d %>% select(Author.Keywords)

# separar en filas por ;
key <- separate_rows(key, Author.Keywords, sep = ";")

# Remove NAs and empty strings
key <- key %>% filter(!is.na(Author.Keywords) & Author.Keywords != "")

# To lowercase
key <- key %>% mutate(Author.Keywords = tolower(Author.Keywords))

# Eliminate spaces at the beginning and end of the words
key <- key %>% mutate(Author.Keywords = trimws(Author.Keywords))


# Count the number of occurrences for each keyword
key <- key %>%
  group_by(Author.Keywords) %>%
  summarise(freq = n()) %>%
  arrange(desc(freq))


# Save the word cloud as a PNG file
png("Figures/18) Key.png",  width = 1800, height = 1200, res=150)

# Create the word cloud
wordcloud(key$Author.Keywords, key$freq, min.freq=1,
          max.words = 150, random.order=FALSE, rot.per=0.35, 
          scale=c(4, 0.5), # Ajusta la escala del tamaño
          colors=brewer.pal(8, "Dark2"))

# close graph
dev.off()



####################################################
############## 19) Journal #########################
####################################################

# select rows of interest
Journal<- d %>% select(Source.Title)

# count of number of journal
Journal <- Journal %>% group_by(Source.Title) %>% 
  summarise(n =length(Source.Title))

# Order based on the count 
Journal<- Journal %>% arrange(desc(n))

# Save the word cloud as a PNG file
png("Figures/19) Journal.png", width = 800, height = 600)
# key <- key[2:50,]

# Generate the word cloud
wordcloud(Journal$Source.Title, Journal$n, scale=c(5,0.5), min.freq=2,
          random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))
# Close the graphic device
dev.off()

## another option
library(treemap)

# select 10
Journal <- Journal %>% 
  slice(1:10) 


png("Figures/20) Journal_tree.png", width = 800, height = 600)

# Create the treemap with additional customizations
treemap(Journal,
        index = "Source.Title",                 # Category to display
        vSize = "n",                    # Size of the rectangles
        vColor = "n",                   # Color based on the value
        type = "value",                     # Treemap type
        title = "",    # Chart title
        palette = "Spectral",               # Color palette (example: Spectral)
        border.col = "white",               # Border color
        fontsize.title = 16,                # Font size for the title
        fontsize.labels = 8,               # Font size for the labels
        fontcolor.labels = "black",         # Label color
        fontface.labels = 2,                # Font style for the labels (2 for bold)
        bg.labels = "transparent",          # Transparent background for the labels
        align.labels = list(c("center", "center")), # Alignment of the labels
        inflate.labels = TRUE,               # Allows label inflation
        fontsize.legend = 20               # Font size for the legend
)

# Close the graphical device
dev.off()

####################################################
############## 20) Prisma Figure####################
####################################################

# Prisma metrics
# Data frame of literature 
d<- read.delim("Database/csv files/savedrecs.csv", sep = ";", stringsAsFactors = FALSE, encoding = "latin1")

# Number of papers
length(unique(d$ID))

## Number of Scopus papers
length(unique(d$ID[d$Fuente == "scopus"]))

## Numer of Web of Science papers
length(unique(d$ID[d$Fuente == "wos"]))

# Number of papers after removing not including CES social media data or spatial data
length(unique(d$ID[d$Relevant == "yes" & d$Spatial == "yes"]))

length(unique(d$ID[d$Relevant == "no"]))


# Number of not relevant and not spatial papers
length(unique(d$ID[d$Relevant == "no" & d$Spatial == "not evaluated"]))

# Number of papers that includes spatial modeling of CES
length(unique(d$ID[d$Includes.models =="yes"]))

# Number of papers that not includes spatial modeling of CES
length(unique(d$ID[d$Includes.models =="no" & d$Includes.models == "not evaluated"]))

# Number of final papers analized
length(unique(d$ID[d$Final.extraction =="yes"]))



png("Figures/21) Prisma Article.png", width = 800, height = 600)
library(PRISMAstatement)

prisma(439, 313, 590, 590, 146, 444,77, 73, 67,
       labels = list(
         found =                "Records identified through\n database searching\n (n = 439) \n(Scopus)", 
         found_other =          "Records identified through\n database searching\n  (n = 313)\n (Web of Science)", 
         no_dupes =             "Records after\n duplicates removed\n (n = 590)", 
         screened =             "Records screened\n (n = 146)", 
         screen_exclusions =    "Records excluded (n = 444)\n for not including CES or social media data",
         full_text =            "Full-text articles assessed\n for eligibility\n (n = 73)", 
         full_text_exclusions = "Articles excluded (n = 73) \n for not including spatial modeling of CES ", 
         qualitative =          "Articles included in\n quantitative synthesis\n (meta-analysis)\n (n = 67)", 
         quantitative =         "Articles included in\n qualitative synthesis\n (meta-analysis)\n (n = 67)"))





dev.off()

##############################################################
##########################END OF SCRIPT#######################
##############################################################




