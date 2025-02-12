# load the required libraries
library(tidyverse)
library(openxlsx)
library(bibliometrix)


# load the data
df<- read.delim("Database/csv files/scopus.csv", sep = ",")


# Select only the columns that are present in the WOS format
df2<- df %>% select( Authors, Author.full.names, Title, Source.title,Language.of.Original.Document, Document.Type,Conference.name, Conference.date, Conference.location, Author.Keywords, Index.Keywords, Abstract, Correspondence.Address, Affiliations, Author.s..ID,  Funding.Details, Funding.Texts, Cited.by,Publisher, ISSN, ISBN, Abbreviated.Source.Title, Year, Volume, Issue, Page.start, Page.end, Art..No., DOI, Link,  Page.count, Open.Access)


# rename the columns to match the WOS format
names(df2) <- c("Authors", "Author Full Names", "Article Title", "Source Title", "Language", "Document Type", "Conference Title", "Conference Date", "Conference Location","Author Keywords", "Keywords Plus", "Abstract", "Addresses","Affiliations", "Researcher Ids", "Funding Orgs", "Funding Text","Cited Reference Count" , "Publisher","ISSN", "ISBN", "Journal Abbreviation" ,"Publication Year", "Volume", "Issue", "Start Page", "End Page", "Article Number", "DOI","DOI link", "Number of Pages", "Open Access" ) 

# Create a new column for the WoS categories
new_column_names <- c("Publication Type", "Authors", "Book Authors", "Book Editors", "Book Group Authors", "Author Full Names", "Book Author Full Names", "Group Authors", "Article Title", "Source Title", "Book Series Title", "Book Series Subtitle", "Language", "Document Type", "Conference Title", "Conference Date", "Conference Location", "Conference Sponsor", "Conference Host", "Author Keywords", "Keywords Plus", "Abstract", "Addresses", "Affiliations", "Reprint Addresses", "Email Addresses", "Researcher Ids", "ORCIDs", "Funding Orgs", "Funding Name Preferred", "Funding Text", "Cited References", "Cited Reference Count", "Times Cited, WoS Core", "Times Cited, All Databases", "180 Day Usage Count", "Since 2013 Usage Count", "Publisher", "Publisher City", "Publisher Address", "ISSN", "eISSN", "ISBN", "Journal Abbreviation", "Journal ISO Abbreviation", "Publication Date", "Publication Year", "Volume", "Issue", "Part Number", "Supplement", "Special Issue", "Meeting Abstract", "Start Page", "End Page", "Article Number", "DOI", "DOI Link", "Book DOI", "Early Access Date", "Number of Pages", "WoS Categories", "Web of Science Index", "Research Areas", "IDS Number", "Pubmed Id", "Open Access Designations", "Highly Cited Status", "Hot Paper Status", "Date of Export", "UT (Unique WOS ID)", "Web of Science Record")

# Obtain the current column names of the dataframe
current_column_names <- names(df2)

# Verify if each column name in the new list exists in the dataframe
# If it does not exist, add that column to the dataframe with NA values
for (column_name in new_column_names) {
  if (!(column_name %in% current_column_names)) {
    df2[[column_name]] <- NA
  }
}

# order the columns in the dataframe
df2 <- df2[, new_column_names]


# write the dataframe to an excel file
write.xlsx(df2, file = "Database/csv files/scopusWosFormat.xls", rowNames = FALSE)

# load WOS database
df3 <- read.delim("Database/csv files/savedrecs.csv", sep = ",", encoding = "latin1")
df3 %>% names()

# Select the columns that are present in the Both databases
intersect(names(df3), names(df2))

# Select only the columns that are present in the Scopus database
df5 <- df3 %>% select(intersect(names(df3), names(df2)))

##  join the two databases
df6 <- bind_rows(df3, df5)

# Escribir los datos en un archivo Excel
write.xlsx(df6, file = "Database/csv files/Joined.xls", rowNames = FALSE)

############################
# Another option
############################

S = convert2df("scopus (3).bib", dbsource = "scopus", format = "bibtex")
W = convert2df("savedrecs (1).bib", dbsource = "wos", format = "bibtex")

Database = mergeDbSources(S, W, remove.duplicated = TRUE)

dim(Database)

write.xlsx(Database, file = "database.xlsx")

# Explore the database with biblioshiny
biblioshiny()
