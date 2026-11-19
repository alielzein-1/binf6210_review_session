# Removing past data in global env ---- 
rm(list = ls())

# Working directories ----
getwd() # used to get working directory
setwd() # used to set working directory

# Installing packages ----
install.packages("") # used to install packages 
#install.packages("tidyverse") 
#install.packages("viridis") 

library() # used for loading installed packages 
library(tidyverse)
conflicted::conflicts_prefer(dplyr::filter())
library(viridis)


# Reading in data ----
read_csv("") # reads in csv files only 
read_tsv("") # reads in tsv files only 

read_csv("../data/trichinella_spiralis_te.csv")
trich_df <- read_csv("../data/trichinella_spiralis_te.csv")


# Inspecting data ----
View() # used to view the data holistically 

View(trich_df)

head() # used to preview first six rows 
head(trich_df)

tail() # used to preview last 6 rows 
tail(trich_df)

summary() # used to create summary stats per column 
summary(trich_df)



# Cleaning data ----
# |> # used to feed previous data to next command, think of "and then"

group_by() # used to group wanted variables 
summarise() # used to display expressions 
table() # Gives a overall count of data according to select variable 

table(trich_df$matching_class)

sort(table(trich_df$matching_class), decreasing = TRUE)

trich_df |> 
  group_by(matching_class) |> 
  summarise() 

matching_class_df <-  trich_df |> 
  group_by(matching_class) |> 
  summarise() 

# we want to look at retrotransposons, so we must create vectors identifying what are TEs vs Non TEs, and what are RTEs 
ls_non_te = c("Simple_repeat", "Low_complexity", "Unknown", 
                 "simple_repeat", "Satellite", "rRNA", "snRNA")

ls_retransposons_sp = c("LINE/CR1", "LINE/RTE-BovB", "SINE/tRNA-RTE", 
                        "SINE/tRNA-Deu-L2", "LTR/Gypsy", "LTR/Pao", "LTR/Ngaro", 
                        "LTR/DIRS")

# Comment where you got your info from! Now, we use our vectors for filtering 

filter() # filters data based on input
#%in%  looks for whatever matches, the output is true or false 
#%notin% looks for whatever does not match, the output is true or false 

trich_df$matching_class %notin% ls_non_te

# Filtering for RTEs   
trich_rte <- trich_df |> 
  filter(matching_class %notin% ls_non_te) |> 
  filter(matching_class %in% ls_retransposons_sp) 

# Filtering for percent insertion 
select() # used to select certain columns 
trich_rte_perc_ins <- trich_rte |> 
  select(matching_class,perc_insert)

# Filtering for anything above 2%
perc_3.5 <- trich_rte_perc_ins |> 
  filter(perc_insert >= 3.5)

# Filtering one TE LTR/Gypsy 
LTR_te <- perc_3.5 |> 
  filter(matching_class == "LTR/Gypsy")


# Visualization ----

# Scatterplot 
ggplot() # used to create visualizations standard formation is the following: ggplot(data, aes(x = " ", y = " ")) + ...

perc_3.5 |> ggplot(
  aes(
    x = matching_class,
    y = perc_insert,
    colour = matching_class
  )
) + 
  geom_point() +
  labs(
    title = "Trichnella Spiralis Retrotransposon Percent Insertion Greater than or equal to 3.5%",
    x = "Retrotransposon",
    y = "Percent Insertion",
    color = "Retrotransposon Type"
  ) +
  theme_minimal()

ggsave("../output/perc3.5_rte.png")

# Histogram 
LTR_te|> ggplot(
  aes(
    x = perc_insert
  )
) + 
  geom_histogram() +
  labs(
    title = "LTR/Gypsy Retrotransposon Percent Insertion Greater than or equal to 3.5%",
    x = "Percent Insertion",
    y = "Frequency",
  ) + 
  theme_minimal()

ggsave("../output/perc3.5_hist.png")

# Boxplot 

trich_rte|> ggplot(
  aes(
    x = matching_class,
    y = perc_insert,
    color = matching_class
  )
) + 
  geom_boxplot() +
  labs(
    title = "Retrotransposon Percent Insertion",
    x = "Retrotransposon",
    y = "Percent Insertion",
  ) + 
theme_minimal()

ggsave("../output/trich_rte_box.png")