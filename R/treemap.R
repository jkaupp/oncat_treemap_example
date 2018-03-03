library(tidyverse)
library(purrr)
library(readxl)
library(tools)
library(treemap)
library(RColorBrewer)


files <- list.files("data", full.names = TRUE, pattern = "LO_V2")

sheets <- excel_sheets(files)[!grepl("Bloom's",excel_sheets(files))]

bloom_sheet <-  excel_sheets(files)[grepl("Bloom's",excel_sheets(files))]

blooms_factor <- read_excel(files, sheet = bloom_sheet, col_names = FALSE) %>% 
  set_names(c("verb", "bloom_level")) %>% 
  mutate(verb = tolower(verb))

data <- map_df(sheets, ~read_excel(files, sheet = .x) %>% mutate(sheet = .x)) %>% 
  separate(sheet, c("level","discipline"), sep = "-") %>% 
  mutate(level = ifelse(level %in% c("Q","UofT","Con"), "University", "College")) %>% 
  mutate(discipline = ifelse(discipline == "ME", "Mechanical Engineering",
                             ifelse(discipline %in% c("EE","Elec"), "Electrical Engineering",
                                    ifelse(discipline == "ElectricalE", "Electrical Engineering",
                                           ifelse(discipline == "ElectronicsE",  "Electronics Engineering", discipline))))) %>% 
  set_names(c("outcome","au","concept","verb","level","discipline")) %>% 
  separate_rows(verb, sep = ",") %>% 
  mutate_each(funs(tolower), outcome:concept) %>% 
  mutate_each(funs(trimws)) %>% 
  mutate_each(funs(toTitleCase), au, concept, verb)


au.colors <- rev(brewer.pal(5, "Set1"))

# Treemap ----

build_treemap <- function(x) {
  
  discipline <- unique(x$discipline)
  
  level <- unique(x$level)
  
  filename <- sprintf("images/%s - %s AU-Concept-Verb Treemap2.png", level, discipline)
  
  
  tree_data <- x %>%
    mutate(au = ifelse(au == "Complementary", "Complementary Studies", au)) %>%
    mutate(au = factor(au, levels = c("Mathematics","Natural Science","Complementary Studies","Engineering Science","Engineering Design"), labels = c("Mathematics","Natural Science","Complementary Studies","Engineering Science","Engineering Design"))) %>% 
    count(level, discipline, au, concept, verb) 
  
  
  png(filename, width = 1000, height = 1600, units = "px")
  treemap(tree_data,
          index = c("au", "concept", "verb"),
          vSize = "n",
          vColor = "au",
          type = "categorical",
          palette = au.colors,
          title = "",
          title.legend = "Academic Unit (AU) Category",
          fontsize.labels = c(0, 30, 20),
          fontcolor.labels = "#f0f0f0",
          inflate.labels = FALSE,
          lowerbound.cex.labels = 1,
          bg.labels = 0,
          position.legend = "bottom",
          border.col = "white",
          border.lwds = c(2, 2, 1),
          align.labels = list(c("left", "top"), c("left", "top"), c("right", "bottom")),
          drop.unused.levels = FALSE,
          aspRatio = NA)
  dev.off()
  
  
}

data %>% 
  split(list(.$level, .$discipline),drop = TRUE) %>% 
  walk(~build_treemap(.x))

