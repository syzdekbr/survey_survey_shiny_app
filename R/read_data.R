
library(tidyverse)

###*** Read data
survey_items_dat <- readxl::read_xlsx("data/different_items_NIC_app_items.xlsx") %>% 
  mutate(
    inputId = tolower(gsub("[^A-z]", "_", label)),
    ui_type = "radioButtons",
    topic = factor(topic, levels = c("GENERAL", unique(topic)[unique(topic) != "GENERAL"]))
    ) %>% 
  rowwise() %>% 
  mutate(choices = list(c("No response", c_across(starts_with("choice_")))),
         .keep = "unused") %>% 
  ungroup() %>% 
  arrange(topic) %>% 
  mutate(
    item_number = row_number(),
    label = paste0(item_number, ". ", label)
    )

###*** This is generic way to read in data to create any ui. One column should be "ui_type" that lists name of function (e.g., "radioButtons")
###* Then list all relevant options as column heads, omitting when there are irrelevant options b/c of multiple ui_type
###* 
# ui_dat <- readxl::read_xlsx("data/<name of ui file>") %>% 
  # mutate(
  # # This steps creates inputId in data from label, so no need to supply in df
  #   inputId = tolower(gsub("[^A-z]", "_", label)),
  # # This creates a grouping factor, such as to set up tabs for groups of items, Here is re-orders by selected topic name first
  #   topic = factor(topic, levels = c("GENERAL", unique(topic)[unique(topic) != "GENERAL"]))
  # ) %>% 
  # ## Then will create an item number for each item, to be added to label, which can be used for calling out un-responded items
  #   arrange(topic) %>% 
  #   mutate(
  #     item_number = row_number(),
  #     label = paste0(item_number, ". ", label)
  #   ) %>% 
#   {. -> tmp
#   ## This is optional step to create named list of options, here used to set maximum number of items to be selected in selectizeInput
#     opt_names <- tmp %>% select(starts_with("options_")) %>% colnames
#     slider::slide_dfr(
#       .x = tmp,
#       .f = ~{
#         .x %>% 
#         ## Removes irrelvant options
#           select(where(~!(is.na(.x)))) %>% 
#         ## List all 
#           mutate(choices = list(c_across(starts_with("choice_"))),
#                  options = list(set_names(list(!! sym(opt_names)), gsub("options_", "", opt_names))),
#                  .keep = "unused")
#       }
#     )
#   }