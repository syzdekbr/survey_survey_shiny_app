
# Server
function(input, output, session) {
  
# 1. Tab Panel UI Widget Content ---------------------------------------------

###*** Function to generate html ui from datasheet, with column names and data as widget arguments
  ui_func <- function(ui_type, ...){
    eval(parse(text = ui_type))(...)
  }
  
###*** Function to create a separate tabPanel for each group of ui, with navigation buttons for each page
  tab_func <- function(
    survey_items_df, # UI data with widget content
    survey_section_title, # The grouping variable for items, which will be each tabPanel
    previous_title, next_title){ # The titles of the groups that will also be ids for next and previous navigation buttons
  ## This creates a df to generate textInput fields for each item, referenceable by "<item_id>_other"
    other_dat <- survey_items_df %>% 
      select(inputId) %>% 
      mutate(
        inputId = paste0(inputId, "_other"),
        ui_type = "textInput",
        label = "Other information"
      )
  ## Create content for each tabPanel
    tabPanel(
      value = tolower(gsub("[^A-z]", "_", survey_section_title)), #id of each panel, same as ids of buttons
      title = gsub(" AND .*| OF .* |,.*| WITH .*| \\(.*", "", survey_section_title), # Reduce length of section titles for ease of reading
    # Module that applies ui_func to each set of UI data
      survey_items_rows_UI("survey_module", survey_item_df = survey_items_df, other_input_df = other_dat),
    ### Adds forward and previous buttons to bottom of page to navigate tabs
    ## Need to append suffix "_previous" or "_next" to give each button unique id
      fluidRow(
        # Will not generate button if title is na, to avoid button for first and last tab
        if(!is.na(previous_title)){
          column(6, align= "center", 
                ## Previous Button
                 shiny::actionButton(
                  inputId = paste0(tolower(gsub("[^A-z]", "_", previous_title)), "_previous"), # same as tab ids
                  label = gsub(" AND .*| OF .* |,.*| WITH .*| \\(.*", "", previous_title), # same as tab titles
                  icon = icon("chevron-left"))
          )
        },
        if(!is.na(next_title)){
          column(6, align= "center", 
                ## Next Button
                 shiny::actionButton(
                   inputId = paste0(tolower(gsub("[^A-z]", "_", next_title)), "_next"), # same as tab ids
                   label = gsub(" AND .*| OF .* |,.*| WITH .*| \\(.*", "", next_title), # same as tab titles
                   icon = icon("chevron-right"))
          )
        }
      ) # fluidRow close
    ) # tabPanel close
  } # tab_func close

###*** Applies tabPanel creation function to each group of data and creates tabsetPanel
  output$tab_UI <- renderUI({
    do.call(
      tabsetPanel,
      args = c(
        id = "main_tabs",
      ## Can include any individual tabs, as here, constructed as separate module
        c(
          list(
            tabPanel(
              value = "introduction_tab",
              title = "INTRODUCTION",
              introduction_UI("introduction_module")
            )
          ),
        ## Apply tab_func to each group of UI data
          survey_items_dat %>% select(-item_number) %>% group_by(topic) %>% nest() %>%
            pmap(
              .l = list(
                .$data,
                .$topic,
                lag(.$topic),
                lead(.$topic)
              ),
              .f = ~ tab_func(..1, ..2, ..3, ..4)
            )
        ) # close c
      ) # close args
    ) # close do.call
  })

# 2. Gather inputs -------------------------------------------------------------

## Ids of all inputs
  input_id_names <- tibble(
    input_id = 
      c(survey_items_dat$inputId, paste0(survey_items_dat$inputId, "_other"))
  )

## ReactiveValues that will hold id (input_id) and content (values) for each input
  all_values_rv <- reactiveValues(
    df = tibble(
      input_id = input_id_names$input_id,
      values = NA
    )
  )
  
## Function to gather content for each input
  all_output_values <- reactive({
    map_dfr(
      .x = input_id_names$input_id,
      .f = ~{
        tibble(
          input_id = .x,
          values = as.character(input[[.x]])
        ) %>%
          mutate(
            values = na_if(values, "NA")
          )
      }
    )
  })
  
## Apply function to gather content for each input
  observe({
    # Will gather content available and NA no inputs--needed if any inputs are reactive-not displayed yet
    all_values_rv$df <- left_join( 
      input_id_names,
      all_output_values(),
      by = "input_id"
    )
  })

# 3. Progress Bar and Submit Enable ------------------------------------------------------------

###*** Progress Bar Update- will increment percent complete with each response
  observe({
    req(all_values_rv$df$values)
    updateProgressBar(
      session = session,
      "completion_progress",
      value = all_values_rv$df %>%
      # Removes optional 'other' responses from being considered, can substitute whatever is non-essential
        filter(!grepl("_other$", input_id)) %>% 
        summarise((sum(!is.na(values) & values != "No response") / nrow(.) * 100)) %>%  # Get percent complete
        pull
    )
  })
  
###*** Submit enable/disable based on if all items answered
  observe({
    req(all_values_rv$df$values)
    if(all(!is.na(all_values_rv$df$values))){ # Criteria for completion
      shinyjs::enable("submit")
    } else{
      shinyjs::disable("submit")
    }
  })

###*** Unanswered item df to hold all items and display
  unanswered_items <- reactive({
  # List all required inputs here, those in survey_items_dat (not "other" items that were created)
    inner_join(
      all_values_rv$df,
      survey_items_dat,
      by = c("input_id" = "inputId")
    ) %>% 
      filter(is.na(values) | values == "No response") %>% # No answer
      mutate(Topic = gsub(" AND .*| OF .* |,.*| WITH .*| \\(.*", "", topic), ) %>% 
      select(Topic, `Item Number` = item_number)
  })  

# 4. Submit responses --------------------------------------------------------

###*** Submit Responses- show unanswered items
  observeEvent(input$submit,{ # When click submit button
  # Only if there are unanswered items, will display
    if(nrow(unanswered_items()) > 0){
      output$unanswered_df <- renderUI({
        tagList(
          h2("Unanswered Items"),
          DT::renderDataTable(DT::datatable(unanswered_items(), rownames = FALSE))
        )
      })
    } else{
  # No unanswered items
       output$unanswered_df <- renderUI({})
    }
    # Confirm if wishes to submit, will display warning but still accepts submission
      showModal(
        modalDialog(
          h3(ifelse(nrow(unanswered_items()) > 0,
                  "You have unanswered items.",
                  "Are you sure you wish to submit? Answers are uneditable afterwards."
          )
          ),
          footer = tagList(
            fluidRow(
              column(6, align = "center",
                modalButton("Cancel") 
              ),
              column(6, align = "center",
                actionButton("submit_responses", label = "Submit and close")
              )
            )
          )
        )
      )
  })

# 5. Save to googlesheet -----------------------------------------------------

## Save data to googlesheet
  ## Create googlesheet with column headers from input ids, like
  # survey_items_dat %>% select(inputId) %>% bind_rows(., survey_items_dat %>% select(inputId) %>% mutate(inputId = paste0(inputId, "_other"))) %>% 
  # pivot_wider(names_from = inputId, values_from = inputId, values_fn = ~"") %>% add_column(date = "", .before = 1)
  # Then range_write to doc, with col_names = TRUE
## Authorize 
# Store this json file in "keys" folder for each project
  drive_auth(path = "./keys/shiny_app_google_sheets_authorization.json") # go to this sheet and authorize for use
  gs4_auth(token = drive_token())
# gs_email_url and gs_response_data_url stored in "keys/googlesheet_url_information.R" sourced in global
## Read sheets
  email_df <- read_sheet(gs_email_url)
  responses_df <- read_sheet(gs_response_data_url)
## If user wishes to enter raffle, record email
  observeEvent(input$submit_responses,{
    if(input$enter_raffle == "Yes"){
      range_write(
        ss = gs_email_url,
        data = tibble(email_address = input$email_address),
        range = paste0("A", nrow(email_df) + 2),
        col_names = FALSE
      )
    }
  ## Record all user data
    range_write(
      ss = gs_response_data_url,
    ## Records data from reactiveValues holding all data; if there is list data (such as multi-input), collapses into semi-colon separated value
      data = all_values_rv$df %>% pivot_wider(names_from = input_id, values_from = values,
                                              values_fn = list(values = ~paste(., collapse = ";"))) %>% 
        add_column(date = Sys.Date(), .before = 1),
      range = paste0("A", nrow(responses_df) + 2),
      col_names = FALSE
    )
  ## Remove Confirm submission modal and run module thanking for participating
    removeModal()
    showModal(
      modalDialog(
        h5("Responses saved. Thank you for participating."),
        footer = tagList(
          modalButton("Dismiss")
        )
      )
    )
    shinyjs::disable("submit")

  })
  
# 6. Previous and Next Buttons -----------------------------------------------

###*** Functionality for created previous and next buttons to navigate adjacent tabPanels
  nested_grouped_data <- function(){
    survey_items_dat %>% select(-item_number) %>% group_by(topic) %>% nest() %>% pull(topic) %>% gsub("[^A-z]", "_", .) %>% tolower() %>% 
      c("introduction_tab", .)
  }
  map(
  ## Capture the click of any previous or next button, the redirect to that tab after stripping suffix
    # Need to add suffixes so button ids not duplicated
    .x = c(paste0(nested_grouped_data(), ("_next")), 
           paste0(nested_grouped_data(), ("_previous"))),
    ~ observeEvent(input[[.x]],{
      updateTabsetPanel(session,
                        inputId = "main_tabs",
                        selected = sub("_[^_]+$", "", .x)) # strip suffix to get page id
      ## Uses js
        js$toTop()
    })
  )

  
}
