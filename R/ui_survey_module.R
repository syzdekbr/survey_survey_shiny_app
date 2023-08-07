
###*** Applies ui generate content function to each group of UI data

###*** Function to generate html ui from datasheet, with column names and data as widget arguments
ui_func <- function(ui_type, ...){
  eval(parse(text = ui_type))(...)
}

survey_items_rows_UI <- function(id, survey_item_df, other_input_df){
  ns <- NS(id)
  tagList(
    h5("The following questions ask your views about how to structure the Hair Design Practical Examination. Please consider how the following proposals
    will affect how well novice candidates will understand the exam, how well the proposals will test whether candidates can demonostrate the skills
    necessary to minimally perform the job, and how ease of administering the practical exam items are affected."),
    slider::pslide(
      .l = list(
        survey_item_df, # Widgets for items data
        other_input_df  # More widgets for items data- in this case textInput
      ),
      .f = function(survey_item_df, other_input_df){
      ## Two column layout
        tagList(
          fluidRow(
            column(6,
                   pmap(survey_item_df, ui_func)
            ),
            column(6,
                   pmap(other_input_df, ui_func)
            ),
            hr(),
          )
        )     
      }
    )
  )
}