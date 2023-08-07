introduction_UI <- function(id){
  tagList(
    h5("You are being asked to participate in a survey about hair design best practices. This survey is being conducted by Prov, a 
      test development company, on behalf of National-Interstate Council of State Boards of Cosmetology (NIC)."),
     tags$br(),
    h5("Your responses will help
      shape the content of the hair design credentialing examinations. Your participation in this survey is completely voluntary. If you 
      choose to participate, you will be asked a series of questions about hair design practices. It is expected that this survey will take
      approximately 20 minutes. Please answer all questions in full to submit the survey."),
    tags$br(),
    h5("After answering all questions, please click the 'Submit' 
      button to submit your responses. You can also choose to provide your email to be entered in a raffle
      to win one of ten $50 Amazon gift cards. The raffle will be held one month after the survey has begun. Your email will only be used to notify you if
      you are selected in the raffle and will not be used for any other purpose. Your email will not be associated with your answers, 
      which will remain confidential. If you have any questions please feel free to contact
      us at support@provexam.com or call at (801)-733-4455."),
    tags$br(),
    h5("Thank you for your contribution to help make the Hair Design licensing exams
      current and to help ensure the profession is selecting the best candidates. When you are ready, click 'Begin Survey' to begin.
      Please only submit one response per licensed Barber professional.
    "),
    fluidRow(
      column(6, align= "center", 
             shiny::actionButton("general_next", "Begin Survey",
                                 icon = icon("chevron-right"))
      )
    )
  )
}