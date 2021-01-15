## OWL Power Rank Aggregator

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(tidyr)
  library(shiny)
  library(shinyjs)
  library(sortable)
  library(googlesheets4)
  library(rclipboard)
  library(ggplot2)
  library(ggimage)
})

#### Functions ####
na.rm <- function(in_list) {
  in_list[!is.na(in_list)]
}

li_tag <- function(img_src, in_name, in_abb) {
  if (is.na(in_name)) {
    tag_list <- tags$div(
      tags$img(src=file.path(list_logos_path, "TBR.png"), width = team_logo_width), 
      tags$strong(tier_break_string), 
      tags$img(src=file.path(list_logos_path, "TBR.png"), width = team_logo_width)
    )
  } else {
    tag_list <- tags$div(
      tags$img(src=img_src, width = team_logo_width),
      in_name, 
      actionLink(paste0("roster_", in_abb), label = "", icon = icon("users"))
    )
  }
  return(tag_list)
}

list2rank <- function(in_list) {
  list_order <- match(in_list, team_colours$team)
  team_labels <- mapply(li_tag, team_colours$list_logo[list_order], team_colours$team[list_order], team_colours$abb[list_order], SIMPLIFY = FALSE)
  names(team_labels) <- replace_na(team_colours$team[list_order], tier_break_string)
  return(team_labels)
}


trim_tb <- function(in_list) {
  ## Trim tier breaks
  first_last_index <- which(in_list %in% teams)[c(1,length(teams))]
  return(
    in_list[first_last_index[1]:first_last_index[2]]
  )
}

check_name <- function(in_name) {
  if (!str_detect(in_name, "[a-zA-Z]+")) {
    ## Must contain a letter:
    return(FALSE)
  } else if (!str_remove_all(in_name, "[a-zA-Z]*[0-9]*#*_*") == "") {
    ## Must not be blank
    return(FALSE)
  }
  return(TRUE)
}

check_pin <- function(in_name) {
  pattern <- "#\\d{4}\\d*$"
  has_pin <- str_detect(in_name, pattern)
  return(has_pin)
}

#### Sheets and Constants ####
RANK_DB_ID <- read_file("www/.secrets/rank_db.txt")
PLAYERS_ID <- read_file("www/.secrets/players.txt")
TOKEN_ID <- read_file("www/.secrets/service_token.txt")

options(gargle_oauth_cache = "www/.secrets")
gs4_auth(path = file.path("www/.secrets", TOKEN_ID))

## Read Data from google sheet (or backup if unavailable)
tryCatch({
  ## Worksheet with columns: 
  ##   Teams, Communities, TierString, MaxTierLimit, LogoWidth
  suppressMessages(extra_info <- read_sheet(RANK_DB_ID, sheet = "Info"))
  ## Spreadsheet with worksheets:
  ##   Info, Log, Submissions, Feedback
  sheet_info <- gs4_get(RANK_DB_ID)
}, 
error = function(e) {
  extra_info <- readRDS("www/backup/extra_info.rds")
  sheet_info <- readRDS("www/backup/sheet_info.rds")
  print(paste("Error on load.", e))
}, 
finally = {
  ## Pulled Info
  teams <- na.rm(extra_info$Teams)
  community_list <- na.rm(extra_info$Communities)
  tier_break_string <- extra_info$TierString[1]
  tier_break_limit <- extra_info$MaxTierLimit[1]
  team_logo_width <- extra_info$LogoWidth[1]
  rows_per_team <- 4 ### ToDo change this
})

## Constants
team_colours <- readr::read_csv("www/TeamLogos/TeamColours.csv", col_types = "cffcccc")
list_logos_path <- "TeamLogos"
export_logos_path <- "www/TeamLogos/Export"
team_colours <- team_colours %>%
  mutate(list_logo = paste(file.path(list_logos_path, abb),".png", sep = "")) %>%
  mutate(export_logo = paste(file.path(export_logos_path, abb),".png", sep = ""))

export_letters <- c(LETTERS[1:length(teams)],0)
host_url <- "https://fishghost.shinyapps.io/OWLPR"

## Data Wranlging
##   None right now

#### UI Input/Output ####
## Text 
ui_user_name <- textInput("user_name", label = "Enter your user name", value = "", placeholder = "user_name#1234")
ui_community <- selectInput("selected_community", label = "Community", choices = community_list, selected = "None")
ui_feedback_contact <- textInput("feedback_contact", label = "Contact Address (optional)", placeholder = "Email, Discord or Reddit username")
ui_feedback <- textAreaInput("feedback", label = "Enter feedback:", value = "", height = "200px", resize = "both")

## Buttons
test_button <- actionButton("button_test", label = "test")

ui_retrieve_button <- actionButton("button_retrieve", label = "Retrieve Old Submission", icon = icon("cloud-download-alt"))
ui_submit_button <- actionButton("button_submit_list", label = "Submit to Database", icon = icon("upload"), class = "btn-primary")

ui_export_img_button <- actionButton("button_export_img", label = "as Image", icon = icon("file-image"))
ui_clipboard_button <- actionButton("button_clipboard", label = "as Text to Clipboard", icon = icon("clipboard"))
ui_export_link <- actionButton("button_export_link", label = "as Link", icon = icon("link"))

ui_add_tier_button <- actionButton("button_add_tier", label = " Tier Break", icon = icon("plus"))
ui_del_tier_button <- actionButton("button_delete_tier", label = " Last Tier Break", icon = icon("minus"))
ui_reset_list_button <- actionButton("button_reset_list", label = "Reset List", icon = icon("redo"))

ui_readme_modal <- actionButton("button_modal_readme", label = "About", icon = icon("info-circle"))
ui_feedback_modal <- actionButton("button_modal_feedback", label = "Submit Feedback", icon = icon("comments"))

ui_collapse_button <- actionButton("button_collapse", label = " Share  Options and Info", icon = icon("toggle-off"), 
                                   `data-toggle` = "collapse", `data-target` = "#collapsable_options",
                                   width = "100%")

## Lists
ui_team_list <- uiOutput("ui_team_list")

## UI Outputs 
ui_user_text <- textOutput("user_text")
ui_modal_text <- textOutput("modal_text")
ui_readme <- tags$div(style="text-align:left;", 
                      includeMarkdown(file.path("www","readme.md")))

## In Modals
ui_reset_confirm <- actionButton("button_reset_confirm", 
                                 "Reset List", 
                                 icon = icon("redo"), 
                                 class = "btn-primary")
ui_feedback_submit <- actionButton("button_feedback_submit", label = "Submit Feedback", class = "btn-primary")

#### Shiny UI ####
ui <- fluidPage(
  theme = "extra.css",
  ## Library initialization
  useShinyjs(), rclipboardSetup(),
  
  titlePanel("Community Power Rank Aggregator"),
  
  sidebarLayout(
    sidebarPanel(
      width = 5, 
      ## Primary inputs: 
      fluidRow(column(6, ui_user_name), column(6, ui_community)), 
      # fluidRow(test_button), 
      fluidRow(style = "text-align:center; height:40px; font-size:large;", ui_user_text), 
      fluidRow(style = "text-align:center; ", column(6,ui_retrieve_button), column(6, ui_submit_button)), 
      br(),
      fluidRow(ui_collapse_button), 
      tags$div(
        id = "collapsable_options", class = "collapse",
        tags$h3("Share"), 
        ui_export_img_button, ui_clipboard_button, ui_export_link,
        tags$h3("Contact"), 
        ui_readme_modal, ui_feedback_modal
      )
    ),
    mainPanel(
      width = 7,
      fluidRow(style = "", 
               column(12, 
                      fluidRow(style = "text-align: center;", 
                               ui_add_tier_button, ui_del_tier_button, ui_reset_list_button),
                      fluidRow(column(8, offset = 2, ui_team_list))))
    )
  )
)

## For use in later updates
ui_func <- function(req) {
  url_query <- parseQueryString(req$QUERY_STRING)
}


#### Shiny Server ####
server <- function(input, output, session) {
  #### ToDo: #### 
  #### Memory ####
  rV <- reactiveValues(retrieved_data = NULL, 
                       starting_list = teams, 
                       list_labels = NULL, 
                       bypass_pin = FALSE, 
                       prior_pressed = rep(0, length(teams)),
                       retrieved_rosters = NULL)
  
  #### URL Param ####
  observe({
    url_query <- parseQueryString(session$clientData$url_search)
    
    if (!is.null(url_query[['c']])) {
      found_community <- community_list[str_detect(community_list, fixed(url_query$c, ignore_case = TRUE))]
      if(length(found_community == 1)) {
        updateSelectInput(session, "selected_community", selected = found_community)
      }
    }
    
    if (!is.null(url_query[['rl']])) {
      url_rl <- url_query[['rl']]
      
      ## Check all known ways to mess up URL manually
      if (any(
        ## Any letters appear more than once or not at all
        !all(str_count(url_rl, export_letters[1:(length(export_letters)-1)]) == 1),
        ## Any letters outside of set
        any(str_count(url_rl, setdiff(LETTERS, export_letters))),
        ## Excessive tier breaks
        str_count(url_rl, tail(export_letters,1)) > tier_break_limit
      )) {
        updateUserText("Badly formed link.", 5000)
      } else {
        ## Convert input parameter into new ordered list of teams
        rV$starting_list <- paste(c(teams,tier_break_string)[match(str_split(url_rl, "")[[1]], export_letters)])
      }

    }
    
  })
  
  
  #### Reset ####
  observeEvent(input$button_reset_list, {
    # Only show modal if list has changed: 
    if (!identical(input$list_teams, rV$starting_list)) {
      showModal(
        modalDialog(
          "Do you want to reset your current rankings?", 
          easyClose = TRUE,
          size = "s",
          footer = tagList(
            modalButton("No"), 
            ui_reset_confirm
          )
        )
      )
    }
  })
  
  observeEvent(input$button_reset_confirm, {
    removeModal()
    resetList()
  })
  
  resetList <- function() {
    rV$list_labels <- NULL
    rV$list_labels <- rV$starting_list
  }

  #### Tier Breaks ####
  observeEvent(input$button_add_tier, {
    ## Check the number of current tier breaks: 
    if (sum(input$list_teams %in% tier_break_string) > tier_break_limit) {
      updateUserText("Too many tier breaks added", 2000)
      return()
    }
    
    new_list <- c(tier_break_string, input$list_teams)
    rV$list_labels <- new_list
  })
  
  observeEvent(input$button_delete_tier, {
    ## If list is currently valid: 
    if (length(input$list_teams) > 0) {
      
      ## Find index of last tier break
      last_break_index <- tail(which(input$list_teams %in% tier_break_string), 1)
      if (length(last_break_index) == 1) {
        rV$list_labels <- input$list_teams[-last_break_index]
      }
    }
  })
  
  #### Rank List Creation/Update ####
  output$ui_team_list <- renderUI({
    ## Populate list first time with starting list
    if (is.null(rV$list_labels)) {
      rV$list_labels <- isolate(rV$starting_list)
    }
    return (
      rank_list(
        input_id = "list_teams",
        text = "Drag teams into ranking; optionally dividing them by tiers",
        labels = list2rank(rV$list_labels),  
        ## Allow for multi-select and don't let item be dragged by roster link
        options = sortable_options(multiDrag = TRUE, filter = "a"), 
        class = c("default-sortable", "owl-teams") ## <- default format
        # class = c("owl-teams") ## <- Slmn-type format
      )
    )
  })
  
  #### Roster ####
  observeEvent(
    ## Observe if any of the roster links are clicked (requires fandangling)
    lapply(paste0("roster_", team_colours$abb), function(x) input[[x]]),
    {
      ## Get all roster buttons as vector in order from team_colours
      rosters_pressed <- unlist(lapply(paste0("roster_", team_colours$abb), function(x) input[[x]][1]))
      ## Buttons sequence upward, so find only the last button pressed
      last_pressed <- rosters_pressed - rV$prior_pressed
      
      if (any(last_pressed == 1)) {
        roster_for_team <- team_colours$team[[which(last_pressed==1)]]
        
        if(is.null(rV$retrieved_rosters)) {
          showModal(
            modalDialog(
              title = paste(str_replace(roster_for_team, ".*\\s", ""), "Roster"),
              tags$h4("performing one-time retrieval..."),
              easyClose = TRUE,
              size = "s"
            )
          )
        }
        
        showModal(
          modalDialog(
            title = paste(str_replace(roster_for_team, ".*\\s", ""), "Roster"),
            column(12, align = "center", renderRoster(roster_for_team)),
            easyClose = TRUE,
            size = "s"
          )
        )
        
        rV$prior_pressed <- rosters_pressed
      }
    }
  )
  
  renderRoster <- function(roster_for_team) {
    row_index <- which(team_colours$team == roster_for_team)*rows_per_team-(rows_per_team-1)
    
    ## If no roster info, go collect it
    if(is.null(rV$retrieved_rosters)) {
      try({
        suppressMessages(rV$retrieved_rosters <- read_sheet(PLAYERS_ID, range = "FG_Rosters", 
                                                            col_names = c("Damage", "Tank", "Support"), 
                                                            col_types = "ccc"))
      })
    }
    
    ## If no roster collected, :(
    if(is.null(rV$retrieved_rosters)) {
      return(
        tags$h4("Error obtaining rosters")
      )
    } else {
      roster <- rV$retrieved_rosters[row_index:(row_index+rows_per_team-1),]
      roster[is.na(roster)] <- "-"
      
      return(
        renderTable(roster)
      )
    }
  }
  
  #### Retrieve ####
  observeEvent(input$button_retrieve, {
    if (!check_name(input$user_name)) {
      updateUserText("Invalid user name to retrieve")
    } else {
      shinyjs::disable(id = "button_retrieve")
      retrieve_submission()
      shinyjs::enable(id = "button_retrieve")
    }
  })
  
  retrieve_submission <- function() {
    ## Pull data from DB if not yet:
    if(is.null(rV$retrieved_data)) {
      try({
        submission_time <- Sys.time()
        suppressMessages(rV$retrieved_data <- read_sheet(RANK_DB_ID, sheet = "Submissions")) # TryCatch
        
        ## Log that data was retrieved: 
        submitLog("retrieval", time = submission_time)
      })
    }
    
    ## Provide error if data retrieval did not work
    if(is.null(rV$retrieved_data)) {
      updateUserText("Retrieval did not work. Try again later or contact.")
      print(paste("Error retrieving data."))
    } else {
      ## Filter for user_name:
      filtered_lists <- rV$retrieved_data %>%
        filter(user_name == input$user_name)
      
      ## Check if user name was found: 
      if (nrow(filtered_lists) == 0) {
        updateUserText(paste("No submission found for", input$user_name,"(case sensitive)"))
      } else {
        last_submission_index <- nrow(filtered_lists)
        
        ## Extract meta data: 
        submitted_date <- filtered_lists$time[last_submission_index]
        submitted_community <- filtered_lists$community[last_submission_index]
        
        ## Remove non-relavant elements and transpose:
        filtered_lists <- filtered_lists %>%
          select(-c(user_name, time, community)) %>%
          t(.)
        
        ## Take the last column (/submission): 
        last_list <- unname(filtered_lists[, last_submission_index])
        
        ## Remove trailing NAs and replace with newest tier-break string:
        last_list <- na.rm(last_list)
        last_list[!(last_list %in% teams)] <- tier_break_string
        
        ## Assign to rank list: 
        rV$list_labels <- last_list
        ## Update list for Reset button: 
        rV$starting_list <- last_list
        
        ## User response:
        found_community <- community_list[str_detect(community_list, fixed(submitted_community, ignore_case = TRUE))]
        if(length(found_community == 1)) {
          updateSelectInput(session, "selected_community", 
                            selected = submitted_community)
        }
        
        updateUserText(paste("Retrieved submission from", 
                             format(as.Date(submitted_date), "%B %d, %Y")), 
                       7500)
      }
    }
  }
  
  #### Submission ####
  observeEvent(input$button_submit_list, {
    if (!check_name(input$user_name)) {
      showModal(
        modalDialog(
          title = "Enter valid user name",
          tags$ul(
            tags$li("must contain alphabet based-name"),
            tags$li("can contain numbers"),
            tags$li("can only use underscore (_) for special character"),
            tags$li(tags$strong("Optionally:"), "4+ digit PIN following # to secure ranking"),
          ),
          tags$p("Eg. fishghost#1329"),
          tags$p("Refer to About button for more info."),
          easyClose = TRUE
        )
      )
    } else {
      submitCheck()
    }
  })
  
  submitCheck <- function() {
    if (input$user_name != "" ) {
      if (!(check_pin(input$user_name) || rV$bypass_pin)) {
        ## (Admittedly poor way to) Check for a pin
        showModal(
          modalDialog(
            title = "No unique identifier", 
            tags$p("Your user name currently has no unique identifier.", 
            "Anyone that types in the current name will be able to retrieve", 
            "and overwrite your latest submission."), 
            tags$p("You can add a unique PIN at the end of your username following a #. (Eg. fishghost#1421)"),
            tags$p(tags$strong("Note: "), "This app is only semi-secure so avoid using an important PIN."),
            easyClose = TRUE, 
            footer = tagList(
              modalButton("No"), 
              actionButton("bypass_pin", 
                           "Submit without identifier", class = "btn-primary")
            )
          )
        )
      } else if (identical(input$list_teams, rV$starting_list)) {
        
        showModal(
          modalDialog(
            title = "No submission changes",
            "The teams you have ranked have not changed. Do you still want to submit?",
            easyClose = TRUE,
            footer = tagList(
              modalButton("No"),
              actionButton("submit_confirm",
                           "Submit unchanged list", class = "btn-primary")
            )
          )
        )
      } else {
        submitList()
      }
    } else (
      updateUserText("Error, could not submit")
    )
  }
  
  observeEvent(input$bypass_pin, {
    removeModal()
    rV$bypass_pin <- TRUE
    submitCheck()
  })
  
  observeEvent(input$submit_confirm, {
    removeModal()
    submitList()
  })
  
  submitList <- function() {
    shinyjs::disable(id = "button_submit_list")
    submission_status <- 0
    submission_time <- as.character(Sys.time())
    
    # No longer necessary
    ranked_list <- str_remove_all(input$list_teams, 
                                  "\\s*\\^?--\\^?\\s*")
    
    ## Assign data as single column...
    temp <- data.frame(col = c(input$user_name,
                               submission_time,
                               input$selected_community, 
                               ranked_list))
    ## ... and transpose to single row
    temp <- as.data.frame(t(temp))
    
    ## Check if entry is valid: 
    if(length(temp) > 3) {
      submission_status <- tryCatch({
        suppressMessages(sheet_append(RANK_DB_ID, 
                                      temp, 
                                      sheet = "Submissions"))
        1 ## Way to return this as successful
      }, 
      error = function(e) {
        print(paste("Error on submission.", e))
        return(2)
      })
    } else {
      submission_status <- 2
    }
    
    ## Submission Report
    submitLog("submission", submission_time, submission_status)
    userSubmissionReport(submission_status)
    shinyjs::enable(id = "button_submit_list")
  }
  
  userSubmissionReport <- function(submission_status) {
    submission_message <- case_when(
      submission_status == 1 ~ "Submission Successful", 
      submission_status == 0 ~ "Submission Failed", 
      submission_status == 2 ~ "Submission Error. Try again later."
    )
    updateUserText(submission_message, delay_time = 5000)
  }
  
  #### Export ####
  observeEvent(input$confirm_clipboard, {
    removeModal()
    updateUserText("Copied!", 5000)
  })
  
  observeEvent(input$button_export_img, {
    ## Create plot: 
    team_bg_colour <- c(setNames(as.character(c(team_colours$background, "#DDDDDD")),
                                 c(team_colours$team, tier_break_string)))
    team_text_colour <- c(setNames(as.character(c(team_colours$text, "#888888")),
                                   c(team_colours$team, tier_break_string)))
    ## Twitter: 1200px X 675px and 16:9

    dim.y <- round(25*length(input$list_teams))
    dim.x <- 282
    aspr <- dim.x/dim.y
    dl_dpi <- 72
        
    plot_data <- data.frame(team = trim_tb(input$list_teams)) %>%
      left_join(team_colours, by = "team") %>%
      mutate(rank = 1:n(), team = reorder(team, desc(rank))) %>%
      mutate(rank = reorder(as.character(rank), desc(rank))) %>%
      mutate(export_logo = ifelse(team == tier_break_string, file.path(export_logos_path,"TBR.png"), export_logo)) %>%
      ggplot(aes(y = rank)) +
      ## Teams
      geom_bar(aes(x = ifelse(team %in% teams, 1, 0), fill = team), 
               stat = "identity", 
               width = 1, na.rm = TRUE) + 
      geom_image(aes(image = ifelse(team %in% teams, export_logo, NA)), 
                 x = 0.125, na.rm = TRUE, 
                 size = 1/length(input$list_teams), by = "height",
                 asp = aspr) +
      geom_text(aes(label = team, colour = team), 
                size = 5, na.rm = TRUE, 
                x = 0.5, fontface = "bold") +
      ## Tier Breaks
      # geom_bar(aes(x = ifelse(team == tier_break_string, 1, 0)), 
      #          stat = "identity", 
      #          width = 0.2, na.rm = TRUE, fill = "black") + 
      # 
      ## Formatting
      scale_fill_manual(values = team_bg_colour) +
      scale_color_manual(values = team_text_colour) +
      theme_void() +
      theme(legend.position = "none", panel.border = element_blank())
    
    ## Modal elemnts: 
    preview_plot <- renderPlot(plot_data, width = dim.x, height = dim.y, 
                               bg = "transparent")
    
    downloader <- downloadHandler(filename = "PowerRank.png",
                                  content = function(file) {
                                    ggsave(file, plot = plot_data, device = "png", 
                                           units = "in", dpi = dl_dpi,
                                           height = dim.y/dl_dpi, width = dim.x/dl_dpi)
                                  })
    
    showModal(
      modalDialog(
        preview_plot,
        size = "s",
        easyClose = TRUE,
        footer = tagList(
          modalButton("Dismiss"), 
          downloader
        )
      )
    )
  })
  
  observeEvent(input$button_clipboard, {
    trimmed <- trim_tb(input$list_teams)
    preview_height <- paste0(21*length(trimmed),"px")
    to_clip <- paste(replace(trimmed, trimmed == tier_break_string, ""), collapse = "\n")
    clip_preview <- disabled(textAreaInput("preview_clip",
                                           "Preview:",
                                           value = to_clip,
                                           height = preview_height))
    
    showModal(
      modalDialog(
        title = "Copy as Text",
        clip_preview,
        easyClose = TRUE,
        size = "s",
        footer = tagList(
          uiOutput("ui_clipboard_confirm")
        )
        
      )
    )
  })
  
  output$ui_clipboard_confirm <- renderUI({
    ui_clip_button <- rclipButton("confirm_clipboard", "Not working", "", icon("clipboard"))
    
    if(is.character(input$preview_clip)) {
      ui_clip_button <- rclipButton("confirm_clipboard",
                                    label = "Copy",
                                    clipText = input$preview_clip,
                                    icon = icon("clipboard"),
                                    modal = TRUE)
    }
    
    return(
      tagList(modalButton("Dismiss"),ui_clip_button)
    )
    
  })
  
  observeEvent(input$button_export_link, {
    ## Convert list into little letter string:
    export_param <- paste(c(LETTERS[1:length(teams)],0)[match(input$list_teams, c(teams, tier_break_string))], collapse = "")
    url_param <- paste0(host_url, "?rl=", export_param)
    if (input$selected_community != community_list[1]) {
      url_param <- paste0(url_param, "&c=", input$selected_community)
    }
    
    link_preview <- disabled(textInput("preview_link",
                                       "Preview:",
                                       width = "100%",
                                       value = url_param))

    showModal(
      modalDialog(
        title = "Copy Link to List",
        link_preview,
        easyClose = TRUE,
        footer = tagList(
          uiOutput("ui_link_confirm")
        )
        
      )
    )
  })
  
  output$ui_link_confirm <- renderUI({
    ui_clip_button <- rclipButton("confirm_clipboard", "Not working", "", icon("clipboard"))
    
    if(is.character(input$preview_link)) {
      ui_clip_button <- rclipButton("confirm_clipboard",
                                    label = "Copy",
                                    clipText = input$preview_link,
                                    icon = icon("clipboard"),
                                    modal = TRUE)
    }
    
    return(
      tagList(modalButton("Dismiss"),ui_clip_button)
    )
    
  })

  #### Feedback ####
  observeEvent(input$button_feedback_submit, {
    
    if (input$user_name == "" & input$feedback_contact == "") {
      updateUserText("Enter user name or contact")
      updateModalText("Enter user name or contact")
    } else if (input$feedback == "") {
      updateUserText("Enter feedback")
      updateModalText("Enter feedback")
    } else {
      shinyjs::disable(id = "button_feedback_submit")
      submission_time <- Sys.time()
      feedback_message <- data.frame(user = input$user_name, 
                                     time = as.character(Sys.time()), 
                                     contact = input$feedback_contact,
                                     feedback = input$feedback)
      
      ## Attempt to append google sheet 
      tryCatch({
        suppressMessages(sheet_append(RANK_DB_ID, 
                                      feedback_message, 
                                      sheet = "Feedback"))
        submitLog("feedback", time = submission_time)
        updateUserText("Feedback submitted!")
      },
      error = function(e) {
        updateUserText("Could not submit feedback. Please email fishmanghost@gmail.com!", 20000)
        print(paste("Error on feedback submission.", e))
      }, 
      finally = {
        removeModal()
        shinyjs::enable(id = "button_feedback_submit")
      })
    }
  })
  
  observeEvent(input$button_modal_feedback, {
    showModal(
      modalDialog(
        title = "Provide Feedback",
        ui_feedback_contact, 
        ui_feedback, 
        ui_modal_text,
        easyClose = TRUE,
        size = "s",
        footer = tagList(
          modalButton("Dismiss"), 
          ui_feedback_submit
        )
      )
    )
  })
  
  #### Log ####
  submitLog <- function(activity, time, status = 1) {
    user <- input$user_name
    if (input$user_name == "" && !is.null(input$feedback_contact)) {
      user <- input$feedback_contact
    }
    
    log_report <- data.frame(user = user,
                                    time = time, 
                                    activity = activity, 
                                    status = status)
    tryCatch({
      suppressMessages(sheet_append(RANK_DB_ID, 
                                    log_report, 
                                    sheet = "Log"))
    }, 
    error = function(e) {
      print(paste("Error in Logging.", e))
    })
  }
  
  #### Reactive User Stuff ####
  updateUserText <- function(update_to, delay_time = 3000) {
    output$user_text <- renderText(update_to)
    delay(delay_time, output$user_text <- renderText(""))
  }
  
  updateModalText <- function(update_to, delay_time = 3000) {
    output$modal_text <- renderText(update_to)
    delay(delay_time, output$modal_text <- renderText(""))
  }
  
  observeEvent(input$button_collapse, {
    ## Starts on 0 and already collapsed
    if (input$button_collapse%%2) {
      updateActionButton(session, "button_collapse", icon = icon("toggle-on"))
    } else {
      updateActionButton(session, "button_collapse", icon = icon("toggle-off"))
    }
  })
  
  observeEvent(input$button_modal_readme, {
    showModal(
      modalDialog(
        ui_readme, 
        easyClose = TRUE, 
        footer = tagList(
          modalButton("Dismiss")
        )
      )
    )
  })
  
  #### Debug Test ####
  observeEvent(input$button_test, {
    debug_text <- paste("thing to test")
    updateUserText(debug_text, 10000)
  })
}

#### Shiny App ####
shinyApp(ui = ui, server = server)