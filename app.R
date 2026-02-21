library(shiny)
library(shinyWidgets)
library(readr)
library(stringr)
library(dplyr)
library(ggplot2)

# Load the dataset
soccer <- read_csv("fifa_world_cup_matches.csv")

# Convert team names to title case
soccer$team1 <- str_to_title(soccer$team1)
soccer$team2 <- str_to_title(soccer$team2)

# Replace "Korea Republic" with "South Korea"
soccer$team1 <- ifelse(soccer$team1 == "Korea Republic", "South Korea", soccer$team1)
soccer$team2 <- ifelse(soccer$team2 == "Korea Republic", "South Korea", soccer$team2)

# Clean up the category column to create group information
soccer$group <- str_extract(soccer$category, "Group [A-H]")

# Convert possession percentages to numeric values
soccer$possession1 <- as.numeric(str_replace(soccer$`possession team1`, "%", ""))
soccer$possession2 <- as.numeric(str_replace(soccer$`possession team2`, "%", ""))

# Define unique groups
group_choices <- LETTERS[1:8]

# Define UI
ui <- fluidPage(
  titlePanel("Men's World Cup in 2022"),
  sidebarLayout(
    sidebarPanel(
      sliderTextInput("Group", "Choose a group", 
                      choices = group_choices, selected = "A", grid = TRUE),
      selectInput("Team", "Choose a team", choices = NULL), 
      selectInput("Opponent", "Choose an opponent", choices = NULL)
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Description", 
                 p("This Shiny app provides an interactive data visualization of the FIFA World Cup Qatar 2022. It allows users to explore different statistics like possession percentage, goals, assists, and goal prevention for different matches. To use the app, simply choose a team from a group (e.g. Group A) and then select their opponent to see how they fared against each other.")
        ),
        tabPanel("Possession",
                 plotOutput("possessionPlot")
        ),
        tabPanel("Goals",
                 plotOutput("goalsPlot")
        ),
        tabPanel("Assists",
                 plotOutput("assistsPlot")
        ),
        tabPanel("Goal Preventions", 
                 plotOutput("goalPreventionPlot")
        )
      )
    )
  )
)

# Define server
server <- function(input, output, session) {
  filtered_teams <- reactive({
    selected_group <- paste("Group", input$Group)
    unique(c(
      soccer %>% filter(group == selected_group) %>% pull(team1),
      soccer %>% filter(group == selected_group) %>% pull(team2)
    )) %>%
      sort()  # List team names alphabetically
  })
  
  observe({
    updateSelectInput(session, "Team", choices = filtered_teams())
  })
  
  filtered_opponents <- reactive({
    team <- input$Team
    unique(c(
      soccer %>% filter(team1 == team) %>% pull(team2),
      soccer %>% filter(team2 == team) %>% pull(team1)
    )) %>%
      sort()  # List opponent names alphabetically
  })
  
  observe({
    updateSelectInput(session, "Opponent", choices = filtered_opponents())
  })
  
  output$possessionPlot <- renderPlot({
    filtered_data <- soccer %>% 
      filter((team1 == input$Team & team2 == input$Opponent) | 
               (team2 == input$Team & team1 == input$Opponent))
    
    possession <- filtered_data %>% 
      mutate(possession_team = ifelse(team1 == input$Team, possession1, possession2),
             possession_opponent = ifelse(team1 == input$Team, possession2, possession1)) %>%
      summarise(possession_team = mean(possession_team),
                possession_opponent = mean(possession_opponent))
    
    possession_df <- data.frame(
      Team = c(input$Team, input$Opponent),
      Possession = c(possession$possession_team, possession$possession_opponent)
    )
    
    ggplot(possession_df, aes(x = "", y = Possession, fill = Team)) + 
      geom_bar(width = 1, stat = "identity") + 
      coord_polar("y") +
      labs(title = "Possession Percentage", x = "", y = "") + 
      theme_void() +
      theme(legend.position = "right") +
      scale_fill_manual(values = c("#1f77b4", "#ff7f0e")) +
      geom_text(aes(label = paste0(round(Possession, 1), "%")), position = position_stack(vjust = 0.5))
  })
  
  output$goalsPlot <- renderPlot({
    # Filter the data for the selected team and opponent
    filtered_data <- soccer %>% 
      filter((team1 == input$Team & team2 == input$Opponent) | 
               (team2 == input$Team & team1 == input$Opponent))
    
    goals <- filtered_data %>% 
      summarise(goals_team = sum(ifelse(team1 == input$Team, `number of goals team1`, `number of goals team2`)),
                goals_opponent = sum(ifelse(team1 == input$Team, `number of goals team2`, `number of goals team1`)))
    
    goals_df <- data.frame(
      Team = c(input$Team, input$Opponent),
      Goals = c(goals$goals_team, goals$goals_opponent)
    )
    
    ggplot(goals_df, aes(x = Team, y = Goals, fill = Team)) + 
      geom_bar(stat = "identity") + 
      labs(title = "Goals", x = "Team", y = "Number of Goals") + 
      geom_text(aes(label = Goals), vjust = -0.5) +
      theme_minimal()
  })
  
  output$assistsPlot <- renderPlot({
    filtered_data <- soccer %>% 
      filter((team1 == input$Team & team2 == input$Opponent) | 
               (team2 == input$Team & team1 == input$Opponent))
    
    assists <- filtered_data %>% 
      summarise(assists_team = sum(ifelse(team1 == input$Team, `assists team1`, `assists team2`)),
                assists_opponent = sum(ifelse(team1 == input$Team, `assists team2`, `assists team1`)))
    
    assists_df <- data.frame(
      Team = c(input$Team, input$Opponent),
      Assists = c(assists$assists_team, assists$assists_opponent)
    )
    
    ggplot(assists_df, aes(x = Team, y = Assists, fill = Team)) + 
      geom_bar(stat = "identity") + 
      labs(title = "Assists", x = "Team", y = "Number of Assists") + 
      geom_text(aes(label = Assists), vjust = -0.5) +
      theme_minimal()
  })
  
  output$goalPreventionPlot <- renderPlot({
    filtered_data <- soccer %>% 
      filter((team1 == input$Team & team2 == input$Opponent) | 
               (team2 == input$Team & team1 == input$Opponent))
    
    goal_prevention <- filtered_data %>% 
      summarise(goal_prevention_team = sum(ifelse(team1 == input$Team, `goal preventions team1`, `goal preventions team2`)),
                goal_prevention_opponent = sum(ifelse(team1 == input$Team, `goal preventions team2`, `goal preventions team1`)))
    
    goal_prevention_df <- data.frame(
      Team = c(input$Team, input$Opponent),
      GoalPrevention = c(goal_prevention$goal_prevention_team, goal_prevention$goal_prevention_opponent)
    )
    
    ggplot(goal_prevention_df, aes(x = Team, y = GoalPrevention, fill = Team)) + 
      geom_bar(stat = "identity") + 
      labs(title = "Goal Preventions", x = "Team", y = "Number of Goal Preventions") + 
      geom_text(aes(label = GoalPrevention), vjust = -0.5) +
      theme_minimal()
  })
  
  
  
}

# Run the app
shinyApp(ui, server)
