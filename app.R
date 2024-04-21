

theme_reach <- function() {
    theme_fivethirtyeight() +
        theme(
            legend.position = "none",
            plot.title = element_text(size = 20, hjust = 0.5, face = "bold"),
            plot.subtitle = element_text(size = 10, hjust = 0.5),
            axis.title.x = element_text(size=14),
            axis.title.y = element_text(size=14)
        )
}

#regular libraries
library(tidyverse) 
library(teamcolors) 
library(ggimage)
library(ggrepel)
library(ggthemes)
library(ggplot2)
library(knitr)
library(stringr)
library(shiny)


colors <- read_rds("data/colors.rds")

DownConv <- read_rds("data/DownConv.rds") %>% 
    arrange(posteam) %>% 
    filter(posteam != "CLE*")


OTTTaPR <- read_rds("data/OTTTaPR.rds") %>% 
    arrange(team_name)%>% 
    filter(team_name != "CLE*")


GenOffense <- read_rds("data/GenOffense.rds") %>% 
    arrange(posteam)%>% 
    filter(posteam != "CLE*")


Big <- read_rds("data/Bigdata.rds") %>% 
    arrange(posteam)%>% 
    filter(posteam != "CLE*")

BigAvg <- Big %>% 
    filter(season >= 2018) %>% 
    group_by() %>% 
    summarise(BoomAvg = mean(Boom, na.rm = T),
              BustAvg = mean(Bust, na.rm = T),
              BigHitAvg = mean(BigHit, na.rm = T),
              BigMissAvg = mean(BigMiss, na.rm = T),
              RateAvg = mean(Rate, na.rm = T),
              AvgydstogoAvg = mean(Avgydstogo),
              AvgairyardsAvg = mean(Avgairyards))

allowed <- read_rds("data/allowed.rds") 


allowedFill <- rbind(allowed, allowed, allowed,
                 allowed, allowed, allowed,
                 allowed) %>% 
    mutate(position = c("QB", "LT", "LG", "C", "RG", "RT", "Other"),
           Pressure = case_when(position == "QB" ~ self_percent,
                                position == "LT" ~ lt_percent,
                                position == "LG" ~ lg_percent,
                                position == "C" ~ ce_percent,
                                position == "RG" ~ rg_percent,
                                position == "RT" ~ rt_percent,
                                T ~ other_percent + te_percent)) %>% 
    group_by(position) %>% 
    mutate(PressureAvg = mean(Pressure, na.rm = T)) %>% 
    group_by() %>% 
    mutate(TotalAvg = mean(Pressure[position != "QB" & position != "Other"], na.rm = T))

level_order <- c("QB", "LT", "LG", "C", "RG", "RT", "Other")


OCfill <- DownConv %>% 
    group_by(OC) %>% 
    summarise(Count = n()) %>% 
    filter(Count >= 8)

GenOffAvg <- GenOffense %>% 
    filter(season >= 2011) %>% 
    summarise(HavocAvg = mean(Havoc, na.rm = T)*100,
              ExplosiveAvg = mean(Explosive, na.rm = T)*100,
              ExplosivePassAvg = mean(ExplosivePass, na.rm = T)*100,
              ExplosiveRushAvg = mean(ExplosiveRush, na.rm = T)*100,
              RunStuffAvg = mean(RunStuff, na.rm = T)*100,
              ADOTAvg = mean(ADOT, na.rm = T),
              TurnoversAvg = mean(Turnovers, na.rm = T))

GenOffAvgFill <- data.frame(
    Index = c("Havoc",
              "Explosive",
              "ExplosivePass",
              "ExplosiveRush",
              "RunStuff",
              "ADOT",
              "Turnovers"),
    Avg = c(head(GenOffAvg$HavocAvg, 1),
            head(GenOffAvg$ExplosiveAvg, 1),
            head(GenOffAvg$ExplosivePassAvg, 1),
            head(GenOffAvg$ExplosiveRushAvg, 1),
            head(GenOffAvg$RunStuffAvg, 1),
            head(GenOffAvg$ADOTAvg, 1),
            head(GenOffAvg$TurnoversAvg, 1)
    )
)

plot_title <- data.frame(
    Index = c("Havoc",
              "Explosive",
              "ExplosivePass",
              "ExplosiveRush",
              "RunStuff",
              "ADOT",
              "Turnovers"),
    Title = c("Havoc Percent",
              "Explosive Play Rate",
              "Explosive Pass Play Rate",
              "Explosive Rush Play Rate",
              "Run Stuff Percent",
              "Average Depth of Target",
              "Turnovers Lost"),
    Axis = c("Havoc Percent (%)",
             "Explosive Play Rate (%)",
             "Explosive Pass Play Rate (%)",
             "Explosive Rush Play Rate (%)",
             "Run Stuff Percent (%)",
             "Average Depth of Target",
             "Turnovers Lost"),
    subtitle = c("Havoc refers to (Plays with <= 0 yards gained + Forced Fumbles + Interceptions + Batted Pass + Pressures) / All Plays,
            Dashed Line is League Average from 2011-2023,
                 Lower = Better",
                 "Explosive Play Rate refers to how many passes gained >= 20 yards and Runs that gained >= 15 yards out of all attempts,
            Dashed Line is League Average from 2011-2023,
                 Higher = Better",
                 "Explosive Pass Play Rate refers to how many passes gained >= 20 yards out of all attempts,
            Dashed Line is League Average from 2011-2023,
                 Higher = Better",
                 "Explosive Rush Play Rate refers to how many Runs that gained >= 15 yards out of all attempts,
            Dashed Line is League Average from 2011-2023,
                 Higher = Better",
                 "Run Stuff Rate refers to the percent of run attempts that allow <= 0 yards,
            Dashed Line is League Average from 2011-2023,
                 Lower = Better",
                 "Average Depth of Target refers to the average air yards (yards from the line of scrimmage to target, not including Yards after Catch) on all throws,
            Dashed Line is League Average from 2011-2023,
                 Higher = Better",
                 "Interceptions and Fumbles Lost,
            Dashed Line is League Average from 2011-2023,
                 Lower = Better"))



defpress <- read_rds("data/defpress.rds") %>% 
  group_by(season) %>% 
  mutate(AGapAVG = mean(AgapPress, na.rm = T),
         BGapAVG = mean(BgapPress, na.rm = T),
         OutAVG = mean(OutPress, na.rm = T),
         OverAVG = mean(OverPress, na.rm = T)) %>% 
  group_by() %>% 
  mutate(TotalAvg = mean(TotalPressRate)) 

deflevel_order <- c("AgapPress", "BgapPress", "OverPress", "OutPress")

DDownConv <- read_rds("data/DDownConv.rds") %>% 
  arrange(defteam)


TTTaPR <- read_rds("data/TTTaPR.rds") %>% 
  arrange(defteam)


GenDefense <- read_rds("data/GenDefense.rds") %>% 
  arrange(defteam)

DCfill <- DDownConv %>% 
  group_by(DC) %>% 
  summarise(Count = n()) %>% 
  filter(Count >= 8)

GenDefAvg <- GenDefense %>% 
  filter(season >= 2011) %>% 
  summarise(HavocAvg = mean(Havoc, na.rm = T)*100,
            ExplosiveAllowedAvg = mean(ExplosiveAllowed, na.rm = T)*100,
            ExplosiveAllowedPassAvg = mean(ExplosiveAllowedPass, na.rm = T)*100,
            ExplosiveAllowedRushAvg = mean(ExplosiveAllowedRush, na.rm = T)*100,
            RunStuffAvg = mean(RunStuff, na.rm = T)*100,
            DADOTAvg = mean(DADOT, na.rm = T),
            TurnoversAvg = mean(Turnovers, na.rm = T))

GenDefAvgFill <- data.frame(
  Index = c("Havoc",
            "ExplosiveAllowed",
            "ExplosiveAllowedPass",
            "ExplosiveAllowedRush",
            "RunStuff",
            "DADOT",
            "Turnovers"),
  Avg = c(head(GenDefAvg$HavocAvg, 1),
          head(GenDefAvg$ExplosiveAllowedAvg, 1),
          head(GenDefAvg$ExplosiveAllowedPassAvg, 1),
          head(GenDefAvg$ExplosiveAllowedRushAvg, 1),
          head(GenDefAvg$RunStuffAvg, 1),
          head(GenDefAvg$DADOTAvg, 1),
          head(GenDefAvg$TurnoversAvg, 1)
  )
)

plot_titledef <- data.frame(
  Index = c("Havoc",
            "ExplosiveAllowed",
            "ExplosiveAllowedPass",
            "ExplosiveAllowedRush",
            "RunStuff",
            "DADOT",
            "Turnovers"),
  Title = c("Havoc Percent",
            "Explosive Play Rate Allowed",
            "Explosive Pass Play Rate Allowed",
            "Explosive Rush Play Rate Allowed",
            "Run Stuff Percent",
            "Defensive Average Depth of Target",
            "Turnovers Generated"),
  Axis = c("Havoc Percent (%)",
           "Explosive Play Rate Allowed (%)",
           "Explosive Pass Play Rate Allowed (%)",
           "Explosive Rush Play Rate Allowed (%)",
           "Run Stuff Percent (%)",
           "Defensive Average Depth of Target",
           "Turnovers Generated"),
  subtitle = c("Havoc refers to (Plays with <= 0 yards gained + Forced Fumbles + Interceptions + Batted Pass + Pressures) / All Plays,
            Dashed Line is League Average from 2011-2023,
                 Higher = Better",
               "Explosive Play Rate Allowed refers to how many passes gained >= 20 yards and Runs that gained >= 15 yards out of all attempts,
            Dashed Line is League Average from 2011-2023,
                 Lower = Better",
               "Explosive Pass Play Rate Allowed refers to how many passes gained >= 20 yards out of all attempts,
            Dashed Line is League Average from 2011-2023,
                 Lower = Better",
               "Explosive Rush Play Rate Allowed refers to how many Runs that gained >= 15 yards out of all attempts,
            Dashed Line is League Average from 2011-2023,
                 Lower = Better",
               "Run Stuff Rate refers to the percent of run attempts that allow <= 0 yards,
            Dashed Line is League Average from 2011-2023,
                 Higher = Better",
               "Defensive Average Depth of Target refers to the average air yards (yards from the line of scrimmage to target, not including Yards after Catch) on all throws against the defense,
            Dashed Line is League Average from 2011-2023,
                 Less Obvious, But I Believe Lower = Better",
               "Interceptions and Fumbles recoverd,
            Dashed Line is League Average from 2011-2023,
                 Higher = Better"))

# Define UI for application that draws a histogram
ui <- fluidPage(
    
    
    
    titlePanel("Offense & Defense Metrics Galore"),
    
    fluidRow( 
        tabsetPanel(
          tabPanel("Intro Page",
                   fluidRow(
                     column(width = 10, offset = 1,
                            helpText(HTML("This shiny app is designed for showing the offense and defense in more granular metrics. This includes moving the chains rates, time to throw adjusted pressure, and overall metrics like explosive play rate. Additionally, it allows for viewing these values across a coordinator's career.</br>
                        </br>
                        
                                   To check out my other shiny apps, follow one of the following links.</br>
                                   </br>

        <a href='https://seththedatascientist.shinyapps.io/QB_Bayesian_Updating/'>Bayesian Updating of composite metrics for Quarterback play for NFL and College QBs</a></br>

        This shiny app displays both college and pro QBs in two composite metrics that show not only their relative playstyles, but also how those values change over their careers. These values have a high correlation from college to pro for predicting playstyle once in the NFL</br>
        </br>

<a href='https://seththedatascientist.shinyapps.io/General_Manager_Toolbox/'>General Manager's Toolbox: A collection of tools to help analyze an NFL Team's Offseason moves.</a></br>

        This shiny app goes over a handful of useful data points that I have found very helpful for analyzing a team's offseason moves, including draft trade calculators (with some linear programming to try and ensure extra value by comparing the Jimmy Johnson trade chart to the Wins Above Replacement values), created metrics to analyze draft prospects in further detail, and team breakdowns of their effective cap and team structure over the coming years.</br>
        </br>

<a href='https://seththedatascientist.shinyapps.io/Offense_And_Defense_Metrics/'>Collection of Offense & Defense efficiency and playstyle metrics for the NFL</a></br>

        This shiny app includes a number of metrics used to understand Offense and Defense in further detail including down conversion values of how often you are allowing a first down or touchdown based on what down it currently is, explosive play rates, big throws and misses by quarterbacks, and more. Most metrics include a feature to isolate a playcaller's history of that metric across all teams they were the playcaller for.</br>
        </br>

<a href='https://seththedatascientist.shinyapps.io/Season_EPA_Tracker/'>Timeline of play measuring efficiency metrics, team season-long rankings, and team tier plots</a></br>

        This shiny app includes many iterations of breaking down expected points added (EPA) adjusted based on opponent strength and situation. Season long graphs to see individual team or starting quarterback trends, team plots for offense and defense including splits for passing and rushing, and a metric for team strength based on the relative margin of domination during the game as well as opponent strength.</br>
        </br>

<a href='https://seththedatascientist.shinyapps.io/WAR_positiongroup/'>Position Wins Above Replacement graphs by team and Watch Index</a></br>

        This shiny app shows Wins Above Replacement (WAR) values and plots for both college and pro broken down into many useful facets like by position group, team, and individual player. Includes receiver custom metrics plotted to compare players both within college and pro, as well as a customizable Watch Index which assigns a values based on relative values of excitement and closeness.</br>
        </br>
                  
                                       To check some of my other work, check out my <a href='https://twitter.com/SethDataScience'>Twitter</a>, <a href='https://www.linkedin.com/in/sethlanza/'>Linkedin</a>, or <a href='https://sites.google.com/view/seth-lanza-portfolio'>Portfolio</a>")),
                     )
                   ),
                   fluidRow(
                     column(width = 10, offset = 1,
                            textOutput("lastDeploymentTime")
                     )
                   )
          ),
            tabPanel("Offense Down Conversion Rates Allowed", 
                     # Sidebar with a slider input for number of bins 
                     fluidRow(
                         column(width = 5,
                                sliderInput(inputId = "seasonsDown",
                                            label = "Season Selection",
                                            min = min(DownConv$season),
                                            max = max(DownConv$season),
                                            value = c(2022,2023),
                                            step = 1,
                                            round = T,
                                            sep = ""),
                                
                                submitButton()
                         ),
                         
                         
                         column(width = 5,
                                sliderInput(inputId = "seasonsDownTeam",
                                            label = "Specific Team Season Selection",
                                            min = min(DownConv$season),
                                            max = max(DownConv$season),
                                            value = c(2018,2023),
                                            step = 1,
                                            round = T,
                                            sep = ""),
                                
                                selectizeInput(inputId = "teamDownTeam",
                                               label = "Team Namecode",
                                               choices = unique(DownConv$posteam),
                                               selected = "BUF"),
                                
                                selectInput(inputId = "OCDownTeam",
                                            label = "Offensive Playcaller (Ignores Above Selections)",
                                            choices = c("", sort(unique(OCfill$OC))))
                         )
                     ),
                     
                     
                     
                     # Show a plot of the generated distribution
                     fluidRow(
                         column(
                             plotOutput("DownPlot", height = 650),
                             width = 10, offset = 1
                         )
                     ), 
                     
                     fluidRow(
                         column(
                             plotOutput("DownTeamPlot", height = 650),
                             width = 10, offset = 1
                         )
                     )
            ),
            tabPanel("Time to Throw Adjusted Pressure Rates", 
                     
                     fluidRow(
                         column(width = 5,
                                sliderInput(inputId = "seasonsTTTaPR",
                                            label = "Season(s) Selection",
                                            min = min(OTTTaPR$season),
                                            max = max(OTTTaPR$season),
                                            value = c(2022,2023),
                                            step = 1,
                                            round = T,
                                            sep = ""),
                                submitButton()
                                
                         ),
                         
                         column(width = 5,
                                sliderInput(inputId = "seasonsTTTaPRTeam",
                                            label = "Specific Team Season Selection",
                                            min = min(OTTTaPR$season),
                                            max = max(OTTTaPR$season),
                                            value = c(2018,2023),
                                            step = 1,
                                            round = T,
                                            sep = ""),
                                
                                selectizeInput(inputId = "teamTTTaPR",
                                               label = "Team Namecode",
                                               choices = unique(OTTTaPR$team_name),
                                               selected = "BUF"),
                                
                                selectInput(inputId = "OCTTTaPR",
                                            label = "Offensive Playcaller (Ignores Above Selections)",
                                            choices = c("", sort(unique(OCfill$OC))))
                         )
                     ),
                     
                     
                     
                     
                     # Show a plot of the generated distribution
                     fluidRow(
                         column(
                             plotOutput("TTTaPRPlot", height = 650),
                             width = 10, offset = 1
                         ),
                         column(
                             plotOutput("TTTaPRTeamPlot", height = 650),
                             width = 10, offset = 1
                         ),
                         column(
                             plotOutput("AllowedPlot", height = 650),
                             width = 10, offset = 1
                         )
                     )
            ),
            tabPanel("General Offense Metrics", 
                     
                     fluidRow(
                         column(width = 5,
                                sliderInput(inputId = "seasonsGenOffense",
                                            label = "Season(s) Selection",
                                            min = min(GenOffense$season),
                                            max = max(GenOffense$season),
                                            value = c(2022,2023),
                                            step = 1,
                                            round = T,
                                            sep = ""),
                                
                                selectizeInput(inputId = "typeGenOffense",
                                               label = "Type of Graph",
                                               choices = c("Havoc Percent" = "Havoc",
                                                           "Explosive Play Rate" = "Explosive",
                                                           "Explosive Pass Play Rate" = "ExplosivePass",
                                                           "Explosive Rush Play Rate" = "ExplosiveRush",
                                                           "Average Depth of Target" = "ADOT",
                                                           "Run Stuff %" = "RunStuff",
                                                           "Turnovers")),
                                
                                submitButton()
                                
                         ),
                         
                         column(width = 5,
                                sliderInput(inputId = "seasonsGenOffenseTeam",
                                            label = "Specific Team Season Selection",
                                            min = min(GenOffense$season),
                                            max = max(GenOffense$season),
                                            value = c(2018,2023),
                                            step = 1,
                                            round = T,
                                            sep = ""),
                                
                                selectizeInput(inputId = "teamGenOffense",
                                               label = "Team Namecode",
                                               choices = unique(GenOffense$posteam),
                                               selected = "BUF"),
                                
                                selectInput(inputId = "OCGenOffense",
                                            label = "Offensive Playcaller (Ignores Above Selections)",
                                            choices = c("", sort(unique(OCfill$OC))))
                         )
                     ),
                     
                     # Show a plot of the generated distribution
                     fluidRow(
                         column(
                             plotOutput("GenOffensePlot", height = 650),
                             width = 10, offset = 1
                         ),
                         
                         column(
                             plotOutput("GenOffenseTeamPlot", height = 650),
                             width = 10, offset = 1
                         )
                     )
                     
            ),
            
            tabPanel("QB Metrics", 
                     # Sidebar with a slider input for number of bins 
                     fluidRow(
                         column(width = 5,
                                sliderInput(inputId = "seasonsBig",
                                            label = "Season Selection",
                                            min = min(Big$season),
                                            max = max(Big$season),
                                            value = c(2022,2023),
                                            step = 1,
                                            round = T,
                                            sep = ""),
                                
                                               
                                submitButton()
                            )
                         ) ,
                     
                     
                     
                     # Show a plot of the generated distribution
                     fluidRow(
                         column(
                             plotOutput("BoomPlot", height = 650),
                             width = 10, offset = 1
                         )
                     ),
                     
                     fluidRow(
                         column(
                             plotOutput("BigHitPlot", height = 650),
                             width = 10, offset = 1
                         )
                     ),
                     
                     fluidRow(
                         column(
                             plotOutput("ThirdPlot", height = 650),
                             width = 10, offset = 1
                         )
                     )
                ),
    tabPanel("Defense Down Conversion Rates Allowed", 
               # Sidebar with a slider input for number of bins 
               fluidRow(
                 column(width = 5,
                        sliderInput(inputId = "seasonsDDown",
                                    label = "Season Selection",
                                    min = min(DDownConv$season),
                                    max = max(DDownConv$season),
                                    value = c(2022,2023),
                                    step = 1,
                                    round = T,
                                    sep = ""),
                        
                        submitButton()
                 ),
                 
                 
                 column(width = 5,
                        sliderInput(inputId = "seasonsDDownTeam",
                                    label = "Specific Team Season Selection",
                                    min = min(DDownConv$season),
                                    max = max(DDownConv$season),
                                    value = c(2018,2023),
                                    step = 1,
                                    round = T,
                                    sep = ""),
                        
                        selectizeInput(inputId = "teamDDownTeam",
                                       label = "Team Namecode",
                                       choices = unique(DDownConv$defteam),
                                       selected = "BUF"),
                        
                        selectInput(inputId = "DCDDownTeam",
                                    label = "Defensive Coordinator (Ignores Above Selections)",
                                    choices = c("", sort(unique(DCfill$DC))))
                 )
               ),
               
               
               
               # Show a plot of the generated distribution
               fluidRow(
                 column(
                   plotOutput("DDownPlot", height = 650),
                   width = 10, offset = 1
                 )
               ), 
               
               fluidRow(
                 column(
                   plotOutput("DDownTeamPlot", height = 650),
                   width = 10, offset = 1
                 )
               )
      ),
      tabPanel("Defense Time to Throw Adjusted Pressure Rates", 
               
               fluidRow(
                 column(width = 5,
                        sliderInput(inputId = "seasonsTTTaPR",
                                    label = "Season(s) Selection",
                                    min = min(TTTaPR$season),
                                    max = max(TTTaPR$season),
                                    value = c(2022,2023),
                                    step = 1,
                                    round = T,
                                    sep = ""),
                        submitButton()
                        
                 ),
                 
                 column(width = 5,
                        sliderInput(inputId = "seasonsTTTaPRTeam",
                                    label = "Specific Team Season Selection",
                                    min = min(TTTaPR$season),
                                    max = max(TTTaPR$season),
                                    value = c(2018,2023),
                                    step = 1,
                                    round = T,
                                    sep = ""),
                        
                        selectizeInput(inputId = "teamTTTaPR",
                                       label = "Team Namecode",
                                       choices = unique(TTTaPR$defteam),
                                       selected = "BUF"),
                        
                        selectInput(inputId = "DCTTTaPR",
                                    label = "Defensive Coordinator (Ignores Above Selections)",
                                    choices = c("", sort(unique(DCfill$DC))))
                 )
               ),
               
               
               
               
               # Show a plot of the generated distribution
               fluidRow(
                 column(
                   plotOutput("defTTTaPRPlot", height = 650),
                   width = 10, offset = 1
                 ),
                 column(
                   plotOutput("defTTTaPRTeamPlot", height = 650),
                   width = 10, offset = 1
                 ),
                 column(
                   plotOutput("defAllowedPlot", height = 650),
                   width = 10, offset = 1
                 )
               )
      ),
      tabPanel("General Defense Metrics", 
               
               fluidRow(
                 column(width = 5,
                        sliderInput(inputId = "seasonsGenDefense",
                                    label = "Season(s) Selection",
                                    min = min(GenDefense$season),
                                    max = max(GenDefense$season),
                                    value = c(2022,2023),
                                    step = 1,
                                    round = T,
                                    sep = ""),
                        
                        selectizeInput(inputId = "typeGenDefense",
                                       label = "Type of Graph",
                                       choices = c("Havoc Percent" = "Havoc",
                                                   "Explosive Play Allowed" = "ExplosiveAllowed",
                                                   "Explosive Pass Play Allowed" = "ExplosiveAllowedPass",
                                                   "Explosive Rush Play Allowed" = "ExplosiveAllowedRush",
                                                   "Defensive Average Depth of Target" = "DADOT",
                                                   "Run Stuff %" = "RunStuff",
                                                   "Turnovers")),
                        
                        submitButton()
                        
                 ),
                 
                 column(width = 5,
                        sliderInput(inputId = "seasonsGenDefenseTeam",
                                    label = "Specific Team Season Selection",
                                    min = min(GenDefense$season),
                                    max = max(GenDefense$season),
                                    value = c(2018,2023),
                                    step = 1,
                                    round = T,
                                    sep = ""),
                        
                        selectizeInput(inputId = "teamGenDefense",
                                       label = "Team Namecode",
                                       choices = unique(GenDefense$defteam),
                                       selected = "BUF"),
                        
                        selectInput(inputId = "DCGenDefense",
                                    label = "Defensive Coordinator (Ignores Above Selections)",
                                    choices = c("", sort(unique(DCfill$DC))))
                 )
               ),
               
               # Show a plot of the generated distribution
               fluidRow(
                 column(
                   plotOutput("GenDefensePlot", height = 650),
                   width = 10, offset = 1
                 ),
                 
                 column(
                   plotOutput("GenDefenseTeamPlot", height = 650),
                   width = 10, offset = 1
                 )
               )
               
      )
    )
      )
    )


# Define server logic required to draw a histogram
server <- function(input, output) {
    
  getLastDeploymentTime <- function() {
    timestamp <- tryCatch(
      readLines("deployment_timestamp.txt"),
      error = function(e) NA
    )
    if (!is.na(timestamp)) {
      as.POSIXct(timestamp, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
    } else {
      NA
    }
  }
  
  # Display the last deployment time
  output$lastDeploymentTime <- renderText({
    lastDeploymentTime <- getLastDeploymentTime()
    if (!is.na(lastDeploymentTime)) {
      paste("Last Deployment Time: ", format(lastDeploymentTime, "%Y-%m-%d %H:%M:%S"))
    } else {
      "Deployment time not available."
    }
  })
  
    output$DownPlot <- renderPlot({
        
        DownConv %>%
            filter(between(season, input$seasonsDown[1], input$seasonsDown[2])) %>% 
            group_by(posteam, down) %>% 
            summarise(EarlyDown = mean(EarlyDown),
                      Rate = mean(Rate),
                      ConversionRate = mean(ConversionRate),
                      NiceRate = paste0(round(ConversionRate, 2)*100,"%"),
                      url = head(url, 1),
                      MaxRate = mean(MaxRate)) %>% 
            ggplot(aes(x = reorder(posteam, -(EarlyDown)),
                       y = Rate))+
            geom_col(aes(fill = (down)))+
            geom_image(aes(image = url, y = MaxRate + 0.065))+
            geom_hline(yintercept = 0.67, linetype = "dashed", color = "white",
                       size = 1)+
            geom_hline(yintercept = 0.47, linetype = "dashed", color = "white",
                       size = 1)+
            geom_hline(yintercept = 0.22, linetype = "dashed", color = "white",
                       size = 1)+
            scale_y_continuous(breaks = scales::pretty_breaks(n = 5),
                               labels = scales::percent_format(accuracy = 1),
                               limits = c(0,1))+
            scale_color_identity(aesthetics = "color")+
            labs(
                y= "Converstion Rate",
                x= "Team",
                title= paste0(
                    input$seasonsDown[1],"-", 
                    input$seasonsDown[2],
                    " Offense Conversion Rate by Down"),
                subtitle = "Sorted by Early Down (1st & 2nd) Conversion Rate, White Dashed lines represent Rough Averages from 2011-2023 for 1st-3rd Down",
                caption = "@SethDataScience"
            ) +
            theme_reach() +
            geom_text(data = DownConv%>%
                          filter(between(season, input$seasonsDown[1], input$seasonsDown[2]),
                                 down != 4) %>% 
                          group_by(posteam, down) %>% 
                          mutate(Rate = mean(Rate),
                                 ConversionRate = mean(ConversionRate),
                                 NiceRate = paste0(round(ConversionRate, 2)*100,"%")),
                      aes(y = ConversionRate, label = NiceRate),
                      nudge_y = -0.015, size = 4, fontface = "bold", color = "gray"
            )  +
            geom_text(data = DownConv%>%
                          filter(between(season, input$seasonsDown[1], input$seasonsDown[2]),
                                 down == 4) %>% 
                          group_by(posteam, down) %>% 
                          mutate(Rate = mean(Rate),
                                 ConversionRate = mean(ConversionRate),
                                 NiceRate = paste0(round(ConversionRate, 2)*100,"%")),
                      aes(y = ConversionRate, label = NiceRate),
                      nudge_y = 0.015, size = 4, fontface = "bold"
            ) 
    })
    
    output$DownTeamPlot <- renderPlot({
        
        DownConv %>%
            filter(between(season, 
                           case_when(input$OCDownTeam == "" ~ input$seasonsDownTeam[1],
                                     T ~ as.integer(0)),
                           case_when(input$OCDownTeam == "" ~ input$seasonsDownTeam[2],
                                     T ~ as.integer(3000))), 
                   posteam == case_when(input$OCDownTeam == "" ~ input$teamDownTeam,
                                        T ~ posteam),
                   OC == case_when(input$OCDownTeam == "" ~ OC,
                                   T ~ input$OCDownTeam)) %>% 
            ggplot(aes(x = season,
                       y = Rate))+
            geom_col(aes(fill = (down)))+
            geom_image(aes(image = url, y = MaxRate + 0.065))+
            geom_hline(yintercept = 0.67, linetype = "dashed", color = "white",
                       size = 1)+
            geom_hline(yintercept = 0.47, linetype = "dashed", color = "white",
                       size = 1)+
            geom_hline(yintercept = 0.22, linetype = "dashed", color = "white",
                       size = 1)+
            scale_y_continuous(breaks = scales::pretty_breaks(n = 5),
                               labels = scales::percent_format(accuracy = 1),
                               limits = c(0,1))+
            scale_x_continuous(breaks = seq(1999,2030,1))+
            scale_color_identity(aesthetics = "color")+
            labs(
                y= "Converstion Rate",
                x= "Season",
                title= paste0(
                    case_when(input$OCDownTeam == "" ~ input$seasonsDownTeam[1],
                              T ~ as.integer(
                                  min(DownConv$season[DownConv$OC == input$OCDownTeam]))
                    ),"-", 
                    case_when(input$OCDownTeam == "" ~ input$seasonsDownTeam[2],
                              T ~ as.integer(
                                  max(DownConv$season[DownConv$OC == input$OCDownTeam]))
                    )," ",
                    case_when(input$OCDownTeam == "" ~ input$teamDownTeam,
                              T ~ input$OCDownTeam),
                    " Offense Conversion Rate by Down"),
                subtitle = "White Dashed lines represent Rough Averages from 2011-2023 for 1st-3rd Down",
                caption = "@SethDataScience"
            ) +
            theme_reach() +
            geom_text(data = DownConv%>%
                          filter(between(season, 
                                         case_when(input$OCDownTeam == "" ~ input$seasonsDownTeam[1],
                                                   T ~ as.integer(0)),
                                         case_when(input$OCDownTeam == "" ~ input$seasonsDownTeam[2],
                                                   T ~ as.integer(3000))), 
                                 posteam == case_when(input$OCDownTeam == "" ~ input$teamDownTeam,
                                                      T ~ posteam),
                                 OC == case_when(input$OCDownTeam == "" ~ OC,
                                                 T ~ input$OCDownTeam), down != 4),
                      aes(y = ConversionRate, label = NiceRate),
                      nudge_y = -0.015, size = 4, fontface = "bold", color = "gray"
            )  +
            geom_text(data = DownConv%>%
                          filter(between(season, 
                                         case_when(input$OCDownTeam == "" ~ input$seasonsDownTeam[1],
                                                   T ~ as.integer(0)),
                                         case_when(input$OCDownTeam == "" ~ input$seasonsDownTeam[2],
                                                   T ~ as.integer(3000))), 
                                 posteam == case_when(input$OCDownTeam == "" ~ input$teamDownTeam,
                                                      T ~ posteam),
                                 OC == case_when(input$OCDownTeam == "" ~ OC,
                                                 T ~ input$OCDownTeam), down == 4),
                      aes(y = ConversionRate, label = NiceRate),
                      nudge_y = 0.015, size = 4, fontface = "bold"
            ) 
    })
    
    
    output$BoomPlot <- renderPlot({
        
        Big %>%
            filter(between(season, input$seasonsBig[1], input$seasonsBig[2])) %>% 
            group_by(posteam, QBname) %>% 
            summarise(BigHit = mean(BigHit),
                      BigMiss = mean(BigMiss),
                      Boom = mean(Boom),
                      Bust = mean(Bust),
                      Count = mean(Count),
                      Avgydstogo = mean(Avgydstogo),
                      Avgairyards = mean(Avgairyards),
                      Rate = mean(Rate),
                      primary = head(primary, 1),
                      secondary = head(secondary, 1)) %>% 
            ggplot(aes(x = Boom, y = Bust))+
            geom_point(aes(size = Count, color = primary, fill = secondary))+
            geom_vline(xintercept = BigAvg$BoomAvg, alpha = 0.75, color = "red", 
                       linetype = "dashed")+
            geom_hline(yintercept = BigAvg$BustAvg, alpha = 0.75, color = "red", 
                       linetype = "dashed")+
            scale_color_identity(aesthetics = c("color", "fill")) +
            scale_x_continuous(breaks = scales::pretty_breaks(n = 10),
                               labels = scales::percent_format(accuracy = 1))+
            scale_y_reverse(breaks = scales::pretty_breaks(n = 10),
                               labels = scales::percent_format(accuracy = 1))+
            theme_reach()+
            labs(title = paste0(
                input$seasonsBig[1], "-",
                input$seasonsBig[2],
                " ",
                "Boom vs Bust Rates for QB"),
                 subtitle = "Size = Count, Red lines represent Mean Values from 2011-2023",
                 x = "Boom Rate (Air_EPA >= 1)",
                 y = "Bust Rate (Air_EPA <= -1)",
                caption = "@SethDataScience")+
        ggrepel::geom_text_repel(aes(label = QBname), size = 4, box.padding = 0.2,
                                 force = 30, max.overlaps = Inf,
                                 min.segment.length = 0)
        
    
    })
    
    output$BigHitPlot <- renderPlot({
        
        Big %>%
            filter(between(season, input$seasonsBig[1], input$seasonsBig[2])) %>% 
            group_by(posteam,QBname) %>% 
            summarise(BigHit = mean(BigHit),
                      BigMiss = mean(BigMiss),
                      Boom = mean(Boom),
                      Bust = mean(Bust),
                      Count = mean(Count),
                      Avgydstogo = mean(Avgydstogo),
                      Avgairyards = mean(Avgairyards),
                      Rate = mean(Rate),
                      primary = head(primary, 1),
                      secondary = head(secondary, 1)) %>% 
            ggplot(aes(x = BigHit, y = BigMiss))+
            geom_point(aes(size = Count, color = primary, fill = secondary))+
            geom_vline(xintercept = BigAvg$BigHitAvg, alpha = 0.75, color = "red", 
                       linetype = "dashed")+
            geom_hline(yintercept = BigAvg$BigMissAvg, alpha = 0.75, color = "red", 
                       linetype = "dashed")+
            scale_color_identity(aesthetics = c("color", "fill")) +
            scale_x_continuous(breaks = scales::pretty_breaks(n = 10),
                               labels = scales::percent_format(accuracy = 1))+
            scale_y_reverse(breaks = scales::pretty_breaks(n = 10),
                            labels = scales::percent_format(accuracy = 1))+
            theme_reach()+
            labs(title = paste0(
                input$seasonsBig[1], "-",
                input$seasonsBig[2],
                " ",
                "Big Hit vs Big Miss Rates for QB"),
                subtitle = "Size = Count, Red lines represent Mean Values from 2011-2023",
                x = "Big Hit Rate (Completed Pass with cp <= 0.65)",
                y = "Big Miss Rate (Incomplete Pass with cp >= 0.65)",
                caption = "@SethDataScience")+
            ggrepel::geom_text_repel(aes(label = QBname), size = 4, box.padding = 0.2,
                                     force = 30, max.overlaps = Inf,
                                     min.segment.length = 0)
        
        
    })
    
    output$ThirdPlot <- renderPlot({
        
        Big %>%
            filter(between(season, input$seasonsBig[1], input$seasonsBig[2])) %>% 
            group_by(posteam, QBname) %>% 
            summarise(BigHit = mean(BigHit),
                      BigMiss = mean(BigMiss),
                      Boom = mean(Boom),
                      Bust = mean(Bust),
                      Count = mean(Count),
                      Avgydstogo = mean(Avgydstogo),
                      Avgairyards = mean(Avgairyards),
                      Rate = mean(Rate),
                      primary = head(primary, 1),
                      secondary = head(secondary, 1)) %>% 
        ggplot(aes(x = Avgairyards, y = Rate))+
        geom_point(aes(size = Count, color = primary))+
        scale_color_identity(aesthetics = c("color", "fill")) +
        scale_x_continuous(breaks = scales::pretty_breaks(n = 10))+
        scale_y_continuous(breaks = scales::pretty_breaks(n = 10),
                           labels = scales::percent_format(accuracy = 1))+
        geom_hline(yintercept = BigAvg$RateAvg, linetype = "dashed")+
            geom_vline(xintercept = 10, linetype = "dashed")+
            geom_vline(xintercept = BigAvg$AvgydstogoAvg, alpha = 0.5, color = "red")+
        theme_reach()+
        labs(title = paste0(
            input$seasonsBig[1], "-",
            input$seasonsBig[2],
            " ",
            "Air Yards vs Conversion Rate on 3rd and 7+ throws"),
             subtitle = "0.2 < WP < 0.8, != 2 min drill,
           Size = Count, Red lines represent Average Yards to Go",
             y = "Conversion Rate",
             x = "Average Air Yards")+
            ggrepel::geom_text_repel(aes(label = QBname), size = 4, box.padding = 0.2,
                                     force = 30, max.overlaps = Inf,
                                     min.segment.length = 0)
        
        
    })
    
    
    output$TTTaPRPlot <- renderPlot({
        
        OTTTaPR %>%
            filter(between(season, input$seasonsTTTaPR[1], input$seasonsTTTaPR[2])) %>% 
            group_by(team_name) %>% 
            summarise(Actual = mean(Actual),
                      Expected = mean(Expected),
                      url = head(url, 1)) %>% 
            ggplot(aes(x = Expected, y = Actual))+
            geom_image(aes(image = url))+
            geom_abline(intercept = mean(OTTTaPR$Actual, na.rm = T) +
                            mean(OTTTaPR$Expected, na.rm = T), slope = -1,
                        linetype = 3,
                        size = 2)+
            geom_hline(yintercept = 30, linetype = 3,
                       color = "blue", size = 2)+
            geom_hline(yintercept = 35, linetype = 3,
                       color = "red", size = 2)+
            scale_color_identity(aesthetics = "color")+
            labs(
                y= "Actual Pressure Rate (%)",
                x= "Expected Pressure Rate (%)",
                title= paste0(
                    input$seasonsTTTaPR[1],"-", 
                    input$seasonsTTTaPR[2],
                    " Actual vs Expected Pressure Rate"),
                subtitle = "Expected Pressure Rate derived from a Linear Regression model that takes Pressured/Non-Pressured Time to Throw as inputs,
    Sloped Line represents if you are Above/Below Expectation, Red Line is 35% and Blue Line is 30% which represent rough high and low thresholds, respectively",
                caption = "@SethDataScience"
            ) +
            theme_reach()
    })
    
    
    output$TTTaPRTeamPlot <- renderPlot({
        
        OTTTaPR %>%
            filter(between(season, 
                           case_when(input$OCTTTaPR == "" ~ input$seasonsTTTaPRTeam[1],
                                     T ~ as.integer(0)),
                           case_when(input$OCTTTaPR == "" ~ input$seasonsTTTaPRTeam[2],
                                     T ~ as.integer(3000))), 
                   team_name == case_when(input$OCTTTaPR == "" ~ input$teamTTTaPR,
                                        T ~ team_name),
                   OC == case_when(input$OCTTTaPR == "" ~ OC,
                                   T ~ input$OCTTTaPR)) %>%
            ggplot(aes(x = Expected, y = Actual))+
            geom_image(aes(image = url))+
            geom_abline(intercept = mean(OTTTaPR$Actual, na.rm = T) +
                            mean(OTTTaPR$Expected, na.rm = T), slope = -1,
                        linetype = 3,
                        size = 2)+
            geom_hline(yintercept = 30, linetype = 3,
                       color = "blue", size = 2)+
            geom_hline(yintercept = 35, linetype = 3,
                       color = "red", size = 2)+
            scale_color_identity(aesthetics = "color")+
            labs(
                y= "Actual Pressure Rate (%)",
                x= "Expected Pressure Rate (%)",
                title= paste0(
                    case_when(input$OCTTTaPR == "" ~ input$seasonsTTTaPRTeam[1],
                              T ~ as.integer(
                                  min(OTTTaPR$season[OTTTaPR$OC == input$OCTTTaPR]))
                    ),"-", 
                    case_when(input$OCTTTaPR == "" ~ input$seasonsTTTaPRTeam[2],
                              T ~ as.integer(
                                  max(OTTTaPR$season[OTTTaPR$OC == input$OCTTTaPR]))
                    )," ",
                    case_when(input$OCTTTaPR == "" ~ input$teamTTTaPR,
                              T ~ input$OCTTTaPR),
                    " Actual vs Expected Pressure Rate"),
                subtitle = "Expected Pressure Rate derived from a Linear Regression model that takes Pressured/Non-Pressured Time to Throw as inputs,
    Sloped Line represents if you are Above/Below Expectation, Red Line is 35% and Blue Line is 30% which represent rough high and low thresholds, respectively",
                caption = "@SethDataScience"
            ) +
            theme_reach()+
            geom_text(data = OTTTaPR %>%
                          filter(between(season, 
                                         case_when(input$OCTTTaPR == "" ~ input$seasonsTTTaPRTeam[1],
                                                   T ~ as.integer(0)),
                                         case_when(input$OCTTTaPR == "" ~ input$seasonsTTTaPRTeam[2],
                                                   T ~ as.integer(3000))), 
                                 team_name == case_when(input$OCTTTaPR == "" ~ input$teamTTTaPR,
                                                        T ~ team_name),
                                 OC == case_when(input$OCTTTaPR == "" ~ OC,
                                                 T ~ input$OCTTTaPR)),
                      aes(label = paste0(substr(season, 3, 4))),
                      nudge_y = -0.02, nudge_x = 0.03,
                      size = 4, fontface = "bold"
            ) 
        
    })
    
    output$AllowedPlot <- renderPlot({
        
        allowedFill %>%
            filter(between(season, 
                           input$seasonsTTTaPRTeam[1],
                            input$seasonsTTTaPRTeam[2]), 
                   team_name == input$teamTTTaPR) %>%
            group_by(team_name, position) %>% 
            summarise(Pressure = mean(Pressure, na.rm = T)) %>%
            group_by(position) %>% 
            mutate(BelowAvg = case_when(Pressure >= mean(allowedFill$PressureAvg) ~ 1,
                                        T~ 0.65)) %>% 
            ggplot(aes(x = factor(position, level = level_order),
                y = (Pressure/100)))+
        geom_col(aes(fill = colors$primary[colors$Code == input$teamTTTaPR],
                     color = colors$secondary[colors$Code == input$teamTTTaPR],
                     alpha = BelowAvg))+
            geom_hline(yintercept = allowedFill$TotalAvg/100, linetype = "dashed",
                       color = "red")+
            scale_color_identity(aesthetics = c("color", "fill")) +
            scale_alpha_identity()+
            scale_y_continuous(breaks = scales::pretty_breaks(n = 10),
                               labels = scales::percent_format(accuracy = 1))+
            labs(
                y= "Percent of Pressure Allowed",
                x= "Position with Responsibility",
                title = paste0(
                    input$seasonsTTTaPRTeam[1], "-",
                    input$seasonsTTTaPRTeam[2],
                    " ",
                    input$teamTTTaPR,
                    " Allowed Pressure Share of Responsibility"),
                subtitle = "Does not necessarily sum to 100% as it is a mean of multiple years and multiple players could be responsible for a single pressure,
                Red line is Mean Average Pressure Share from All OL positions across 2011-2023. Lower Transparency Columns are Better than Positional Average for Pressure Share.",
                caption = "@SethDataScience"
            ) +
            theme_reach() 
            
            
    })
    
    output$GenOffensePlot <- renderPlot({
        
        GenOffense %>% 
            filter(between(season, input$seasonsGenOffense[1], input$seasonsGenOffense[2])) %>%
            group_by(posteam) %>% 
            summarise(url = head(url, 1),
                      primary = head(primary, 1),
                      secondary = head(secondary, 1),
                      Havoc = mean(Havoc, na.rm = T)*100,
                      Explosive = mean(Explosive, na.rm = T)*100,
                      ExplosivePass = mean(ExplosivePass, na.rm = T)*100,
                      ExplosiveRush = mean(ExplosiveRush, na.rm = T)*100,
                      RunStuff = mean(RunStuff, na.rm = T)*100,
                      ADOT = mean(ADOT, na.rm = T),
                      Turnovers = mean(Turnovers, na.rm = T)) %>% 
            ggplot(aes(x = reorder(posteam, -.data[[input$typeGenOffense]]),
                       y = .data[[input$typeGenOffense]]))+
            geom_col(aes(color = secondary, fill = primary))+
            geom_image(aes(image = url))+
            geom_hline(yintercept = GenOffAvgFill$Avg[GenOffAvgFill$Index == input$typeGenOffense],
                       linetype = "dashed",
                       color = "black", size = 1)+
            theme_reach()+
            theme(axis.text.x = element_text(size=12))+
            scale_color_identity(aesthetics = c("color", "fill"))+
            labs(title = paste0(
                input$seasonsGenOffense[1], "-",
                input$seasonsGenOffense[2],
                " ",
                plot_title$Title[plot_title$Index == input$typeGenOffense]),
                subtitle = plot_title$subtitle[plot_title$Index == input$typeGenOffense],
                y = plot_title$Axis[plot_title$Index == input$typeGenOffense],
                x = "Team",
                caption = "@SethDataScience"
            )
    })
    
    
    output$GenOffenseTeamPlot <- renderPlot({
        
        GenOffense %>% 
            filter(between(season, 
                           case_when(input$OCGenOffense == "" ~ input$seasonsGenOffenseTeam[1],
                                     T ~ as.integer(0)),
                           case_when(input$OCGenOffense == "" ~ input$seasonsGenOffenseTeam[2],
                                     T ~ as.integer(3000))), 
                   posteam == case_when(input$OCGenOffense == "" ~ input$teamGenOffense,
                                        T ~ posteam),
                   OC == case_when(input$OCGenOffense == "" ~ OC,
                                   T ~ input$OCGenOffense)) %>%
            mutate(Havoc = Havoc * 100,
                   Explosive = Explosive * 100,
                   ExplosivePass = ExplosivePass * 100,
                   ExplosiveRush = ExplosiveRush * 100,
                   RunStuff = RunStuff * 100) %>% 
            ggplot(aes(x = season,
                       y = .data[[input$typeGenOffense]]))+
            geom_col(aes(color = secondary, fill = primary))+
            geom_image(aes(image = url))+
            geom_hline(yintercept = GenOffAvgFill$Avg[GenOffAvgFill$Index == input$typeGenOffense],
                       linetype = "dashed",
                       color = "black", size = 1)+
            theme_reach()+
            theme(axis.text.x = element_text(size=12))+
            scale_color_identity(aesthetics = c("color", "fill"))+
            scale_x_continuous(breaks = seq(1999,2030,1))+
            scale_y_continuous(breaks = scales::pretty_breaks(n = 5))+
            labs(title = paste0(
                case_when(input$OCGenOffense == "" ~ input$seasonsGenOffenseTeam[1],
                          T ~ as.integer(
                              min(GenOffense$season[GenOffense$OC == input$OCGenOffense]))
                ),"-", 
                case_when(input$OCGenOffense == "" ~ input$seasonsGenOffenseTeam[2],
                          T ~ as.integer(
                              max(GenOffense$season[GenOffense$OC == input$OCGenOffense]))
                ),
                " ",
                case_when(input$OCGenOffense == "" ~ input$teamGenOffense,
                          T ~ input$OCGenOffense), " ",
                plot_title$Title[plot_title$Index == input$typeGenOffense]),
                subtitle = plot_title$subtitle[plot_title$Index == input$typeGenOffense],
                y = plot_title$Axis[plot_title$Index == input$typeGenOffense],
                x = "Season",
                caption = "@SethDataScience"
            )
    })  
    
    
    
    
    output$DDownPlot <- renderPlot({
      
      DDownConv %>%
        filter(between(season, input$seasonsDDown[1], input$seasonsDDown[2])) %>% 
        group_by(defteam, down) %>% 
        summarise(EarlyDown = mean(EarlyDown),
                  Rate = mean(Rate),
                  ConversionRate = mean(ConversionRate),
                  NiceRate = paste0(round(ConversionRate, 2)*100,"%"),
                  url = head(url, 1),
                  MaxRate = mean(MaxRate)) %>% 
        ggplot(aes(x = reorder(defteam, (EarlyDown)),
                   y = Rate))+
        geom_col(aes(fill = (down)))+
        geom_image(aes(image = url, y = MaxRate + 0.065))+
        geom_hline(yintercept = 0.67, linetype = "dashed", color = "white",
                   size = 1)+
        geom_hline(yintercept = 0.47, linetype = "dashed", color = "white",
                   size = 1)+
        geom_hline(yintercept = 0.22, linetype = "dashed", color = "white",
                   size = 1)+
        scale_y_continuous(breaks = scales::pretty_breaks(n = 5),
                           labels = scales::percent_format(accuracy = 1),
                           limits = c(0,1))+
        scale_color_identity(aesthetics = "color")+
        labs(
          y= "Converstion Rate",
          x= "Team",
          title= paste0(
            input$seasonsDDown[1],"-", 
            input$seasonsDDown[2],
            " Defense Conversion Rate by Down"),
          subtitle = "Sorted by Early Down (1st & 2nd) Conversion Rate, White Dashed lines represent Rough Averages from 2011-2023 for 1st-3rd Down",
          caption = "@SethDataScience"
        ) +
        theme_reach() +
        geom_text(data = DDownConv%>%
                    filter(between(season, input$seasonsDDown[1], input$seasonsDDown[2]),
                           down != 4) %>% 
                    group_by(defteam, down) %>% 
                    mutate(Rate = mean(Rate),
                           ConversionRate = mean(ConversionRate),
                           NiceRate = paste0(round(ConversionRate, 2)*100,"%")),
                  aes(y = ConversionRate, label = NiceRate),
                  nudge_y = -0.015, size = 4, fontface = "bold", color = "gray"
        )  +
        geom_text(data = DDownConv%>%
                    filter(between(season, input$seasonsDDown[1], input$seasonsDDown[2]),
                           down == 4) %>% 
                    group_by(defteam, down) %>% 
                    mutate(Rate = mean(Rate),
                           ConversionRate = mean(ConversionRate),
                           NiceRate = paste0(round(ConversionRate, 2)*100,"%")),
                  aes(y = ConversionRate, label = NiceRate),
                  nudge_y = 0.015, size = 4, fontface = "bold"
        ) 
    })
    
    output$DDownTeamPlot <- renderPlot({
      
      DDownConv %>%
        filter(between(season, 
                       case_when(input$DCDDownTeam == "" ~ input$seasonsDDownTeam[1],
                                 T ~ as.integer(0)),
                       case_when(input$DCDDownTeam == "" ~ input$seasonsDDownTeam[2],
                                 T ~ as.integer(3000))), 
               defteam == case_when(input$DCDDownTeam == "" ~ input$teamDDownTeam,
                                    T ~ defteam),
               DC == case_when(input$DCDDownTeam == "" ~ DC,
                               T ~ input$DCDDownTeam)) %>% 
        ggplot(aes(x = season,
                   y = Rate))+
        geom_col(aes(fill = (down)))+
        geom_image(aes(image = url, y = MaxRate + 0.065))+
        geom_hline(yintercept = 0.67, linetype = "dashed", color = "white",
                   size = 1)+
        geom_hline(yintercept = 0.47, linetype = "dashed", color = "white",
                   size = 1)+
        geom_hline(yintercept = 0.22, linetype = "dashed", color = "white",
                   size = 1)+
        scale_y_continuous(breaks = scales::pretty_breaks(n = 5),
                           labels = scales::percent_format(accuracy = 1),
                           limits = c(0,1))+
        scale_x_continuous(breaks = seq(1999,2030,1))+
        scale_color_identity(aesthetics = "color")+
        labs(
          y= "Converstion Rate",
          x= "Season",
          title= paste0(
            case_when(input$DCDDownTeam == "" ~ input$seasonsDDownTeam[1],
                      T ~ as.integer(
                        min(DDownConv$season[DDownConv$DC == input$DCDDownTeam]))
            ),"-", 
            case_when(input$DCDDownTeam == "" ~ input$seasonsDDownTeam[2],
                      T ~ as.integer(
                        max(DDownConv$season[DDownConv$DC == input$DCDDownTeam]))
            )," ",
            case_when(input$DCDDownTeam == "" ~ input$teamDDownTeam,
                      T ~ input$DCDDownTeam),
            " Defense Conversion Rate by Down"),
          subtitle = "White Dashed lines represent Rough Averages from 2011-2023 for 1st-3rd Down",
          caption = "@SethDataScience"
        ) +
        theme_reach() +
        geom_text(data = DDownConv%>%
                    filter(between(season, 
                                   case_when(input$DCDDownTeam == "" ~ input$seasonsDDownTeam[1],
                                             T ~ as.integer(0)),
                                   case_when(input$DCDDownTeam == "" ~ input$seasonsDDownTeam[2],
                                             T ~ as.integer(3000))), 
                           defteam == case_when(input$DCDDownTeam == "" ~ input$teamDDownTeam,
                                                T ~ defteam),
                           DC == case_when(input$DCDDownTeam == "" ~ DC,
                                           T ~ input$DCDDownTeam), down != 4),
                  aes(y = ConversionRate, label = NiceRate),
                  nudge_y = -0.015, size = 4, fontface = "bold", color = "gray"
        )  +
        geom_text(data = DDownConv%>%
                    filter(between(season, 
                                   case_when(input$DCDDownTeam == "" ~ input$seasonsDDownTeam[1],
                                             T ~ as.integer(0)),
                                   case_when(input$DCDDownTeam == "" ~ input$seasonsDDownTeam[2],
                                             T ~ as.integer(3000))), 
                           defteam == case_when(input$DCDDownTeam == "" ~ input$teamDDownTeam,
                                                T ~ defteam),
                           DC == case_when(input$DCDDownTeam == "" ~ DC,
                                           T ~ input$DCDDownTeam), down == 4),
                  aes(y = ConversionRate, label = NiceRate),
                  nudge_y = 0.015, size = 4, fontface = "bold"
        ) 
    })
    
    
    output$defTTTaPRPlot <- renderPlot({
      
      TTTaPR %>%
        filter(between(season, input$seasonsTTTaPR[1], input$seasonsTTTaPR[2])) %>% 
        group_by(defteam) %>% 
        summarise(Actual = mean(Actual),
                  Expected = mean(Expected),
                  url = head(url, 1)) %>% 
        ggplot(aes(x = Expected, y = Actual))+
        geom_image(aes(image = url))+
        geom_abline(intercept = mean(TTTaPR$Actual, na.rm = T) +
                      mean(TTTaPR$Expected, na.rm = T), slope = -1,
                    linetype = 3,
                    size = 2)+
        geom_hline(yintercept = 30, linetype = 3,
                   color = "blue", size = 2)+
        geom_hline(yintercept = 35, linetype = 3,
                   color = "red", size = 2)+
        scale_color_identity(aesthetics = "color")+
        labs(
          y= "Actual Pressure Rate (%)",
          x= "Expected Pressure Rate (%)",
          title= paste0(
            input$seasonsTTTaPR[1],"-", 
            input$seasonsTTTaPR[2],
            " Actual vs Expected Pressure Rate"),
          subtitle = "Expected Pressure Rate derived from a Linear Regression model that takes Pressured/Non-Pressured Time to Throw as inputs,
    Sloped Line represents if you are Above/Below Expectation, Red Line is 35% and Blue Line is 30% which represent rough high and low thresholds, respectively",
          caption = "@SethDataScience"
        ) +
        theme_reach()
    })
    
    
    output$defTTTaPRTeamPlot <- renderPlot({
      
      TTTaPR %>%
        filter(between(season, 
                       case_when(input$DCTTTaPR == "" ~ input$seasonsTTTaPRTeam[1],
                                 T ~ as.integer(0)),
                       case_when(input$DCTTTaPR == "" ~ input$seasonsTTTaPRTeam[2],
                                 T ~ as.integer(3000))), 
               defteam == case_when(input$DCTTTaPR == "" ~ input$teamTTTaPR,
                                    T ~ defteam),
               DC == case_when(input$DCTTTaPR == "" ~ DC,
                               T ~ input$DCTTTaPR)) %>%
        ggplot(aes(x = Expected, y = Actual))+
        geom_image(aes(image = url))+
        geom_abline(intercept = mean(TTTaPR$Actual, na.rm = T) +
                      mean(TTTaPR$Expected, na.rm = T), slope = -1,
                    linetype = 3,
                    size = 2)+
        geom_hline(yintercept = 30, linetype = 3,
                   color = "blue", size = 2)+
        geom_hline(yintercept = 35, linetype = 3,
                   color = "red", size = 2)+
        scale_color_identity(aesthetics = "color")+
        labs(
          y= "Actual Pressure Rate (%)",
          x= "Expected Pressure Rate (%)",
          title= paste0(
            case_when(input$DCTTTaPR == "" ~ input$seasonsTTTaPRTeam[1],
                      T ~ as.integer(
                        min(TTTaPR$season[TTTaPR$DC == input$DCTTTaPR]))
            ),"-", 
            case_when(input$DCTTTaPR == "" ~ input$seasonsTTTaPRTeam[2],
                      T ~ as.integer(
                        max(TTTaPR$season[TTTaPR$DC == input$DCTTTaPR]))
            )," ",
            case_when(input$DCTTTaPR == "" ~ input$teamTTTaPR,
                      T ~ input$DCTTTaPR),
            " Actual vs Expected Pressure Rate"),
          subtitle = "Expected Pressure Rate derived from a Linear Regression model that takes Pressured/Non-Pressured Time to Throw as inputs,
    Sloped Line represents if you are Above/Below Expectation, Red Line is 35% and Blue Line is 30% which represent rough high and low thresholds, respectively",
          caption = "@SethDataScience"
        ) +
        theme_reach()+
        geom_text(data = TTTaPR %>%
                    filter(between(season, 
                                   case_when(input$DCTTTaPR == "" ~ input$seasonsTTTaPRTeam[1],
                                             T ~ as.integer(0)),
                                   case_when(input$DCTTTaPR == "" ~ input$seasonsTTTaPRTeam[2],
                                             T ~ as.integer(3000))), 
                           defteam == case_when(input$DCTTTaPR == "" ~ input$teamTTTaPR,
                                                T ~ defteam),
                           DC == case_when(input$DCTTTaPR == "" ~ DC,
                                           T ~ input$DCTTTaPR)),
                  aes(label = paste0(substr(season, 3, 4))),
                  nudge_y = -0.02, nudge_x = 0.03,
                  size = 4, fontface = "bold"
        ) 
      
    })
    
    output$defAllowedPlot <- renderPlot({
      
      defpressfill <- defpress %>%
        filter(between(season, 
                       input$seasonsTTTaPRTeam[1],
                       input$seasonsTTTaPRTeam[2]), 
               team_name == input$teamTTTaPR) %>%
        arrange(desc(season)) %>% 
        group_by() %>% 
        summarise(AgapPress = mean(AgapPress, na.rm = T),
                  BgapPress = mean(BgapPress, na.rm = T),
                  OutPress = mean(OutPress, na.rm = T),
                  OverPress = mean(OverPress, na.rm = T),
                  AGapAVG = head(AGapAVG,1),
                  BGapAVG = head(BGapAVG,1),
                  OverAVG = head(OverAVG,1),
                  OutAVG = head(OutAVG,1)
        ) %>% 
        distinct() %>% 
        mutate(BelowAvgA = case_when(AgapPress >= AGapAVG ~ 1,
                                     T~ 0.65),
               BelowAvgB = case_when(BgapPress >= BGapAVG ~ 1,
                                     T~ 0.65),
               BelowAvgO = case_when(OverPress >= OverAVG ~ 1,
                                     T~ 0.65),
               BelowAvgOU = case_when(OutPress >= OutAVG ~ 1,
                                      T~ 0.65)) %>%   
        distinct() 
      
      ggplot(defpressfill,aes())+
        geom_col(aes(x = "0-1 tech/A Gap",
                     y = AgapPress,
                     fill = colors$primary[colors$Code == input$teamTTTaPR],
                     color = colors$secondary[colors$Code == input$teamTTTaPR],
                     alpha = BelowAvgA))+
        
        geom_col(aes(x = "2-3 tech/B Gap", y = BgapPress, fill = colors$primary[colors$Code == input$teamTTTaPR],
                     color = colors$secondary[colors$Code == input$teamTTTaPR],
                     alpha = BelowAvgB))+
        
        
        geom_col(aes(x = "4-5 tech/Over Tackle", y = OverPress, fill = colors$primary[colors$Code == input$teamTTTaPR],
                     color = colors$secondary[colors$Code == input$teamTTTaPR],
                     alpha = BelowAvgO))+
        
        
        geom_col(aes(x = "6-9 tech/Outside Tackle", y = OutPress, fill = colors$primary[colors$Code == input$teamTTTaPR],
                     color = colors$secondary[colors$Code == input$teamTTTaPR],
                     alpha = BelowAvgOU))+
        scale_color_identity(aesthetics = c("color", "fill")) +
        scale_alpha_identity()+
        scale_y_continuous(breaks = scales::pretty_breaks(n = 10),
                           labels = scales::percent_format(accuracy = 1))+
        labs(
          y= "Percent of Pressure Generated",
          x= "Location Generated",
          title = paste0(
            input$seasonsTTTaPRTeam[1], "-",
            input$seasonsTTTaPRTeam[2],
            " ",
            input$teamTTTaPR,
            " Generated Pressure By Location"),
          subtitle = "Does not necessarily sum to 100% as it is a mean of multiple years. Lower Transparency Columns are worse than Positional and Season Average for Pressure Generated",
          caption = "@SethDataScience"
        ) +
        theme_reach() 
      
      
    })
    
    output$GenDefensePlot <- renderPlot({
      
      GenDefense %>% 
        filter(between(season, input$seasonsGenDefense[1], input$seasonsGenDefense[2])) %>%
        group_by(defteam) %>% 
        summarise(url = head(url, 1),
                  primary = head(primary, 1),
                  secondary = head(secondary, 1),
                  Havoc = mean(Havoc, na.rm = T)*100,
                  ExplosiveAllowed = mean(ExplosiveAllowed, na.rm = T)*100,
                  ExplosiveAllowedPass= mean(ExplosiveAllowedPass, na.rm = T)*100,
                  ExplosiveAllowedRush = mean(ExplosiveAllowedRush, na.rm = T)*100,
                  RunStuff = mean(RunStuff, na.rm = T)*100,
                  DADOT = mean(DADOT, na.rm = T),
                  Turnovers = mean(Turnovers, na.rm = T)) %>% 
        ggplot(aes(x = reorder(defteam, -.data[[input$typeGenDefense]]),
                   y = .data[[input$typeGenDefense]]))+
        geom_col(aes(color = secondary, fill = primary))+
        geom_image(aes(image = url))+
        geom_hline(yintercept = GenDefAvgFill$Avg[GenDefAvgFill$Index == input$typeGenDefense],
                   linetype = "dashed",
                   color = "black", size = 1)+
        theme_reach()+
        theme(axis.text.x = element_text(size=12))+
        scale_color_identity(aesthetics = c("color", "fill"))+
        labs(title = paste0(
          input$seasonsGenDefense[1], "-",
          input$seasonsGenDefense[2],
          " ",
          plot_titledef$Title[plot_titledef$Index == input$typeGenDefense]),
          subtitle = plot_titledef$subtitle[plot_titledef$Index == input$typeGenDefense],
          y = plot_titledef$Axis[plot_titledef$Index == input$typeGenDefense],
          x = "Team",
          caption = "@SethDataScience"
        )
    })
    
    
    output$GenDefenseTeamPlot <- renderPlot({
      
      GenDefense %>% 
        filter(between(season, 
                       case_when(input$DCGenDefense == "" ~ input$seasonsGenDefenseTeam[1],
                                 T ~ as.integer(0)),
                       case_when(input$DCGenDefense == "" ~ input$seasonsGenDefenseTeam[2],
                                 T ~ as.integer(3000))), 
               defteam == case_when(input$DCGenDefense == "" ~ input$teamGenDefense,
                                    T ~ defteam),
               DC == case_when(input$DCGenDefense == "" ~ DC,
                               T ~ input$DCGenDefense)) %>%
        mutate(Havoc = Havoc * 100,
               ExplosiveAllowed = ExplosiveAllowed * 100,
               ExplosiveAllowedPass = ExplosiveAllowedPass * 100,
               ExplosiveAllowedRush = ExplosiveAllowedRush * 100,
               RunStuff = RunStuff * 100) %>% 
        ggplot(aes(x = season,
                   y = .data[[input$typeGenDefense]]))+
        geom_col(aes(color = secondary, fill = primary))+
        geom_image(aes(image = url))+
        geom_hline(yintercept = GenDefAvgFill$Avg[GenDefAvgFill$Index == input$typeGenDefense],
                   linetype = "dashed",
                   color = "black", size = 1)+
        theme_reach()+
        theme(axis.text.x = element_text(size=12))+
        scale_color_identity(aesthetics = c("color", "fill"))+
        scale_x_continuous(breaks = seq(1999,2030,1))+
        scale_y_continuous(breaks = scales::pretty_breaks(n = 5))+
        labs(title = paste0(
          case_when(input$DCGenDefense == "" ~ input$seasonsGenDefenseTeam[1],
                    T ~ as.integer(
                      min(GenDefense$season[GenDefense$DC == input$DCGenDefense]))
          ),"-", 
          case_when(input$DCGenDefense == "" ~ input$seasonsGenDefenseTeam[2],
                    T ~ as.integer(
                      max(GenDefense$season[GenDefense$DC == input$DCGenDefense]))
          ),
          " ",
          case_when(input$DCGenDefense == "" ~ input$teamGenDefense,
                    T ~ input$DCGenDefense), " ",
          plot_titledef$Title[plot_titledef$Index == input$typeGenDefense]),
          subtitle = plot_titledef$subtitle[plot_titledef$Index == input$typeGenDefense],
          y = plot_titledef$Axis[plot_titledef$Index == input$typeGenDefense],
          x = "Season",
          caption = "@SethDataScience"
        )
    }) 
}

# Run the application 
shinyApp(ui = ui, server = server)
