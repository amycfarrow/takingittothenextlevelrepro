#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(cowplot)
library(finalfit)
library(here)

ddata <- read_csv("ddata.csv")

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Improving Instructor-Student Relationships by Establishing Similarity"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        
        sidebarPanel(
            selectInput('subset', 
                        "Which subset of students would you like to see results for?",
                        choices = c("All students",
                                    "First-generation college students",
                                    "Hispanic and/or Black students"))
        ),

        # Show a plot of the generated distribution
        mainPanel(
            tabsetPanel(
                tabPanel("Plot", plotOutput("distPlot")), 
                tabPanel("Table", tableOutput("table")), 
                tabPanel("Information", htmlOutput("summary"))
            )
           
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$distPlot <- renderPlot({
        
        bin = 30

        if (input$subset == "All students") {
            data <- ddata
        } else if (input$subset == "First-generation college students") {
            data <- ddata %>%
                filter(s_firstgen == 1)
        } else {
            data <- ddata %>%
                filter(s_hisp_black == 1)
        }
        
        s1_simplot <- data %>% 
            ggplot(aes(x = s1_sim, fill = treatment)) +
            geom_histogram(position = "dodge", bins = bin) +
            labs(x = "Initial student instructor-student similarity rating", y = "Frequency", fill = "Group") +
            scale_x_continuous(limits = c(.8,5.2)) +
            scale_y_continuous(limits = c(0,155)) +
            scale_fill_manual(values = c("#52f2b9", "#868cf7")) +
            theme_minimal()
        
        s1_tsrplot <- data %>% 
            ggplot(aes(x = s1_tsr, fill = treatment)) +
            geom_histogram(position = "dodge", bins = bin) +
            labs(x = "Student anticipated instructor-student relationship rating", y = "Frequency") +
            scale_x_continuous(limits = c(.8,5.2)) +
            scale_y_continuous(limits = c(0,130)) +
            scale_fill_manual(values = c("#52f2b9", "#868cf7")) +
            theme_minimal()
        
        s2_simplot <- data %>% 
            ggplot(aes(x = s2_sim, fill = treatment)) +
            geom_histogram(position = "dodge", bins = bin) +
            labs(x = "End-of-term student instructor-student similarity rating", y = "Frequency") +
            scale_x_continuous(limits = c(.8,5.2)) +
            scale_y_continuous(limits = c(0,100)) +
            scale_fill_manual(values = c("#52f2b9", "#868cf7")) +
            theme_minimal()
        
        s2_tsrplot <- data %>% 
            ggplot(aes(x = s2_tsr, fill = treatment)) +
            geom_histogram(position = "dodge", bins = bin) +
            labs(x = "End-of-term student instructor-student relationship rating", y = "Frequency") +
            scale_x_continuous(limits = c(.8,5.2)) +
            scale_y_continuous(limits = c(0,100)) +
            scale_fill_manual(values = c("#52f2b9", "#868cf7")) +
            theme_minimal()
        
        t2_sim1plot <- data %>% 
            ggplot(aes(x = t2_sim1, fill = treatment)) +
            geom_bar(position = "dodge", width = .2) +
            labs(x = "End-of-term instructor instructor-student similarity rating", y = "Frequency") +
            #scale_x_continuous(limits = c(.8,5.2)) +
            scale_y_continuous(limits = c(0,405)) +
            scale_fill_manual(values = c("#52f2b9", "#868cf7")) +
            theme_minimal()
        
        t2_tsrplot <- data %>% 
            ggplot(aes(x = t2_tsr, fill = treatment)) +
            geom_histogram(position = "dodge", bins = bin) +
            labs(x = "End-of-term instructor instructor-student relationship rating", y = "Frequency") +
            scale_x_continuous(limits = c(.8,5.2)) +
            scale_y_continuous(limits = c(0,150)) +
            scale_fill_manual(values = c("#52f2b9", "#868cf7")) +
            theme_minimal()
        
        gradeplot <- data %>% 
            ggplot(aes(x = grade, fill = treatment)) +
            geom_histogram(position = "dodge", bins = bin) +
            labs(x = "Course grade", y = "Frequency") +
            scale_x_continuous(limits = c(-.1,4.5)) +
            scale_y_continuous(limits = c(0,310)) +
            scale_fill_manual(values = c("#52f2b9", "#868cf7")) +
            theme_minimal()
        
        std_gradeplot <- data %>% 
            ggplot(aes(x = std_grade, fill = treatment)) +
            geom_histogram(position = "dodge", bins = bin) +
            labs(x = "Standardized course grade", y = "Frequency") +
            scale_x_continuous(limits = c(-4.5,3)) +
            scale_y_continuous(limits = c(0,160)) +
            scale_fill_manual(values = c("#52f2b9", "#868cf7")) +
            theme_minimal()
        
        t2_finalexamplot <- data %>%
            filter(obj_exam == 1) %>%
            ggplot(aes(x = t2_finalexam, fill = treatment)) +
            geom_histogram(position = "dodge", bins = bin) +
            labs(x = "Final exam grade", y = "Frequency") +
            scale_x_continuous(limits = c(-.1,4.5)) +
            scale_y_continuous(limits = c(0,60)) +
            scale_fill_manual(values = c("#52f2b9", "#868cf7")) +
            theme_minimal()
        
        f17_enrolledplot <- data %>% 
            ggplot(aes(x = f17_enrolled, fill = treatment)) +
            geom_bar(position = "dodge", width = .2) +
            labs(x = "Re-enrollment the following term", y = "Frequency") +
            scale_y_continuous(limits = c(0,850)) +
            scale_fill_manual(values = c("#52f2b9", "#868cf7")) +
            scale_x_continuous(breaks = c(0,1), labels = c("No", "Yes"), limits = c(-1,2)) +
            theme_minimal()
        
        combined <- plot_grid(s1_simplot + theme(legend.position = "none"), s1_tsrplot + theme(legend.position = "none"),
                              s2_simplot + theme(legend.position = "none"), s2_tsrplot + theme(legend.position = "none"),
                              t2_sim1plot + theme(legend.position = "none"), t2_tsrplot + theme(legend.position = "none"),
                              std_gradeplot + theme(legend.position = "none"), 
                              f17_enrolledplot + theme(legend.position = "none"), 
                              ncol = 2)
        
        legend <- get_legend(s1_simplot + 
                                 guides(color = guide_legend(nrow = 1)) +
                                 theme(legend.position = "bottom"))
        
        plot_grid(combined,
                  legend,
                  ncol = 1,
                  rel_heights = c(4, .2))
        
        
    }, height = 900, width = 700)
    
    output$table <- renderTable({
        
        if (input$subset == "All students") {
            data <- ddata
        } else if (input$subset == "First-generation college students") {
            data <- ddata %>%
                filter(s_firstgen == 1)
        } else {
            data <- ddata %>%
                filter(s_hisp_black == 1)
        }
        
        sumtable <- data %>%
            mutate(t2_sim1 = as.factor(t2_sim1)) %>%
            summary_factorlist("treatment",
                               c("s1_sim", "s1_tsr", "s2_sim", "s2_tsr", "t2_sim1", "t2_tsr",
                                 "grade", "std_grade", "t2_finalexam", "f17_enrolled"),
                               p = TRUE, 
                               add_dependent_label = TRUE, 
                               dependent_label_prefix = "",
                               add_col_totals = TRUE,
                               add_row_totals = TRUE,
                               include_row_missing_col = TRUE,
                               col_totals_rowname = "",
                               total_col = TRUE,
                               col_totals_prefix = "N(%) = ") %>%
            rename(N = "Total N", Missing = "Missing N") 
        
        # Rename variables for readability
        sumtable[,1] <- c("", "Initial student instructor-student similarity rating", 
                          "Initial student instructor-student similarity rating", 
                          "End-of-term student instructor-student similarity rating", 
                          "End-of-term student instructor-student relationship rating",
                          "End-of-term instructor instructor-student similarity rating", "", "", "", "", 
                          "End-of-term instructor instructor-student relationship rating", 
                          "Course grade", 
                          "Standardized course grade", 
                          "Final exam grade",
                          "Re-enrollment the Following Term", "")
        # replace nominal encodings with true values
        sumtable[15,4] <- "No"
        sumtable[16,4] <- "Yes"
        
        # put in table.
        sumtable
        
        
    }, striped = TRUE)
    
    output$summary <- renderText({
        "<b>About the study</b>
        <br>
        This data comes from a Taking It to the Next Level: A Field Experiment to Improve Instructor-Student Relationships in College (Robinson, Scott, and Gottfried 2019). 
        This research tested an intervention to improve college retention and performance. 
        In a field experiment, they evaluated the relationships between perceived similarity, instructor-student relationships, and measures of student success. 
        Based on extensive K-12 research about the importance of instructor-student relationship for student success, Robinson, Scott, and Gottfried (2019) aimed to establish how instructor-student relationships could be improved at the college level, and to test if this improvement had a positive result. 
        The experiment consisted of a randomized controlled trial where some undergraduate students were informed of similarities they shared with their instructor, while others were not. 
        Student and instructor perceptions of similarity and the instructor-student relationship were measured through surveys, and student performance measures were collected from school records.
        <br>
        <b>Study results</b>
        <br>
        The intervention had a weak positive effect on student perceptions of instructor-student similarity, but no effect on student perceptions of instructor-student relationship, instructor perception of similarity or instructor-student relationship, grades, or re-enrollment.
        <br>
        <b>More information</b>
        <br>
        More information about my analysis of Robinson, Scott, and Gottfried's data can be found <a href='https://github.com/amycfarrow/takingittothenextlevelrepro'>on my GitHub page</a>."
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
