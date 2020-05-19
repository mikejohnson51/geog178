library(shiny)
library(DT)
library(dplyr)
library(readxl)
library(shinythemes)
library(ggplot2)

xlsx.path  = #'./inst/geog178_gradebook_2020/data/grades.xlsx'
'./data/grades.xlsx'
sheets = c(paste0("lab", 1:6), "summary")
labs = list()
for(i in 1:length(sheets)){
    labs[[i]] = read_xlsx(xlsx.path, sheet = sheets[i])
}


process = function(lab, perm){
    
    xx = labs[[as.numeric(lab)]]
    d = data.frame(t(dplyr::filter(xx, Perm %in% c("total", perm))), stringsAsFactors = FALSE)
    if(ncol(d) == 2){
        names(d) = c("Maximum", d[1,2])
        d = d[!rownames(d) %in% c('Name', "Comment", "Perm"),]
        question  = rownames(d)
        comment = d[rownames(d) == 'Comment', 2 ]
        
        if(length(comment) == 0) { comment = NA}
        if(is.na(comment) | length(comment) == 0){ comment = ""}
        
        d = d[, c(2,1)]
        d = mutate_all(d, as.numeric)
        d = mutate_all(d, round, 2)
        
        rownames(d) = question
    } else {
        d = NULL
    }
    
    return(list(d = d, comment = comment, question = question))
}

# Define UI for application that draws a histogram
ui <- fluidPage(
    theme = shinytheme("cerulean"),

    titlePanel(h1("Geography 178/258 Grades (Winter 2020)", 
                  style='background-color:#003660;color:#FEBC11;padding-left: 10px;padding-bottom: 10px;padding-top: 10px;')),

    sidebarLayout(
        sidebarPanel(
            textInput("perm", "Enter PERM Number", value = "class"),
            selectInput('lab', "Pick Lab", 
                        choices = list("Lab 1" = 1, 
                                       "Lab 2" = 2,
                                       "Lab 3" = 3, 
                                       "Lab 4" = 4,
                                       "Lab 5" = 5, 
                                       "Lab 6" = 6,
                                       "Summary" = 7))
        ),
        mainPanel( DTOutput("table"),
                   plotOutput("plot"))
    )
)

server <- function(input, output) {

    output$plot = renderPlot({
        
        d  = process(input$lab, input$perm)
        c2 =  process(input$lab, "class")
        
    ggplot() + 
        geom_col(data = c2$d, aes(x = rownames(c2$d), y = c2$d[,1]), fill = 'gray90') +
        geom_line(data = d$d, aes(x = rownames(c2$d), 
                                y = d$d[,1], 
                                group = 1), 
                  color = "red", lwd = 2 ) + 
        theme_minimal() + 
        scale_x_discrete(guide = guide_axis(n.dodge=3))+
        labs(title = "Your Lab Score vs Class Average",
             x = '', 
             y = "Points") + 
        ylim(c(0,10))
    })
    
    output$table <- renderDT({
        d  = process(input$lab, input$perm)
   
        datatable(d$d, caption = d$comment , 
                  options = list(paging = FALSE, 
                                 searching = FALSE,
                                 ordering  = FALSE, 
                                 dom = 't'
                            )) 

    })
}

# Run the application 
shinyApp(ui = ui, server = server)
