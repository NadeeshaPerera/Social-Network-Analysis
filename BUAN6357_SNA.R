######################################################################################################
# Name : Nadeesha Perera
# Date : 02/14/2018
# Topic : BUAN 6357 - Social Network Analysis 
# Purpose : Build a Shiny app - SNA
#####################################################################################################

# Clear the environment
rm(list = ls(all = TRUE))

# Load the required libraries
library(shiny)
library(shinydashboard)
library(data.table)
library(igraph)
library(dplyr)
library(tidyr)
library(networkD3)

#######################################################################
# ui section

ui <- shinyUI(
  dashboardPage(
    dashboardHeader(title = "Nadeesha_Perera_Social_Network_Analysis", titleWidth = 500),
    dashboardSidebar(
      
      # Title for sidebar
      tags$h4("Email Log File"),
      
      # Extra vertical spacing
      tags$br(),
      
      # A space for user to upload the data file
      fileInput("email_file", "Choose file to upload", multiple = FALSE, accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv")),
      
      # A numeric input for user to define the number of connections he wants to view
      numericInput(inputId = "connections", label = "Number of connections to view: ", value = 10, min = 0, max = 25571),
      
      # A button to trigger the table output in the Data Files tab panel with the number of numeric inputs displayed
      actionButton(inputId = "update", label = "Update!"),
      
      # Vertical spacing to seperate the sections
      tags$br(),
      tags$br(),
      
      # Horizontal line to seperate the sections
      tags$hr(),
      
      # Vertical spacing to seperate the sections      
      tags$br(),
      
      # Title for second section
      tags$h4("Department File"),
      
      #Extra vertical spacing
      tags$br(),
      
      # A space for user to upload the data file
      fileInput("dept_file", "Choose file to upload", multiple = FALSE, accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv"))
      
      
    ),
    
    dashboardBody(
      # Output : Tabset with email summary, department summary
      tabsetPanel( type = "tabs",
                   
                   # Panel 1 - Data Files
                   tabPanel("Data files",
                            tags$br(),
                            tags$h4("Email Log Data File"),
                            tags$br(),
                            dataTableOutput("email_content"), 
                            tags$br(),
                            tags$hr(),
                            tags$br(),
                            tags$h4("Department Data File"),
                            tags$br(),
                            dataTableOutput("dept_content")),
                   
                   # Panel 2 - Summary of emails sent
                   tabPanel("Emails sent", 
                            tags$br(),
                            tags$h4("Summary of emails sent by each employee"),
                            tags$br(),
                            dataTableOutput("summary_sent")),
                   
                   # Panel 3 - Summary of emails received
                   tabPanel("Emails received",
                            tags$br(),
                            tags$h4("Summary of emails received by each employee"),
                            tags$br(),
                            dataTableOutput("summary_got")),
                   
                   # Panel 4 - 2 hop neighbors of top 10 emails sent employees
                   tabPanel("2-hop: Sent",
                            tags$br(),
                            tags$h4("2-hop  neighbors of the 10 employees sending most email"),
                            tags$br(),
                            tags$h5("The top 10 senders of emails are:"),
                            tags$br(),
                            tableOutput("sender_id"),
                            tags$br(),
                            tags$h4("Please select for which employee to see the 2-hop neighbors of"),
                            tags$br(),
                            textInput(inputId = "Emp_num", label = "Enter Employee Id and click submit", value = ""),
                            # A button to trigger the table output in the Data Files tab panel with the number of numeric inputs displayed
                            actionButton(inputId = "Display_sent", label = "Submit"),
                            tags$br(),
                            tags$br(),
                            dataTableOutput("hop2_sent")),
                   
                   # Panel 5 - 2 hop neighbors of top 10 emails received employees
                   tabPanel("2-hop: Received",
                            tags$br(),
                            tags$h4("2-hop  neighbors of the 10 employees receiving most email"),
                            tags$br(),
                            tags$h5("The top 10 receivers of emails are:"),
                            tags$br(),
                            tableOutput("receiver_id"),
                            tags$br(),
                            tags$h4("Please select for which employee to see the 2-hop neighbors of"),
                            tags$br(),
                            textInput(inputId = "Emp_num_rec", label = "Enter Employee Id and click submit", value = ""),
                            # A button to trigger the table output in the Data Files tab panel with the number of numeric inputs displayed
                            actionButton(inputId = "Display_received", label = "Submit"),
                            tags$br(),
                            tags$br(),
                            dataTableOutput("hop2_received")),
                   
                   # Panel 6 - Degree centrality considering as undirected
                   tabPanel("Degree centrality",
                            tags$br(),
                            tags$h4("The degree centrality of the employees considering as undirected network"),
                            #tags$br(),
                            #dataTableOutput("deg_cent_un"),
                            #tags$br(),
                            tags$br(),
                            tags$h5("The 10 employees with highest degree centrality are:"),
                            tags$br(),
                            tableOutput("deg_cent_un_10"),
                            tags$br(),
                            tags$h4("The network diagram - depicting upto 2-hop neighbors of the above employees"),
                            tags$br(),
                            forceNetworkOutput("deg_cent_un_plot")
                   ),
                   
                   # Panel 7 - Betweenness centrality considering as undirected
                   tabPanel("Betweenness centrality",
                            tags$br(),
                            tags$h4("The betweenness centrality of the employees considering as undirected network"),
                            #tags$br(),
                            #dataTableOutput("bet_cent_un"),
                            #tags$br(),
                            tags$br(),
                            tags$h5("The 10 employees with highest betweenness centrality are:"),
                            tags$br(),
                            tableOutput("bet_cent_un_10"),
                            tags$br(),
                            tags$h4("The network diagram - depicting upto 2-hop neighbors of the above employees"),
                            tags$br(),
                            forceNetworkOutput("bet_cent_un_plot")
                   ),
                   
                   
                   # Panel 8 - In-degree centrality
                   tabPanel("In-degree centrality",
                            tags$br(),
                            tags$h4("The in-degree centrality of network"),
                            #tags$br(),
                            #dataTableOutput("indeg_cent_un"),
                            #tags$br(),
                            tags$br(),
                            tags$h5("The 10 employees with highest in-degree centrality are:"),
                            tags$br(),
                            tableOutput("in_cent_un_10"),
                            tags$br(),
                            tags$h4("The network diagram - depicting upto 2-hop neighbors of the above employees"),
                            tags$br(),
                            forceNetworkOutput("in_cent_un_plot")
                   ),
                   
                   # Panel 9 - Department level summary
                   tabPanel("Department summary",
                            tags$br(),
                            tags$h4("The department level summary of network"),
                            tags$br(),
                            tags$br(),
                            dataTableOutput("dept_table"),
                            tags$br(),
                            tags$h4("The network diagram"),
                            tags$br(),
                            forceNetworkOutput("dept_plot")
                   ),
                   
                   
                   # Panel 10 - Discussion
                   tabPanel("Discussion",
                            tags$br(),
                            tags$h4("Observations in comparing visualizations of degree centrality, betweenness centrality and in-degree centrality"),
                            tags$br(),
                            tags$br(),
                            tags$h5("When looking upto the 2-hop level of the top 10 employees in terms of degree centrality,
                                    betweenness centrality and in-degree centrality it can be easily concluded that these employees
                                    are central figues in the organization."),
                            tags$br(),
                            tags$h5("These employees reach almost all other employees with in 2 hops: 949, 955 and 947 out of the 1005 employees in the cases of
                                    degree centrality, betweenness centrality and in-degree centrality respectively. Hence information
                                    can be disseminated with in the organization fast. Further, this implies that the organization has a flat structure"),
                            tags$br(),
                            tags$h5("The employees 160, 86, 121, 107 and 62 are in the top 10 with respect to all 3 measures showing
                                    that they are central figures in the organization."),
                            tags$br(),
                            tags$h5("The employees 121, 434, 183, 64 and 128 have high in degree centrality but not in the top 10 of the
                                    other 2 measures. Hence, they can be considered as prominent figures in the organization who other employees connect with."),
                            tags$br(),
                            tags$h5("The employees 377 and 211 are in the top 10 in terms of betweenness but not the other 2 measures.
                                    Hence,they can be seen as employees that bridge nodes that are not connected to each other.
                                    They are probably in the higher levels of the hierarchy."),
                            tags$br(),
                            tags$h5("In summary, though this is a network of 1005 employees in 42 departments, it is a highly
                                    connected organization that can be reached across in a few hops.")  
                            
                            
                            )
                   
                   
                   
                   
                            )
    
      
    )
    
  )
  
)


server <- shinyServer(function(input, output){
  
  # Wait for the user to upload the data file, input the number of rows to view and click update
  observeEvent(input$update, {
    output$email_content <- renderDataTable({
      req(input$email_file)
      df1 <- fread(input$email_file$datapath, header = "auto", col.names = c("Email_From", "Email_To"))
    },
    
    options = list(pageLength = input$connections)
    #    rownames = FALSE,
    #    class = 'cell-border stripe'
    
    )
  }
  )
  
  # Wait for user to upload the department data file     
  output$dept_content <- renderDataTable({
    req(input$dept_file)
    df2 <- fread(input$dept_file$datapath, header = "auto", col.names = c("Employee", "Department"))
  },
  options = list(pageLength = 10)
  #rownames = FALSE,
  #class = 'cell-border stripe'
  )
  
  # The summary of emails sent
  # Use the email log file uploaded by the user 
  output$summary_sent <- renderDataTable({
    req(input$email_file)
    df3 <- fread(input$email_file$datapath, header = "auto", col.names = c("Email_From", "Email_To"))
    # Extract the 1st column of the user input file to a variable in a table format and make a table out of it
    summary_sent <- data.table(table(df3[,1]))
    # Define the column names
    setnames(summary_sent, c("V1", "N"), c("Employee", "Number of emails sent"))
    # Return the values to output
    return(summary_sent)
  },
  options = list(pageLength = 10)
  #    rownames = FALSE,
  #   class = 'cell-border stripe'
  )
  
  # The summary of emails received  
  # Use the email log file uploaded by the user 
  output$summary_got <- renderDataTable({
    req(input$email_file)
    df4 <- fread(input$email_file$datapath, header = "auto", col.names = c("Email_From", "Email_To"))
    # Extract the 2nd column of the user input file to a variable in a table format and make a table out of it
    summary_got <- data.table(table(df4[,2]))
    # Define the column names
    setnames(summary_got, c("V1", "N"), c("Employee", "Number of emails received"))
    # Return the values to output
    return(summary_got)
  },
  options = list(pageLength = 10)
  #  rownames = FALSE,
  #  class = 'cell-border stripe'
  )
  
  # The list of 2-hop neighbors of the top 10 employees in terms of number of emails sent  
  # Use the email log file uploaded by the user
  
  output$sender_id <- renderTable({    
    req(input$email_file)
    df6 <- fread(input$email_file$datapath, header = "auto", col.names = c("Email_From", "Email_To"))
    # Extract the 1st column of the user input file to a variable in a table format and make a table out of it
    df6_sent <- data.table(table(df6[,1]))
    # sort in decending of the number of emails sent
    df6_sent <- df6_sent[order(-N)]
    
    # Define the column names
    setnames(df6_sent, c("V1", "N"), c("Employee", "Number of emails sent"))
    
    
    sender_id <- df6_sent[1:10,]
    
    
    # Return the values to output
    return(sender_id)
    
  })  
  
  
  observeEvent(input$Display_sent, {
    output$hop2_sent <- renderDataTable({
      req(input$email_file)
      df5 <- fread(input$email_file$datapath, header = "auto", col.names = c("Email_From", "Email_To"))
      
      # Formatting the Data to Create an iGraph Style Edge List and Graph
      df5_1 <- graph.data.frame(df5, directed = TRUE)
      
      # Get the adjacency matrix
      df5_matrix <- as_adjacency_matrix(df5_1)
      
      
      # Get the 2 hop neighbors
      df5_2hop <- df5_matrix %*% df5_matrix
      
      # convert back to data tables for ease of access
      
      df5_matrix_df <- as.matrix(df5_matrix)
      df5_2hop_df <- as.matrix(df5_2hop)
      
      
      #################################################################################
      # Display 2 hop neighbor of a input z where z is the sender of emails
      
      d1_dt <- data.table(email_from = V(df5_1)$name)
      d <- data.table()
      
      h <- data.table()
      
      
      neighbors_out <- function(z){
        # Find the row number where the employee number is in the data table
        i <- which(d1_dt$email_from == z)
        # Define a data table
        g <- data.table()
        # Define a data table d with 1st column as the employee number
        d <- data.table(vertex = d1_dt)
        # To data table d add the columns of the 2 hop and 1 hop neighbors from the respective data frames 
        d <- cbind(d, hop2 = df5_2hop_df[i,], hop1 = df5_matrix_df[i,])
        # Define a variable NA
        f <- NA
        # Run a loop throughout data table d to check for a particular emploee sending emails, if the other employee is a one hop and 2 hop neighbor. If it is, assign that employee number to f else assign NA
        for(j in 1:nrow(d)){
          if(d[j,2] > 0 && d[j,3] == 0){
            f <-  d[j,1]
          }
          
          else
          {
            f <- NA
          }
          # If F is not NA, the 2 hop neighbor is added to the data table
          if(!is.na(f)){
            if(j == 1){
              g <- f
            }
            
            else{
              g <- rbind(g, f, use.names = FALSE, idcol = FALSE)
            }
          }
          
          j = j+1
        }
        
        return(g) 
      }
      
      ############################################################
      
      # Use the employee id selected to get the table of 2 hop neighbors
      hop2_sent <- neighbors_out(input$Emp_num)
      setnames(hop2_sent, c("vertex.email_from"), c("List of 2 hop neighbors"))
      
      # Return the values to output
      return(hop2_sent)
    },
    options = list(pageLength = 10)
    #  rownames = FALSE,
    #  class = 'cell-border stripe'
    )
  })
  
  
  output$receiver_id <- renderTable({    
    req(input$email_file)
    df7 <- fread(input$email_file$datapath, header = "auto", col.names = c("Email_From", "Email_To"))
    # Extract the 2nd column of the user input file to a variable in a table format and make a table out of it
    df7_received <- data.table(table(df7[,2]))
    # sort in decending of the number of emails received
    df7_received <- df7_received[order(-N)]
    
    # Define the column names
    setnames(df7_received, c("V1", "N"), c("Employee", "Number of emails received"))
    
    
    receiver_id <- df7_received[1:10,]
    
    
    # Return the values to output
    return(receiver_id)
    
  })  
  
  
  observeEvent(input$Display_received, {
    output$hop2_received <- renderDataTable({
      req(input$email_file)
      df8 <- fread(input$email_file$datapath, header = "auto", col.names = c("Email_From", "Email_To"))
      
      # Formatting the Data to Create an iGraph Style Edge List and Graph
      df8_1 <- graph.data.frame(df8, directed = TRUE)
      
      # Get the adjacency matrix
      df8_matrix <- as_adjacency_matrix(df8_1)
      
      
      # Get the 2 hop neighbors
      df8_2hop <- df8_matrix %*% df8_matrix
      
      # convert back to data tables for ease of access
      
      df8_matrix_df <- as.matrix(df8_matrix)
      df8_2hop_df <- as.matrix(df8_2hop)
      
      
      #################################################################################
      # Display 2 hop neighbor of a input z where z is the sender of emails
      
      d1_dt <- data.table(email_receiver = V(df8_1)$name)
      d <- data.table()
      
      h <- data.table()
      
      
      neighbors_out <- function(z){
        # Find the column number where the employee number is in the data table
        i <- which(d1_dt$email_receiver == z)
        # Define a data table
        g <- data.table()
        # Define a data table d with 1st column as the employee number
        d <- data.table(vertex = d1_dt)
        # To data table d add the columns of the 2 hop and 1 hop neighbors from the respective data frames 
        d <- cbind(d, hop2 = df8_2hop_df[i,], hop1 = df8_matrix_df[i,])
        # Define a variable NA
        f <- NA
        # Run a loop throughout data table d to check for a particular emploee sending emails, if the other employee is a one hop and 2 hop neighbor. If it is, assign that employee number to f else assign NA
        for(j in 1:nrow(d)){
          if(d[j,2] > 0 && d[j,3] == 0){
            f <-  d[j,1]
          }
          
          else
          {
            f <- NA
          }
          # If F is not NA, the 2 hop neighbor is added to the data table
          if(!is.na(f)){
            if(j == 1){
              g <- f
            }
            
            else{
              g <- rbind(g, f, use.names = FALSE, idcol = FALSE)
            }
          }
          
          j = j+1
        }
        
        return(g) 
      }
      
      ############################################################
      
      # Use the employee id selected to get the table of 2 hop neighbors
      hop2_received <- neighbors_out(input$Emp_num_rec)
      setnames(hop2_received, c("vertex.email_receiver"), c("List of 2 hop neighbors"))
      
      # Return the values to output
      return(hop2_received)
    },
    options = list(pageLength = 10)
    #  rownames = FALSE,
    #  class = 'cell-border stripe'
    )
  })
  
  
  # output$deg_cent_un <- renderDataTable({
  #   req(input$email_file)
  #   df9 <- fread(input$email_file$datapath, header = "auto", col.names = c("Email_From", "Email_To"))
  #   # Create i graph style edgelist
  #   df9_Edgelist <- df9
  #   # Create graph object
  #   df9_graph <- graph.data.frame(df9_Edgelist, directed = FALSE)
  #   # calculate degree centrality
  #   V(df9_graph)$degree <- centr_degree(df9_graph, mode = "all", loops = TRUE, normalized = FALSE)$res
  #   # take vertex number and degree to a data frame
  #   deg_cent_un <- get.data.frame(df9_graph, what = "vertices")
  #   # Return the values to output
  #   return(deg_cent_un)
  # },
  # options = list(pageLength = 10)
  # #    rownames = FALSE,
  # #   class = 'cell-border stripe'
  # )
  # 
  
  output$deg_cent_un_10 <- renderTable({    
    req(input$email_file)
    df10 <- fread(input$email_file$datapath, header = "auto", col.names = c("Email_From", "Email_To"))
    # Create i graph style edgelist
    df10_Edgelist <- df10
    # Create graph object
    df10_graph <- graph.data.frame(df10_Edgelist, directed = FALSE)
    # calculate degree centrality
    V(df10_graph)$degree <- centr_degree(df10_graph, mode = "all", loops = TRUE, normalized = FALSE)$res
    # take vertex number and degree to a data frame
    deg_cent_un_10_a <- get.data.frame(df10_graph, what = "vertices")
    
    # sort in decending of the degree
    deg_cent_un_10 <- deg_cent_un_10_a[order(-deg_cent_un_10_a$degree),]
    deg_cent_un_10 <- deg_cent_un_10[1:10,]
    
    
    # Return the values to output
    return(deg_cent_un_10)
    
  })  
  
  
  output$deg_cent_un_plot <- renderForceNetwork({
    req(input$email_file)
    df11 <- fread(input$email_file$datapath, header = "auto", col.names = c("Email_From", "Email_To"))
    
    req(input$dept_file)
    df12 <- fread(input$dept_file$datapath, header = "auto", col.names = c("Employee", "Department"))
    
    # Create i graph style edgelist
    df11_Edgelist <- df11
    # Create graph object
    df11_graph <- graph.data.frame(df11_Edgelist, directed = FALSE)
    # calculate degree centrality
    V(df11_graph)$degree <- centr_degree(df11_graph, mode = "all", loops = TRUE, normalized = FALSE)$res
    
    # take vertex number and degree to a data frame
    deg_cent_un_11_a <- get.data.frame(df11_graph, what = "vertices")
    
    # sort in decending of the degree
    deg_cent_un_11 <- deg_cent_un_11_a[order(-deg_cent_un_11_a$degree),]
    deg_cent_un_11 <- deg_cent_un_11[1:10,1]
    
    # From the main email log, select the rows where these 10 are the senders
    df11_senders <- df11[which(df11$Email_From %in% deg_cent_un_11),]
    # Take a list of the unique employee IDs
    df11_1hop <- unique(df11_senders$Email_To)
    # From the main email log, select the rows where these df11 senders are the senders
    df11_2hops <- df11[which(df11$Email_From %in% df11_1hop),]
    # create combined data frame
    df11_combined <- rbind(df11_senders, df11_2hops)
    
    # Prepare data table to be merged
    deg_cent_un_11_a$name <- as.integer(deg_cent_un_11_a$name)
    # Include degree to df19
    df12_b <- left_join(df12, deg_cent_un_11_a, by = c("Employee" = "name"))
    df12_b$degree <- df12_b$degree/150
    
    
    # create graph object
    
    df11_comb_graph <- forceNetwork(Links = df11_combined,
                                    Nodes = df12_b,
                                    Source = "Email_From", Target = "Email_To",
                                    #Value = "value",
                                    NodeID = "Employee",
                                    Group = "Department",
                                    Nodesize = "degree",
                                    width = 800,
                                    height = 800,
                                    fontSize = 25,
                                    zoom = TRUE,
                                    opacity = 0.8)
    
    
    
    # Return the values to output
    return(df11_comb_graph)
    
  })
  
  
  # output$bet_cent_un <- renderDataTable({
  #   req(input$email_file)
  #   df14 <- fread(input$email_file$datapath, header = "auto", col.names = c("Email_From", "Email_To"))
  #   # Create i graph style edgelist
  #   df14_Edgelist <- df14
  #   # Create graph object
  #   df14_graph <- graph.data.frame(df14_Edgelist, directed = FALSE)
  #   # calculate degree centrality
  #   V(df14_graph)$between <- betweenness(df14_graph, v = V(df14_graph), directed = FALSE, normalized = FALSE)
  #   # take vertex number and degree to a data frame
  #   bet_cent_un <- get.data.frame(df14_graph, what = "vertices")
  #   # Return the values to output
  #   return(bet_cent_un)
  # },
  # options = list(pageLength = 10)
  # #    rownames = FALSE,
  # #   class = 'cell-border stripe'
  # )
  # 
  
  output$bet_cent_un_10 <- renderTable({    
    req(input$email_file)
    df15 <- fread(input$email_file$datapath, header = "auto", col.names = c("Email_From", "Email_To"))
    # Create i graph style edgelist
    df15_Edgelist <- df15
    # Create graph object
    df15_graph <- graph.data.frame(df15_Edgelist, directed = FALSE)
    # calculate betweenness centrality
    V(df15_graph)$between <- betweenness(df15_graph, v = V(df15_graph), directed = FALSE, normalized = FALSE)
    # take vertex number and degree to a data frame
    bet_cent_un_10_a <- get.data.frame(df15_graph, what = "vertices")
    
    # sort in decending of the degree
    bet_cent_un_10 <- bet_cent_un_10_a[order(-bet_cent_un_10_a$bet),]
    bet_cent_un_10 <- bet_cent_un_10[1:10,]
    
    
    # Return the values to output
    return(bet_cent_un_10)
    
  })  
  
  
  output$bet_cent_un_plot <- renderForceNetwork({
    req(input$email_file)
    df16 <- fread(input$email_file$datapath, header = "auto", col.names = c("Email_From", "Email_To"))
    
    req(input$dept_file)
    df17 <- fread(input$dept_file$datapath, header = "auto", col.names = c("Employee", "Department"))
    
    # Create i graph style edgelist
    df16_Edgelist <- df16
    # Create graph object
    df16_graph <- graph.data.frame(df16_Edgelist, directed = FALSE)
    # calculate degree centrality
    V(df16_graph)$between <- betweenness(df16_graph, v = V(df16_graph), directed = FALSE, normalized = FALSE)
    
    # take vertex number and degree to a data frame
    bet_cent_un_11_a <- get.data.frame(df16_graph, what = "vertices")
    
    # sort in decending of the degree
    bet_cent_un_11 <- bet_cent_un_11_a[order(-bet_cent_un_11_a$between),]
    bet_cent_un_11 <- bet_cent_un_11[1:10,1]
    
    # From the main email log, select the rows where these 10 are the senders
    df16_senders <- df16[which(df16$Email_From %in% bet_cent_un_11),]
    # Take a list of unique employee ID's
    df16_1hop <- unique(df16_senders$Email_To)
    # From the main email log, select the rows where df16_1hop are the senders
    df16_2hops <- df16[which(df16$Email_From %in% df16_1hop),]
    # create combined data frame
    df16_combined <- rbind(df16_senders, df16_2hops)
    
    # Prepare this data frame to be be joined to department table
    bet_cent_un_11_a$name <- as.integer(bet_cent_un_11_a$name)
    # Include degree to df19
    df17_b <- left_join(df17, bet_cent_un_11_a, by = c("Employee" = "name"))
    df17_b$between <- df17_b$between/10000
    
    # create graph object
    
    df16_comb_graph <- forceNetwork(Links = df16_combined,
                                    Nodes = df17_b,
                                    Source = "Email_From", Target = "Email_To",
                                    #Value = "value",
                                    NodeID = "Employee",
                                    Group = "Department",
                                    Nodesize = "between",
                                    height = 800,
                                    width = 800,
                                    fontSize = 25,
                                    zoom = TRUE,
                                    opacity = 0.8)
    
    
    # Return the values to output
    return(df16_comb_graph)
    
  })
  
  
  # output$indeg_cent_un <- renderDataTable({
  #   req(input$email_file)
  #   df17_a <- fread(input$email_file$datapath, header = "auto", col.names = c("Email_From", "Email_To"))
  #   # Create i graph style edgelist
  #   df17_a_Edgelist <- df17_a
  #   # Create graph object
  #   df17_a_graph <- graph.data.frame(df17_a_Edgelist, directed = TRUE)
  #   # calculate degree centrality
  #   V(df17_a_graph)$degree <- centr_degree(df17_a_graph, mode = "in", loops = TRUE, normalized = FALSE)$res
  #   # take vertex number and degree to a data frame
  #   indeg_cent_un <- get.data.frame(df17_a_graph, what = "vertices")
  #   # Return the values to output
  #   return(indeg_cent_un)
  # },
  # options = list(pageLength = 10)
  # #    rownames = FALSE,
  # #   class = 'cell-border stripe'
  # )
  
  
  output$in_cent_un_10 <- renderTable({    
    req(input$email_file)
    df17 <- fread(input$email_file$datapath, header = "auto", col.names = c("Email_From", "Email_To"))
    # Create i graph style edgelist
    df17_Edgelist <- df17
    # Create graph object
    df17_graph <- graph.data.frame(df17_Edgelist, directed = TRUE)
    # calculate degree centrality
    V(df17_graph)$degree <- centr_degree(df17_graph, mode = "in", loops = TRUE, normalized = TRUE)$res
    # take vertex number and degree to a data frame
    in_cent_un_10_a <- get.data.frame(df17_graph, what = "vertices")
    
    # sort in decending of the degree
    in_cent_un_10 <- in_cent_un_10_a[order(-in_cent_un_10_a$degree),]
    in_cent_un_10 <- in_cent_un_10[1:10,]
    
    
    # Return the values to output
    return(in_cent_un_10)
    
  })  
  
  
  output$in_cent_un_plot <- renderForceNetwork({
    req(input$email_file)
    df18 <- fread(input$email_file$datapath, header = "auto", col.names = c("Email_From", "Email_To"))
    
    req(input$dept_file)
    df19 <- fread(input$dept_file$datapath, header = "auto", col.names = c("Employee", "Department"))
    
    # Create i graph style edgelist
    df18_Edgelist <- df18
    # Create graph object
    df18_graph <- graph.data.frame(df18_Edgelist, directed = TRUE)
    # calculate degree centrality
    V(df18_graph)$degree <- centr_degree(df18_graph, mode = "in", loops = TRUE, normalized = TRUE)$res
    
    # take vertex number and degree to a data frame
    in_cent_un_11_a <- get.data.frame(df18_graph, what = "vertices")
    
    # sort in decending of the degree
    in_cent_un_11 <- in_cent_un_11_a[order(-in_cent_un_11_a$degree),]
    in_cent_un_11 <- in_cent_un_11[1:10,1]
    
    # From the main email log, select the rows where these 10 are the senders
    df18_senders <- df18[which(df18$Email_From %in% in_cent_un_11),]
    # Get the unique 1 hop neighbors
    df18_1hop <- unique(df18_senders$Email_To)
    # From the main email log, select the rows where these senders are the senders
    df18_2hops <- df18[which(df18$Email_From %in% df18_1hop),]
    # create combined data frame
    df18_combined <- rbind(df18_senders, df18_2hops)
    
    # Prepare data frame to merge
    in_cent_un_11_a$name <- as.integer(in_cent_un_11_a$name)
    # Include degree to df19
    df19_b <- left_join(df19, in_cent_un_11_a, by = c("Employee" = "name"))
    df19_b$degree <- df19_b$degree/150
    
    # create graph object
    
    df18_comb_graph <- forceNetwork(Links = df18_combined,
                                    Nodes = df19_b,
                                    Source = "Email_From", Target = "Email_To",
                                    #Value = "value",
                                    NodeID = "Employee",
                                    Group = "Department",
                                    Nodesize = "degree",
                                    height = 800,
                                    width = 800,
                                    fontSize = 25,
                                    zoom = TRUE,
                                    opacity = 0.8)
    
    
    # Return the values to output
    return(df18_comb_graph)
    
  })
  
  
  output$dept_table <- renderDataTable({
    req(input$email_file)
    df20 <- fread(input$email_file$datapath, header = "auto", col.names = c("Email_From", "Email_To"))
    
    req(input$dept_file)
    df21 <- fread(input$dept_file$datapath, header = "auto", col.names = c("Employee", "Department"))
    
    # Combine the data frames
    df22 <- left_join(df20,df21, by = c("Email_From" = "Employee"))
    df22 <- left_join(df22, df21, by = c("Email_To" = "Employee"))
    
    # add a column to concatnate dept originating and receiving
    df22$combined <- paste(df22$Department.x, "-", df22$Department.y)
    
    # Create a table using this column
    df23 <- data.table(table(df22[,5]))
    
    # Split the 
    dept_table <- separate(data = df23, col = V1, into = c("Dept_sent", "Dept_rcvd"), sep = "-", remove = TRUE)
    setnames(dept_table, c("Dept_sent", "Dept_rcvd", "N"), c("Department_sent", "Department_received" ,"Number_of_emails"))
    # Return the values to output
    return(dept_table)
  },
  options = list(pageLength = 10)
  #    rownames = FALSE,
  #   class = 'cell-border stripe'
  )
  
  
  output$dept_plot <- renderForceNetwork({
    req(input$email_file)
    df24 <- fread(input$email_file$datapath, header = "auto", col.names = c("Email_From", "Email_To"))
    
    req(input$dept_file)
    df25 <- fread(input$dept_file$datapath, header = "auto", col.names = c("Employee", "Department"))
    
    # Combine the data frames
    df26 <- left_join(df24,df25, by = c("Email_From" = "Employee"))
    df26 <- left_join(df26, df25, by = c("Email_To" = "Employee"))
    
    # add a column to concatnate dept originating and receiving
    df26$combined <- paste(df26$Department.x, "-", df26$Department.y)
    
    # Create a table using this column
    df27 <- data.table(table(df26[,"combined"]))
    
    # Split the 
    df28 <- separate(data = df27, col = V1, into = c("Dept_sent", "Dept_rcvd"), sep = "-", remove = TRUE)
    
    # Create i graph style edgelist
    df28_Edgelist <- df28
    # Create graph object
    df28_graph <- simplify(graph.data.frame(df28_Edgelist, directed = TRUE)) 
    # Create nodelist
    df28_nodes <- data.frame(ID = c(0:(igraph::vcount(df28_graph) - 1)), nName = igraph::V(df28_graph)$name)
    
    
    # Calculate some node properties and node similarities that will be used to illustrate 
    # different plotting abilities and add them to the edge and node lists
    
    # Calculate degree for all nodes
    df28_nodes <- cbind(df28_nodes, nodeDegree=igraph::degree(df28_graph, v = igraph::V(df28_graph), mode = "all"))
    
    # Calculate betweenness for all nodes
    betAll <- igraph::betweenness(df28_graph, v = igraph::V(df28_graph), directed = FALSE) / (((igraph::vcount(df28_graph) - 1) * (igraph::vcount(df28_graph)-2)) / 2)
    betAll.norm <- (betAll - min(betAll))/(max(betAll) - min(betAll))
    df28_nodes <- cbind(df28_nodes, nodeBetweenness=10*betAll.norm) # We are scaling the value by multiplying it by 100 for visualization purposes only (to create larger nodes)
    rm(betAll, betAll.norm)
    
    # Convert to integer for plotting
    df28$Dept_sent <- as.integer(df28$Dept_sent)
    df28$Dept_rcvd <- as.integer(df28$Dept_rcvd)
    df28_nodes$nName <- as.integer(df28_nodes$nName)
    

    # create graph object

    
    dept_plot <- forceNetwork(Links = df28,
                 Nodes = df28_nodes,
                 Source = "Dept_sent",
                 Target = "Dept_rcvd",
                 Value = "N",
                 NodeID = "nName",
                 Group = "nodeDegree",
                 Nodesize = "nodeBetweenness",
                 height = 800,
                 width = 800,
                 fontSize = 12,
                 zoom = TRUE,
                 opacity = 0.8)


    
    # Return the values to output
    return(dept_plot)
    
  })
  
  
  
  
}
  
)


shinyApp(ui = ui, server = server)






