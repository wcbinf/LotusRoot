#
# This is my laboratory information management system (LIMS)
# Shiny web application server 
#

options(useFancyQuotes = F)

## Functions --------------------------------------------------------------------
# print_rows_cols
# print row+column selected
print_rows_cols = function(id) {
  cat('Rows selected:\n')
  print(input[[paste0(id, '_rows_selected')]])
  cat('Columns selected:\n')
  print(input[[paste0(id, '_columns_selected')]])
}



## Read files---------------------------------------------------------------

## Connect to DB

db_file <- "db/database.sqlite"
con <- dbConnect(drv=RSQLite::SQLite(), dbname=db_file)

# future work: datas<-readRDS("df_data.rds") 
datas <- read.csv('df_data.csv')
proj <- read.csv('df_proj.csv')
#user <- read.csv('register.csv')
# admin tables
am_method <- read.table('admin_method.csv',sep = ',',header = T)  
am_group <- read.table('admin_group.csv',sep = ',',header = T)   #am_group <- read.csv('admin_group.csv', header = T)



server <- function(input, output,session) {
  
  ## *HOME page  ------
  
  # links
  github_url <- a("GitHub Repository", href="https://github.com/MINGYU-XU/LotusRoot")
  NCBI_url <- a("NCBI", href="https://www.ncbi.nlm.nih.gov/")
  GEO_url <- a("GEO-NCBI", href="https://www.ncbi.nlm.nih.gov/geo/")
  output$links <- renderUI({
    tagList(github_url,br(),NCBI_url,br(),GEO_url)
    #tagList("GitHub:", github_url)
  })
  
  
  
  
  # show login/register dialog box when initiated
  # define the ui of the login dialog box
  login_dialog <- modalDialog(
    title = 'Welcome to My LIMS!',
    #footer = actionButton('useless','  '),
    size = 'l',
    box(width = 5,
        h4('Login'),
        textInput('tab_login.username','Username'),
        passwordInput('tab_login.password','Password'),
        actionButton('tab_login.login','Login'),
        tags$div(class = 'warn-text',textOutput('tab_login.login_msg'))),
    box(width = 7,
        h4('Register'),
        useShinyFeedback(), 
        textInput('registerName','Username', placeholder = "Please enter your full name"),
        passwordInput('registerpw','Password'),
        textInput("email", "Email :",placeholder = "Please enter your email"),
        uiOutput("register_group"),
        actionButton('register_ok','New User Register'),
        verbatimTextOutput("successfully_registered")
    )
  )  
  showModal(login_dialog)
  #register group options
  output$register_group <- renderUI(
    selectInput(inputId = "group", "Which Group/Lab/Research center you belong to?", 
                choices =  am_group[,1])
  )
  
  
  # validate the login username and password 
  userVal <- reactiveVal(tbl(con,"user"))
  observeEvent(input$tab_login.login, {
    
    username <- input$tab_login.username
    password <- input$tab_login.password
    #row.names(user) <- user$Name
    
    if(username %in% (userVal() %>% pull("Name"))
       & password == userVal() %>% filter(Name==username) %>% pull(Password)) 
    {
      # real time login
      #if(password == userVal() %>% filter(Name==username) %>% pull(Password)){
      # succesfully log in
      removeModal() # remove login dialog
      output$tab_login.welcome_text <- renderText(
        paste0('Welcome,', ' ',username,'!')
      )
      shinyjs::show('tab_login.welcome_div') # show welcome message
    } else if (username %in% (userVal() %>%  pull("Name")) 
               & !(password == userVal() %>% filter(Name==username) %>% pull(Password)))
    {
      # password incorrect, show the warning message
      # warning message disappear in 5 sec
      output$tab_login.login_msg <- renderText('Incorrect Password')
      shinyjs::show('tab_login.login_msg')
      shinyjs::delay(5000, hide('tab_login.login_msg')) ##Delay disappear
    } else #if( !(username %in% userVal()[,'Name']) )
    {
      # username not found, show the warning message
      # warning message disappear in 5 sec
      output$tab_login.login_msg <- renderText('Username Not Found. Please register.')
      shinyjs::show('tab_login.login_msg')
      shinyjs::delay(5000, hide('tab_login.login_msg'))  ##Delay disappear
    }
    # When the wrong name is entered, an error is reported
    ###Warning: Error in if: argument is of length zero ???????????
  })
  
  
  # new user register 
  # check unique user name
  observeEvent(input$registerName, {
    if(input$registerName %in% (tbl(con,"user") %>% pull(Name))) {
      showFeedbackDanger(
        inputId = "registerName",
        text = "The user name is already taken."
      )
      shinyjs::hide("register_ok") 
    } else {
      hideFeedback("registerName")
      shinyjs::show("register_ok") 
    }
  })
  # register
  observeEvent(input$register_ok,{
    # Tips for successful registration
    output$successfully_registered <- renderPrint({
      cat("Successfully registered!")
      shinyjs::delay(2000, hide('successfully_registered'))
    })
    u <- data.frame(Name = input$registerName,
                          Password = input$registerpw,
                          Email = input$email,
                          Group.Lab.Center = input$group,
                          #Permissions = input$permissions
                          Permissions = 'General_Staff'
                    )
    userVal(u)
    
    #Clear text input after submit
    updateTextInput(session, "registerName", value = "")     
    updateTextInput(session, "registerpw", value = "")     
    updateTextInput(session, "email", value = "")   
    updateTextInput(session, "group", value = "")  
    
    dbAppendTable(con,"user",u)
    #fwrite(userVal(),'register.csv',row.names = FALSE)
  })
  
  
  ### User permissions ------
  observeEvent(input$tab_login.login,{
    un=input$tab_login.username
    pm <- userVal() %>% filter(Name==un) %>% pull(Permissions)
    output$tab_login.permissions_text <- renderText(paste0('Your permissions: ',pm))
    print(pm)    
    #'General_Staff','Data_Administrator','Project_Supervisor ','System_Maintenance'
    if(pm == 'System_Maintenance'){
      #show Administrator
      output$admin_item <- renderMenu({
        menuItem("Administrator", tabName = "admin",icon = icon(name="user-cog"),
                 menuSubItem("User Information", tabName = "admin_user", 
                             icon = icon(name = "id-card")),
                 menuSubItem("Project Options", tabName = "admin_p", 
                             icon = icon(name = "project-diagram")),
                 menuSubItem("Dataset Options", tabName = "admin_d", 
                             icon = icon(name = "server")),
                 startExpanded = F)
      })
    } else if(pm == 'General_Staff'){  
      shinyjs::hide("add_proj")
      shinyjs::hide("edit_proj")
      shinyjs::hide("delete_proj")
      shinyjs::hide("add_data")
      shinyjs::hide("edit_data")
      shinyjs::hide("delete_data")
      #shinyjs::hide("admin_item")
    } else if(pm == 'Project_Supervisor'){
      #shinyjs::hide("add_data")
      #shinyjs::hide("edit_data")
      #shinyjs::hide("delete_data")
      output$admin_item <- renderMenu({
        menuItem("Administrator", tabName = "admin",icon = icon(name="user-cog"),
                 menuSubItem("Project Options", tabName = "admin_p", 
                             icon = icon(name = "project-diagram")),
                 startExpanded = F)
      })
    } else if(pm == 'Data_Administrator'){
      #shinyjs::hide("add_proj")
      #shinyjs::hide("edit_proj")
      #shinyjs::hide("delete_proj")
      output$admin_item <- renderMenu({
        menuItem("Administrator", tabName = "admin",icon = icon(name="user-cog"),
                 menuSubItem("Dataset Options", tabName = "admin_d", 
                             icon = icon(name = "server")),
                 startExpanded = F)
      })
    } 
  })
  
  
  #Proj/Data
  options(DT.options = list(pageLength = 10)) 
  
  # *ADD -----
  
  # Check unique proj name 
  observeEvent(input$projName, {
    proj <- projVal()
    if(input$projName %in% proj[,'Project.Name']) {
      ## feedback
      showFeedbackDanger(
        inputId = "projName",
        text = "The project name is already taken."
      )
      shinyjs::hide("add_proj") 
    } else {
      hideFeedback("projName")
      shinyjs::show("add_proj") 
    }
  })
  
  # proj options 
  output$proj_researcher <- renderUI(
    selectizeInput("projResearcher", "Researcher:",
                   choices = am_researcher_val()[,1],
                   multiple = F,
                   selected = NULL
                   #options = list(`actions-box` = TRUE)
    )
  )
  observe({
    ## 'Bioinformatician' synchronize changes with 'Researcher'
    output$proj_bioinfo <- renderUI(
      selectizeInput("projBioinformatician", "Bioinformatician:",
                     choices = am_researcher_val()[,1],
                     selected = input$projResearcher,
                     multiple = F
                     #options = list(`actions-box` = TRUE)
      )
    )
  })
  output$proj_group <- renderUI(
    selectizeInput("projGroup", "Group:",
                   choices = am_group_val()[,1],
                   multiple = F,
                   selected = NULL
                   #options = list(`actions-box` = TRUE)
    )
  )
  output$proj_parent <- renderUI(
    selectizeInput("projParent","Parent Project Name(optional):",
                   choices = projVal()[,2],
                   selected = NULL,
                   multiple = TRUE
                   #options = list(`actions-box` = TRUE)
    )
    
  )
  
  
  # ADD proj -----
  # Project ID generated by the system 
  projVal <- reactiveVal(proj)
  output$ proj_id<- renderText({
    proj <- projVal()
    cp <<- max(proj[,1]+1)
    cp })
  
  observeEvent(input$add_proj,{
    # change input:'Project.Name' to 'Project.ID', and store 'Project.ID' into the project table
    if(length(input$projParent) == 0 ){input_parentID=""}
    else{
      input_parentID <- projVal() %>% filter(Project.Name==input$projParent) %>% pull(Project.ID)
    }
    
    p <- rbind(data.frame(
      Project.ID = cp, #nrow(projVal())+1, 
      Project.Name = input$projName, 
      Parent = input_parentID,   #ã€€store 'Project.ID' into the project table
      Description = input$projDescription, 
      Start.Date = as.character(input$projDate),
      Path = input$projPath, 
      Sample.Sheet = input$projSampleSheet,
      Researcher = input$projResearcher, 
      Bioinformatician = input$projBioinformatician,
      Group = input$projGroup,
      Report = input$projReport,
      Status = input$projStatus, 
      Data.Repository = input$projdataRepository,
      Code.Repository = input$codeRepository,
      Permissions = input$projPermissions),projVal())                   
    projVal(p)
    
    
    # Successfully added
    output$new_proj_added <- renderPrint({
      cat("Successfully added!")
      shinyjs::delay(2000, hide('new_proj_added'))
    })
    
    #Clear text input after submit
    updateTextInput(session, "projID", value = "")     
    updateTextInput(session, "projName", value = "")     
    updateTextInput(session, "projParent", value = "")   
    updateTextInput(session, "projDescription", value = "")
    updateTextInput(session, "projDate", value = "")     
    updateTextInput(session, "projDate2", value = "")     
    updateTextInput(session, "projPath", value = "")   
    updateTextInput(session, "projSampleSheet", value = "")
    updateTextInput(session, "projResearcher", value = "")     
    updateTextInput(session, "projBioinformatician", value = "")     
    updateTextInput(session, "projGroup", value = "")   
    updateTextInput(session, "projReport", value = "")
    updateTextInput(session, "projStatus", value = "")     
    updateTextInput(session, "projdataRepository", value = "")     
    updateTextInput(session, "codeRepository", value = "")   
    updateTextInput(session, "projPermissions", value = "")
    
    fwrite(projVal(),'df_proj.csv',row.names = FALSE)
  })
  
  
  # ADD data -----
  
  # Check unique dataName 
  observeEvent(input$dataName, {
    datas <- dataVal() 
    if(input$dataName %in% datas[,'Data.Name']) {
      ## feedback
      showFeedbackDanger(
        inputId = "dataName",
        text = "The sample name is already taken."
      )
      shinyjs::hide("add_data") 
    } else {
      hideFeedback("dataName")
      shinyjs::show("add_data") 
    }
  })
  
  # data options 
  output$related_project_name <- renderUI(
    selectizeInput('dataprojID', 'Related Project Name(required):',
                   choices = projVal()[,2]
                   #selected = NULL
                   #multiple = FALSE
    )
  )
  output$data_method <- renderUI(
    selectizeInput("method", "Method:",
                   choices = am_method_val()[,1],
                   multiple = F
                   #selected = NULL
                   #options = list(`actions-box` = TRUE)
    )
  )
  output$data_organism <- renderUI(
    selectizeInput("organism", "Organism:",
                   choices = am_organism_val()[,1]
                   #multiple = F,
                   #selected = NULL
                   #options = list(`actions-box` = TRUE)
    )
  )
  output$data_cell <- renderUI(
    selectizeInput("cell", "Tissue/Cell:",
                   choices = am_cell_val()[,1]
                   #multiple = F,
                   #selected = NULL
                   #options = list(`actions-box` = TRUE)
    )
  )
  output$data_format <- renderUI(
    selectizeInput("format", "Format:",
                   choices = am_format_val()[,1]
                   #multiple = F,
                   #selected = NULL
                   #options = list(`actions-box` = TRUE)
    )
  )
  
  
  # ADD data 
  
  # Data ID generated by the system
  dataVal <- reactiveVal(datas)
  output$data_id <- renderText({
    datas <- dataVal()
    cd <<- max(datas[,1]+1)
    cd  })
  
  #dataVal <- reactiveVal(datas)
  observeEvent(input$add_data,{
    #if(!is.null(input$dataprojID)) {
    if(!input$dataprojID==""){
      # change input:'Project.Name' to 'Project.ID', and store 'Project.ID' into the project table
      input_related_parentID <- projVal() %>% filter(Project.Name==input$dataprojID) %>% pull(Project.ID)
      
      t <- rbind(data.frame(
        Data.ID = cd,#counter_d(),#nrow(datas)+1
        Related.ProjectID = input_related_parentID,  #store 'Project.ID' into the table
        Data.Name = input$dataName,
        Description = input$dataDescription, 
        Date = as.character(input$dataDate),
        Path = input$dataPath, 
        Data.Repository = input$dataRepository,
        Method = input$method,
        Organism = input$organism,
        Tissue.Cell = input$cell, 
        Genotype = input$genotype,
        Format = input$format
      ),dataVal()) 
      dataVal(t)
      
      #Successfully added
      output$new_data_added <- renderPrint({
        cat("Successfully added!")
        shinyjs::delay(2000, hide('new_data_added'))
      })
      
      #Clear text input after submit
      updateTextInput(session, "dataprojID", value = "")     
      updateTextInput(session, "dataName", value = "")   
      updateTextInput(session, "dataDescription", value = "")
      updateTextInput(session, "dataDate", value = "")     
      updateTextInput(session, "dataPath", value = "")     
      updateTextInput(session, "dataRepository", value = "")
      updateTextInput(session, "method", value = "")     
      updateTextInput(session, "organism", value = "")     
      updateTextInput(session, "cell", value = "")   
      updateTextInput(session, "genotype", value = "")
      updateTextInput(session, "format", value = "")     
      #updateTextInput(session, "treatment", value = "")     
      
      fwrite(dataVal(),'df_data.csv',row.names = FALSE)
    } else {
      #input_related_parentID <- 'NA'
      output$new_data_added <- renderPrint({
        cat("Please choose a project!")
        #shinyjs::delay(5000, hide('new_data_added'))
      })
      
    }
  })
  
  
  
  # *DELETE  -----
  
  # delete data row -----
  # These values allow the actions made in the modal to be delayed until the modal is closed
  values = reactiveValues(modal_closed=F )
  
  # Open the modal when delete button clicked
  observeEvent(input$delete_data, {
    values$modal_closed <- F
    showModal(modalDialog(h5('Are you sure you want to delete the data?'),
                          h5('If you confirm the deletion, click the Delete button below.'),
                          h5('If you do not want to delete it, you can click outside the dialog box to cancel.'), 
                          title = "Delete Dataset", 
                          easyClose = TRUE, ##If TRUE, the modal dialog can be dismissed by clicking outside the dialog box
                          footer = actionButton("delete_d",label = "Delete"))
    )
  })
  
  # This event is triggered by the actionButton inside the modalDialog
  # It closes the modal, and by setting values$modal_closed <- T
  observeEvent(input$delete_d,{
    values$modal_closed <- T
    removeModal()
  })  
  
  # only updated once the modal is closed
  observe({
    if(values$modal_closed){
      observeEvent(input$delete_d, {
        d = dataVal()
        if (!is.null(input$x2_rows_selected)) {
          d <- d[-as.numeric(input$x2_rows_selected),]
        }
        dataVal(d)
        fwrite(dataVal(),'df_data.csv',row.names = FALSE)
        values$modal_closed <- F
      })
    }
  })
  
  
  # delete proj row -----
  values_p = reactiveValues(modal_closed=F)
  observeEvent(input$delete_proj, {
    values_p$modal_closed <- F
    showModal(modalDialog("Are you sure you want to delete the project?
                          If you confirm the deletion, click the Delete button below.
                          If you don't want to delete it, you can click outside the dialog box to cancel.", 
                          title = "Delete Project", 
                          easyClose = TRUE, ##If TRUE, the modal dialog can be dismissed by clicking outside the dialog box
                          footer = actionButton("delete_p",label = "Delete"))
    )
  })
  observeEvent(input$delete_p,{
    values_p$modal_closed <- T
    removeModal()
  })  
  
  observe({
    if(values_p$modal_closed){
      observeEvent(input$delete_p, {
        dp = projVal() 
        if (!is.null(input$x1_rows_selected)) {
          dp <- dp[-as.numeric(input$x1_rows_selected),]
        }
        projVal(dp)
        fwrite(projVal(),'df_proj.csv',row.names = FALSE)
        values_p$modal_closed <- F
      })
    }
  })
  
  
  # *EDIT  -----
  # Edit data row -----
  
  edit_data_dialog <- modalDialog(
    title = 'Edit Datasets',
    footer = actionButton('useless','  '), 
    size = 'l', # large
    easyClose = TRUE, ##If TRUE, the modal dialog can be dismissed by clicking outside the dialog box
    helpText("To modify, please double-click where you want to edit."),
    helpText('Press "Enter" or "Ctrl+Enter" to confirm your changes.'),
    helpText("Note: Your changes are NOT automatically saved. Please click the save button after editing!!!"),
    box(
      width = 12,
      uiOutput("edit_popup_x4"), #x4: edit data table
      actionButton('save_data.popup','Save!',style = "color: white; background-color: red")
    )  
  )  
  
  output$edit_popup_x4 <- renderUI(
    DTOutput(outputId='x4')
  )
  
  output$x4 <-
    renderDT({
      edit_data_popup <<- dataVal()[input$x2_rows_selected,]    #before edit
      datatable(
        edit_data_popup,  
        escape = FALSE,
        rownames = FALSE, # hide row names
        editable = list(target = "cell",  
                        disable = list(columns = c(0,1,4))), 
        # cannot edit id, start date
        options = list(SortClasses = TRUE,
                       scrollX = TRUE,
                       dom = 'frtip')
      )
    })
  proxy_x4 <- dataTableProxy("x4")
  
  observeEvent(input$x4_cell_edit, {
    info <- input$x4_cell_edit
    str(info)
    i <- info$row
    j <- info$col+1  # column index offset by 1
    v <- info$value
    edit_data_popup[i,j] <<- DT::coerceValue(v,edit_data_popup[i,j])
    replaceData(proxy_x4, edit_data_popup, resetPaging = FALSE, rownames = FALSE)  # important
    
    edited_data_popup <<- edit_data_popup  # after edit
  })
  
  edit_value_d = reactiveValues(modal_closed=F)
  observeEvent(input$edit_data, {
    edit_value_d$modal_closed <- F
    showModal(edit_data_dialog)
  })
  # popup close
  observeEvent(input$save_data.popup,{  
    edit_value_d$modal_closed <- T
    removeModal()  
  })  
  
  observe({
    if(edit_value_d$modal_closed){
      observeEvent(input$save_data.popup, {
        d = dataVal()
        # delete old rows
        if (!is.null(input$x2_rows_selected)) {
          d <- d[-as.numeric(input$x2_rows_selected),]
        }
        # then add edited rows
        ed <<- rbind(edited_data_popup,d)	
        dataVal(ed)
        # save to file
        fwrite(dataVal(),'df_data.csv',row.names = FALSE)
      })
    }
  })
  
  
  # Edit proj row ----- 
  edit_proj_dialog <- modalDialog(
    title = 'Edit Projects',
    footer = actionButton('useless','  '), 
    size = 'l', # large
    easyClose = TRUE,     # If TRUE, the modal dialog can be dismissed by clicking outside the dialog box
    helpText("To modify, please double-click where you want to edit."),
    helpText('Press "Enter" or "Ctrl+Enter" to confirm your changes.'),
    helpText("Note: Your changes are NOT automatically saved. Please click the save button after editing!!!"),
    box(
      width = 12,
      uiOutput("edit_popup_x3"), #x3: edit proj table 
      actionButton('save_proj.popup','Save!',style = "color: white; background-color: red")
    )  
  )  
  
  output$edit_popup_x3 <- renderUI(
    DTOutput(outputId='x3')
  )
  
  output$x3 <-
    renderDT({
      edit_proj_popup <<- projVal()[input$x1_rows_selected,]    #before edit
      datatable(
        edit_proj_popup,  
        escape = FALSE,
        rownames = FALSE, # hide row names
        editable = list(target = "cell",  
                        disable = list(columns = c(0,4))), ###
        # cannot edit project id, start date
        options = list(SortClasses = TRUE,
                       scrollX = TRUE,
                       dom = 'frtip')
      )
    })
  proxy_x3 <- dataTableProxy("x3")
  
  observeEvent(input$x3_cell_edit, {
    info <- input$x3_cell_edit
    str(info)
    i <- info$row
    j <- info$col+1  # column index offset by 1
    v <- info$value
    edit_proj_popup[i,j] <<- DT::coerceValue(v,edit_proj_popup[i,j])
    replaceData(proxy_x3, edit_proj_popup, resetPaging = FALSE, rownames = FALSE)  # important
    
    edited_proj_popup <<- edit_proj_popup  # after edit
  })
  
  edit_value_p = reactiveValues(modal_closed=F)
  observeEvent(input$edit_proj, {
    edit_value_p$modal_closed <- F
    showModal(edit_proj_dialog)
  })
  # popup close
  observeEvent(input$save_proj.popup,{  
    edit_value_p$modal_closed <- T
    removeModal()  
  })  
  
  observe({
    if(edit_value_p$modal_closed){
      observeEvent(input$save_proj.popup, {
        p = projVal()
        # delete old rows
        if (!is.null(input$x1_rows_selected)) {
          p <- p[-as.numeric(input$x1_rows_selected),]
        }
        # then add edited rows
        edited_p <<- rbind(edited_proj_popup,p)	
        projVal(edited_p)
        # save to file
        fwrite(projVal(),'df_proj.csv',row.names = FALSE)
      })
    }
  })
  
  
  
  
  
  
  # *Clear selected rows -----
  # clear selected proj rows 
  x1_proxy <- DT::dataTableProxy("x1")
  observeEvent(input$clear_selected_proj,{
    selectRows(x1_proxy, NULL)
  })
  # clear selected data rows 
  x2_proxy <- DT::dataTableProxy("x2")
  observeEvent(input$clear_selected_data,{
    selectRows(x2_proxy, NULL)
  })
  
  
  
  
  # *Associating two tables -----
  
  # proj -> data-----
  ## if no row(in x1) selected, No table
  ## if select one parent proj, display all sub proj 
  ## if select one sub proj, display the datasets included in that proj
  
  # Project.ID
  pids <- reactive({
    if(length(input$x1_rows_selected)==0) {return(NULL)}   ## No output
    else { pids <- projVal()[input$x1_rows_selected,"Project.ID"] }
    return(pids)
  })
  # Parent ID
  parent_ids <- reactive({
    if(length(input$x1_rows_selected)==0) {return(NULL)}   ## No output
    else { parent_ids <- projVal()[input$x1_rows_selected,"Parent"] }
    return(parent_ids)
  })
  # all parent IDs
  ptids <- reactive({
    if(length(input$x1_rows_selected)==0) {return(NULL)}   
    else { ptids <- projVal()[,"Parent"] }
    return(ptids)
  })
  
  # related datasets -----
  output$related_datasets <- renderDT({
    pid<-pids()
    ptids <-ptids()
    if(length(input$x1_rows_selected)==0) {return(NULL)}   ## No output
    ## if: parent proj, output datasets related to all parent&sub-projs
    if(length(intersect(pid,ptids))>0){
      subp <- projVal() %>% filter(Parent %in% pid)  # get sub projs
      all_id <- rbind(pid,subp[,'Project.ID'])  # get all project.id
    } else { 
      ## if: sub proj, output datasets related to the sub proj
      all_id <- pid   
    }
    ds <- dataVal() %>% filter(Related.ProjectID %in% all_id)   ## Filter the dataset table
    datatable(ds,
              rownames = FALSE,
              selection=list(target = 'row'),#"single", 
              extensions = "FixedColumns",
              options = list(SortClasses = TRUE, 
                             scrollX = TRUE,
                             fixedHeader=TRUE,
                             fixedColumns = list(leftColumns = 1))
    )
  })
  
  # related projects -----
  output$rp <- renderDT({
    pid<-pids() #seleted project id(s)
    parent_id <- parent_ids() # selected parent id(s)
    all_parent_id <- ptids() # all parent IDs
    
    rp_all <- NULL
    
    if(length(input$x1_rows_selected)==0){
      return(NULL)
    }
    
    for (id in pid){
      if(id %in% all_parent_id){
        rp1 <- projVal() %>% filter(Parent %in% pid) 
        rp2 <- projVal() %>% filter(Project.ID==pid)
        rp0 <- rbind(rp2,rp1)
      } else {
        rp1 <- projVal() %>% filter(Parent  %in% na.omit(parent_id)) ##Remove NAs
        rp2 <- projVal() %>% filter(Project.ID %in% na.omit(parent_id)) ###Remove NAs and use %in% #(Project.ID==parent_id)
        rp0 <- rbind(rp2,rp1)
      }
      rp_all <- rbind(rp_all,rp0)
    }
    
    
    #rp1 <- projVal() %>% filter(Parent  %in% pid)   
    ## Filter subs, é€‰parentæ‰æ˜¾ç¤ºsubï¼Œé€‰sbuä¸æ˜¾ç¤º
    #rp2 <- projVal() %>% filter(Project.ID  %in% parent_id) 
    ## Filter parent, é€‰subæ‰æ˜¾ç¤ºï¼Œé€‰parentä¸æ˜¾ç¤º
    
    #rp0 <- rbind(rp2,projVal()[input$x1_rows_selected,],rp1)#,rp2) #,projVal()[input$x1_rows_selected,])
    
    rp_all.new <<- rp_all[!duplicated(rp_all),] ## Delete duplicate rows
    #rp_all <<- unique(rp_all)
    
    datatable(rp_all.new, 
              rownames = FALSE,
              selection="single", 
              #extensions = "FixedColumns",
              options = list(SortClasses = TRUE, 
                             scrollX = TRUE
                             #fixedColumns = list(leftColumns = 1)
              )
    ) %>% formatStyle(c('Project.Name','Status'),
                      'Status',                                
                      backgroundColor = styleEqual(
                        ## different status,different colours
                        c('On Hold','Ongoing','Completed','Published'), 
                        c("#C1CDCD", "#FDDBC7", "#92C5DE", "#B4EEB4")
                      )
    )
  })
  # Datasets per project
  output$one_proj_datasets <- renderDT({
    if(length(input$rp_rows_selected)==0) {return(NULL)}   ## No output 
    else { 
      show_id <- rp_all.new[input$rp_rows_selected,"Project.ID"]
      show_ds <- dataVal() %>% filter(Related.ProjectID %in% show_id)   ## Filter the dataset table
      datatable(show_ds,
                rownames = FALSE,
                selection=list(target = 'row'),#"single", 
                #extensions = "FixedColumns",
                options = list(SortClasses = TRUE, 
                               scrollX = TRUE,
                               fixedHeader=TRUE
                               #fixedColumns = list(leftColumns = 1)
                )
      )
    }
    
  })
  
  # related proj -> related dataset -----
  #show_ids <- reactive({
  #  if(length(input$rp_rows_selected)==0) {return(NULL)}   ## No output
  #  else { show_ids <- projVal()[input$rp_rows_selected,"Project.ID"] }
  #  return(show_ids)
  #})
  
  
  # data -> proj  -----
  ## if no row(in x1) selected, No table
  ## if select one row, display the projs include the data
  ## returnï¼š'pids2' is the Related.ProjectID
  pids2<-reactive({
    if(length(input$x2_rows_selected)==0) {return(NULL)}   ## No output
    else { pids2 <- dataVal()[input$x2_rows_selected,"Related.ProjectID"] }
    return(pids2)
  })
  
  output$related_proj <- renderDT({
    pid2<-pids2()
    ds2 <- projVal() %>% filter(Project.ID  %in% pid2)   ## Filter the table
    #rp2 <- 
    datatable(ds2,
              rownames = FALSE, 
              selection="single", 
              extensions = "FixedColumns",
              options = list(SortClasses = TRUE, scrollX = TRUE,fixedColumns = list(leftColumns = 1))
    ) %>% formatStyle(c('Project.Name','Status'),
                      'Status',                                
                      backgroundColor = styleEqual(
                        ## different status,different colours
                        c('On Hold','Ongoing','Completed','Published'), 
                        c("#C1CDCD", "#FDDBC7", "#92C5DE", "#B4EEB4")
                      )
    )
  })
  
  # future work: related data -> go to project page
  #observeEvent(input$go_to_proj,{
  #  updateTabItems(session,
  #                 inputId = "myproject",
  #                 selected = "current_project")
  #  })
  
  
  
  
  # *output -----
  
  # output proj table -----
  projVal <- reactiveVal(proj)
  output$x1 <- renderDT({
    #output$x1 <- renderRHandsontable({
    server = FALSE    ## client-side processing
    datatable(
      projVal(),
      rownames = FALSE, # hide row names
      selection = list(target = 'row'),   ## Multiple selection: rows
      #editable = list(target = "cell", disable = list(columns = c(0,1,4))), # cannot edit somr columns
      filter = list(position = 'top', clear = FALSE),
      extensions = c('Buttons','Select', 'SearchPanes'), # 'FixedColumns',
      options = list(
        dom = 'Blfrtip', 
        style = 'os', 
        items = 'row',
        scrollX = TRUE,
        #fixedColumns = list(leftColumns = 1),
        buttons = c('csv', 'excel', 'pdf'),
        searchHighlight = TRUE,
        search = list(regex = TRUE)
        #columnDefs = list(list(targets = c(3), searchable = FALSE)) 
        #Disable Searching for Individual Columns
        #columnDefs = list(list(searchPanes = list(show = FALSE), targets = 1:2)) 
        ## ??? no searchPanes
      ) 
      
    ) %>% formatStyle(c('Project.Name','Status'),
                      'Status',                                
                      backgroundColor = styleEqual(
                        ## different status,different colours
                        c('On Hold','Ongoing','Completed','Published'), 
                        c("#C1CDCD", "#FDDBC7", "#92C5DE", "#B4EEB4")
                      )
    )
    
  })
  
  
  # output dataset table -----
  output$x2 <- renderDT(
    dataVal(),
    rownames = FALSE,
    server = FALSE,     ## client-side processing
    selection = list(target = 'row'),   ## Multiple selection: rows  #selection = 'single',  #selection = 'none',
    #editable = 'cell', 
    #editable = list(target = "cell", disable = list(columns = c(0))), ## cannot edit some columns
    filter = list(position = 'top', clear = FALSE),
    extensions = c('Buttons','FixedColumns','Select', 'SearchPanes'),
    options = list(
      dom = 'PBlfrtip',  ##dom = 'PBlfrtip',P:searchpane,B:button
      style = 'os', 
      items = 'row',
      scrollX = TRUE,
      #fixedColumns = list(leftColumns = 1),
      buttons = c('csv', 'excel', 'pdf'), 
      searchHighlight = TRUE,
      search = list(regex = TRUE),
      #columnDefs = list(list(targets = c(1), searchable = FALSE))   
      #Disable Searching for Individual Columns
      columnDefs = list(list(searchPanes = list(show = F), targets = 1:7)) ##searchPanes
    )         
    
  )  
  
  
  # *Administrator -----
  
  # admin_user ------
  
  # ADD user info
  userVal <- reactiveVal(tbl(con,"user"))
  observeEvent(input$userName, {
    # check unique user name
    if(input$userName %in% (userVal() %>% pull("Name"))) {
      showFeedbackDanger(
        inputId = "userName",
        text = "The user name is already taken."
      )
      shinyjs::hide("add_user") 
    } else {
      hideFeedback("userName")
      shinyjs::show("add_user") 
    }
  })
  
  # admin user group options 
  output$admin_user_group <- renderUI(
    column(4, selectInput("userGroup", "Group", choices = am_group[,1]))
  )
  observeEvent(input$add_user,{
    u <-data.frame(
        Name = input$userName,  
        Email = input$userEmail,  
        Group.Lab.Center = input$userGroup, 
        Permissions = input$userPermissions,
        Password = input$userPW
      ) 
    
    output$user_successfully_added <- renderPrint({
      cat("Successfully added!")
      shinyjs::delay(2000, hide('user_successfully_added'))
    })
    #Clear text input after submit
    updateTextInput(session, "userName", value = "")     
    updateTextInput(session, "userPW", value = "")   
    updateTextInput(session, "userEmail", value = "")
    updateTextInput(session, "userGroup", value = "")     
    updateTextInput(session, "userPermissions", value = "")     
    # save
    dbAppendTable(con,"user",u)
    userVal(1)
    userVal(tbl(con,"user"))
    #fwrite(userVal(),'register.csv',row.names = FALSE)
  })
  
  ## DELETE user 
  values_u = reactiveValues(modal_closed=F)
  observeEvent(input$delete_user, {
    values_u$modal_closed <- F
    showModal(modalDialog("Are you sure you want to delete?
                          If you confirm the deletion, click the Delete button below.
                          If you don't want to delete it, you can click outside the dialog box to cancel.", 
                          title = "Delete User", 
                          easyClose = TRUE, ##If TRUE, the modal dialog can be dismissed by clicking outside the dialog box
                          footer = actionButton("delete_u",label = "Delete"))
    )
  })
  observeEvent(input$delete_u,{
    values_u$modal_closed <- T
    removeModal()
  })  
  observe({
    if(values_u$modal_closed){
      observeEvent(input$delete_u, {
        if (!is.null(input$admin_user_info_rows_selected)) {
          r<-userVal() %>% data.frame() %>% slice(input$admin_user_info_rows_selected)
          q=paste0("DELETE FROM user WHERE Name IN (",paste(sQuote(r$Name),collapse = ","),")")
          dbExecute(con,q)
          #r <- r[-as.numeric(input$admin_user_info_rows_selected),]
        }
        userVal(1) ##Hack to refresh table...
        userVal(tbl(con,"user"))
        #userVal(r)
        #fwrite(userVal(),'register.csv',row.names = FALSE)
        values_u$modal_closed <- F
      })
    }
  })
  
  # output user_info
  output$admin_user_info <- DT::renderDT(
    userVal() %>% select(-Password) %>% data.frame(), ##hide password col
    rownames = FALSE,
    server = FALSE,     ## client-side processing 
    selection = list(target = 'row'),   ## Multiple selection: rows
    editable = list(target = "cell"), # disable = list(columns = c(0))# cannot edit column1
    filter = list(position = 'top', clear = FALSE),
    extensions = c('Buttons'),
    options = list(dom = 'lfrtip', style = 'os', items = 'row',
                   scrollX = TRUE,
                   #buttons = c('csv', 'excel', 'pdf'),
                   searchHighlight = TRUE,search = list(regex = TRUE))
  )
  
  
  ## Modal edit
  
  labelMandatory <- function(label) {
    tagList(
      label,
      span("*", class = "mandatory_star")
    )
  }
  
  appCSS <- ".mandatory_star { color: red; }"
  
  fieldsMandatory <- c("userName", "userPW","userGroup","userEmail","userPermissions")
  
  observe({
    mandatoryFilled <-
      vapply(fieldsMandatory,
             function(x) {
               !is.null(input[[x]]) && input[[x]] != ""
             },
             logical(1))
    mandatoryFilled <- all(mandatoryFilled)
    
    shinyjs::toggleState(id = "submit", 
                         condition = mandatoryFilled)
  })
  
  entry_form <- function(button_id){
    dft<-userVal() %>% data.frame()
    showModal(
      modalDialog(
        div(id=("entry_form"),
            tags$head(tags$style(".modal-dialog{ width:400px}")), #Modify the width of the dialog
            tags$head(tags$style(HTML(".shiny-split-layout > div {overflow: visible}"))), #Necessary to show the input options
            fluidPage(
              fluidRow(
                renderText(paste("User name:",dft[input$admin_user_info_rows_selected, "Name"])),
                #textInput("userNameM", labelMandatory("Name"), placeholder = ""),
                passwordInput('userPWM','Password'),
                textInput("userEmailM", labelMandatory("Email"), placeholder = ""),
                selectInput("userGroupM", labelMandatory("Group"), choices = am_group[,1]),
                selectInput(inputId = "userPermissionsM", labelMandatory("User Permissions:"), 
                          choices = c('General_Staff','Data_Administrator','Project_Supervisor','System_Maintenance')),
                helpText(labelMandatory(""), paste("Mandatory field.")),
                actionButton(button_id, "Submit")
              ),
              easyClose = TRUE
            )
          )
        )
      )
  }
  
  observeEvent(input$edit_user, priority = 20,{
    showModal(
      if(length(input$admin_user_info_rows_selected) > 1 ){
        modalDialog(
          title = "Warning",
          paste("Please select only one row." ),easyClose = TRUE)
      } else if(length(input$admin_user_info_rows_selected) < 1){
        modalDialog(
          title = "Warning",
          paste("Please select a row." ),easyClose = TRUE)
      })  
    
    if(length(input$admin_user_info_rows_selected) == 1 ){
      
      entry_form("submit_edit")
      
      dft<-userVal() %>% data.frame()
      updateTextInput(session, "userPWM", value = dft[input$admin_user_info_rows_selected, "Password"])
      updateTextInput(session, "userEmailM", value = dft[input$admin_user_info_rows_selected, "Email"])
      updateTextInput(session, "userGroupM", value = dft[input$admin_user_info_rows_selected, "Group.Lab.Center"])
      updateTextInput(session, "userPermissionsM", value = dft[input$admin_user_info_rows_selected, "Permissions"])
    }
    
  }) 
  
  observeEvent(input$submit_edit, priority = 20, {
    
    dft<-userVal() %>% data.frame()
    row_selection <- dft[input$admin_user_info_row_last_clicked, "Name"] 
    dbExecute(con, sprintf('UPDATE "user" SET "Password" = ?, "Email" = ?,
                          "Group.Lab.Center" = ?, "Permissions" = ? WHERE "Name" = ("%s")', row_selection), 
              param = list(input$userPWM,
                           input$userEmailM,
                           input$userGroupM,
                           input$userPermissionsM))
    removeModal()
    userVal(1) ##Hack to refresh table...
    userVal(tbl(con,"user"))
    
  })
  
  # edit user information (no edit button)
  proxy_admin_user_info <- dataTableProxy("admin_user_info")
  observeEvent(input$admin_user_info_cell_edit, {
    edit_user_table <<- userVal()
    
    info <- input$admin_user_info_cell_edit
    str(info)
    i <- info$row
    j <- info$col+1  # column index offset by 1
    v <- info$value
    edit_user_table[i,j] <<- DT::coerceValue(v,edit_user_table[i,j])
    replaceData(proxy_admin_user_info, edit_user_table, resetPaging = FALSE, rownames = FALSE)  # important
    
    edited_user_table <<- edit_user_table  # after edit
    
    userVal(edited_user_table)
    
    fwrite(userVal(),'register.csv',row.names = FALSE)
  })
  
  
  # admin_proj -----
  # 1 researcher/ bioinformatician -----
  am_researcher <- read.csv('admin_researcher.csv')
  am_researcher_val <- reactiveVal(am_researcher)
  output$admin_researcher <- renderDT(
    am_researcher_val(),
    server = FALSE, 
    selection = list(target = 'row'),
    editable = list(target = "cell", disable = list(columns = c(0))), 
    filter = list(position = 'top', clear = FALSE),
    options = list(#dom = 'Blfrtip', 
      style = 'os', 
      items = 'row',
      scrollX = TRUE,
      searchHighlight = TRUE)
  )
  # check unique name
  observeEvent(input$researcherName, {
    if(input$researcherName %in% am_researcher_val()[,1]) {
      showFeedbackDanger(
        inputId = "researcherName",
        text = "The researcher name is already taken."
      )
      shinyjs::hide("add_researcher") 
    } else {
      hideFeedback("researcherName")
      shinyjs::show("add_researcher") 
    }
  })
  # add
  am_researcher_val <- reactiveVal(am_researcher)
  observeEvent(input$add_researcher,{
    re <- rbind(data.frame(Researcher = input$researcherName),
                am_researcher_val() )
    am_researcher_val(re)
    output$researcher_successfully_added <- renderPrint({
      cat("Successfully added!")
      shinyjs::delay(2000, hide('researcher_successfully_added'))
    })
    updateTextInput(session, "researcherName", value = "")    #Clear text input after submit  
    fwrite(am_researcher_val(),'admin_researcher.csv',row.names = FALSE) # save
  })
  ## DELETE researcher 
  values_r = reactiveValues(modal_closed=F)
  observeEvent(input$delete_researcher, {
    values_r$modal_closed <- F
    showModal(modalDialog("Are you sure you want to delete?
                          If you confirm the deletion, click the Delete button below.
                          If you don't want to delete it, you can click outside the dialog box to cancel.", 
                          title = "Delete Researcher/Bioinformatician", 
                          easyClose = TRUE,  ##If TRUE, the modal dialog can be dismissed by clicking outside the dialog box
                          footer = actionButton("delete_rb",label = "Delete"))
    )
  })
  observeEvent(input$delete_rb,{
    values_r$modal_closed <- T
    removeModal()
  })  
  observe({
    if(values_r$modal_closed){
      observeEvent(input$delete_rb, {
        rb <- am_researcher_val()
        if (!is.null(input$admin_researcher_rows_selected)) {
          rb <- data.frame( Researcher = rb[-as.numeric(input$admin_researcher_rows_selected),])
        }
        am_researcher_val(rb)
        fwrite(am_researcher_val(),'admin_researcher.csv',row.names = FALSE)
        values_r$modal_closed <- F
      })
    }
  })
  
  # 2 group -----
  am_group_val <- reactiveVal(am_group)
  output$admin_group <- renderDT(
    am_group_val(),
    server = FALSE, 
    selection = list(target = 'row'),
    editable = list(target = "cell", disable = list(columns = c(0))), 
    filter = list(position = 'top', clear = FALSE),
    options = list(#dom = 'Blfrtip', 
      style = 'os', 
      items = 'row',
      scrollX = TRUE,
      searchHighlight = TRUE)
  )
  # check unique user name
  observeEvent(input$groupName, {
    if(input$groupName %in% am_group_val()[,1]) {
      showFeedbackDanger(
        inputId = "groupName",
        text = "The group name is already taken."
      )
      shinyjs::hide("add_group") 
    } else {
      hideFeedback("groupName")
      shinyjs::show("add_group") 
    }
  })
  # add
  observeEvent(input$add_group,{
    g <- rbind(data.frame(Group = input$groupName),
               am_group_val() )
    am_group_val(g)
    output$group_successfully_added <- renderPrint({
      cat("Successfully added!")
      shinyjs::delay(2000, hide('group_successfully_added'))
    })
    #Clear text input after submit
    updateTextInput(session, "groupName", value = "")    #Clear text input after submit  
    fwrite(am_group_val(),'admin_group.csv',row.names = FALSE) # save
  })
  ## DELETE group
  values_g = reactiveValues(modal_closed=F)
  observeEvent(input$delete_group, {
    values_g$modal_closed <- F
    showModal(modalDialog("Are you sure you want to delete?
                          If you confirm the deletion, click the Delete button below.
                          If you don't want to delete it, you can click outside the dialog box to cancel.", 
                          title = "Delete Group", 
                          easyClose = TRUE,  ##If TRUE, the modal dialog can be dismissed by clicking outside the dialog box
                          footer = actionButton("delete_g",label = "Delete"))
    )
  })
  observeEvent(input$delete_g,{
    values_g$modal_closed <- T
    removeModal()
  })  
  observe({
    if(values_g$modal_closed){
      observeEvent(input$delete_g, {
        g <- am_group_val()
        if (!is.null(input$admin_group_rows_selected)) {
          g <- data.frame(Group = g[-as.numeric(input$admin_group_rows_selected),]) 
        }
        am_group_val(g)
        fwrite(am_group_val(),'admin_group.csv',row.names = FALSE)
        values_g$modal_closed <- F
      })
    }
  })
  
  
  # admin_dataset -----
  ## 1 method-----
  am_method_val <- reactiveVal(am_method)
  output$admin_method <- renderDT(
    am_method_val(),
    server = FALSE, 
    selection = list(target = 'row'),
    editable = list(target = "cell", disable = list(columns = c(0))), 
    filter = list(position = 'top', clear = FALSE),
    options = list(#dom = 'Blfrtip', 
      style = 'os', 
      items = 'row',
      scrollX = TRUE,
      searchHighlight = TRUE)
  )
  # check unique name
  observeEvent(input$methodName, {
    if(input$methodName %in% am_method_val()[,1]) {
      showFeedbackDanger(
        inputId = "methodName",
        text = "The method name is already taken."
      )
      shinyjs::hide("add_method") 
    } else {
      hideFeedback("methodName")
      shinyjs::show("add_method") 
    }
  })
  # add 
  observeEvent(input$add_method,{
    m <- rbind(data.frame(Method = input$methodName),
               am_method_val() )
    am_method_val(m)
    output$method_successfully_added <- renderPrint({
      cat("Successfully added!")
      shinyjs::delay(2000, hide('method_successfully_added'))
    })
    updateTextInput(session, "methodName", value = "")    #Clear text input after submit  
    fwrite(am_method_val(),'admin_method.csv',row.names = FALSE) # save
  })
  ## DELETE 
  values_m = reactiveValues(modal_closed=F)
  observeEvent(input$delete_method, {
    values_m$modal_closed <- F
    showModal(modalDialog("Are you sure you want to delete?
                          If you confirm the deletion, click the Delete button below.
                          If you don't want to delete it, you can click outside the dialog box to cancel.", 
                          title = "Delete Method", 
                          easyClose = TRUE,  ##If TRUE, the modal dialog can be dismissed by clicking outside the dialog box
                          footer = actionButton("delete_m",label = "Delete"))
    )
  })
  observeEvent(input$delete_m,{
    values_m$modal_closed <- T
    removeModal()
  })  
  observe({
    if(values_m$modal_closed){
      observeEvent(input$delete_m, {
        m <- am_method_val()
        if (!is.null(input$admin_method_rows_selected)) {
          m <- data.frame(Method = m[-as.numeric(input$admin_method_rows_selected),])
        }
        am_method_val(m)
        fwrite(am_method_val(),'admin_method.csv',row.names = FALSE)
        values_m$modal_closed <- F
      })
      
    }
  })
  
  
  ## 2 organism-----
  am_organism <- read.table('admin_organism.csv',sep = ',',header = T)
  am_organism_val <- reactiveVal(am_organism)
  output$admin_organism <- renderDT(
    am_organism_val(),
    server = FALSE, 
    selection = list(target = 'row'),
    editable = list(target = "cell", disable = list(columns = c(0))), 
    filter = list(position = 'top', clear = FALSE),
    options = list(#dom = 'Blfrtip', 
      style = 'os', 
      items = 'row',
      scrollX = TRUE,
      searchHighlight = TRUE)
  )
  # check unique name
  observeEvent(input$organismName, {
    if(input$organismName %in% am_organism_val()[,1]) {
      showFeedbackDanger(
        inputId = "organismName",
        text = "The organism name is already taken."
      )
      shinyjs::hide("add_organism") 
    } else {
      hideFeedback("organismName")
      shinyjs::show("add_organism") 
    }
  })
  # add 
  observeEvent(input$add_organism,{
    o <- rbind(data.frame(Organism = input$organismName),
               am_organism_val() )
    am_organism_val(o)
    output$organism_successfully_added <- renderPrint({
      cat("Successfully added!")
      shinyjs::delay(2000, hide('organism_successfully_added'))
    })
    updateTextInput(session, "organismName", value = "")    #Clear text input after submit  
    fwrite(am_organism_val(),'admin_organism.csv',row.names = FALSE) # save
  })
  ## DELETE 
  values_o = reactiveValues(modal_closed=F)
  observeEvent(input$delete_organism, {
    values_o$modal_closed <- F
    showModal(modalDialog("Are you sure you want to delete?
                          If you confirm the deletion, click the Delete button below.
                          If you don't want to delete it, you can click outside the dialog box to cancel.", 
                          title = "Delete Organism", 
                          easyClose = TRUE,  ##If TRUE, the modal dialog can be dismissed by clicking outside the dialog box
                          footer = actionButton("delete_o",label = "Delete"))
    )
  })
  observeEvent(input$delete_o,{
    values_o$modal_closed <- T
    removeModal()
  })  
  observe({
    if(values_o$modal_closed){
      observeEvent(input$delete_o, {
        o <- am_organism_val()
        if (!is.null(input$admin_organism_rows_selected)) {
          o <- data.frame( Organism = o[-as.numeric(input$admin_organism_rows_selected),])
        }
        am_organism_val(o)
        fwrite(am_organism_val(),'admin_organism.csv',row.names = FALSE)
        values_o$modal_closed <- F
      })
    }
  })
  
  
  ## 3 tissue.cell -----
  am_cell <- read.table('admin_cell.csv',sep = ',',header = T)
  am_cell_val <- reactiveVal(am_cell)
  output$admin_cell <- renderDT(
    am_cell_val(),
    server = FALSE, 
    selection = list(target = 'row'),
    editable = list(target = "cell", disable = list(columns = c(0))), 
    filter = list(position = 'top', clear = FALSE),
    options = list(dom = 'lfrtip', 
                   style = 'os', 
                   items = 'row',
                   scrollX = TRUE,
                   searchHighlight = TRUE)
  )
  # check unique name
  observeEvent(input$cellName, {
    if(input$cellName %in% am_cell_val()[,1]) {
      showFeedbackDanger(
        inputId = "cellName",
        text = "The cell name is already taken."
      )
      shinyjs::hide("add_cell") 
    } else {
      hideFeedback("cellName")
      shinyjs::show("add_cell") 
    }
  })
  # add 
  observeEvent(input$add_cell,{
    c <- rbind(data.frame(Tissue.Cell = input$cellName),
               am_cell_val() )
    am_cell_val(c)
    output$cell_successfully_added <- renderPrint({
      cat("Successfully added!")
      shinyjs::delay(2000, hide('cell_successfully_added'))
    })
    updateTextInput(session, "cellName", value = "")    #Clear text input after submit  
    fwrite(am_cell_val(),'admin_cell.csv',row.names = FALSE) # save
  })
  ## DELETE 
  values_c = reactiveValues(modal_closed=F)
  observeEvent(input$delete_cell, {
    values_c$modal_closed <- F
    showModal(modalDialog("Are you sure you want to delete?
                          If you confirm the deletion, click the Delete button below.
                          If you don't want to delete it, you can click outside the dialog box to cancel.", 
                          title = "Delete Tissue/Cell", 
                          easyClose = TRUE,  ##If TRUE, the modal dialog can be dismissed by clicking outside the dialog box
                          footer = actionButton("delete_c",label = "Delete"))
    )
  })
  observeEvent(input$delete_c,{
    values_c$modal_closed <- T
    removeModal()
  })  
  observe({
    if(values_c$modal_closed){
      observeEvent(input$delete_c, {
        c <- am_cell_val()
        if (!is.null(input$admin_cell_rows_selected)) {
          c <- data.frame( Tissue.Cell = c[-as.numeric(input$admin_cell_rows_selected),])
        }
        am_cell_val(c)
        fwrite(am_cell_val(),'admin_cell.csv',row.names = FALSE)
        values_c$modal_closed <- F
      })
    }
  })
  
  ## 4 format -----
  am_format <- read.table('admin_format.csv',sep = ',',header = T)
  am_format_val <- reactiveVal(am_format)
  output$admin_format <- renderDT(
    am_format_val(),
    server = FALSE, 
    selection = list(target = 'row'),
    editable = list(target = "cell", disable = list(columns = c(0))), 
    filter = list(position = 'top', clear = FALSE),
    options = list(#dom = 'Blfrtip', 
      style = 'os', 
      items = 'row',
      scrollX = TRUE,
      searchHighlight = TRUE)
  )
  # check unique name
  observeEvent(input$formatName, {
    if(input$formatName %in% am_format_val()[,1]) {
      showFeedbackDanger(
        inputId = "formatName",
        text = "The format name is already taken."
      )
      shinyjs::hide("add_format") 
    } else {
      hideFeedback("formatName")
      shinyjs::show("add_format") 
    }
  })
  observeEvent(input$add_format,{
    f <- rbind(data.frame(Format = input$formatName),
               am_format_val() )
    am_format_val(f)
    output$format_successfully_added <- renderPrint({
      cat("Successfully added!")
      shinyjs::delay(2000, hide('format_successfully_added'))
    })
    updateTextInput(session, "formatName", value = "")    #Clear text input after submit  
    fwrite(am_format_val(),'admin_format.csv',row.names = FALSE) # save
  })
  ## DELETE 
  values_f = reactiveValues(modal_closed=F)
  observeEvent(input$delete_format, {
    values_f$modal_closed <- F
    showModal(modalDialog("Are you sure you want to delete?
                          If you confirm the deletion, click the Delete button below.
                          If you don't want to delete it, you can click outside the dialog box to cancel.", 
                          title = "Delete Format", 
                          easyClose = TRUE,  ##If TRUE, the modal dialog can be dismissed by clicking outside the dialog box
                          footer = actionButton("delete_f",label = "Delete"))
    )
  })
  observeEvent(input$delete_f,{
    values_f$modal_closed <- T
    removeModal()
  })  
  observe({
    if(values_f$modal_closed){
      observeEvent(input$delete_f, {
        f <- am_format_val()
        if (!is.null(input$admin_format_rows_selected)) {
          f <- data.frame( Format = f[-as.numeric(input$admin_format_rows_selected),])
        }
        am_format_val(f)
        fwrite(am_format_val(),'admin_format.csv',row.names = FALSE)
        values_f$modal_closed <- F
      })
    }
  })
  
  
  # *About us page  -----
  
  # manual
  manual_link <- a("LotusRoot Manual", href="https://github.com/MINGYU-XU/LotusRoot/blob/master/www/LotusRoot_Manual.pdf")
  output$LotusRoot_manual <- renderUI({
    #tabItem("aboutus", 
    #         tags$iframe(style="height:600px; width:100%; scrolling=yes", 
    #                     src="https://github.com/MINGYU-XU/LotusRoot/blob/master/www/LotusRoot_Manual.pdf"))
    tagList(manual_link)
  })
  output$home_manual <- renderUI({
    tagList(manual_link)
  })
  
  
  
  # *Log out
  observeEvent(input$logout_btn,{
    showModal(modalDialog(
      "You successfully logged out! Thanks for using LotueRoot!", 
      title = "Logout", 
      easyClose = F,  ##If TRUE, the modal dialog can be dismissed by clicking outside the dialog box
      footer = list(
        actionButton("close_page",label = "Close App"),
        actionButton("go_to_login",label = "Login again")
      ))
    )
  })
  observeEvent(input$close_page,{
    stopApp()
  })
  observeEvent(input$go_to_login,{
    showModal(login_dialog)
  })
  
  
  
  
}


## secure credentials info (optional)
#if (interactive()) {
#  # define some credentials
#  credentials <- data.frame(
#    user = c("test", "123"),
#    password = c("test", "123"),
#    stringsAsFactors = FALSE
#  )}
# (optional)check_credentials returns a function to authenticate users
#res_auth <- secure_server(
#  check_credentials = check_credentials(credentials)
#)
#output$auth_output <- renderPrint({
#  reactiveValuesToList(res_auth)
#})