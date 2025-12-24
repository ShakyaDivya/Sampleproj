{
  rm(list=ls())#clear the environment
  gc() #clean the cache
  
  library(shiny)
  library(shinyjs)
  library(shinyauthr) #login
  library(tidyverse) #data data1 <- dataframe%>%filter()%>%select()%>%mod..()
  library(data.table) #datatable[][][]
  library(arrow)
  library(shinyWidgets)
  library(plotly)
  library(transformr)
  library(tools)
  options(scipen = 10000)
  
  
  #--------------- Functions --------------------
  
  source("functions/pyramid.r") #pyramid function is here
  
  source("functions/helpers.R")
  
  
  #---------------DATA PREPARATION--------------------

  region.id <- read.csv("data/prov dist gapa ward id.csv")
  setDT(region.id)
  
  region.id[,distmuni:=dist*100 + gapa]
  prov.dist.list <- unique(region.id[,.(provname,dname)])
  
  dist.list <- region.id[,unique(dname)]
  distnames <- unique(region.id[,.(dist,dname)][,dist:=as.integer(dist*10000)])
  
  provnames <- unique(region.id[,.(prov,provname)])[,setnames(.SD,c("prov","provname"),c("dist","dname"))
  ][,dist:=as.integer(dist*1000000)]
  nepalname <- data.table(dist=as.integer(10000000),dname="Nepal")
  areas <- rbind(nepalname,provnames,distnames)
  
  #list of muni
  dist.muni.list <- unique(region.id[,.(distmuni,dname,GaPa_NaPa)])
  dist.muni.list[,distmuni:=distmuni*100]
  
  
  dist.muni <- unique(region.id[,distmuniname:=paste(dname,GaPa_NaPa,sep="_")
  ][,.(distmuni,distmuniname)])
  # load(file="data/reshmistest.rda")
  prov.ext <- function(ward) {prov=(ward%/%1000000);return(prov)}
  dist.ext <- function(ward) {dist=(ward%%1000000)%/%10000;return(dist)}
  #DDMM
  distmuni.ext <- function(ward) {distmuni=(ward%%1000000)%/%100;return(distmuni)}
  muni.ext <- function(ward) {muni=(ward%%10000)%/%100;return(muni)}
  ward.ext <- function(ward) {ward=(ward%%100);return(ward)}
  
  
  prov.dist.id.proj <-unique(region.id[,.(prov,dist)])[,`:=`(prov=prov*1000000,dist=dist*10000)]
  
  
  id.cols = c("Time","scen","sex","agest")
  measure.vars <-c("pop","births", "idom","odom","absemi","absen","ret","imm","deaths","popabs","deaths.abs")
  measure.vars.nm <-  c("Population","Births", "In-Domestic","Out-Domestic","AbsEmigrants","AbsenteeFlow","Returnees",
                        "Immigrants","Deaths","Absentee","Deaths.Absentee")
  names(measure.vars) <- measure.vars.nm
  names(measure.vars.nm) <- measure.vars
  scens =c(
    "Medium", #"n22" 
    "Low", #"n23"
    "High")  #"n24"
} #prepare data




# UI ----------------------------------------------------------------------
{
  ui <- fluidPage(
    useShinyjs(),
    # Application title
    h3(strong("Nepal: Population  Projection 2021-2051")),
    # useShinyjs(),  # Initialize shinyjs
    
    
    uiOutput("sidebarpanel")
  )
  
}#ui

#--------------------- server ---------------------------------------------
server <- function(input, output, session) {
  observeEvent(input$toggleSidebar, {
    shinyjs::toggle(id = "sidebarPanelDiv", anim = TRUE)
  })
  observeEvent(input$toggle_population_cards, {
    shinyjs::toggle(id = "population_card_box", anim = TRUE)
  })
  
  selected_time <- reactive({
    if (input$tabselected == "Population Change") {
      input$Time_pop
    } else {
      input$Time_others
    }
  })
  
  credentials <- shinyauthr::loginServer(
    id = "login",
    data = user_base,
    user_col = user,
    pwd_col = password,
    sodium_hashed = TRUE,
    log_out = reactive(logout_init())
  )
  if (!dir.exists("www")) dir.create("www")
  
  
  # Logout to hide
  logout_init <- shinyauthr::logoutServer(
    id = "logout",
    active = reactive(credentials()$user_auth)
  )
  
  
  #ui sidebar pop changed
  output$sidebarpanel <- renderUI({
    
    # Show only when authenticated
    # req(credentials()$user_auth)
    
    column(width = 4,
           p(paste("You have", credentials()$info[["permissions"]],"permission"))
    )
    
    sidebarLayout(
      sidebarPanel(width = 2, 
                   div(id = "sidebarPanelDiv",
                       selectInput(inputId = "Scenario", #this name is used in the server
                                   label = "Select Scenarios",
                                   choices = scens,
                                   selected = "Medium",
                                   multiple = TRUE
                       ),
                       
                       # First select input that controls the second one
                       selectInput(inputId = "prov",
                                   label = "Select Province",
                                   choices = c("Nepal",provnames$dname),
                                   selected = "Nepal",
                                   multiple = FALSE
                       ),
                       # Second select input that will be updated based on the first input
                       conditionalPanel(
                         condition = "input.prov !='Nepal'",
                         selectInput("dist",
                                     "Select District (Select Province First)",
                                     choices=c("All"),
                                     selected = "All")),  # Start with no choices
                       # Thired select input that will be updated based on the first input
                       
                       conditionalPanel(
                         condition = "input.dist !='All'", 
                         selectInput("muni",
                                     "Select Palika (Select District First)",
                                     choices=c("All"),
                                     selected = "All")),  # Start with no choices
                       conditionalPanel(
                         condition = "input.muni != 'All'", 
                         selectInput("ward",
                                     "Select Ward (Select Ward First)",
                                     choices=c("All"),
                                     selected = "All")),  # Start with no choices
                       
                       conditionalPanel(
                         condition = "input.tabselected == 'Population Change'",
                         selectInput(inputId = "Time_pop", 
                                     label = "Select Time ",
                                     choices = seq(2021, 2051, 1),
                                     selected = c(2021, 2051),
                                     multiple = TRUE)
                       ),
                       conditionalPanel(
                         condition = "input.tabselected != 'Population Change'",
                         selectInput(inputId = "Time_others", 
                                     label = "Select Time ",
                                     choices = seq(2021, 2051, 1),
                                     selected = c(2021, 2050),
                                     multiple = TRUE)
                       ),
                       
                       
                       selectInput(inputId = "indicator", #for Pyramid
                                   label = "Select Indicator for Line",
                                   choices = names(measure.vars),#
                                   selected = "Population",
                                   multiple = F
                       )
                   )#sidebar panel
      ), #sidebar Layout
      
      
      
      mainPanel(
        navbarPage("RESULTS",id = "tabselected",
                   
                   
                   
                   tabPanel("Population Change",
                            fluidRow(
                              column(8,
                                     div(style = "margin-bottom: 10px;",
                                         sliderInput(
                                           inputId = "ageRange_totline",
                                           label = "Select Age",
                                           min = 0,
                                           max = 100,
                                           value = c(0, 100),
                                           step = 1
                                         )
                                     ),
                                     plotlyOutput("totline"),
                                     downloadButton(outputId = "downloadDataTotline", label = "Download Data (CSV)"),
                                     plotOutput("pyr"),
                                     
                                     
                              ),
                              column(4,
                                     div(style = "float: right;",
                                         actionButton("toggle_population_cards", "Dependency Indicators", icon = icon("eye")),
                                         br(), br(),
                                         shinyjs::hidden(
                                           div(id = "population_card_box",
                                               div(
                                                 style = "background-color: #f7f7f7; padding: 15px; border-radius: 12px; box-shadow: 0 4px 8px rgba(0,0,0,0.1);",
                                                 uiOutput("depRatioCard"),
                                                 uiOutput("childdepRatioCard"),
                                                 uiOutput("OldagedepRatioCard")
                                               )
                                           )
                                         )
                                     )
                              )
                            ),
                            div(
                              style = "width: 90%;overflow-x: auto;overflow-y: auto;max-height: 400px;border: 1px solid #ddd;
                                             padding: 15px;margin-top: 15px;background-color: white;
                                             box-shadow: 0 2px 6px rgba(0,0,0,0.1); ",
                              tableOutput("tottable")
                            )
                   ),#tabPanel1
                   
        )#navbarpage
      )#mainPanel
    )#sidebarLayout
  })
  
  
  # Population Change -------------------------------------------------------
  #input 
  {
    
    iscenX <- reactive({input$Scenario})
    
    iprov <- reactive({input$prov})
    idist <- reactive({xx<-input$dist
    if(xx =="All")  xx<-iprov()
    xx
    })
    imuni <- reactive({xx<-input$muni
    if(xx =="All")  xx<-idist()
    xx
    })
    iward <- reactive({xx<-input$ward
    if(xx =="All")  xx<-imuni()
    xx
    })
    iTime <- reactive({
      if (input$tabselected == "Population Change") {
        input$Time_pop
      } else {
        input$Time_others
      }
    })
    
    iminAge <- reactive({as.numeric(input$minAge)})
    imaxAge <- reactive({as.numeric(input$maxAge)})
    
    iIndi <- reactive({ yyy <-measure.vars[input$indicator]
    names(yyy) = NULL
    yyy
    })
    
    #Area Name
    iarea <- reactive({
      if(input$dist!="All"){
        if(input$muni!="All"){
          if(input$ward!="All"){#ward
            paste(input$muni,"ward",input$ward)
          } else {#muni
            paste(input$muni)
          }
        } else {#dist
          input$dist
        }
      } else {
        input$prov
      }
      
    })
    iareanum <- reactive({
      
      xx <- if(input$dist!="All"){
        if(input$muni!="All"){
          unique(region.id[dname==input$dist&GaPa_NaPa==input$muni,distmuni*100])
        } else {#dist
          areas[dname==input$dist,dist]
        }
      } else {#Nepal/Provname
        areas[dname==input$prov,dist]
      }
      
      xx
    })
    
    
    seldist <- reactive({ 
      selecteddist = prov.dist.list[provname==iprov(),dname]
      selecteddist
    })
    selmuni <- reactive({ 
      selectedmuni = dist.muni.list[dname==idist(),GaPa_NaPa]
      selectedmuni
    })
    selward <- reactive({ 
      selectedward = region.id[dname==idist()&GaPa_NaPa==imuni(),ward]
      
      selectedward
    })
    
    
    observeEvent(input$prov, {
      updateSelectInput(session, "dist", 
                        choices =c("All", seldist()),
                        selected = "All"
      )
    })
    observeEvent(input$dist, {
      updateSelectInput(session, "muni", 
                        choices =c("All", selmuni()),
                        selected = "All"
      )
    })
    observeEvent(input$muni, {
      updateSelectInput(session, "ward", 
                        choices =c("All", selward()),
                        selected = "All"
      )
    })
    
    iSelvars <- reactive({
      yyy <- c(id.cols,measure.vars[input$indicator])
      names(yyy)=NULL
      # print(yyy)
      
      yyy})
  }
  
  
  
  ##read data
  { 
    global <- reactiveValues(data = NULL) #ignore - collect new data from scenario Run
    
    #reactive to imuni()
    data1 <- reactive({
      
      # print(iarea())
      print("data1 starts")
      print(iareanum())
      
      xx = NULL #output from selected scenarios 
      setDT(xx)
      
      for(i in iscenX()){ #only one scenario at a time
        # print(dist.num)
        if(T){# dist.num="10000"
          
          #choose scneario 
          
          #n21 #district
          
          #n41 #palika
          iscen.code <- c("n22","n23","n24")[grep(i,scens)]
          
          ifoldername <- grep(iscen.code,dir("../data/output",full.names = T),value = T)
          # print(ifoldername)
          
          #n01 #ward
          
          if(input$ward!="All"){
            inpfile = paste0(ifoldername,"/region",
                             paste0(iareanum(),"ww"),".feather")
            
            
            
            xxx <- read_feather(inpfile)
            
            xxx <- copy(xxx)[,ward:=(region%%100)][ward==input$ward]
            
          } else {
            
            inpfile = paste0(ifoldername,"/region",iareanum(),".feather")
            
            xxx <- read_feather(inpfile)
            
          }
          xxx <- copy(xxx)[,scen:=i]
          
          xx <- data.table::rbindlist(list(xx, xxx), use.names = TRUE, fill = TRUE)
        } else {
          print("prepare a nepal file and province files")
          s3BucketName <- "nepalpopward"
          s3File <- paste0("datadist/dist",dist.num/10000,".feather") 
          
          
          xx <- s3read_using(FUN = read_feather, 
                             object = s3File,
                             bucket = s3BucketName) #reads as a CSV or data.frame
          setDT(xx) #convert to data.table
        }      
        
        
      }#end of iscenX() 
      
      if(!is.null(global$data)){ #after scenario run
        
        print("I am in global data")
        #iareanum() = district
        if(input$dist!="All"){
          yy <- global$data[region==iareanum()][,region:=NULL]
        } else if (input$prov!="Nepal"){
          #iareanum() = province
          print("I am in Province")
          sel.dists = prov.dist.id.proj[prov==iareanum(),dist]
          print(sel.dists)
          print(names(global$data))
          yy <- global$data[region%in%sel.dists][,region:=NULL]
          
          
          sum_cols <- setdiff(names(yy),c("scen","Time","sex","agest"))
          print(sum_cols)
          
          yy <- yy[,by=.(scen,Time,sex,agest),lapply(.SD, sum, na.rm = TRUE), .SDcols = sum_cols]
          print(yy)
        } else {
          #iareanum() = Nepal
          print("I am in Nepal")
          yy <- copy(global$data)[,region:=NULL]
          sum_cols <- setdiff(names(yy), c("scen","Time","sex","agest"))
          yy <- yy[,by=.(scen,Time,sex,agest),lapply(.SD, sum, na.rm = TRUE), .SDcols = sum_cols]
        }
        print(yy)
        
        data.table::setDT(yy)
        zz <- data.table::rbindlist(list(xx, yy), use.names = TRUE, fill = TRUE)
        
      } else {
        # print(names(xx))
        zz <- xx 
      }
      zz
    }) #collect data by municipality
    
    
    #reacts to changing iSelvars
    data2 <- reactive({
      #iscenX = "SSP2"
      # print("data2")
      
      xxx <- data1()%>%select(iSelvars()) #[#,..iSelvars()#.(Time,scen,region,agest,sex,edu,pop)#iSelvars()#.(region,Time,scen,sex,agest,edu,)
      xxx <- xxx[,setnames(.SD,iIndi(),"value")]#[,name:=region]
      
      
      if(iIndi()!="pop") {xxx[Time < max(Time)]} else (xxx)
      
    }) #collect data
    
    
    # Births animated line plot
    databirths <- reactive({
      df <- data1()
      req("births" %in% colnames(df))
      df[, .(Time, scen, agest, sex, births)]
    })
    
    # print(databirths)
    
    
    # deaths animated line plot
    datadeaths <- reactive({
      df <- data1()
      req("deaths" %in% colnames(df))
      df[, .(Time, scen, agest, sex, deaths)]
    })
  }
  
  
  {#population change
    # Pop change --------------------------------------------------------------
    #Population Change Plot
    #reactive functions for the plots
    plottotline <- reactive({
      
      dtx <- data2()[agest >= input$ageRange_totline[1] & agest <= input$ageRange_totline[2] & 
                       Time %in% seq(min(selected_time()), max(selected_time()), by = 1),
                     by = .(scen, Time),
                     .(value = sum(value))]
      
      year_breaks <- if (max(selected_time()) == 2051)
      { seq(2021, 2051, by = 10)}
      else
      {seq(min(selected_time()),max(selected_time()),by=1)}
      
      p<- dtx %>%
        ggplot(aes(x = Time, y = value, col = scen)) +
        geom_line() + 
        geom_point(aes(text = paste("Value:", round(value))))+
        scale_x_continuous(breaks = year_breaks)+
        ggtitle(paste(iarea(), measure.vars.nm[iIndi()]))+ #add names  
        # theme(legend.position = "none")+
        labs(
          title = paste("Projected", iarea(), measure.vars.nm[iIndi()]),
          x = "Year",
          y = "Number",
          color = "Scenario" 
        ) +
        theme_minimal() +
        theme(
          plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
          legend.title = element_text(size = 12, face = "bold"),
          legend.text = element_text(size = 11),
          legend.position = "right"  
        )
      ggplotly(p, tooltip = "text")
    })
    
    output$totline <- renderPlotly({ plottotline()}) 
    
    #plot download code
    output$downloadPlottotline <- downloadHandler(
      filename = function() {
        paste("Total-",iarea(), input$indicator, Sys.Date(), ".png", sep = "")
      },
      content = function(file) {
        ggsave(file, plot =plottotline(), device = "png")
      }
    )
    # Download CSV handler
    output$downloadDataTotline <- downloadHandler(
      filename = function() {
        paste0(input$indicator, "-TotlineData-", Sys.Date(), ".csv")
      },
      content = function(file) {
        selectedData <- data2()[
          agest >= input$ageRange_totline[1] & agest <= input$ageRange_totline[2],
          .(value = sum(value)),
          by = .(scen, Time)
        ]
        selectedData[, indicator := input$indicator]
        setcolorder(selectedData, c("indicator", "scen", "Time", "value"))
        fwrite(selectedData, file)
      }
    )
    
    #pyramid
    plotpyr <- reactive({
      
      figval = data2()[value>0][Time%in%iTime()
      ][,.(Time,scen,agest,sex,value)
      ][,value:=value/1000]#[,scen:="Medium Scenario"]
      
      gg1 <- funpyrwrapper.x(res1 = figval,
                             # ivar="pop",
                             iarea =iareanum(),#,iarea(),
                             iindi = iIndi(), #
                             iTime=iTime(),
                             ititle.text  = "size",
                             iages = NULL, #default
                             iiscen= unique(figval$scen),
                             iscale = 1,#in Millions
                             facet.scale = "free")+
        ggtitle(paste(iarea()," ", measure.vars.nm[iIndi()]))
      gg1
    })
    
    output$pyr <- renderPlot({ plotpyr()}) #pyramid 
    
    #Download pyramid
    output$downloadPlotpyr <- downloadHandler(
      filename = function() {
        paste("Pyramid-",iarea(), input$indicator,Sys.Date(), ".png", sep = "")
      },
      content = function(file) {
        ggsave(file, plot = plotpyr(), device = "png")
      }
    )
    
    # tottable
    output$tottable <- renderTable({
      a<-data1()
      req(nrow(a)>0)
      
      id.cols.here = c("Time","sex","agest","scen") 
      vars = setdiff(names(a),id.cols.here)
      
      print(vars)
      final.summ.temp <- a[ Time %in% seq(2021, 2051, by = 1),
                            lapply(.SD, sum, na.rm = TRUE),
                            .SDcols = vars,
                            by = .(Time, scen)
      ][
        , pop := pop - births
      ][
        , pop1 := NULL
      ]#births are already in 'pop'
      #Drop coloumns if zero
      # Remove columns if absemi sum is zero
      if (final.summ.temp[, sum(absemi, na.rm = TRUE)] == 0) {
        cols_to_remove <- intersect(c("edutran", "absemi", "imm"), names(final.summ.temp))
        final.summ.temp[, (cols_to_remove) := NULL]
      }
      
      # Remove Idom and Odom if province is Nepal
      if (input$prov == "Nepal") {
        cols_to_remove_nepal <- intersect(c("idom", "odom"), names(final.summ.temp))
        final.summ.temp[, (cols_to_remove_nepal) := NULL]
      }
      
      # Initialize output list
      output.table <- list(
        Time = sprintf("%.0f", final.summ.temp$Time),
        Scenario=final.summ.temp$scen,
        Population = sprintf("%.0f", final.summ.temp$pop),
        Births = sprintf("%.0f", final.summ.temp$births),
        Deaths = sprintf("%.0f", final.summ.temp$deaths)
      )
      
      # Add optional columns only if they exist
      if ("popabs" %in% names(final.summ.temp)) {
        output.table$Absentees <- sprintf("%.0f", final.summ.temp$popabs)
      }
      if ("absemi" %in% names(final.summ.temp)) {
        output.table$AbsEmi <- sprintf("%.0f", final.summ.temp$absemi)
      }
      if ("imm" %in% names(final.summ.temp)) {
        output.table$Imm <- sprintf("%.0f", final.summ.temp$imm)
      }
      if ("ret" %in% names(final.summ.temp)) {
        output.table$Returnees <- sprintf("%.0f", final.summ.temp$ret)
      }
      if ("idom" %in% names(final.summ.temp)) {
        output.table$Dom_In <- sprintf("%.0f", final.summ.temp$idom)
      }
      if ("odom" %in% names(final.summ.temp)) {
        output.table$Dom_Out <- sprintf("%.0f", final.summ.temp$odom)
      }
      
      # Convert to data.table
      as.data.table(output.table)
    })
    
    dependency_ratio <- reactive({
      req(data1())
      
      dr <- data1()[agest >= 0, .(
        children = sum(pop[agest <= 14], na.rm = TRUE),
        working  = sum(pop[agest >= 15 & agest <= 64], na.rm = TRUE),
        elderly  = sum(pop[agest >= 65], na.rm = TRUE)
      ), by = .(Time, scen)]
      
      dr[, `:=`(
        total_dep_ratio = round((children + elderly) / working * 100, 1),
        child_dep_ratio = round(children / working * 100, 1),
        old_dep_ratio   = round(elderly / working * 100, 1)
      )]
      
      dr
    })
    output$depRatioCard <- renderUI({
      df <- dependency_ratio() %>% filter(Time%in%selected_time())
      req(nrow(df) > 0)
      
      div(
        style = "background-color: #2980b9; color: white; 
           padding: 10px 15px; border-radius: 10px;
           margin-bottom: 15px; width: fit-content;
           text-align: left; min-width: 140px;",
        
        tags$div(
          style = "font-size: 15px; font-weight: bold; margin-bottom: 6px;",
          "Dependency Ratio"
        ),
        
        tagList(
          df %>%
            split(.$scen) %>%
            lapply(function(group) {
              scen_name <- unique(group$scen)
              tagList(
                tags$div(style = "font-weight: bold; font-size: 13px; margin-top: 5px;",
                         paste("Scenario:", scen_name)),
                lapply(1:nrow(group), function(i) {
                  tags$div(
                    style = "font-size: 13px;",
                    paste0("Year ", group$Time[i], ": ", group$total_dep_ratio[i])
                  )
                })
              )
            })
        )
      )
    })
    output$childdepRatioCard <- renderUI({
      df <- dependency_ratio() %>% filter(Time%in%selected_time())
      req(nrow(df) > 0)
      
      div(
        style = "background-color: #2980b9; color: white; 
           padding: 10px 15px; border-radius: 10px;
           margin-bottom: 15px; width: fit-content;
           text-align: left; min-width: 140px;",
        
        tags$div(
          style = "font-size: 15px; font-weight: bold; margin-bottom: 6px;",
          "Child Dependency Ratio"
        ),
        
        tagList(
          df %>%
            split(.$scen) %>%
            lapply(function(group) {
              scen_name <- unique(group$scen)
              tagList(
                tags$div(style = "font-weight: bold; font-size: 13px; margin-top: 5px;",
                         paste("Scenario:", scen_name)),
                lapply(1:nrow(group), function(i) {
                  tags$div(
                    style = "font-size: 13px;",
                    paste0("Year ", group$Time[i], ": ", group$child_dep_ratio[i])
                  )
                })
              )
            })
        )
      )
    })  
    output$OldagedepRatioCard <- renderUI({
      df <- dependency_ratio() %>% filter(Time%in%selected_time())
      req(nrow(df) > 0)
      
      div(
        style = "background-color: #2980b9; color: white; 
           padding: 10px 15px; border-radius: 10px;
           margin-bottom: 15px; width: fit-content;
           text-align: left; min-width: 140px;",
        
        tags$div(
          style = "font-size: 15px; font-weight: bold; margin-bottom: 6px;",
          "Elderly Dependency Ratio"
        ),
        
        tagList(
          df %>%
            split(.$scen) %>%
            lapply(function(group) {
              scen_name <- unique(group$scen)
              tagList(
                tags$div(style = "font-weight: bold; font-size: 13px; margin-top: 5px;",
                         paste("Scenario:", scen_name)),
                lapply(1:nrow(group), function(i) {
                  tags$div(
                    style = "font-size: 13px;",
                    paste0("Year ", group$Time[i], ": ", group$old_dep_ratio[i])
                  )
                })
              )
            })
        )
      )
    })
  }#pop changes
}

shinyApp(ui, server)
