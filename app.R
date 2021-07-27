#########################################################
#LIBRERias                                              #
#########################################################
library(readxl)
library(tibble)
library(caret)
library(lattice)
library(base)
library(stringr)
library(writexl)
library(tm)
library(readr)
library(stats)
library(graphics)
library(grDevices)
library(utils)
library(datasets)
library(methods)
library(base)
library(dplyr)
library(ggplot2)
library(janitor)
library(tidytext)
library(tidyr)
library(arules)
library(Matrix)
library(arulesViz)
library(grid)
library(readxl)
library(reshape)
library(rfm)
library(tm)
library(NLP)
library(stats)
library(datetime)
library(lubridate)
library(shiny)
library(shinythemes)
library(quantmod)
library(tidyverse)
library(RPostgreSQL)
library(DBI)
library(jsonlite)
library(googleCloudStorageR)
library(xgboost)

#-----------------------------------------------------------

###########################################################################
#         EXTIENDE EL TAMAÑO DEL ARCHIVO A CARGAR EN SHINY:               #
###########################################################################

options(shiny.maxRequestSize=100*1024^2)

shinyApp(
  
  #########################################################################################################################################################
  #                                                                                                                                                       #
  #                                                               INICIO PARTE: ui                                                                        #
  #                                                                                                                                                       #
  #########################################################################################################################################################
  
  ui <- tagList(
    
    ##################################################
    #         TITULO Y TEMA DE LA APP:               #
    ##################################################
    
    #fluidPage(  
    navbarPage("SEGMENTACIÓN DE CLIENTES",
               
               # color app
               theme = shinytheme("flatly"),
               
               
               ###############################################################################################################################################
               #                                 INICIO PRIMERA PESTANA PRINCIPAL:  INGRESO DE DATOS                                                         #
               ###############################################################################################################################################
               
               ##################################################
               #           TITULO PRIMERA PESTANA:              #
               ##################################################
               
               tabPanel("INGRESO DE DATOS",
                        
                        #sidebarLayout(
                        
                        ##################################################
                        #    TITULO CONTENIDO PRIMERA PESTANA:           #
                        ##################################################
                        
                        titlePanel("1. Revisión general de los datos"),
                        
                        sidebarPanel(
                          
                          #########################################
                          #         CARGA DE ARCHIVO              #
                          #########################################
                          fileInput("file1", "Seleccionar el archivo a cargar:",
                                    multiple = TRUE,
                                    #accept = c("text/csv",
                                    #          "text/comma-separated-values,text/plain",
                                    #          ".csv")),
                                    accept = c(".xlsx")),
                          # Horizontal line ----
                          tags$hr(),
                          
                          #######################################################
                          #         CARGA DE MODELO DE EFECTIVIDAD              #
                          #######################################################
                          fileInput("file2", "Seleccionar el archivo de efectividad:",
                                    multiple = TRUE),
                          # Horizontal line ----
                          tags$hr(),

                          #######################################################
                          #             BOTÓN PARA CARGAR A AWS                 #
                          #######################################################
                          actionButton(inputId = "ejecutar01", label = "Cargar a AWS"),
                        
                          # Horizontal line ----
                          tags$hr()
                          
                        ),
                        #fin de sidebarPanel
                        
                        
                        ##################################################
                        #    TITULOS DE PESTANA Y DE SUS CONTENIDOS      #
                        ##################################################
                        
                        mainPanel(
                          #tableOutput("contents"),
                          
                          tabsetPanel(type = "tabs",
                                      tabPanel("Tabla de datos",
                                               h4("Visualización de datos cargados:"),DT::dataTableOutput("tabledata1")),
                                      tabPanel("Resumen de datos",
                                               h4("Estadísticos principales:"),verbatimTextOutput("tabledata2"))
                          )
                          
                          #dataTableOutput('horario'),
                          
                          # verbatimTextOutput("summary"),
                          
                          #tableOutput("view")
                          
                          
                          
                        )
                        #fin de mainPanel
                        
                        #)
                        #fin de sidebarLayout
               ),
               #fin de tabPanel
               
               
               ###############################################################################################################################################
               #                                    FIN PRIMERA PESTANA PRINCIPAL:  INGRESO DE DATOS                                                         #
               ###############################################################################################################################################
               
               
               ###############################################################################################################################################
               #                                 INICIO SEGUNDA PESTANA PRINCIPAL:  EFECTIVIDAD                                                        #
               ###############################################################################################################################################
               
               ##################################################
               #           TITULO SEGUNDA PESTANA:              #
               ##################################################  
               
               tabPanel("EFECTIVIDAD",
                        #sidebarLayout(
                        
                        ##################################################
                        #    TITULO CONTENIDO SEGUNDA PESTANA:           #
                        ##################################################        
                        # titulo app
                        titlePanel("2. Modelo de efectividad"),
                        
                        sidebarPanel(
                          
                          
                          #########################################
                          #         SELECCION DE VARX_30            #
                          #########################################
                          
                          selectInput(inputId = "VARX_30",
                                      label = "Variables para el modelo:",
                                      choices = c("CÁLCULO TOTAL DE UNIDADES","FECHA DE ACTIVACIÓN DE CONTRATO","PLAN DESTINO","CLUSTER MODELO","PROMEDIO DE RECARGA","RB_IG","PLAN DESTINO","RECICLADOS","REGIÓN"),
                                      selected = "TOTAL",
                                      multiple = FALSE)
                          #"GAP"
                          
                        ),
                        #fin de sidebarPanel
                        
                        ##################################################
                        #    TITULOS DE PESTANA Y DE SUS CONTENIDOS      #
                        ##################################################      
                        mainPanel(
                          
                          #tableOutput("contents"),
                          
                          tabsetPanel(type = "tabs",
                                      tabPanel("Base a segmentar",  
                                               h4("Tabla con variables agrupadas:"),DT::dataTableOutput("tabledata30")),
                                      tabPanel("Gráficos por variables agrupadas",
                                               h4("Diagrama de frecuencias de variables agrupadas:"),plotOutput("plot30")),
                                      tabPanel("Base segmentada",  
                                               h4("Tabla con variables de efectividad:"),DT::dataTableOutput("tabledata31")),
                                      tabPanel("Gráficos por variables de efectividad",
                                               h4("Diagrama de frecuencias de variables de efectividad:"),plotOutput("plot31"))
                          )
                          
                          #dataTableOutput('horario'),
                          
                          # verbatimTextOutput("summary"),
                          
                          #tableOutput("view")
                          
                          
                        )
                        #fin de mainPanel
                        
                        #)
                        #fin de sidebarLayout
               ),
               #fin de tabPanel
               
               ###############################################################################################################################################
               #                                    FIN SEGUNDA PESTANA PRINCIPAL:  ANALISIS DE DATOS                                                        #
               ###############################################################################################################################################
               
               
               ###############################################################################################################################################
               #                                INICIO TERCERA PESTANA PRINCIPAL:  MODELO DE SEGMENTACION                                                    #
               ###############################################################################################################################################
               
               ##################################################
               #          TITULO DE TERCERA PESTANA             #
               ##################################################
               
               tabPanel("BASE A GESTIONAR",
                        
                        
                        #sidebarLayout(
                        ##################################################
                        #   TITULO DE CONTENIDO TERCERA PESTANA          #
                        ##################################################
                        
                        titlePanel("3. Formato de carga"),
                        
                        sidebarPanel(
                          
                          downloadButton("downloadData", "Download")
                        
                          # Horizontal line ----

                        ),
                        #fin de sidebarPanel
 
                        ##################################################
                        #    TITULOS DE PESTANA Y DE SUS CONTENIDOS      #
                        ##################################################        
                        
                        mainPanel(
                          
                          #tableOutput("contents"),
                          
                          tabsetPanel(type = "tabs",
                                      tabPanel("Tabla de datos",  
                                               h4("Tabla de clientes y segmentos"),DT::dataTableOutput("tabledata50")),
                                      tabPanel("Resumen de segmentos de efectividad",  
                                               h4("R's por efectividad:"),DT::dataTableOutput("tabledata51"))
                          )
                          
                          #dataTableOutput('horario'),
                          
                          # verbatimTextOutput("summary"),
                          
                          #tableOutput("view")
                          
                          
                        )
                        #fin de mainPanel
                        
                        #)
                        #fin de sidebarLayout
               )
               #fin de tabPanel              
    )
    # fin de navbarPage
  ),
  
  #########################################################################################################################################################
  #                                                                                                                                                       #
  #                                                               FIN PARTE: ui                                                                           #
  #                                                                                                                                                       #
  #########################################################################################################################################################
  
  
  #-------------------------------------------------------------------------------
  
  
  #########################################################################################################################################################
  #                                                                                                                                                       #
  #                                                               INICIO PARTE: SERVER                                                                    #
  #                                                                                                                                                       #
  #########################################################################################################################################################
  
  server <- function(input, output) {
    
    ############################################################
    #               CARGANDO LA BASE AL TEMPORAL               #
    ############################################################     
    
    observeEvent(input$ejecutar01,{
      
      ############################################
      #          SUBIENDO DATA A AWS             #
      ############################################
      
      library("DBI")
      library("RMySQL")
      
      withProgress(message = 'Subiendo a AWS', value = 0, {
        
        for (i in 1:1) {
          if(i==1) {
            ############################################################       
            #                  RECALCULANDO VARIABLES                  #
            ############################################################  
            db2 <- dbConnect(RMySQL::MySQL(),
                             dbname = "movil",
                             host = "database-movil.catjbiapkswk.us-east-1.rds.amazonaws.com",
                             user = "user_movil",
                             password = rstudioapi::askForPassword("Database password"),
                             Port = 3306)
            dbExecute(db2, "TRUNCATE TABLE OUTBOUND_MIGRACION")
            dbWriteTable(conn = db2,"OUTBOUND_MIGRACION", value = datos2, append = TRUE, row.names = FALSE)
            
            incProgress(1/1, detail = paste("parte ", i))
            
            Sys.sleep(0.1)
          }
        }
      })
    })
    
    ##################################################
    #               GUARDANDO ARCHIVO                #
    ##################################################  
    
    datasetInput1 <- reactive({
      
      #cargamos el archivo
      inFile1 <- input$file1
      if (is.null(inFile1))
        return(NULL)
      
      library(readxl)
      #datos <- read_excel(inFile$datapath)
      #datos1 <- read.csv(inFile$datapath,sep=";")
      datos1 <- read_excel(inFile1$datapath)
      datos1 <- as.data.frame(datos1)
      
    })
    #fin de datasetInput1
    
    modeloVenta <- reactive({
      
      #cargamos el archivo
      inFile2 <- input$file2
      file2 <- inFile2$datapath
      
    })
    #fin de datasetInput1
    
    datasetInput2 <- reactive({
      
      datos2 <- as.data.frame(datasetInput1())
      
      ############################################
      #          RECALCULANDO VARIABLES          #
      ############################################
      
      datos2$TELEFONO <- as.factor(datos2$TELEFONO) ##9
      datos2$TELEFONO <- as.factor(paste("88",substring(datos2$TELEFONO,3,11),sep=""))
      
      datos2$RUCCOMPANIA <- as.factor(datos2$RUCCOMPANIA) ##8
      datos2$RUCCOMPANIA <- as.factor(str_pad(datos2$RUCCOMPANIA,8,pad="0"))
      
      datos2$TIPO_DE_IDENTIFICACION[nchar(as.character(datos2$RUCCOMPANIA)) == 8] <- 'DNI' ##49
      datos2$TIPO_DE_IDENTIFICACION[nchar(as.character(datos2$RUCCOMPANIA)) != 8] <- 'OTROS'
      datos2$TIPO_DE_IDENTIFICACION <- as.factor(datos2$TIPO_DE_IDENTIFICACION)
      
      datos2$FLAG_NG <- 0 ##50
      datos2$FLAG_NG <- as.factor(datos2$FLAG_NG)
      
      datos2$PLAN_DESTINO_1 <- as.factor(datos2$PLAN_DESTINO_1) ##29
      datos2$RB_IG <- as.factor(substring(datos2$PLAN_DESTINO_1,12,16))
      
      ###############################################
      #          AGREGANDO PERIODO_CARTERA          #
      ###############################################
      
      if (as.numeric(format(Sys.Date(),"%d")) >= 25) {
        datos2$PERIODO_CARTERA <- paste(as.character(format(Sys.Date(),"%Y")),if(as.numeric(format(Sys.Date(),"%m")) >= 10)
        { as.character(as.numeric(format(Sys.Date(),"%m")))
        } else {
          paste('0',as.character(as.numeric(format(Sys.Date(),"%m"))+1),sep="")
        }, sep="")
      } else {
        datos2$PERIODO_CARTERA <- paste(as.character(format(Sys.Date(),"%Y")),if(as.numeric(format(Sys.Date(),"%m")) >= 10)
        { as.character(as.numeric(format(Sys.Date(),"%m")))
        } else {
          paste('0',as.character(as.numeric(format(Sys.Date(),"%m"))),sep="")
        }, sep="") 
      }
      
      datos2$PERIODO_CARTERA <- as.factor(datos2$PERIODO_CARTERA) ##50

      ##########################################
      #          AGREGANDO ID_CLIENTE          #
      ##########################################
            
      datos2$ID_CLIENTE <- paste(datos2$PERIODO_CARTERA,'_',datos2$TELEFONO,'_1',sep="") ##51
      datos2$ID_CLIENTE <- as.factor(datos2$ID_CLIENTE)

      ##########################################
      #          AGREGANDO ID_CAMPANA          #
      ##########################################
            
      datos2$ID_CAMPANA <- 1 ##52
      datos2$ID_CAMPANA <- as.factor(datos2$ID_CAMPANA)
      
      ############################################################
      #          GENERANDO VARIABLES PARA MODELO_VENTAS          #
      ############################################################      
    
      ### VARIABLE CALC_TOTAL_UNIDADES_2 ##54
      datos2$CALC_TOTAL_UNIDADES_2[datos2$CALC_TOTAL_UNIDADES == 4 | datos2$CALC_TOTAL_UNIDADES == 5] <- '[4-5]'
      datos2$CALC_TOTAL_UNIDADES_2[is.na(datos2$CALC_TOTAL_UNIDADES_2)] <- '[OTRO]'
      datos2$CALC_TOTAL_UNIDADES_2 <- as.factor(datos2$CALC_TOTAL_UNIDADES_2)
      
      ### VARIABLE FECHAACTIVACIONCONTRATO_2 ##55
      datos2$FECHAACTIVACIONCONTRATO <- as.factor(str_pad(datos2$FECHAACTIVACIONCONTRATO,8,pad="0"))
      inicio <- paste(substring(datos2$PERIODO_CARTERA,1,4),'-',substring(datos2$PERIODO_CARTERA,5,6),'-','01', sep="")
      fin <- paste(substring(datos2$FECHAACTIVACIONCONTRATO,5,8),'-',substring(datos2$FECHAACTIVACIONCONTRATO,3,4),'-',substring(datos2$FECHAACTIVACIONCONTRATO,1,2), sep="")
      calculo <- as.data.frame(round((as.Date(inicio,"%Y-%m-%d") - as.Date(fin,"%Y-%m-%d"))/30,0))
      names(calculo) <- c('calculo')
      #calculo$FECHAACTIVACIONCONTRATO_2[calculo$calculo == 0] <- '[OTRO]'
      #calculo$FECHAACTIVACIONCONTRATO_2[calculo$calculo >= 1 & calculo$calculo <= 3] <- '[X<3 MESES]'
      calculo$FECHAACTIVACIONCONTRATO_2[calculo$calculo <= 3] <- '[X<3 MESES]'
      calculo$FECHAACTIVACIONCONTRATO_2[calculo$calculo >= 4 & calculo$calculo <= 6] <- '[4-6 MESES]'
      calculo$FECHAACTIVACIONCONTRATO_2[calculo$calculo >= 7 & calculo$calculo <= 12] <- '[7-12 MESES]'
      calculo$FECHAACTIVACIONCONTRATO_2[calculo$calculo >= 13 & calculo$calculo <= 24] <- '[13-24 MESES]'
      calculo$FECHAACTIVACIONCONTRATO_2[calculo$calculo >= 25 & calculo$calculo <= 36] <- '[25-36 MESES]'
      calculo$FECHAACTIVACIONCONTRATO_2[calculo$calculo > 36] <- '[X>36 MESES]'
      calculo$FECHAACTIVACIONCONTRATO_2[is.na(calculo$FECHAACTIVACIONCONTRATO)] <- '[OTRO]'
      calculo$FECHAACTIVACIONCONTRATO_2 <- as.factor(calculo$FECHAACTIVACIONCONTRATO_2)
      
      datos2 <- cbind(datos2,calculo$FECHAACTIVACIONCONTRATO_2)
      names(datos2)[55] = "FECHAACTIVACIONCONTRATO_2"
      
      ### VARIABLE PLAN_TARIFARIO_2 ##56
      datos2$PLAN_TARIFARIO_2[datos2$PLAN_TARIFARIO == 'Desconocido' | datos2$PLAN_TARIFARIO == 'Plan Demo'] <- '[DESCONOCIDO-DEMO]'
      datos2$PLAN_TARIFARIO_2[datos2$PLAN_TARIFARIO == 'Entel Prepago' | datos2$PLAN_TARIFARIO == 'Entel Prepago Power 5'] <- '[PREPAGO+POWER]'
      datos2$PLAN_TARIFARIO_2[datos2$PLAN_TARIFARIO == 'Prepago Chip 29'] <- '[PREPAGO CHIP29]'
      datos2$PLAN_TARIFARIO_2[is.na(datos2$PLAN_TARIFARIO_2)] <- '[OTROS]'
      datos2$PLAN_TARIFARIO_2 <- as.factor(datos2$PLAN_TARIFARIO_2)
      
      ### VARIABLE CLUSTER_MODELO_2 ##57
      datos2$CLUSTER_MODELO_2[datos2$CLUSTER_MODELO == 'Grupo 1' | datos2$CLUSTER_MODELO == 'Grupo 4'] <- '[GRUPO 1 Y 4]'
      datos2$CLUSTER_MODELO_2[datos2$CLUSTER_MODELO == 'Grupo 2'] <- '[GRUPO 2]'
      datos2$CLUSTER_MODELO_2[datos2$CLUSTER_MODELO == 'Grupo 3'] <- '[GRUPO 3]'
      datos2$CLUSTER_MODELO_2[is.na(datos2$CLUSTER_MODELO)] <- '[OTRO]'
      datos2$CLUSTER_MODELO_2 <- as.factor(datos2$CLUSTER_MODELO_2)
      
      ### VARIABLE PROM_REC ##27
      datos2$PROM_REC <- as.numeric(datos2$PROM_REC)
      datos2$PROM_REC[is.na(datos2$PROM_REC)] <- 0
      
      ### VARIABLE RB_IG ##30
      datos2$RB_IG <- as.numeric(datos2$RB_IG)
      
      ### VARIABLE GAP2 ##37
      datos2$GAP2[is.na(datos2$PROM_REC)] <- 0
      datos2$GAP2 <- datos2$PROM_REC - datos2$RB_IG
      
      ### VARIABLE PLAN_DESTINO_2 ##59
      datos2$PLAN_DESTINO_3[datos2$PLAN_DESTINO_1 == 'Entel Chip 20.90 REV'] <- '[GRUPO1]'
      datos2$PLAN_DESTINO_3[is.na(datos2$PLAN_DESTINO_3)] <- '[GRUPO2]'
      datos2$PLAN_DESTINO_3 <- as.factor(datos2$PLAN_DESTINO_3)
      
      ### VARIABLE RECICLADOS_2 ##60
      inicio1 <- paste(substring(datos2$PERIODO_CARTERA,1,4),'-',substring(datos2$PERIODO_CARTERA,5,6),'-','01', sep="")
      fin1 <- datos2$RECICLADOS
      fin1[is.na(fin1)] <- paste('9999',substring(datos2$PERIODO_CARTERA,5,6), sep="")
      fin1 <- paste(substring(fin1,1,4),'-',substring(fin1,5,6),'-','01', sep="")
      calculo1 <- as.data.frame(round((as.Date(inicio1,"%Y-%m-%d") - as.Date(fin1,"%Y-%m-%d"))/30,0))
      names(calculo1) <- c('calculo1')
      calculo1$RECICLADOS_2[calculo1$calculo1 < 0] <- '[NUEVO]'
      calculo1$RECICLADOS_2[calculo1$calculo1 == 0] <- '[OTROS]'
      calculo1$RECICLADOS_2[calculo1$calculo1 >= 1 & calculo1$calculo1 <= 6] <- '[1-6 MESES]'
      calculo1$RECICLADOS_2[calculo1$calculo1 > 6] <- '[7 A MAS MESES]'
      calculo1$RECICLADOS_2 <- as.factor(calculo1$RECICLADOS_2)
      
      datos2 <- cbind(datos2,calculo1$RECICLADOS_2)
      names(datos2)[60] = "RECICLADOS_2"
      
      ### VARIABLE REGION ##48
      datos2$REGION <- as.factor(datos2$REGION)
      
      ### VARIABLE VERSION_MODELO ##61
      datos2$VERSION_MODELO <- 'V1'
      datos2$VERSION_MODELO <- as.factor(datos2$VERSION_MODELO)
      
      datos2
      
    })
    #fin de datasetInput1
    
    datasetInput3 <- reactive({
      
      datos3 <- as.data.frame(datasetInput2())
      datos3 <- select(datos3,ID_CLIENTE,PERIODO_CARTERA,ID_CAMPANA,REGION,CALC_TOTAL_UNIDADES_2,FECHAACTIVACIONCONTRATO_2,CLUSTER_MODELO_2,RECICLADOS_2,PLAN_DESTINO_3,GAP2,RB_IG,PROM_REC)
      names(datos3)[9] = "PLAN_DESTINO_2"
      
      datos3
      
    })
    #fin de datasetInput3
    
    datasetInput4 <- reactive({
      
      datos4 <- as.data.frame(datasetInput2())
      datos4 <- select(datos4,ID_CLIENTE,PERIODO_CARTERA,ID_CAMPANA,REGION,FECHAACTIVACIONCONTRATO,PLAN_TARIFARIO,CLUSTER_MODELO,PLAN_DESTINO_1,RECICLADOS_3)
      names(datos4)[9] = "RECICLADOS"
    
      datos4
      
    })
    #fin de datasetInput4

    datasetInput5 <- reactive({
    
      datos5 <- as.data.frame(datasetInput2())
      which(colSums(is.na(datos5))!=0)#buscar nulls
      datos5 <- select(datos5,ID_CLIENTE,PERIODO_CARTERA,ID_CAMPANA,TELEFONO,COMPANIA,RUCCOMPANIA,REGION,CALC_TOTAL_UNIDADES_2,FECHAACTIVACIONCONTRATO_2,CLUSTER_MODELO_2,RECICLADOS_2,PLAN_DESTINO_3,GAP2,RB_IG,PROM_REC)
      names(datos5)[12] = "PLAN_DESTINO_2"
      
      #############################################################
      #     GENERAMOS CLUSTER PARA VARIABLES CUANTITATIVAS        #
      ############################################################# 
      
      cluster<-data.frame(GAP2=datos5$GAP2,RB_IG=datos5$RB_IG,PROM_REC=datos5$PROM_REC)
      res<-kmeans(scale(cluster),3)
      datos5_1<-cbind(datos5,cluster_cuanti=res$cluster)

      ###############################################################
      #     FORMATEAMOS LAS VARIABLES QUE ENTRARAN AL MODELO        #
      ############################################################### 
            
      datos5_2<-data.frame(REGION=datos5_1$REGION,
                             CALC_TOTAL_UNIDADES_2=datos5_1$CALC_TOTAL_UNIDADES_2,
                             FECHAACTIVACIONCONTRATO_2=datos5_1$FECHAACTIVACIONCONTRATO_2,
                             CLUSTER_MODELO_2=datos5_1$CLUSTER_MODELO_2,
                             RECICLADOS_2=datos5_1$RECICLADOS_2,
                             PLAN_DESTINO_2=datos5_1$PLAN_DESTINO_2,
                             cluster_cuanti=datos5_1$cluster_cuanti)

      #################################
      #     CARGAMOS EL MODELO        #
      #################################
            
      #setwd("Z:/Staff/Gerencia de Calidad/Compartido Calidad/12 - Business analytics/4.-QUERYS/QUERYS ENTEL_PERU_MIGRACIONES/R/")
      #load("modeloventa_entel_migra_v2")
      #load(InputModVenta())
      #summary(modelo1)
      load(modeloVenta())
      
      #########################################
      #     GENERAMOS LAS PREDICCIONES        #
      #########################################
            
      datos5_2$pred1_prob<- predict(modelo1,datos5_2,type="response")
      datos5_2$pred1_Clas<- ifelse(datos5_2$pred1_prob>0.5,"1","0")
      datos5_2$pred1_Segm<- ifelse(datos5_2$pred1_prob>0.77,"R1",ifelse(datos5_2$pred1_prob<0.6530,"R3","R2"))

      datos5_2<-data.frame(TELEFONO=datos5$TELEFONO,
                                PERIODO_CARTERA=datos5$PERIODO_CARTERA,
                                NOMBRE_CLIENTE=datos5$COMPANIA,
                                DOCUMENTO=datos5$RUCCOMPANIA,
                                #PROBVENT=datos5_2$pred1_prob,
                                #VENTA=datos5_2$pred1_Clas,
                                SEGMVENTA=datos5_2$pred1_Segm)
                                #PROBCONTACT=round(pred2_prob,3),
                                #CONTAC=pred2_Clas,
                                #SEGMCONT=pred2_Segm)
      
      datos5_2
      
      #names(datos36) <- c("TELÉFONO","PERIODO CARTERA","NOMBRE CLIENTE","DOCUMENTO","SEGMENTO DE VENTAS")
      
    })
    #fin de datasetInput5
    
    datasetInput7 <- reactive({
    
      datos7_1 <- data.frame(datasetInput2())
      datos7_2 <- data.frame(datasetInput5())
      #datos7_3 <- data.frame(datasetInput6())
    
      datos7_4 <- data.frame(CORRELATIVO=datos7_1$CORRELATIVO,
                             CALC_TOTAL_UNIDADES=datos7_1$CALC_TOTAL_UNIDADES,
                             PRODUCTO=datos7_1$PRODUCTO,
                             CODIGO_COMPANIA=datos7_1$CODIGO_COMPANIA,
                             TIPOCOMPANIA=datos7_1$TIPOCOMPANIA,
                             COMPANIA=datos7_1$COMPANIA,
                             TIPOCUENTA=datos7_1$TIPOCUENTA,
                             RUCCOMPANIA=datos7_1$RUCCOMPANIA,
                             TELEFONO=datos7_1$TELEFONO,
                             ESTADOCONTRATO=datos7_1$ESTADOCONTRATO,
                             FECHAACTIVACIONCONTRATO=datos7_1$FECHAACTIVACIONCONTRATO,
                             CODIGOCONTRATOBSCS=datos7_1$CODIGOCONTRATOBSCS,
                             CICLOFACTURACION=datos7_1$CICLOFACTURACION,
                             PLAN_TARIFARIO=datos7_1$PLAN_TARIFARIO,
                             TIPO_PLAN=datos7_1$TIPO_PLAN,
                             CLUSTER_MODELO=datos7_1$CLUSTER_MODELO,
                             COSTO_MIN_NET=datos7_1$COSTO_MIN_NET,
                             COSTO_MIN_TFIJO=datos7_1$COSTO_MIN_TFIJO,
                             COSTO_MIN_TMOV=datos7_1$COSTO_MIN_TMOV,
                             COSTO_SMS=datos7_1$COSTO_SMS,
                             COSTO_MB=datos7_1$COSTO_MB,
                             PROM_CONS_CD=datos7_1$PROM_CONS_CD,
                             PROM_CONS_IXON=datos7_1$PROM_CONS_IXON,
                             PROM_CONS_IXOFF=datos7_1$PROM_CONS_IXOFF,
                             PROM_CONS_IXTOT=datos7_1$PROM_CONS_IXTOT,
                             PROM_CONS_MB=datos7_1$PROM_CONS_MB,
                             PROM_REC=datos7_1$PROM_REC,
                             GAP_1=datos7_1$GAP_1,
                             PLAN_DESTINO_1=datos7_1$PLAN_DESTINO_1,
                             RB_IG=datos7_1$RB_IG,
                             BUCKET_ONNET=datos7_1$BUCKET_ONNET,
                             BUCKET_OFFNET=datos7_1$BUCKET_OFFNET,
                             BUCKET_MB=datos7_1$BUCKET_MB,
                             BUCKET_SMS=datos7_1$BUCKET_SMS,
                             MOD_VENTA=datos7_1$MOD_VENTA,
                             EQUIP_OFREC=datos7_1$EQUIP_OFREC,
                             GAP_2=datos7_1$GAP_2,
                             PLAN_DESTINO_2=datos7_1$PLAN_DESTINO_2,
                             RB_IGV_PLANDESTINO_2=datos7_1$RB_IGV_PLANDESTINO_2,
                             BUCKET_ONNET_2=datos7_1$BUCKET_ONNET_2,
                             BUCKET_OFFNET_2=datos7_1$BUCKET_OFFNET_2,
                             BUCKET_MB_2=datos7_2$SEGMVENTA,
                             BUCKET_SMS_2=datos7_1$BUCKET_SMS_2,
                             CORREOCOMPANIA=datos7_1$CORREOCOMPANIA,
                             EQUIP_OFREC_2=datos7_1$EQUIP_OFREC_2,
                             RECICLADOS=datos7_1$RECICLADOS,
                             SAT_OK=datos7_1$SAT_OK,
                             REGION=datos7_1$REGION
    )
      
    })
    #fin de datasetInput7
    
    ###############################################################################################################################################
    #                                 INICIO PRIMERA PESTANA PRINCIPAL:  INGRESO DE DATOS                                                         #
    ###############################################################################################################################################  
    
    ##################################################
    #     PESTANA: TABLA DE DATOS- tabledata1        #
    ##################################################    
    
    output$tabledata1 <- DT::renderDataTable({
      
      DT::datatable(datasetInput1())
      
    })
    #fin de renderDataTable
    
    
    ##################################################
    #     PESTANA: RESUMEN DE DATOS- tabledata2      #
    ##################################################
    
    output$tabledata2 <- renderPrint({

      #summary(datasetInput1())
      summary(datasetInput1())

    })
    #fin de renderPrint
    
    
    ###############################################################################################################################################
    #                                 FIN PRIMERA PESTANA PRINCIPAL:  INGRESO DE DATOS                                                            #
    ###############################################################################################################################################
    
    
    ###############################################################################################################################################
    #                                 INICIO SEGUNDA PESTANA PRINCIPAL:  EFECTIVIDAD                                                              #
    ###############################################################################################################################################  
    
    ##################################################
    #PESTANA: ANNLISIS X CLIENTE- tabledata30        #
    ##################################################
    
    output$tabledata30 <- DT::renderDataTable({

      datos30 <- as.data.frame(datasetInput2())
      datos30 <- select(datos30,ID_CLIENTE,PERIODO_CARTERA,ID_CAMPANA,REGION,CALC_TOTAL_UNIDADES_2,FECHAACTIVACIONCONTRATO_2,CLUSTER_MODELO_2,RECICLADOS_2,PLAN_DESTINO_3,GAP2,RB_IG,PROM_REC)
      
      DT::datatable(datos30)
      
    })
    #fin de renderDataTable
    
    
    ##################################################
    #PESTANA: Gráficos X CLIENTE- plot30             #
    ##################################################
    
    output$plot30 <- renderPlot({

      datos32 <- as.data.frame(datasetInput2()) 
      datos32 <- select(datos32,REGION,CALC_TOTAL_UNIDADES_2,FECHAACTIVACIONCONTRATO_2,CLUSTER_MODELO_2,RECICLADOS_2,PLAN_DESTINO_3,GAP2,RB_IG,PROM_REC)
      names(datos32) <- c("REGIÓN","CÁLCULO TOTAL DE UNIDADES","FECHA DE ACTIVACIÓN DE CONTRATO","CLUSTER MODELO","RECICLADOS","PLAN DESTINO","GAP","RB_IG","PROMEDIO DE RECARGA")
      
      inputVARX_30 <- input$VARX_30
      
      if (inputVARX_30=="CÁLCULO TOTAL DE UNIDADES") {
        i30 <- 2
      } else if (inputVARX_30=="FECHA DE ACTIVACIÓN DE CONTRATO") {
        i30 <- 3
      } else if (inputVARX_30=="PLAN DESTINO") {
        i30 <- 6
      } else if (inputVARX_30=="CLUSTER MODELO") {
        i30 <- 4
      } else if (inputVARX_30=="PROMEDIO DE RECARGA") {
        i30 <- 9
      } else if (inputVARX_30=="RB_IG") {
        i30 <- 8
      } else if (inputVARX_30=="GAP") {
        i30 <- 7
      } else if (inputVARX_30=="RECICLADOS") {
        i30 <- 5
      } else if (inputVARX_30=="REGIÓN") {
        i30 <- 1
      }
      
      LBL <- names(datos32)
      
      datos33 = subset(datos32[,c(1,i30)])
      names(datos33) <- c("COMODIN","X")

      #HISTOGRAMA
      ggplot(datos33, aes(x=X)) +
        geom_bar(position = 'stack', stat = 'count') +
        labs(x=LBL[i30], y="FRECUENCIAS") +
        scale_fill_manual(values=c("#FF0033", "#3300FF","#00CC00","lightgreen","lightgoldenrod","#00CC00"))
      
    })

    
      ##################################################
      #PESTANA: ANNLISIS X CLIENTE- tabledata31        #
      ##################################################
    
      output$tabledata31 <- DT::renderDataTable({
      
      datos36 <- as.data.frame(datasetInput5())
      names(datos36) <- c("TELÉFONO","PERIODO CARTERA","NOMBRE CLIENTE","DOCUMENTO","SEGMENTO DE VENTAS")
      
      DT::datatable(datos36)
      
    })
    #fin de renderDataTable

    ##################################################
    #PESTANA: Gráficos X CLIENTE- plot31             #
    ##################################################
    
    output$plot31 <- renderPlot({
      
      datos39 <- as.data.frame(datasetInput5())
      datos040 = subset(datos39[,c(1,5)])
      names(datos040) <- c("COMODIN","X")
      
      #HISTOGRAMA
      ggplot(datos040, aes(x=X)) +
        geom_bar(position = 'stack', stat = 'count') +
        labs(x="Segmentos de efectividad", y="Frecuencias") +
        scale_fill_manual(values=c("#FF0033", "#3300FF","#00CC00","lightgreen","lightgoldenrod","#00CC00"))
      
    })

    ###############################################################################################################################################
    #                                 FIN SEGUNDA PESTANA PRINCIPAL:  ANALISIS DE DATOS                                                           #
    ###############################################################################################################################################      
    
  ###############################################################################################################################################
  #                                 INICIO TERCERA PESTANA PRINCIPAL:  MODELO DE SEGMENTACION                                                   #
  ############################################################################################################################################### 
  
    ##################################################
    #PESTANA: RESULTADOS- tabledata50                #
    ##################################################    
    
    output$tabledata50 <- DT::renderDataTable({
      
      DT::datatable(datasetInput7())
      
    })
    #fin de renderDataTable
    
    #############################################################
    ##DESCARGA: descarga de formato de carga                    #
    #############################################################
    
    output$downloadData <- downloadHandler(
      filename = function() {
        paste("FC_ENTEL_MIGRACIONES", ".csv", sep = "")
      },
      content = function(file) {
        write.csv(datasetInput7(), file, row.names = FALSE)
      }
      
      
      
    )
    
    ##################################################
    #PESTANA: RESULTADOS- tabledata51                #
    ##################################################    
    
    output$tabledata51 <- DT::renderDataTable({
      
      datos54 <- as.data.frame(datasetInput7())
      datos54 = subset(datos54[,c("CORRELATIVO","BUCKET_MB_2")])
      
      datos54$CONTEO <- 1 
      datos55<-aggregate(datos54$CONTEO,by=list(datos54$BUCKET_MB_2),FUN=sum)
      names(datos55) <- c("R EFECTIVIDAD","Q REGISTROS")
      
      DT::datatable(datos55)
      
    })
    #fin de renderDataTable
    
  #########################################################################################################################################################
  #                                                                                                                                                       #
  #                                                               FIN PARTE: SERVER                                                                       #
  #                                                                                                                                                       #
  #########################################################################################################################################################
}
)
# FIN DE SHINYAPP
