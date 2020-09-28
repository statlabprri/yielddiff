library(readr)
library(tidyverse)
library(broom)
library(shiny)
library(plotly)
library(Hmisc)
library(sf)
library(leaflet)



provlist<-read.csv("./provlist.csv")

ui <- fluidPage(
    
    navbarPage("Yield comparison: 2019DS RBFHS v 2020 DS RCEP",
               tabPanel("Yield Comparison",
    
    plotlyOutput("plot"),
                 
    fluidRow(
        column(3,
               wellPanel(
               selectInput("provx", "Province",c("ALL PROVINCES",provlist$province)),
            hr(),
            checkboxInput(inputId = "loss",
                          label="Exclude farmers who experienced production loss",
                          value=FALSE),
            hr(),
            helpText("Data from 2019DS RBFHS survey data and 2020 DS RCEP quick survey")
        )),
    column(9,
           tabsetPanel(tabPanel("Descriptive",plotlyOutput("tab")),
                tabPanel("T Test", plotlyOutput("tab2"))
            )
    )
    
        )),
    tabPanel("Yield Table",
        br(),
        fluidRow(
            column(6,
        wellPanel(
            checkboxInput(inputId = "loss2",
                          label="Exclude farmers who experienced production loss",
                          value=FALSE))),
        column(6,
               wellPanel(
                   selectInput("rp", "Group",c("Provincial","Regional"))
               )
        )),
        br(),
        tableOutput("ytab")),
    
    tabPanel("Yield Map",
          
          fluidRow(
              leafletOutput("ymap",height=1000),
              
              absolutePanel(id = "controls", fixed = TRUE,
                            draggable = TRUE, top = 100,bottom="auto", left="auto", right=300, width = "auto", height = "auto",
                            
                            selectInput("layer","Characterisitc",
                                        c("2019 DS RBFHS Baseline Average Yield"="X2019.DS.RBFHS",
                                          "2020 DS RCEP Monitoring Average Yield"="X2020.DS.RCEP",
                                          "Yield Difference (2020DS RCEP - 2019DS RBFHS"="Yielddiff",
                                          "T.Test of significant difference"="sigtest")))
            
          )
    ) 
    
)

)



server <- function(input, output, session) {
    
    observe({

    data<-read.csv("./metadata.csv")
    
    data2<-data%>%as_tibble()%>%
        mutate(prov=toupper(prov),
               id=1:n())%>%
        filter(RCEP_recip==1)%>%
        filter(planted2==1)%>%
        filter(YIELD_ed>0)
    
    
    bdata<-read.csv("./baseline.csv")%>%
        mutate(seedrate=as.numeric(seedrate))
    
    
    
    yielda<-bdata%>%
        filter(rcat!=2)%>%
        mutate(YIELD=as.numeric(YIELD),
               prov=case_when(Province=="Samar"~"SAMAR (WESTERN SAMAR)",
                             TRUE~toupper(Province)),
               ID=FID,
               type="base",
               PRODN=as.numeric(PRODN),
               Loss=case_when(ProductionLoss==1~1,
                              TRUE~0))%>%
        select(c(type,ID,prov,YIELD,PRODN,AreaHarvested,seedrate,Loss))%>%
        full_join(data2%>%
                      select(-YIELD)%>%
                      mutate(Loss=case_when(harvested==2~1,
                                            TRUE~0),
                             prov=case_when(prov=="COMPOSTELA VALLEY"~"DAVAO DE ORO",
                                            TRUE~prov),
                             PRODN=as.numeric(PRODKG),
                             AreaHarvested=as.numeric(harvested_area),
                             type="mon1",
                             YIELD=YIELD_ed/1000)%>%
                      rename(ID=id,
                             )%>%
                      select(type,ID,prov,YIELD,PRODN,AreaHarvested,seedrate,Loss))%>%
        left_join(provlist, by=c("prov"="province"))%>%
        dplyr::arrange(regid)
    
    yieldx<-
        if (input$loss) {
            yielda%>%filter(Loss==0)
        } else {
            yielda
        }
    

    yield<- if (input$provx=="ALL PROVINCES") {
        yieldx
      } else {
        yieldx%>%filter(prov==input$provx)
    }
    
    yieldx2<-
        if (input$loss2) {
            yielda%>%filter(Loss==0)
        } else {
            yielda
        }
    
if (input$rp=="Provincial"){
    yieldsum<-yieldx2%>%
        group_by(type,regid,region,prov)%>%
        dplyr::arrange(regid)%>%
        summarise(sample=n(),
                  totprod=sum(PRODN,na.rm=TRUE),
                  totareah=sum(AreaHarvested,na.rm=TRUE),
                  yield=mean(YIELD,na.rm = TRUE))%>%
        mutate(type=case_when(type=="base"~"2019 DS RBFHS",
                              type=="mon1"~"2020 DS RCEP"))%>%
        pivot_wider(names_from=c(type),
                    values_from=c(sample,totprod,totareah,yield))%>%
        mutate(`yield_2019 DS RBFHS`=case_when(prov=="QUEZON"~4.00417474066905,
                                               TRUE~`yield_2019 DS RBFHS`),
               `yield difference`=`yield_2020 DS RCEP`-`yield_2019 DS RBFHS`,
               `%change`=`yield difference`/`yield_2019 DS RBFHS`,
               changeinprod=case_when(`yield difference`<0~"decreased",
                                      `yield difference`>0~"increased",
                                      TRUE~"no change"))%>%
        full_join(yieldx2%>%
                      group_by(type)%>%
                      summarise(sample=n(),
                                totprod=sum(PRODN,na.rm=TRUE),
                                totareah=sum(AreaHarvested,na.rm=TRUE),
                                yield=mean(YIELD,na.rm = TRUE))%>%
                      mutate(type=case_when(type=="base"~"2019 DS RBFHS",
                                            type=="mon1"~"2020 DS RCEP"),
                             prov=" ",
                             region="PHILIPPINES")%>%
                      pivot_wider(names_from=c(type),
                                  values_from=c(sample,totprod,totareah,yield))%>%
                      mutate(`yield difference`=`yield_2020 DS RCEP`-`yield_2019 DS RBFHS`,
                             `%change`=`yield difference`/`yield_2019 DS RBFHS`,
                             changeinprod=case_when(`yield difference`<0~"decreased",
                                                    `yield difference`>0~"increased",
                                                    TRUE~"no change"))
        )%>%
        rename(PROVINCE=prov,
               REGION=region)%>%
        ungroup(regid)%>%
        select(-c(regid))
} else {
    yieldsum<-yieldx2%>%
        group_by(type,regid,region)%>%
        dplyr::arrange(regid)%>%
        summarise(sample=n(),
                  totprod=sum(PRODN,na.rm=TRUE),
                  totareah=sum(AreaHarvested,na.rm=TRUE),
                  yield=mean(YIELD,na.rm = TRUE))%>%
        mutate(type=case_when(type=="base"~"2019 DS RBFHS",
                              type=="mon1"~"2020 DS RCEP"))%>%
        pivot_wider(names_from=c(type),
                    values_from=c(sample,totprod,totareah,yield))%>%
        mutate(`yield difference`=`yield_2020 DS RCEP`-`yield_2019 DS RBFHS`,
               `%change`=`yield difference`/`yield_2019 DS RBFHS`,
               changeinprod=case_when(`yield difference`<0~"decreased",
                                      `yield difference`>0~"increased",
                                      TRUE~"no change"))%>%
        full_join(yieldx2%>%
                      group_by(type)%>%
                      summarise(sample=n(),
                                totprod=sum(PRODN,na.rm=TRUE),
                                totareah=sum(AreaHarvested,na.rm=TRUE),
                                yield=mean(YIELD,na.rm = TRUE))%>%
                      mutate(type=case_when(type=="base"~"2019 DS RBFHS",
                                            type=="mon1"~"2020 DS RCEP"),
                             region="PHILIPPINES")%>%
                      pivot_wider(names_from=c(type),
                                  values_from=c(sample,totprod,totareah,yield))%>%
                      mutate(`yield difference`=`yield_2020 DS RCEP`-`yield_2019 DS RBFHS`,
                             `%change`=`yield difference`/`yield_2019 DS RBFHS`,
                             changeinprod=case_when(`yield difference`<0~"decreased",
                                                    `yield difference`>0~"increased",
                                                    TRUE~"no change"))
        )%>%
        rename(REGION=region)%>%
        ungroup(regid)%>%
        select(-c(regid))
}
    

    stattab<-
        if (input$provx=="QUEZON"|input$provx=="LANAO DEL SUR"){
            yield%>%
                summarise(sample = n(),
                          mean = mean(YIELD,na.rm = TRUE),
                          `standard deviation` = sd(YIELD,na.rm = TRUE),
                          minimum = min(YIELD,na.rm = TRUE),
                          maximum = max(YIELD,na.rm = TRUE),
                          `95% Confidence Interval`=(paste(mean_cl_normal(YIELD)$ymin,",",mean_cl_normal(YIELD)$ymax))
                )%>%t()

        } else {
            yield%>%
                group_by(type)%>%
                summarise(sample = n(),
                          mean = mean(YIELD,na.rm = TRUE),
                          `standard deviation` = sd(YIELD,na.rm = TRUE),
                          minimum = min(YIELD,na.rm = TRUE),
                          maximum = max(YIELD,na.rm = TRUE),
                          `95% Confidence Interval`=(paste(mean_cl_normal(YIELD)$ymin,",",mean_cl_normal(YIELD)$ymax)
                          ))%>%t()

        }
    

    yvt<- if (input$provx!="QUEZON" & input$provx!="LANAO DEL SUR"){
        yield%>%
        var.test(YIELD~type, data=.)
    }
    
    ytt<-
    if (input$provx=="QUEZON"){
        yielda%>%
            filter(prov=='QUEZON')%>%
            do(tidy(t.test(.$YIELD, mu=4.00417474066905,data=.)))%>%
            mutate(decision=case_when(p.value<0.05 & estimate<0~"Yield significantly increased",
                                      p.value<0.05 & estimate>0 ~"Yield significantly decreased",
                                      TRUE~"Yield did not change significantly"),
                   `Yield Difference`=4.00417474066905-estimate)%>%
            select(c(`Yield Difference`,p.value,decision))%>%
            t()
    } else if (input$provx=="LANAO DEL SUR") {
        c("no test","no test")%>%t()
    } else if (yvt[["p.value"]]<0.05) {
        yield%>%
            do(tidy(t.test(YIELD~type, var.equal=FALSE,data=.)))%>%
            mutate(decision=case_when(p.value<0.05 & estimate<0~"Yield significantly increased",
                                      p.value<0.05 & estimate>0 ~"Yield significantly decreased",
                                 TRUE~"Yield did not change significantly"),
                   `Yield Difference`=estimate*-1)%>%
            select(c(`Yield Difference`,p.value,decision))%>%
            t()
    } else {
        yield%>%
            do(tidy(t.test(YIELD~type, var.equal=TRUE,data=.)))%>%
            mutate(decision=case_when(p.value<0.05 & estimate<0~"Yield significantly increased",
                                      p.value<0.05 & estimate>0 ~"Yield significantly decreased",
                                      TRUE~"Yield did not change significantly"),
                   `Yield Difference`=estimate*-1)%>%
            select(c(`Yield Difference`,p.value,decision))%>%
            t()
    }
    
    ph<-st_read("./Provinces.shp")
    
    phprov<-as_tibble(ph)
    
    
    yieldmap <-read.csv("./ymap.csv")
    
    
    ymapdata <-yieldmap%>%right_join(phprov,by=c("Province"="PROVINCE"))
    
    ymap<-st_as_sf(ymapdata)
    phprov<-as_tibble(ph)
    
    
    class(ymap)
    
    
        
     basex <- reactive({
            density(yield[yield$type=="base",]$YIELD)$x
            
            
        })
        
        basey <- reactive({
            density(yield[yield$type=="base",]$YIELD)$y
            
            
        })
        
        monitorx <- reactive({
            density(yield[yield$type=="mon1",]$YIELD)$x
        })
        
        monitory <- reactive({
            density(yield[yield$type=="mon1",]$YIELD)$y
        })
    
    

    output$plot <- if (input$provx=="QUEZON" | input$provx=="LANAO DEL SUR"){
        
        renderPlotly({
            plot1<-plot_ly(
                x = ~monitorx(), 
                y = ~monitory(), 
                type = 'scatter', 
                mode = 'lines', 
                name = '2020DS RCEF Monitoring', 
                fill = 'tozeroy')%>%
                layout(title="Density plot",
                       xaxis = list(title = 'Yield'),
                       yaxis = list(title = 'Density'),
                       legend = list(orientation = 'h'))
        })
        
    } else {
        renderPlotly({
        plot1<-plot_ly(
            x = ~basex(), 
            y = ~basey(), 
            type = 'scatter', 
            mode = 'lines', 
            name = '2019DS RBFHS (Baseline)', 
            fill = 'tozeroy')%>%
        add_trace(
            x = ~monitorx(), 
            y = ~monitory(), 
            type = 'scatter', 
            mode = 'lines', 
            name = '2020DS RCEF Monitoring', 
            fill = 'tozeroy')%>%
        layout(title="Density plot",
               xaxis = list(title = 'Yield'),
               yaxis = list(title = 'Density'),
               legend = list(orientation = 'h'))
    }
    )
    }
    
    output$tab <-renderPlotly({
        
        fig <- plot_ly(
            type='table',
            columnwidth = c(50, 100,100),
            header=list(
                values= c("<b> Statistics </b>", "<b>2019 DS RBFHS (Baseline)</b>", "<b>2020 DS RCEF Monitoring</b>"),
            align=c('left','center','center'),
            line=list(color = '#506784'),
            fill = list(color = '#119DFF'),
            font=list(color=c("black"))
            ),
            cells=list(
                values= if (input$provx=="QUEZON"){
                    rbind(rownames(stattab),
                          c(NA,"4.00417474066905 (FROM PSA)",NA,NA,NA),
                          t(unname(stattab)))
                } else if (input$provx=="LANAO DEL SUR") {
                    rbind(rownames(stattab),
                          c(NA,NA,NA,NA,NA),
                          t(unname(stattab)))
                    } else {
                        rbind(
                            rownames(stattab[-1,]),
                            t(as.matrix(unname(stattab[-1,]))))
                    },
                align=c('left','center','center'),
                fill = list(color = c('#119DFF','white','white','white')),
                line=list(color = '#506784'),
                font=list(color=c("black"))
                )
            )
    })
    
    
    
    
    output$tab2 <- renderPlotly({
        
        fig2 <- plot_ly(
            type='table',
            columnwidth = c(50,100),
            header=list(
                values= c(paste0("<b>",input$provx,"</b>"),"estimate"),
                align=c('left','center','center'),
                line=list(color = '#506784'),
                fill = list(color = '#119DFF'),
                font=list(color=c("black"))
            ),
            cells=list(
                values= rbind(rownames(ytt),
                             t(ytt)),
                align=c('left','center'),
                fill=list(color=c('#119DFF','white')),
                line=list(color='#506784'),
                font=list(color=c("black"))
            ))

        
    })
    
    output$ytab <- renderTable({yieldsum},  
                              striped = TRUE,  
                              hover = TRUE,  
                              width = '100%')
    
    output$ymap<- renderLeaflet({
    leaflet()%>%
        addTiles()%>%
        setView(lng = 122.8987799, lat =12.332891, zoom = 6.45)
    })
    
    
    observe({
    fildata<-ymap[[input$layer]]
    
    col=if (input$layer=="sigtest") {
        
        colorFactor(c("#ffffbf","#d7191c","#33a02c"),ymap$sigtest)
        
      } else if (input$layer=="Yielddiff") {
        
        colorNumeric("RdYlGn",domain = c(-1.5,2))
        
      } else {
        
        colorNumeric("RdYlGn",domain = c(0,6))
      }
    
    leafletProxy("ymap",data=ymap)%>%
      clearShapes()%>%
      addPolygons(stroke = TRUE,color="black",weight = 1,fillOpacity = 1,
                  fillColor=~col(fildata),
                  label=~paste0(Province, " (",fildata,")"),
                  labelOptions = labelOptions(textsize = "20px"))%>%
      addLegend(position="topright",pal=col,values=~`fildata`,
                opacity=1,
                title="legend",
                layerId="leg")
    
    
    })

})
        
    }

shinyApp(ui,server)