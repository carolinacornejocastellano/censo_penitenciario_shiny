# Librerías


library(haven)
library(rio)
library(tidyverse)
library(mlr)

library(shiny)
library(shinydashboard)
library(gapminder)
library(kableExtra)

library(highcharter)
library(gt)
library(htmltools)
library(htmlwidgets)
library(viridis)
library(dplyr)
library(bslib) #Ayuda a mejorar los temas del navbarpage

library(factoextra)
library(cluster)
library(ggrepel)
library(scatterplot3d)
library(rgl)
library(NbClust)




# Subir la base resumida a GitHub, e importarla a este Rmd
gapminder_data <- read_csv("https://github.com/DanielSotoHurtado/Censo_Nacional_Penitenciario/raw/main/Assignment_3/sel_clean_F.csv")



ui <-navbarPage("I Censo Nacional Penitenciario 2016",
                
                tabPanel("Resumen nacional",br(),hr(),h5(strong("El Primer Censo Nacional de Población Penitenciaria fue ejecutado el 2016 por el Instituto Nacional de Estadística e Informática (INEI) con apoyo del Ministerio de Justicia y Derechos Humanos (Minjus) y el Instituto Nacional Penitenciario (INPE). Su fin fue obtener información estadística sobre las características sociodemográficas y situación jurídica de la población penitenciaria de 18 años y más de edad recluida en todos los establecimientos penitenciarios del país. Se tiene una contabilidad total de 75963 internos(as).
A continuación se muestran los porcentajes de las principales variables a nivel nacional.")),
p(style="text-align: justify; font-size = 25px",
  "El Censo abordó 456 variables divididas en 5 módulos. Los datos son de libre acceso en", 
  em("la web del INEI:"),
  a(href = "http://iinei.inei.gob.pe/microdatos/",
    "MICRODATOS - INEI")),hr(),

fluidRow(box(plotOutput("GENERO1")), box(plotOutput("DELITOS")),
         box("El 94% de la población penitenciaria en el Perú es de sexo masculino."), 
         box("El robo agravado es notablemente uno de los delitos con mayor población penitenciaria a nivel nacional.")),
fluidRow(box(plotOutput("REOS")),    box(plotOutput("EDUCACION"))),
box("Este gráfico muestra la situación procesal en los 10 penales con mayor población penitenciaria a nivel nacional. El porcentaje más grande es el de sentenciados."), 
box("De modo similar, este gráfico presenta la distribución por nivel educativo aprobado en los 10 penales más populosos del país.")
                ),


tabPanel("Clústers",br(),hr(),h5(strong(" Se han generado dos clusters a partir de 6 variables:"), br(), 
                                 ( div(HTML("- DROGAS, describe a la proporción de internos(as) que respondieron 'Sí' a la pregunta <em>'¿Consumía drogas antes de ingresar al establecimiento penitenciario?',</em> "))), br(), 
                                 ( div(HTML("- ALCOHOL, describe a la proporción de internos(as) que respondieron 'Sí' a la pregunta <em>'¿Consumía alcohol antes de ingresar al establecimiento penitenciario?',</em> "))),  br(), 
                                 ( div(HTML("- VIOLEN_INFANTIL, describe a la proporción de internos(as) que respondieron 'Sí' a la pregunta <em>'¿Sus padres o cuidadores le pegaban cuando era niño?',</em> "))),  br(), 
                                 ( div(HTML("- FAM_ALCOHOL, describe a la proporción de internos(as) que respondieron 'Sí' a la pregunta <em>'Cuando usted era niño(a), ¿sus padres o adultos que vivían con usted tomaban alcohol/licor frecuentemente?',</em> "))), br(), 
                                 ( div(HTML("- AGRESION_PADRES, describe a la proporción de internos(as) que respondieron 'Sí' a la pregunta <em>'¿A tu mamá le pegaba tu papá o su pareja?',</em> "))), br(), 
                                 ( div(HTML("- PRIMARIA, describe a la proporción de internos(as) que indican <em>'Primaria' como su máximo nivel de educación aprobado.</em> "))), br(),
                                 p(style="text-align: justify; font-size = 25px",
                                   em("El grupo A y el grupo B"), 
                                   "como se puede observar en el cuadro de acuerdo a cada establecimiento penitenciario"),hr(),
                                 fluidRow(box(tableOutput("Tabla_Clusters")),box(strong("El grupo A reúne a 14 establecimientos penitenciarios, mientras que el grupo B reúne a 44.")))
)),

tabPanel("Establecimiento penitenciario",br(),hr(),h5(strong("Desglose de principales variables por Establecimiento Penitenciario (EP)")),
         p(style="text-align: justify; font-size = 25px",
           "Seleccione un", 
           em("establecimiento penitenciario"),"para observar la distribución de población penitenciaria según variables relevantes para este análisis."),hr(),
         dashboardPage(
           dashboardHeader(),
           dashboardSidebar(selectInput("EST_PENIT", "Seleccione un establecimiento penitenciario", 
                                        choices = gapminder_data |>
                                          select(EST_PENIT)|>
                                          distinct()|>
                                          arrange(EST_PENIT)|>
                                          drop_na())),
           dashboardBody(
             fluidRow(box(plotOutput("DROGAS")),box(plotOutput("ALCOHOL"))),
             fluidRow(box(plotOutput("VIOLENCIA")),box(tableOutput("tabla"))))))

)



server <- function(input, output, session) {
  
  output$GENERO1<- renderPlot ({
    gapminder_data |> 
      group_by(GENERO)|>
      summarise(conteo=n())|>
      ggplot(aes(x=GENERO,y=conteo,fill=GENERO) )+
      geom_bar(stat = "identity",position = "dodge") +
      geom_text(aes(label=conteo), vjust=-0.3, size=3)+
      theme_minimal()+
      #scale_fill_manual(values=c("darkgoldenrod1","aquamarine3"))+
      scale_fill_brewer(palette="Spectral")+
      labs(x = "Sexo", 
           y = "Cantidad", 
           title = "Población penitenciaria, por sexo, a nivel nacional",
           caption = str_c("Fuente:INEI", "Elaboración: Grupo 3", sep = "        |        "))+
      theme(legend.position = "topright",panel.grid.minor = element_blank(),
            panel.grid.major = element_blank())
  })
  
  output$DELITOS<- renderPlot ({
    gapminder_data |> 
      group_by(DELITO_ESPECIFICO)|>
      summarise(conteo=n())|>
      top_n((n=5))|>
      ggplot(aes(x=fct_reorder(DELITO_ESPECIFICO,conteo),y=conteo, fill=DELITO_ESPECIFICO) )+
      geom_col(position = "stack") +
      coord_flip()+
      geom_text(aes(label=conteo), position = "stack", hjust = 2, size=3)+
      theme_minimal()+
      #scale_fill_manual(values = wes_palette("Royal1", n = 4))+
      scale_fill_brewer(palette="Spectral")+
      labs(x = "Delito", 
           y = "Cantidad", 
           title = "Población penitenciaria, por delito, a nivel nacional",
           caption = str_c("Fuente:INEI", "Elaboración: Grupo 3", sep = "        |        "))+
      theme(legend.position = "topright",panel.grid.minor = element_blank(),
            panel.grid.major = element_blank())
  })
  
  output$REOS<- renderPlot ({
    gapminder_data |> 
      group_by(EST_PENIT) |> 
      count(EST_PENIT, P216) |> 
      mutate(pct = round((n/sum(n))*100, digits = 2), total = sum(n)) |> 
      drop_na() |> 
      ungroup() |> 
      arrange(desc(total)) |> 
      top_n(20,total ) |> 
      ggplot(aes(x = fct_reorder(EST_PENIT, total), y = total, fill = P216  )) +
      geom_col( position = "stack") +
      guides(fill=guide_legend(title = "Situación penal"))+
      coord_flip()+
      geom_text(aes(label=pct), position = "stack", hjust = 2,size=3)+
      theme_minimal()+
      scale_fill_brewer(palette="Spectral")+
      labs(x = "Establecimiento penitenciario", 
           y = "Porcentaje", 
           title = "Población penitenciaria por situación procesal",
           #subtitle = str_c("Población total del ",":",sep = " "),  
           caption = str_c("Fuente: INEI", "Elaboración: Grupo 3", sep = "        |        "))+
      theme(legend.position = "topright",panel.grid.minor = element_blank(),
            panel.grid.major = element_blank())
  })
  
  output$EDUCACION<- renderPlot ({
    gapminder_data |> 
      group_by(EST_PENIT) |> 
      count(EST_PENIT, grupo_P104_1) |> 
      mutate(total = sum(n)) |>
      mutate(grupo_P104_1 = 
               fct_collapse(grupo_P104_1, NULL = "Missing"))|>
      mutate(grupo_P104_1 = fct_relevel(grupo_P104_1,"Sin_nivel")) |> 
      drop_na() |> 
      ungroup() |> 
      arrange(desc(total)) |> 
      top_n(40,total ) |> 
      ggplot(aes(x = fct_reorder(EST_PENIT,total), y = total, fill = grupo_P104_1)) +
      geom_col(position = "stack", alpha = 1) +
      guides(fill=guide_legend(title = "Nivel Educativo",reverse = T))+
      coord_flip()+
      geom_text(aes(label=total), position = "fill", vjust=-0.3, size=3, hjust = -0.3,  color = "grey20")+
      theme_minimal()+
      scale_fill_brewer(palette = "Spectral",
                        labels = c("Sin Nivel", "Primaria","Secundaria","Superior"))+
      theme(legend.position = c(0.8, 0.5),
            legend.title = element_text(colour="grey30",size=10),
            legend.text = element_text(colour="grey50",size=10),
            panel.grid.minor = element_blank(),
            panel.grid.major = element_blank())+
      labs(x = "Establecimiento Penitenciario", 
           y = "Cantidad", 
           title = "Población penitenciaria, según nivel de educación aprobado (distribución porcentual)",
           caption = str_c("Fuente: INEI", "Elaboración: Grupo 3", sep = "        |        "))
  })
  
  output$DROGAS<- renderPlot({
    gapminder_data |> 
      filter(EST_PENIT==input$EST_PENIT) |> 
      group_by(P109_1)|>
      summarise(TOTAL=n())|>
      arrange(desc(TOTAL))|>
      ggplot(aes(x=fct_reorder(P109_1,TOTAL),y=TOTAL,fill=P109_1))+
      geom_bar(stat = "identity",position = "dodge") +
      geom_text(aes(label=TOTAL), vjust=-0.3, size=4)+
      theme_minimal()+
      scale_fill_brewer(palette="Spectral")+
      labs(x = "¿Consumía drogas?", 
           y = "Cantidad", 
           title = "Población penitenciaria que consumía algún tipo de droga antes de ingresar al penal",
           subtitle = str_c("Población total del ",input$EST_PENIT, ":",sep = " "),  
           caption = str_c("Fuente: INEI", "Elaboración: Grupo 3", sep = "        |        "))+
      theme(legend.position = "topright",panel.grid.minor = element_blank(),
            panel.grid.major = element_blank())
  })
  
  output$VIOLENCIA<- renderPlot({
    gapminder_data |> 
      filter(EST_PENIT==input$EST_PENIT) |> 
      group_by(P126)|>
      summarise(TOTAL=n())|>
      arrange(desc(TOTAL))|>
      ggplot(aes(x=fct_reorder(P126,TOTAL),y=TOTAL,fill=P126))+
      geom_bar(stat = "identity",position = "dodge") +
      geom_text(aes(label=TOTAL), vjust=-0.3, size=4)+
      theme_minimal()+
      scale_fill_brewer(palette="Spectral")+
      labs(x = "¿Le pegaban cuando era niño(a)?", 
           y = "Total (Cantidad)", 
           title = "Población penitenciaria víctima de agresión física en su infancia",
           subtitle = str_c("Población total del ",input$EST_PENIT, ":",sep = " "),  
           caption = str_c("Fuente: INEI", "Elaboración: Grupo 3", sep = "        |        "))+
      theme(legend.position = "topright",panel.grid.minor = element_blank(),
            panel.grid.major = element_blank())
  })
  
  output$ALCOHOL<- renderPlot({
    gapminder_data |> 
      filter(EST_PENIT==input$EST_PENIT) |> 
      group_by(P109_2)|>
      summarise(TOTAL=n())|>
      arrange(desc(TOTAL))|>
      ggplot(aes(x=fct_reorder(P109_2,TOTAL),y=TOTAL,fill=P109_2))+
      geom_bar(stat = "identity",position = "dodge") +
      geom_text(aes(label=TOTAL), vjust=-0.3, size=4)+
      theme_minimal()+
      scale_fill_brewer(palette="Spectral")+
      labs(x = "¿Consumía alcohol?", 
           y = "Total (Cantidad)", 
           title = "Población penitenciaria que consumía algún tipo de alcohol antes de ingresar al penal",
           subtitle = str_c("Población total del ",input$EST_PENIT, ":",sep = " "),  
           caption = str_c("Fuente:INEI", "Elaboración: Grupo 3", sep = "        |        "))+
      theme(legend.position = "topright",panel.grid.minor = element_blank(),
            panel.grid.major = element_blank())
  })
  
  output$tabla<- function(){
    gapminder_data |> 
      filter(EST_PENIT==input$EST_PENIT) |>
      group_by(DELITO_ESPECIFICO)|>
      mutate(TOTAL=n())|>
      select(DELITO_ESPECIFICO,GENERO,TOTAL)|>
      count(GENERO,TOTAL)|>
      mutate(tot_gen_delit=n)|>
      spread(GENERO,n,fill = 0)|>
      select(-tot_gen_delit)|>
      #ungroup()|>
      arrange(desc(TOTAL))|>
      head(12)|>
      kbl(caption = "Población penitenciaria por sexo, según delito específico")|>
      kable_styling(bootstrap_options = "striped", full_width = F, position = "center",font_size = 10,fixed_thead = T)|>
      kable_paper(full_width = F)|>
      column_spec(1, bold = T, border_right = T) |>
      column_spec(2, width = "2em",border_right = T, background = "gold")
  }
  
  
  output$Tabla_Clusters<- function(){
    bd_clust<-gapminder_data|>
      group_by(EST_PENIT)|>
      mutate(TOTAL_EST_PENIT=n())|>
      select(EST_PENIT,si_P109_1,si_P109_2,si_P126,si_P127,si_P128,si_P129,TOTAL_EST_PENIT,Primaria)|>
      mutate(DROGAS=sum(si_P109_1)/TOTAL_EST_PENIT,
             ALCOHOL=sum(si_P109_2)/TOTAL_EST_PENIT,
             VIOLEN_INFANTIL=sum(si_P126)/TOTAL_EST_PENIT,
             FAM_ALCOHOL=sum(si_P127)/TOTAL_EST_PENIT,
             AGRESION_PADRES=sum(si_P129)/TOTAL_EST_PENIT,
             PRIMARIA=sum(Primaria)/TOTAL_EST_PENIT)|>
      select(EST_PENIT,DROGAS,ALCOHOL,VIOLEN_INFANTIL,FAM_ALCOHOL,AGRESION_PADRES,PRIMARIA)
    
    bd_clust<-bd_clust[!duplicated(bd_clust),] 
    
    bd_clust<-bd_clust|>
      column_to_rownames(var="EST_PENIT")
    
    ###AGRUPANDO POR CLUSTERS
    
    distancias=daisy(bd_clust, metric="gower")
    
    GRUPO=hcut(distancias, k = 2,hc_func='diana') 
    
    bd_clust|> mutate(GRUPO=GRUPO$cluster) |> group_by(GRUPO) |> summarise(
      DROGAS = mean(DROGAS),
      VIOLEN_INFANTIL = mean(VIOLEN_INFANTIL), 
      FAM_ALCOHOL=mean(FAM_ALCOHOL),
      AGRESION_PADRES=mean(AGRESION_PADRES),
      ALCOHOL=mean(ALCOHOL),
      PRIMARIA=mean(PRIMARIA))
    
    bd_clust$GRUPO = GRUPO$cluster
    bd_clust$GRUPO = as.factor(bd_clust$GRUPO)
    levels(bd_clust$GRUPO) = c("A","B")
    
    
    # names(bd_clust)=c("CONSUMO DE DROGAS ANTES DE INGRESAR AL PENAL","CONSUMO DE ALCOHOL ANTES DE INGRESAR AL PENAL","ANTECEDENTES DE VIOLENCIA INFANTIL","FAMILIARES QUE CONSUMIERON ALCOHOL","VIOLENCIA ENTRE SUS PADRES","ESTUDIOS HASTA PRIMARIA","GRUPO")
    
    bd_clust|>
      arrange(GRUPO)|>
      kbl(caption = "AGRUPACIÓN POR CLUSTERS")|>
      kable_styling(bootstrap_options = "hover", full_width = F, position = "center",font_size = 10,fixed_thead = T)|>
      kable_paper("hover", full_width = F)|>
      column_spec(1, bold = T, border_right = T) |>
      column_spec(8, width = "2em",border_right = F, background = "gold")
    
  }
  
}

shinyApp(ui, server)

