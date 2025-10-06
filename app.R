
library(shiny)
library(shinydashboard)
library(rio)
library(tidyverse)
library(archive)

# cargar bd --------------------------------------------------------------------

# bd=import('D:/pruebas shiny/ECE_4P_2024_alumnos_innominado fin v2.1 (10 var).xlsx')
# bd2=import('ECE_4P_2024_alumnos_innominado fin v2.1 (10 var).xlsx')
# b='D:/pruebas shiny/mi dashboard sidebar/bd comprimida.zip'
bd=read.csv(archive_read('bd comprimida.zip'))

# funcion que corrige el redondeo por default del R ----------------------------

round2 = function(x, n=0) {
  posneg = sign(x)
  z = abs(x)*10^n
  z = z + 0.5
  z = trunc(z)
  z = z/10^n
  z*posneg
}

# crear interfaz ---------------------------------------------------------------

## header ----------------------------------------------------------------------
header=dashboardHeader(title = 'TODO')

## sidebar ---------------------------------------------------------------------
sidebar=dashboardSidebar(
  sidebarMenu(
    # pagina 1 y 11
    menuItem('tarjetas', tabName='pag1', icon=icon('list-ol'),
      menuSubItem('tarjetas 2', tabName='pag11', icon=NULL),
      
      # input 1 filtrar por ...
      selectInput('filtro_t2', 'filtrar por:',
                  list('Nacional',
                       'Sexo'=list('Hombre','Mujer'),
                       'Area'=list('Urbano','Rural'),
                       'Gestión'=list('Público','Privado'),
                       'Caracteristica'=list('Polidocente Completo','Unidocente / Multigrado')),
                  selectize=F,
                  size=13)
    ),
    
    # pagina 2 y 21
    menuItem('graficos', tabName='pag2', icon=icon('chart-column'),
      menuSubItem('graficos 2', tabName='pag21', icon=NULL),
      
      # input 2 graficar por ...
      selectInput('filtro', 'graficar por:',
                  c('Sexo','Area','Gestión','Caracteristica'))
    )
  )
)

## body ------------------------------------------------------------------------
body=dashboardBody(
  tabItems(
    # pagina 11
    tabItem(tabName='pag11',
            
            # fila 1 - lectura - medida promedio
            fluidRow(
              # medida promedio
              infoBoxOutput('MP_lec', width=12)
            ),
            
            # fila 2 - lectura - nivel de logro
            fluidRow(
              # previo al inicio
              infoBoxOutput('NL_0_lec', width=3),
              # en inicio
              infoBoxOutput('NL_1_lec', width=3),
              # en proceso
              infoBoxOutput('NL_2_lec', width=3),
              # satisfactorio
              infoBoxOutput('NL_3_lec', width=3)
            ),
            
            # fila 3 - matematica - medida promedio
            fluidRow(
              # medida promedio
              infoBoxOutput('MP_mat', width=12)
            ),
            
            # fila 4 - matematica - nivel de logro
            fluidRow(
              # previo al inicio
              infoBoxOutput('NL_0_mat', width=3),
              # en inicio
              infoBoxOutput('NL_1_mat', width=3),
              # en proceso
              infoBoxOutput('NL_2_mat', width=3),
              # satisfactorio
              infoBoxOutput('NL_3_mat', width=3)
            )
    ),
    
    # pagina 21
    tabItem(tabName='pag21',
            
            # fila 1
            fluidRow(
              # box 1 grafico lectura
              box(plotOutput('grafico1'),title='Lectura',
                  status='info',solidHeader=T,width=5),
              
              # box 2 grafico matematica
              box(plotOutput('grafico2'),title='Matematica',
                  status='info',solidHeader=T,width=5)
            ),
            
            # fila 2
            fluidRow(
              # box 3 boton guardar graf lectura
              box(downloadButton('guardar_graf1', 'Guardar grafica lectura'),
                  width=5),
              
              # box 4 boton guardar graf matematica
              box(downloadButton('guardar_graf2', 'Guardar grafica matematica'),
                  width=5)
            )
    )
  )
)

# servidor ---------------------------------------------------------------------
server=function(input, output){
  
  # salidas de pagina 11 -------------------------------------------------------
  
  temp11=reactive({
    # reducir la bd si selecciono algun filtro
    switch(input$filtro_t2,
           'Nacional'=bd,
           'Hombre'=bd|>filter(sexo==0),
           'Mujer'=bd|>filter(sexo==1),
           'Urbano'=bd|>filter(area==1),
           'Rural'=bd|>filter(area==2),
           'Público'=bd|>filter(gestion2==1),
           'Privado'=bd|>filter(gestion2==2),
           'Polidocente Completo'=bd|>filter(caracteristica2==1),
           'Unidocente / Multigrado'=bd|>filter(caracteristica2==2))
  })
  
  # calculo de la MP lectura
  MP_lec=reactive({
    temp11()|>
      summarise(xxx=weighted.mean(medida500_L,ajuste_por_no_respuesta_L,na.rm=T))|>
      round2()
  })
  
  # calculo de los nivel de logro lectura
  NL_lec=reactive({
    temp11()|>
      count(grupo_L,wt=ajuste_por_no_respuesta_L)|>
      filter(!is.na(grupo_L))|>
      mutate(por=n/sum(n)*100,
             por=por|>round2(1)|>paste0('%'),
             .keep='unused')
  })
  
  # calculo de la MP matematica
  MP_mat=reactive({
    temp11()|>
      summarise(xxx=weighted.mean(medida500_M,ajuste_por_no_respuesta_M,na.rm=T))|>
      round2()
  })
  
  # calculo de los nivel de logro matematica
  NL_mat=reactive({
    temp11()|>
      count(grupo_M,wt=ajuste_por_no_respuesta_M)|>
      filter(!is.na(grupo_M))|>
      mutate(por=n/sum(n)*100,
             por=por|>round2(1)|>paste0('%'),
             .keep='unused')
  })
  
  # lectura - medida promedio
  output$MP_lec=renderInfoBox({
    infoBox('media promedio de lectura', MP_lec(),
            icon=icon('book-open'))
  })
  
  # lectura - previo al inicio
  output$NL_0_lec=renderInfoBox({
    infoBox('previo al inicio', NL_lec()[1,2],
            icon=icon('thumbs-down'), color='black', fill=T)
  })
  
  # lectura - en inicio
  output$NL_1_lec=renderInfoBox({
    infoBox('en inicio', NL_lec()[2,2],
            icon=icon('arrow-right'), color='red', fill=T)
  })
  
  # lectura - en proceso
  output$NL_2_lec=renderInfoBox({
    infoBox('en proceso', NL_lec()[3,2],
            icon=icon('signal'), color='orange', fill=T)
  })
  
  # lectura - satisfactorio
  output$NL_3_lec=renderInfoBox({
    infoBox('satisfactorio', NL_lec()[4,2],
            icon=icon('thumbs-up'), color='olive', fill=T)
  })
  
  # matematica - medida promedio
  output$MP_mat=renderInfoBox({
    infoBox('media promedio de matematica', MP_mat(),
            icon=icon('calculator'))
  })
  
  # matematica - previo al inicio
  output$NL_0_mat=renderInfoBox({
    infoBox('previo al inicio', NL_mat()[1,2],
            icon=icon('thumbs-down'), color='black', fill=T)
  })
  
  # matematica - en inicio
  output$NL_1_mat=renderInfoBox({
    infoBox('en inicio', NL_mat()[2,2],
            icon=icon('arrow-right'), color='red', fill=T)
  })
  
  # matematica - en proceso
  output$NL_2_mat=renderInfoBox({
    infoBox('en proceso', NL_mat()[3,2],
            icon=icon('signal'), color='orange', fill=T)
  })
  
  # matematica - satisfactorio
  output$NL_3_mat=renderInfoBox({
    infoBox('satisfactorio', NL_mat()[4,2],
            icon=icon('thumbs-up'), color='olive', fill=T)
  })
  
  # salidas de pagina 21 -------------------------------------------------------
  
  # NUEVO CODIGO
  bd_mod=bd|>
    mutate(sexo=if_else(sexo==0,1,2),
           grupo_L=factor(grupo_L,0:3,labels=c('Previo al inicio','En inicio','En proceso','Satisfactorio')),
           grupo_M=factor(grupo_M,0:3,labels=c('Previo al inicio','En inicio','En proceso','Satisfactorio')))
  
  # crear nueva var en funcion a la variable a graficar
  temp21=reactive({
    switch(input$filtro,
           'Sexo'=bd_mod|>mutate(mi_var=sexo),
           'Area'=bd_mod|>mutate(mi_var=area),
           'Gestión'=bd_mod|>mutate(mi_var=gestion2),
           'Caracteristica'=bd_mod|>mutate(mi_var=caracteristica2))
  })
  
  # bd para grafico lectura
  bd_lec=reactive({
    temp21()|>
      mutate(mi_var=9)|>
      bind_rows(temp21())|>
      group_by(mi_var)|>
      count(grupo_L, wt=ajuste_por_no_respuesta_L)|>
      filter(!is.na(grupo_L))|>
      mutate(por=n/sum(n)*100,
             etiq=round2(por,1),
             h_barra=if_else(grupo_L=='Satisfactorio',por,por*-1),
             h_etiq=cumsum(por)-por/2-(sum(por[1:3])))
  })
  
  bd_lec2=reactive({
    switch(input$filtro,
           'Sexo'=bd_lec()|>
             mutate(mi_var=factor(mi_var,c(9,1,2),c('Nacional','Hombre','Mujer'))),
           'Area'=bd_lec()|>
             mutate(mi_var=factor(mi_var,c(9,1,2),c('Nacional','Urbano','Rural'))),
           'Gestión'=bd_lec()|>
             mutate(mi_var=factor(mi_var,c(9,1,2),c('Nacional','Público','Privado'))),
           'Caracteristica'=bd_lec()|>
             mutate(mi_var=factor(mi_var,c(9,1,2),c('Nacional','Polidocente Completo','Unidocente / Multigrado'))))
  })
  
  # bd para grafico matematica
  bd_mat=reactive({
    temp21()|>
      mutate(mi_var=9)|>
      bind_rows(temp21())|>
      group_by(mi_var)|>
      count(grupo_M, wt=ajuste_por_no_respuesta_M)|>
      filter(!is.na(grupo_M))|>
      mutate(por=n/sum(n)*100,
             etiq=round2(por,1),
             h_barra=if_else(grupo_M=='Satisfactorio',por,por*-1),
             h_etiq=cumsum(por)-por/2-(sum(por[1:3])))
  })
  
  bd_mat2=reactive({
    switch(input$filtro,
           'Sexo'=bd_mat()|>
             mutate(mi_var=factor(mi_var,c(9,1,2),c('Nacional','Hombre','Mujer'))),
           'Area'=bd_mat()|>
             mutate(mi_var=factor(mi_var,c(9,1,2),c('Nacional','Urbano','Rural'))),
           'Gestión'=bd_mat()|>
             mutate(mi_var=factor(mi_var,c(9,1,2),c('Nacional','Público','Privado'))),
           'Caracteristica'=bd_mat()|>
             mutate(mi_var=factor(mi_var,c(9,1,2),c('Nacional','Polidocente Completo','Unidocente / Multigrado'))))
  })
  
  # grafico lectura
  graf_lec=reactive({
    ggplot(bd_lec2(),aes(x=mi_var,y=h_barra,fill=grupo_L))+
      geom_col()+
      geom_text(aes(y=h_etiq,label=etiq))+
      geom_hline(yintercept = 0)+
      scale_fill_manual(values=c('#A6A6A6','#A74D4B','#F79646','#9BBB59'))+
      theme(legend.position = 'bottom',
            legend.title = element_blank(),
            axis.ticks = element_blank(),
            axis.text.y = element_blank(),
            axis.title = element_blank(),
            panel.background = element_blank())
  })
  
  # grafico matematica
  graf_mat=reactive({
    ggplot(bd_mat2(),aes(x=mi_var,y=h_barra,fill=grupo_M))+
      geom_col()+
      geom_text(aes(y=h_etiq,label=etiq))+
      geom_hline(yintercept = 0)+
      scale_fill_manual(values=c('#A6A6A6','#A74D4B','#F79646','#9BBB59'))+
      theme(legend.position = 'bottom',
            legend.title = element_blank(),
            axis.ticks = element_blank(),
            axis.text.y = element_blank(),
            axis.title = element_blank(),
            panel.background = element_blank())
  })
  
  # output grafico lectura
  output$grafico1=renderPlot({
    graf_lec()
  })
  
  # output grafico matematica
  output$grafico2=renderPlot({
    graf_mat()
  })
  
  # nombre de la foto de la grafica de lectura
  nom_foto_lec=reactive({
    switch(input$filtro,
           'Sexo'='grafico sexo lectura.png',
           'Area'='grafico area lectura.png',
           'Gestión'='grafico gestion lectura.png',
           'Caracteristica'='grafico caracteristica lectura.png')
  })
  
  # nombre de la foto de la grafica de matematica
  nom_foto_mat=reactive({
    switch(input$filtro,
           'Sexo'='grafico sexo matematica.png',
           'Area'='grafico area matematica.png',
           'Gestión'='grafico gestion matematica.png',
           'Caracteristica'='grafico caracteristica matematica.png')
  })
  
  # output guardar graf lectura
  output$guardar_graf1=downloadHandler(
    filename=nom_foto_lec,
    content=function(file){
      ggsave(filename=file, plot=graf_lec(), dpi=900)
    }
  )
  
  # output guardar graf matematica
  output$guardar_graf2=downloadHandler(
    filename=nom_foto_mat,
    content=function(file){
      ggsave(filename=file, plot=graf_mat(), dpi=900)
    }
  )
}

# correr -----------------------------------------------------------------------
shinyApp(dashboardPage(header,sidebar,body), server)
