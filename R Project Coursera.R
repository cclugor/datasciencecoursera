## week 4 - Coursera R Project --------
## Cindy Lugo

setwd("C:/Users/lugor/Downloads/rprog_data_ProgAssignment3-data")

outcome <- read.csv("outcome-of-care-measures.csv",na.strings="Not Available",stringsAsFactors=FALSE)
head(outcome)
ncol(outcome)

### hist of 30-day death rates from heart attack ###
outcome[, 11] <- as.numeric(outcome[, 11])
## You may get a warning about NAs being introduced; that is okay
hist(outcome[, 11])


#### Finding the best hospital in a state ####
best <- function(state, outcome) {
  ## Read outcome data
  x<-read.csv("outcome-of-care-measures.csv",na.strings="	Not Available",stringsAsFactors=FALSE)
  ## Check that state and outcome are valid
  if(all(state %in% x$State)==F){
    stop("invalid state")
  } else if (all(outcome %in% c("heart attack","pneumonia","heart failure"))==F){
    stop("invalid outcome")
  } else if (all(outcome %in% c("heart attack","pneumonia","heart failure"))==T&all(state %in% x$State)==T){
    ## Return hospital name in that state with lowest 30-day death
    class(x$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack)<-"numeric"
    class(x$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure)<-"numeric"
    class(x$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia)<-"numeric"
    
    if(outcome=="heart attack"){
      best<-x%>%
        filter(State==state)%>%
        top_n(-1,wt=Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack)%>%
        select(Hospital.Name)%>%
        arrange(Hospital.Name)
      return(best[1,1])
    } else if(outcome=="heart failure"){
      best<-x%>%
        filter(State==state)%>%
        top_n(-1,wt=Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure)%>%
        select(Hospital.Name)%>%
        arrange(Hospital.Name)
      return(best[1,1])
    }  else if(outcome=="pneumonia"){
      best<-x%>%
        filter(State==state)%>%
        top_n(-1,wt=Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia)%>%
        select(Hospital.Name)%>%
        arrange(Hospital.Name)
      return(best[1,1])
    }
    
  }
  
}


outcome<-"heart failure"

outcome<-"pneumonia"

outcome<-"heart attack"

state<-"TX"
state<-"MD"
state<-"CA"
state<-"BB"


best(state, outcome)

num<-"worst"


rankhospital <- function(state, outcome, num = "best") {
  ## Read outcome data
  x<-read.csv("outcome-of-care-measures.csv",na.strings="	Not Available",stringsAsFactors=FALSE)
  num<-ifelse(num=="worst",-1,ifelse(num=="best",1,num))
  ## Check that state and outcome are valid
  if(all(state %in% x$State)==F){
    stop("invalid state")
  } else if (all(outcome %in% c("heart attack","pneumonia","heart failure"))==F){
    stop("invalid outcome")
  } else if (all(outcome %in% c("heart attack","pneumonia","heart failure"))==T&all(state %in% x$State)==T){
    ## Return hospital name in that state with lowest 30-day death
    class(x$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack)<-"numeric"
    class(x$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure)<-"numeric"
    class(x$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia)<-"numeric"
    
    if(outcome=="heart attack"){
      best<-x%>%
        filter(State==state)%>%
        arrange(Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack,Hospital.Name)%>%
        mutate(rank=row_number(Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack))%>%
        top_n(-num,wt=Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack)%>%
        select(Hospital.Name,rank,Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack)%>%
        arrange(rank)
      return(best[1:abs(num),1:3])
    } else if(outcome=="heart failure"){
      best<-x%>%
        filter(State==state)%>%
        select(Hospital.Name,Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure)%>%
        arrange(Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure,Hospital.Name)%>%
        mutate(rank=row_number(Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure))%>%
        top_n(-num,wt=Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure)%>%
        select(Hospital.Name,rank,Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure)%>%
        arrange(rank)
      return(best[1:abs(num),1:3])
    }  else if(outcome=="pneumonia"){
      best<-x%>%
        filter(State==state)%>%
        arrange(Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia,Hospital.Name)%>%
        mutate(rank=row_number(Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia))%>%
        top_n(-num,wt=Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia)%>%
        select(Hospital.Name,rank,Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia)%>%
        arrange(rank)
      return(best[1:abs(num),1:3])
    }
}
}

num<-3

rankhospital(state, outcome, -5)
rankhospital("MD", "heart failure", -5)

## Función rankall para que se imprima el rank seleccionado para cada uno de los estados
rankall <- function(outcome, num = "best") {
  ## Read outcome data
    x<-read.csv("outcome-of-care-measures.csv",na.strings="	Not Available",stringsAsFactors=FALSE)
  num<-ifelse(num=="worst",-1,ifelse(num=="best",1,num))
  ## Check that state and outcome are valid
  if(all(outcome %in% c("heart attack","pneumonia","heart failure"))==F){
    stop("invalid outcome")
  } else {
  
      ## Return hospital name in that state with lowest 30-day death
    class(x$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack)<-"numeric"
    #x$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack <- as.numeric(x$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack)
    class(x$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure)<-"numeric"
    #x$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure <- as.numeric(x$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure)
    class(x$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia)<-"numeric"
    #x$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia <- as.numeric(x$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia)
    
        
    if(outcome=="heart attack"){
      best<-x%>%
        group_by(State)%>%
        arrange(Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack,Hospital.Name)%>%
        mutate(rank=row_number(Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack))%>%
        top_n(-num,wt=Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack)%>%
        select(Hospital.Name,rank,Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack)%>%
        arrange(State)
      best<-best[best$rank==abs(num),]
      return(best)
    } else if(outcome=="heart failure"){
      best<-x%>%
        group_by(State)%>%
        select(Hospital.Name,Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure)%>%
        arrange(Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure,Hospital.Name)%>%
        mutate(rank=row_number(Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure))%>%
        top_n(-num,wt=Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure)%>%
        select(Hospital.Name,rank,Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure)%>%
        arrange(State)
      best<-best[best$rank==abs(num),]
      return(best)
    }  else if(outcome=="pneumonia"){
      best<-x%>%
        group_by(State)%>%
        arrange(Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia,Hospital.Name)%>%
        mutate(rank=row_number(Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia))%>%
        top_n(-num,wt=Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia)%>%
        select(Hospital.Name,rank,Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia)%>%
        arrange(State)
        best<-best[best$rank==abs(num),]
      return(best)
    }
  }
}

rankall(outcome, 1)
rankall(outcome, 20)

num<-20
best("SC", "heart attack")
dim(x)
x$`heart attack`<-x$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack
x$`heart failure`<-x$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure
x$`pneumonia`<-x$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia
x$outcome
## rate
best("NY", "pneumonia")
num<--1
best<-x%>%
  filter(State=="AK")%>%
  select(Hospital.Name,Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack)
  arrange(Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack,Hospital.Name)%>%
  mutate(rank=row_number(Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack))%>%
  top_n(-num,wt=Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack)%>%
  select(Hospital.Name,rank,Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack) %>%
  arrange(State)
best<-best[best$rank==abs(num),]
return(best)


best<-x%>%
  filter(State==state)%>%
  top_n(-1,wt=Upper.Mortality.Estimate...Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia)%>%
  select(Hospital.Name)%>%
  arrange(Hospital.Name)
best[1,1]

yyy<-x[,c(2,7,11,17,23)]

rankhospital("AK", "heart attack", 2)

r <- rankall("heart failure", 11)
as.character(subset(r, State == "AK")$Hospital.Name)

colnames(x)
head(rankall("heart attack", 20), 10)
head(rankall("heart attack", 20), 15)

best("AK", "pneumonia")
rankhospital("NC", "heart attack", 2)
rankhospital("WA", "heart attack", 7)
rankhospital("WA", "heart attack", 7)
rankhospital("TX", "pneumonia", 15)
rankhospital("NY", "heart attack", 7)
r <- rankall("heart attack", 4)
as.character(subset(r, State == "HI")$Hospital.Name)

r <- rankall("pneumonia", 1)
as.character(subset(r, State == "NJ")$Hospital.Name)
r <- rankall("heart failure", 10)
as.character(subset(r, State == "NV")$Hospital.Name)



best("SC", "heart attack")
# ok

best("NY", "pneumonia")

best("AK", "pneumonia")

rankhospital("NC", "heart attack", "worst")
# not ok

rankhospital("WA", "heart attack", 7)
#ok

rankhospital("TX", "pneumonia", 10)
# not ok

rankhospital("NY", "heart attack", 7)
# ok

r <- rankall("heart attack", 4)
as.character(subset(r, State == "HI")$Hospital.Name)
#ok

r <- rankall("pneumonia", "worst")
as.character(subset(r, State == "NJ")$Hospital.Name)

r <- rankall("pneumonia", 5000)
as.character(subset(r, State == "NJ")$Hospital.Name)


#not ok
r <- rankall("heart failure", 10)
as.character(subset(r, state == "NV")$hospital)

r <- rankall("heart failure", 10)
as.character(subset(r, State == "NV")$Hospital.Name)

#ok

## hacen falta dos detalles, el primero es que si hay más rank que datos se entregue NA
## hace falta que se el worst de la última función imprima correctamente porque no lo está haciendo bien
# Write a function called best that take two arguments: the 2-character abbreviated name of a state and an

# outcome name. The function reads the outcome-of-care-measures.csv file and returns a character vector
# with the name of the hospital that has the best (i.e. lowest) 30-day mortality for the specified outcome
# in that state. The hospital name is the name provided in the Hospital.Name variable. The outcomes can
# be one of “heart attack”, “heart failure”, or “pneumonia”. Hospitals that do not have data on a particular
# outcome should be excluded from the set of hospitals when deciding the rankings.
# Handling ties. If there is a tie for the best hospital for a given outcome, then the hospital names should
# be sorted in alphabetical order and the first hospital in that set should be chosen (i.e. if hospitals “b”, “c”,
# and “f” are tied for best, then hospital “b” should be returned).
# The function should use the following template.
# best <- function(state, outcome) {
#   ## Read outcome data
#   ## Check that state and outcome are valid
#   ## Return hospital name in that state with lowest 30-day death
#   ## rate
# }
# The function should check the validity of its arguments. If an invalid state value is passed to best, the
# function should throw an error via the stop function with the exact message “invalid state”. If an invalid
# outcome value is passed to best, the function should throw an error via the stop function with the exact
# message “invalid outcome”.

data$primero<-NA
data$ult_precio<-NA
for (i in 1:(nrow(data))) {
  for (j in (length(data)-2):2) {
    if(is.na(data[i,j])==F){
      data$ult_precio[i]<-data[i,j]
      break
    } else {
      j<-j+1
    }
  }
}

#------
for (i in 1:(nrow(data))) {
  for (j in 2:(length(data)-2)) {
    if(is.na(data[i,j])==F){
      data$primero[i]<-data[i,j]
      break
    } else {
      j<-j+1
    }
  }
}


iris

ggiris <- qplot(Petal.Width, Sepal.Length, data = iris, color = Species)
ggplotly(ggiris)

data(canada.cities, package = "maps")
viz <- ggplot(canada.cities, aes(long, lat)) +
  borders(regions = "canada") +
  coord_equal() +
  geom_point(aes(text = name, size = pop), colour = "red", alpha = 1/2)
ggplotly(viz, tooltip = c("text", "size"))

# linked scatterplot brushing
d <- highlight_key(mtcars)
qplot(data = d, x = mpg, y = wt) %>%
  subplot(qplot(data = d, x = mpg, y = vs)) %>% 
  layout(title = "Click and drag to select points") %>%
  highlight("plotly_selected")


# more brushing (i.e. highlighting) examples
demo("crosstalk-highlight-ggplotly", package = "plotly")

# client-side linked brushing in a scatterplot matrix
highlight_key(iris) %>%
  GGally::ggpairs(aes(colour = Species), columns = 1:4) %>%
  ggplotly(tooltip = c("x", "y", "colour")) %>%
  highlight("plotly_selected")




sin_na_1<-sin_na %>%
  ungroup()%>%
  mutate(`Nombre DANE`=replace(`Nombre DANE`, `Nombre DANE`=="GUANTES PARA EXAMEN, NO EST?RILES", "GUANTES PARA EXAMEN"),`Nombre DANE`=replace(`Nombre DANE`, `Nombre DANE`=="RESPIRADORES CON MASCARA DE FILTRADO (MASCARA DE ALTA EFICIENCIA O MASCARA N95)", "MASCARA N95"),`Nombre DANE`=replace(`Nombre DANE`, `Nombre DANE`=="SOLUCI?N O GEL DESINFECTANTE A BASE DE ALCOHOL", "GEL DESINFECTANTE"),`Nombre DANE`=replace(`Nombre DANE`, `Nombre DANE`=="JAB?N DE TOCADOR EN BARRA", "JAB?N DE TOCADOR"))

save(sin_na_1,file = "G:/Mi unidad/2020 SIC/Docs Coyuntura informe Super DANE/Resultados/sin_na_1.RData")
x<-ggplot(sin_na_1,aes(y=`Precio/Kg`,x=`Nombre DANE`))+geom_boxplot()+theme(axis.text.x = element_text(angle = 90,size=10))
ggplotly(x)
y<-ggplot(sin_na_1,aes(y=variacion,x=`Nombre DANE`))+geom_boxplot()+theme(axis.text.x = element_text(angle = 90,size=10))


Nombres<-sin_na_1%>%
  ungroup()%>%
  distinct(`Nombre DANE`)
Alimentos<-Nombres[c(1,2,4,7,9,14,16,17,20:23),1]
Medicamentos<-Nombres[c(5,6,10,11,13,18),]
Elem_protec<-Nombres[c(3,8,12,15,19),1]
class(Alimentos)

sin_na_1<-sin_na %>%
  ungroup()%>%
  mutate(Categoria=replace(Categoria, `Nombre DANE`=="GUANTES PARA EXAMEN, NO EST?RILES", "GUANTES PARA EXAMEN"),`Nombre DANE`=replace(`Nombre DANE`, `Nombre DANE`=="RESPIRADORES CON MASCARA DE FILTRADO (MASCARA DE ALTA EFICIENCIA O MASCARA N95)", "MASCARA N95"),`Nombre DANE`=replace(`Nombre DANE`, `Nombre DANE`=="SOLUCI?N O GEL DESINFECTANTE A BASE DE ALCOHOL", "GEL DESINFECTANTE"),`Nombre DANE`=replace(`Nombre DANE`, `Nombre DANE`=="JAB?N DE TOCADOR EN BARRA", "JAB?N DE TOCADOR"))

sin_na_1[sin_na_1$`Nombre DANE` %in% Alimentos[2:3],]

Alimentos<-c("LECHE LARGA VIDA", "ARROZ PARA SECO","AZ?CAR REFINADA", "HUEVO", "ACEITE DE GIRASOL", "ARVEJA","NARANJA", "CEBOLLA CABEZONA","PAPA NEGRA","QUESO CAMPESINO", "CEBOLLA EN RAMA","PAPA CRIOLLA")

Medicamentos<-c("ACETAMINOF?N","AMOXICILINA","IBUPROFENO","IVERMECTINA","NAPROXENO","HIDROXICLOROQUINA")

Elem_protec<-c("JAB?N DE TOCADOR","GEL DESINFECTANTE","MASCARA N95","GUANTES QUIR?RGICOS","GUANTES PARA EXAMEN")

Aliment<-sin_na_1%>%
  filter(`Nombre DANE` %in% Alimentos)
x<-ggplot(Aliment,xlab="Producto",aes(y=`Precio/Kg`,x=`Nombre DANE`,fill=`Nombre DANE`,color=`Nombre DANE`))+geom_boxplot(alpha=0.3,outlier.colour="black", outlier.shape=16,outlier.size=1)+theme(axis.text.x = element_text(angle = 90,size=10))
ggplotly(x)


Medicam<-sin_na_1%>%
  filter(`Nombre DANE` %in% Medicamentos)
y<-ggplot(Medicam,xlab="Producto",aes(y=`Precio/Kg`,x=`Nombre DANE`,fill=`Nombre DANE`,color=`Nombre DANE`))+geom_boxplot(alpha=0.3,outlier.colour="black", outlier.shape=16,outlier.size=1)+theme(axis.text.x = element_text(angle = 90,size=10))
ggplotly(y)

Elem_p<-sin_na_1%>%
  filter(`Nombre DANE` %in% Elem_protec)
z<-ggplot(Elem_p,xlab="Producto",aes(y=`Precio/Kg`,x=`Nombre DANE`,fill=`Nombre DANE`,color=`Nombre DANE`))+geom_boxplot(alpha=0.3,outlier.colour="black", outlier.shape=16,outlier.size=1)+theme(axis.text.x = element_text(angle = 90,size=10))
ggplotly(z)


x<-ggplot(sin_na_1,xlab="Producto",aes(y=`Precio/Kg`,x=`Nombre DANE`,fill=`Nombre DANE`,color=`Nombre DANE`))+geom_boxplot(alpha=0.3,outlier.colour="black", outlier.shape=16,outlier.size=1)+theme(axis.text.x = element_text(angle = 90,size=10))
ggplotly(x)  

<!-- Seguimiento de Precios - DANE - Decreto 507 (2) -->
  <!-- ======================================================================= -->
  
  
  Column {data-width=500}
-------------------------------------
  ### **Precios de productos de proteccion de primera necesidad**
  
  ```{r}
# map tab added by Art Steinmetz
library(leaflet)
library(leafpop)
library(purrr)
library(ggplot2)
library(dplyr)
library(plotly)
#load("G:/Mi unidad/2020 SIC/Docs Coyuntura informe Super DANE/Resultados/sin_na_1.RData")
zz<-ggplot(sin_na_1,xlab="Producto",aes(y=variacion,x=`Nombre DANE`,fill=`Nombre DANE`,color=`Nombre DANE`))+geom_boxplot(alpha=0.3,outlier.colour="gray", outlier.shape=16,outlier.size=1)+theme(axis.text.x = element_text(angle = 90,size=10))
ggplotly(zz)
# incluir la tabla de muertes por mill?n de los diferentes pa?ses 
```




Aliment<-sin_na_1%>%
  filter(`Nombre DANE` %in% Alimentos)
x<-ggplot(Aliment,xlab="Producto",aes(y=`Precio/Kg`,x=`Nombre DANE`,fill=`Nombre DANE`,color=`Nombre DANE`))+geom_boxplot(alpha=0.3,outlier.colour="black", outlier.shape=16,outlier.size=1)+theme(axis.text.x = element_text(angle = 90,size=10))+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())
ggplotly(x)


library(plotly)

fig <- plot_ly() 
# fig <- fig %>% add_trace( ... )
# fig <- fig %>% layout( ... ) 

library(dash)
library(dashCoreComponents)
library(dashHtmlComponents)

app <- Dash$new()
app$layout(
  htmlDiv(
    list(
      dccGraph(figure=fig) 
    )
  )
)

app$run_server(debug=TRUE, dev_tools_hot_reload=FALSE)



library(plotly)    

p <- iris %>%
  plot_ly(
    type = 'scatter', 
    x = ~Sepal.Length, 
    y = ~Petal.Length,
    text = ~Species,
    hoverinfo = 'text',
    mode = 'markers', 
    transforms = list(
      list(
        type = 'filter',
        target = ~Species,
        operation = '=',
        value = unique(iris$Species)[1]
      )
    )) %>% layout(
      updatemenus = list(
        list(
          type = 'dropdown',
          active = 0,
          buttons = list(
            list(method = "restyle",
                 args = list("transforms[0].value", unique(iris$Species)[1]),
                 label = unique(iris$Species)[1]),
            list(method = "restyle",
                 args = list("transforms[0].value", unique(iris$Species)[2]),
                 label = unique(iris$Species)[2]),
            list(method = "restyle",
                 args = list("transforms[0].value", unique(iris$Species)[3]),
                 label = unique(iris$Species)[3])
          )
        )
      )
    )

##https://plotly.com/r/choropleth-maps/


library(plotly)
library(rjson)

url <- 'https://raw.githubusercontent.com/plotly/datasets/master/election.geojson'
geojson <- rjson::fromJSON(file=url)
url2<- "https://raw.githubusercontent.com/plotly/datasets/master/election.csv"
df <- read.csv(url2)


g <- list(
  fitbounds = "locations",
  visible = FALSE
)
fig <- plot_ly() 
fig <- fig %>% add_trace(
  type="choropleth",
  geojson=geojson,
  locations=df$district,
  z=df$Bergeron,
  colorscale="Viridis",
  featureidkey="properties.district"
)
fig <- fig %>% layout(
  geo = g
)
fig <- fig %>% colorbar(title = "Bergeron Votes")
fig <- fig %>% layout(
  title = "2013 Montreal Election"
)
fig





library(highcharter)
hcmap("countries/co/co-all")

library(dplyr)
mapdata <- get_data_from_map(download_map_data("countries/co/co-all"))
save(mapdata,file = "G:/Mi unidad/2020 SIC/Docs Coyuntura informe Super DANE/Resultados/mapacolombia.RData")
glimpse(mapdata)
mapdata$`woe-name`

#Hacer el join con divipola y luego sacar el incremento de precios promedio por encima del ipc departamental para poder hacer este mapa
#arreglar las fechas de los productos para poder hacer dashboards por producto
data_colombia <- mapdata %>% 
  select(PROVINCIA = `hc-a2`) %>% 
  mutate(X = 1:34)
glimpse(data_colombia)



hcmap("countries/co/co-all", data = data_colombia, value = "X",
      joinBy = c("hc-a2", "PROVINCIA"),
      dataLabels = list(enabled = TRUE, format = '{point.name}')) 


hcmap("countries/co/co-all", data = data_colombia, value = "X",
      joinBy = c("hc-a2", "PROVINCIA"),
      dataLabels = list(enabled = TRUE, format = '{point.name}'),
      borderColor = "#FAFAFA", borderWidth = 0.5,
      tooltip = list(valueDecimals = 2, valuePrefix = "$", valueSuffix = " USD")) %>%
  hc_mapNavigation(enabled = TRUE) %>%
  hc_title(text = "Mapa din?mico de Colombia - R Users Group - Colombia",
           align = "center", style = list(color = "#000000", fontWeight = "bold"))

easypackages::libraries(c("rjson", "stringr"))


dir <- ifelse(Sys.info()[[1]]!="Linux", "C:/CRISBEN/", "/mnt/hgfs/CRISBEN/")
ecuador <- fromJSON(file = paste0(dir,"APPS/VAE/BaseMapa/ec-all.geo.json"))

ecuador <- fromJSON(file = "https://raw.githubusercontent.com/Rusersgroup/mapa_ecuador/master/ec-all.geo.json")


load(url("https://raw.githubusercontent.com/Rusersgroup/mapa_ecuador/master/poblacion_provincia_ecuador_2010.Rdata"))
glimpse(poblacion_provincia_ecuador_2010)


highchart() %>%
  hc_title(text = "<i>Mapa din?mico de Ecuador</i> - <b>R Users Group - Ecuador</b>",
           margin = 20, align = "center",
           style = list(color = "#08338F", useHTML = TRUE)) %>% 
  hc_subtitle(text = "Poblaci?n por provincia",
              align = "center",
              style = list(color = "#0C5C9E", fontWeight = "bold")) %>% 
  hc_tooltip(followPointer =  FALSE) %>%
  hc_add_series_map(ecuador, poblacion_provincia_ecuador_2010, name = "Poblaci?n",
                    value = "Poblacion", joinBy = c("name", "Provincia"),
                    dataLabels = list(enabled = TRUE,
                                      format = '{point.properties.woe-name}')) %>%
  hc_colorAxis(minColor = "#B7D4EB", maxColor = "#08338F")  %>%
  hc_legend(align = "center", x = 0, y = -70) %>%
  hc_mapNavigation(enabled = TRUE) %>%
  hc_add_theme(hc_theme_ffx()) %>% 
  hc_add_annotation(xValue = 0, yValue = 0, title = list(text = 'Fuente: INEC')) %>% 
  hc_chart(borderColor = "#08338F",
           borderRadius = 10,
           borderWidth = 2)