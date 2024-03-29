% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/LineaVertical.R
\name{lineasignifv}
\alias{lineasignifv}
\title{Barra de significación vertical
Inserta barra de significación vertical.}
\usage{
lineasignifv(
  data,
  linea_pos,
  linea_inicio,
  linea_fin,
  linea_sup_long = NULL,
  linea_inf_long = NULL,
  linea_sup_pos = NULL,
  linea_inf_pos = NULL,
  texto,
  texto_pos_h = 0.5,
  texto_size = 3.5,
  texto_familia = "sans",
  texto_girar = "derecha",
  texto_estilo = "plain",
  texto_color = "black",
  linea_color = "black",
  linea_grosor = 1,
  linea_estilo = "solid",
  facet_column = NULL,
  facet_value = NULL
)
}
\arguments{
\item{data}{Nombre del dataframe.}

\item{linea_pos}{Posición de la línea en el eje x.}

\item{linea_inicio}{Posición del eje y inicia la línea.}

\item{linea_fin}{Posición del eje y termina la línea.}

\item{linea_sup_long}{Longitud de la línea vertical extrema superior. Se puede remplazar con linea_sup_pos.}

\item{linea_inf_long}{Longitud de la línea vertical extrema inferior. Se puede remplazar con linea_inf_pos.}

\item{linea_sup_pos}{Posición donde termina la línea vertical extrema superior. Se puede remplazar con linea_sup_long.}

\item{linea_inf_pos}{Posición donde termina la línea vertical extrema inferior. Se puede remplazar con linea_inf_log.}

\item{texto}{Texto que se va a mostrar sobre la línea Ej. “P<0.005”}

\item{texto_pos_h}{Ajusta la posición del texto con respecto a la línea, se puede mover a la izquierda o derecha. Se usa la escala de medida del eje x.}

\item{texto_size}{Ajusta el tamaño del texto}

\item{texto_familia}{Ajusta la familia del texto}

\item{texto_girar}{Gira el texto, puede ser “normal”, “izquierda” o “derecha”}

\item{texto_estilo}{Ajusta el estilo del texto (Negritas, cursiva, etc.)}

\item{texto_color}{Ajusta el color del texto}

\item{linea_color}{Cambia el color de las líneas}

\item{linea_grosor}{Cambia el grosor de las líneas}

\item{linea_estilo}{Cambia el estilo de las líneas (Continua, punteada, etc.)}

\item{facet_column}{Nombre de la columna del DataFrame se está utilizando para facetado en el gráfico ggplot2. Por ejemplo, si tu DataFrame tiene una columna 'grupo' que define las facetas, deberías establecer facet_column = "grupo".}

\item{facet_value}{Este parámetro se utiliza en combinación con facet_column para especificar en qué faceta particular quieres que aparezcan las líneas y el texto. Por ejemplo, si la columna 'grupo' tiene valores 'A', 'B' y 'C', y deseas dibujar en la faceta correspondiente a 'B', debes establecer facet_value = "B".}
}
\value{
Barra de significación
}
\description{
Barra de significación vertical
Inserta barra de significación vertical.
}
\examples{
 data <- data.frame(
 manufacturer = c('Manufacturer A', 'Manufacturer B',
                  'Manufacturer C', 'Manufacturer D', 'Manufacturer E'),
 cyl = c(4, 7, 6, 5, 8)  # Valores específicos en lugar de aleatorios
 )
 # Creating the plot
 p <- ggplot(data, aes(x = manufacturer, y = cyl)) +
   geom_col(fill = '#112446') +
   theme_minimal() +
   coord_flip()+
   xlab('Manufacturer') +
   ylab('Count of Cars') +
   ggtitle('Car Counts by Manufacturer') +
   theme(axis.text.x = element_text(angle = 45, hjust = 1))+
   ylim(0, 12)

 p
 p+
   lineasignifv(data,
                linea_pos = 10,
                linea_inicio = 5,
                linea_fin = 2,
                texto = "P= 0.005",
               linea_sup_pos =  8.5,
                linea_inf_pos = 7.5,
                linea_grosor = 3)


}
