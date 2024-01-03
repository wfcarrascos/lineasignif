#' Barra de significación horizontal
#' Inserta barra de significación horizontal.
#'
#' @param data Nombre del dataframe.
#' @param linea_inicio Posición del eje x inicia la línea.
#' @param linea_fin Posición del eje x termina la línea.
#' @param linea_pos Posición de la línea en el eje y.
#' @param linea_izq_pos Posición donde termina la línea vertical extrema de la izquierda. Se puede remplazar con linea_izq_long
#' @param linea_der_pos Posición donde termina la línea vertical extrema de la derecha. Se puede remplazar con linea_der_long
#' @param linea_izq_long Longitud de la línea vertical extrema de la izquierda. Se puede remplazar con linea_izq_pos.
#' @param linea_der_long Longitud de la línea vertical extrema de la derecha. . Se puede remplazar con linea_der_pos.
#' @param texto Texto que se va a mostrar sobre la línea Ej. “P<0.005”
#' @param texto_ajusta_pos_v Ajusta la posición del texto con respecto a la línea, se puede subir o bajar. Se usa la escala de medida del eje y.
#' @param texto_size Ajusta el tamaño del texto
#' @param texto_familia Ajusta la familia del texto
#' @param texto_estilo Ajusta el estilo del texto (Negritas, cursiva, etc.)
#' @param texto_color Ajusta el color del texto
#' @param linea_color Cambia el color de las líneas
#' @param linea_grosor Cambia el grosor de las líneas
#' @param linea_estilo Cambia el estilo de las líneas (Continua, punteada, etc.)
#' @param facet_column Nombre de la columna del DataFrame se está utilizando para facetado en el gráfico ggplot2. Por ejemplo, si tu DataFrame tiene una columna 'grupo' que define las facetas, deberías establecer facet_column = "grupo".
#' @param facet_value Este parámetro se utiliza en combinación con facet_column para especificar en qué faceta particular quieres que aparezcan las líneas y el texto. Por ejemplo, si la columna 'grupo' tiene valores 'A', 'B' y 'C', y deseas dibujar en la faceta correspondiente a 'B', debes establecer facet_value = "B".
#'
#' @return LineaHorizontal
#'
#' @importFrom ggplot2 geom_segment aes geom_text geom_path ggplot
#' @importFrom dplyr %>% filter
#' @importFrom tibble tibble
#'
#' @export
#'
#' @examples
#'  data <- data.frame(
#' manufacturer = c('Manufacturer A', 'Manufacturer B',
#'                 'Manufacturer C', 'Manufacturer D', 'Manufacturer E'),
#' cyl = c(4, 7, 6, 5, 8)  # Valores específicos en lugar de aleatorios
#' )
#' # Creating the plot
#' p <- ggplot(data, aes(x = manufacturer, y = cyl)) +
#'  geom_col(fill = '#112446') +
#'  theme_minimal() +
#'  xlab('Manufacturer') +
#'  ylab('Count of Cars') +
#'  ggtitle('Car Counts by Manufacturer') +
#'  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
#'  ylim(0, 12) #Es para que el texto no se recorte+
#' coord_flip()
#'
#' print(p)
#'
#' #Crear la linea de significación
#' ln <- lineasignif(data,
#'                  linea_pos = 10,
#'                  linea_inicio = 1,
#'                  linea_fin = 5,
#'                  texto = "P= 0.005",
#'                  linea_izq_long =  5,
#'                  linea_der_pos =  8.3,
#'                  linea_grosor = 3)
#'
#'p+ln
#'
#'
lineasignif <- function(data, linea_inicio, linea_fin, linea_pos,
                        linea_izq_pos = NULL, linea_der_pos = NULL,
                        linea_izq_long = NULL, linea_der_long = NULL, texto,
                        texto_ajusta_pos_v = 0, texto_size = 3.5, texto_familia = "sans",
                        texto_estilo = "plain", texto_color = "black", linea_color = "black",
                        linea_grosor = 1, linea_estilo = "solid",  # New parameter for text color
                        facet_column = NULL, facet_value = NULL) {

  if (!is.null(facet_column) & !is.null(facet_value)) {
    data <- data %>% filter(!!as.name(facet_column) == facet_value)
  }

  # Define la posición final de las líneas verticales
  final_izq <- if (!is.null(linea_izq_long)) {
    linea_pos - linea_izq_long
  } else if (!is.null(linea_izq_pos)) {
    linea_izq_pos
  } else {
    linea_pos
  }

  final_der <- if (!is.null(linea_der_long)) {
    linea_pos - linea_der_long
  } else if (!is.null(linea_der_pos)) {
    linea_der_pos
  } else {
    linea_pos
  }

  # Crea el dataframe para geom_path asegurando un camino continuo
  ejx = c(linea_inicio, linea_inicio, linea_inicio, linea_fin, linea_fin, linea_fin)
  ejy = c(linea_pos, final_izq, linea_pos, linea_pos, final_der, linea_pos)
  grupo = c(1,1,1,1,1,1)

  path_data <- tibble(
    ejx,
    ejy,
    grupo  # Este es necesario para que geom_path sepa que todos estos puntos están conectados
  )

  layers <- list(
    geom_path(data = path_data, aes(x = ejx, y = ejy, group = grupo),
              color = linea_color, size = linea_grosor, linetype = linea_estilo),
    geom_text(data = data, aes(x = (linea_inicio + linea_fin) / 2, y = linea_pos + texto_ajusta_pos_v, label = texto),
              color = texto_color,  # Use the new parameter for text color
              size = texto_size, family = texto_familia, fontface = texto_estilo, vjust = -1)
  )

  return(layers)
}

