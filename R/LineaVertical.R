#' Barra de significación vertical
#' Inserta barra de significación vertical.
#'
#' @param data Nombre del dataframe.
#' @param linea_pos Posición de la línea en el eje x.
#' @param linea_inicio Posición del eje y inicia la línea.
#' @param linea_fin Posición del eje y termina la línea.
#' @param linea_sup_long Longitud de la línea vertical extrema superior. Se puede remplazar con linea_sup_pos.
#' @param linea_inf_long Longitud de la línea vertical extrema inferior. Se puede remplazar con linea_inf_pos.
#' @param linea_sup_pos Posición donde termina la línea vertical extrema superior. Se puede remplazar con linea_sup_long.
#' @param linea_inf_pos Posición donde termina la línea vertical extrema inferior. Se puede remplazar con linea_inf_log.
#' @param texto Texto que se va a mostrar sobre la línea Ej. “P<0.005”
#' @param texto_pos_h Ajusta la posición del texto con respecto a la línea, se puede mover a la izquierda o derecha. Se usa la escala de medida del eje x.
#' @param texto_size Ajusta el tamaño del texto
#' @param texto_familia Ajusta la familia del texto
#' @param texto_girar Gira el texto, puede ser “normal”, “izquierda” o “derecha”
#' @param texto_estilo Ajusta el estilo del texto (Negritas, cursiva, etc.)
#' @param texto_color Ajusta el color del texto
#' @param linea_color Cambia el color de las líneas
#' @param linea_grosor Cambia el grosor de las líneas
#' @param linea_estilo Cambia el estilo de las líneas (Continua, punteada, etc.)
#' @param facet_column Nombre de la columna del DataFrame se está utilizando para facetado en el gráfico ggplot2. Por ejemplo, si tu DataFrame tiene una columna 'grupo' que define las facetas, deberías establecer facet_column = "grupo".
#' @param facet_value Este parámetro se utiliza en combinación con facet_column para especificar en qué faceta particular quieres que aparezcan las líneas y el texto. Por ejemplo, si la columna 'grupo' tiene valores 'A', 'B' y 'C', y deseas dibujar en la faceta correspondiente a 'B', debes establecer facet_value = "B".
#'
#' @return Barra de significación
#'
#' @importFrom ggplot2 geom_segment aes geom_text geom_path ggplot
#' @importFrom dplyr %>% filter case_when
#' @importFrom tibble tibble
#'
#' @export
#'
#' @examples
#'  data <- data.frame(
#'  manufacturer = c('Manufacturer A', 'Manufacturer B',
#'                   'Manufacturer C', 'Manufacturer D', 'Manufacturer E'),
#'  cyl = c(4, 7, 6, 5, 8)  # Valores específicos en lugar de aleatorios
#'  )
#'  # Creating the plot
#'  p <- ggplot(data, aes(x = manufacturer, y = cyl)) +
#'    geom_col(fill = '#112446') +
#'    theme_minimal() +
#'    coord_flip()+
#'    xlab('Manufacturer') +
#'    ylab('Count of Cars') +
#'    ggtitle('Car Counts by Manufacturer') +
#'    theme(axis.text.x = element_text(angle = 45, hjust = 1))+
#'    ylim(0, 12)
#'
#'  p
#'  p+
#'    lineasignifv(data,
#'                 linea_pos = 10,
#'                 linea_inicio = 5,
#'                 linea_fin = 2,
#'                 texto = "P= 0.005",
#'                linea_sup_pos =  8.5,
#'                 linea_inf_pos = 7.5,
#'                 linea_grosor = 3)
#'
#'
lineasignifv <- function(data, linea_pos, linea_inicio, linea_fin,
                         linea_sup_long = NULL, linea_inf_long = NULL, linea_sup_pos = NULL, linea_inf_pos = NULL, texto,
                         texto_pos_h = 0.5, texto_size = 3.5, texto_familia= "sans", texto_girar = "derecha",
                         texto_estilo = "plain", texto_color = "black", linea_color = "black", linea_grosor = 1, linea_estilo = "solid",
                         facet_column = NULL, facet_value = NULL) {

  if (!is.null(facet_column) & !is.null(facet_value)) {
    data <- data %>% filter(!!as.name(facet_column) == facet_value)
  }

  # Define la posición final de las líneas perpendiculares
  final_sup <- if (!is.null(linea_sup_long)) {
    linea_inicio + linea_sup_long
  } else if (!is.null(linea_sup_pos)) {
    linea_sup_pos
  } else {
    linea_inicio
  }

  final_inf <- if (!is.null(linea_inf_long)) {
    linea_fin - linea_inf_long
  } else if (!is.null(linea_inf_pos)) {
    linea_inf_pos
  } else {
    linea_fin
  }

  # Crea el dataframe para geom_path asegurando un camino continuo
  ejx = c(linea_inicio, linea_inicio, linea_inicio, linea_fin, linea_fin, linea_fin)
  ejy = c(linea_pos, final_sup, linea_pos, linea_pos, final_inf, linea_pos)
  grupo = c(1,1,1,1,1,1)

  path_data <- tibble(
    ejx,
    ejy,
    grupo  # Este es necesario para que geom_path sepa que todos estos puntos están conectados
  )

  layers <- list(
    geom_path(data = path_data, aes(x = ejx, y = ejy, group = grupo),
              color = linea_color, size = linea_grosor, linetype = linea_estilo),
    geom_text(data = data, aes(y = linea_pos + texto_pos_h, x = (linea_inicio + linea_fin) / 2, label = texto),
              color = texto_color,  # Use the new parameter for text color
              size = texto_size, family = texto_familia, fontface = texto_estilo, hjust = 0.5, vjust = 0.5, angle = case_when(
                texto_girar == "normal" ~ 0,
                texto_girar == "izquierda" ~ 90,
                texto_girar == "derecha" ~ -90,
                TRUE ~ 0  # Default to 0 if an unrecognized value is passed
              ))
  )

  return(layers)
}
