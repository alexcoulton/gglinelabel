############################
#GEOM FUNCTIONS 
############################

#' ggplot2 extension for labelling of line graphs
#'
#' Used for labelling line graphs, for example genomic features
#' (such as genes or molecular markers) along a chromosome.
#'
#' @param shape Shape of point in cloud layout
#' @param colour Colour of lines and text
#' @param linetype The type of line leading to label, e.g. 'solid', 'dashed', 'dotted'
#' @param text.size Size of the labels
#' @param angle Angle of the labels
#' @param hjust Horizontal justification of labels
#' @param vjust Vertical justification of labels
#' @param alpha Transparency of elements
#' @param family Font family
#' @param fontface Font face
#' @param lineheight Height of lines
#' @param layout Layout of lines, options are 'fan', 'star', 'cloud' and 'straight'
#' @param nudge_y Integer, specify a value to adjust the position of the linelabels on the y-axis
#' @param nudge_x Integer, specify a value to adjust the position of the linelabels on the x-axis
#' @param fan.height Numeric, ranges from 0 to 1, proportion of line that fans out
#' 
#' @examples
#' data(chr1)
#' ggplot(chr1, aes(x = start, y = expression, label = gene)) +
#'     geom_line() + geom_linelabel()
#' @return a ggproto object that can be added to a ggplot2 object
#' @import ggplot2
#' @import dplyr
#' @export
geom_linelabel <- function(mapping = NULL, data = NULL, stat = "identity",
                              position = "identity", na.rm = FALSE, show.legend = NA, 
                              inherit.aes = TRUE, ...) {
  layer(
    geom = GeomLineLabel, mapping = mapping,  data = data, stat = stat, 
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}

#' @import ggplot2
#' @import dplyr
#' @import grid
GeomLineLabel <- ggproto("GeomLineLabel", Geom,
    required_aes = c("x", "y"),
    default_aes = aes(shape = 19, colour = "black", linetype = 'dotted',
        text.size = 4, angle = 90, hjust = 0,
        vjust = 0.5, alpha = NA, family = "", fontface = 1, lineheight = 1.2,
        layout = 'fan',
        nudge_y = 0,
        nudge_x = 0,
        spread.iter = 1,
        cloud.straight = F,
        label.straight = F,
        min.y = NA,
        fan.height = NA,
        label = NA,
        min.x = NA,
        max.x = NA,
        rotate = NA,
        max.y = NA,
        text.colour = 'black',
        nudge_label = NA,
        spread.factor = 1,
        spread.threshold = Inf
        ),
    draw_key = draw_key_point,

    draw_panel = function(data, panel_params, coord , arrow = NULL,
        lineend = "butt", linejoin = "round", linemitre = 10,
        na.rm = FALSE, linetype = 'solid'){ 

        nudge_y = unique(data$nudge_y)
        spread.iter = unique(data$spread.iter)
        cloud.straight = unique(data$cloud.straight)
        label.straight = unique(data$label.straight)

        #set sensible defaults for min.y
        if(is.na(unique(data$min.y))){
            min.y = max(data$y, na.rm = T)
        } else {
            min.y = unique(data$min.y)
        }

        #same for max y
        if(is.na(unique(data$max.y))){
            scale1 = abs(min(data$y, na.rm = T) - max(data$y, na.rm = T)) / 10
            max.y = max(data$y, na.rm = T) + scale1
        } else {
            max.y = unique(data$max.y)
        }

        #set sensible defaults for min.x
        if(is.na(unique(data$min.x))){
            min.x = min(data$x, na.rm = T)
        } else {
            min.x = unique(data$min.x)
        }

        #same for max.x
        if(is.na(unique(data$max.x))){
            max.x = max(data$x, na.rm = T) 
        } else {
            max.x = unique(data$max.x)
        }

        fan.height = unique(data$fan.height)
        rotate = unique(data$rotate)
        nudge_label = data$nudge_label
        spread.factor = unique(data$spread.factor)
        spread.threshold = unique(data$spread.threshold)
        text.color = unique(data$text.colour)

        #all.text.color = c(text.color, text.colour)
        #if(any(all.text.color != 'black')){
            #text.color = all.text.color[all.text.color != 'black']
        #} else {
            #text.color = 'black'
        #}

       ############################
       #GEOM_PATH DRAW FUNCTION 
       ############################ 
        if (!anyDuplicated(data$group)) {
            ggplot2:::message_wrap("geom_path: Each group consists of only one observation. ",
            "Do you need to adjust the group aesthetic?")
        }

        if (any(duplicated(data$x))){
            ggplot2:::message_wrap("geom_linelabel: Duplicate x values are present. ",
                "This will result in conflicts.")
        }

        #if(any(is.na(data$label))){
            #message_wrap("geom_linelabel: Some labels are NA; removing these.")
            #data = data[!is.na(data$label), ]
        #}

        render.params = list(
                data = data,
                coord = coord,
                panel_params = panel_params,
                lineend = lineend,
                linejoin = linejoin,
                linemitre = linemitre,
                linetype = linetype,
                na.rm = na.rm,
                arrow = arrow,
                nudge_y = nudge_y,
                spread.iter = spread.iter,
                cloud.straight = cloud.straight,
                label.straight = label.straight,
                min.y = min.y,
                max.y = max.y,
                min.x = min.x,
                max.x = max.x,
                fan.height = fan.height,
                rotate = rotate,
                nudge_label = nudge_label,
                spread.factor = spread.factor,
                spread.threshold = spread.threshold,
                text.color = text.color
        )

        if(unique(data$layout) == 'cloud'){
            return(render.cloud(render.params))
        }

        if(unique(data$layout) == 'star'){
            return(render.star(render.params))
        }

        if(unique(data$layout) == 'straight'){
            return(render.straight(render.params))
        }

        if(unique(data$layout) == 'fan'){
            return(render.fan(render.params))
        }

    }
)

