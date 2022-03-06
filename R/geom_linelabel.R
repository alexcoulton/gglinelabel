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
        spread.factor = NA
        ),
    draw_key = draw_key_point,

    draw_panel = function(data, panel_params, coord , arrow = NULL,
        lineend = "butt", linejoin = "round", linemitre = 10,
        na.rm = FALSE, linetype = 'solid'){ 

        nudge_y = unique(data$nudge_y)
        spread.iter = unique(data$spread.iter)
        cloud.straight = unique(data$cloud.straight)
        label.straight = unique(data$label.straight)
        min.y = unique(data$min.y)
        max.y = unique(data$max.y)
        min.x = unique(data$min.x)
        max.x = unique(data$max.x)
        fan.height = unique(data$fan.height)
        rotate = unique(data$rotate)
        nudge_label = data$nudge_label
        spread.factor = unique(data$spread.factor)

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

        if(unique(data$layout) == 'cloud'){

            ############################
            #PATH ELEMENTS 
            ############################

            # must be sorted on group
            data.cloud2 = make.fanline.df.cloud(
                data,
                nudge_y,
                cloud.straight,
                fan.height = fan.height,
                min.x = min.x,
                max.x = max.x
            )


            data.cloud2 <- data.cloud2[order(data.cloud2$group), , drop = FALSE]
            munched <- coord_munch(coord, data.cloud2, panel_params)

            # Silently drop lines with less than two points, preserving order
            rows <- stats::ave(seq_len(nrow(munched)), munched$group, FUN = length)
            munched <- munched[rows >= 2, ]
            if (nrow(munched) < 2) return(zeroGrob())

            # Work out grouping variables for grobs
            n <- nrow(munched)
            group_diff <- munched$group[-1] != munched$group[-n]
            start <- c(TRUE, group_diff)
            end <-   c(group_diff, TRUE)

            id <- match(munched$group, unique(munched$group))
            path_elements = polylineGrob(
                munched$x, munched$y, id = id,
                default.units = "native", arrow = arrow,
                gp = gpar(
                  col = alpha(munched$colour, munched$alpha)[start],
                  fill = alpha(munched$colour, munched$alpha)[start],
                  #lwd = munched$size[start] * .pt,
                  lwd = 1,
                  lty = munched$linetype[start],
                  lineend = lineend,
                  linejoin = linejoin,
                  linemitre = linemitre
                )
            )

            ############################
            #LINE ELEMENT 
            ############################

            data = data[!is.na(data$label), ]
            data.cloud = make.hline.df.cloud(data, nudge_y)
            #data.cloud = data
            coords <- coord$transform(data.cloud, panel_params)
            #coords <- coord$transform(data, panel_params)

            coords$linetype = 'solid'

            lwd1 = coords$size * .pt
            if(length(lwd1) == 0) lwd1 = NULL
            line_elements = grid::linesGrob(
                coords$x, coords$y, 
                default.units = "native",
                gp = grid::gpar(
                    col = coords$colour,
                    lwd = lwd1,
                    lty = coords$linetype
                )
            )

            ############################
            #POINT ELEMENTS 
            ############################

            data.cloud3 = make.point.df.cloud(data, nudge_y)
            coords <- coord$transform(data.cloud3, panel_params)
            point_elements = grid::pointsGrob(
                coords$x, coords$y,
                #pch = coords$shape,
                size = unit(1, 'mm'),
                gp = grid::gpar(col = coords$colour)
            )

            ############################
            #LABEL ELEMENTS 
            ############################

            data.label = make.label.df(
                data,
                nudge_y,
                label.straight,
                fan.height = fan.height,
                min.x = min.x,
                max.x = max.x,
                max.y = max.y
            )


            lab <- data.label$label
            lab <- ggplot2:::parse_safe(as.character(lab))

            data.label <- coord$transform(data.label, panel_params)

            if (is.character(data.label$vjust)) {
              data.label$vjust <- compute_just(data.label$vjust, data.label$y, data.label$x, data.label$angle)
            }
            if (is.character(data.label$hjust)) {
              data.label$hjust <- compute_just(data.label$hjust, data.label$x, data.label$y, data.label$angle)
            }

            label_elements = textGrob(
              lab,
              data.label$x, data.label$y, default.units = "native",
              hjust = data.label$hjust, vjust = data.label$vjust,
              rot = data.label$angle,
              gp = gpar(
                col = alpha(text.color, data.label$alpha),
                fontsize = data.label$text.size * .pt,
                fontfamily = data.label$family,
                fontface = data.label$fontface,
                lineheight = data.label$lineheight
              )
            )

            gList(line_elements, point_elements, path_elements, label_elements)
        } else if(unique(data$layout) == 'fan'){
            ############################
            #FAN ELEMENTS 
            ############################

            # must be sorted on group
            data.fan = make.line.df.fan(
                data,
                nudge_y,
                min.y = min.y,
                max.y = max.y,
                fan.height = fan.height,
                min.x = min.x,
                max.x = max.x,
                spread.factor = spread.factor
            )
            data.fan <- data.fan[order(data.fan$group), , drop = FALSE]
            munched <- coord_munch(coord, data.fan, panel_params)

            # Silently drop lines with less than two points, preserving order
            rows <- stats::ave(seq_len(nrow(munched)), munched$group, FUN = length)
            munched <- munched[rows >= 2, ]
            if (nrow(munched) < 2) return(zeroGrob())

            # Work out grouping variables for grobs
            n <- nrow(munched)
            group_diff <- munched$group[-1] != munched$group[-n]
            start <- c(TRUE, group_diff)
            end <-   c(group_diff, TRUE)

            id <- match(munched$group, unique(munched$group))
            path_elements = polylineGrob(
                munched$x, munched$y, id = id,
                default.units = "native", arrow = arrow,
                gp = gpar(
                  col = alpha(munched$colour, munched$alpha)[start],
                  fill = alpha(munched$colour, munched$alpha)[start],
                  #lwd = munched$size[start] * .pt,
                  lwd = 1,
                  lty = munched$linetype[start],
                  lineend = lineend,
                  linejoin = linejoin,
                  linemitre = linemitre
                )
            )

            ############################
            #LABEL ELEMENTS 
            ############################

            data.label = make.label.df(
                data,
                nudge_y,
                fan.height = fan.height,
                min.x = min.x,
                max.x = max.x,
                max.y = max.y,
                spread.factor = spread.factor
            )

            lab <- data.label$label
            lab <- ggplot2:::parse_safe(as.character(lab))

            data.label <- coord$transform(data.label, panel_params)

            if (is.character(data.label$vjust)) {
              data.label$vjust <- compute_just(data.label$vjust, data.label$y, data.label$x, data.label$angle)
            }
            if (is.character(data.label$hjust)) {
              data.label$hjust <- compute_just(data.label$hjust, data.label$x, data.label$y, data.label$angle)
            }

            label_elements = textGrob(
              lab,
              data.label$x, data.label$y, default.units = "native",
              hjust = data.label$hjust, vjust = data.label$vjust,
              rot = data.label$angle,
              gp = gpar(
                col = alpha(text.color, data.label$alpha),
                fontsize = data.label$text.size * .pt,
                fontfamily = data.label$family,
                fontface = data.label$fontface,
                lineheight = data.label$lineheight
              )
            )

            gList(path_elements, label_elements)

        } else if(unique(data$layout) == 'star'){
            ############################
            #STAR ELEMENTS 
            ############################
            # must be sorted on group
            #data.star = data[!is.na(data$label), ]
            data.star = make.line.df.star(data, nudge_y, spread.iter = spread.iter, min.y = min.y, max.y = max.y, min.x = min.x, max.x = max.x, rotate = rotate)
            data.star <- data.star[order(data.star$group), , drop = FALSE]
            munched <- coord_munch(coord, data.star, panel_params)

            # Silently drop lines with less than two points, preserving order
            rows <- stats::ave(seq_len(nrow(munched)), munched$group, FUN = length)
            munched <- munched[rows >= 2, ]
            if (nrow(munched) < 2) return(zeroGrob())

            # Work out grouping variables for grobs
            n <- nrow(munched)
            group_diff <- munched$group[-1] != munched$group[-n]
            start <- c(TRUE, group_diff)
            end <-   c(group_diff, TRUE)

            id <- match(munched$group, unique(munched$group))
            path_elements = polylineGrob(
                munched$x, munched$y, id = id,
                default.units = "native", arrow = arrow,
                gp = gpar(
                  col = alpha(munched$colour, munched$alpha)[start],
                  fill = alpha(munched$colour, munched$alpha)[start],
                  #lwd = munched$size[start] * .pt,
                  lwd = 1,
                  lty = munched$linetype[start],
                  lineend = lineend,
                  linejoin = linejoin,
                  linemitre = linemitre
                )
            )

            data.star.split = split(data.star, data.star$group)
            data.star.x2 = bind_rows(lapply(data.star.split, function(x) x[2, ]))$x

            ############################
            #LABEL ELEMENTS 
            ############################

            data.label = make.label.df(data, nudge_y, fan.height = fan.height, min.x = min.x, max.x = max.x, max.y = max.y)
            data.label$x = data.star.x2

            lab <- data.label$label
            lab <- ggplot2:::parse_safe(as.character(lab))

            data.label <- coord$transform(data.label, panel_params)

            if (is.character(data.label$vjust)) {
              data.label$vjust <- compute_just(data.label$vjust, data.label$y, data.label$x, data.label$angle)
            }
            if (is.character(data.label$hjust)) {
              data.label$hjust <- compute_just(data.label$hjust, data.label$x, data.label$y, data.label$angle)
            }

            label_elements = textGrob(
              lab,
              data.label$x, data.label$y, default.units = "native",
              hjust = data.label$hjust, vjust = data.label$vjust,
              rot = data.label$angle,
              gp = gpar(
                col = alpha(text.color, data.label$alpha),
                fontsize = data.label$text.size * .pt,
                fontfamily = data.label$family,
                fontface = data.label$fontface,
                lineheight = data.label$lineheight
              )
            )

            gList(path_elements, label_elements)


        } else if(unique(data$layout) == 'straight'){
            ############################
            #STAR ELEMENTS 
            ############################
            # must be sorted on group
            data.straight = data[!is.na(data$label), ]
            data.straight = make.line.df.straight(
                data.straight,
                min.y = min.y,
                max.y = max.y,
                spread.factor = spread.factor
            )
            data.straight <- data.straight[order(data.straight$group), , drop = FALSE]
            munched <- coord_munch(coord, data.straight, panel_params)

            # Silently drop lines with less than two points, preserving order
            rows <- stats::ave(seq_len(nrow(munched)), munched$group, FUN = length)
            munched <- munched[rows >= 2, ]
            if (nrow(munched) < 2) return(zeroGrob())

            # Work out grouping variables for grobs
            n <- nrow(munched)
            group_diff <- munched$group[-1] != munched$group[-n]
            start <- c(TRUE, group_diff)
            end <-   c(group_diff, TRUE)

            id <- match(munched$group, unique(munched$group))

            #head(munched)
            #munched$y 
            #ylim1 = panel_params$y$get_limits()
            #ylim1

            path_elements = polylineGrob(
                munched$x, munched$y, id = id,
                default.units = "native", arrow = arrow,
                gp = gpar(
                  col = alpha(munched$colour, munched$alpha)[start],
                  fill = alpha(munched$colour, munched$alpha)[start],
                  #lwd = munched$size[start] * .pt,
                  lwd = 1,
                  lty = munched$linetype[start],
                  lineend = lineend,
                  linejoin = linejoin,
                  linemitre = linemitre
                )
            )

            data.straight.split = split(data.straight, data.straight$group)
            data.straight.x2 = bind_rows(lapply(data.straight.split, function(x) x[2, ]))$x

            ############################
            #LABEL ELEMENTS 
            ############################

            data.label = make.label.df(data, fan.height = fan.height, min.x = min.x, max.x = max.x, max.y = max.y)
            data.label$y = data.label$y + unique(data.label$nudge_y)
            data.label$x = data.straight.x2


            lab <- data.label$label
            lab <- ggplot2:::parse_safe(as.character(lab))

            data.label <- coord$transform(data.label, panel_params)

            if (is.character(data.label$vjust)) {
              data.label$vjust <- compute_just(data.label$vjust, data.label$y, data.label$x, data.label$angle)
            }
            if (is.character(data.label$hjust)) {
              data.label$hjust <- compute_just(data.label$hjust, data.label$x, data.label$y, data.label$angle)
            }

            label_elements = textGrob(
              lab,
              data.label$x, data.label$y, default.units = "native",
              hjust = data.label$hjust, vjust = data.label$vjust,
              rot = data.label$angle,
              gp = gpar(
                col = alpha(text.color, data.label$alpha),
                fontsize = data.label$text.size * .pt,
                fontfamily = data.label$family,
                fontface = data.label$fontface,
                lineheight = data.label$lineheight
              )
            )

            gList(path_elements, label_elements)

        }

    }
)

############################
#PROCESSING FUNCTIONS 
############################

make.line.df.fan = function(
    data.df,
    nudge_y,
    min.y = NA,
    fan.height = NA,
    min.x = NA,
    max.x = NA,
    max.y = NA,
    spread.factor = 1
){
    data.df2 = data.df
    data.df.naomit = data.df[!is.na(data.df$label), ]
    data.df2 = data.df.naomit

    if(is.na(min.x)) min.x = min(data.df2$x)
    if(is.na(max.x)) max.x = max(data.df2$x)

    orig.x = data.df2$x
    x2 = add.spread.factor(orig.x, spread.factor)

    y2 = max(data.df2$y) + (max(data.df2$y) / 5) + nudge_y


    data.df2$group = 1:nrow(data.df2)

    verticallinedf = data.df2
    #colnames(verticallinedf) = c('x1', 'y1', 'x2', 'y2', 'group')
    verticallinedf1 = verticallinedf
    verticallinedf2 = verticallinedf


    verticallinedf2$y = max(data.df2$y) + (max(data.df2$y) / 10) + nudge_y

    if(!is.na(min.y)) verticallinedf1$y = min.y
    if(!is.na(min.y)) verticallinedf2$y = verticallinedf2$y + min.y
    if(!is.na(max.y)) verticallinedf2$y = max.y

    vert.line.df = bind_rows(verticallinedf1, verticallinedf2)

    if(!any(is.na(data.df2$nudge_label))) x2 = x2 + data.df2$nudge_label

    linelabeldf = data.df2
    linelabeldf1 = linelabeldf
    linelabeldf2 = linelabeldf
    linelabeldf2$x = x2
    linelabeldf2$y = y2

    if(!is.na(fan.height)) linelabeldf2$y = linelabeldf2$y + fan.height

    linelabeldf1$y = max(data.df2$y) + (max(data.df2$y) / 10) + nudge_y

    if(!is.na(min.y)) linelabeldf1$y = linelabeldf1$y + min.y
    if(!is.na(max.y)) linelabeldf1$y = max.y
    if(!is.na(min.y) & is.na(fan.height)) linelabeldf2$y = linelabeldf2$y + min.y


    data.df3 = bind_rows(linelabeldf1, linelabeldf2)
    data.df3$label = data.df2$label
    data.df3

    data.df4 = bind_rows(vert.line.df, data.df3)
    #data.df4[data.df4$group == 6, ]
    data.df4
}


make.line.df.star = function(
    data.df,
    nudge_y = 0,
    spread.iter = 1,
    min.y = NA,
    max.y = NA,
    min.x = NA,
    max.x = NA,
    rotate = NA
){
    data.df2 = data.df[!is.na(data.df$label), ]
    if(is.na(min.x)) min.x = min(data.df$x)
    if(is.na(max.x)) max.x = max(data.df$x)
    x2 = data.df2$x

    x2[[1]] = min.x
    x2[[length(x2)]] = max.x

    spread.x = function(x){
        for(i in 2:(length(x) - 1)){
            x[i] = (x[i + 1] + x[i - 1]) / 2
        }
        x
    }

    for(i in 1:spread.iter){
        x2 = spread.x(x2)
    }

    y2 = max(data.df2$y) + (max(data.df2$y) / 5) + nudge_y

    data.df2$group = 1:nrow(data.df2)
    order(data.df2$x)

    linelabeldf = data.df2
    linelabeldf1 = linelabeldf
    linelabeldf2 = linelabeldf


    if(!is.na(min.y)) linelabeldf1$y = min.y
    if(!is.na(max.y)) y2 = max.y
    if(!any(is.na(data.df2$nudge_label))) x2 = x2 + data.df2$nudge_label

    linelabeldf2$x = x2
    linelabeldf2$y = y2


    data.df3 = bind_rows(linelabeldf1, linelabeldf2)
    data.df3
}

add.spread.factor = function(x, spread.factor){
    #x: numeric vector of x coordinates
    min.x = min(x)
    max.x = max(x)
    x.spread = seq(
        min.x,
        max.x,
        length.out = length(x)
    )

    spread2 = x.spread /  x 
    x.dist = x.spread - x
    x.dist2 = x.dist * spread.factor
    x = x + x.dist2
    x
}


make.line.df.straight = function(
    data.df,
    min.y = NA,
    max.y = NA,
    spread.factor = 0
){
    data.df2 = data.df

    x2 = data.df2$x

    if(!is.na(spread.factor)){
        x2 = add.spread.factor(x2, spread.factor)
    }

    nudge_y = unique(data.df$nudge_y)

    y2 = max(data.df2$y) + (max(data.df2$y) / 5) + nudge_y

    data.df2$group = 1:nrow(data.df2)
    order(data.df2$x)

    linelabeldf = data.df2
    linelabeldf1 = linelabeldf
    if(!is.na(min.y)) linelabeldf1$y = min.y
    if(!is.na(max.y)) y2 = max.y
    linelabeldf2 = linelabeldf

    if(!any(is.na(data.df2$nudge_label))) x2 = x2 + data.df2$nudge_label


    linelabeldf2$x = x2
    linelabeldf2$y = y2

    data.df3 = bind_rows(linelabeldf1, linelabeldf2)
    data.df3
}


make.hline.df.cloud = function(data.df, nudge_y){
    #makes horizontal line for cloud layout
    data.df2 = data.df[c('x', 'y')]
    data.df2 = data.df[1:2, ]
    data.df2$x = c(min(data.df$x), max(data.df$x))
    data.df2$y = c(max(data.df$y) + max(data.df$y) / 10 + nudge_y, max(data.df$y) + max(data.df$y) / 10 + nudge_y)
    data.df2
}


make.fanline.df.cloud = function(
    data.df,
    nudge_y = 0,
    cloud.straight = F,
    fan.height = NA,
    min.x = NA,
    max.x = NA
){
    if(is.na(min.x)) min.x = min(data.df$x)
    if(is.na(max.x)) max.x = max(data.df$x)
    #makes fan lines for cloud layout
    data.df
    data.df.naomit = data.df[!is.na(data.df$label), ]

    if(cloud.straight == F){
        x2 = seq(
            min.x,
            max.x,
            length.out = nrow(data.df.naomit)
        )
    } else {
        x2 = data.df.naomit$x
    }

    data.df = data.df.naomit

    if(!any(is.na(data.df.naomit$nudge_label))) x2 = x2 + data.df.naomit$nudge_label

    y2 = max(data.df$y) + (max(data.df$y) / 5) + nudge_y

    data.df$group = 1:nrow(data.df)
    order(data.df$x)

    linelabeldf1 = data.df
    linelabeldf1$y = max(data.df$y) + (max(data.df$y) / 10) + nudge_y

    linelabeldf2 = data.df
    linelabeldf2$x = x2
    linelabeldf2$y = y2
    if(!is.na(fan.height)) linelabeldf2$y = linelabeldf2$y + fan.height

    data.df3 = bind_rows(linelabeldf1, linelabeldf2)
    data.df3
}

make.point.df.cloud = function(data.df, nudge_y){
    #makes points for cloud layout
    data.df = data.df[!is.na(data.df$label), ]
    data.df2 = data.df[c('x', 'y')]

    data.df2$y = max(data.df$y) + max(data.df$y) / 10 + nudge_y
    data.df2
}

make.label.df = function(
    data.df,
    nudge_y = 0,
    label.straight = F,
    fan.height = NA,
    min.x = NA,
    max.x = NA,
    max.y = NA,
    spread.factor = 1
){
    data.df = data.df[!is.na(data.df$label), ]
    data.df$y = max(data.df$y) + (max(data.df$y) / 5) + nudge_y
    if(!is.na(fan.height)) data.df$y = data.df$y + fan.height
    if(!is.na(max.y)) data.df$y = max.y
    if(!is.na(fan.height) & !is.na(max.y)) data.df$y = max.y + fan.height

    data.df.naomit = data.df[!is.na(data.df$label), ]
    if(is.na(min.x)) min.x = min(data.df$x)
    if(is.na(max.x)) max.x = max(data.df$x)
    if(label.straight == T){
        x2 = data.df.naomit$x
    } else {
        x2 = add.spread.factor(data.df.naomit$x, spread.factor)
    }
    if(!any(is.na(data.df.naomit$nudge_label))) x2 = x2 + data.df.naomit$nudge_label
    data.df.naomit$x = x2
    data.df.naomit
}

