load_all('~/work/external.repos/ggplot2_testing/')

source('~/work/external.repos/IceCast/R/selfIntersections.R')

library(reshape2)
source('~/work/ucl/scripts/misc/functions.R')
library(devtools)
library(ggplot2testing)


#devtools::install('~/work/external.repos/ggplot2_testing/')
#source('~/work/external.repos/ggplot2_testing/R/geom-point.r')
#source('~/work/external.repos/ggplot2_testing/R/layer.r')


chr1 = get.genes(1, 0, 1000000000000)
chr1$expression = rnorm(nrow(chr1), mean = 100, sd = 20)
chr1$expression[chr1$expression < 0] = 0
chr1 = chr1[1:100, ]

head(chr1)

label.df = chr1[c('gene', 'expression')]
head(label.df)

test_function = function(){
    layer_data(data)
}

head(chr1)





linelabel_chr = chr1[c('start', 'expression', 'gene')]
colnames(linelabel_chr) = c('x', 'y', 'label')

make.line.df.fan = function(data.df, nudge_y){
    data.df2 = data.df
    data.df.naomit = data.df[!is.na(data.df$label), ]
    x2 = seq(
        min(data.df2$x),
        max(data.df2$x),
        length.out = nrow(data.df.naomit)
    )

    y2 = max(data.df2$y) + (max(data.df2$y) / 5) + nudge_y

    data.df2 = data.df.naomit

    data.df2$group = 1:nrow(data.df2)

    verticallinedf = data.df2
    #colnames(verticallinedf) = c('x1', 'y1', 'x2', 'y2', 'group')
    verticallinedf1 = verticallinedf
    verticallinedf2 = verticallinedf

    verticallinedf2$y = max(data.df2$y) + (max(data.df2$y) / 10) + nudge_y


    vert.line.df = bind_rows(verticallinedf1, verticallinedf2)

    linelabeldf = data.df2
    linelabeldf1 = linelabeldf
    linelabeldf2 = linelabeldf
    linelabeldf2$x = x2
    linelabeldf2$y = y2

    linelabeldf1$y = max(data.df2$y) + (max(data.df2$y) / 10) + nudge_y

    data.df3 = bind_rows(linelabeldf1, linelabeldf2)
    data.df3$label = data.df2$label
    data.df3

    data.df4 = bind_rows(vert.line.df, data.df3)
    #data.df4[data.df4$group == 6, ]
    data.df4
}


make.line.df.star = function(data.df, nudge_y = 0){
    data.df2 = data.df
    x2 = data.df2$x

    spread.x = function(x){
        for(i in 2:(length(x) - 1)){
            x[i] = (x[i + 1] + x[i - 1]) / 2
        }
        x
    }

    x2 = spread.x(x2)
    #x2 = seq(
        #min(data.df2$x),
        #max(data.df2$x),
        #max(data.df2$x) / nrow(data.df2)
    #)

    y2 = max(data.df2$y) + (max(data.df2$y) / 5) + nudge_y

    data.df2$group = 1:nrow(data.df2)
    order(data.df2$x)

    linelabeldf = data.df2
    linelabeldf1 = linelabeldf
    linelabeldf2 = linelabeldf

    linelabeldf2$x = x2
    linelabeldf2$y = y2

    data.df3 = bind_rows(linelabeldf1, linelabeldf2)


    #correct.intersects = function(data.df3){
        #x2.coord = (nrow(data.df3) / 2)
        #x2.coord = x2.coord + 1
        #x2 = data.df3$x[x2.coord:nrow(data.df3)]

        #data.df3.split = split(data.df3, data.df3$group)
        #data.df3.split[[1]]

        #buf1 = max(data.df3$x) / 10
        #x.buf.vals = as.numeric()
        #data.df3.split
        #for(i in 2:length(data.df3.split)){
            #intersect.bool = check_intersect(
                #c(data.df3.split[[i]]$x[1], data.df3.split[[i]]$y[1]),
                #c(data.df3.split[[i]]$x[2], data.df3.split[[i]]$y[2]),
                #c(data.df3.split[[i - 1]]$x[1], data.df3.split[[i - 1]]$y[1]),
                #c(data.df3.split[[i - 1]]$x[2], data.df3.split[[i - 1]]$y[2])
            #)

            #if(intersect.bool){
                #df1 = data.df3.split[[i - 1]]
                #df2 = data.df3.split[[i]]

                ##is the max of the first point before the min of the second or after?

                #x.diff = df1$x[which.max(df1$y)] - df2$x[which.min(df2$y)]
                #x.buf.vals = c(x.buf.vals, x.diff) 
                ##test.df = bind_rows(data.df3.split[[i - 1]], data.df3.split[[i]])
                ##pl(
                    ##ggplot(test.df, aes(x = x, y = y, group = group)) + geom_line() +
                        ##coord_cartesian(ylim = c(0, 200), xlim = c(min(data.df3$x), max(data.df3$x)))
                ##)
            #} else {
                #x.buf.vals = c(x.buf.vals, 0)
            #}
        #}

        #x.buf.vals = c(x.buf.vals, 0)

        #for(i in 1:length(x.buf.vals)){
            #if(x.buf.vals[[i]] > 0){
                ##x2[1:i] - x.buf.vals[[i]] - buf1
                #x2[1:i] = x2[1:i] * (x2[[i]] - x.buf.vals[[i]]) / x2[[i]] 
            #} else if(x.buf.vals[[i]] < 0){
                #x2.max = max(x2)
                #x2[i:length(x2)] = x2[i:length(x2)] - x.buf.vals[[i]] + buf1
                #x2[i:length(x2)] = x2[i:length(x2)] * (x2.max / max(x2[i:length(x2)]))
            #}
        #}


        #if(any(x2 < 0)) x2 = x2 + abs(min(x2))

        #x2 = x2 * (max(data.df3$x) / max(x2))

        #x2.coord = (nrow(data.df3) / 2)
        #x2.coord = x2.coord + 1
        #data.df3[x2.coord:nrow(data.df3), ]$x = x2
        #data.df3
    #}

    #data.df3 = correct.intersects(data.df3)
    #for(i in 1:100){
        #data.df3 = correct.intersects(data.df3)
        #print(i)
    #}

    data.df3
}


make.line.df.straight = function(data.df){
    data.df2 = data.df
    x2 = data.df2$x

    nudge_y = unique(data.df$nudge_y)

    y2 = max(data.df2$y) + (max(data.df2$y) / 5) + nudge_y

    data.df2$group = 1:nrow(data.df2)
    order(data.df2$x)

    linelabeldf = data.df2
    linelabeldf1 = linelabeldf
    linelabeldf2 = linelabeldf

    linelabeldf2$x = x2
    linelabeldf2$y = y2

    data.df3 = bind_rows(linelabeldf1, linelabeldf2)
    data.df3
}


make.line.df.cloud = function(data.df, nudge_y){
    #makes horizontal line for cloud layout
    data.df2 = data.df[c('x', 'y')]
    data.df2 = data.df[1:2, ]
    data.df2$x = c(min(data.df$x), max(data.df$x))
    data.df2$y = c(max(data.df$y) + max(data.df$y) / 10 + nudge_y, max(data.df$y) + max(data.df$y) / 10 + nudge_y)
    data.df2
}


make.line.df.cloud2 = function(data.df, nudge_y = 0){
    #makes fan lines for cloud layout
    data.df
    data.df.naomit = data.df[!is.na(data.df$label), ]


    x2 = seq(
        min(data.df$x),
        max(data.df$x),
        length.out = nrow(data.df.naomit)
    )

    data.df = data.df.naomit

    y2 = max(data.df$y) + (max(data.df$y) / 5) + nudge_y

    data.df$group = 1:nrow(data.df)
    order(data.df$x)

    linelabeldf1 = data.df
    linelabeldf1$y = max(data.df$y) + (max(data.df$y) / 10) + nudge_y

    linelabeldf2 = data.df
    linelabeldf2$x = x2
    linelabeldf2$y = y2

    data.df3 = bind_rows(linelabeldf1, linelabeldf2)
    data.df3
}

make.line.df.cloud3 = function(data.df, nudge_y){
    #makes points for cloud layout
    data.df = data.df[!is.na(data.df$label), ]
    data.df2 = data.df[c('x', 'y')]

    data.df2$y = max(data.df$y) + max(data.df$y) / 10 + nudge_y
    data.df2
}

make.label.df = function(data.df, nudge_y = 0){
    data.df$y = max(data.df$y) + (max(data.df$y) / 5) + nudge_y
    data.df.naomit = data.df[!is.na(data.df$label), ]
    x2 = seq(
        min(data.df$x),
        max(data.df$x),
        length.out = nrow(data.df.naomit)
    )

    data.df.naomit$x = x2
    data.df.naomit
}

StatCloud1 = ggproto('StatCloud1', Stat,
    compute_group = function(data, scales){
        make.line.df.cloud(data)
    },
    required_aes = c('x', 'y', 'group')
)

StatCloud2 = ggproto('StatCloud2', Stat,
    compute_group = function(data, scales){
        make.line.df.cloud2(data)
    },
    required_aes = c('x', 'y', 'group')
)

StatCloud3 = ggproto('StatCloud3', Stat,
    compute_group = function(data, scales){
        make.line.df.cloud3(data)
    },
    required_aes = c('x', 'y', 'group')
)


stat_cloud1 <- function(mapping = NULL, data = NULL, geom = "path",
                       position = "identity", na.rm = FALSE, show.legend = NA, 
                       inherit.aes = TRUE, ...) {
  layer(
    stat = StatCloud1, data = data, mapping = mapping, geom = geom, 
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}


stat_cloud2 <- function(mapping = NULL, data = NULL, geom = "path",
                       position = "identity", na.rm = FALSE, show.legend = NA, 
                       inherit.aes = TRUE, ...) {
  layer(
    stat = StatCloud2, data = data, mapping = mapping, geom = geom, 
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}

stat_cloud3 <- function(mapping = NULL, data = NULL, geom = "point",
                       position = "identity", na.rm = FALSE, show.legend = NA, 
                       inherit.aes = TRUE, ...) {
  layer(
    stat = StatCloud3, data = data, mapping = mapping, geom = geom, 
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}

StatFan = ggproto('StatFan', Stat,
    compute_group = function(data, scales){
        make.line.df.fan(data)
    },
    required_aes = c('x', 'y', 'group')
)

stat_fan <- function(mapping = NULL, data = NULL, geom = "path",
                       position = "identity", na.rm = FALSE, show.legend = NA, 
                       inherit.aes = TRUE, ...) {
  layer(
    stat = StatFan, data = data, mapping = mapping, geom = geom, 
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}


StatStar = ggproto('StatStar', Stat,
    compute_group = function(data, scales){
        make.line.df.star(data)
    },
    required_aes = c('x', 'y', 'group')
)

stat_star <- function(mapping = NULL, data = NULL, geom = "path",
                       position = "identity", na.rm = FALSE, show.legend = NA, 
                       inherit.aes = TRUE, ...) {
  layer(
    stat = StatStar, data = data, mapping = mapping, geom = geom, 
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}


StatLineLabel = ggproto('StatLineLabel', Stat,
    compute_group = function(data, scales){
        labeldf = make.label.df(data)
    },
    required_aes = c('x', 'y')
)

stat_linelabel <- function(mapping = NULL, data = NULL, geom = "text",
                       position = "identity", na.rm = FALSE, show.legend = NA, 
                       inherit.aes = TRUE, ...) {
  layer(
    stat = StatLineLabel, data = data, mapping = mapping, geom = geom, 
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}

############################
#GROB 
############################

GeomLineLabel <- ggproto("GeomLineLabel", Geom,
    required_aes = c("x", "y"),
    default_aes = aes(shape = 19, colour = "black", linetype = 'dotted',
        text.size = 4, angle = 90, hjust = 0,
        vjust = 0.5, alpha = NA, family = "", fontface = 1, lineheight = 1.2,
        layout = 'cloud',
        nudge_y = 0,
        nudge_x = 0
        ),
    draw_key = draw_key_point,

    draw_panel = function(data, panel_params, coord , arrow = NULL,
        lineend = "butt", linejoin = "round", linemitre = 10,
        na.rm = FALSE, linetype = 'solid'){ 

        nudge_y = unique(data$nudge_y)
       ############################
       #GEOM_PATH DRAW FUNCTION 
       ############################ 
        if (!anyDuplicated(data$group)) {
          message_wrap("geom_path: Each group consists of only one observation. ",
            "Do you need to adjust the group aesthetic?")
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
            data.cloud2 = make.line.df.cloud2(data, nudge_y)
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

            data.cloud = make.line.df.cloud(data, nudge_y)
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

            data.cloud3 = make.line.df.cloud3(data, nudge_y)
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

            data.label = make.label.df(data, nudge_y)

            lab <- data.label$label
            lab <- parse_safe(as.character(lab))

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
                col = alpha(data.label$colour, data.label$alpha),
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
            data.fan = make.line.df.fan(data, nudge_y)
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

            data.label = make.label.df(data, nudge_y)

            lab <- data.label$label
            lab <- parse_safe(as.character(lab))

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
                col = alpha(data.label$colour, data.label$alpha),
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
            data.star = data[!is.na(data$label), ]
            data.star = make.line.df.star(data.star, nudge_y)
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

            data.label = make.label.df(data, nudge_y)
            data.label$x = data.star.x2

            lab <- data.label$label
            lab <- parse_safe(as.character(lab))

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
                col = alpha(data.label$colour, data.label$alpha),
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
            data.straight = make.line.df.straight(data.straight)
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

            data.label = make.label.df(data)
            data.label$y = data.label$y + unique(data.label$nudge_y)
            data.label$x = data.straight.x2

            lab <- data.label$label
            lab <- parse_safe(as.character(lab))

            data.label <- coord$transform(data.label, panel_params)

            if (is.character(data.label$vjust)) {
              data.label$vjust <- compute_just(data.label$vjust, data.label$y, data.label$x, data.label$angle)
            }
            if (is.character(data.label$hjust)) {
              data.label$hjust <- compute_just(data.label$hjust, data.label$x, data.label$y, data.label$angle)
            }

            make.label.df(data)
            data.label

            label_elements = textGrob(
              lab,
              data.label$x, data.label$y, default.units = "native",
              hjust = data.label$hjust, vjust = data.label$vjust,
              rot = data.label$angle,
              gp = gpar(
                col = alpha(data.label$colour, data.label$alpha),
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

geom_linelabel <- function(mapping = NULL, data = NULL, stat = "identity",
                              position = "identity", na.rm = FALSE, show.legend = NA, 
                              inherit.aes = TRUE, ...) {
  layer(
    geom = GeomLineLabel, mapping = mapping,  data = data, stat = stat, 
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}

#geom_simple_point <- function(mapping = NULL, data = NULL, stat = "identity",
                              #position = "identity", na.rm = FALSE, show.legend = NA, 
                              #inherit.aes = TRUE, ...) {
  #layer(
    #geom = GeomSimplePoint, mapping = mapping,  data = data, stat = stat, 
    #position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    #params = list(na.rm = na.rm, ...)
  #)
#}

topnum = 100

library(ggrepel)

chr1.new = chr1
#chr1.new[sample(1:100, 70), ]$gene = NA

expression.gen = rnorm(100)
expression.gen[expression.gen < 0] = 0
expression.gen = expression.gen * 100
chr1.new$expression = expression.gen


plot.ggrepel = ggplot(chr1.new[1:topnum, ], aes(x = start, y = expression, label = gene)) + 
    geom_point() +
    geom_line() +
    geom_text_repel(
        nudge_y = 200,
        direction = 'x',
        hjust = 1,
        angle = 90,
        segment.color = "grey50",
        segment.linetype = 'dotted'
        ) +
    scale_y_continuous(limits = c(NA, 180))
    #geom_simple_point(angle = 90, hjust = 0, text.size = 2, layout = 'star', linetype = 'dotted') +
    #geom_simple_point(colour = '#d8d654', linetype = 'dashed') +

pl(plot.ggrepel)

chr1.new$sig.genes = NA
chr1.new$sig.genes[chr1.new$expression > 100] = chr1.new$gene[chr1.new$expression > 100]

plot.star = ggplot(chr1.new[1:topnum, ], aes(x = start, y = expression, label = sig.genes)) + 
    geom_point() +
    geom_line() +
    geom_linelabel(layout = 'star', nudge_y = -50) +
    scale_y_continuous(limits = c(NA, 300))
    #coord_cartesian(ylim = c(0, 400))

pl(plot.star)

plot.straight = ggplot(chr1.new[1:topnum, ], aes(x = start, y = expression, label = sig.genes)) + 
    geom_point() +
    geom_line() +
    geom_linelabel(layout = 'straight', nudge_y = -20) +
    scale_y_continuous(limits = c(NA, 300))
    #coord_cartesian(ylim = c(0, 400))

pl(plot.straight)

plot.cloud = ggplot(chr1.new[1:topnum, ], aes(x = start, y = expression, label = gene)) + 
    geom_point() +
    geom_line() +
    geom_linelabel(layout = 'cloud', nudge_y = -20, text.size = 2) +
    coord_cartesian(ylim = c(0, 300)) 

pl(plot.cloud)


plot.fan = ggplot(chr1.new[1:topnum, ], aes(x = start, y = expression, label = gene)) + 
    geom_point() +
    geom_line() +
    geom_linelabel(layout = 'fan', nudge_y = -200) +
    coord_cartesian(ylim = c(0, 210))

pl(plot.fan)


#geom_text(aes(label = gene))
#stat_star(linetype = 'dotted', color = '#888888') +
stat_fan(linetype = 'dotted', color = '#888888')
#stat_cloud1() +
#stat_cloud2(linetype = 'dotted', color = '#888888') +
#stat_cloud3() +
stat_linelabel(angle = 90, hjust = 0, size = 2) 
#coord_cartesian(ylim = c(0, 250))
#stat_alex(linetype = 'dotted', color = '#888888')
#stat_chull(fill = NA, color = 'black')


pl(plot1)



plot1 = ggplot(chr1, aes(x = start, y = expression, label = gene)) + 
    geom_point() +
    geom_line() +
    #geom_text(aes(label = gene))
    #stat_star(linetype = 'dotted', color = '#888888') +
    stat_fan(linetype = 'dotted', color = '#888888') +
    #stat_cloud1() +
    #stat_cloud2(linetype = 'dotted', color = '#888888') +
    #stat_cloud3() +
    stat_linelabel(angle = 90, hjust = 0, size = 2) 
    #coord_cartesian(ylim = c(0, 250))
    #stat_alex(linetype = 'dotted', color = '#888888')
    #stat_chull(fill = NA, color = 'black')


pl(plot1)



GeomLongFunction <- ggproto("GeomLongFunction", Geom,
    required_aes = c("x", "y"),
    default_aes = aes(
	colour = NA, fill = "grey20", size = 0.5,
	linetype = 1, alpha = 1
    ),
    draw_key = draw_key_abline,

    draw_group = function(data, panel_params, coord,
			  component = c("m1", "m2", "m3", "m4", "m5")) {
	component <- match.arg(component, several.ok = TRUE)
	result_grob <- lapply(component,function(comp){
	    data_comp <- data
	    data_comp$y <- data[,comp]
	    coords <- coord$transform(data_comp, panel_params)
	    grid::linesGrob(
		coords$x, coords$y, 
		default.units = "native",
		gp = grid::gpar(
		    col = coords$colour,
		    lwd = coords$size * .pt,
		    lty = coords$linetype
		)
	    )
	})
	do.call(grid::gList, result_grob)
    }
)

geom_longfunction <- function(mapping = NULL, data = NULL, stat = "long_function",
                              position = "identity", show.legend = NA, 
                              inherit.aes = TRUE,
                              component = c("m1", "m2", "m3", "m4", "m5"), ...) {
    layer(
        geom = GeomLongFunction, mapping = mapping, data = data, stat = stat, 
        position = position, show.legend = show.legend, inherit.aes = inherit.aes,
        params = list(component = component, ...)
    )
}
















plot1 = ggplot(chr1, aes(x = start, y = expression, label = gene)) + 
    geom_point() +
    geom_line() +
    #geom_text(aes(label = gene))
    stat_star(linetype = 'dotted', color = '#888888') +
    #stat_fan(linetype = 'dotted', color = '#888888') +
    #stat_cloud1() +
    #stat_cloud2(linetype = 'dotted', color = '#888888') +
    #stat_cloud3() +
    stat_linelabel(angle = 90, hjust = 0, size = 2) 
    #coord_cartesian(ylim = c(0, 250))
    #stat_alex(linetype = 'dotted', color = '#888888')
    #stat_chull(fill = NA, color = 'black')


pl(plot1)


plot1 = ggplot(chr1, aes(x = start, y = expression, label = gene)) + 
    geom_point() +
    geom_line() +
    stat_star(linetype = 'dotted', color = '#888888')
    #stat_chull(fill = NA, color = 'black')

plot1 = ggplot(chr1, aes(x = start, y = expression, label = gene)) + 
    geom_point() +
    geom_line() +
    stat_fan(linetype = 'dotted', color = '#888888')

pl(plot1)

layer_data(plot1)

layer_data
pl(plot1)




geom_linelabel <- function(label.df){
    head(chr1)
}

geom_linelabel(chr1)



############################
#HW REF CODE 
############################

#https://cran.r-project.org/web/packages/ggplot2/vignettes/extending-ggplot2.html

#StatChull <- ggproto("StatChull", Stat,
  #compute_group = function(data, scales) {
    #data[chull(data$x, data$y), , drop = FALSE]
  #},
  
  #required_aes = c("x", "y")
#)

#stat_chull <- function(mapping = NULL, data = NULL, geom = "line",
                       #position = "identity", na.rm = FALSE, show.legend = NA, 
                       #inherit.aes = TRUE, ...) {
  #layer(
    #stat = StatChull, data = data, mapping = mapping, geom = geom, 
    #position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    #params = list(na.rm = na.rm, ...)
  #)
#}

