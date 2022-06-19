render.cloud = function(
    render.params
    ){
    #unpack render.params to local env
    for(i in 1:length(render.params)){
        assign(names(render.params)[[i]], render.params[[i]])
    }

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
}

render.star = function(
    render.params
    ){
    #unpack render.params to local env
    for(i in 1:length(render.params)){
        assign(names(render.params)[[i]], render.params[[i]])
    }

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

    data.label = make.label.df(data,
        nudge_y, fan.height = fan.height, min.x = min.x, max.x = max.x, max.y = max.y,
        label.straight = label.straight)
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

}

render.fan = function(
    render.params
    ){
    for(i in 1:length(render.params)){
        assign(names(render.params)[[i]], render.params[[i]])
    }

    ############################
    #FAN ELEMENTS 
    ############################

    #set default fan height
    if(is.na(fan.height)){
        fan.height = abs(max(data$y, na.rm = T) - min(data$y, na.rm = T)) / 6
    }

    # must be sorted on group
    data.fan = make.line.df.fan(
        data,
        nudge_y,
        min.y = min.y,
        max.y = max.y,
        fan.height = fan.height,
        min.x = min.x,
        max.x = max.x,
        spread.factor = spread.factor,
        spread.threshold
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

    head(data.fan, n = 20)

    data.label = make.label.df(
        data,
        nudge_y,
        min.x = min.x,
        max.x = max.x,
        max.y = max.y,
        spread.factor = spread.factor,
        spread.threshold = spread.threshold
    )

    max(data.fan$y)
    max(data.label$y)

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

render.straight = function(
    render.params
    ){
    for(i in 1:length(render.params)){
        assign(names(render.params)[[i]], render.params[[i]])
    }

    ############################
    #STAR ELEMENTS 
    ############################
    # must be sorted on group
    data.straight = data[!is.na(data$label), ]
    data.straight = make.line.df.straight(
        data.straight,
        min.y = min.y,
        max.y = max.y,
        spread.factor = spread.factor,
        spread.threshold
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

    data.label = make.label.df(data,
        fan.height = fan.height, min.x = min.x, max.x = max.x, max.y = max.y,
        label.straight = label.straight)
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

