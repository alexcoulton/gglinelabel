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
    spread.factor = 1,
    spread.threshold = 0
    ){

    data.df2 = data.df
    data.df.naomit = data.df[!is.na(data.df$label), ]
    data.df2 = data.df.naomit

    if(is.na(min.x)) min.x = min(data.df2$x)
    if(is.na(max.x)) max.x = max(data.df2$x)

    orig.x = data.df2$x
    x2 = add.spread.factor(orig.x, spread.factor, spread.threshold)

    data.df2$group = 1:nrow(data.df2)

    verticallinedf1 = data.df2
    verticallinedf2 = data.df2

    verticallinedf2$y = max(data.df2$y) + (max(data.df2$y) / 10) + nudge_y

    if(!is.na(min.y)) verticallinedf1$y = min.y

    vert.line.df = bind_rows(verticallinedf1, verticallinedf2)

    if(!any(is.na(data.df2$nudge_label))) x2 = x2 + data.df2$nudge_label

    linelabeldf1 = data.df2
    linelabeldf1$y = max(data.df2$y) + (max(data.df2$y) / 10) + nudge_y

    linelabeldf2 = data.df2
    linelabeldf2$x = x2
    linelabeldf2$y = max(data.df2$y) + (max(data.df2$y) / 5) + nudge_y

    data.df3 = bind_rows(linelabeldf1, linelabeldf2)
    data.df3$label = data.df2$label
    data.df4 = bind_rows(vert.line.df, data.df3)

    if(!is.na(max.y)){
        data.df4$y[data.df4$y != min(data.df4$y, na.rm = T)] = 
            data.df4$y[data.df4$y != min(data.df4$y, na.rm = T)] *
            (max.y / unique(data.df4$y[data.df4$y == max(data.df4$y, na.rm = T)]))
    }

    if(!is.na(fan.height)){
        if(fan.height > 1) fan.height = 1
        if(fan.height < 0) fan.height = 0
        y.vals = unique(data.df4$y)
        y.min = min(y.vals, na.rm = T)
        y.max = max(y.vals, na.rm = T)
        y.dist = abs(max(y.vals, na.rm = T) -  min(y.vals, na.rm = T))
        y.midval = y.min + (y.dist * fan.height)

        data.df4$y[data.df4$y == y.vals[2]] = y.midval
    }

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
    if(nrow(data.df) <= 2) return(data.df)
    print(data.df)

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

add.spread.factor = function(x, spread.factor, spread.threshold){
    #x: numeric vector of x coordinates
    #spread.factor: number from 0 to 1; 0 = straight line layout, 1 = even spread between points
    #spread.threshold: adjacent points on the x-axis whose distance
        #is smaller than this threshold will be spread;
        #we don't want to spread points that are far apart anyway
    min.x = min(x)
    max.x = max(x)
    x.spread = seq(
        min.x,
        max.x,
        length.out = length(x)
    )

    spread2 = x.spread /  x 
    x.dist = x.spread - x
    spread.threshold.list = abs(x[1] - x[2]) < spread.threshold

    for(i in 2:(length(x) - 1)){
        spread.threshold.list = c(
            spread.threshold.list,
            any(
                abs(x[i] - x[i - 1]) < spread.threshold,
                abs(x[i] - x[i + 1]) < spread.threshold
            )
        )
    }

    spread.threshold.list = c(
        spread.threshold.list,
        abs(x[(length(x) - 1)] - x[length(x)]) < spread.threshold
    )

    x.dist2 = x.dist * spread.factor
    x[spread.threshold.list] = x[spread.threshold.list] + x.dist2[spread.threshold.list]
    x
}

make.line.df.straight = function(
    data.df,
    min.y = NA,
    max.y = NA,
    spread.factor = 0,
    spread.threshold = 0
    ){
    data.df2 = data.df

    x2 = data.df2$x

    if(!is.na(spread.factor)){
        x2 = add.spread.factor(x2, spread.factor, spread.threshold)
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
    min.x = NA,
    max.x = NA,
    max.y = NA,
    spread.factor = 1,
    spread.threshold = 0
    ){
    data.df = data.df[!is.na(data.df$label), ]
    data.df$y = max(data.df$y) + (max(data.df$y) / 5) + nudge_y
    if(!is.na(max.y)) data.df$y = max.y
    #if(!is.na(fan.height) & !is.na(max.y)) data.df$y = max.y + fan.height

    data.df.naomit = data.df[!is.na(data.df$label), ]
    if(is.na(min.x)) min.x = min(data.df$x)
    if(is.na(max.x)) max.x = max(data.df$x)
    if(label.straight == T){
        x2 = data.df.naomit$x
    } else {
        x2 = add.spread.factor(data.df.naomit$x, spread.factor, spread.threshold)
    }
    if(!any(is.na(data.df.naomit$nudge_label))) x2 = x2 + data.df.naomit$nudge_label
    data.df.naomit$x = x2[1:nrow(data.df.naomit)]
    #browser()
    data.df.naomit
}

