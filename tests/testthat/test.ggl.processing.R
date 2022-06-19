test_that("label height equals fan height", {
    load(paste0(test_path(), '/data/render.params.rda'))
    for(i in 1:length(render.params)){
        assign(names(render.params)[[i]], render.params[[i]])
    }

    #fan.height = 10
    max.y
    fan.height

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

    data.label = make.label.df(
        data,
        nudge_y,
        min.x = min.x,
        max.x = max.x,
        max.y = max.y,
        spread.factor = spread.factor,
        spread.threshold = spread.threshold
    )


    expect_equal(
        max(data.label$y, na.rm = T),
        max(data.fan$y, na.rm = T)
    )

    #here the fan.height argument is not 1, and so
    #the middle values (top of vertical segment)
    #should not equal the highest values
    expect_false(
        sort(unique(data.fan$y))[2] == sort(unique(data.fan$y))[3],
    )
})
