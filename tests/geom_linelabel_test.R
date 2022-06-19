source('~/work/ucl/scripts/misc/functions.R')
library(devtools)
library(ggplot2)
library(dplyr)
library(ggplot2testing)
library(gglinelabel)
library(ggrepel)
library(rmarkdown)

#render('./vignettes/using-gglinelabel.Rmd')
load_all('~/work/external.repos/gglinelabel/')
data(chr1)

#chr1 = get.genes(1, 0, 1000000000000)
#chr1$expression = rnorm(nrow(chr1))
#chr1$expression[chr1$expression < 0] = 0
#chr1 = chr1[c('chr', 'start', 'end', 'gene', 'expression')]
#chr1$sig.genes = NA
#chr1$sig.genes[chr1$expression > 0.7] = chr1$gene[chr1$expression > 0.7]
#chr1$cosmic = NA
#chr1$cosmic[is.cosmic(chr1$gene) & chr1$expression > 1.5] = chr1$gene[is.cosmic(chr1$gene) & chr1$expression > 1.5]
#chr1$cosmic[which(chr1$cosmic == 'AKT3') + 1] = 'TEST'

#saveRDS(chr1, '~/work/external.repos/gglinelabel/tests/chr1.ggrepel.fail.rds')
#chr1 = readRDS('~/work/external.repos/gglinelabel/tests/chr1.ggrepel.fail.rds')

#save(chr1, file = '~/work/external.repos/gglinelabel/data/chr1orderggrepel.rda')

#data(chr1orderggrepel)

plot1 = ggplot(chr1, aes(x = start, y = expression, label = cosmic)) +
    geom_bar(stat = 'identity') +
    #geom_line() +
    #geom_linelabel(
        #layout = 'fan',
        #text.size = 4
        #) + 
    geom_text_repel(
        nudge_y = 10,
        direction = 'x',
        angle = 90,
        force = 10,
        nudge_x = c(0, 0, 10, 1000000000)
    ) +
    coord_cartesian(ylim = c(0, 4))

#pl(plot1, 15, 5)


head(chr1)
plot1 = ggplot(chr1, aes(x = start, y = expression, label = gene)) +
    geom_line(stat = 'identity') +
    #geom_line() +
    geom_linelabel(
        layout = 'cloud',
        text.size = 3,
        fan.height = 10
        #spread.factor = 0
        ) + 
    coord_cartesian(ylim = c(0, 300))

pl(plot1, 15, 5)

devtools::test()

load_all('~/work/external.repos/gglinelabel/')
plot1 = ggplot(chr1, aes(x = start, y = expression, label = gene)) +
    geom_line(stat = 'identity') +
    #geom_line() +
    geom_linelabel(
        layout = 'fan',
        text.size = 3,
        min.y = 170,
        max.y = 200,
        fan.height = 0.4
        #spread.factor = 1,
        #spread.threshold = 100000000000
        ) +
    coord_cartesian(ylim = c(0, 300))

pl(plot1, 15, 5)

plot1 = ggplot(chr1, aes(x = start, y = expression, label = cosmic)) +
    geom_bar(stat = 'identity') +
    #geom_line() +
    geom_linelabel(
        layout = 'straight',
        spread.factor = 0.04,
        spread.threshold = 10000000,
        max.y = 3
        ) 

#pl(plot1, 15, 5)

plot1 = ggplot(chr1, aes(x = start, y = expression, label = cosmic)) +
    geom_bar(stat = 'identity') +
    #geom_line() +
    geom_linelabel(
        fan.height = 0.7,
        spread.factor = 0.2,
        spread.threshold = 1000000
        ) 

#pl(plot1, 15, 5)


plot.ggrepel = ggplot(chr1, aes(x = start, y = expression, label = sig.genes)) + 
    geom_point() +
    geom_line() +
    geom_text_repel(
        nudge_y = 250,
        direction = 'x',
        hjust = 0,
        angle = 90,
        segment.color = "grey50",
        segment.linetype = 'dotted'
        ) +
    scale_y_continuous(limits = c(NA, 5))

#pl(plot.ggrepel, 5, 5)

#pl(plot1)

#save(chr1, file = '~/work/external.repos/gglinelabel/data/chr1.rda')

data(chr1)
chr1$gene[[2]] = chr1$gene[[1]]
chr1$gene[30:40] = chr1$gene[[1]]

#pl(plot1)

chr1 = chr1[1:30, ]

#devtools::install('~/work/external.repos/ggplot2_testing/')
#source('~/work/external.repos/ggplot2_testing/R/geom-point.r')
#source('~/work/external.repos/ggplot2_testing/R/layer.r')

label.df = chr1[c('gene', 'expression')]

test_function = function(){
    layer_data(data)
}


linelabel_chr = chr1[c('start', 'expression', 'gene')]
colnames(linelabel_chr) = c('x', 'y', 'label')

topnum = 30
chr1.new = chr1
#chr1.new[sample(1:100, 70), ]$gene = NA

expression.gen = rnorm(100)
expression.gen[expression.gen < 0] = 0
expression.gen = expression.gen * 100
chr1.new$expression = expression.gen[1:nrow(chr1.new)]
chr1.new$sig.genes = NA
chr1.new$sig.genes[chr1.new$expression > 100] = chr1.new$gene[chr1.new$expression > 100]

plot.ggrepel = ggplot(chr1.new[1:topnum, ], aes(x = start, y = expression, label = sig.genes)) + 
    geom_point() +
    geom_line() +
    geom_text_repel(
        nudge_y = 250,
        direction = 'x',
        hjust = 1,
        angle = 90,
        segment.color = "grey50",
        segment.linetype = 'dotted'
        ) +
    scale_y_continuous(limits = c(NA, 330))
    #geom_simple_point(angle = 90, hjust = 0, text.size = 2, layout = 'star', linetype = 'dotted') +
    #geom_simple_point(colour = '#d8d654', linetype = 'dashed') +

#pl(
    ggplot(chr1.new, aes(x = start, y = expression, label = sig.genes)) +
        geom_point() +
        geom_line() +
        geom_text_repel(
            nudge_y = 250,
            direction = 'x',
            hjust = 1,
            angle = 90,
            segment.color = "grey50",
            segment.linetype = 'dotted'
            ) +
        scale_y_continuous(limits = c(NA, 330))
)

chr1.new
#pl(
    #ggplot(chr1.new, aes(x = start, y = expression, label = sig.genes)) +
        #geom_point() +
        #geom_line() +
        ##geom_linelabel() +
        #scale_y_continuous(limits = c(NA, 330))
#)


#pl(plot.ggrepel, 5, 5)


topnum = 30

plot.star = ggplot(chr1.new[1:topnum, ], aes(x = start, y = expression, label = sig.genes)) + 
    geom_point() +
    geom_line() +
    geom_linelabel(layout = 'fan', nudge_y = 0, spread.iter = 1000) +
    scale_y_continuous(limits = c(NA, 5)) +
    coord_cartesian(ylim = c(0, 400))

pl(plot.star, 5, 5)

plot.straight = ggplot(chr1.new[1:topnum, ], aes(x = start, y = expression, label = sig.genes)) + 
    geom_point() +
    geom_line() +
    geom_linelabel(layout = 'straight', nudge_y = -20) +
    scale_y_continuous(limits = c(NA, 300)) +
    coord_cartesian(ylim = c(0, 800))

#pl(plot.straight)

plot.cloud = ggplot(chr1.new[1:topnum, ], aes(x = start, y = expression, label = sig.genes)) + 
    geom_point() +
    geom_line() +
    geom_linelabel(layout = 'cloud', nudge_y = -20, text.size = 4) +
    coord_cartesian(ylim = c(0, 400)) 

#pl(plot.cloud)

chr1$gene[chr1$expression < 1] = NA
chr1$nudge = 0
chr1$nudge[chr1$gene == 'LINC01128'] = 20000

plot.fan = ggplot(chr1, aes(x = start, y = expression, label = gene)) + 
    geom_point() +
    geom_line() +
    geom_linelabel(layout = 'straight', nudge_y = -0, text.size = 2, aes(nudge_label = nudge, max.y = 2.3)) +
    coord_cartesian(ylim = c(0, 3))

#pl(plot.fan, 10, 10)

cn.dat = data.frame(
    sample = 'test',
    chr = 1,
    start = c(1, 40001, 100001, 106101),
    end = c(40000, 100000, 106100, 200000),
    cn = c(2, 1, 3, 2)
)

cn.dat + geom_linelabel(aes(label = sample))

make.copy.number.plot

make.copy.number.plot = function(
    cn.df,
    ylim1 = c(0, 10),
    ybreaks = seq(0, 10, 2),
    colnames1 = c('sample', 'chr', 'start', 'end', 'cn'),
    yintercept1 = 2
){
    library(gdata)
    #args:
        #cn.df: data.frame with columns sample, chr, start, end, cn and patient

    cn.df.p1 = cn.df[c('sample', 'chr', 'start', 'cn')]
    colnames(cn.df.p1)[3] = 'point'
    cn.df.p2 = cn.df[c('sample', 'chr', 'end', 'cn')]
    colnames(cn.df.p2)[3] = 'point'


    cn.df = interleave(cn.df.p1, cn.df.p2)

    ggplot(cn.df, aes(x = point, y = cn)) +
        coord_cartesian(ylim = ylim1) +
        geom_hline(yintercept = yintercept1, color = 'red', linetype = 'dotted') +
        scale_y_continuous(breaks = ybreaks) +
        facet_grid(cols = vars(chr), scales = 'free_x') +
        geom_line() +
        rot.lab() +
        xlab('Position')
}

chr.test = chr1.new[1:30, ]

chr.test = chr.test[1:6, ]

chr.test = chr.test[c('chr', 'start', 'end', 'gene', 'expression')]
cn.ranges = GRanges(cn.dat)
chr.ranges = GRanges(chr.test)

overlaps = as.data.frame(findOverlaps(cn.ranges, chr.ranges))
chr.test = chr.test[overlaps$subjectHits, ]
chr.test$cn = cn.dat[overlaps$queryHits, ]$cn

nudge_label = 100000
names(nudge_label) = 'OR4F5'

chr.test$nudge = 0
chr.test$nudge[chr.test$gene == 'OR4F5'] = 100000


nudgelayout = 'straight'
newplot = make.copy.number.plot(cn.dat) +
    geom_linelabel(
        data = chr.test,
        inherit.aes = F,
        aes(
            x = start,
            y = cn,
            label = gene,
            layout = nudgelayout,
            nudge_label = nudge
        ),
        spread.iter = 100,
        nudge_y = -5,
        #cloud.straight = T,
        #label.straight = T,
        #min.y = 4,
        fan.height = -2,
        #angle = 0,
        #angle = 270,
        min.x = 1,
        max.x = 200000,
        #min.y = -4,
        #max.y = -7,
        rotate = 0,
        color = 'red',
        text.colour = 'blue'
        #linetype = 'solid'
        ) +
    geom_point() +
    coord_cartesian(ylim = c(-10, 10))

#pl(newplot)

newplot = make.copy.number.plot(cn.dat) +
    geom_linelabel(
        data = chr.test,
        inherit.aes = F,
        aes(
            x = start,
            y = cn,
            label = gene,
            layout = nudgelayout
        ),
        spread.iter = 100,
        nudge_y = -5,
        #cloud.straight = T,
        #label.straight = T,
        #min.y = 4,
        fan.height = -2,
        #angle = 0,
        #angle = 270,
        min.x = 1,
        max.x = 200000,
        #min.y = -4,
        #max.y = -7,
        rotate = 0,
        color = 'red',
        text.colour = 'blue'
        #linetype = 'solid'
        ) +
    geom_point() +
    coord_cartesian(ylim = c(-10, 10))


#pl(newplot)

library(reshape2)
cn.dat$group = 1:nrow(cn.dat)
cn.dat

cn.dat2 = melt(cn.dat, id = c('cn', 'group'))

#pl(ggplot(cn.dat2, aes(x = value, y = cn, group = group)) + geom_line())


