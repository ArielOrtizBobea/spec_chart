schart <- function(data, labels=NA, highlight=NA, n=1, index.est=1, index.se=2, index.ci=NA,
                   order="asis", ci=.95, ylim=NA, axes=T, heights=c(1,1), leftmargin=11, offset=c(0,0), ylab="Coefficient", lwd.border=1, horizontal=T,
                   lwd.est=4, pch.est=21, lwd.symbol=2, ref=0, lwd.ref=1, lty.ref=2, col.ref="black", band.ref=NA, col.band.ref=NA,length=0,
                   col.est=c("grey60", "red3"), col.est2=c("grey80","lightcoral"), bg.est=c("white", "white"),
                   col.dot=c("grey60","grey95","grey95","red3"),
                   bg.dot=c("grey60","grey95","grey95","white"),
                   pch.dot=c(22,22,22,22), fonts=c(2,1), adj=c(1,1),cex=c(1,1)) {

  # Authors: Ariel Ortiz-Bobea (ao332@cornell.edu).
  # Version: March 22, 2021
  # If you like this function and use it, please send me a note. It might motivate
  # me improve it or write new ones and share them.

  # Description of arguments

  # Data:
  # data: data.frame with data, ideally with columns 1-2 with coef and SE, then logical variables.
  # labels: list of labels by group. Can also be a character vector if no groups. Default is rownames of data.
  # index.est: numeric indicating position of the coefficient column.
  # index.se: numeric indicating position of the SE column.
  # index.ci: numeric vector indicating position of low-high bars for SE. Can take up to 2 CI, so vector can be up to length 4

  # Arrangement and basic setup:
  # highlight: numeric indicating position(s) of models (row) to highlight in original dataframe.
  # n: size of model grouping. n=1 removes groupings. A vector yields arbitrary groupings.
  # order: whether models should be sorted or not. Options: "asis", "increasing", "decreasing"
  # ci: numeric indicating level(s) of confidence. 2 values can be indicated.
  # ylim: if one wants to set an arbitrary range for Y-axis

  # Figure layout:
  # heights: Ratio of top/bottom panel. Default is c(1,1) for 1 50/50 split
  # (Note: ratio for left/right panel if horizontal=F)
  # leftmargin: amount of space on the left margin
  # offset: vector of numeric with offset for the group and specific labels
  # ylab: Label on the y-axis of top panel. Default is "Coefficient"
  # lwd.border: width of border and other lines
  # horizontal:  should the plot be horizontal? (default is TRUE)

  # Line and symbol styles and colors:
  # lwd.est: numeric indicating the width of lines in the top panel
  # ref: numeric vector indicating horizontal reference lines(s) Default is 0.
  # lty.ref: Style of reference lines. Default is dash line (lty=2).
  # lwd.ref. Width of reference lines. Default is 1.
  # col.ref: vector of colors of reference lines. Default is black.
  # band.ref: vector of 2 numerics indicating upper abdn lower height for a band
  # col.band.ref: color of this band
  # col.est: vector of 2 colors indicating for "other" and "highlighted" models
  # col.est2: same for outer confidence interval if more than 1 confidence interval
  # col.dot: vector of 4 colors indicating colors for borders of symbol in bottom panel for "yes", "no", "NA", and "yes for highlighted model"
  # bg.dot : vector of 4 colors indicating colors for background of symbol in bottom panel for "yes", "no", "NA", and "yes for highlighted model"
  # pch.dot: style of symbols in bottom panel for "yes", "no", "NA", and "yes for highlighted model"
  # length: length of the upper notch on th vertical lines. default is 0.

  # Letter styles
  # fonts: numeric vector indicating font type for group (first) and other labels (second) (e.g. 1:normal, 2:bold, 3:italic)
  # adj: numeric vector indicating alignment adjustment for text label: 0 is left, .5 is center, 1 is right.
  # cex: numeric vector for size of fonts for top panel (first) and bottom panel (Second)

  # 1. Set up
  if (T) {
    # Arrange data
    d <- data
    rownames(d) <- 1:nrow(d)

    # Create ordering vector
    if (order=="asis")       o <- 1:length(d[,index.est])
    if (order=="increasing") o <- order(d[,index.est])
    if (order=="decreasing") o <- order(-d[,index.est])
    if (!is.numeric(d[,index.est])) {warning("index.est does not point to a numeric vector.") ; break}
    d <- d[o,]
    est <- d[,index.est] # Estimate
    if (length(index.ci)>1) {
      l1 <- d[,index.ci[1]]
      h1 <- d[,index.ci[2]]
      if (length(index.ci)>2) {
        l2 <- d[,index.ci[3]]
        h2 <- d[,index.ci[4]]
      }
    } else {
      if (!is.numeric(d[,index.se]))  {warning("index.se does not point to a numeric vector.") ; break}
      se  <- d[,index.se] # Std error
      ci <- sort(ci)
      a <- qnorm(1-(1-ci)/2)
      l1 <- est - a[1]*se
      h1 <- est + a[1]*se
      if (length(ci)>1) {
        l2 <- est - a[2]*se
        h2 <- est + a[2]*se
      }
    }

    # Table
    if (length(index.ci)>1) remove.index <- c(index.est,index.ci) else remove.index <- c(index.est,index.se)
    remove.index <- remove.index[!is.na(remove.index)]
    tab <- t(d[,-remove.index]) # get only the relevant info for bottom panel
    if (!is.list(labels) & !is.character(labels)) labels <- rownames(tab)

    # Double check we have enough labels
    if ( nrow(tab) != length(unlist(labels))) {
      print("Warning: number of labels don't match number of models.")
      labels <- rownames(tab)
    }

    # Plotting objects
    xs <- 1:nrow(d) # the Xs for bars and dots
    if (n[1]>1 & length(n)==1) xs <- xs + ceiling(seq_along(xs)/n) - 1 # group models by n
    if (length(n)>1) {
      if (sum(n) != nrow(d) ) {
        warning("Group sizes don't add up.")
      } else {
        idx <- unlist(lapply(1:length(n), function(i) rep(i,n[i])))
        xs <- xs + idx - 1
      }
    }
    h <- nrow(tab) + ifelse(is.list(labels),length(labels),0) # number of rows in table
    # Location of data and labels
    if (is.list(labels)) {
      index <- unlist(lapply(1:length(labels), function(i) rep(i, length(labels[[i]])) ))
      locs <- split(1:length(index),index)
      locs <- lapply(unique(index), function(i) {
        x <- locs[[i]]+i-1
        x <- c(x,max(x)+1)
      })
      yloc  <- unlist(lapply(locs, function(i) i[-1])) # rows where data points are located
      yloc2 <- sapply(locs, function(i) i[1]) # rows where group lables are located
    } else {
      yloc <- 1:length(labels)
    }

    # Range
    if (is.na(ylim[1]) | length(ylim)!=2) {
      if (length(index.ci)>2 | length(ci)>1) {
        ylim <- range(c(l2,h2,ref)) # range that includes reference lines
      } else {
        ylim <- range(c(l1,h1,ref))
      }
      ylim <- ylim + diff(ylim)/10*c(-1,1) # and a bit more
    }
    xlim <- range(xs) #+ c(1,-1)
  }

  # 2. Plot
  if (T) {
    #par(mfrow=c(2,1), mar=c(0,leftmargin,0,0), oma=oma, xpd=F, family=family)
    if (horizontal) {
      layout(t(t(2:1)), height=heights, widths=1)
      par(mar=c(0,leftmargin,0,0), xpd=F)
    } else  {
      layout(t(1:2), height=1, widths=heights)
      par(mar=c(leftmargin,0,0,0), xpd=F)
    }

    # Bottom panel (plotted first)
    if (horizontal)  plot(1:nrow(tab), xlab="", ylab="", axes=F, type="n", ylim=c(h,1), xlim=xlim)
    if (!horizontal) plot(1:nrow(tab), xlab="", ylab="", axes=F, type="n", ylim=xlim, xlim=c(1,h))
    lapply(1:nrow(tab), function(i) {
      # Get colors and point type
      type <- ifelse(is.na(tab[i,]),3,ifelse(tab[i,]==TRUE,1, ifelse(tab[i,]==FALSE,2,NA)))
      type <- ifelse(names(type) %in% paste(highlight) & type==1,4,type) # replace colors for baseline model
      col <- col.dot[type]
      bg  <- bg.dot[type]
      pch <- as.numeric(pch.dot[type])
      sel <- is.na(pch)
      # Plot points
      if (horizontal) {
        points(xs, rep(yloc[i],length(xs)), col=col, bg=bg, pch=pch, lwd=lwd.symbol)
        points(xs[sel], rep(yloc[i],length(xs))[sel], col=col[sel], bg=bg[sel], pch=pch.dot[3]) # symbol for missing value
      } else {
        points(rep((yloc)[i],length(xs)),xs, col=col, bg=bg, pch=pch, lwd=lwd.symbol)
        points(rep((yloc)[i],length(xs))[sel],xs[sel], col=col[sel], bg=bg[sel], pch=pch.dot[3]) # symbol for missing value
      }

    })
    par(xpd=T)
    if (is.list(labels)) {
      if (horizontal) {
        text(-offset[1], yloc2, labels=names(labels), adj=adj[1], font=fonts[1], cex=cex[2])
      } else {
        text((yloc2), -offset[1], labels=names(labels), adj=adj[1], font=fonts[1], cex=cex[2], srt=90)
      }
    }
    # Does not accomodate subscripts
    if (horizontal) {
      text(-rev(offset)[1], yloc , labels=unlist(labels), adj=rev(adj)[1], font=fonts[2], cex=cex[2])
    } else {
      text(yloc,-rev(offset)[1], labels=unlist(labels), adj=rev(adj)[1], font=fonts[2], cex=cex[2], srt=90)
    }
    # Accomodates subscripts at the end of each string
    if (F) {
      labels1 <- unlist(labels)
      lapply(1:length(labels1), function(i) {
        a  <- labels1[i]
        a1 <- strsplit(a,"\\[|\\]")[[1]][1]
        a2 <- rev(strsplit(a,"\\[|\\]")[[1]])[1]
        if (identical(a1,a2))  a2 <- NULL
        text(-rev(offset)[1], yloc[i], labels=bquote(.(a1)[.(a2)]), adj=adj[2], font=fonts[2], cex=cex[2])
      })
    }
    par(xpd=F)

    # Top panel (plotted second)
    colvec  <- ifelse(colnames(tab) %in% paste(highlight), col.est[2], col.est[1])
    bg.colvec  <- ifelse(colnames(tab) %in% paste(highlight), bg.est[2], bg.est[1])
    colvec2 <- ifelse(colnames(tab) %in% paste(highlight),col.est2[2], col.est2[1])
    if (horizontal)   plot(est, xlab="", ylab="", axes=F, type="n", ylim=ylim, xlim=xlim)
    if (!horizontal)  plot(est, xlab="", ylab="", axes=F, type="n", ylim=xlim, xlim=ylim)
    # Band if present
    if (!is.na(band.ref[1])) {
      if (horizontal) {
        rect(xleft=min(xlim)-diff(xlim)/10, ybottom=band.ref[1], xright=max(xlim)+diff(xlim)/10, ytop=band.ref[2],
             col=col.band.ref, border=NA)
      } else {
        rect(ybottom=min(xlim)-diff(xlim)/10, xleft=band.ref[1], ytop=max(xlim)+diff(xlim)/10, xright=band.ref[2],
             col=col.band.ref, border=NA)
      }
    }
    # Reference lines
    if (horizontal) {
      abline(h=ref, lty=lty.ref, lwd=lwd.ref, col=col.ref)
    } else {
      abline(v=ref, lty=lty.ref, lwd=lwd.ref, col=col.ref)
    }
    # Vertical bars
    if (horizontal) {
      if (length(ci)>1 | length(index.ci)>2) arrows(x0=xs, y0=l2, x1=xs, y1=h2, length=length, code=3, lwd=rev(lwd.est)[1], col=colvec2, angle=90)
      arrows(x0=xs, y0=l1, x1=xs, y1=h1, length=length, code=3, lwd=lwd.est[1]     , col=colvec, angle=90)
      points(xs, est, pch=pch.est, lwd=lwd.symbol, col=colvec, bg=bg.colvec)
    } else {
      if (length(ci)>1 | length(index.ci)>2) arrows(y0=xs, x0=l2, y1=xs, x1=h2, length=length, code=3, lwd=rev(lwd.est)[1], col=colvec2, angle=90)
      arrows(y0=xs, x0=l1, y1=xs, x1=h1, length=length, code=3, lwd=lwd.est[1]     , col=colvec, angle=90)
      points(est,xs, pch=pch.est, lwd=lwd.symbol, col=colvec, bg=bg.colvec)
    }
    # Axes
    if (axes) {
      axis(ifelse(horizontal,2,1), las=2, cex.axis=cex[1], lwd=lwd.border)
      axis(ifelse(horizontal,4,3), labels=NA, lwd=lwd.border)
    }
    mtext(ylab, side=ifelse(horizontal,2,1), line=3.5, cex=cex[1])
    box(lwd=lwd.border)

  }

}