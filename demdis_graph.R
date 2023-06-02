library(stringi)
library(stringr)
library(dplyr)

draw.clusters <- function(info) {
  # Draws opinion clusters, input is polis.clust output
  pts <- info[, tail(1:dim(info)[2], 2)]
  cl <- info[, tail(1:dim(info)[2], 3)[1]]
  # Colors 
  clrs <- c('#000000', '#f8cc04', '#08a6a6', '#a60808')
  
  plot(pts, col = clrs[cl], pch = 1, cex = 1.4,
       ylim = c(min(pts[, 2]) * 1.1, max(pts[, 2]) * 1.4), xlab = '', 
       ylab = '', axes = F, bty = 'o', asp = 1)
  points(pts, col = alpha(clrs[cl], 0.8), pch = 19, cex = 1.4)
  legend('topright', pch = 19, col = clrs[1:length(unique(cl))], bty = 'n',
         legend = paste('Skupina', LETTERS[1:length(unique(cl))]), 
         cex  = 1.7)
  abline(h = 0, lty = 2, col = 'gray')
  abline(v = 0, lty = 2, col = 'gray')
  
}

draw.graph <- function(all, id, group, custom = '') {
  # Creates a graph for given comment; analysis has to be already done 
  # (top_comms results stored in the first variable)
  one <- all$clust %>% filter(comment.id == id, group.id == group)
  one <- one %>% mutate(total = n.votes + unseen)
  first <- with(one, n.agr / total)
  second <- with(one, (n.agr + n.skip) / total)
  third <- with(one, (n.agr + n.skip + n.dis) / total)
  plot(1, 1, xlim = c(0, 1), ylim = c(-2, 1), type = 'n', bty = 'n', 
       axes = F, xlab = '', ylab = '')
  par(mar = c(0, 0, 0, 0))
  ycoord <- c(0, 0, 0.5, 0.5)
  polygon(x = c(0, first, first, 0), border = NA,
          y = ycoord, col = '#08A608',)
  polygon(x = c(first, second, second, first), y = ycoord, col = '#AAAAAA')
  polygon(x = c(second, third, third, second), y = ycoord, col = '#A60808')
  polygon(x = c(third, 1, 1, third), y = ycoord, col = 'white')
  polygon(x = c(0, 1, 1, 0), y = c(0, 0, 0.5, 0.5), col = rgb(1, 1, 1, 0))
  # Number and cluster texts
  if (custom != '') text(0, 0.8, paste(str_to_title(custom), 'users'), adj = 0, 
                         cex = 2)
  else text(0, 0.8, paste('Skupina', LETTERS[group]), adj = 0, cex = 2)
  text(0, -0.5, paste(round(100 * with(one, n.agr / (n.agr + n.dis + n.skip)), 1), ' % (', 
                      one$n.agr, ') za', sep = ''), 
       col = '#08A608', adj = 0, cex = 2)
  text(0, -0.9, paste(round(100 * with(one, n.dis / (n.agr + n.dis + n.skip)), 1), ' % (', 
                      one$n.dis, ') proti', sep = ''), 
       col = '#A60808', adj = 0, cex = 2)
  text(0, -1.3, paste(round(100 * with(one, n.skip / (n.agr + n.dis + n.skip)), 1), ' % (', 
                      one$n.skip, ') preskočilo', sep = ''), 
       col = '#AAAAAA', adj = 0, cex = 2)
  text(0, -1.7, paste(round(100 * with(one, unseen / total), 1), ' % (', 
                      one$unseen, ') nevidelo', sep = ''), 
       col = 'black', adj = 0, cex = 2)
  return()
}

heder <- function(all, id, spl = 70) {
  # Graphs the header of a comment; 'all' is the top_comms result
  com <- all$final %>% filter(comment.id == id) %>% select(comment.body) %>% 
    unlist
  inds <- (strsplit(com, split = '') %>% unlist == ' ') %>% which
  spaces <- c()
  for (i in 1:3) {
    sp <- inds[inds > spl * i][1]
    spaces <- c(spaces, sp)
  }
  spaces <- spaces[!is.na(spaces)]
  if (length(spaces) != 0) {
    for (i in 1:length(spaces) - 1)  stri_sub(com, spaces[i + 1] + 1 + i, 
                                              spaces[i + 1] + i) <- '\n'
  }
  plot(0, 0, type = 'n', axes = F, bty = 'n', xlim = c(0, 1), ylim = c(-2, 2))
  text(-0.02, 1, adj = c(0, 1), paste(com, ' (', id, ')', sep = ''), cex = 2)
  return(com)
}

draw.comments <- function(all, id) {
  # Draws the whole graph a comment for 2, 3, 4 clusters
  par(mar = c(0, 0, 0, 0))
  len <- all$clust$group.id %>% unique %>% length
  if (len == 2) {
  par(mfrow = c(1, 2))
  draw.graph(all, id, 1)
  draw.graph(all, id, 2)
  }
  else if (len == 3) {
    par(mfrow = c(1, 3))
    draw.graph(all, id, 1)
    draw.graph(all, id, 2)
    draw.graph(all, id, 3)
  }
  else {
    par(mfrow = c(2, 2))
    draw.graph(all, id, 1)
    draw.graph(all, id, 2)
    draw.graph(all, id, 3)
    draw.graph(all, id, 4)
  }
  return()
}

draw.onecomm <- function(all, id, group) {
  par(mar = rep(1, 4), mfrow = c(1, 1))
  draw.graph(all, id, group)
  return()
}
