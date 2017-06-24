do.stand <- function(row, col, dim = 20, stand.rule = "five"){
  if(aud[x == row & y == col, stand] == TRUE){
    return(TRUE)
  }
  else{
    if(stand.rule == "five"){
    neighbor.ids <- c(NULL)
    if(row %in% 1:(dim-1) & col == 1){
      neighbor.ids[1] <- aud[x == row+1 & y == col, id]
      neighbor.ids[2] <- aud[x == row & y == col+1, id]
      neighbor.ids[3] <- aud[x == row+1 & y == col+1, id]
    }
    if(row %in% 1:(dim-1) & col == dim){
      neighbor.ids[1] <- aud[x == row+1 & y == col, id]
      neighbor.ids[2] <- aud[x == row & y == col-1, id]
      neighbor.ids[3] <- aud[x == row+1 & y == col-1, id]
    }
    if(row == dim & col == 1){
      neighbor.ids[1] <- aud[x == row & y == col+1, id]
    }
    if(row == dim & col == dim){
      neighbor.ids[1] <- aud[x == row & y == col-1, id]
    }
    if(row == dim & col %in% 2:(dim-1)){
      neighbor.ids[1] <- aud[x == row & y == col+1, id]
      neighbor.ids[2] <- aud[x == row & y == col-1, id]
    }
    if(row %in% 1:(dim-1) & col %in% 2:(dim-1)){
      neighbor.ids[1] <- aud[x == row & y == col-1, id]
      neighbor.ids[2] <- aud[x == row & y == col+1, id]
      neighbor.ids[3] <- aud[x == row+1 & y == col-1, id]
      neighbor.ids[4] <- aud[x == row+1 & y == col, id]
      neighbor.ids[5] <- aud[x == row+1 & y == col+1, id]
    }
    pct.stand <- sum(aud[id %in% neighbor.ids, stand])/length(neighbor.ids)
    return(pct.stand > aud[x == row & y == col, st])
    }
    if(stand.rule == "global"){
      return(mean(aud$stand) > aud[x == row & y == col, st])
    }
    }
}
