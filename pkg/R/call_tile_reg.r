#' call_tile_reg
#'
#' For an ATAC-STAR dataset on a given chromosome, this function calls tile regions (the regions covered by at least one read).
#' @param data A data.frame for an ATAC-STAR dataset for one chromosome. The data.frame must contain four columns: 'start', 'end', 'PLASMID', 'RNA', and is sorted by 'start'. 
#' @keywords sharpr2 ATAC-STAR
#' @return tile_reg: A list containing the row ids in the data for each tile region.
#' @return size the: number of reads in each tile region.
#' @return num_r: the total number of tile regions.
#' @export
#' @examples
#' # call_tile_reg(data)


call_tile_reg <- function(data)
{
	# data <- data[order(data$start),]
	reg_set <- vector("list", nrow(data))
	n_s <- 0
	nr <- nrow(data)
	if(nr==0)
	{
		stop("No reads pass the quality control.")
	}else{
		if(nr==1)
		{
			n_s <- n_s + 1
			reg_set[[n_s]] <- 1
		}else{
			pos <- data[1,'start']
			i <- 1
			while(i <= nr)
			{
				temp_set <- i
				j <- i + 1
				bd <- data[i,'end']
				while((j<=nr)&(bd>=data[j,'start']))
				{
					temp_set <- c(temp_set, j)
					bd <- max(bd, data[j,'end'])
					j <- j + 1		
				}
				n_s <- n_s + 1
				reg_set[[n_s]] <- temp_set
				
				i <- j
			}

		}
	}
	
	size <- unlist(lapply(reg_set, function(x) length(x)))[1:n_s]
	return(invisible(list(tile_reg=reg_set[1:n_s], size=size, num_r = n_s)))
}
