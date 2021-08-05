# constants
min_duration <- 60 #data shorter than min_duration in seconds will be excluded.

# functions
reverse_code <- function(min_x, max_x, x) {
  #takes a vector of values and reverse codes it
  x_rev <- rep(NA, length(x))
  orig_range <- min_x:max_x
  reverse_range <- rev(orig_range)
  for (i in orig_range){
    x_rev <- ifelse(x == orig_range[i],reverse_range[i], x_rev)
  }
  return(x_rev)
}

create_scale <- function(scales_tbl, main_df) {
  # takes a main data frame with existing questions,
  # and based on the specs in scales_tbl computes psych scales
  # the scales are added to be main_df, and the updated df is returned
  #scales_tbl <- scales_tbl
  #main_df <- df_cleaned
  
  # remove nas from the last rows (if any)
  scales_tbl <- scales_tbl[!is.na(scales_tbl$question_label),]
  
  # reverse code the revent questions
  for (i in 1:nrow(scales_tbl)) {
    if (scales_tbl$reverse_coded[i] == 1) {
      main_df[,scales_tbl$question_label[i]] <- reverse_code(
        scales_tbl$min_value[i],
        scales_tbl$max_value[i],
        main_df[,scales_tbl$question_label[i]])
    }
  }
  
  
  
  scale_names <- unique(scales_tbl$scale_name) 
  
  for (scale_name in scale_names) {
    questions <- scales_tbl$question_label[scales_tbl$scale_name == scale_name]
    main_df[scale_name] <- rowMeans(main_df[,questions], na.rm = TRUE)
  }
  return(main_df)
}

pi = function(p, gamma) {
  (p^gamma)/(p^gamma + (1-p)^gamma)^(1/gamma)
}
pi_vect <- Vectorize(pi)

P_luce <- function(x1,p1,x2,p2,alpha, beta, gamma){
  
  ESU1 <- (x1^alpha)*pi(p1, gamma)
  ESU2 <- (x2^alpha)*pi(p2, gamma)
  
  output <- 1/(1+exp(-beta*(ESU1-ESU2)))
  
  # if the outcomes are negative, swap the options and make them them positive 
  #if((ESU1 < 0) & (ESU2< 0)) {output <- 1/(1+exp((ESU2-ESU1))) }
  return(output)
}

P_luce_vect <- Vectorize(P_luce)
