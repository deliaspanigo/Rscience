

super_sheet_info <- function(name_sheets){

  pos_sheets <- 1:length(name_sheets)

  # Stock pos
  mount_digits_pos <- nchar(as.character(max(pos_sheets)))
  if(mount_digits_pos < 2) mount_digits_pos <- 2
  stock_pos_sheets <- stringr::str_pad(string = pos_sheets, width = mount_digits_pos,
                                       side = "left", pad = "0")

  # Stock name
  mount_digits_name <- max(nchar(name_sheets))
  stock_name_sheets <- stringr::str_pad(string = name_sheets, width = mount_digits_name,
                                        side = "right", pad = " ")


  # full_info_sheets <- paste0("Sheet ", stock_pos_sheets, " - ", stock_name_sheets)
  full_info_sheets <- paste0(stock_pos_sheets, " - ", stock_name_sheets)

  #output_vector <- name_sheets
  output_vector <- pos_sheets
  names(output_vector) <- full_info_sheets

  return(output_vector)

}


setup_var_info <- function(all_var_names){

  pos_var <- 1:length(all_var_names)
  letter_var <- openxlsx::int2col(pos_var)
  letter_var_mod <- paste0("(", letter_var, ")")

  # Stock pos
  mount_digits <- nchar(as.character(max(pos_var)))
  if(mount_digits < 2) mount_digits <- 2
  stock_pos_var <- stringr::str_pad(string = pos_var, width = mount_digits,
                                    side = "left", pad = "0")

  # Stock letter
  mount_letter <- max(nchar(letter_var_mod))
  stock_letter_var <- stringr::str_pad(string = letter_var_mod, width = mount_letter,
                                       side = "left", pad = " ")


  # Stock name
  mount_name <- max(nchar(all_var_names))
  stock_name_var <- stringr::str_pad(string = all_var_names, width = mount_name,
                                     side = "right", pad = " ")


  full_info_var <- paste0(stock_pos_var, " - ", stock_letter_var, " - ", stock_name_var)

  # output_vector <- pos_var
  output_vector <- all_var_names
  names(output_vector) <- full_info_var

  return(output_vector)

}

