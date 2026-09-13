
#' @title insert_table_sql
#' @description Insert a table into the database
#' @param conn An established database connection
#' @param db_table_name Name of a table in the database
#' @param table A table in the database
#' @param batch_size Optional number of rows per INSERT statement. Default NULL,
#' which inserts the whole table in one statement.
#' @param check_db_table whether to check database table. Default = TRUE.
#' 
#' @keywords internal
#' 
#' @export
insert_table_sql <- function(
    conn, 
    db_table_name, 
    table,
    batch_size = NULL,
    check_db_table = TRUE
){
  
  # Get table column names
  db_col_names <- SigRepo::getDBColNames(
    conn = conn,
    db_table_name = db_table_name,
    check_db_table = check_db_table
  )
  
  # Check if table is a data frame object and not empty
  if(!methods::is(table, "data.frame") || base::length(table) == 0){
    # Disconnect from database ####
    base::suppressWarnings(DBI::dbDisconnect(conn))  
    # Return error message
    base::stop("\n'table' must be a data frame object and cannot be empty.\n")
  }
  
  # If table is not empty, import table into database
  if(base::nrow(table) > 0){
    
    # Get overlapping column names
    tbl_col_names <- base::colnames(table)[base::which(base::colnames(table) %in% db_col_names)]
    
    # Join column variables
    coln_var <- paste0("(", base::paste0(tbl_col_names, collapse = ", "), ")")    
    
    if (base::is.null(batch_size) || !base::is.numeric(batch_size) || batch_size <= 0) {
      batch_size <- base::nrow(table)
    }
    batch_size <- base::as.integer(batch_size[1])

    row_batches <- base::split(
      base::seq_len(base::nrow(table)),
      ceiling(base::seq_len(base::nrow(table)) / batch_size)
    )

    purrr::walk(
      row_batches,
      function(batch_rows) {
        batch_tbl <- table[batch_rows, , drop = FALSE]

        coln_val <- base::seq_len(base::nrow(batch_tbl)) |>
          purrr::map_chr(
            function(r){
              row_values <- purrr::map_chr(
                tbl_col_names,
                function(col_name) {
                  cell_value <- batch_tbl[r, col_name][[1]]
                  if (cell_value %in% c("'NULL'", "NULL")) {
                    return("NULL")
                  }
                  as.character(DBI::dbQuoteString(conn, as.character(cell_value)))
                }
              )

              values <- base::paste0(row_values, collapse = ", ")
              if(r < base::nrow(batch_tbl)){
                values <- base::paste0("(", values, "),\n")
              }else{
                values <- base::paste0("(", values, ");\n")
              }
            }
          ) |> base::paste0(collapse = "")

        statement <- base::sprintf(
          "
          INSERT INTO %s %s
          VALUES %s
          ", db_table_name, coln_var, coln_val
        )

        base::tryCatch({
          base::suppressWarnings(DBI::dbGetQuery(conn = conn, statement = statement))
        }, error = function(e){
          base::suppressWarnings(DBI::dbDisconnect(conn))
          base::stop(e, "\n")
        })
      }
    )
    
  }
}

#' @title delete_table_sql
#' @description delete an entry from database table
#' @param conn An established database connection
#' @param db_table_name Name of a table in the database
#' @param delete_coln_var A column variable in the table for removing rows
#' @param delete_coln_val A list of values associated with delete_coln_var to be removed.
#' @param check_db_table whether to check database table. Default = TRUE.
#' @keywords internal
#' @export
delete_table_sql <- function(
    conn, 
    db_table_name, 
    delete_coln_var, 
    delete_coln_val,
    check_db_table = TRUE
){
  
  # Get table column names
  db_col_names <- SigRepo::getDBColNames(
    conn = conn,
    db_table_name = db_table_name,
    check_db_table = check_db_table
  )
  
  # Check delete_coln_var
  if(!base::length(delete_coln_var) == 1 || base::any(delete_coln_var %in% c(NA, ""))){
    # Disconnect from database ####
    base::suppressWarnings(DBI::dbDisconnect(conn))  
    # Return error message
    base::stop("\n'delete_coln_var' must have length of 1 and cannot be empty.\n")
  }
  
  # Check column fields
  if(base::any(!delete_coln_var %in% db_col_names)){
    # Disconnect from database ####
    base::suppressWarnings(DBI::dbDisconnect(conn))  
    # Return error message
    base::stop(base::sprintf("\n'%s' table does not have the following column names: %s.\n", db_table_name, base::paste0(delete_coln_var[base::which(!delete_coln_var %in% db_col_names)], collapse = ", ")))
  }
  
  # Check delete_coln_val
  if(base::length(delete_coln_val) == 0 || base::any(delete_coln_val %in% c(NA, ""))){
    # Disconnect from database ####
    base::suppressWarnings(DBI::dbDisconnect(conn))  
    # Return error message
    base::stop("\n'delete_coln_val' cannot be empty.\n")
  }
  
  # Create a where clause to remove entry 
  delete_where_clause <- base::paste0(delete_coln_var, " IN (", base::paste0("'", delete_coln_val, "'", collapse = ", "), ")")
  
  # Create sql statement
  statement <- base::sprintf(
    "
    DELETE FROM %s \n
    WHERE %s;
    ", db_table_name, delete_where_clause
  )
  
  # Set foreign key checks to false when dropping tables
  base::suppressWarnings(DBI::dbGetQuery(conn = conn, statement = "SET FOREIGN_KEY_CHECKS=0;"))
  
  # Delete entry from database
  base::tryCatch({
    base::suppressWarnings(DBI::dbGetQuery(conn = conn, statement = statement))
  }, error = function(e){
    # Disconnect from database ####
    base::suppressWarnings(DBI::dbDisconnect(conn))  
    # Return error message
    base::stop(e, "\n")
  })
  
}

#' Build the WHERE clause lookup_table_sql() sends to the database
#'
#' Every filter is a pair of predicates on the same quoted values:
#' `(col IN ('a', 'b') AND trim(lower(col)) IN ('a', 'b'))`. The bare
#' predicate lets MySQL use the column's index -- `trim(lower(col))` alone
#' forces a full scan: looking up one signature's features in
#' signature_feature_set took 1.31 s scanning 1.3 million rows, and 0.002 s
#' through the index. The original `trim(lower(col))` predicate keeps the
#' original matching semantics exactly: on its own the bare predicate would
#' also match integer columns against strings such as '1abc', '01' or '1.0'
#' through MySQL's string-to-number conversion. It only runs on the rows the
#' index returns. Values are prepared as before (lower-cased, then trimmed)
#' and quoted in one vectorised DBI::dbQuoteString() call -- quoting them one
#' at a time cost 0.9 s per 15,000 feature ids.
#'
#' @param conn A DBI connection (or DBI::ANSI()) used to quote values.
#' @param filter_coln_var Column names to filter on.
#' @param filter_coln_val A named list of values per column in filter_coln_var.
#' @param filter_var_by Logical operators joining the filters, length
#'   length(filter_coln_var) - 1.
#' @return The clause without the WHERE keyword.
#' @noRd
build_lookup_where_clause <- function(conn, filter_coln_var, filter_coln_val, filter_var_by = NULL){

  clauses <- base::vapply(
    base::seq_along(filter_coln_var),
    function(s){
      values <- base::trimws(base::tolower(filter_coln_val[[filter_coln_var[s]]]))
      if(base::length(values) == 0){
        # A filter column with zero values can never match a row -- emit an
        # explicit always-false clause rather than invalid `IN ()` SQL. ####
        return("1 = 0")
      }
      value_list <- base::paste0(base::as.character(DBI::dbQuoteString(conn, values)), collapse = ", ")
      # The bare predicate lets MySQL use the index; the trim(lower()) one
      # keeps the original matching on the rows the index returns. ####
      base::sprintf("(%1$s IN (%2$s) AND trim(lower(%1$s)) IN (%2$s))", filter_coln_var[s], value_list)
    },
    base::character(1)
  )

  # Join the filters with their logical operators: a1 op1 a2 op2 a3 ... ####
  n <- base::length(clauses)
  if(n > 1){
    clauses[-n] <- base::paste(clauses[-n], filter_var_by[base::seq_len(n - 1)])
  }
  base::paste(clauses, collapse = " ")
}

#' @title lookup_table_sql
#' @description Look up a list of variables based on a particular variable 
#' and its associated values in the database
#' @param conn An established database connection
#' @param db_table_name A table in the database
#' @param return_var a list of column variables to be returned from the given table. 
#' Default '*' (means everything).
#' @param exclude_return_var a list of column names to be excluded from the returned table.
#' @param filter_coln_var a list of column variables in the given table. Default NULL.
#' @param filter_coln_val a list of values associated with 'filter_coln_var' variables. 
#' Most importantly, 'filter_coln_val' must have names or labels that matched the values of 'filter_coln_var'.
#' Default NULL.
#' @param filter_var_by if length(filter_coln_var) > 1, then 'filter_var_by' must be
#' provided as a vector of logical operators (e.g., OR/AND) with n = length(filter_coln_var) - 1. 
#' Default NULL.
#' @param check_db_table Check whether table exists in the database. Default = TRUE.
#' @keywords internal
#' @export
lookup_table_sql <- function(
    conn, 
    db_table_name, 
    return_var = "*", 
    exclude_return_var = NULL,
    filter_coln_var = NULL, 
    filter_coln_val = NULL, 
    filter_var_by = NULL, 
    check_db_table = TRUE
){
  
  # Get table column names
  db_col_names <- SigRepo::getDBColNames(
    conn = conn,
    db_table_name = db_table_name,
    check_db_table = check_db_table
  )
  
  # Check return_var
  if(base::length(return_var) == 0 || base::any(return_var %in% c(NA, ""))){
    # Disconnect from database ####
    base::suppressWarnings(DBI::dbDisconnect(conn))  
    # Return error message
    base::stop("\n'return_var' cannot be empty.\n")
  }
  
  # Check column fields
  if(base::any(!filter_coln_var %in% db_col_names)){
    # Disconnect from database ####
    base::suppressWarnings(DBI::dbDisconnect(conn))  
    # Return error message
    base::stop(base::sprintf("\n'%s' table does not have the following column names: %s.\n", db_table_name, base::paste0(filter_coln_var[base::which(!filter_coln_var %in% db_col_names)], collapse = ", ")))
  }
  
  # Check filter_coln_var and filter_coln_val
  if(base::length(filter_coln_var) == 0 && base::length(filter_coln_val) == 0){
    
    where_clause <- ""
    
  }else{  
    
    if(base::length(filter_coln_var) != base::length(filter_coln_val) || !base::all(base::names(filter_coln_val) %in% filter_coln_var)){
      # Disconnect from database ####
      base::suppressWarnings(DBI::dbDisconnect(conn))  
      # Return error message
      base::stop(
        "\nThe length of 'filter_coln_var' must equal to the length of 'filter_coln_val'.\n",
        "\nFurthermore, 'filter_coln_val' must a list with names or labels that matched the values of 'filter_coln_var'.\n"
      )
    }
    
    if((base::length(filter_coln_var) > 1) && (base::length(filter_var_by) != (base::length(filter_coln_var)-1)) && (!base::any(toupper(filter_var_by) %in% c("OR", "AND")))){
      # Disconnect from database ####
      base::suppressWarnings(DBI::dbDisconnect(conn))  
      # Return error message
      base::stop("\n'filter_var_by' must contain a vector of logical operators (e.g, AND/OR) with n = length(filter_coln_var) - 1.\n")
    }
    
    # Create a where clause to look up values. Values are escaped with
    # DBI::dbQuoteString() (rather than pasted into the statement with manual
    # quotes) since filter_coln_val routinely carries caller-supplied search
    # text -- unescaped interpolation here would be SQL injectable. Each
    # filter pairs a bare-column predicate, which lets MySQL use the index,
    # with the original trim(lower(col)) predicate, which keeps the original
    # matching exactly (a bare integer column would also match strings such
    # as '1abc') and only runs on the rows the index returns; see
    # build_lookup_where_clause(). ####
    where_clause <- base::paste0(
      "WHERE ",
      build_lookup_where_clause(
        conn = conn,
        filter_coln_var = filter_coln_var,
        filter_coln_val = filter_coln_val,
        filter_var_by = filter_var_by
      )
    )
    
  }
  
  # Create a list of return variables
  return_var_list <- base::paste0(return_var, collapse = ", ")
  
  # Create SQL statement to return table
  statement <- base::sprintf(
    "
    SELECT %s
    FROM %s %s
    ", return_var_list, db_table_name, where_clause
  )
  
  # Get table
  table <- base::tryCatch({
    base::suppressWarnings(DBI::dbGetQuery(conn = conn, statement = statement))
  }, error = function(e){
    # Disconnect from database ####
    base::suppressWarnings(DBI::dbDisconnect(conn))  
    # Return error message
    base::stop(e, "\n")
  }) 
  
  # Whether to exclude selected column names from the returned table
  if(base::length(exclude_return_var) > 0){
    table <- table |> dplyr::select(dplyr::all_of(base::colnames(table)[base::which(!base::colnames(table) %in% exclude_return_var)]))
  }
  
  # Return table
  return(table)
  
}
