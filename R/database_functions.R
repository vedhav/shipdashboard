# Kills all the unclosed db connections that occur due to exceptions or timeouts
kill_dbx_connections <- function () {
    all_cons <- dbListConnections(MySQL())
    for(con in all_cons)
        dbxDisconnect(con)
    print(paste(length(all_cons), " connections killed."))
    print("If you have not set up the MySQL database please swith to local_file branch of the repo { git checkout local_file } so you can use this app without it")
}

#' To insert new values inside the MySQL table
#'
#' @param table_name A string which contains the MySQL table name where the data has to be inserted
#' @param values A tbl_df/tbl/data.frame which is the same structure as the MySQL table
#' @param db_name A string which contains the name of the database
#' @param host_name A string which contains the username to access the database
#' @param host_password A string which contains the password to access the database
#' @param host_ip A string which contains the host address of the database
#'
#' @return Returns the result obtained after the insert, NULL if the data was inserted successfully
insert_into_db <- function(table_name, values, db_name = default_db_name, host_name = default_host_name,
    host_password = default_host_password, host_ip = default_host_ip) {
    #adding support for '\' - extending dbx
    for(field in 1:length(values)) {
        if(typeof(values[[field]]) == 'character') {
            values[[field]] <- gsub("[\\]", "\\\\\\\\", values[[field]])
        }
    }
    conn <- tryCatch({
        dbxConnect(
            adapter = "mysql",
            user = host_name,
            password = host_password,
            host = host_ip,
            port = default_db_port,
            dbname = db_name
        )
    }, error = function(err) {
        kill_dbx_connections()
        dbxConnect(
            adapter = "mysql",
            user = host_name,
            password = host_password,
            host = host_ip,
            port = default_db_port,
            dbname = db_name
        )
    })
    result <- suppressWarnings(dbxInsert(conn, table_name, values, batch_size = 1000))
    on.exit(dbxDisconnect(conn))
    return(result)
}

#' To fetch data from database using select query
#'
#' @param query A string with the select query
#' @param params A list of optional parameters that fill the ? in the query
#' @param db_name A string which contains the name of the database
#' @param host_name A string which contains the username to access the database
#' @param host_password A string which contains the password to access the database
#' @param host_ip A string which contains the host address of the database
#'
#' @return A tibble which is the result of the query
get_data_from_db <- function(query, params = NULL, db_name = default_db_name, host_name = default_host_name,
    host_password = default_host_password, host_ip = default_host_ip) {
    if(query =="") {
        return(data.frame())
    }
    conn <- tryCatch({
        dbxConnect(
            adapter = "mysql",
            user = host_name,
            password = host_password,
            host = host_ip,
            port = default_db_port,
            dbname = db_name
        )
    }, error = function(err) {
        kill_dbx_connections()
        dbxConnect(
            adapter = "mysql",
            user = host_name,
            password = host_password,
            host = host_ip,
            port = default_db_port,
            dbname = db_name
        )
    })
    result <- NULL
    if(!is.null(params)) {
        #adding support for '\' - extending dbx
        for(field in 1:length(params))
        {
            if(typeof(params[[field]]) == 'character')
            {
                params[[field]] <- gsub("[\\]", "\\\\\\\\", params[[field]])
            }
        }
        result <- suppressWarnings(dbxSelect(conn, query, params))
    } else {
        result <- suppressWarnings(dbxSelect(conn, query))
    }
    #To remove the unnecessary results that we get from executing a stored procedure
    while(RMySQL::dbMoreResults(conn)) {
        try(RMySQL::dbNextResult(conn))
    }
    on.exit(dbxDisconnect(conn))
    return(tibble::as_tibble(result))
}
