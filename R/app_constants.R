# database connection related constants
default_db_port <<- 3306
default_db_name <<- Sys.getenv("MYSQL_DB_NAME", "ships")
default_host_ip <<- Sys.getenv("DB_HOST_IP", "127.0.0.1")
default_host_name <<- Sys.getenv("DB_HOST_NAME", "root")
default_host_password <<- Sys.getenv("DB_HOST_PASSWORD", "admin@123")

# default_db_name <<- Sys.getenv("MYSQL_DB_NAME", "ships")
# default_host_ip <<- Sys.getenv("DB_HOST_IP", "104.197.126.18")
# default_host_name <<- Sys.getenv("DB_HOST_NAME", "vedha")
# default_host_password <<- Sys.getenv("DB_HOST_PASSWORD", "MySQLP@ssW0rD")

# app related global constants
ship_types_enum <<- c('Cargo', 'Fishing', 'High Special', 'Navigation', 'Passenger', 'Pleasure', 'Tanker', 'Tug', 'Unspecified')
