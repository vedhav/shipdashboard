DROP DATABASE ships;
CREATE DATABASE ships;
USE ships;

CREATE TABLE `ais_data` (
    `SHIP_ID` FLOAT(32) NOT NULL,
    `LAT` FLOAT(32) NOT NULL,
    `LON` FLOAT(32) NOT NULL,
    `SPEED` FLOAT(32) NOT NULL,
    `COURSE` FLOAT(32) NOT NULL,
    `HEADING` FLOAT(32) NOT NULL,
    `DESTINATION` VARCHAR(50) DEFAULT NULL,
    `DATETIME` TIMESTAMP NOT NULL,
    `vessel_port` VARCHAR(50) NOT NULL,
    `location_port` VARCHAR(50) NOT NULL,
    `is_parked` TINYINT(1) NOT NULL
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4;

CREATE TABLE `ships_data` (
    `SHIP_ID` FLOAT(32) NOT NULL,
    `SHIPNAME` VARCHAR(250) NOT NULL,
    `ship_type` ENUM('Cargo', 'Fishing', 'High Special', 'Navigation', 'Passenger', 'Pleasure', 'Tanker', 'Tug', 'Unspecified') NOT NULL,
    `LENGTH` FLOAT(16) DEFAULT NULL,
    `WIDTH` FLOAT(16) DEFAULT NULL,
    `DWT` FLOAT(16) DEFAULT NULL,
    `FLAG` VARCHAR(8) NOT NULL,
    UNIQUE KEY `SHIP_ID` (`SHIP_ID`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4;

ALTER TABLE `ships_data` ADD INDEX(`SHIP_ID`);
ALTER TABLE `ships_data` ADD INDEX(`SHIPNAME`);
ALTER TABLE `ais_data` ADD INDEX(`SHIP_ID`);

-- After this run populate_ships.R to populate this table from R