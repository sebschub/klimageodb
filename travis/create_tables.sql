CREATE TABLE site (
    site_id       serial PRIMARY KEY,
    site_name     varchar(20) NOT NULL UNIQUE,
    site_lat      real CHECK (site_lat      >=  -90. AND site_lat      <=    90.),
    site_lon      real CHECK (site_lon      >= -180. AND site_lat      <=   180.),
    site_altitude real CHECK (site_altitude >= -500. AND site_altitude <= 10000.),
    site_comment  varchar(50),
    CONSTRAINT site_latlon_consistency_check
      CHECK( (site_lat IS NULL AND site_lon IS NULL) | (site_lat IS NOT NULL AND site_lon IS NOT NULL) )
    );
  COMMENT ON TABLE  site               IS 'measurement site or campaign';
  COMMENT ON COLUMN site.site_id       IS 'ID';
  COMMENT ON COLUMN site.site_name     IS 'name of site or name of campaign';
  COMMENT ON COLUMN site.site_lat      IS 'geographical latitude WGS84';
  COMMENT ON COLUMN site.site_lon      IS 'geographical longitude WGS84';
  COMMENT ON COLUMN site.site_altitude IS 'height above sea level of surface in m';
  COMMENT ON COLUMN site.site_comment  IS 'additional information';


CREATE TABLE device_manufacturer (
    devman_id      serial PRIMARY KEY,
    devman_name    varchar(20) NOT NULL UNIQUE,
    devman_comment varchar(50)
    );
  COMMENT ON TABLE  device_manufacturer                IS 'device manufacturer';
  COMMENT ON COLUMN device_manufacturer.devman_id      IS 'ID';
  COMMENT ON COLUMN device_manufacturer.devman_name    IS 'name';
  COMMENT ON COLUMN device_manufacturer.devman_comment IS 'additional information';


CREATE TABLE device_type (
    devtype_id      serial PRIMARY KEY,
    devtype_name    varchar(20) NOT NULL UNIQUE,
    devtype_comment varchar(50)
    );
  COMMENT ON TABLE  device_type                 IS 'measurement device type';
  COMMENT ON COLUMN device_type.devtype_id      IS 'ID';
  COMMENT ON COLUMN device_type.devtype_name    IS 'name, e.g. thermometer';
  COMMENT ON COLUMN device_type.devtype_comment IS 'additional information';


CREATE TABLE device_model (
    devmod_id      serial PRIMARY KEY,
    devmod_name    varchar(20) NOT NULL UNIQUE,
    devman_id      integer REFERENCES device_manufacturer(devman_id),
    devtype_id     integer NOT NULL REFERENCES device_type(devtype_id),
    devmod_comment varchar(50)
    );
  COMMENT ON TABLE  device_model                IS 'measurement device model';
  COMMENT ON COLUMN device_model.devmod_id      IS 'ID';
  COMMENT ON COLUMN device_model.devmod_name    IS 'name of model';
  COMMENT ON COLUMN device_model.devman_id      IS 'references device manufacturer';
  COMMENT ON COLUMN device_model.devtype_id     IS 'references type of device';
  COMMENT ON COLUMN device_model.devmod_comment IS 'additional information';


CREATE TABLE device (
    dev_id                     serial PRIMARY KEY,
    dev_name                   varchar(40) NOT NULL UNIQUE,
    devmod_id                  integer NOT NULL REFERENCES device_model(devmod_id),
    dev_identifier             varchar(30) UNIQUE,
    dev_comment                varchar(50)
    );
  COMMENT ON TABLE  device                           IS 'measurement device';
  COMMENT ON COLUMN device.dev_id                    IS 'ID';
  COMMENT ON COLUMN device.dev_name                  IS 'name';
  COMMENT ON COLUMN device.devmod_id                 IS 'references device model';
  COMMENT ON COLUMN device.dev_identifier            IS 'identifier, e.g. serial number';
  COMMENT ON COLUMN device.dev_comment               IS 'additional information';


CREATE TABLE calibrated_device (
    caldev_id         serial PRIMARY KEY,
    dev_id            integer NOT NULL REFERENCES device(dev_id),
    caldev_datetime   timestamp WITH TIME ZONE CHECK (caldev_datetime >= '1980-01-01' AND caldev_datetime < NOW()),
    caldev_parameter  varchar(50),
    caldev_comment    varchar(50),
    UNIQUE (dev_id, caldev_datetime)
    );
  COMMENT ON TABLE  calibrated_device                  IS 'specific calibration of device';
  COMMENT ON COLUMN calibrated_device.caldev_datetime  IS 'date of calibration';
  COMMENT ON COLUMN calibrated_device.caldev_parameter IS 'values of calibration parameters';
  COMMENT ON COLUMN calibrated_device.caldev_comment   IS 'additional information';


CREATE TABLE physical_quantity (
    pq_id         serial PRIMARY KEY,
    pq_name       varchar(20) NOT NULL,
    pq_unit       varchar(10) NOT NULL,
    pq_comment    varchar(50),
    UNIQUE (pq_name, pq_unit)
    );
  COMMENT ON TABLE  physical_quantity            IS 'physical quantity, e.g. air temperature';
  COMMENT ON COLUMN physical_quantity.pq_id      IS 'ID';
  COMMENT ON COLUMN physical_quantity.pq_name    IS 'name, e.g. air temperature';
  COMMENT ON COLUMN physical_quantity.pq_unit    IS 'unit, "1" for unitless';
  COMMENT ON COLUMN physical_quantity.pq_comment IS 'additional information';



CREATE TABLE integration_type (
    inttype_id          serial PRIMARY KEY,
    inttype_name        varchar(20) NOT NULL UNIQUE,
    inttype_description varchar(100) NOT NULL
    );
  COMMENT ON TABLE  integration_type                     IS 'integration type';
  COMMENT ON COLUMN integration_type.inttype_id          IS 'ID';
  COMMENT ON COLUMN integration_type.inttype_name        IS 'name';
  COMMENT ON COLUMN integration_type.inttype_description IS 'detailed description';



CREATE TABLE integration (
    int_id                   serial PRIMARY KEY,
    inttype_id               integer NOT NULL REFERENCES integration_type(inttype_id),
    int_measurement_interval real NOT NULL
      CHECK (int_measurement_interval > 0. AND int_measurement_interval <= 86400.),
    int_interval             real NOT NULL,
      CHECK (int_interval             > 0. AND int_interval             <= 86400.),
    int_comment              varchar(50),
    UNIQUE (inttype_id, int_measurement_interval, int_interval),
    CONSTRAINT integration_intervals_consistency_check CHECK (int_measurement_interval <= int_interval),
    CONSTRAINT integration_single_measurement_consistency_check
      CHECK ((inttype_id = 1 AND int_measurement_interval = int_interval) OR inttype_id > 1)
  -- iff inttype_id = 1, int_measurement_interval = int_interval
    );
  COMMENT ON TABLE  integration                          IS 'measurement integration';
  COMMENT ON COLUMN integration.int_id                   IS 'ID';
  COMMENT ON COLUMN integration.inttype_id               IS 'reference to integration type';
  COMMENT ON COLUMN integration.int_measurement_interval IS 'interval between measurements in s';
  COMMENT ON COLUMN integration.int_interval             IS 'integration interval in s of one stored measurement (see inttype_id)';
  COMMENT ON COLUMN integration.int_comment              IS 'additional information';


CREATE TABLE person (
    pers_id      serial PRIMARY KEY,
    pers_name    varchar(30) NOT NULL UNIQUE,
    pers_comment varchar(50)
    );
  COMMENT ON TABLE  person              IS 'person';
  COMMENT ON COLUMN person.pers_id      IS 'ID';
  COMMENT ON COLUMN person.pers_name    IS 'name';
  COMMENT ON COLUMN person.pers_comment IS 'additional information';


CREATE TABLE measurand (
    md_id             serial PRIMARY KEY,
    md_name           varchar(10) NOT NULL,
    md_setup_datetime timestamp with time zone NOT NULL CHECK (md_setup_datetime >= '1980-01-01' AND md_setup_datetime < NOW()),
    pq_id             integer NOT NULL REFERENCES physical_quantity(pq_id),
    site_id           integer NOT NULL REFERENCES site(site_id),
    caldev_id         integer NOT NULL REFERENCES calibrated_device(caldev_id),
    md_height         real CHECK (md_height >= -10. AND md_height <= 10000.),
    int_id            integer NOT NULL REFERENCES integration(int_id),
    pers_id           integer REFERENCES person(pers_id),
    md_comment        varchar(50),
    UNIQUE (md_name, md_setup_datetime),
    UNIQUE (md_setup_datetime, pq_id, site_id, caldev_id, md_height, int_id)
    -- if md_setup_datetime and dev_id equal, site_id unique
    );
  COMMENT ON TABLE  measurand                   IS 'combination of physical quantity, site, integration...';
  COMMENT ON COLUMN measurand.md_id             IS 'ID';
  COMMENT ON COLUMN measurand.md_name           IS 'name, used e.g. in a logger';
  COMMENT ON COLUMN measurand.md_setup_datetime IS 'set-up date and time of measurand';
  COMMENT ON COLUMN measurand.pq_id             IS 'reference to measured physical quantity';
  COMMENT ON COLUMN measurand.site_id           IS 'reference to site where measures';
  COMMENT ON COLUMN measurand.caldev_id         IS 'reference to measuring calibrated device';
  COMMENT ON COLUMN measurand.md_height         IS 'measurement height';
  COMMENT ON COLUMN measurand.int_id            IS 'reference to integration details';
  COMMENT ON COLUMN measurand.pers_id           IS 'reference to contact person';
  COMMENT ON COLUMN measurand.md_comment        IS 'additional information';



CREATE TABLE stationadlershof_measurement_raw (
    statadmr_id       bigserial PRIMARY KEY,
    statadmr_datetime timestamp with time zone NOT NULL
      CHECK (statadmr_datetime >= '2000-01-01' AND statadmr_datetime < NOW()),
    md_id             integer NOT NULL REFERENCES measurand(md_id),
    statadmr_value    double precision,
    UNIQUE (statadmr_datetime, md_id)
    );
  COMMENT ON TABLE  stationadlershof_measurement_raw                   IS 'unchecked measurements at Adlershof sites';
  COMMENT ON COLUMN stationadlershof_measurement_raw.statadmr_id       IS 'ID';
  COMMENT ON COLUMN stationadlershof_measurement_raw.statadmr_datetime IS 'date and time (with time zone) of measurement';
  COMMENT ON COLUMN stationadlershof_measurement_raw.md_id             IS 'reference to measurand that was measured';
  COMMENT ON COLUMN stationadlershof_measurement_raw.statadmr_value    IS 'actual value of measurement';


CREATE VIEW device_model_detail AS
  SELECT devmod_id, devmod_name, devtype_name, devman_name, devmod_comment FROM device_model
    LEFT OUTER JOIN device_type         ON (device_model.devtype_id  = device_type.devtype_id)
    LEFT OUTER JOIN device_manufacturer ON (device_model.devman_id   = device_manufacturer.devman_id);

CREATE VIEW device_detail AS
  SELECT dev_id, dev_name, devtype_name, devmod_name, devman_name, dev_comment FROM device
    LEFT OUTER JOIN device_model_detail ON (device.devmod_id         = device_model_detail.devmod_id);

CREATE VIEW calibrated_device_detail AS
  SELECT caldev_id, dev_name, devtype_name, devmod_name, devman_name, caldev_datetime, caldev_parameter, caldev_comment FROM calibrated_device
    LEFT OUTER JOIN device_detail ON (calibrated_device.dev_id = device_detail.dev_id);

CREATE VIEW integration_detail AS
  SELECT int_id, inttype_name, int_measurement_interval, int_interval, int_comment FROM integration
    LEFT OUTER JOIN integration_type ON (integration.inttype_id = integration_type.inttype_id);

CREATE VIEW measurand_detail AS
  SELECT md_id, md_name, md_setup_datetime, pq_name, pq_unit, site_name, dev_name, devmod_name, caldev_datetime, md_height, inttype_name, int_measurement_interval, int_interval, pers_name, md_comment FROM measurand
    LEFT OUTER JOIN physical_quantity        ON (measurand.pq_id = physical_quantity.pq_id)
    LEFT OUTER JOIN site                     ON (measurand.site_id = site.site_id)
    LEFT OUTER JOIN calibrated_device_detail ON (measurand.caldev_id = calibrated_device_detail.caldev_id)
    LEFT OUTER JOIN integration_detail       ON (measurand.int_id = integration_detail.int_id)
    LEFT OUTER JOIN person                   ON (measurand.pers_id = person.pers_id);
