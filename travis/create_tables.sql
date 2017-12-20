CREATE TABLE site (
    site_id       smallserial PRIMARY KEY,
    site_name     varchar(30) NOT NULL UNIQUE,
    site_lat      double precision CHECK (site_lat      >=  -90. AND site_lat      <=    90.),
    site_lon      double precision CHECK (site_lon      >= -180. AND site_lat      <=   180.),
    site_altitude double precision CHECK (site_altitude >= -500. AND site_altitude <= 10000.),
    site_comment  varchar(50),
    CONSTRAINT site_latlon_consistency_check
      CHECK( (site_lat IS NULL AND site_lon IS NULL) OR (site_lat IS NOT NULL AND site_lon IS NOT NULL) )
    );
  COMMENT ON TABLE  site               IS 'measurement site or campaign';
  COMMENT ON COLUMN site.site_id       IS 'ID';
  COMMENT ON COLUMN site.site_name     IS 'name of site or name of campaign';
  COMMENT ON COLUMN site.site_lat      IS 'geographical latitude WGS84';
  COMMENT ON COLUMN site.site_lon      IS 'geographical longitude WGS84';
  COMMENT ON COLUMN site.site_altitude IS 'height above sea level of surface in m';
  COMMENT ON COLUMN site.site_comment  IS 'additional information';


CREATE TABLE device_manufacturer (
    devman_id      smallserial PRIMARY KEY,
    devman_name    varchar(30) NOT NULL UNIQUE,
    devman_comment varchar(50)
    );
  COMMENT ON TABLE  device_manufacturer                IS 'device manufacturer';
  COMMENT ON COLUMN device_manufacturer.devman_id      IS 'ID';
  COMMENT ON COLUMN device_manufacturer.devman_name    IS 'name';
  COMMENT ON COLUMN device_manufacturer.devman_comment IS 'additional information';


CREATE TABLE device_type (
    devtype_id      smallserial PRIMARY KEY,
    devtype_name    varchar(50) NOT NULL UNIQUE,
    devtype_comment varchar(50)
    );
  COMMENT ON TABLE  device_type                 IS 'measurement device type';
  COMMENT ON COLUMN device_type.devtype_id      IS 'ID';
  COMMENT ON COLUMN device_type.devtype_name    IS 'name, e.g. thermometer';
  COMMENT ON COLUMN device_type.devtype_comment IS 'additional information';


CREATE TABLE device_model (
    devmod_id      smallserial PRIMARY KEY,
    devmod_name    varchar(30) NOT NULL UNIQUE,
    devtype_id     smallint NOT NULL REFERENCES device_type(devtype_id),
    devman_id      smallint REFERENCES device_manufacturer(devman_id),
    devmod_comment varchar(50)
    );
  COMMENT ON TABLE  device_model                IS 'measurement device model';
  COMMENT ON COLUMN device_model.devmod_id      IS 'ID';
  COMMENT ON COLUMN device_model.devmod_name    IS 'name of model';
  COMMENT ON COLUMN device_model.devtype_id     IS 'references type of device';
  COMMENT ON COLUMN device_model.devman_id      IS 'references device manufacturer';
  COMMENT ON COLUMN device_model.devmod_comment IS 'additional information';


CREATE TABLE device (
    dev_id                     smallserial PRIMARY KEY,
    dev_name                   varchar(40) NOT NULL UNIQUE,
    devmod_id                  smallint NOT NULL REFERENCES device_model(devmod_id),
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
    caldev_id         smallserial PRIMARY KEY,
    dev_id            smallint NOT NULL REFERENCES device(dev_id),
    caldev_datetime   timestamp WITH TIME ZONE CHECK (caldev_datetime >= '1980-01-01' AND caldev_datetime < NOW()),
    caldev_parameter  varchar(200),
    caldev_comment    varchar(50)
    );
  -- there should be only one entry for each dev_id and caldev_datetime including NULL
  -- NULL caldev_datetime are mapped to '0001-01-01'; ensure that this is never a real value
  CREATE UNIQUE INDEX calibrated_device_dev_id_caldev_datetime ON calibrated_device
    (dev_id, COALESCE(caldev_datetime, '0001-01-01+00'));
  COMMENT ON TABLE  calibrated_device                  IS 'specific calibration of device';
  COMMENT ON COLUMN calibrated_device.caldev_datetime  IS 'date and time of calibration';
  COMMENT ON COLUMN calibrated_device.caldev_parameter IS 'values of calibration parameters';
  COMMENT ON COLUMN calibrated_device.caldev_comment   IS 'additional information';


CREATE TABLE physical_quantity (
    pq_id          smallserial PRIMARY KEY,
    pq_name        varchar(200) NOT NULL,
    pq_unit        varchar(20) NOT NULL,
    pq_description varchar(3000),
    pq_comment     varchar(50),
    UNIQUE (pq_name)
    );
  COMMENT ON TABLE  physical_quantity                IS 'physical quantity following CF conventions';
  COMMENT ON COLUMN physical_quantity.pq_id          IS 'ID';
  COMMENT ON COLUMN physical_quantity.pq_name        IS 'name, e.g. air_temperature';
  COMMENT ON COLUMN physical_quantity.pq_unit        IS 'unit, "1" for unitless';
  COMMENT ON COLUMN physical_quantity.pq_description IS 'description';
  COMMENT ON COLUMN physical_quantity.pq_comment     IS 'additional information';



CREATE TABLE integration_type (
    inttype_id          smallserial PRIMARY KEY,
    inttype_name        varchar(30) NOT NULL UNIQUE,
    inttype_description varchar(100) NOT NULL UNIQUE,
    inttype_comment     varchar(50)
    );
  COMMENT ON TABLE  integration_type                     IS 'integration type';
  COMMENT ON COLUMN integration_type.inttype_id          IS 'ID';
  COMMENT ON COLUMN integration_type.inttype_name        IS 'name';
  COMMENT ON COLUMN integration_type.inttype_description IS 'detailed description';
  COMMENT ON COLUMN integration_type.inttype_comment     IS 'additional information';



CREATE TABLE integration (
    int_id                   smallserial PRIMARY KEY,
    inttype_id               smallint NOT NULL REFERENCES integration_type(inttype_id),
    int_measurement_interval double precision NOT NULL
      CHECK (int_measurement_interval > 0. AND int_measurement_interval <= 86400.),
    int_interval             double precision NOT NULL,
      CHECK (int_interval             > 0. AND int_interval             <= 86400.),
    int_comment              varchar(50),
    UNIQUE (inttype_id, int_measurement_interval, int_interval),
    CONSTRAINT integration_intervals_consistency_check CHECK (int_measurement_interval <= int_interval),
    CONSTRAINT integration_single_measurement_consistency_check
      CHECK ((inttype_id = 1 AND int_measurement_interval = int_interval) OR inttype_id > 1)
    );
  COMMENT ON TABLE  integration                          IS 'measurement integration';
  COMMENT ON COLUMN integration.int_id                   IS 'ID';
  COMMENT ON COLUMN integration.inttype_id               IS 'reference to integration type';
  COMMENT ON COLUMN integration.int_measurement_interval IS 'interval between measurements in s';
  COMMENT ON COLUMN integration.int_interval             IS 'integration interval in s of one stored measurement (see inttype_id)';
  COMMENT ON COLUMN integration.int_comment              IS 'additional information';


CREATE TABLE person (
    pers_id      smallserial PRIMARY KEY,
    pers_name    varchar(30) NOT NULL UNIQUE,
    pers_comment varchar(50)
    );
  COMMENT ON TABLE  person              IS 'person';
  COMMENT ON COLUMN person.pers_id      IS 'ID';
  COMMENT ON COLUMN person.pers_name    IS 'name';
  COMMENT ON COLUMN person.pers_comment IS 'additional information';


CREATE TABLE measurand (
    md_id             smallserial PRIMARY KEY,
    md_name           varchar(10) NOT NULL,
    md_setup_datetime timestamp with time zone NOT NULL CHECK (md_setup_datetime >= '1980-01-01' AND md_setup_datetime < NOW()),
    pq_id             smallint NOT NULL REFERENCES physical_quantity(pq_id),
    site_id           smallint NOT NULL REFERENCES site(site_id),
    caldev_id         smallint NOT NULL REFERENCES calibrated_device(caldev_id),
    int_id            smallint NOT NULL REFERENCES integration(int_id),
    md_height         double precision CHECK (md_height >= -10. AND md_height <= 10000.),
    md_orientation    double precision CHECK (md_orientation >= -180. AND md_orientation <= 180.),
    md_tilt           double precision CHECK (md_tilt >= 0. AND md_tilt <= 90.),
    pers_id           smallint REFERENCES person(pers_id),
    md_comment        varchar(50),
    UNIQUE (md_name, md_setup_datetime),
    UNIQUE (md_setup_datetime, pq_id, site_id, caldev_id, md_height, md_orientation, md_tilt, int_id)
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
  COMMENT ON COLUMN measurand.md_orientation    IS 'measurement north-south orientation, 0° north, positive clockwise, negative counterclockwise';
  COMMENT ON COLUMN measurand.md_tilt           IS 'measurement tilt, 0° upward, 90° downward';
  COMMENT ON COLUMN measurand.int_id            IS 'reference to integration details';
  COMMENT ON COLUMN measurand.pers_id           IS 'reference to contact person';
  COMMENT ON COLUMN measurand.md_comment        IS 'additional information';


CREATE TABLE quality_flag (
    qf_id          smallint PRIMARY KEY CHECK (qf_id > 0), -- no serial, because we want to choose values here
    qf_name        varchar(30) NOT NULL UNIQUE,
    qf_description varchar(100) NOT NULL UNIQUE,
    qf_comment     varchar(50) UNIQUE
    );
  COMMENT ON TABLE  quality_flag                IS 'quality flags';
  COMMENT ON COLUMN quality_flag.qf_id          IS 'ID, 1<=qf_id<=9: value ok, qf_id>=10: value not ok, NULL: not analysed';
  COMMENT ON COLUMN quality_flag.qf_name        IS 'date and time (with time zone) of measurement';
  COMMENT ON COLUMN quality_flag.qf_description IS 'description of quality flag';
  COMMENT ON COLUMN quality_flag.qf_comment     IS 'additional information';



CREATE TABLE station_adlershof (
    stadl_id       serial PRIMARY KEY, -- conversion to bigserial might be required later
    stadl_datetime timestamp with time zone NOT NULL
      CHECK (stadl_datetime >= '2000-01-01' AND stadl_datetime < NOW()),
    md_id          smallint NOT NULL REFERENCES measurand(md_id),
    stadl_value    double precision,
    qf_id          smallint REFERENCES quality_flag(qf_id),
    UNIQUE (stadl_datetime, md_id)
    );
  COMMENT ON TABLE  station_adlershof                IS 'unchecked measurements at Adlershof sites';
  COMMENT ON COLUMN station_adlershof.stadl_id       IS 'ID';
  COMMENT ON COLUMN station_adlershof.stadl_datetime IS 'date and time (with time zone) of measurement';
  COMMENT ON COLUMN station_adlershof.md_id          IS 'reference to measurand that was measured';
  COMMENT ON COLUMN station_adlershof.stadl_value    IS 'actual value of measurement';
  COMMENT ON COLUMN station_adlershof.qf_id          IS 'references quality flag, 1<=qf<=9: value ok, qf>=10: value not ok, NULL: not analysed';

CREATE TABLE station_adlershof_correction (
    stadlcor_id       serial PRIMARY KEY, -- conversion to bigserial might be required later
    stadl_id          integer NOT NULL UNIQUE REFERENCES station_adlershof(stadl_id), -- conversion to bigint might be required later
    stadlcor_datetime timestamp with time zone NOT NULL
      CHECK (stadlcor_datetime >= '2000-01-01' AND stadlcor_datetime < NOW()),
    md_id             smallint NOT NULL REFERENCES measurand(md_id),
    stadlcor_value    double precision,
    UNIQUE (stadlcor_datetime, md_id)
    );
  COMMENT ON TABLE  station_adlershof_correction                   IS 'corrected measurements at Adlershof sites';
  COMMENT ON COLUMN station_adlershof_correction.stadlcor_id       IS 'ID';
  COMMENT ON COLUMN station_adlershof_correction.stadl_id          IS 'references the measurement to correct in station adlershof';
  COMMENT ON COLUMN station_adlershof_correction.stadlcor_datetime IS 'date and time (with time zone) of corrected measurement';
  COMMENT ON COLUMN station_adlershof_correction.md_id             IS 'reference to measurand that was measured';
  COMMENT ON COLUMN station_adlershof_correction.stadlcor_value    IS 'actual value of corrected measurement';



CREATE VIEW device_model_detail AS
  SELECT devmod_id, devmod_name, devtype_name, devman_name, devmod_comment FROM device_model
    LEFT OUTER JOIN device_type         USING (devtype_id)
    LEFT OUTER JOIN device_manufacturer USING (devman_id);
  COMMENT ON VIEW device_model_detail IS 'measurement device model with joined details';

CREATE VIEW device_detail AS
  SELECT dev_id, dev_name, devtype_name, devmod_name, devman_name, dev_comment FROM device
    LEFT OUTER JOIN device_model_detail USING (devmod_id);
  COMMENT ON VIEW device_detail IS 'measurement device with joined details';

CREATE VIEW calibrated_device_detail AS
  SELECT caldev_id, dev_name, devtype_name, devmod_name, devman_name, caldev_datetime, caldev_parameter, caldev_comment FROM calibrated_device
    LEFT OUTER JOIN device_detail USING (dev_id);
  COMMENT ON VIEW calibrated_device_detail IS 'calibrated measurement device with joined details';

CREATE VIEW integration_detail AS
  SELECT int_id, inttype_name, int_measurement_interval, int_interval, int_comment FROM integration
    LEFT OUTER JOIN integration_type USING (inttype_id);
  COMMENT ON VIEW integration_detail IS 'integration with joined details';

CREATE VIEW measurand_detail AS
  SELECT md_id, md_name, md_setup_datetime, pq_name, pq_unit, site_name, dev_name, devmod_name, caldev_datetime, md_height, md_orientation, md_tilt, inttype_name, int_measurement_interval, int_interval, pers_name, md_comment FROM measurand
    LEFT OUTER JOIN physical_quantity        USING (pq_id)
    LEFT OUTER JOIN site                     USING (site_id)
    LEFT OUTER JOIN calibrated_device_detail USING (caldev_id)
    LEFT OUTER JOIN integration_detail       USING (int_id)
    LEFT OUTER JOIN person                   USING (pers_id);
  COMMENT ON VIEW measurand_detail IS 'measurand with joined details';
