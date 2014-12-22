CREATE TABLE Cameras (
    /* added cameraID to simplify primary key */
    cameraID                INTEGER NOT NULL,
    manufacturer            VARCHAR(64) NOT NULL,
    model                   VARCHAR(32) NOT NULL,
    releaseDate             DATE NOT NULL,
    sensorSize              INTEGER NOT NULL,
    retailPrice             INTEGER NOT NULL,
    stockNumber             INTEGER NOT NULL,
    hasElecViewfinder       BOOLEAN NOT NULL,
    opticalOrlensOrRange    ENUM('OPTICAL', 'LENS', 'RANGEFINDER') NOT NULL,
    
    PRIMARY KEY (cameraID)
);

CREATE TABLE BasicCameras (
    cameraID                INTEGER NOT NULL,
    focalRange              INTEGER NOT NULL,
    apertureRange           INTEGER NOT NULL,
    
    PRIMARY KEY (cameraID),
    FOREIGN KEY (cameraID) REFERENCES Cameras (cameraID)
);


CREATE TABLE compatability (
    /* added compatabilityID to simplify primary key */
    compatabilityID         INTEGER NOT NULL,
    camera                  INTEGER NOT NULL,
    lens                    INTEGER NOT NULL,
    
    PRIMARY KEY (compatabilityID),
    FOREIGN KEY (camera) REFERENCES Cameras (cameraID),
    FOREIGN KEY (lens) REFERENCES Lenses (lensID)
);


CREATE TABLE Lenses (
    /* added lensID to simplify primary key */
    lensID                  INTEGER NOT NULL
    manufacturer            VARCHAR(64) NOT NULL,
    model                   VARCHAR(32) NOT NULL,
    releaseDate             DATE NOT NULL,
    focalRange              INTEGER NOT NULL,
    apertureRange           INTEGER NOT NULL
    retailPrice             INTEGER NOT NULL,
    stockNumber             INTEGER NOT NULL,
    
    PRIMARY KEY (lensID)
);

CREATE TABLE Customers (
    customerNumber          INTEGER NOT NULL,
    name                    VARCHAR(128) NOT NULL,
    email                   VARCHAR(128) NOT NULL,
    shippingAddress         VARCHAR(128) NOT NULL,
    isDomestic              BOOLEAN NOT NULL,
    
    PRIMARY KEY (customerNumber)
);

CREATE TABLE Orders (
    PONumber                INTEGER NOT NULL,
    isOutstanding           BOOLEAN NOT NULL,
    sellingPrice            INTEGER NOT NULL,
    /* constraint: order being one camera or lens is not satisfied */
    camera                  INTEGER,
    lens                    INTEGER,
    
    PRIMARY KEY (PONumber),
    FOREIGN KEY (camera) REFERENCES Cameras (cameraID),
    FOREIGN KEY (lens) REFERENCES Lenses (lensID)
);

CREATE TABLE Evaluations (
    reviewNumber            INTEGER NOT NULL,
    customer                INTEGER,
    lens                    INTEGER,
    /* constraint: review being one camera or lens is not satisfied */
    camera                  INTEGER NOT NULL,
    score                   INTEGER NOT NULL,
    comment                 VARCHAR(MAX) NOT NULL,
    
    PRIMARY KEY (reviewNumber),
    FOREIGN KEY (camera) REFERENCES Cameras (cameraID),
    FOREIGN KEY (lens) REFERENCES Lenses (lensID)
);