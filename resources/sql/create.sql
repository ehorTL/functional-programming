CREATE TABLE users
(
    id SERIAL PRIMARY KEY,
    first_name CHARACTER VARYING(30),
    last_name CHARACTER VARYING(30),
    passport_code CHARACTER VARYING(30),
    password_hash CHARACTER VARYING(100)
);

CREATE TABLE software
(
    id SERIAL PRIMARY KEY,
    name_ CHARACTER VARYING(60),
    terms_and_conditions CHARACTER VARYING (60),
    author CHARACTER VARYING(40)
);

CREATE TABLE soft_distribution
(
    id SERIAL PRIMARY KEY,
    software_id INTEGER,
    version_ CHARACTER VARYING(10),
    path_to_distribution VARCHAR(100),
    license_from VARCHAR(20),
    license_to VARCHAR(20),
    FOREIGN KEY (software_id) REFERENCES software (id) ON DELETE CASCADE
);

CREATE TABLE user_distribution_downloads
(
    id SERIAL PRIMARY KEY,
    user_id INTEGER,
    distribution_id INTEGER,
    downloaded_on_date DATE DEFAULT CURRENT_DATE,
    FOREIGN KEY (user_id) REFERENCES users (id) ON DELETE CASCADE,
    FOREIGN KEY (distribution_id) REFERENCES soft_distribution(id) ON DELETE CASCADE
);

CREATE TABLE stat
(
    id SERIAL PRIMARY KEY,
    distribution_id INTEGER,
    downloaded_by_users_times INTEGER,
    FOREIGN KEY (distribution_id) REFERENCES soft_distribution (id) ON DELETE CASCADE
);