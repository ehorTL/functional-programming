INSERT INTO users
    (first_name, last_name, passport_code, password_hash)
VALUES
    ('Dan', 'Smith', '1234567890', crypt('dan', 'dan')),
    ('Bob', 'Smith', '0123456789', crypt('bob', 'bob')),
    ('Alice', 'Johnson', '2345678901', crypt('alice', 'alice')),
    ('Bart', 'Simpson', '2345633901', crypt('bart', 'bart'));

INSERT INTO software
    (name_, terms_and_conditions, author)
VALUES
    ('Visual Code', 'terms & conditions 1', 'microsoft'),
    ('Sublime Text', 'terms & conditions 2', 'author of sublime text'),
    ('Adobe Acrobat', 'terms & conditions 3', 'Adobe'),
    ('Adobe Photoshop', 'terms & conditions 4', 'Adobe'),
    ('Adobe InDesign', 'terms & conditions 5', 'Adobe'),
    ('Intelij IDEA', 'terms & conditions 6', 'JetBrains'),
    ('PHPStorm', 'terms & conditions 7', 'JetBrains'),
    ('PyCharm', 'terms & conditions 8', 'JetBrains');


INSERT INTO soft_distribution
    (software_id, version_, path_to_distribution, license_from, license_to)
VALUES
    (1, '1.0', 'path/to/this/distr/path1.iso', '01.01.2010', '01.01.2030'),
    (2, '1.0', 'path/to/this/distr/path2.iso', '01.01.2010', '01.01.2030'),
    (3, '1.0', 'path/to/this/distr/path3.iso', '01.01.2010', '01.01.2030'),
    (4, '1.0', 'path/to/this/distr/path4.iso', '01.01.2010', '01.01.2030'),
    (5, '1.0', 'path/to/this/distr/path5.iso', '01.01.2010', '01.01.2030'),
    (6, '1.0', 'path/to/this/distr/path6.iso', '01.01.2010', '01.01.2030'),
    (7, '1.0', 'path/to/this/distr/path7.iso', '01.01.2010', '01.01.2030'),
    (8, '1.0', 'path/to/this/distr/path8.iso', '01.01.2010', '01.01.2030'),
    (1, '2.0', 'path/to/this/distr/path9.iso', '01.01.2010', '01.01.2030'),
    (2, '2.0', 'path/to/this/distr/path10.iso', '01.01.2010', '01.01.2030'),
    (3, '2.0', 'path/to/this/distr/path11.iso', '01.01.2010', '01.01.2030'),
    (4, '2.0', 'path/to/this/distr/path12.iso', '01.01.2010', '01.01.2030'),
    (5, '2.0', 'path/to/this/distr/path13.iso', '01.01.2010', '01.01.2030'),
    (6, '2.0', 'path/to/this/distr/path14.iso', '01.01.2010', '01.01.2030'),
    (7, '2.0', 'path/to/this/distr/path15.iso', '01.01.2010', '01.01.2030'),
    (8, '2.0', 'path/to/this/distr/path16.iso', '01.01.2010', '01.01.2030'),
    (1, '3.0', 'path/to/this/distr/path17.iso', '01.01.2010', '01.01.2030'),
    (2, '3.0', 'path/to/this/distr/path18.iso', '01.01.2010', '01.01.2030'),
    (3, '3.0', 'path/to/this/distr/path19.iso', '01.01.2010', '01.01.2030'),
    (4, '3.0', 'path/to/this/distr/path20.iso', '01.01.2010', '01.01.2030'),
    (5, '3.0', 'path/to/this/distr/path21.iso', '01.01.2010', '01.01.2030'),
    (6, '3.0', 'path/to/this/distr/path22.iso', '01.01.2010', '01.01.2030'),
    (7, '3.0', 'path/to/this/distr/path23.iso', '01.01.2010', '01.01.2030'),
    (8, '3.0', 'path/to/this/distr/path24.iso', '01.01.2010', '01.01.2030');

INSERT INTO user_distribution_downloads
    (user_id, distribution_id)
VALUES
    (1, 1),
    (2, 1),
    (3, 1),
    (4, 1),
    (1, 2),
    (2, 2),
    (3, 2),
    (4, 2),
    (1, 3),
    (2, 3),
    (3, 3),
    (4, 3),
    (1, 4),
    (2, 4),
    (3, 4),
    (4, 4),

    (1, 5),
    (1, 6),
    (1, 7),
    (1, 8),
    (1, 9),
    (1, 10),
    (1, 11),
    (1, 12),
    (1, 13),
    (1, 13),
    (1, 13),
    (1, 13),
    (2, 20),
    (2, 20),
    (2, 20),
    (2, 20),
    (2, 20),
    (2, 20),
    (2, 20),
    (2, 20),
    (2, 20),
    (2, 20);

INSERT INTO stat
    (distribution_id, downloaded_by_users_times)
VALUES
    (1, 4),
    (2, 4),
    (3, 4),
    (4, 4),
    (5, 1),
    (6, 1),
    (7, 1),
    (8, 1),
    (9, 1),
    (10, 1),
    (11, 1),
    (12, 1),
    (13, 4),
    (20, 10);
