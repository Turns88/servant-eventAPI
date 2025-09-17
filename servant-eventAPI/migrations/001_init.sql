BEGIN TRANSACTION;

-- Users table
CREATE TABLE users (
    userId TEXT PRIMARY KEY,
    email TEXT NOT NULL UNIQUE,
    pwHash TEXT NOT NULL
);

-- Posts table
CREATE TABLE events (
    eventId TEXT PRIMARY KEY,
    creatorId TEXT NOT NULL,
    title TEXT NOT NULL,
    description TEXT NOT NULL,
    status TEXT NOT NULL,
    location TEXT NOT NULL,
    ticketsRemaining INT NOT NULL,
    eventTime TEXT NOT NULL,
    imagePath TEXT,
    created_at TEXT DEFAULT (datetime('now')),
    FOREIGN KEY(creatorId) REFERENCES users(userId)
);


-- Bookings table
CREATE TABLE bookings (
    bookingId TEXT PRIMARY KEY,
    userId TEXT NOT NULL,
    eventId TEXT NOT NULL,
    created_at TEXT DEFAULT (datetime('now')),
    FOREIGN KEY(userId) REFERENCES users(userId),
    FOREIGN KEY(eventId) REFERENCES events(eventId)
);


-- Comments table
CREATE TABLE comments (
    commentId TEXT PRIMARY KEY,
    userId TEXT NOT NULL,
    eventId TEXT NOT NULL,
    message TEXT NOT NULL,
    created_at TEXT DEFAULT (datetime('now')),
    FOREIGN KEY(userId) REFERENCES users(userId),
    FOREIGN KEY(eventId) REFERENCES events(eventId)
);


-- Migration tracking table
CREATE TABLE schema_migrations (
    version TEXT PRIMARY KEY,
    applied_at TEXT DEFAULT (datetime('now'))
);

-- Record that this migration was applied
INSERT INTO schema_migrations (version) VALUES ('001_init');

COMMIT;

