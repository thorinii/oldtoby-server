-- SQL installation script for OldToby API server --

CREATE TABLE job (
  id TEXT PRIMARY KEY,
  name TEXT NOT NULL,
  pipeline TEXT NOT NULL
);

CREATE TABLE page (
  id TEXT PRIMARY KEY,
  job TEXT REFERENCES job(id)
);
