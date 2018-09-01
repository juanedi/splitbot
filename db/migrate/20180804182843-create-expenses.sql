CREATE TABLE expenses (
  id                SERIAL PRIMARY KEY,
  amount            INTEGER NOT NULL,
  payer_share       INTEGER NOT NULL,
  payer             VARCHAR(100) NOT NULL,
  peer              VARCHAR(100) NOT NULL
);
