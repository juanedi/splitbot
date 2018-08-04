CREATE TABLE expenses (
  id                 SERIAL PRIMARY KEY,
  payer              VARCHAR(100) NOT NULL,
  buddy              VARCHAR(100) NOT NULL,
  payer_share        VARCHAR(100) NOT NULL,
  buddy_share        VARCHAR(100) NOT NULL,
  amount             INTEGER NOT NULL
);
