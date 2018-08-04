CREATE TABLE expenses (
  id                 SERIAL PRIMARY KEY,
  payer              VARCHAR(100) NOT NULL,
  budy               VARCHAR(100) NOT NULL,
  payer_share        VARCHAR(100) NOT NULL,
  budy_share         VARCHAR(100) NOT NULL,
  amount             INTEGER NOT NULL
);
