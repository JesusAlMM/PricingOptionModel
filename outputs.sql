CREATE DATABASE IF NOT EXISTS black_scholes_app;
USE black_scholes_app;
drop database black_scholes_app;
CREATE TABLE IF NOT EXISTS Input (
    id INT AUTO_INCREMENT PRIMARY KEY,
    StockPrice DECIMAL(10, 2),
    StrikePrice DECIMAL(10, 2),
    InterestRate DECIMAL(10, 2),
    Volatility DECIMAL(10, 2),
    TimeToExpiry DECIMAL(10, 2),
    InvAmCall DECIMAL(10, 2),
    InvAmPut DECIMAL (10, 2),
    Timestamp DATETIME DEFAULT CURRENT_TIMESTAMP
);

CREATE TABLE Output (
    CalculationId INT NOT NULL,
    OutputId INT AUTO_INCREMENT PRIMARY KEY,
	IsCallPut TINYINT NOT NULL,  
    OptionPrice DECIMAL(10, 4) NOT NULL,          
    ShockStockPrice DECIMAL(10, 4) NOT NULL,      
    ShockVolatility DECIMAL(10, 4) NOT NULL,      
    PnL DECIMAL(10,4) NOT NULL,
    FOREIGN KEY (CalculationId) REFERENCES Input(id)
);