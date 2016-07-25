index.1 <- c('AAPL','DIS', 'TWX', 'EL', 'THG', 'ORCL', 'GOOGL', 'CS', 'DPS', 
             'KO', 'MSFT', 'SBUX', 'TGT', 'TSLA', 'DAL', 'CSCO', 'UPS', 'SYY',
             'KR', 'UNH', 'AMZN','IBM', 'ACN', 'ED', 'WDFC','CAKE', 'TMUS', 'TM',
             'LMT', 'AMGN', 'COF', 'GIS', 'FB', 'T')

Company <- c('Apple','Disney', 'Time warner', 'Estee lauder', 
             'Hanover insurance', 'Oracle', 'Google', 'Credit Suisse', 'Snapple', 
             'Coca cola', 'Microsoft', 'Starbucks', 'Target', 'Tesla', 'Delta Airlines',
             'Cisco', 'UPS', 'Sysco', 'Kroger', 'Unitedhealth', 'Amazon', 'IBM', 'Accenture',
             'Coned', 'WD-40', 'Cheesecake factory', 't mobile', 'Toyota', 'Lockheed martin',
             'Amgen', 'Capital one', 'General Mills', 'Facebook', 'AT&T')
Indices <- data.frame(index.1, Company)
write.csv(Indices, 'indices.csv')

