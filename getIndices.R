index.1 <- c('AAPL','DIS', 'TWX', 'EL', 'THG', 'GOOGL', 'CS', 
             'KO', 'MSFT', 'SBUX',  'DAL', 'UPS', 'SYY',
             'KR', 'UNH', 'AMZN','IBM', 'ACN', 'ED', 'CAKE', 'TMUS', 'TM',
             'AMGN', 'COF', 'GIS', 'FB', 'F', 'BAC', 'XOM', 'AZN',
             'C', 'CHK', 'IBN', 'GM', 'RIG', 'XRX', 'VRX', 'CVX', 'WFC', 'GLW')

Company <- c('Apple','Disney', 'Time warner', 'Estee lauder', 
             'Hanover insurance',  'Google', 'Credit Suisse', 
             'Coca cola', 'Microsoft', 'Starbucks', 'Delta Airlines',
             'UPS', 'Sysco', 'Kroger', 'Unitedhealth', 'Amazon', 'IBM', 'Accenture',
             'Coned', 'Cheesecake factory', 't mobile', 'Toyota',
             'Amgen', 'Capital one', 'General Mills', 'Facebook', 'Ford Motor Co', 'Bank of America', 
             'Exxon Mobile', 'Astrazeneca', 'Citigroup',
              'Chesapeake', 'Icici Bank', 'General Motors', 'Transocean', 'Xerox', 
             'Valient', 'Chevron', 'Wells Fargo', 'Corning')
Indices <- data.frame(index.1, Company)
write.csv(Indices, 'indices.csv')
