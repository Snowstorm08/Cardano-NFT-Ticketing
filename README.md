# Cardano NFT Ticketing

A Plutus project demonstrating a ticking system using NFT's on Cardano.
Includes tracing and testing.

1. Call "createEvent" endpoint (Offchain) with EventParams.
    An example: 
    
```
        EventParams{ 
          epEventName           = "NYE2023",
          epEventTime           = 1666531889000,
          epNumberOfTickets     = 5
        }
```
    
epEventName - The name of the event, will be used in the Token Name.

epEventTime - [POSIX time](https://www.epochconverter.com/) of the event. This will be used as a minting deadline.

epNumberOfTickets - The number of tickets to mint.

This will mint the tickets using the Event Name and Ticket Number as the Token Name.

2. Call "startSale" endpoint (Offchain) with StartParams.
An example:    
        
```
        StartParams{
           sPrice         = 5000000, -- 5 ADA
           sToken         = "NYE2023_1",
           sCurrSym       = "a8038825de6ee33c0f4970c8ddec533366e0b7a436a1df8b59a6808e"
        }
 ```
        
Note: The ticket price will be paid to the wallet holding the NFT.

sPrice - The price of the ticket (in lovelace)

sCurrSym - The currency symbol of the NFT.

sToken - The token name of the NFT.

3. Call "buyTicket" endpoint (Offchain) with BuyParams.
An example:
```    
        BuyParams{
           bToken    = "NYE2023_1",
           bCurrSym  = "a8038825de6ee33c0f4970c8ddec533366e0b7a436a1df8b59a6808e",
        }
``` 
bToken - The name of the NFT to buy.

bCurrSym - The currency symbol of the NFT.
