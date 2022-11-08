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

2. Call "startSaleAllTickets" endpoint (Offchain) with StartSaleParams.
An example:    
        
```
        StartSaleParams{
           saPrice         = 5000000, -- 5 ADA
        }
 ```
        
Note: The ticket price will be paid to the wallet holding the NFT.

sPrice - The price of the ticket (in lovelace).

This will find all tickets for sale at the wallet address and lock them for sale with a script.

3. Call "buyTicketByName" endpoint (Offchain) with BuyTicketByNameParams.
An example:
```    
       BuyTicketByNameParams{
           btTicketName    = "Event2023_2",
           btSellerAddress = mockWalletPaymentPubKeyHash $ knownWallet 1
        }
``` 
btTicketName - The name of the ticket to buy. This will be in the format of [event name]_[ticket number]

btSellerAddress - The wallet address of the ticket seller.