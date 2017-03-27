# receive-order

Create a HTTP endpoint to create multiple receive orders.

## Requirements

* [Haskell stack tool](https://docs.haskellstack.org/en/stable/README/) (for development only)

## Development Setup

After cloning the repo execute in the project root folder:

    stack setup && stack build

## Running the server

You just need to give the command a PostgreSQL connection string with the database to store the orders:

    stack exec receive-order-api -- postgres://localhost/receive_order

The above command will listen on `8080` port by default, you can change this using the `-p` flag.
For a complete help on the command try:

    stack exec receive-order-api -- -h
