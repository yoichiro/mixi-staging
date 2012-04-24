# mixi-staging

This project is a software for reserving and managing many staging servers in mixi.

## How to install

 * Install Erlang OTP (R14 or higher).
 * Retrieve this project.
 * make
 * ./start-dev.sh
 * mnesia:stop().
 * mnesia:create_schema([node()]).
 * mnesia:start().
 * maintenance:create_tables().
 * maintenance:create_servers().
 * Access to http://[server-name]:8080/ on your browser.

## How to quit from the node.

 * Type "q().".

## How to execute tests

 * make test

## How to generate documents

 * make edoc

## How to clean up

 * make clean
