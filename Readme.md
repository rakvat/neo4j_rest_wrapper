# neo4j rest wrapper

This is work in progress. 

neo4j_rest_wrapper is an erlang library that wraps parts of the neo4j rest api. This library can be used in erlang applications to interact with the neo4j graph database.

## Example Usage

*todo*

## Build Dependencies
* rebar
* make
* kerl (optional)

The rebar binary should be copied to the root folder of this repository. 

To activate a specific erlang version I use [kerl](https://github.com/spawngrid/kerl) and source the `.env` file which contains the path to the used erlang version in the `~/.kerl` folder.

## Build and Test

To build the neo4j_rest_wrapper library just run `make`. 

`make test` to run the tests.

See `Makefile` for other targets.


