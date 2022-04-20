# jq

Erlang library for [jq](https://github.com/stedolan/jq) with a NIF-based
implementation and a port-based implementationp.

## Compiling
  ```
  $ rebar3 compile
  ```

## Testing

The [valgrind](https://valgrind.org/) memory analyzer is needed for one test
case.

  ```
  $ rebar3 eunit
  ```
  
## Usage

  ```
  $ rebar3 shell
  ...
  1> jq:process_json(<<".a">>, <<"{\"b\": 1}">>).
  {ok,[<<"null">>]}
  
  2> jq:process_json(<<".a">>, <<"{\"a\": {\"b\": {\"c\": 1}}}">>).
  {ok,[<<"{\"b\":{\"c\":1}}">>]}
  
  3> jq:process_json(<<".a|.[]">>, <<"{\"a\": [1,2,3]}">>).
  {ok,[<<"1">>,<<"2">>,<<"3">>]}
  ```

See the [jq documentation](https://stedolan.github.io/jq/manual/v1.6) for more information about how to write jq filter
programs.

## Configuration

The configurations can be changed by passing `-jq OptionName Value` as an `erl`
command line option or programmatically by using one of the
`application:set_env` functions before the jq library has been loaded.


* `jq_filter_program_lru_cache_max_size` (default value = 500) - Set the size of
  the LRU caches that are holding compiled JQ programs to prevent frequent
  expensive recompilation of the same program.
* `jq_implementation_module` (default value = `jq_port`) - Set the implementation
  that will be used. The options are:
  * `jq_port` - This implemementation uses a port program to interact with jq.
    This is the most safe option as a bug in jq cannot
    cause the Erlang VM to crach or leak memory.
  * `jq_nif` - This implementation uses a NIF library to interact with 
    jq. This option is faster than the `jq_port` option but it is also
    less safe even though we are not not aware of any problems with this option.
* `jq_port_nr_of_jq_port_servers` (default value =
  `erlang:system_info(schedulers)`) (only relevant for the `jq_port` option) -
  Use this option to set how many port programs that will handle jq requests.
  Higher values can lead to better performance (due to parallelizim) at the
  expense of increased memory usage and cache locality. 

## Test with address sanitizer
  
  There are scripts that can help when one wants to run the eunit tests with
  address sanitizer. This is only relevant for the NIF-based implementation.
  The port-based implementation is tested with `valgrind` in the eunit suit
  automatically. Address sanitizer is included in recent versions of gcc and
  clang.
  
  Use the following commands to run the eunit tests with
  address sanitizer:
  ```
  ./test/address_sanitizer_setup.sh 
  ./test/address_sanitizer_run_eunit.sh
  ```
  The "test/address_sanitizer_setup.sh" scripts compiles an
  erlang VM with address sanitizer support and don't need
  to be executed every time a change is made to the NIF.

## About hot upgrading

This library supports hot code reloading/upgrading. For the hot upgrade from an
old version to a new version of the NIF-based implementation to go smoothly,
the new version needs to have a different number in the version macro
(`-define(VERSION, NUM)`) in `src/jq_nif.erl`. This version number is read by
the `Makefile` for the shared library. The `Makefile` produces a shared library
with the version number in its file name. [Most operating systems require that
the new shared library has a different name than the previous
one](https://www.erlang.org/doc/man/erlang.html#load_nif-2) (otherwise, the
operating system will not load the new library). One should thus always
increase the version number in the version macro before releasing a new version
of this library.

