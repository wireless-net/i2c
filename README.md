
#The Lumenosys Obsidian I2C port application#

### Interface description ###

### Dependencies ###

To build you will need a working installation of Erlang 17 (or
later). <br/>
Please refer to [Erlang/OTP](http://www.erlang.org) for information on building and installing Erlang/OTP.

The I2c port application is built using [rebar](https://github.com/rebar/rebar). Refer to [building rebar](https://github.com/rebar/rebar/wiki/Building-rebar) for information on building and using rebar.

### Downloading

```sh
$ git clone git://github.com/dbutter/i2c.git
```
### Configuration
#### Concepts
...
#### Files
...
### Building

Compile:

```sh
$ cd i2c
$ rebar compile
...
==> i2c (compile)
```

### Usage example
...
```sh
$ export ERL_LIBS=/path/to/i2c
$ erl -sname obsidian@ion1 -boot start_sasl -eval "application:start(i2c)"
...
```


