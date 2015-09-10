i2c
===

**i2c** is an Erlang driver for accessing the timer devices on the Analog Devices Blackfin processor. This driver can be used by applications that need to do things like read data from I2C sensors, control IO devices, etc., on the Lumenosys Robotics [BMOD][1] board. This driver was written as an async driver to avoid possible VM hangs.

Dependencies
------------

To build you will need a working installation of Erlang 17 (or
later). <br/>
Please refer to [Erlang/OTP](http://www.erlang.org) for information on building and installing Erlang/OTP.

This application is built using [rebar](https://github.com/rebar/rebar). Refer to [building rebar](https://github.com/rebar/rebar/wiki/Building-rebar) for information on building and using rebar.

Downloading
-----------

```sh
$ git clone git://github.com/lumenosys/i2c.git
```
Building
--------

Compile:

```sh
$ cd i2c
$ make all
...
==> i2c (compile)
```

Usage example
-------------

```erlang
%% the IOX board's I2C address (set by the jumpers on the board)
IOXAddr = 16#21,
%% The config register address for bottom 8 bits
Conf_L = 16#6,

%% set the bottom 8 IOs to be outputs
i2c:smbus_write_byte_data(IOXAddr, Conf_L, 16#0),
    
%% write 0xa5 to the lower order 8 bits of register
ByteReg = 16#2,
i2c:smbus_write_byte_data(IOXAddr, ByteReg, 16#a5),
```

Copyright and License
---------------------

> %CopyrightBegin%
>
> Copyright Lumenosys Robotics 2014-2015. All Rights Reserved.
>
> Licensed under the Apache License, Version 2.0 (the "License");
> you may not use this file except in compliance with the License.
> You may obtain a copy of the License at
>
>     http://www.apache.org/licenses/LICENSE-2.0
>
> Unless required by applicable law or agreed to in writing, software
> distributed under the License is distributed on an "AS IS" BASIS,
> WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
> See the License for the specific language governing permissions and
> limitations under the License.
>
> %CopyrightEnd%


[1]: https://lumenosys.com/products