%%%-------------------------------------------------------------------
%%% @author Devin Butterfield <dbutter@db>
%%% @copyright (C) 2014, Devin Butterfield
%%% @doc
%%%
%%% @end
%%% Created : 11 Aug 2014 by Devin Butterfield <dbutter@db>
%%%-------------------------------------------------------------------
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% 
%% Commentary:
%%
%% This Erlang port driver provides access to I2c devices connected to
%% the i2c bus on the Lumenosys Obsidian Blackfin board. Some code
%% and ideas borrowed from Tony Rogvall's I2C driver:
%% https://github.com/tonyrog/i2c
%% 
%% This driver was written using driver_async to ensure the VM would
%% not hang due to any delay in talking to I2C devices (which could be
%% caused by noise, contention, device failures, etc.). Also, the best
%% way to really understand something is to do it yourself!
%% 
%% Example:
%%
%% Note:
%% Nice way to format data:
%% io:format("~8.16.0B~n",[Id]).
%%
%% L3G4200D example configuration
%%
%% Write CTRL_REG2:
%%   i2c:smbus_write_byte_data(16#68, 16#21, 16#0). % default filter settings
%% Write CTRL_REG3
%%   i2c:smbus_write_byte_data(16#68, 16#22, 16#0). % no interrupts enabled
%% Write CTRL_REG4
%%   i2c:smbus_write_byte_data(16#68, 16#23, 16#80). % enable block data update (BDU)
%% Write Reference
%%   i2c:smbus_write_byte_data(16#68, 16#25, 16#0). % just zero, no ref for interrupt generation
%% Write CTRL_REG5
%%   i2c:smbus_write_byte_data(16#68, 16#24, 16#0). % no fifo, interrupt or filtering paths needed
%% Write CTRL_REG1
%%   i2c:smbus_write_byte_data(16#68, 16#20, 16#6f). % 200Hz rate, 50Hz cutoff, enable device, all axis
%%
%%
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% 
%% Change Log:
%%
%% FIXME: limit topic length here to prevent from overrunning port's
%% buffers. Also fix this in pwm_cap port.
%% 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-module(i2c).

-export([start_link/0, init/0]).

-define(SERVER, ?MODULE).

-export([open/0, close/0]).

-export([smbus/5,smbus_read/3,smbus_write/4]).
-export([smbus_read_byte/1,
	 smbus_read_byte_data/2,
	 smbus_read_word_data/2,
	 smbus_read_block_data/2,
	 %% smbus_write_quick/2,
	 smbus_write_byte/2,
	 smbus_write_byte_data/3,
	 smbus_write_word_data/3,
	 smbus_write_block_data/3]).
	 %% smbus_process_call/3,
	 %% smbus_read_i2c_block_data/3,
	 %% smbus_write_i2c_block_data/3,
	 %% smbus_block_process_call/3]).

-define(CMD_OPEN, 1).
-define(CMD_CLOSE, 2).
%% -define(CMD_SET_RETRIES, 3).
%% -define(CMD_SET_TIMEOUT, 4).
%% -define(CMD_SET_SLAVE, 5).
%% -define(CMD_SET_SLAVEF, 6).
%% -define(CMD_SET_TENBIT, 7).
%% -define(CMD_SET_PEC, 8).
%% -define(CMD_GET_FUNCS, 9).
%% -define(CMD_RDWR, 10).
-define(CMD_SMBUS, 11).
%% -define(CMD_DEBUG, 12).

-define(I2C_SMBUS_READ,	 1).
-define(I2C_SMBUS_WRITE, 0).
-define(I2C_SMBUS_QUICK, 0).
-define(I2C_SMBUS_BYTE,	 1).
-define(I2C_SMBUS_BYTE_DATA, 2).
-define(I2C_SMBUS_WORD_DATA, 3).
-define(I2C_SMBUS_PROC_CALL, 4).
-define(I2C_SMBUS_BLOCK_DATA, 5).
-define(I2C_SMBUS_I2C_BLOCK_BROKEN, 6).
-define(I2C_SMBUS_BLOCK_PROC_CALL, 7).	%% SMBus 2.0
-define(I2C_SMBUS_I2C_BLOCK_DATA, 8).

%%%===================================================================
%%% API
%%%===================================================================

open() ->
    call_port(?CMD_OPEN, <<>>).

close() ->
    call_port(?CMD_CLOSE, <<>>).

smbus(Addr, ReadWrite, Command, Size, Data) ->
    call_port(?CMD_SMBUS, <<Addr:16/unsigned-little-integer, 
			    ReadWrite:8/unsigned-little-integer, 
			    Command:8/unsigned-little-integer, 
			    Size:32/unsigned-little-integer, 
			    Data/binary>>).

smbus_read(Addr, Command, Size) ->
    smbus(Addr, ?I2C_SMBUS_READ, Command, Size, <<>>).

smbus_write(Addr, Command, Size, Data) ->
    smbus(Addr, ?I2C_SMBUS_WRITE, Command, Size, Data).

smbus_read_byte(Addr) ->
    case smbus_read(Addr, 0, ?I2C_SMBUS_BYTE) of
	{ok, <<Value:8, _/binary>>} ->
	    {ok,Value};
	Error -> Error
    end.

smbus_read_byte_data(Addr, Command) ->
    case smbus_read(Addr, Command, ?I2C_SMBUS_BYTE_DATA) of
	{ok, <<Value:8, _/binary>>} ->
	    {ok,Value};
	Error -> Error
    end.

smbus_read_word_data(Addr, Command) ->
    case smbus_read(Addr, Command, ?I2C_SMBUS_WORD_DATA) of
	{ok, <<Value:16/native, _/binary>>} ->
	    {ok, Value};
	Error ->
	    Error
    end.

smbus_read_block_data(Addr, Command) ->
    case smbus_read(Addr, Command, ?I2C_SMBUS_BLOCK_DATA) of
	{ok, <<N, Return:N/binary, _/binary>>} ->
	    {ok, Return};
	Error -> Error
    end.

%% smbus_write_quick(Bus, Value)
%%   when is_integer(Value) ->
%%     smbus(Bus,Value,0,?I2C_SMBUS_QUICK,
%% 	  <<>>).

smbus_write_byte(Addr, Value) when is_integer(Value) ->
    smbus_write(Addr, Value, ?I2C_SMBUS_BYTE, <<>>).

smbus_write_byte_data(Addr, Command, Value) when is_integer(Command), is_integer(Value) ->
    smbus_write(Addr, Command, ?I2C_SMBUS_BYTE_DATA, <<Value>>).

smbus_write_word_data(Addr, Command, Value) when is_integer(Command), is_integer(Value) ->
    smbus_write(Addr, Command, ?I2C_SMBUS_WORD_DATA, <<Value:16/native>>).

smbus_write_block_data(Addr, Command, Data) when is_integer(Command), is_binary(Data) ->
    N = max(byte_size(Data), 32),
    smbus_write(Addr,Command, ?I2C_SMBUS_BLOCK_DATA, <<N,Data:N/binary>>).

%% smbus_process_call(Bus, Command, Value) ->
%%     case smbus(Bus,?I2C_SMBUS_WRITE,Command,
%% 	       ?I2C_SMBUS_PROC_CALL,<<Value:16/native>>) of
%% 	{ok,<<Return:16/native>>} ->
%% 	    {ok,Return};
%% 	Error -> Error
%%     end.
%% smbus_read_i2c_block_data(Bus, Command, Length)
%%   when is_integer(Command), is_integer(Length), Length >= 0 ->
%%     N = max(Length, 32),
%%     Data = <<N>>,
%%     Size = if N =:= 32 ->
%% 		   ?I2C_SMBUS_I2C_BLOCK_BROKEN;
%% 	      true ->
%% 		   ?I2C_SMBUS_I2C_BLOCK_DATA
%% 	   end,
%%     case smbus(Bus,?I2C_SMBUS_READ,Command,Size,Data) of
%% 	{ok,<<N,Return:N/binary,_/binary>>} ->
%% 	    {ok,Return};
%% 	Error -> Error
%%     end.
%% smbus_write_i2c_block_data(Bus, Command, Data)
%%   when is_integer(Command), is_binary(Data) ->
%%     N = max(byte_size(Data),32),
%%     smbus(Bus,?I2C_SMBUS_WRITE,Command,
%% 	  ?I2C_SMBUS_I2C_BLOCK_BROKEN,<<N,Data:N/binary>>).
%% smbus_block_process_call(Bus, Command, Values) ->
%%     N = max(byte_size(Values),32),
%%     Data = <<N,Values:N/binary>>,
%%     case smbus(Bus,?I2C_SMBUS_WRITE,Command,
%% 	       ?I2C_SMBUS_BLOCK_PROC_CALL,Data) of
%% 	{ok,<<M,Return:M/binary,_/binary>>} ->
%% 	    {ok,Return};
%% 	Error -> Error
%%     end.

%%====================================================================
%% Internal functions
%%====================================================================


%%--------------------------------------------------------------------
%% Internal functions
%%--------------------------------------------------------------------

%% blocking call port driver
call(Port, Cmd, Data) ->
    case erlang:port_control(Port, Cmd, Data) of
	<<0>> ->
	    ok;
	<<1>> ->
	    {ok, pending};
	Error ->
	    io:format("unexpected response ~p~n", [Error]),
	    error
    end.

wait_for_completion(Port) ->
    receive
	{Port, {data, RespData}} ->
	    case RespData of
		[0] -> 
		    ok;
		[255|<<E/binary>>] ->
		    {error, erlang:binary_to_atom(E, latin1)};
		[1|<<Y>>] -> {ok,Y};
		[2|<<Y:16/native-unsigned>>] -> {ok, Y};
		[4|<<Y:32/native-unsigned>>] -> {ok, Y};
		[8|<<Y:64/native-unsigned>>] -> {ok, Y};
		[3|<<Return/binary>>] -> {ok,Return}
	    end
	%% Error ->
	%%     io:format("DEBUGGING: unexpected response ~p~n", [Error]),
	%%     error
    end.

call_port(Cmd, Data) ->
    i2c_port_proc ! {call, self(), Cmd, Data},
    receive
        {i2c_port_proc, Result} ->
            Result;
	%% If the calling process has setup a monitor on the tunnel,
	%% this will tell them it exited so they don't hange waiting
	%% forever!
	{'DOWN', _Ref, process, _Pid, Reason} ->
	    {error_down, Reason}
    end.

start_link() ->
    Pid = spawn_link(?SERVER, init, []),
    {ok, Pid}.

load_driver() ->
%%    case erl_ddll:load_driver(code:priv_dir("i2c"), "i2c") of
    case erl_ddll:load_driver(".", "i2c") of
	ok -> ok; 
	{error, already_loaded} -> ok;
	Reason -> exit({error, could_not_load_driver, Reason})
    end.

init() ->
    register(i2c_port_proc, self()),
    ok = load_driver(),
    Port = erlang:open_port({spawn_driver, "i2c"},[binary]),
    %% open the i2c driver
    ok = call(Port, ?CMD_OPEN, <<>>),
    loop(Port).

%% The i2c_port_proc process loop
loop(Port) ->
    receive
        {call, Caller, Cmd, Data} ->
	    %% If the command send to the driver results in an
	    %% immediate response, we get ok, just return it. If the
	    %% command requires any work at all, we get {ok, pending}
	    %% and must wait for the command to complete.
	    case call(Port, Cmd, Data) of
		ok ->
		    Caller ! {i2c_port_proc, ok};
		{ok, pending} ->
		    PendingResult = wait_for_completion(Port),
		    Caller ! {i2c_port_proc, PendingResult}
	    end;
	%% {Port, _Data} ->
        %%     io:format("Warning: the port sent us something unxpected!");
        stop ->
	    ok = call(Port, ?CMD_CLOSE, <<>>),
	    exit(normal)
    end,
    loop(Port).
