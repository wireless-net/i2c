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
%% Commentary: This Erlang port provides access devices connected to
%% the i2c bus on the Lumenosys Obsidian Blackfin board.
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
%%   i2c:write_byte(16#68, 16#21, 16#0). % default filter settings
%% Write CTRL_REG3
%%   i2c:write_byte(16#68, 16#22, 16#0). % no interrupts enabled
%% Write CTRL_REG4
%%   i2c:write_byte(16#68, 16#23, 16#80). % enable block data update (BDU)
%% Write Reference
%%   i2c:write_byte(16#68, 16#25, 16#0). % just zero, no ref for interrupt generation
%% Write CTRL_REG5
%%   i2c:write_byte(16#68, 16#24, 16#0). % no fifo, interrupt or filtering paths needed
%% Write CTRL_REG1
%%   i2c:write_byte(16#68, 16#20, 16#6f). % 200Hz rate, 50Hz cutoff, enable device, all axis
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

-export([write/4, write_byte/2, write_byte/3, write_word/3, read/3, read_byte/1, read_byte/2,
         read_word/2, publish/7, publish/5, publish/4, publish/8, stop_publisher/1,
         subscribe/5, subscribe/4, stop_subscriber/1]).

%%%===================================================================
%%% API
%%%===================================================================

%% stop() ->
%%     i2c_port_proc ! stop,
%%     unregister(i2c_port_proc).

%% read(Addr, Len) ->
%%     call_port({i2c_read, Addr, Len}).

read(Addr, Reg, Len) ->
    call_port({i2c_read_reg, Addr, Reg, Len}).

read_byte(Addr) ->
    call_port({i2c_read, Addr, 1}).

read_byte(Addr, Reg) ->
    call_port({i2c_read_reg, Addr, Reg, 1}).

%% read_word(Addr) ->
%%     call_port({i2c_read, Addr, 2}).

read_word(Addr, Reg) ->
    call_port({i2c_read_reg, Addr, Reg, 2}).

%% write(Addr, Len, Data) ->
%%     call_port({i2c_write, Addr, Len, Data}).

write(Addr, Reg, Len, Data) ->
    call_port({i2c_write_reg, Addr, Reg, Len, Data}).

write_byte(Addr, Data) ->
    call_port({i2c_write, Addr, 1, Data}).

write_byte(Addr, Reg, Data) ->
    call_port({i2c_write_reg, Addr, Reg, 1, Data}).

%% write_word(Addr, Data) ->
%%     call_port({i2c_write, Addr, 2, Data}).

write_word(Addr, Reg, Data) ->
    call_port({i2c_write_reg, Addr, Reg, 2, Data}).

publish(Addr, Size, Topic, Priority) ->
    case string:len(Topic) > 20 of
        true ->
            io:format("topic length must be less than 21 characters~n"),
            error;
        false ->
            call_port({i2c_publish, Addr, Size, Topic, Priority, encode_policy(q_fifo), 5}),
            ok
    end.

publish(Addr, Reg, Size, Topic, Priority) ->
    case string:len(Topic) > 20 of
        true ->
            io:format("topic length must be less than 21 characters~n"),
            error;
        false ->
            call_port({i2c_publish_reg, Addr, Reg, Size, Topic, Priority, encode_policy(q_fifo), 5}),
            ok
    end.

publish(Addr, Size, Topic, Priority, Period, QPolicy, MaxQMessages) ->
    case string:len(Topic) > 20 of
        true ->
            io:format("topic length must be less than 21 characters~n"),
            error;
        false ->
            call_port({i2c_publish, Addr, Size, Topic, Priority, Period, QPolicy, MaxQMessages}),
            ok
    end.

publish(Addr, Reg, Size, Topic, Priority, Period, QPolicy, MaxQMessages) ->
    case string:len(Topic) > 20 of
        true ->
            io:format("topic length must be less than 21 characters~n"),
            error;
        false ->
            call_port({i2c_publish_reg, Addr, Reg, Size, Topic, Priority, Period, QPolicy, MaxQMessages}),
            ok
    end.


%% publish(Addr, Reg, Size, Topic) ->
%%     call_port({i2c_publish, Addr, Reg, Size, Topic, 99, encode_policy(q_fifo), 5}).

stop_publisher(Topic) ->
    call_port({i2c_stop_publisher, Topic}).

subscribe(Addr, Size, Topic, Priority) ->
    case string:len(Topic) > 20 of
        true ->
            io:format("topic length must be less than 21 characters~n"),
            error;
        false ->
            call_port({i2c_subscribe, Addr, Size, Topic, Priority}),
            ok
    end.

subscribe(Addr, Reg, Size, Topic, Priority) ->
    case string:len(Topic) > 20 of
        true ->
            io:format("topic length must be less than 21 characters~n"),
            error;
        false ->
            call_port({i2c_subscribe_reg, Addr, Reg, Size, Topic, Priority}),
            ok
    end.


%% subscribe(Addr, Reg, Size, Topic) ->
%%     call_port({i2c_subscribe, Addr, Reg, Size, Topic, 99}).

stop_subscriber(Topic) ->
    call_port({i2c_stop_subscriber, Topic}).

%%====================================================================
%% Internal functions
%%====================================================================

call_port(Msg) ->
    i2c_port_proc ! {call, self(), Msg},
    receive
        { i2c_port_proc, Result } ->
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

init() ->
    register(i2c_port_proc, self()),
    process_flag(trap_exit, true),
    PortPrg = filename:join(code:priv_dir("i2c"), "i2c_port"),
    Port = open_port({spawn_executable, PortPrg}, 
		     [{packet, 2}, use_stdio, binary, exit_status]),
    loop(Port).

%% The i2c_port_proc process loop
loop(Port) ->
    receive
        {call, Caller, Msg} ->
%            io:format("got call, calling port...~n"),
            Port ! {self(), {command, encode(Msg)}},
%            io:format("waiting for result~n"),
            receive
                {Port, {data, Data}} ->
		    %%io:format("got data ~p~n", [Data]),
                    Caller ! {i2c_port_proc, decode(Data)}
            end,
            loop(Port);
        {Port, _Data} ->
            io:format("The port sent us something!"),
            loop(Port);
        stop ->
            Port ! {self(), close},
            receive
                {Port, closed} ->
                    exit(normal)
            end;
        {'EXIT', Port, Reason} ->
            io:format("Port terminated for reason: ~p~n", [Reason]),
            exit(port_terminated)
    end.

%% encode takes atoms and converts to corresponding op-code
encode({i2c_read, Addr, Size}) ->
    [<<1:16/unsigned-little-integer>>, 
     <<Addr:16/unsigned-little-integer>>, 
     <<Size:16/unsigned-little-integer>>];
encode({i2c_read_reg, Addr, Reg, Size}) ->
    [<<2:16/unsigned-little-integer>>, 
     <<Addr:16/unsigned-little-integer>>, 
     <<Reg:16/unsigned-little-integer>>, 
     <<Size:16/unsigned-little-integer>>];
encode({i2c_write, Addr, Size, Data}) ->
    [<<3:16/unsigned-little-integer>>, 
     <<Addr:16/unsigned-little-integer>>, 
     <<Size:16/unsigned-little-integer>>,
     <<Data:16/unsigned-little-integer>>];
encode({i2c_write_reg, Addr, Reg, Size, Data}) ->
    [<<4:16/unsigned-little-integer>>, 
     <<Addr:16/unsigned-little-integer>>, 
     <<Reg:16/unsigned-little-integer>>, 
     <<Size:16/unsigned-little-integer>>,
     <<Data:16/unsigned-little-integer>>];
encode({i2c_publish, Addr, Size, Topic, Priority, Period, QPolicy, MaxQMessages}) ->
    TopicLen = string:len(Topic),
    [<<5:16/unsigned-little-integer>>, 
     <<Addr:16/unsigned-little-integer>>, 
     <<Size:16/unsigned-little-integer>>,
     <<Priority:32/unsigned-little-integer>>,
     <<Period:32/unsigned-little-integer>>,
     <<QPolicy:32/unsigned-little-integer>>,
     <<MaxQMessages:32/unsigned-little-integer>>,
     <<TopicLen:32/unsigned-little-integer>>,
     Topic];
encode({i2c_publish_reg, Addr, Reg, Size, Topic, Priority, Period, QPolicy, MaxQMessages}) ->
    TopicLen = string:len(Topic),
    [<<6:16/unsigned-little-integer>>, 
     <<Addr:16/unsigned-little-integer>>, 
     <<Reg:16/unsigned-little-integer>>, 
     <<Size:16/unsigned-little-integer>>,
     <<Priority:32/unsigned-little-integer>>,
     <<Period:32/unsigned-little-integer>>,
     <<QPolicy:32/unsigned-little-integer>>,
     <<MaxQMessages:32/unsigned-little-integer>>,
     <<TopicLen:32/unsigned-little-integer>>,
     Topic];
encode({i2c_subscribe, Addr, Size, Topic, Priority}) ->
    TopicLen = string:len(Topic),
    [<<7:16/unsigned-little-integer>>, 
     <<Addr:16/unsigned-little-integer>>, 
     <<Size:16/unsigned-little-integer>>,
     <<Priority:32/unsigned-little-integer>>,
     <<TopicLen:32/unsigned-little-integer>>,
     Topic];
encode({i2c_subscribe_reg, Addr, Reg, Size, Topic, Priority}) ->
    TopicLen = string:len(Topic),
    [<<8:16/unsigned-little-integer>>, 
     <<Addr:16/unsigned-little-integer>>, 
     <<Reg:16/unsigned-little-integer>>, 
     <<Size:16/unsigned-little-integer>>,
     <<Priority:32/unsigned-little-integer>>,
     <<TopicLen:32/unsigned-little-integer>>,
     Topic];
encode({i2c_stop_publisher, Topic}) ->
    TopicLen = string:len(Topic),
    [<<9:16/unsigned-little-integer>>, 
     <<TopicLen:32/unsigned-little-integer>>,
     Topic];
encode({i2c_stop_subscriber, Topic}) ->
    TopicLen = string:len(Topic),
    [<<10:16/unsigned-little-integer>>, 
     <<TopicLen:32/unsigned-little-integer>>,
     Topic].


%% encode the policy to use for queuing published messages
encode_policy(q_fifo) ->
    0.                                          % 0 is Q_FIFO == XNSYNCH_FIFO

%% decode matches against response codes and handles accordingly Any
%% response not expected is an error and we crash (intentionally).
decode(<<0:16/unsigned-little-integer,_Arg/binary>>) ->                              
    ok;						% command OK
decode(<<1:16/unsigned-little-integer,Arg/binary>>) ->                              
    Arg.				% command OK, with data

%% decode([0|_Arg]) ->                              
%%     ok;                                        % command OK
%% decode([1,0|Arg]) ->                              
%%     Arg.
