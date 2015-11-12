-module(h5tv_channel_manager).

-behaviour(gen_server).

-export([start_link/0]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).


init([]) ->
    ets:new(
      h5tv_channels,
      [named_table, set, protected, {read_concurrency, true}]),
    {ok, 1}.


handle_call({create_channel, Name, Timestamp}, _From, NextId) ->
    ets:insert_new(h5tv_channels, {NextId, Name, Timestamp}),
    {reply, NextId, NextId + 1}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
