%%%----------------------------------------------------------------
%%% @author  Tristan Sloughter <tristan.sloughter@gmail.com>
%%% @doc
%%% @end
%%% @copyright 2011 Tristan Sloughter
%%%----------------------------------------------------------------
-module(ece_db).

-behaviour(gen_server).

%% API
-export([start_link/3,
         all/0,
         find/1,
         create/1,
         update/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).
-export_type([]).

-define(SERVER, ?MODULE).

-record(state, {db}).

%%%===================================================================
%%% Public Types
%%%===================================================================

%%%===================================================================
%%% API
%%%===================================================================

start_link(Server, Port, DB) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [Server, Port, DB], []).

all() ->
    mochijson2:encode([{struct, [{id, <<"ID">>},
                                 {title, <<"Title">>},
                                 {created_at, <<"CreateAt">>},
                                 {body, <<"Body">>}]}]).

find(_ID) ->
    mochijson2:encode([{id, <<"ID">>},
                       {title, <<"Title">>},
                       {created_at, <<"CreateAt">>},
                       {body, <<"Body">>}]).

create(_JsonDoc) ->
    ok.

update(_ID, _JsonDoc) ->
    ok.

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%% @private
init([Server, Port, DB]) ->
    CouchServer = couchbeam:server_connection(Server, Port, "", []),
    {ok, CouchDB} = couchbeam:open_db(CouchServer, DB),

    {ok, #state{db=CouchDB}}.

%% @private
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%% @private
handle_cast(_Msg, State) ->
    {noreply, State}.

%% @private
handle_info(_Info, State) ->
    {noreply, State}.

%% @private
terminate(_Reason, _State) ->
    ok.

%% @private
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
