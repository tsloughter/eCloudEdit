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
    gen_server:call(?SERVER, all).

find(ID) ->
    gen_server:call(?SERVER, {find, ID}).

create(Doc) ->
    gen_server:call(?SERVER, {create, Doc}).

update(ID, JsonDoc) ->
    gen_server:call(?SERVER, {update, ID, JsonDoc}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%% @private
init([Server, Port, DB]) ->
    CouchServer = couchbeam:server_connection(Server, Port, "", []),
    {ok, CouchDB} = couchbeam:open_db(CouchServer, DB),

    {ok, #state{db=CouchDB}}.

%% @private
handle_call(all, _From, #state{db=DB}=State) ->
    {ok, AllDocs} = couchbeam:view(DB, {"all", "find"}, []),
    {ok, Results} = couchbeam_view:fetch(AllDocs),
    {[{<<"total_rows">>, _Total},
      {<<"offset">>, _Offset},
      {<<"rows">>, Rows}]} = Results,

    Docs = lists:map(fun({Row}) ->
                             {<<"value">>, {Value}} = lists:keyfind(<<"value">>, 1, Row),
                             Value
                     end, Rows),

    {reply, mochijson2:encode(Docs), State};
handle_call({find, ID}, _From, #state{db=DB}=State) ->
    {ok, View} = couchbeam:view(DB, {"all", "find"}, [{key, list_to_binary(ID)}]),
    {ok, Results} = couchbeam_view:fetch(View),

    {[{<<"total_rows">>, _Total},
      {<<"offset">>, _Offset},
      {<<"rows">>, [{Row}]}]} = Results,

    {<<"value">>, {Doc}} = lists:keyfind(<<"value">>, 1, Row),

    {reply, mochijson2:encode(Doc), State};
handle_call({create, Doc}, _From, #state{db=DB}=State) ->
    {ok, Doc1} = couchbeam:save_doc(DB, Doc),
    {NewDoc} = couchbeam_doc:set_value(<<"id">>, couchbeam_doc:get_id(Doc1), Doc1),

    {reply, mochijson2:encode(NewDoc), State};
handle_call({update, ID, NewDoc}, _From, #state{db=DB}=State) ->
    IDBinary = list_to_binary(ID),
    {ok, Doc} = couchbeam:open_doc(DB, IDBinary),
    NewDoc2 = couchbeam_doc:set_value(<<"_id">>, IDBinary, {NewDoc}),
    NewDoc3 = couchbeam_doc:set_value(<<"_rev">>, couchbeam_doc:get_rev(Doc), NewDoc2),
    {ok, {Doc1}} = couchbeam:save_doc(DB, NewDoc3),
    {reply, mochijson2:encode(Doc1), State}.

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
