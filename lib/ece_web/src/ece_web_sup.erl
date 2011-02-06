%%%----------------------------------------------------------------
%%% @author  Tristan Sloughter <tristan.sloughter@gmail.com>
%%% @doc
%%% @end
%%% @copyright 2011 Tristan Sloughter
%%%----------------------------------------------------------------
-module(ece_web_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API functions
%%%===================================================================

-spec start_link() -> {ok, pid()} | any().
start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================


%% @private
-spec init(list()) -> {ok, {SupFlags::any(), [ChildSpec::any()]}} |
                          ignore | {error, Reason::any()}.
init([]) ->
    WebChild = {webmachine_mochiweb,
                {webmachine_mochiweb, start, [config()]},
                permanent, 5000, worker, dynamic},

    RestartStrategy = one_for_one,
    MaxRestarts = 3,
    MaxSecondsBetweenRestarts = 10,
    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

    {ok, {SupFlags , [WebChild]}}.


config() ->
    {ok, IP} = application:get_env(webmachine_ip),
    {ok, Port} = application:get_env(webmachine_port),
    {ok, App}= application:get_application(),
    LogDir = code:priv_dir(App) ++ "/logs",
    {ok, Dispatch} = file:consult(filename:join([code:priv_dir(App), "dispatch"])),

    [{ip, IP},
     {port, Port},
     {log_dir, LogDir},
     {backlog, 128},
     {dispatch, Dispatch}].

%%%===================================================================
%%% Internal functions
%%%===================================================================


