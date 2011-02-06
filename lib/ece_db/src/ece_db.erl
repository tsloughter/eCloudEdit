%%%----------------------------------------------------------------
%%% @author  Tristan Sloughter <tristan.sloughter@gmail.com>
%%% @doc
%%% @end
%%% @copyright 2011 Tristan Sloughter
%%%----------------------------------------------------------------
-module(ece_db).

-export([all/0,
         find/1,
         create/1,
         update/2]).
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
