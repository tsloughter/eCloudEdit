-module(ece_resource_documents).

-export([init/1,
         allowed_methods/2,
         content_types_provided/2,
         content_types_accepted/2,
         finish_request/2,
         from_json/2,
         to_json/2,
         process_post/2]).

-include_lib("webmachine/include/webmachine.hrl").

-record(ctx, {db}).

init([]) ->
    {ok, PID} = ece_db_sup:start_child(),
    {ok, #ctx{db=PID}}.

allowed_methods(ReqData, Ctx) ->
    {['HEAD', 'GET', 'POST', 'PUT'], ReqData, Ctx}.

content_types_accepted(ReqData, Ctx) ->
    {[{"application/json", from_json}], ReqData, Ctx}.

content_types_provided(ReqData, Ctx) ->
    {[{"application/json", to_json}], ReqData, Ctx}.

finish_request(ReqData, Ctx) ->
    ece_db_sup:terminate_child(Ctx#ctx.db),
    {true, ReqData, Ctx}.

process_post(ReqData, Ctx) ->
    [{JsonDoc, _}] = mochiweb_util:parse_qs(wrq:req_body(ReqData)),
    {struct, Doc} = mochijson2:decode(JsonDoc),
    NewDoc = ece_db:create(Ctx#ctx.db, {Doc}),
    ReqData2 = wrq:set_resp_body(NewDoc, ReqData),
    {true, ReqData2, Ctx}.

to_json(ReqData, Ctx) ->
    case wrq:path_info(id, ReqData) of
        undefined ->
            All = ece_db:all(Ctx#ctx.db),
            {All, ReqData, Ctx};
        ID ->
            JsonDoc = ece_db:find(Ctx#ctx.db, ID),
            {JsonDoc, ReqData, Ctx}
    end.

from_json(ReqData, Ctx) ->
    case wrq:path_info(id, ReqData) of
        undefined ->
            {false, ReqData, Ctx};
        ID ->
            JsonDoc = wrq:req_body(ReqData),
            {struct, Doc} = mochijson2:decode(JsonDoc),
            NewDoc = ece_db:update(Ctx#ctx.db, ID, Doc),
            ReqData2 = wrq:set_resp_body(NewDoc, ReqData),
            {true, ReqData2, Ctx}
    end.
