-module(ece_resource_documents).

-export([init/1,
         allowed_methods/2,
         resource_exists/2,
         content_types_provided/2,
         content_types_accepted/2,
         from_json/2,
         to_json/2,
         process_post/2]).

-include_lib("webmachine/include/webmachine.hrl").
-include_lib("kernel/include/file.hrl").

init([]) ->
    {ok, undefined}.

allowed_methods(ReqData, Ctx) ->
    {['HEAD', 'GET', 'POST', 'PUT'], ReqData, Ctx}.

resource_exists(ReqData, Ctx) ->
    {true, ReqData, Ctx}.

content_types_accepted(ReqData, Ctx) ->
    {[{"application/json", from_json}], ReqData, Ctx}.

content_types_provided(ReqData, Ctx) ->
    {[{"application/json", to_json}], ReqData, Ctx}.

process_post(ReqData, Ctx) ->
    [{JsonDoc, _}] = mochiweb_util:parse_qs(wrq:req_body(ReqData)),
    {struct, Doc} = mochijson2:decode(JsonDoc),
    NewDoc = ece_db:create({Doc}),
    ReqData2 = wrq:set_resp_body(NewDoc, ReqData),
    {true, ReqData2, Ctx}.

to_json(ReqData, Ctx) ->
    case wrq:path_info(id, ReqData) of
        undefined ->
            All = ece_db:all(),
            {All, ReqData, Ctx};
        ID ->
            JsonDoc = ece_db:find(ID),
            {JsonDoc, ReqData, Ctx}
    end.

from_json(ReqData, Ctx) ->
    case wrq:path_info(id, ReqData) of
        undefined ->
            {false, ReqData, Ctx};
        ID ->
            JsonDoc = wrq:req_body(ReqData),
            {struct, Doc} = mochijson2:decode(JsonDoc),
            NewDoc = ece_db:update(ID, Doc),
            ReqData2 = wrq:set_resp_body(NewDoc, ReqData),
            {true, ReqData2, Ctx}
    end.
