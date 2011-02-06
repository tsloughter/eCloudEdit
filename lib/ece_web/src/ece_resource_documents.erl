-module(ece_resource_documents).

-export([init/1,
         allowed_methods/2,
         resource_exists/2,
         content_types_provided/2,
         to_json/2,
         process_post/2]).

-include_lib("webmachine/include/webmachine.hrl").
-include_lib("kernel/include/file.hrl").

init([]) ->
    {ok, undefined}.

allowed_methods(ReqData, Ctx) ->
    {['HEAD', 'GET', 'POST'], ReqData, Ctx}.

resource_exists(ReqData, Ctx) ->
    {true, ReqData, Ctx}.

content_types_provided(ReqData, Ctx) ->
    {[{"application/json", to_json}], ReqData, Ctx}.

process_post(ReqData, Ctx) ->
    [{JsonDoc, _}] = mochiweb_util:parse_qs(wrq:req_body(ReqData)),
    {struct, Doc} = mochijson2:decode(JsonDoc),
    ece_db:create({Doc}),
    {true, ReqData, Ctx}.

to_json(ReqData, Ctx) ->
    case wrq:path_info(id, ReqData) of
        undefined ->
            All = ece_db:all(),
            {All, ReqData, Ctx};
        ID ->
            JsonDoc = ece_db:find(ID),
            {JsonDoc, ReqData, Ctx}
    end.

