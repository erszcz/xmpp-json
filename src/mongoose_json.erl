-module(mongoose_json).
-include_lib("exml/include/exml.hrl").

%% Encoder/decoder of xmlel()s to/from JSON
-export([xmlel_to_json/1,
         json_to_xmlel/1]).

%% Test-only exports.
-export([xmlel_to_mochistruct/1,
         mochistruct_to_xmlel/1]).

-type xmlel() :: #xmlel{}.

%%
%% Encoder/decoder of xmlel()s to/from JSON
%%

%% El == json_to_xmlel(xmlel_to_json(El)) must hold.
%%
%% We start with a piece of XML:
%%
%% <iq from='localhost'
%%     to='alice@localhost/escalus-default-resource'
%%     type='result'>
%%   <query xmlns='cdk:xmpp:frequent-contacts'/>
%% </iq>
%%
%% Let's get an `xmlel()` by parsing the XML:
%%
%% {ok, El} = exml:parse(<<"<iq from='localhost'"
%%                         "    to='alice@localhost/escalus-default-resource'"
%%                         "    type='result'>"
%%                         "  <query xmlns='cdk:xmpp:frequent-contacts'/>"
%%                         "</iq>">>).
%%
%% `El` is now:
%%
%% {xmlel,<<"iq">>,
%%        [{<<"from">>,<<"localhost">>},
%%         {<<"to">>,<<"alice@localhost/escalus-default-resource">>},
%%         {<<"type">>,<<"result">>}],
%%        [{xmlcdata,<<"  ">>},
%%         {xmlel,<<"query">>,
%%                [{<<"xmlns">>,<<"cdk:xmpp:frequent-contacts">>}],
%%                []}]}
%%
%% `mongoose_json:xmlel_to_json(El)` is an iolist() representing
%% the resulting JSON:
%%
%% {"name"    : "iq",
%%  "attrs"   : {"from": "localhost",
%%               "to"  : "alice@localhost/escalus-default-resource",
%%               "type": "result"},
%%  "children": [{"cdata": "  "},
%%               {"name"    : "query",
%%                "attrs"   : {"xmlns":"cdk:xmpp:frequent-contacts"},
%%                "children": []}]}
%%
%% The intermediate struct passed to MochiJson2 (i.e. result
%% of `mongoose_json:xmlel_to_mochistruct(El)`) is:
%%
%% {struct,[{<<"name">>,<<"iq">>},
%%          {<<"attrs">>,
%%           {struct,[{<<"from">>,<<"localhost">>},
%%                    {<<"to">>,<<"alice@localhost/escalus-default-resource">>},
%%                    {<<"type">>,<<"result">>}]}},
%%          {<<"children">>,
%%           [{struct,[{<<"cdata">>,<<"  ">>}]},
%%            {struct,[{<<"name">>,<<"query">>},
%%                     {<<"attrs">>,
%%                      {struct,[{<<"xmlns">>,<<"cdk:xmpp:frequent-contacts">>}]}},
%%                     {<<"children">>,[]}]}]}]}
%%
%% Given `Data = mongoose_json:xmlel_to_json(El)` to get back the `xmlel()`
%% we have to do `mongoose_json:json_to_xmlel(Data)`:
%%
%% {xmlel,<<"iq">>,
%%        [{<<"from">>,<<"localhost">>},
%%         {<<"to">>,<<"alice@localhost/escalus-default-resource">>},
%%         {<<"type">>,<<"result">>}],
%%        [{xmlcdata,<<"  ">>},
%%         {xmlel,<<"query">>,
%%                [{<<"xmlns">>,<<"cdk:xmpp:frequent-contacts">>}],
%%                []}]}
%%
%% Finally, we got `El == json_to_xmlel(xmlel_to_json(El))`.

-spec xmlel_to_json(xmlel()) -> iolist().
xmlel_to_json(#xmlel{} = XML) -> mochijson2:encode(xmlel_to_mochistruct(XML)).

-spec json_to_xmlel(iolist()) -> xmlel().
json_to_xmlel(Data) -> mochistruct_to_xmlel(mochijson2:decode(Data)).

%%
%% Encoder/decoder helpers
%%

xmlel_to_mochistruct({xmlcdata, CData}) -> {struct, [{<<"cdata">>, CData}]};
xmlel_to_mochistruct(#xmlel{name = Name,
                            attrs = Attrs,
                            children = Children}) ->
    xmlel_to_mochistruct(Name,
                         attrs_to_mochistruct(Attrs),
                         [ xmlel_to_mochistruct(El) || El <- Children ]).

attrs_to_mochistruct([]) -> [];
attrs_to_mochistruct(Attrs) ->
    {struct, Attrs}.

xmlel_to_mochistruct(Name, [], []) ->
    {struct, [{<<"name">>, Name}]};
xmlel_to_mochistruct(Name, Attrs, []) ->
    {struct, [{<<"name">>, Name},
              {<<"attrs">>, Attrs}]};
xmlel_to_mochistruct(Name, [], Children) ->
    {struct, [{<<"name">>, Name},
              {<<"children">>, Children}]};
xmlel_to_mochistruct(Name, Attrs, Children) ->
    {struct, [{<<"name">>, Name},
              {<<"attrs">>, Attrs},
              {<<"children">>,  Children}]}.

%% Just CData.
mochistruct_to_xmlel({struct, [{<<"cdata">>, CData}]}) ->
    {xmlcdata, CData};

%% No attrs, no children.
mochistruct_to_xmlel({struct, [{<<"name">>, Name}]}) ->
    xmlel(Name, [], []);

%% Attrs, but no children.
mochistruct_to_xmlel({struct, [{<<"name">>, Name}, {<<"attrs">>, Attrs}]}) ->
    xmlel(Name, mochistruct_to_attrs(Attrs), []);
mochistruct_to_xmlel({struct, [{<<"attrs">>, Attrs}, {<<"name">>, Name}]}) ->
    xmlel(Name, mochistruct_to_attrs(Attrs), []);

%% Children, but no attrs.
mochistruct_to_xmlel({struct, [{<<"name">>, Name}, {<<"children">>, Children}]}) ->
    xmlel(Name, [], [ mochistruct_to_xmlel(MochiStruct)
                      || MochiStruct <- Children ]);
mochistruct_to_xmlel({struct, [{<<"children">>, Children}, {<<"name">>, Name}]}) ->
    xmlel(Name, [], [ mochistruct_to_xmlel(MochiStruct)
                      || MochiStruct <- Children ]);

%% All fields present.
mochistruct_to_xmlel({struct, Prefabs}) ->
    [{<<"attrs">>, MochiAttrs},
     {<<"children">>, Children},
     {<<"name">>, Name}] = lists:sort(Prefabs),
    #xmlel{name = Name,
           attrs = mochistruct_to_attrs(MochiAttrs),
           children = [ mochistruct_to_xmlel(MochiStruct)
                        || MochiStruct <- Children ]}.

xmlel(Name, Attrs, Children) ->
    #xmlel{name = Name,
           attrs = Attrs,
           children = Children}.

mochistruct_to_attrs({struct, Attrs}) -> Attrs.
