-module(mongoose_json_SUITE).

-compile([export_all]).

-include_lib("common_test/include/ct.hrl").
-include_lib("proper/include/proper.hrl").
-include_lib("eunit/include/eunit.hrl").
-include_lib("exml/include/exml.hrl").

-define(a(E), ?assert(E)).
-define(ae(E, A), ?assertEqual(E, A)).

all() ->
    [{group, exml_bson},
     {group, properties}].

groups() ->
    [{exml_bson, [identity_no_attrs_no_children_test,
                  identity_no_attrs_with_children_test,
                  identity_with_attrs_no_children_test,
                  identity_with_attrs_with_children_test]},
     {properties, [identity]}].

%%
%% Tests
%%

identity_no_attrs_no_children_test(_) ->
    identity_test(no_attrs, no_children).

identity_no_attrs_with_children_test(_) ->
    identity_test(no_attrs, with_children).

identity_with_attrs_no_children_test(_) ->
    identity_test(with_attrs, no_children).

identity_with_attrs_with_children_test(_) ->
    identity_test(with_attrs, with_children).

identity_test(Attrs, Children) ->
    Enc = fun mongoose_json:xmlel_to_json/1,
    Dec = fun mongoose_json:json_to_xmlel/1,
    El = static_xmlel(Attrs, Children),
    ?ae(El, Dec(Enc(El))).

identity(_) ->
    property(identity, ?FORALL(El, xmlel(), dec_enc(El))).

dec_enc(El) ->
    Enc = fun mongoose_json:xmlel_to_json/1,
    Dec = fun mongoose_json:json_to_xmlel/1,
    El =:= Dec(Enc(El)).

%%
%% Helpers
%%

static_xmlel(no_attrs, no_children) ->
    #xmlel{name = <<"elem">>};
static_xmlel(no_attrs, with_children) ->
    #xmlel{name = <<"elem">>,
           children = [#xmlel{name = <<"with-subelem">>}]};
static_xmlel(with_attrs, no_children) ->
    #xmlel{name = <<"elem">>,
           attrs = [{<<"with">>, <<"attr1">>},
                    {<<"and">>, <<"attr2">>}]};
static_xmlel(with_attrs, with_children) ->
    #xmlel{name = <<"elem">>,
           attrs = [{<<"with">>, <<"attr1">>},
                    {<<"and">>, <<"attr2">>}],
           children = [#xmlel{name = <<"with-subelem">>}]}.

property(Name, Prop) ->
    Props = proper:conjunction([{Name, Prop}]),
    ?a(proper:quickcheck(Props, [verbose, long_result, {numtests, 50}])).

%%
%% Generators
%%

ascii_text() ->
    non_empty(list(choose($a, $z))).

xmlel_attr() ->
    ?LET({Key, Val}, {ascii_text(), ascii_text()},
         {list_to_binary(Key), list_to_binary(Val)}).

xmlel_attrs() ->
    ?LET(Len, choose(0, 5), vector(Len, xmlel_attr())).

xmlel() ->
    ?SIZED(Size, xmlel(Size)).

xmlel(0) ->
    ?LET({Name, Attrs}, {ascii_text(), xmlel_attrs()},
         #xmlel{name = list_to_binary(Name),
                attrs = Attrs});
xmlel(Size) ->
    ?LET({Name, Attrs}, {ascii_text(), xmlel_attrs()},
         #xmlel{name = list_to_binary(Name),
                attrs = Attrs,
                children = xmlel_children(Size)}).

xmlel_children(Size) ->
    ?LET(Len, choose(0, 5), vector(Len, xmlel_child(Size))).

xmlel_child(Size) ->
    ?LET(CData, ascii_text(),
         oneof([#xmlcdata{content = list_to_binary(CData)},
                xmlel(Size div 3)])).
