-module(mongoose_json).
-include_lib("exml/include/exml.hrl").

%% Encoder/decoder of xmlel()s to/from BSON
-export([xmlel_to_json/1,
         json_to_xmlel/1]).

%%
%% Encoder/decoder of xmlel()s to/from BSON
%%

%% El == json_to_xmlel(xmlel_to_json(El)) must hold.

xmlel_to_json(#xmlel{name = Name, attrs = [], children = []}) ->
    {name, bson:utf8(Name)};
xmlel_to_json(#xmlel{name = Name, attrs = Attrs, children = []}) ->
    {name, bson:utf8(Name),
     attrs, attrs_to_bson(Attrs)};
xmlel_to_json(#xmlel{name = Name, attrs = [], children = Children}) ->
    {name, bson:utf8(Name),
     children, [xmlel_to_json(C) || C <- Children]};
xmlel_to_json(#xmlel{name = Name, attrs = Attrs, children = Children}) ->
    {name, bson:utf8(Name),
     attrs, attrs_to_bson(Attrs),
     children, [xmlel_to_json(C) || C <- Children]};
xmlel_to_json(#xmlcdata{content = CData}) ->
    {cdata, bson:utf8(CData)}.

json_to_xmlel({name, Name}) ->
    #xmlel{name = Name};
json_to_xmlel({name, Name, attrs, BSONAttrs}) ->
    #xmlel{name = Name,
           attrs = bson_to_attrs(BSONAttrs)};
json_to_xmlel({name, Name, children, BSONChildren}) ->
    #xmlel{name = Name,
           children = [json_to_xmlel(BC) || BC <- BSONChildren]};
json_to_xmlel({name, Name, attrs, BSONAttrs, children, BSONChildren}) ->
    #xmlel{name = Name,
           attrs = bson_to_attrs(BSONAttrs),
           children = [json_to_xmlel(BC) || BC <- BSONChildren]};
json_to_xmlel({cdata, CData}) ->
    #xmlcdata{content = CData}.

%%
%% Encoder/decoder helpers
%%

%% Ok, this is a bit problematic on BSON side.
%% Basically, the keys in a BSON object are atoms,
%% so we don't want to build objects from XML attribute lists,
%% because they may contain infinite possible attribute names,
%% while the number of existing atoms in BEAM is always finite.
%% Hence, in order not to cause a crash,
%% we encode atoms as a BSON array of BSON two-element arrays ( aka pairs ;).
%% For:
%%
%%   #xmlel{name = <<"elem">>,
%%          attrs = [{<<"with">>, <<"attr">>}]},
%%
%% We get (lookup with `db.xml.find()`) in mongo console:
%%
%%   { "_id" : ObjectId("53725bbfbf947d0d92000002"),
%%     "name" : "elem",
%%     "attrs" : [ [ "with", "attr" ] ],
%%     "children" :  }
attrs_to_bson(Attrs) ->
    [attr_to_bson(A) || A <- Attrs].

attr_to_bson({Name, Value}) ->
    [bson:utf8(Name), bson:utf8(Value)].

bson_to_attrs(BSONAttrs) ->
    [erlang:list_to_tuple(Pair) || Pair <- BSONAttrs].
