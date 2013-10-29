-module(niftest).

%%-export([init/0, read/0 , write/1 , pref/0, test/0, test_1000/2, get_]).

-compile(export_all).

start(Ptr, Number) ->
    spawn(niftest, reader, [Ptr,Number]).

init() ->
    erlang:load_nif("./niftest", 0).

get_pic() ->
    "NIF library not loaded".

show_pic(F) ->
    "NIF library not loaded".

read_part(Ptr,Number) ->
    "NIF library not loaded".

read_complete(Ptr) ->
    "NIF library not loaded".

process_complete(Ptr) ->
    "NIF library not loaded".

trace_pic(Ptr) ->
    "NIF library not loaded".

process_pic() ->
    "NIF library not loaded".

fuck() ->
    haha.

%% INTERNAL

reader(Ptr,Number) ->
    List = read_part(Ptr,Number),
    gen_server:cast(niftestserv, {add, List,Number}).

test() ->
    {ok,Ref} = get_pic(),
    trace_pic(Ref).



pick(List) ->
    pick(List,{0,0}).

pick([{X,Y}|T], {_MX,MY}) when Y>MY ->
    pick(T,{X,Y});
pick([{X,Y}|T], {MX,MY})->
    pick(T,{MX,MY});
pick([],Res) ->
    Res.
    
    
