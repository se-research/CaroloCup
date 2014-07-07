-module(temp).

-compile(export_all).

pred({_X,10}) ->
    true;
pred(_) ->
    false.
