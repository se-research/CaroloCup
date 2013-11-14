-module(imgproc_nif).

-compile(export_all).

-define(NIF_STUB, exit(nif_library_not_loaded)).

-on_load(init/0).

init() ->
     erlang:load_nif("./ebin/imgproc_nif", 0).

get_pic() ->
    ?NIF_STUB.

show_pic(F) ->
    ?NIF_STUB.

process_pic(Ref, IMG_NO) ->
    ?NIF_STUB.

deinit_camera() ->
    ?NIF_STUB.


%% INTERNAL

% retrieves 1000 test images, and saves them to /c_src/images
test_stream()->
    test_stream(100).

test_stream(0) ->
  ok;
test_stream(X) ->
  {ok, Ref} = get_pic(),
  show_pic(Ref),
  test_stream(X-1).  

