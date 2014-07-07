-module(test_imgproc_nif).
-include_lib("eunit/include/eunit.hrl").

% Prevents this code from being included as compiled code in ebin
-ifdef(TEST).

% Function that initializes the camera, used as a pseudo setup function
initialize_cam() ->
    imgproc_nif:init().

% Function that deinitalizes the camera, used as a pseudo teardown function
deinitialize_cam() ->
    imgproc_nif:deinit_camera().

% Unit test that checks whether the imgproc_nif:init() initializes the camera
init_test() ->
    ?assertEqual({error,{reload,"Reload not supported by this NIF library."}}, initialize_cam()). 

% Unit test that checks whether imgproc_nif:get_pic() can retrive an image 
get_pic_test() ->
    initialize_cam(), 
    case imgproc_nif:get_pic() of 
        {Status, _Ref} ->
            ?assertEqual(ok, Status) 
    end.

% Unit test that checks whether the imgproc_nif:deinit_camera() deinitializes the
% camera 
deinit_test() ->
    ?assertEqual(ok,deinitialize_cam()).

-endif.
