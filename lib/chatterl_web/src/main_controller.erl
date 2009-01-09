%%%----------------------------------------------------------------
%%% @author  Yomi Colledge <yomi@boodah.net>
%%% @doc Web interface Main Controller
%%%
%%% Chatterl Administration web interface controller
%%%
%%% Sends messages to middle man and passes the results to the view.
%%% @end
%%% @copyright 2008 Yomi Colledge
%%%---------------------------------------------------------------
-module(main_controller).

-export([index/1]).
-export([handle_request/2, before_filter/1]).

-behaviour(gen_controller).
-include("beepbeep.hrl").


index(Params) ->
    gen_controller:call(?MODULE,index,Params).


%% Callbacks
handle_request(index,Params) ->
    S = Params#params.sid,
    Groups = chatterl_mid_man:list_groups(),
    {render, "main/index.html",[{sess_key,S},{groups,Groups],Params}.


%% Callback filter
before_filter(Params) ->
    {ok}.
