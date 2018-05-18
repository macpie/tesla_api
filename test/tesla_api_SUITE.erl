-module(tesla_api_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-export([
    all/0
    ,init_per_suite/1
    ,end_per_suite/1
]).

-export([
    get_oauth_token/1
    ,get_vehicles/1
    ,get_mobile_access/1
    ,get_charge_state/1
    ,get_climate_settings/1
    ,get_driving_position/1
    ,get_gui_settings/1
    ,get_state/1
    ,wake_up/1
    ,set_valet_mode/1, reset_valet_pin/1
    ,open_charge_port/1
    ,set_charge_limit_to_standard/1, set_charge_limit_to_max/1, set_charge_limit/1
    ,start_charging/1, stop_charging/1
    ,flash_lights/1
    ,honk_horn/1
    ,unlock_doors/1, lock_doors/1
    ,set_tempeature/1, start_hvac/1, stop_hvac/1
    ,open_roof/1
    ,remote_start/1
    ,open_close_trunk/1
]).

-define(TOKEN, <<"token">>).

%%--------------------------------------------------------------------
%% COMMON TEST CALLBACK FUNCTIONS
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
%% @public
%% @doc
%%   Running tests for this suite
%% @end
%%--------------------------------------------------------------------
all() ->
    [get_oauth_token, get_vehicles, get_mobile_access, get_charge_state
     ,get_climate_settings, get_driving_position, get_gui_settings, get_state
     ,wake_up, set_valet_mode, reset_valet_pin, open_charge_port
     ,set_charge_limit_to_standard, set_charge_limit_to_max, set_charge_limit
     ,start_charging, stop_charging, flash_lights, honk_horn, unlock_doors, lock_doors
     ,set_tempeature, start_hvac, stop_hvac, open_roof, remote_start, open_close_trunk].

%%--------------------------------------------------------------------
%% @public
%% @doc
%%   Special init config for suite
%% @end
%%--------------------------------------------------------------------
init_per_suite(Config) ->
    ct:pal("START OF ~p", [?MODULE]),
    application:ensure_all_started(hackney),
    Config.

%%--------------------------------------------------------------------
%% @public
%% @doc
%%   Special end config for groups
%% @end
%%--------------------------------------------------------------------
end_per_suite(_Config) ->
    ct:pal("END OF ~p", [?MODULE]),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES
%%--------------------------------------------------------------------
get_oauth_token(_Config) ->
    Resp = tesla_api:get_oauth_token(
        <<"CLINET_ID">>
        ,<<"CLINET_SECRET">>
        ,<<"EMAIL">>
        ,<<"PWD">>
    ),
    ?assertMatch({200, _, _}, Resp),
    {_, _, Map} = Resp,
    ?assertEqual(true, maps:is_key(<<"access_token">>, Map)),
    ?assertEqual(true, maps:is_key(<<"token_type">>, Map)),
    ?assertEqual(true, maps:is_key(<<"expires_in">>, Map)),
    ?assertEqual(true, maps:is_key(<<"created_at">>, Map)).

get_vehicles(_Config) ->
    Resp = tesla_api:get_vehicles(?TOKEN),
    ?assertMatch({200, _, _}, Resp),
    {_, _, Map} = Resp,
    ?assertEqual(true, maps:is_key(<<"response">>, Map)),
    ?assertEqual(true, maps:is_key(<<"count">>, Map)),
    Cars = maps:get(<<"response">>, Map),
    ?assertEqual(maps:get(<<"count">>, Map), erlang:length(Cars)),
    Keys = [
        <<"color">>
        ,<<"display_name">>
        ,{<<"id">>, is_integer}
        ,{<<"option_codes">>, is_binary}
        ,{<<"user_id">>, is_integer}
        ,{<<"vehicle_id">>, is_integer}
        ,{<<"vin">>, is_binary}
        ,{<<"tokens">>, is_list}
        ,{<<"state">>, is_binary}
    ],
    lists:foreach(
        fun(Car) ->
            ?assertEqual(ok, check_map(Car, Keys))
        end
        ,Cars
    ).

get_mobile_access(_Config) ->
    Resp = tesla_api:get_mobile_access(?TOKEN, <<"1">>),
    ?assertMatch({200, _, _}, Resp),
    {_, _, Map} = Resp,
    ?assertEqual(true, erlang:is_boolean(maps:get(<<"response">>, Map))).

get_charge_state(_Config) ->
    Resp = tesla_api:get_charge_state(?TOKEN, <<"1">>),
    ?assertMatch({200, _, _}, Resp),
    {_, _, Map} = Resp,
    ?assertEqual(true, maps:is_key(<<"response">>, Map)),
    Keys = [
        {<<"charging_state">>, is_binary}, {<<"charge_to_max_range">>, is_boolean}
        ,{<<"max_range_charge_counter">>, is_integer}, {<<"fast_charger_present">>, is_boolean}
        ,{<<"battery_range">>, is_float}, {<<"est_battery_range">>, is_float}
        ,{<<"ideal_battery_range">>, is_float}, {<<"battery_level">>, is_integer}
        ,{<<"battery_current">>, is_float}, <<"charge_starting_range">>
        ,<<"charge_starting_soc">>, {<<"charger_voltage">>, is_integer}
        ,{<<"charger_pilot_current">>, is_integer}, {<<"charger_actual_current">>, is_integer}
        ,{<<"charger_power">>, is_integer}, <<"time_to_full_charge">>
        ,{<<"charge_rate">>, is_float}, {<<"charge_port_door_open">>, is_boolean}
    ],
    ?assertEqual(ok, check_map(maps:get(<<"response">>, Map), Keys)).

get_climate_settings(_Config) ->
    Resp = tesla_api:get_climate_settings(?TOKEN, <<"1">>),
    ?assertMatch({200, _, _}, Resp),
    {_, _, Map} = Resp,
    ?assertEqual(true, maps:is_key(<<"response">>, Map)),
    Keys = [
        {<<"inside_temp">>, is_float}, {<<"outside_temp">>, is_float}
        ,{<<"driver_temp_setting">>, is_float}, {<<"passenger_temp_setting">>, is_float}
        ,{<<"is_auto_conditioning_on">>, is_boolean}, <<"is_front_defroster_on">>
        ,{<<"is_rear_defroster_on">>, is_boolean}, {<<"fan_status">>, is_integer}
    ],
    ?assertEqual(ok, check_map(maps:get(<<"response">>, Map), Keys)).

get_driving_position(_Config) ->
    Resp = tesla_api:get_driving_position(?TOKEN, <<"1">>),
    ?assertMatch({200, _, _}, Resp),
    {_, _, Map} = Resp,
    ?assertEqual(true, maps:is_key(<<"response">>, Map)),
    Keys = [
        <<"shift_state">>, <<"speed">>
        ,{<<"latitude">>, is_float}, {<<"longitude">>, is_float}
        ,{<<"heading">>, is_integer}, {<<"gps_as_of">>, is_integer}
    ],
    ?assertEqual(ok, check_map(maps:get(<<"response">>, Map), Keys)).

get_gui_settings(_Config) ->
    Resp = tesla_api:get_gui_settings(?TOKEN, <<"1">>),
    ?assertMatch({200, _, _}, Resp),
    {_, _, Map} = Resp,
    ?assertEqual(true, maps:is_key(<<"response">>, Map)),
    Keys = [
        {<<"gui_distance_units">>, is_binary}, {<<"gui_temperature_units">>, is_binary}
        ,{<<"gui_charge_rate_units">>, is_binary}, {<<"gui_24_hour_time">>, is_boolean}
        ,{<<"gui_range_display">>, is_binary}
    ],
    ?assertEqual(ok, check_map(maps:get(<<"response">>, Map), Keys)).

get_state(_Config) ->
    Resp = tesla_api:get_state(?TOKEN, <<"1">>),
    ?assertMatch({200, _, _}, Resp),
    {_, _, Map} = Resp,
    ?assertEqual(true, maps:is_key(<<"response">>, Map)),
    Keys = [
        {<<"df">>, is_boolean}, {<<"dr">>, is_boolean}
        ,{<<"pf">>, is_boolean}, {<<"pr">>, is_boolean}
        ,{<<"ft">>, is_boolean}, {<<"rt">>, is_boolean}
        ,{<<"car_verson">>, is_binary}, {<<"locked">>, is_boolean}
        ,{<<"sun_roof_installed">>, is_boolean}, {<<"sun_roof_state">>, is_binary}
        ,{<<"sun_roof_percent_open">>, is_integer}, {<<"dark_rims">>, is_boolean}
        ,{<<"wheel_type">>, is_binary}, {<<"has_spoiler">>, is_boolean}
        ,{<<"roof_color">>, is_binary}, {<<"perf_config">>, is_binary}
    ],
    ?assertEqual(ok, check_map(maps:get(<<"response">>, Map), Keys)).

wake_up(_Config) ->
    Resp = tesla_api:wake_up(?TOKEN, <<"1">>),
    ?assertMatch({200, _, _}, Resp),
    {_, _, Map} = Resp,
    ?assertEqual(true, maps:is_key(<<"response">>, Map)),
    Keys = [
        {<<"result">>, is_boolean}
    ],
    ?assertEqual(ok, check_map(maps:get(<<"response">>, Map), Keys)).

set_valet_mode(_Config) ->
    Resp = tesla_api:set_valet_mode(?TOKEN, <<"1">>),
    ?assertMatch({200, _, _}, Resp),
    {_, _, Map} = Resp,
    ?assertEqual(true, maps:is_key(<<"response">>, Map)),
    Keys = [
        {<<"result">>, is_boolean}
    ],
    ?assertEqual(ok, check_map(maps:get(<<"response">>, Map), Keys)).

reset_valet_pin(_Config) ->
    Resp = tesla_api:reset_valet_pin(?TOKEN, <<"1">>),
    ?assertMatch({200, _, _}, Resp),
    {_, _, Map} = Resp,
    ?assertEqual(true, maps:is_key(<<"response">>, Map)),
    Keys = [
        {<<"result">>, is_boolean}
    ],
    ?assertEqual(ok, check_map(maps:get(<<"response">>, Map), Keys)).

open_charge_port(_Config) ->
    Resp = tesla_api:open_charge_port(?TOKEN, <<"1">>),
    ?assertMatch({200, _, _}, Resp),
    {_, _, Map} = Resp,
    ?assertEqual(true, maps:is_key(<<"response">>, Map)),
    Keys = [
        {<<"result">>, is_boolean}
    ],
    ?assertEqual(ok, check_map(maps:get(<<"response">>, Map), Keys)).

set_charge_limit_to_standard(_Config) ->
    Resp = tesla_api:set_charge_limit_to_standard(?TOKEN, <<"1">>),
    ?assertMatch({200, _, _}, Resp),
    {_, _, Map} = Resp,
    ?assertEqual(true, maps:is_key(<<"response">>, Map)),
    Keys = [
        {<<"result">>, is_boolean}
    ],
    ?assertEqual(ok, check_map(maps:get(<<"response">>, Map), Keys)).

set_charge_limit_to_max(_Config) ->
    Resp = tesla_api:set_charge_limit_to_max(?TOKEN, <<"1">>),
    ?assertMatch({200, _, _}, Resp),
    {_, _, Map} = Resp,
    ?assertEqual(true, maps:is_key(<<"response">>, Map)),
    Keys = [
        {<<"result">>, is_boolean}
    ],
    ?assertEqual(ok, check_map(maps:get(<<"response">>, Map), Keys)).

set_charge_limit(_Config) ->
    Resp = tesla_api:set_charge_limit(?TOKEN, <<"1">>, <<"75">>),
    ?assertMatch({200, _, _}, Resp),
    {_, _, Map} = Resp,
    ?assertEqual(true, maps:is_key(<<"response">>, Map)),
    Keys = [
        {<<"result">>, is_boolean}
    ],
    ?assertEqual(ok, check_map(maps:get(<<"response">>, Map), Keys)).

start_charging(_Config) ->
    Resp = tesla_api:start_charging(?TOKEN, <<"1">>),
    ?assertMatch({200, _, _}, Resp),
    {_, _, Map} = Resp,
    ?assertEqual(true, maps:is_key(<<"response">>, Map)),
    Keys = [
        {<<"result">>, is_boolean}
    ],
    ?assertEqual(ok, check_map(maps:get(<<"response">>, Map), Keys)).

stop_charging(_Config) ->
    Resp = tesla_api:stop_charging(?TOKEN, <<"1">>),
    ?assertMatch({200, _, _}, Resp),
    {_, _, Map} = Resp,
    ?assertEqual(true, maps:is_key(<<"response">>, Map)),
    Keys = [
        {<<"result">>, is_boolean}
    ],
    ?assertEqual(ok, check_map(maps:get(<<"response">>, Map), Keys)).

flash_lights(_Config) ->
    Resp = tesla_api:flash_lights(?TOKEN, <<"1">>),
    ?assertMatch({200, _, _}, Resp),
    {_, _, Map} = Resp,
    ?assertEqual(true, maps:is_key(<<"response">>, Map)),
    Keys = [
        {<<"result">>, is_boolean}
    ],
    ?assertEqual(ok, check_map(maps:get(<<"response">>, Map), Keys)).

honk_horn(_Config) ->
    Resp = tesla_api:honk_horn(?TOKEN, <<"1">>),
    ?assertMatch({200, _, _}, Resp),
    {_, _, Map} = Resp,
    ?assertEqual(true, maps:is_key(<<"response">>, Map)),
    Keys = [
        {<<"result">>, is_boolean}
    ],
    ?assertEqual(ok, check_map(maps:get(<<"response">>, Map), Keys)).

unlock_doors(_Config) ->
    Resp = tesla_api:unlock_doors(?TOKEN, <<"1">>),
    ?assertMatch({200, _, _}, Resp),
    {_, _, Map} = Resp,
    ?assertEqual(true, maps:is_key(<<"response">>, Map)),
    Keys = [
        {<<"result">>, is_boolean}
    ],
    ?assertEqual(ok, check_map(maps:get(<<"response">>, Map), Keys)).

lock_doors(_Config) ->
    Resp = tesla_api:lock_doors(?TOKEN, <<"1">>),
    ?assertMatch({200, _, _}, Resp),
    {_, _, Map} = Resp,
    ?assertEqual(true, maps:is_key(<<"response">>, Map)),
    Keys = [
        {<<"result">>, is_boolean}
    ],
    ?assertEqual(ok, check_map(maps:get(<<"response">>, Map), Keys)).

set_tempeature(_Config) ->
    Resp = tesla_api:set_tempeature(?TOKEN, <<"1">>, <<"21.7">>, <<"22.7">>),
    ?assertMatch({200, _, _}, Resp),
    {_, _, Map} = Resp,
    ?assertEqual(true, maps:is_key(<<"response">>, Map)),
    Keys = [
        {<<"result">>, is_boolean}
    ],
    ?assertEqual(ok, check_map(maps:get(<<"response">>, Map), Keys)).

start_hvac(_Config) ->
    Resp = tesla_api:start_hvac(?TOKEN, <<"1">>),
    ?assertMatch({200, _, _}, Resp),
    {_, _, Map} = Resp,
    ?assertEqual(true, maps:is_key(<<"response">>, Map)),
    Keys = [
        {<<"result">>, is_boolean}
    ],
    ?assertEqual(ok, check_map(maps:get(<<"response">>, Map), Keys)).

stop_hvac(_Config) ->
    Resp = tesla_api:stop_hvac(?TOKEN, <<"1">>),
    ?assertMatch({200, _, _}, Resp),
    {_, _, Map} = Resp,
    ?assertEqual(true, maps:is_key(<<"response">>, Map)),
    Keys = [
        {<<"result">>, is_boolean}
    ],
    ?assertEqual(ok, check_map(maps:get(<<"response">>, Map), Keys)).

open_roof(_Config) ->
    Resp = tesla_api:open_roof(?TOKEN, <<"1">>, <<"75">>),
    ?assertMatch({200, _, _}, Resp),
    {_, _, Map} = Resp,
    ?assertEqual(true, maps:is_key(<<"response">>, Map)),
    Keys = [
        {<<"result">>, is_boolean}
    ],
    ?assertEqual(ok, check_map(maps:get(<<"response">>, Map), Keys)).

remote_start(_Config) ->
    Resp = tesla_api:remote_start(?TOKEN, <<"1">>, <<"password">>),
    ?assertMatch({200, _, _}, Resp),
    {_, _, Map} = Resp,
    ?assertEqual(true, maps:is_key(<<"response">>, Map)),
    Keys = [
        {<<"result">>, is_boolean}
    ],
    ?assertEqual(ok, check_map(maps:get(<<"response">>, Map), Keys)).

open_close_trunk(_Config) ->
    Resp = tesla_api:open_close_trunk(?TOKEN, <<"1">>),
    ?assertMatch({200, _, _}, Resp),
    {_, _, Map} = Resp,
    ?assertEqual(true, maps:is_key(<<"response">>, Map)),
    Keys = [
        {<<"result">>, is_boolean}
    ],
    ?assertEqual(ok, check_map(maps:get(<<"response">>, Map), Keys)).

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------
check_map(_Map, []) -> ok;
check_map(Map, [{Key, Type}|Keys]) ->
    case maps:is_key(Key, Map) of
        false ->
            ct:fail("missing ~p in ~p", [Key, Map]);
        true ->
            Val = maps:get(Key, Map),
            case erlang:Type(Val) of
                false ->
                    ct:fail("wrong value type for ~p [~p] = ~p", [Key, Type, Val]);
                true ->
                    check_map(Map, Keys)
            end
    end;
check_map(Map, [Key|Keys]) ->
    case maps:is_key(Key, Map) of
        false ->
            ct:fail("missing ~p in ~p", [Key, Map]);
        true ->
            check_map(Map, Keys)
    end.
