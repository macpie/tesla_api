%%%-------------------------------------------------------------------
%% @author Macpie
%% @doc
%% == Tesla APIs ==
%% @end
%% Copyright (c) 2018 Macpie
%%%-------------------------------------------------------------------
-module(tesla_api).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------
-export([
    get_oauth_token/4
    ,get_vehicles/1
    ,get_mobile_access/2
    ,get_charge_state/2
    ,get_climate_settings/2
    ,get_driving_position/2
    ,get_gui_settings/2
    ,get_state/2
    ,wake_up/2
    ,set_valet_mode/2, reset_valet_pin/2
    ,open_charge_port/2
    ,set_charge_limit_to_standard/2, set_charge_limit_to_max/2, set_charge_limit/3
    ,start_charging/2, stop_charging/2
    ,flash_lights/2
    ,honk_horn/2
    ,unlock_doors/2, lock_doors/2
    ,set_tempeature/4, start_hvac/2, stop_hvac/2
    ,open_roof/3
    ,remote_start/3
    ,open_close_trunk/2
]).

-ifdef(TEST).
-define(URL, <<"https://private-anon-869c23fa6d-timdorr.apiary-mock.com">>).
-else.
-define(URL, <<"https://owner-api.teslamotors.com">>).
-endif.

-define(V, <<"/api/1">>).
-define(HEADERS(Bearer), [{<<"Authorization">>, <<"Bearer ", Bearer/binary>>}]).

-type api_return() :: {number(), list(), map()}.

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

%%--------------------------------------------------------------------
%% @doc
%% Get an Access Token.
%% @end
%%--------------------------------------------------------------------

-spec get_oauth_token(ClientID :: binary(), ClientSecret :: binary()
                      ,Email :: binary(), Password :: binary()) -> api_return().
get_oauth_token(ClientID, ClientSecret, Email, Password) ->
    Payload = {form, [
        {<<"grant_type">>, <<"password">>}
        ,{<<"client_id">>, ClientID}
        ,{<<"client_secret">>, ClientSecret}
        ,{<<"email">>, Email}
        ,{<<"password">>, Password}
    ]},
    post(<<"/oauth/token">>, Payload, []).

%%--------------------------------------------------------------------
%% @doc
%% Retrieve a list of your owned vehicles.
%% @end
%%--------------------------------------------------------------------
-spec get_vehicles(Token :: binary()) -> api_return().
get_vehicles(Token) ->
    get(<<?V/binary, "/vehicles">>, ?HEADERS(Token)).

%%--------------------------------------------------------------------
%% @doc
%% Determines if mobile access to the vehicle is enabled.
%% @end
%%--------------------------------------------------------------------
-spec get_mobile_access(Token :: binary(), ID :: binary()) -> api_return().
get_mobile_access(Token, ID) ->
    get(<<?V/binary, "/vehicles/", ID/binary, "/mobile_enabled">>, ?HEADERS(Token)).

%%--------------------------------------------------------------------
%% @doc
%% Returns the state of charge in the battery.
%% @end
%%--------------------------------------------------------------------
-spec get_charge_state(Token :: binary(), ID :: binary()) -> api_return().
get_charge_state(Token, ID) ->
    get(<<?V/binary, "/vehicles/", ID/binary, "/data_request/charge_state">>, ?HEADERS(Token)).

%%--------------------------------------------------------------------
%% @doc
%% Returns the current temperature and climate control state.
%% @end
%%--------------------------------------------------------------------
-spec get_climate_settings(Token :: binary(), ID :: binary()) -> api_return().
get_climate_settings(Token, ID) ->
    get(<<?V/binary, "/vehicles/", ID/binary, "/data_request/climate_state">>, ?HEADERS(Token)).

%%--------------------------------------------------------------------
%% @doc
%% Returns the driving and position state of the vehicle.
%% @end
%%--------------------------------------------------------------------
-spec get_driving_position(Token :: binary(), ID :: binary()) -> api_return().
get_driving_position(Token, ID) ->
    get(<<?V/binary, "/vehicles/", ID/binary, "/data_request/drive_state">>, ?HEADERS(Token)).

%%--------------------------------------------------------------------
%% @doc
%% Returns various information about the GUI settings of the car, such as unit format and range display.
%% @end
%%--------------------------------------------------------------------
-spec get_gui_settings(Token :: binary(), ID :: binary()) -> api_return().
get_gui_settings(Token, ID) ->
    get(<<?V/binary, "/vehicles/", ID/binary, "/data_request/gui_settings">>, ?HEADERS(Token)).

%%--------------------------------------------------------------------
%% @doc
%% Returns the vehicle's physical state, such as which doors are open.
%% @end
%%--------------------------------------------------------------------
-spec get_state(Token :: binary(), ID :: binary()) -> api_return().
get_state(Token, ID) ->
    get(<<?V/binary, "/vehicles/", ID/binary, "/data_request/vehicle_state">>, ?HEADERS(Token)).

%%--------------------------------------------------------------------
%% @doc
%% Wakes up the car from the sleep state. Necessary to get some data from the car.
%% @end
%%--------------------------------------------------------------------
-spec wake_up(Token :: binary(), ID :: binary()) -> api_return().
wake_up(Token, ID) ->
    post(<<?V/binary, "/vehicles/", ID/binary, "/wake_up">>, <<>>, ?HEADERS(Token)).

%%--------------------------------------------------------------------
%% @doc
%% Sets valet mode on or off with a PIN to disable it from within the car.
%% Reuses last PIN from previous valet session. Valet Mode limits the car's top
%% speed to 70MPH and 80kW of acceleration power. It also disables Homelink,
%% Bluetooth and Wifi settings, and the ability to disable mobile access to the car.
%% It also hides your favorites, home, and work locations in navigation.
%% @end
%%--------------------------------------------------------------------
-spec set_valet_mode(Token :: binary(), ID :: binary()) -> api_return().
set_valet_mode(Token, ID) ->
    post(<<?V/binary, "/vehicles/", ID/binary, "/command/set_valet_mode">>, <<>>, ?HEADERS(Token)).

%%--------------------------------------------------------------------
%% @doc
%% Resets the PIN set for valet mode, if set.
%% @end
%%--------------------------------------------------------------------
-spec reset_valet_pin(Token :: binary(), ID :: binary()) -> api_return().
reset_valet_pin(Token, ID) ->
    post(<<?V/binary, "/vehicles/", ID/binary, "/command/reset_valet_pin">>, <<>>, ?HEADERS(Token)).

%%--------------------------------------------------------------------
%% @doc
%% Opens the charge port. Does not close the charge port (for now...). This endpoint also unlocks the charge port if it's locked.
%% @end
%%--------------------------------------------------------------------
-spec open_charge_port(Token :: binary(), ID :: binary()) -> api_return().
open_charge_port(Token, ID) ->
    post(<<?V/binary, "/vehicles/", ID/binary, "/command/charge_port_door_open">>, <<>>, ?HEADERS(Token)).

%%--------------------------------------------------------------------
%% @doc
%% Set the charge mode to standard (90% under the new percentage system introduced in 4.5).
%% @end
%%--------------------------------------------------------------------
-spec set_charge_limit_to_standard(Token :: binary(), ID :: binary()) -> api_return().
set_charge_limit_to_standard(Token, ID) ->
    post(<<?V/binary, "/vehicles/", ID/binary, "/command/charge_standard">>, <<>>, ?HEADERS(Token)).

%%--------------------------------------------------------------------
%% @doc
%% Set the charge mode to max range (100% under the new percentage system introduced in 4.5). Use sparingly!
%% @end
%%--------------------------------------------------------------------
-spec set_charge_limit_to_max(Token :: binary(), ID :: binary()) -> api_return().
set_charge_limit_to_max(Token, ID) ->
    post(<<?V/binary, "/vehicles/", ID/binary, "/command/charge_max_range">>, <<>>, ?HEADERS(Token)).

%%--------------------------------------------------------------------
%% @doc
%% Set the charge limit to a custom percentage.
%% @end
%%--------------------------------------------------------------------
-spec set_charge_limit(Token :: binary(), ID :: binary(), Limit :: binary()) -> api_return().
set_charge_limit(Token, ID, Limit) ->
    post(<<?V/binary, "/vehicles/", ID/binary, "/command/set_charge_limit?percent=", Limit/binary>>, <<>>, ?HEADERS(Token)).

%%--------------------------------------------------------------------
%% @doc
%% Start charging. Must be plugged in, have power available, and not have reached your charge limit.
%% @end
%%--------------------------------------------------------------------
-spec start_charging(Token :: binary(), ID :: binary()) -> api_return().
start_charging(Token, ID) ->
    post(<<?V/binary, "/vehicles/", ID/binary, "/command/charge_start">>, <<>>, ?HEADERS(Token)).

%%--------------------------------------------------------------------
%% @doc
%% Stop charging. Must already be charging.
%% @end
%%--------------------------------------------------------------------
-spec stop_charging(Token :: binary(), ID :: binary()) -> api_return().
stop_charging(Token, ID) ->
    post(<<?V/binary, "/vehicles/", ID/binary, "/command/charge_stop">>, <<>>, ?HEADERS(Token)).

%%--------------------------------------------------------------------
%% @doc
%% Flash the lights once.
%% @end
%%--------------------------------------------------------------------
-spec flash_lights(Token :: binary(), ID :: binary()) -> api_return().
flash_lights(Token, ID) ->
    post(<<?V/binary, "/vehicles/", ID/binary, "/command/flash_lights">>, <<>>, ?HEADERS(Token)).

%%--------------------------------------------------------------------
%% @doc
%% Honk the horn once.
%% @end
%%--------------------------------------------------------------------
-spec honk_horn(Token :: binary(), ID :: binary()) -> api_return().
honk_horn(Token, ID) ->
    post(<<?V/binary, "/vehicles/", ID/binary, "/command/honk_horn">>, <<>>, ?HEADERS(Token)).

%%--------------------------------------------------------------------
%% @doc
%% Unlock the car's doors.
%% @end
%%--------------------------------------------------------------------
-spec unlock_doors(Token :: binary(), ID :: binary()) -> api_return().
unlock_doors(Token, ID) ->
    post(<<?V/binary, "/vehicles/", ID/binary, "/command/door_unlock">>, <<>>, ?HEADERS(Token)).

%%--------------------------------------------------------------------
%% @doc
%% Lock the car's doors.
%% @end
%%--------------------------------------------------------------------
-spec lock_doors(Token :: binary(), ID :: binary()) -> api_return().
lock_doors(Token, ID) ->
    post(<<?V/binary, "/vehicles/", ID/binary, "/command/door_lock">>, <<>>, ?HEADERS(Token)).

%%--------------------------------------------------------------------
%% @doc
%% Set the temperature target for the HVAC system
%% @end
%%--------------------------------------------------------------------
-spec set_tempeature(Token :: binary(), ID :: binary(), DriverTemp :: binary()
                     ,PassenderTemp :: binary()) -> api_return().
set_tempeature(Token, ID, DriverTemp, PassenderTemp) ->
    Url = <<?V/binary, "/vehicles/", ID/binary, "/command/set_temps?driver_temp="
            ,DriverTemp/binary, "&passenger_temp=", PassenderTemp/binary>>,
    post(Url, <<>>, ?HEADERS(Token)).

%%--------------------------------------------------------------------
%% @doc
%% Start the climate control system. Will cool or heat automatically, depending on set temperature.
%% @end
%%--------------------------------------------------------------------
-spec start_hvac(Token :: binary(), ID :: binary()) -> api_return().
start_hvac(Token, ID) ->
    post(<<?V/binary, "/vehicles/", ID/binary, "/command/auto_conditioning_start">>, <<>>, ?HEADERS(Token)).

%%--------------------------------------------------------------------
%% @doc
%% Stop the climate control system.
%% @end
%%--------------------------------------------------------------------
-spec stop_hvac(Token :: binary(), ID :: binary()) -> api_return().
stop_hvac(Token, ID) ->
    post(<<?V/binary, "/vehicles/", ID/binary, "/command/auto_conditioning_stop">>, <<>>, ?HEADERS(Token)).

%%--------------------------------------------------------------------
%% @doc
%% Controls the car's panoramic roof, if installed.
%% @end
%%--------------------------------------------------------------------
-spec open_roof(Token :: binary(), ID :: binary(), Percent :: binary()) -> api_return().
open_roof(Token, ID, Percent) ->
    Url = <<?V/binary, "/vehicles/", ID/binary
            ,"/command/sun_roof_control?state=open&percent=", Percent/binary>>,
    post(Url, <<>>, ?HEADERS(Token)).

%%--------------------------------------------------------------------
%% @doc
%% Start the car for keyless driving. Must start driving within 2 minutes of issuing this request.
%% @end
%%--------------------------------------------------------------------
-spec remote_start(Token :: binary(), ID :: binary(), Password :: binary()) -> api_return().
remote_start(Token, ID, Password) ->
    Url = <<?V/binary, "/vehicles/", ID/binary
            ,"/command/remote_start_drive?password=", Password/binary>>,
    post(Url, <<>>, ?HEADERS(Token)).

%%--------------------------------------------------------------------
%% @doc
%% Open the trunk. Call the endpoint again to close.
%% @end
%%--------------------------------------------------------------------
-spec open_close_trunk(Token :: binary(), ID :: binary()) -> api_return().
open_close_trunk(Token, ID) ->
    post(<<?V/binary, "/vehicles/", ID/binary, "/command/trunk_open">>, <<>>, ?HEADERS(Token)).

%%====================================================================
%% Internal functions
%%====================================================================
-spec get(binary(), list()) -> api_return().
get(Url, Headers) ->
    {ok, RespCode, RespHeaders, Client} = hackney:request(get, <<?URL/binary , Url/binary>>, Headers, <<>>, []),
    {ok, RespBody} = hackney:body(Client),
    case jsx:is_json(RespBody) of
        false ->
            {RespCode, RespHeaders, RespBody};
        true ->
            {RespCode, RespHeaders, jsx:decode(RespBody, [return_maps])}
    end.

-spec post(binary(), binary(), list()) -> api_return().
post(Url, Payload, Headers) ->
    {ok, RespCode, RespHeaders, Client} = hackney:request(post, <<?URL/binary , Url/binary>>, Headers, Payload, []),
    {ok, RespBody} = hackney:body(Client),
    case jsx:is_json(RespBody) of
        false ->
            {RespCode, RespHeaders, RespBody};
        true ->
            {RespCode, RespHeaders, jsx:decode(RespBody, [return_maps])}
    end.
