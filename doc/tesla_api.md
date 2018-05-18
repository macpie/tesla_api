

# Module tesla_api #
* [Description](#description)
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)



### <a name="Tesla_APIs">Tesla APIs</a> ###
.

__Authors:__ Macpie.

<a name="types"></a>

## Data Types ##




### <a name="type-api_return">api_return()</a> ###


<pre><code>
api_return() = {number(), list(), map()}
</code></pre>

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#flash_lights-2">flash_lights/2</a></td><td>
Flash the lights once.</td></tr><tr><td valign="top"><a href="#get_charge_state-2">get_charge_state/2</a></td><td>
Returns the state of charge in the battery.</td></tr><tr><td valign="top"><a href="#get_climate_settings-2">get_climate_settings/2</a></td><td>
Returns the current temperature and climate control state.</td></tr><tr><td valign="top"><a href="#get_driving_position-2">get_driving_position/2</a></td><td>
Returns the driving and position state of the vehicle.</td></tr><tr><td valign="top"><a href="#get_gui_settings-2">get_gui_settings/2</a></td><td>
Returns various information about the GUI settings of the car, such as unit format and range display.</td></tr><tr><td valign="top"><a href="#get_mobile_access-2">get_mobile_access/2</a></td><td>
Determines if mobile access to the vehicle is enabled.</td></tr><tr><td valign="top"><a href="#get_oauth_token-4">get_oauth_token/4</a></td><td>
Get an Access Token.</td></tr><tr><td valign="top"><a href="#get_state-2">get_state/2</a></td><td>
Returns the vehicle's physical state, such as which doors are open.</td></tr><tr><td valign="top"><a href="#get_vehicles-1">get_vehicles/1</a></td><td>
Retrieve a list of your owned vehicles.</td></tr><tr><td valign="top"><a href="#honk_horn-2">honk_horn/2</a></td><td>
Honk the horn once.</td></tr><tr><td valign="top"><a href="#lock_doors-2">lock_doors/2</a></td><td>
Lock the car's doors.</td></tr><tr><td valign="top"><a href="#open_charge_port-2">open_charge_port/2</a></td><td>
Opens the charge port.</td></tr><tr><td valign="top"><a href="#open_close_trunk-2">open_close_trunk/2</a></td><td>
Open the trunk.</td></tr><tr><td valign="top"><a href="#open_roof-3">open_roof/3</a></td><td>
Controls the car's panoramic roof, if installed.</td></tr><tr><td valign="top"><a href="#remote_start-3">remote_start/3</a></td><td>
Start the car for keyless driving.</td></tr><tr><td valign="top"><a href="#reset_valet_pin-2">reset_valet_pin/2</a></td><td>
Resets the PIN set for valet mode, if set.</td></tr><tr><td valign="top"><a href="#set_charge_limit-3">set_charge_limit/3</a></td><td>
Set the charge limit to a custom percentage.</td></tr><tr><td valign="top"><a href="#set_charge_limit_to_max-2">set_charge_limit_to_max/2</a></td><td>
Set the charge mode to max range (100% under the new percentage system introduced in 4.5).</td></tr><tr><td valign="top"><a href="#set_charge_limit_to_standard-2">set_charge_limit_to_standard/2</a></td><td>
Set the charge mode to standard (90% under the new percentage system introduced in 4.5).</td></tr><tr><td valign="top"><a href="#set_tempeature-4">set_tempeature/4</a></td><td>
Set the temperature target for the HVAC system.</td></tr><tr><td valign="top"><a href="#set_valet_mode-2">set_valet_mode/2</a></td><td>
Sets valet mode on or off with a PIN to disable it from within the car.</td></tr><tr><td valign="top"><a href="#start_charging-2">start_charging/2</a></td><td>
Start charging.</td></tr><tr><td valign="top"><a href="#start_hvac-2">start_hvac/2</a></td><td>
Start the climate control system.</td></tr><tr><td valign="top"><a href="#stop_charging-2">stop_charging/2</a></td><td>
Stop charging.</td></tr><tr><td valign="top"><a href="#stop_hvac-2">stop_hvac/2</a></td><td>
Stop the climate control system.</td></tr><tr><td valign="top"><a href="#unlock_doors-2">unlock_doors/2</a></td><td>
Unlock the car's doors.</td></tr><tr><td valign="top"><a href="#wake_up-2">wake_up/2</a></td><td>
Wakes up the car from the sleep state.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="flash_lights-2"></a>

### flash_lights/2 ###

<pre><code>
flash_lights(Token::binary(), ID::binary()) -&gt; <a href="#type-api_return">api_return()</a>
</code></pre>
<br />

Flash the lights once.

<a name="get_charge_state-2"></a>

### get_charge_state/2 ###

<pre><code>
get_charge_state(Token::binary(), ID::binary()) -&gt; <a href="#type-api_return">api_return()</a>
</code></pre>
<br />

Returns the state of charge in the battery.

<a name="get_climate_settings-2"></a>

### get_climate_settings/2 ###

<pre><code>
get_climate_settings(Token::binary(), ID::binary()) -&gt; <a href="#type-api_return">api_return()</a>
</code></pre>
<br />

Returns the current temperature and climate control state.

<a name="get_driving_position-2"></a>

### get_driving_position/2 ###

<pre><code>
get_driving_position(Token::binary(), ID::binary()) -&gt; <a href="#type-api_return">api_return()</a>
</code></pre>
<br />

Returns the driving and position state of the vehicle.

<a name="get_gui_settings-2"></a>

### get_gui_settings/2 ###

<pre><code>
get_gui_settings(Token::binary(), ID::binary()) -&gt; <a href="#type-api_return">api_return()</a>
</code></pre>
<br />

Returns various information about the GUI settings of the car, such as unit format and range display.

<a name="get_mobile_access-2"></a>

### get_mobile_access/2 ###

<pre><code>
get_mobile_access(Token::binary(), ID::binary()) -&gt; <a href="#type-api_return">api_return()</a>
</code></pre>
<br />

Determines if mobile access to the vehicle is enabled.

<a name="get_oauth_token-4"></a>

### get_oauth_token/4 ###

<pre><code>
get_oauth_token(ClientID::binary(), ClientSecret::binary(), Email::binary(), Password::binary()) -&gt; <a href="#type-api_return">api_return()</a>
</code></pre>
<br />

Get an Access Token.

<a name="get_state-2"></a>

### get_state/2 ###

<pre><code>
get_state(Token::binary(), ID::binary()) -&gt; <a href="#type-api_return">api_return()</a>
</code></pre>
<br />

Returns the vehicle's physical state, such as which doors are open.

<a name="get_vehicles-1"></a>

### get_vehicles/1 ###

<pre><code>
get_vehicles(Token::binary()) -&gt; <a href="#type-api_return">api_return()</a>
</code></pre>
<br />

Retrieve a list of your owned vehicles.

<a name="honk_horn-2"></a>

### honk_horn/2 ###

<pre><code>
honk_horn(Token::binary(), ID::binary()) -&gt; <a href="#type-api_return">api_return()</a>
</code></pre>
<br />

Honk the horn once.

<a name="lock_doors-2"></a>

### lock_doors/2 ###

<pre><code>
lock_doors(Token::binary(), ID::binary()) -&gt; <a href="#type-api_return">api_return()</a>
</code></pre>
<br />

Lock the car's doors.

<a name="open_charge_port-2"></a>

### open_charge_port/2 ###

<pre><code>
open_charge_port(Token::binary(), ID::binary()) -&gt; <a href="#type-api_return">api_return()</a>
</code></pre>
<br />

Opens the charge port. Does not close the charge port (for now...). This endpoint also unlocks the charge port if it's locked.

<a name="open_close_trunk-2"></a>

### open_close_trunk/2 ###

<pre><code>
open_close_trunk(Token::binary(), ID::binary()) -&gt; <a href="#type-api_return">api_return()</a>
</code></pre>
<br />

Open the trunk. Call the endpoint again to close.

<a name="open_roof-3"></a>

### open_roof/3 ###

<pre><code>
open_roof(Token::binary(), ID::binary(), Percent::binary()) -&gt; <a href="#type-api_return">api_return()</a>
</code></pre>
<br />

Controls the car's panoramic roof, if installed.

<a name="remote_start-3"></a>

### remote_start/3 ###

<pre><code>
remote_start(Token::binary(), ID::binary(), Password::binary()) -&gt; <a href="#type-api_return">api_return()</a>
</code></pre>
<br />

Start the car for keyless driving. Must start driving within 2 minutes of issuing this request.

<a name="reset_valet_pin-2"></a>

### reset_valet_pin/2 ###

<pre><code>
reset_valet_pin(Token::binary(), ID::binary()) -&gt; <a href="#type-api_return">api_return()</a>
</code></pre>
<br />

Resets the PIN set for valet mode, if set.

<a name="set_charge_limit-3"></a>

### set_charge_limit/3 ###

<pre><code>
set_charge_limit(Token::binary(), ID::binary(), Limit::binary()) -&gt; <a href="#type-api_return">api_return()</a>
</code></pre>
<br />

Set the charge limit to a custom percentage.

<a name="set_charge_limit_to_max-2"></a>

### set_charge_limit_to_max/2 ###

<pre><code>
set_charge_limit_to_max(Token::binary(), ID::binary()) -&gt; <a href="#type-api_return">api_return()</a>
</code></pre>
<br />

Set the charge mode to max range (100% under the new percentage system introduced in 4.5). Use sparingly!

<a name="set_charge_limit_to_standard-2"></a>

### set_charge_limit_to_standard/2 ###

<pre><code>
set_charge_limit_to_standard(Token::binary(), ID::binary()) -&gt; <a href="#type-api_return">api_return()</a>
</code></pre>
<br />

Set the charge mode to standard (90% under the new percentage system introduced in 4.5).

<a name="set_tempeature-4"></a>

### set_tempeature/4 ###

<pre><code>
set_tempeature(Token::binary(), ID::binary(), DriverTemp::binary(), PassenderTemp::binary()) -&gt; <a href="#type-api_return">api_return()</a>
</code></pre>
<br />

Set the temperature target for the HVAC system

<a name="set_valet_mode-2"></a>

### set_valet_mode/2 ###

<pre><code>
set_valet_mode(Token::binary(), ID::binary()) -&gt; <a href="#type-api_return">api_return()</a>
</code></pre>
<br />

Sets valet mode on or off with a PIN to disable it from within the car.
Reuses last PIN from previous valet session. Valet Mode limits the car's top
speed to 70MPH and 80kW of acceleration power. It also disables Homelink,
Bluetooth and Wifi settings, and the ability to disable mobile access to the car.
It also hides your favorites, home, and work locations in navigation.

<a name="start_charging-2"></a>

### start_charging/2 ###

<pre><code>
start_charging(Token::binary(), ID::binary()) -&gt; <a href="#type-api_return">api_return()</a>
</code></pre>
<br />

Start charging. Must be plugged in, have power available, and not have reached your charge limit.

<a name="start_hvac-2"></a>

### start_hvac/2 ###

<pre><code>
start_hvac(Token::binary(), ID::binary()) -&gt; <a href="#type-api_return">api_return()</a>
</code></pre>
<br />

Start the climate control system. Will cool or heat automatically, depending on set temperature.

<a name="stop_charging-2"></a>

### stop_charging/2 ###

<pre><code>
stop_charging(Token::binary(), ID::binary()) -&gt; <a href="#type-api_return">api_return()</a>
</code></pre>
<br />

Stop charging. Must already be charging.

<a name="stop_hvac-2"></a>

### stop_hvac/2 ###

<pre><code>
stop_hvac(Token::binary(), ID::binary()) -&gt; <a href="#type-api_return">api_return()</a>
</code></pre>
<br />

Stop the climate control system.

<a name="unlock_doors-2"></a>

### unlock_doors/2 ###

<pre><code>
unlock_doors(Token::binary(), ID::binary()) -&gt; <a href="#type-api_return">api_return()</a>
</code></pre>
<br />

Unlock the car's doors.

<a name="wake_up-2"></a>

### wake_up/2 ###

<pre><code>
wake_up(Token::binary(), ID::binary()) -&gt; <a href="#type-api_return">api_return()</a>
</code></pre>
<br />

Wakes up the car from the sleep state. Necessary to get some data from the car.

