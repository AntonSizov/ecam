-module(ecam_newcamd_client).

-behaviour(gen_fsm).

-define(get_mandatory_val(Key, List),
	((fun() ->
		case proplists:get_value(Key, List) of
			undefined ->
				erlang:error({missing_mandatory_value,
					[{key, Key},
					{proplist, List}] });
			Value -> Value
		end
	end)())).

-define(CWS_NETMSGSIZE, 362).
-define(NCD_CLIENT_ID, 16#8888).

-define(CWS_FIRSTCMDNO, 16#e0).

-define(MSG_CLIENT_2_SERVER_LOGIN, 		?CWS_FIRSTCMDNO).
-define(MSG_CLIENT_2_SERVER_LOGIN_ACK, 	?MSG_CLIENT_2_SERVER_LOGIN + 1).
-define(MSG_CLIENT_2_SERVER_LOGIN_NAK, 	?MSG_CLIENT_2_SERVER_LOGIN + 2).
-define(MSG_CARD_DATA_REQ, 				?MSG_CLIENT_2_SERVER_LOGIN + 3).
-define(MSG_CARD_DATA, 					?MSG_CLIENT_2_SERVER_LOGIN + 4).
-define(MSG_SERVER_2_CLIENT_NAME, 		?MSG_CLIENT_2_SERVER_LOGIN + 5).
-define(MSG_SERVER_2_CLIENT_NAME_ACK, 	?MSG_CLIENT_2_SERVER_LOGIN + 6).
-define(MSG_SERVER_2_CLIENT_NAME_NAK, 	?MSG_CLIENT_2_SERVER_LOGIN + 7).
-define(MSG_SERVER_2_CLIENT_LOGIN,		?MSG_CLIENT_2_SERVER_LOGIN + 8).
-define(MSG_SERVER_2_CLIENT_LOGIN_ACK, 	?MSG_CLIENT_2_SERVER_LOGIN + 9).
-define(MSG_SERVER_2_CLIENT_LOGIN_NAK, 	?MSG_CLIENT_2_SERVER_LOGIN + 10).
-define(MSG_ADMIN, 						?MSG_CLIENT_2_SERVER_LOGIN + 11).
-define(MSG_ADMIN_ACK, 					?MSG_CLIENT_2_SERVER_LOGIN + 12).
-define(MSG_ADMIN_LOGIN, 				?MSG_CLIENT_2_SERVER_LOGIN + 13).
-define(MSG_ADMIN_LOGIN_ACK, 			?MSG_CLIENT_2_SERVER_LOGIN + 14).
-define(MSG_ADMIN_LOGIN_NAK, 			?MSG_CLIENT_2_SERVER_LOGIN + 15).
-define(MSG_ADMIN_COMMAND, 				?MSG_CLIENT_2_SERVER_LOGIN + 16).
-define(MSG_ADMIN_COMMAND_ACK,			?MSG_CLIENT_2_SERVER_LOGIN + 17).
-define(MSG_ADMIN_COMMAND_NAK, 			?MSG_CLIENT_2_SERVER_LOGIN + 18).
-define(MSG_KEEPALIVE,					?CWS_FIRSTCMDNO + 0x1d).
-define(MSG_SERVER_2_CLIENT_OSD, 		16#d1).
-define(MSG_SERVER_2_CLIENT_ALLCARDS,	16#d2).
-define(MSG_SERVER_2_CLIENT_ADDCARD, 	16#d3).
-define(MSG_SERVER_2_CLIENT_REMOVECARD, 16#d4).
-define(MSG_SERVER_2_CLIENT_CHANGE_KEY, 16#d5).
-define(MSG_SERVER_2_CLIENT_GET_VERSION,16#d6).
-define(MSG_SERVER_2_CLIENT_ADDSID,		16#d7).
-define(MSG_CLIENT_2_SERVER_CARDDISCOVER,16#d8).

-define(COMMTYPE_CLIENT, 0).
-define(COMMTYPE_SERVER, 1).

%% API Functions Exports
-export([
	start_link/1
]).

%% gen_fsm Function Exports
-export([
	init/1,
	handle_event/3,
	handle_sync_event/4,
	handle_info/3,
	terminate/3,
	code_change/4,

	st_unbound/2,
	st_unbound/3,

	st_negotiating/2,
	st_negotiating/3
]).

-type state() ::
	st_unbound		| %% client disconnected
	st_negotiating 	| %% client connected and wait for init seq
	st_binding		| %% client sent auth req and wait for auth resp
	st_bound.		  %% client got auth resp

-record(st, {
	user 	:: list(),
	pwd 	:: list(),
	ncd_key :: integer(),
	host 	:: list(),
	port 	:: integer(),

	sock	:: pid(),
	ref 	:: reference()
}).

%% ===================================================================
%% API Function Definitions
%% ===================================================================

start_link(Spec) ->
    gen_fsm:start_link({local, ?MODULE}, ?MODULE, [Spec], []).

%% ===================================================================
%% GenServer Function Definitions
%% ===================================================================

init([Spec]) ->
	lager:debug("Newcamd client started: ~p", [Spec]),
	User = ?get_mandatory_val(user, Spec),
	Pwd = ?get_mandatory_val(pwd, Spec),
	NcdKey = ?get_mandatory_val(key, Spec),
	Host = ?get_mandatory_val(host, Spec),
	Port = ?get_mandatory_val(port, Spec),
	Ref = make_ref(),
	connect(Ref, 0),
    {ok, st_unbound, #st{	user = User,
						pwd = Pwd,
						ncd_key = NcdKey,
						host = Host,
						port = Port,
						ref	= Ref }}.

handle_event(Event, _StateName, St) ->
	{stop, {unexp_event, Event}, St}.

handle_sync_event(Event, _From, _StateName, StateData) ->
	{stop, {unexp_event, Event}, StateData}.

handle_info({tcp, _Sock, InitSeq = <<_:112>>}, st_negotiating, St) ->
	lager:debug("Got init seq: ~p", [InitSeq]),
	%% below client should send auth req

	%% {ok, SessionDesKey} = oscam:des_login_key_get(InitSeq, NewcamdKey),
	%% {ok, PassCrypt} = oscam:md5_crypt(Pass, "$1$abcdefgh$"),
	%% AuthReq = <<?MSG_CLIENT_2_SERVER_LOGIN, 0:16, User/binary, 0, PassCrypt/binary, 0>>,
	%% network_message_send(Sock, 0, AuthReq, InitKey, ?COMMTYPE_CLIENT, ?NCD_CLIENT_ID, undefined),

	{next_state, st_binding, St};
handle_info({tcp, _Sock, Data}, StName, St) ->
	lager:debug("Got data: ~p", [Data]),
	{next_state, StName, St};
handle_info({tcp_closed, _Sock}, _StName, St) ->
	lager:warning("Socket closed"),
	%% below client should reconnect
	%% connect(Ref),
	{next_state, st_unbound, St};
handle_info({tcp_error, _Sock, Error}, _StName, St) ->
	lager:error("Tcp error: ~p", [Error]),
	%% below client should reconnect
	%% connect(Ref),
	{next_state, st_unbound, St};
handle_info(Info, _StName, St) ->
    {stop, {bad_info, Info}, St}.

code_change(_OldVsn, StateName, StateData, _Extra) ->
	{ok, StateName, StateData}.

terminate(_Reason, _StateName, _StateData) ->
	ok.

%% ===================================================================
%% UNBOUND State
%% ===================================================================

st_unbound({Ref, connect}, St = #st{ref = Ref}) ->
	Host = St#st.host,
	Port = St#st.port,
	lager:debug("Connect to ~s:~p", [Host, Port]),
	try
	    {ok, Sock} = gen_tcp:connect(Host, Port, [binary, {packet, 0}]),
		lager:info("Successfully connected to ~s:~p [~p]", [Host, Port, Sock]),
		{next_state, st_negotiating, St#st{sock = Sock}}
	catch
		_EType:Error ->
			lager:error("Connection failed: ~p", [Error]),
			connect(Ref),
			{next_state, st_unbound, St}
	end.

st_unbound(Event, From, St) ->
	{stop, {unexp_event, Event}, St}.

%% ===================================================================
%% NEGOTIATING State
%% ===================================================================

st_negotiating(Event, St) ->
	{stop, {unexp_event, Event}, St}.

st_negotiating(Event, _From, St) ->
	{stop, {unexp_event, Event}, St}.

%% ===================================================================
%% Internal Functions
%% ===================================================================

connect(Ref) ->
	connect(Ref, 2000).
connect(Ref, Sleep) ->
	{ok, _TRef} = timer:apply_after(Sleep, gen_fsm, send_event, [?MODULE, {Ref, connect}]).
