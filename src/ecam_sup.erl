
-module(ecam_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(NewcamdClient(SpecId, Spec),
	{SpecId, {ecam_newcamd_client, start_link, [Spec]},
			permanent, 5000, worker, [ecam_newcamd_client]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
	{ok, Readers} = application:get_env(ecam, readers),

	%% perform newcamd readers spec
	NewcamdReaders = proplists:get_value(newcamd, Readers, []),
	{NewcamdReadersSpec, _} = lists:foldl(fun(Reader, {Acc, N}) ->
		SpecId = list_to_atom("newcamd_client_" ++ integer_to_list(N)),
		Spec = ?NewcamdClient(SpecId, Reader),
		{[Spec | Acc], N + 1}
	end, {[], 1}, NewcamdReaders),

    {ok, { {one_for_one, 5, 10}, NewcamdReadersSpec} }.
