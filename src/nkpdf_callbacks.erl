%% -------------------------------------------------------------------
%%
%% Copyright (c) 2017 Carlos Gonzalez Florido.  All Rights Reserved.
%%
%% This file is provided to you under the Apache License,
%% Version 2.0 (the "License"); you may not use this file
%% except in compliance with the License.  You may obtain
%% a copy of the License at
%%
%%   http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing,
%% software distributed under the License is distributed on an
%% "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
%% KIND, either express or implied.  See the License for the
%% specific language governing permissions and limitations
%% under the License.
%%
%% -------------------------------------------------------------------
-module(nkpdf_callbacks).
-export([plugin_deps/0, plugin_syntax/0]).
-export([service_init/2]).
-export([nkpdf_get_generator/2, nkpdf_parse_generator/2, nkpdf_print/3]).
-include("nkpdf.hrl").


%% ===================================================================
%% Plugin callbacks
%%
%% These are used when nkpdf is started as a NkSERVICE plugin
%% ===================================================================


plugin_deps() ->
    [].

plugin_syntax() ->
    #{}.

service_init(_Service, #{id:=SrvId}=State) ->
    ?INFO("service init: ~p", [SrvId]),
    lists:foreach(
        fun 
            (#{id := Id}=Data) ->
                case SrvId:nkpdf_parse_generator(Data, #{}) of
                    {ok, generator, _} ->
                        ?WARN("loading generator ~p", [Id]),
                        nkpdf_app:put_generator(nklib_util:to_binary(Id), generator);
                    {error, Error} ->
                        ?WARN("error with generator ~p: ~p", [Data, Error])
                end;
            (Data) ->
                ?WARN("invalid generator: ~p", [Data])
        end, 
        nkpdf_app:get(generators, [])), 
    {ok, State}.

%% ===================================================================
%% Image processing callbacks
%% ===================================================================

%% @doc Gets a generator
-spec nkpdf_get_generator(nkservice:id(), nkpdf:generator_id()) ->
    {ok, nkpdf:generator()} | {error, term()}.

nkpdf_get_generator(_SrvId, Id) ->
    case nkpdf_app:get_generator(Id) of
        not_found ->
            {error, {generator_not_found, Id}};
        generator ->
            {ok, generator}
    end.

%% @doc Parses a generator
-spec nkpdf_parse_generator(map(), nklib_syntax:parse_opts()) ->
    {ok, nkpdf:generator(), [binary()]} | {error, term()}.

nkpdf_parse_generator(_generator, _Opts) ->
    {error, invalid_generator}.


%% @doc Prints a pdf, using the specified generator and options
-spec nkpdf_print(nkservice:id(), nkpdf:generator(), nkpdf:options()) ->
    {ok, binary()} | {error, term()}.

nkpdf_print(_SrvId, _generator, _Options) ->
    {error, invalid_generator}.
