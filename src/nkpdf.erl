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
-module(nkpdf).
-export([parse_generator/3]).
-export_type([generator_id/0, generator/0, options/0]).
-include("nkpdf.hrl").


%% ===================================================================
%% Types
%% ===================================================================
-type generator_id() :: term().
-type generator() :: #{ id => atom(), class => atom(), config => map() }.
-type options() :: #{ template_url => binary(), 
                      fields => map(),
                      config => map() }.

%% @doc
-spec parse_generator(nkservice:id(), map(), nklib_syntax:parse_opts()) ->
    {ok, generator(), [binary()]} | {error, term()}.

parse_generator(SrvId, Map, ParseOpts) ->
    SrvId:nkpdf_parse_generator(Map, ParseOpts).
