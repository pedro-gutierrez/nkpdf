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
-module(nkpdf_phantomjs_callbacks).
-export([plugin_deps/0]).
-export([nkpdf_parse_generator/2, nkpdf_generator_syntax/0, nkpdf_print/3]).
-include("../../include/nkpdf.hrl").

%% ===================================================================
%% Plugin callbacks
%%
%% These are used when nkpdf phantomjs is started as a NkSERVICE plugin
%% ===================================================================

plugin_deps() ->
    [nkpdf, nkfile].

%% ===================================================================
%% nkpdf callbacks
%% ===================================================================
nkpdf_parse_generator(Data, ParseOpts) ->
    nkpdf_phantomjs:parse_generator(Data, ParseOpts).

nkpdf_generator_syntax() ->
    nkpdf_phantomjs:generator_syntax().

nkpdf_print(SrvId, #{class:= phantomjs}=G, Options) ->
    nkpdf_phantomjs:print(SrvId, G, Options);

nkpdf_print(_, _, _) ->
    continue.
