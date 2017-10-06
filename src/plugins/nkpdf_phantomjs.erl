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
-module(nkpdf_phantomjs).
-export([parse_generator/2, generator_syntax/0, print/3]).
-include("../../include/nkpdf.hrl").

parse_generator(Data, ParseOpts) ->
    case nklib_syntax:parse(Data, #{class=> atom}) of
        {ok, #{class:=phantomjs}, _} ->
            case nklib_syntax:parse(Data, generator_syntax(), ParseOpts) of
                  {ok, Generator, UnknownFields} ->
                      {ok, Generator, UnknownFields};
                  {error, Error} ->
                      {error, Error}
              end;
          _ ->
              continue
      end.

generator_syntax() ->
    Base = nkpdf_util:generator_syntax(),
    Base#{
        config := #{
            server => binary,
            user => binary,
            password => binary,
            '__mandatory' => [server]
        }
    }.


-spec print(nkservice:id(), nkpdf:generator(), nkpdf:options()) ->
    {ok, binary()} | {error, term()}.

print(_SrvId, #{ config:=#{ server := Server, user := User, password := Password }}, Options) ->
    AuthToken = base64:encode_to_string( 
                 binary_to_list(<<User/binary, <<":">>/binary, Password/binary>>)),
    handle(request(Server, Options, AuthToken));

print(_SrvId, #{ config:=#{ server := Server} }, Options) -> 
    handle(request(Server, Options, undefined)).

%%====================================================================
%% Internal functions
%%====================================================================
request(BaseUrl, Options, AuthToken) ->
    DefaultHeaders = [{"Content-Type", ?APPLICATION_JSON_MIME},
                      {"Accept", ?APPLICATION_JSON_MIME}],
    Headers = case AuthToken of
                  undefined -> DefaultHeaders;
                  _ ->
                      [{"Authorization", "Basic " ++ AuthToken}|DefaultHeaders]
              end,
    httpc:request(post, 
                  {erlang:binary_to_list(BaseUrl), 
                   Headers, 
                   ?APPLICATION_JSON_MIME, 
                    nklib_json:encode(Options) 
                  },[],[]).

handle({ok, {{_, 200, _}, _, Body}}) ->
    case nklib_json:decode(list_to_binary(Body)) of 
        #{ <<"result">> := <<"ok">>, 
         <<"data" >> := #{ <<"pdf">> := Pdf }} -> 
            {ok, base64:decode(Pdf)};
        _ -> 
            {error, {invalid_json, Body}}
    end;

handle({ok, {{_, _, _}, _, Body}}) ->
    {error, Body};

handle(E) ->
    E.
