%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2007-2013. All Rights Reserved.
%%
%% The contents of this file are subject to the Erlang Public License,
%% Version 1.1, (the "License"); you may not use this file except in
%% compliance with the License. You should have received a copy of the
%% Erlang Public License along with this software. If not, it can be
%% retrieved online at http://www.erlang.org/.
%%
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%% the License for the specific language governing rights and limitations
%% under the License.
%%
%% %CopyrightEnd%
%%

%%

-module(ssl_config).

-include("ssl_internal.hrl").
-include("ssl_connection.hrl").
-include_lib("public_key/include/public_key.hrl"). 

-export([init/2]).

init(SslOpts, Role) ->
    
    init_manager_name(SslOpts#ssl_options.erl_dist),

    {ok, CertDbRef, CertDbHandle, FileRefHandle, PemCacheHandle, CacheHandle, OwnCert} 
	= init_certificates(SslOpts, Role),
    PrivateKey =
	init_private_key(PemCacheHandle, SslOpts#ssl_options.key, SslOpts#ssl_options.keyfile,
			 SslOpts#ssl_options.password, Role),
    DHParams = init_diffie_hellman(PemCacheHandle, SslOpts#ssl_options.dh, SslOpts#ssl_options.dhfile, Role),
    {ok, CertDbRef, CertDbHandle, FileRefHandle, CacheHandle, OwnCert, PrivateKey, DHParams}.

init_manager_name(false) ->
    put(ssl_manager, ssl_manager:manager_name(normal));
init_manager_name(true) ->
    put(ssl_manager, ssl_manager:manager_name(dist)).

init_certificates(#ssl_options{cacerts = CaCerts,
			       cacertfile = CACertFile,
			       certfile = CertFile,
			       cert = Cert}, Role) ->
    {ok, CertDbRef, CertDbHandle, FileRefHandle, PemCacheHandle, CacheHandle} =
	try 
	    Certs = case CaCerts of
			undefined ->
			    CACertFile;
			_ ->
			    {der, CaCerts}
		    end,
	    {ok, _, _, _, _, _} = ssl_manager:connection_init(Certs, Role)
	catch
	    _:Reason ->
		file_error(CACertFile, {cacertfile, Reason})
	end,
    init_certificates(Cert, CertDbRef, CertDbHandle, FileRefHandle, PemCacheHandle, 
		      CacheHandle, CertFile, Role).

init_certificates(undefined, CertDbRef, CertDbHandle, FileRefHandle, PemCacheHandle, CacheHandle, <<>>, _) ->
    {ok, CertDbRef, CertDbHandle, FileRefHandle, PemCacheHandle, CacheHandle, undefined};

init_certificates(undefined, CertDbRef, CertDbHandle, FileRefHandle, PemCacheHandle, 
		  CacheHandle, CertFile, client) ->
    try 
	%% Ignoring potential proxy-certificates see: 
	%% http://dev.globus.org/wiki/Security/ProxyFileFormat
	[OwnCert|_] = ssl_certificate:file_to_certificats(CertFile, PemCacheHandle),
	{ok, CertDbRef, CertDbHandle, FileRefHandle, PemCacheHandle, CacheHandle, OwnCert}
    catch _Error:_Reason  ->
	    {ok, CertDbRef, CertDbHandle, FileRefHandle, PemCacheHandle, CacheHandle, undefined}
    end;

init_certificates(undefined, CertDbRef, CertDbHandle, FileRefHandle, 
		  PemCacheHandle, CacheRef, CertFile, server) ->
    try
	[OwnCert|_] = ssl_certificate:file_to_certificats(CertFile, PemCacheHandle),
	{ok, CertDbRef, CertDbHandle, FileRefHandle, PemCacheHandle, CacheRef, OwnCert}
    catch
	_:Reason ->
	    file_error(CertFile, {certfile, Reason})	    
    end;
init_certificates(Cert, CertDbRef, CertDbHandle, FileRefHandle, PemCacheHandle, CacheRef, _, _) ->
    {ok, CertDbRef, CertDbHandle, FileRefHandle, PemCacheHandle, CacheRef, Cert}.

init_private_key(_, undefined, <<>>, _Password, _Client) ->
    undefined;
init_private_key(DbHandle, undefined, KeyFile, Password, _) ->
    try
	{ok, List} = ssl_manager:cache_pem_file(KeyFile, DbHandle),
	[PemEntry] = [PemEntry || PemEntry = {PKey, _ , _} <- List,
				  PKey =:= 'RSAPrivateKey' orelse
				      PKey =:= 'DSAPrivateKey' orelse
				      PKey =:= 'ECPrivateKey' orelse
				      PKey =:= 'PrivateKeyInfo'
		     ],
	private_key(public_key:pem_entry_decode(PemEntry, Password))
    catch 
	_:Reason ->
	    file_error(KeyFile, {keyfile, Reason}) 
    end;

init_private_key(_,{Asn1Type, PrivateKey},_,_,_) ->
    private_key(init_private_key(Asn1Type, PrivateKey)).

init_private_key(Asn1Type, PrivateKey) ->
    public_key:der_decode(Asn1Type, PrivateKey).

private_key(#'PrivateKeyInfo'{privateKeyAlgorithm =
				 #'PrivateKeyInfo_privateKeyAlgorithm'{algorithm = ?'rsaEncryption'},
			     privateKey = Key}) ->
    public_key:der_decode('RSAPrivateKey', iolist_to_binary(Key));

private_key(#'PrivateKeyInfo'{privateKeyAlgorithm =
				 #'PrivateKeyInfo_privateKeyAlgorithm'{algorithm = ?'id-dsa'},
			     privateKey = Key}) ->
    public_key:der_decode('DSAPrivateKey', iolist_to_binary(Key));

private_key(Key) ->
    Key.

-spec(file_error(_,_) -> no_return()).
file_error(File, Throw) ->
    case Throw of
	{Opt,{badmatch, {error, {badmatch, Error}}}} ->
	    throw({options, {Opt, binary_to_list(File), Error}});
	_ ->
	    throw(Throw)
    end.

init_diffie_hellman(_,Params, _,_) when is_binary(Params)->
    public_key:der_decode('DHParameter', Params);
init_diffie_hellman(_,_,_, client) ->
    undefined;
init_diffie_hellman(_,_,undefined, _) ->
    ?DEFAULT_DIFFIE_HELLMAN_PARAMS;
init_diffie_hellman(DbHandle,_, DHParamFile, server) ->
    try
	{ok, List} = ssl_manager:cache_pem_file(DHParamFile,DbHandle),
	case [Entry || Entry = {'DHParameter', _ , _} <- List] of
	    [Entry] ->
		public_key:pem_entry_decode(Entry);
	    [] ->
		?DEFAULT_DIFFIE_HELLMAN_PARAMS
	end
    catch
	_:Reason ->
	    file_error(DHParamFile, {dhfile, Reason}) 
    end.
