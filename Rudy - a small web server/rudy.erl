-module(rudy).
-export([start/1, stop/0, close_client/1]).


init(Port) ->
    Opt = [list, {active, false}, {reuseaddr, true}],
    case gen_tcp:listen(Port, Opt) of
        {ok, Listen} ->
            %io:format("rudy: listen socket: ~w~n", [Listen]),
            handler(Listen),
            ok;
        {error, _Error} ->
            error
    end.
    
handler(Listen) ->
    case gen_tcp:accept(Listen) of
        {ok, Client} ->
            %io:format("rudy: client socket: ~w~n", [Client]),
            request(Client),
            handler(Listen);
        {error, _Error} ->
            error
    end.

request(Client) ->
    request(Client, []).    
    
request(Client, OldStr) ->
    Recv = gen_tcp:recv(Client, 0),
    timer:apply_after(20000, rudy, close_client,[Client]),
    case Recv of
        {ok, NewStr} ->
            %io:format("Received Http Request Part: ~p~n", [NewStr]),            
            case http:isMessageCompleted(OldStr ++ NewStr) of
                {true, Str} -> 
                    %io:format("Received Completed Http Request: ~p~n", [Str]),        
                    Request = http:parse_request(Str),
                    Response = reply(Request),
                    %io:format("Sending Http Response: ~p~n", [Response]),        
                    gen_tcp:send(Client, Response);
                false -> 
                    request(Client, OldStr ++ NewStr)
            end;
        {error, Error} ->
            io:format("rudy: error: ~w~n", [Error])
    end,
     %io:format("rudy: client socket closing: ~w~n", [Client]),
    gen_tcp:close(Client).

close_client(Client) ->
    %io:format("rudy: http request receiving timeout, client socket closing ~n"),
    gen_tcp:close(Client).
        
            
reply({{get, URI, _}, _, _}) ->
    %timer:sleep(40),
    http:ok("<html><head><title>Rudy</title></head><body>This is a test.<br/>" ++ URI ++ "</body></html>").
    

start(Port) ->
    register(rudy, spawn(fun() -> init(Port) end)).
    
stop() ->
    exit(whereis(rudy), "time to die").
    
