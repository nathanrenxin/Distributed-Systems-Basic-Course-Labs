-module(http).
-export([parse_request/1]).
-export([ok/1]).
-export([get/1]).
-export([isMessageCompleted/1]).

parse_request(R0) ->
    {Request, R1} = request_line(R0),
    {Headers, R2} = headers(R1),
    {Body, _} = message_body(R2),
    {Request, Headers, Body}.
    
request_line([$G, $E, $T, 32 |R0]) ->
    {URI, R1} = request_uri(R0),
    {Ver, R2} = http_version(R1),
    [13,10|R3] = R2,
    {{get, URI, Ver}, R3}.
    
request_uri([32|R0])->
    {[], R0};
request_uri([C|R0]) ->
    {Rest, R1} = request_uri(R0),
    {[C|Rest], R1}.
    
http_version([$H, $T, $T, $P, $/, $1, $., $1 | R0]) ->
    {v11, R0};
http_version([$H, $T, $T, $P, $/, $1, $., $0 | R0]) ->
    {v10, R0}.
    
headers([13,10|R0]) ->
    {[],R0};
headers(R0) ->
    {Header, R1} = header(R0),
    {Rest, R2} = headers(R1),
    {[Header|Rest], R2}.
    
header([13,10|R0]) ->
    {[], R0};
header([C|R0]) ->
    {Rest, R1} = header(R0),
    {[C|Rest], R1}.
    
message_body(R) ->
    {R, []}.
    
ok(Body) ->
    "HTTP/1.1 200 OK\r\n" ++ "\r\n" ++ Body.
    
get(URI) ->
    "GET " ++ URI ++ " HTTP/1.1\r\n" ++ "\r\n".
    
isMessageCompleted(Str) ->
    case string:split(Str, [13,10,13,10]) of
        [Str] -> false;
        [Headers, Body] -> 
            Content_length = get_content_length(Headers),
            Body_size = length(Body),
            if 
                Content_length > Body_size -> false;
                Content_length == Body_size -> {true, Str};
                Content_length < Body_size -> {true, Headers++[13,10,13,10]++ lists:sublist(Body, Content_length)}
            end
   end.
   
get_content_length (Headers) -> 
    case string:split(Headers, [$C,$o,$n,$t,$e,$n,$t,$-,$L,$e,$n,$g,$t,$h]) of
        [Headers] -> 0;
        [_, RestHeaders] -> 
            case string:split(RestHeaders, [13,10]) of
                [RestHeaders] -> list_to_integer(get_value(RestHeaders));
                [Value, _RestHeaders1] -> list_to_integer(get_value(Value))
            end
    end.
 
 get_value([]) -> [];
get_value([$:|T]) -> get_value(T);
get_value([32|T]) -> get_value(T);
get_value([H|T]) -> [H] ++ get_value(T).