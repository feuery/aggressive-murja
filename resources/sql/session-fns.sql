-- name: set-session-val* @execute
insert into blog.session_store (session_key, var_name, val)
values ($1, $2, $3)
on conflict(session_key, var_name) do
update set val = excluded.val;

-- name: ensure-session*
-- count: single
select ss.session_key
from blog.serialized_session ss 
where ss.owner = $2 AND ss.session_key = $3 AND $1 < ss.expires_at;

-- name: ensure-username-session*
-- count: single
select ss.session_key
from blog.serialized_session ss
join blog.users usr on ss.owner = usr.id 
where usr.username = $2 AND ss.session_key = $3 AND $1 < ss.expires_at;

-- name: login-query-session*
-- count: single
select ss.session_key
from blog.serialized_session ss
join blog.users usr on ss.owner = usr.id 
where usr.username = $2 AND $1 < ss.expires_at;

-- name: insert-session*
-- returns: :array-hash
insert into blog.serialized_session (owner)
select usr.id
from blog.users usr
where usr.username = $2 
returning session_key, expires_at - $1 AS max_age;

-- name: get-session-val*
-- returns: :array-hash
select sstore.val
from blog.session_store sstore
join blog.serialized_session ss on ss.session_key = sstore.session_key
where ss.owner = $2 AND sstore.var_name = $3 AND $1 < ss.expires_at;

-- name: all-session-vals
-- returns: :array-hash
select sstore.var_name, sstore.val
from blog.session_store sstore
join blog.serialized_session ss on sstore.session_key = ss.session_key
join blog.users usr on ss.owner = usr.id 
where usr.username = $2 and ss.session_key = $3 and $1 < ss.expires_at;
