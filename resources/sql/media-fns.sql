-- name: insert-media
insert into blog.media (name, data) values ($1, $2) returning id;

-- name: get-media
-- returns: :array-hash
select name, data from blog.media where id = $1::uuid ;

-- name: list-pictures*
-- returns: :array-hash
select id, name from blog.media;

-- name: delete-picture* :!
-- returns: :array-hash
delete from blog.media where id = $1::uuid;

-- name: select-referencing-posts* :?
-- returns: :array-hash
select * from blog.media_post_pairing where media_id = $1;
