-- name: post-comments*
SELECT c.ID, c.parent_comment_id, 
c.Content,
c.created_at,
u.Username, u.Nickname, u.Img_location
FROM blog.Comment c
JOIN blog.Users u ON u.ID = c.creator_id
WHERE c.parent_post_id = :parent-post-id
ORDER BY c.created_at;

-- $1 == allow-hidden?
-- name: get-titles-by-year*
-- returns: :array-hash
SELECT p.Title AS "Title",
       EXTRACT(MONTH FROM p.created_at) AS "Month",
       EXTRACT(YEAR FROM p.created_at) AS "Year",
       p.id as "Id",
       p.Tags as "Tags"
FROM blog.Post p
WHERE $1 OR (NOT p.tags ? 'unlisted' AND NOT p.tags ? 'hidden')
ORDER BY p.created_at DESC;

-- name: post-versions*
SELECT version 
FROM blog.Post_History 
WHERE ID = :post-id AND NOT tags ?? 'hidden' 
ORDER BY version ASC;

-- name: next-post-id
SELECT p.ID 
FROM blog.Post p
WHERE p.ID < :post-id AND NOT p.tags ?? 'hidden'
ORDER BY p.ID DESC
LIMIT 1;

-- name: prev-post-id 
SELECT p.ID 
FROM blog.Post p
WHERE p.ID > :post-id AND NOT p.tags ?? 'hidden'
ORDER BY p.ID ASC
LIMIT 1;

-- name: get-by-id*
-- returns: :array-hash
SELECT p.ID,
       p.Title,
       p.created_at,
       p.Content,
       p.tags,
       '[]'::json as "Comments",
       json_build_object('username',
			 u.Username,
			 'nickname',
			 u.Nickname,
			 'img_location',
			 u.Img_location) as "creator",
       null as "prev-post-id",
       '[]'::json as "versions",
       null as "version",
       null as "next-post-id"
FROM blog.Post p
JOIN blog.Users u ON u.ID = p.creator_id
WHERE p.ID = $1 AND (NOT p.tags ? 'hidden' OR (p.tags ? 'hidden' AND $2))
GROUP BY p.ID, u.ID;

-- name: get-versioned-by-id*
-- returns: :array-hash
-- $1 == post-id
-- $2 == version-id
SELECT p.ID, p.Title, p.created_at, p.Content, p.tags, p.version, 0 AS "amount-of-comments", json_build_object('username',
			 u.Username,
			 'nickname',
			 u.Nickname,
			 'img_location',
			 u.Img_location) as "creator",
       null as "prev-post-id",
       '[]'::json as "versions",
       null as "next-post-id"		 
FROM blog.Post_History p
JOIN blog.Users u ON u.ID = p.creator_id
WHERE p.ID = $1 AND p.version = $2 AND not tags ? 'hidden';


-- name: get-all*
SELECT p.id, p.Title, p.Content, p.created_at, p.tags, u.Username, u.Nickname, u.Img_location, COUNT(c.ID) AS "amount-of-comments"
FROM blog.Post p
JOIN blog.Users u ON u.ID = p.creator_id
LEFT JOIN blog.Comment c ON c.parent_post_id = p.ID
WHERE NOT p.tags ?? 'hidden'
GROUP BY p.ID, u.ID
ORDER BY p.created_at DESC
-- this isn't going to work :)
--~ (when (contains? params :limit) "LIMIT :limit") ;
;


-- $1 == page-id
-- $2 == page-size
-- $3 == show-hidden?
-- name: get-page*
-- returns: :array-hash
SELECT p.ID, p.Title, p.Content, p.created_at, p.tags, COUNT(c.ID) AS "amount-of-comments", json_build_object('username', u.Username, 'nickname', u.Nickname, 'img_location', u.Img_location) as "creator", json_agg(DISTINCT version) as "versions"
FROM blog.Post p
JOIN blog.Users u ON u.ID = p.creator_id
LEFT JOIN blog.Comment c ON c.parent_post_id = p.ID
LEFT JOIN blog.Post_History ph ON (ph.id = p.id AND ((not ph.tags ? 'unlisted') OR $3) AND ((not ph.tags ? 'hidden') OR $3))
WHERE ((NOT p.tags ? 'unlisted') OR $3)
  AND ((NOT p.tags ? 'hidden') OR $3)
GROUP BY p.ID, u.ID
ORDER BY p.created_at DESC
LIMIT $2
OFFSET $1;

-- name: landing-page-ids*
SELECT id
FROM blog.Post 
WHERE tags ?? 'landing-page' AND NOT tags ?? 'hidden';

-- name: get-posts-tags* 
SELECT tags FROM blog.Post WHERE id = :post-id;

-- name: update-tags* @execute
update blog.post
set tags = :new-tags
where id = :post-id;

-- name: delete-post @execute
delete blog.post
where id = :id
-- also: this
--~ (when (contains? params :version) "AND version = :version");
;

-- name: delete-comment @execute
delete blog.comment
where id = :id;

-- name: insert-comment @execute
insert into blog.comment (parent_post_id, parent_comment_id, content, creator_id)
values (:parent-post-id, :parent-comment-id, :content, :creator-id);

-- name: get-landing-page* 
SELECT p.ID, p.Title, p.created_at, p.Content, p.tags, u.Username, u.Nickname, u.Img_location, COUNT(c.ID) AS \"amount-of-comments\"
FROM blog.Post p
JOIN blog.Users u ON u.ID = p.creator_id
LEFT JOIN blog.Comment c ON c.parent_post_id = p.ID
WHERE p.tags ?? 'landing-page' AND NOT p.tags ?? 'hidden'
GROUP BY p.ID, u.ID;

-- name: landing-page-title 
SELECT p.Title, p.Id
FROM blog.Post p
WHERE p.tags ?? 'landing-page' AND NOT p.tags ?? 'hidden';


-- name: get-tagged*
SELECT p.ID, p.Title, p.created_at, p.Content, p.tags, u.Username, u.Nickname, u.Img_location, 0 AS "amount-of-comments"
FROM blog.Post p
JOIN blog.Users u ON u.ID = p.creator_id
WHERE p.tags @> :tags AND (NOT p.tags ?? 'hidden' OR (p.tags ?? 'hidden' AND :show-hidden))  	     	
      	     	      and ((NOT p.tags ?? 'unlisted') OR :show-hidden);

-- name: insert-post
-- (:title, :content, :creator-id, :tags) ==
-- ($1, $2, $3, $4)
insert into blog.post (title, content, creator_id, tags)
values ($1, $2, $3, $4) returning id;

-- name: update-post
-- (:title, :content, :tags, :creator-id) ==
-- ($1, $2, $3, $4)
update blog.post
set title = $1,
    content = $2,
    tags = $3
where id = $4;
