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
       p.Tags as "Tags", p.hidden, p.unlisted
FROM blog.Post p
WHERE $1 OR (NOT p.unlisted AND NOT p.hidden)
ORDER BY p.created_at DESC;

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
       (SELECT MAX(version) + 1 FROM blog.Post_History phh WHERE (phh.id = p.id AND ((not phh.unlisted) OR $2) AND ((not phh.hidden) OR $2))) AS version,
       json_agg(DISTINCT version) as "versions", p.hidden, p.unlisted,
       json_agg(to_jsonb (pl.*) - 'referencee_id') as previously
FROM blog.Post p
JOIN blog.Users u ON u.ID = p.creator_id
LEFT JOIN blog.Previously_Link_Titles pl ON p.id = pl.referencee_id
LEFT JOIN blog.Post_History ph ON (ph.id = p.id AND ((not ph.unlisted) OR $2) AND ((not ph.hidden) OR $2))
WHERE p.ID = $1 AND (NOT p.hidden OR (p.hidden AND $2))
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
     json_agg(DISTINCT ph.version) as "versions", p.hidden, p.unlisted,
     json_agg(to_jsonb (pl.*) - 'referencee_id') as previously
FROM blog.Post_History p
JOIN blog.Users u ON u.ID = p.creator_id
LEFT JOIN blog.Post_History ph ON (ph.id = p.id AND (not ph.unlisted) AND (not ph.hidden))
LEFT JOIN blog.Previously_Link_Titles pl ON p.id = pl.referencee_id
WHERE p.ID = $1 AND p.version = $2 AND not p.hidden
GROUP BY p.ID, p.Title, p.created_at, p.Content, p.tags, p.version, "amount-of-comments", u.username, u.nickname, u.img_location;


-- name: get-all*
SELECT p.id, p.Title, p.Content, p.created_at, p.tags, u.Username, u.Nickname, u.Img_location, COUNT(c.ID) AS "amount-of-comments", p.hidden, p.unlisted,
       json_agg(to_jsonb (pl.*) - 'referencee_id') as previously
FROM blog.Post p
JOIN blog.Users u ON u.ID = p.creator_id
LEFT JOIN blog.Comment c ON c.parent_post_id = p.ID
LEFT JOIN blog.Previously_Link_Titles pl ON p.id = pl.referencee_id
WHERE NOT p.hidden
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
SELECT p.ID, p.Title, p.Content, p.created_at, p.tags, COUNT(c.ID) AS "amount-of-comments", json_build_object('username', u.Username, 'nickname', u.Nickname, 'img_location', u.Img_location) as "creator", json_agg(DISTINCT version) as "versions", p.hidden, p.unlisted,
       json_agg(to_jsonb (pl.*) - 'referencee_id') as previously
FROM blog.Post p
JOIN blog.Users u ON u.ID = p.creator_id
LEFT JOIN blog.Comment c ON c.parent_post_id = p.ID
LEFT JOIN blog.Post_History ph ON (ph.id = p.id AND ((not ph.unlisted) OR $3) AND ((not ph.hidden) OR $3))
LEFT JOIN blog.Previously_Link_Titles pl ON p.id = pl.referencee_id
WHERE ((NOT p.unlisted) OR $3)
  AND ((NOT p.hidden) OR $3)
GROUP BY p.ID, u.ID
ORDER BY p.created_at DESC
LIMIT $2
OFFSET $1;

-- name: landing-page-ids*
SELECT id
FROM blog.Post 
WHERE tags ? 'landing-page' AND NOT hidden;

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
WHERE p.tags ? 'landing-page' AND NOT p.hidden
GROUP BY p.ID, u.ID;

-- name: landing-page-title 
SELECT p.Title, p.Id
FROM blog.Post p
WHERE p.tags ? 'landing-page' AND NOT p.hidden;


-- name: get-tagged*
-- returns: :array-hash
SELECT p.ID, p.Title, p.Content, p.created_at, p.tags, 0 AS "amount-of-comments", json_build_object('username', u.Username, 'nickname', u.Nickname, 'img_location', u.Img_location) as "creator", json_agg(DISTINCT version) as "versions", p.hidden, p.unlisted
FROM blog.Post p
JOIN blog.Users u ON u.ID = p.creator_id
LEFT JOIN blog.Post_History ph ON ph.id = p.id 
WHERE p.tags ? $1 AND (NOT p.hidden OR (p.hidden AND $2))
      	     	      and ((NOT p.unlisted) OR $2)
GROUP BY p.ID, u.ID;

-- name: insert-post
-- (:title, :content, :creator-id, :tags, :hidden, :unlisted) ==
-- ($1, $2, $3, $4, $5, §6)
insert into blog.post (title, content, creator_id, tags, hidden, unlisted)
values ($1, $2, $3, $4, $5, $6) returning id;

-- name: update-post
-- (:title, :content, :tags, :creator-id) ==
-- ($1, $2, $3, $4)
update blog.post
set title = $1,
    content = $2,
    tags = $3,
    hidden = $4,
    unlisted = $5
where id = $6;

-- name: set-hidden?
update blog.post
set hidden = $2 
where id = $1;

-- name: link-previously
INSERT INTO blog.Previously_Link VALUES ($1, $2) ON CONFLICT DO NOTHING;

-- name: search-posts
-- returns: :array-hash
select post.id, post.title from blog.Post where (title ilike '%'||$1||'%' or content ilike '%'||$1||'%') and not unlisted and not hidden;
